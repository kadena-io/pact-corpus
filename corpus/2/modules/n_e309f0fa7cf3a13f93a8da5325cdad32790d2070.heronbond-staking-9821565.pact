(module heronbond-staking GOVERNANCE "Heronbond Staking Pool - Distributes 2% of HERON supply to Heronbond Stakers"
      
          ;;;;; CONSTANTS
          (defconst ACCOUNT_ID_CHARSET CHARSET_LATIN1
          " Allowed character set for Account IDs. ")
      
          (defconst ACCOUNT_ID_MIN_LENGTH 3
            " Minimum character length for account IDs. ")
      
          (defconst ACCOUNT_ID_MAX_LENGTH 256
            " Maximum character length for account IDs. ")
      
          (defconst NAME_MIN_LENGTH 3
            " Minimum character length for account IDs. ")
      
          (defconst NAME_MAX_LENGTH 30
            " Maximum character length for account IDs. ")
      
          ;;;;; CAPABILITIES
      
          (defcap GOVERNANCE ()
            @doc " Give the admin full access to call and upgrade the module. "
            (enforce-keyset "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heron-token-gov")
            )
      
          (defcap ACCOUNT_GUARD(account:string)
              @doc "Verifies account meets format and belongs to caller"
              (enforce (= "k:" (take 2 account)) "For security only support k accounts")
              (enforce-guard
                  (at "guard" (coin.details account))
              )
          )
      
          (defcap POOL_CREATOR_GUARD(pool-id:string)
              @doc "Verifies account belongs to pool creator"
              (let
                      (
                          (pool-data (read pools pool-id ["account"]))
                      )
                      (enforce-guard
                          (at "guard" (coin.details (at "account" pool-data) ))
                      )
              )
      
          )
      
          (defcap CLAIM_CHECK
            ()
          true)
      
          (defcap USER_POOL_CHECK
            ()
          true)
      
          (defcap USER_CLAIM_CHECK
            ()
          true)
      
          (defcap PRIVATE_RESERVE
            ()
          true)
      
          (defun enforce-private-reserve:bool
              ()
            (require-capability (PRIVATE_RESERVE)))
      
          (defun create-pool-guard:guard
              ()
            (create-user-guard (enforce-private-reserve)))
      
          (defun get-account-principal:string ()
           (create-principal (create-pool-guard))
          )
      
          ;;;;;;;;;; SCHEMAS AND TABLES ;;;;;;;;;;;;;;
          (defschema pools-schema
              @doc "Pool information, a unique id is the key"
              id:string
              name:string
              balance:decimal
              reward-token:module{fungible-v2}
              stake-token:module{fungible-v2}
              account:string
              active:bool
              claim-wait-seconds:decimal
              start-time:time
              reward-duration:decimal
              reward-amount:decimal
              stakers:decimal
              withdraw-duration:decimal
              start-balance:decimal
              initialized:bool
              end-time:time
          )
      
          (defschema pools-usage-schema
              @doc "Pool usage data"
              tokens-locked:decimal
              last-updated:time
              paid:decimal
              multiplier:decimal
              next-multiplier:decimal
              has-vesting-connection:bool
              vesting-pool-id:string
          )
      
          (defschema pools-user-stats-schema
            @doc "User total reward earnings in a pool"
            total-earned:decimal)
      
      
          (defschema stakes-schema
              @doc "Stores staking information for users, key is account + pool name"
              id:string
              pool-id:string
              balance:decimal
              last-updated:time
              account:string
              rewards:decimal
              last-claimed:time
              last-withdraw:time
              multiplier:decimal
              start-time:time
          )
      
          (deftable pools:{pools-schema})
          (deftable pools-usage:{pools-usage-schema})
          (deftable pool-user-stats:{pools-user-stats-schema})
          (deftable stakes:{stakes-schema})
      
          ;;;;;;;;; CODE THAT NEEDS PERMISSIONS / CAPABILITIES ;;;;;;;;;;;;;;;
      
       (defun enforce-valid-stake (account:string t_account:string t_balance:decimal)
        @doc "Enforces that the user has a valid stake in the pool"
        (enforce (and (= account t_account) (> t_balance 0.0)) "You have no stake in this pool.")
       )
      
      (defun unstake (pool-id:string account:string)
        @doc "Claims the rewards a user is owed and unstakes a user's staked tokens"
        (with-capability (ACCOUNT_GUARD account)
          (with-default-read stakes (get-stake-id-key account pool-id)
            { "account" : 'nonulls, "balance" : 0.0 }
            { "account" := t_account, "balance" := t_balance }
            (let* ((stake-id (get-stake-id-key account pool-id))
                   (pool-data (read pools pool-id))
                   (stake (read stakes stake-id))
                   (reward-token:module{fungible-v2} (at "reward-token" pool-data))
                   (stake-token:module{fungible-v2} (at "stake-token" pool-data))
                   (to-pay-max (calculate-rewards pool-id account))
                   (available (at "balance" pool-data))
                   (to-pay to-pay-max)
                   (to-pay-stake (at "balance" stake)))
              (enforce-valid-stake account t_account t_balance)
              (enforce (= account (at "account" stake)) "Not authorized to claim this stake")
              (enforce (> to-pay-stake 0.0) "You have no stake to claim in this pool.")
              ;; (enforce-withdraw-duration pool-data stake)
              ;; (if (> to-pay 0.0)
              ;;   (with-capability (USER_CLAIM_CHECK)
              ;;     (pay-user-rewards account to-pay))
              ;;   true)
              (with-capability (USER_CLAIM_CHECK)
                (pay-user-stake account to-pay-stake)
                (update-user-stake-stats stake-id to-pay-stake))
              (with-capability (USER_POOL_CHECK)
                (update-pool-usage-data-unstake pool-id to-pay to-pay-stake)
                (update-pool-data-unstake pool-id to-pay to-pay-stake))
              (format "Unstaked for account: {} amount: {} token: {}" [account to-pay-stake stake-token])
            )
          )
        )
      )
      
      
          ;;///////////////////////
          ;;UTILITIES
          ;;//////////////////////
      
          (defun calculate-multiplier (pool-id:string)
            @doc " Calculates a pools current multiplier "
            (let*
                (
                  (pool-data (read pools pool-id ["reward-amount" "reward-duration" "end-time"]))
                  (pool-usage-data (read pools-usage pool-id ["last-updated" "multiplier" "tokens-locked"]))
                  (days-since-last-update (floor (/ (diff-time  (at "block-time" (chain-data)) (at "last-updated" (read pools-usage pool-id))) (at "reward-duration" pool-data)) 0)  )
                  (days-since-end-time (floor (/ (diff-time (at "block-time" (chain-data))  (at "end-time" (read pools pool-id))) (at "reward-duration" pool-data)) 0)  )
                  (tokenslocked (at "tokens-locked" (read pools-usage pool-id)))
                  (days-since (- days-since-last-update days-since-end-time) )
                  (multiplier (if (> tokenslocked 0.0) (+ (at "multiplier" pool-usage-data) (/ (* (min days-since-last-update (abs days-since)) (at "reward-amount" pool-data) ) (at "tokens-locked" (read pools-usage pool-id))  ) ) 0.0))
                )
               multiplier
      
            )
          )
      
          (defun calculate-rewards (pool-id:string account:string)
            @doc " Calculates a users rewards in a pool "
            (let*
                    (
                        (pool-data (read pools pool-id ["reward-token" "balance"]))
                        (stake-id (get-stake-id-key account pool-id))
                        (stake (read stakes stake-id ["balance" "multiplier"]))
                        (pool-balance (at "balance" pool-data))
                        (rewards-max (min (floor (* (at "balance" stake) (- (calculate-multiplier pool-id) (at "multiplier" stake) ) ) (heron.precision)) pool-balance)  )
                        (rewards (if (> rewards-max 0.0) rewards-max 0.0) )
      
                    )
                          rewards
                 )
          )
      
          (defun calculate-apy (pool-id:string)
              @doc " Calculates a pools current apy "
              (let*
                  (
                      (pool-data (read pools pool-id ["reward-duration" "reward-amount" "reward-token" "start-time" "start-balance"]))
                      (pool-usage-data (read pools-usage pool-id ["tokens-locked" "paid"]))
                      (time-passed (diff-time (add-time (at "block-time" (chain-data)) (days 365) )  (at "start-time" pool-data) ) )
                      (total-available (floor (* (/ time-passed (at "reward-duration" pool-data)) (at "reward-amount" pool-data)) (heron.precision) ))
                      (max-available (at "start-balance" pool-data) )
                  )
      
                          (if (<= total-available max-available )
                            (if (> (at "tokens-locked" pool-usage-data) 0.0)
                              (/ (* 100 (- total-available (at "paid" pool-usage-data) ) ) (at "tokens-locked" pool-usage-data) )
                              (/ (* 100 (- total-available (at "paid" pool-usage-data) ) ) 100) )
                            (if (> (at "tokens-locked" pool-usage-data) 0.0)
                              (/ (* 100 (- max-available (at "paid" pool-usage-data) ) ) (at "tokens-locked" pool-usage-data) )
                              (/ (* 100 (- max-available (at "paid" pool-usage-data) ) ) 100) )
      
                  )
              )
          )
      
          (defun calculate-total-emitted-tokens (pool-id:string)
              @doc " Calculates the current number of tokens emitted by a pool "
              (let*
                  (
                      (pool-data (read pools pool-id ["reward-duration" "reward-amount" "reward-token" "start-time" "start-balance"]))
                      (time-passed (diff-time  (at "block-time" (chain-data)) (at "start-time" pool-data) ) )
                      (total-available (floor (* (/ time-passed (at "reward-duration" pool-data)) (at "reward-amount" pool-data)) (heron.precision) ))
                      (max-available (at "start-balance" pool-data))
                  )
                         (if (<= total-available max-available ) total-available max-available)
              )
      
          )
      
          (defun calculate-pool-end-time (pool-id:string do-start-balance:bool do-add-balance:bool amount-to-add:decimal)
              @doc " Calculates the time at which a pool has emitted all rewards "
               (let*
                      (
                          (pool-data (read pools pool-id ["reward-duration" "reward-amount" "start-balance" "balance" "end-time"]))
                          (number-of-reward-durations-add-balance (/ amount-to-add (at "reward-amount" pool-data) ))
                          (number-of-reward-durations-balance-end-time (/ (at "balance" pool-data) (at "reward-amount" pool-data) ))
                          (number-of-reward-durations-end-time (/ (at "start-balance" pool-data) (at "reward-amount" pool-data) ))
                          (number-of-reward-durations (if (= do-add-balance true)
                                                            number-of-reward-durations-add-balance
                                                            (if (= do-start-balance true)
                                                              number-of-reward-durations-end-time
                                                              number-of-reward-durations-balance-end-time
                                                            )
                                                          )
                              )
                              (time-until-exhausted (* (at "reward-duration" pool-data) number-of-reward-durations) )
                              (end-time-add-balance (add-time (at "end-time" pool-data) time-until-exhausted))
                              (end-time-other (add-time (at "block-time" (chain-data)) time-until-exhausted))
                              (end-time (if (= do-add-balance true)
                                                                  end-time-add-balance
                                                                  (if (= do-start-balance true)
                                                                    end-time-other
                                                                    end-time-other
                                                                  )
                                                                )
                                        )
                                  )
                                end-time
      
                            )
          )
      
      
      
      
          ;///////////////////////////
          ;MORE UTILITIES
          ;//////////////////////////
      
      
          (defun get-stake-id-key ( account:string pool-id:string )
            @doc " Returns id/account data structure "
            (format "{}:{}" [account pool-id])
          )
      
          (defun enforce-pool-id ( id:string )
            @doc " Enforces a unique pool-id "
            (with-default-read pools id
                { 'id: 'nonulls }
                { 'id := id }
                (enforce (= 'nonulls id) "This ID already exists.")
            )
          )
      
          (defun get-pools ()
            (keys pools)
          )
      
          (defun get-pool-info (pool-id:string)
            (+ (+ (read pools pool-id) (read pools-usage pool-id)) { "apy" : (calculate-apy pool-id) } )
          )
      
          (defun get-user-stakes (pool-id:string account:string)
            (read stakes (get-stake-id-key account pool-id))
          )
      
          (defun get-pool-info-obj (pool-id:object{stakes-schema})
            (bind pool-id {
                            "pool-id" := id,
                            "balance" := stake_balance,
                            "last-updated" := stake_last-updated,
                            "rewards" := stake_rewards,
                            "last-claimed" := stake_last-claimed,
                            "last-withdraw" := stake_last-withdraw,
                            "multiplier" := stake_multiplier,
                            "start-time" := stake_start-time,
                            "account" := stake_account
                          }
                           (+
                             (+
                               (read pools id)
                               (read pools-usage id)
                              )
                              {
                                "stake_balance" : stake_balance,
                                "stake_last-updated" : stake_last-updated,
                                "stake_rewards" : stake_rewards,
                                "stake_last-claimed" : stake_last-claimed,
                                "stake_last-withdraw" : stake_last-withdraw,
                                "stake_multiplier" : stake_multiplier,
                                "stake_start-time" : stake_start-time,
                                "apy" : (calculate-apy id),
                                "stake_pending_rewards" : (calculate-rewards id stake_account)
                                }
                            )
            )
          )
      
          (defun get-pool-info-obj2 (pool-id:object{pools-schema})
            (bind pool-id { "id" := id }
            (+ (+ (read pools id) (read pools-usage id)) { "apy" : (calculate-apy id) } )
            )
          )
      
          (defun get-all-user-pools ( account:string )
            @doc " Get a detailed list of pools a user is staking in "
            (let ((x (select stakes ['pool-id 'balance 'last-updated 'rewards 'last-claimed 'last-withdraw 'account 'multiplier 'start-time]
                (and? (where 'account (= account))
                  (where 'balance (< 0.0))))))
                  (map (get-pool-info-obj) x ))
          )
      
          (defun get-all-pools ( )
            @doc " Get a detailed list of pools a user is staking "
            (let ((x (select pools ['id] (constantly true))))
                  (map (get-pool-info-obj2) x ))
          )
      
          (defun get-user-created-pools ( account:string )
            @doc " Get a list of pool IDs that a user has created "
              (select pools (where 'account (= account)))
          )
      
        ;; Enforce Functions
      
      
        (defun enforce-active-pool (pool-id:string pool-data:object)
          @doc "Enfore that a pool is active and has a balance"
          (enforce (and (= (at "active" pool-data) true) (> (at "balance" pool-data) 0.0))
                 (format "The pool {} is currently deactivated and not accepting stakers" [pool-id])
          )
        )
      
        (defun update-pool-usage-multiplier (pool-id:string pool-data:object)
          @doc "Update the pools multiplier if it has stakers"
          (require-capability (USER_POOL_CHECK))
          (if (> (at "stakers" pool-data) 0.0)
            (update pools-usage pool-id
              {
                "last-updated": (at "block-time" (chain-data)),
                "multiplier": (calculate-multiplier pool-id)
              })
            true
          )
        )
      
        (defun update-user-stake-information (stake-id:string t_id:string t_pool-id:string t_balance:decimal t_last-updated:time t_account:string t_rewards:decimal t_last-claimed:time t_last-withdraw:time amount:decimal pool-id:string pool-usage-data:object)
          @doc "Update a users stake information in a pool"
          (require-capability (ACCOUNT_GUARD t_account))
          (write stakes stake-id {
            "id": t_id,
            "pool-id": t_pool-id,
            "balance": (+ t_balance amount),
            "last-updated": (at "block-time" (chain-data)),
            "account": t_account,
            "rewards": t_rewards,
            "last-claimed": t_last-claimed,
            "last-withdraw": t_last-withdraw,
            "multiplier": (if (> (at "stakers" (read pools pool-id)) 0.0)
                            (calculate-multiplier pool-id)
                            (at "multiplier" pool-usage-data)),
            "start-time": (at "block-time" (chain-data))
          }
          )
        )
      
        (defun initialize-pool-start-end-time (pool-id:string pool-data:object)
          @doc "Initialize a pools start and end time if it no current stakers or is not initialized"
          (require-capability (USER_POOL_CHECK))
          (if (or (= (at "initialized" pool-data) false) (= (at "stakers" (read pools pool-id)) 0.0))
            (update pools pool-id
              {
                "initialized": true,
                "start-time": (if (> (at "stakers" pool-data) 0.0)
                                (at "start-time" pool-data)
                                (at "block-time" (chain-data))),
                "end-time": (if (= (at "stakers" (read pools pool-id)) 0.0)
                              (calculate-pool-end-time pool-id false false 0.0)
                              (calculate-pool-end-time pool-id true false 0.0))
              })
            true
          )
        )
      
        ;; Claim Check Helper Functions
      
        (defun pay-user-rewards (account:string to-pay:decimal)
        @doc "Pays the user their owed rewards"
          (require-capability (USER_CLAIM_CHECK))
          (install-capability (heron.TRANSFER (get-account-principal) account to-pay))
          (with-capability (PRIVATE_RESERVE)
            (heron.transfer-create (get-account-principal) account (at "guard" (coin.details account)) to-pay)
          )
        )
      
      (defun update-pool-usage-data (pool-id:string to-pay:decimal )
        @doc "Updates the pool's usage data after paying rewards"
          (require-capability (USER_POOL_CHECK))
          (let ((pool-usage-data (read pools-usage pool-id)))
            (update pools-usage pool-id
              {
                "last-updated": (at "block-time" (chain-data)),
                "paid": (+ (at "paid" pool-usage-data) to-pay),
                "multiplier": (calculate-multiplier pool-id)
              })
          )
      )
      
      (defun update-pool-data (pool-id:string to-pay:decimal to-pay-stake:decimal)
        @doc "Updates the pool's balance and active status after paying rewards"
        (require-capability (USER_POOL_CHECK))
        (let ((pool-data (read pools pool-id)))
          (update pools pool-id
            {
              "balance": (- (at "balance" pool-data) to-pay),
              "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data)),
              "stakers": (if (> to-pay-stake 0.0) (- (at "stakers" pool-data) 1.0) (at "stakers" pool-data) )
            }
          )
        )
      )
      
      (defun claim-update-pool-data (pool-id:string to-pay:decimal)
        @doc "Updates the pool's balance and active status after paying rewards"
        (require-capability (USER_POOL_CHECK))
        (let ((pool-data (read pools pool-id)))
           (update pools pool-id
            {
                "balance": (- (at "balance" pool-data) to-pay),
                "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data) )
            }
          )
        )
      )
      
      
      (defun update-user-stake-data (stake-id:string to-pay:decimal)
        @doc "Updates the user's stake data after claiming rewards"
        (require-capability (USER_CLAIM_CHECK))
        (let ((stake (read stakes stake-id)))
          (update stakes stake-id
            {
              "last-updated": (at "block-time" (chain-data)),
              "rewards": (+ (at "rewards" stake) to-pay),
              "last-claimed": (at "block-time" (chain-data)),
              "multiplier": (at "multiplier" (read pools-usage (at "pool-id" stake)))
            }
          )
        )
      )
      
      ;; Unstake Helper Functions
      
      
      
      (defun update-pool-usage-data-unstake (pool-id:string to-pay:decimal to-pay-stake:decimal)
        @doc "Updates the pool's usage data after unstaking"
        (require-capability (USER_POOL_CHECK))
        (let ((pool-usage-data (read pools-usage pool-id)))
          (update pools-usage pool-id
            {
              "last-updated": (at "block-time" (chain-data)),
              "tokens-locked": (- (at "tokens-locked" pool-usage-data) to-pay-stake),
              "paid": (+ (at "paid" pool-usage-data) to-pay),
              "multiplier": (calculate-multiplier pool-id)
            })))
      
      (defun update-pool-data-unstake (pool-id:string to-pay:decimal to-pay-stake:decimal)
        @doc "Updates the pool's data after unstaking"
        (require-capability (USER_POOL_CHECK))
        (let ((pool-data (read pools pool-id)))
          (update pools pool-id
            {
              "balance": (- (at "balance" pool-data) to-pay),
              "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data)),
              "stakers": (if (> to-pay-stake 0.0) (- (at "stakers" pool-data) 1.0) (at "stakers" pool-data))
            })))
      
      (defun enforce-withdraw-duration (pool-data:object stake:object)
        @doc "Enforces the withdraw duration for unstaking"
        (if (= (at "active" pool-data) true)
          (enforce (>= (diff-time (at "block-time" (chain-data)) (at 'last-withdraw stake)) (at "withdraw-duration" pool-data))
                   "You must wait the full withdraw wait duration before claiming your stake.")
          true
        )
      )
      
      (defun pay-user-stake (account:string to-pay-stake:decimal)
        @doc "Pays the user their staked tokens"
        (require-capability (USER_CLAIM_CHECK))
        (install-capability (heronbond.TRANSFER (get-account-principal) account to-pay-stake))
        (with-capability (PRIVATE_RESERVE)
          (heronbond.transfer (get-account-principal) account to-pay-stake)
        )
      )
      
      (defun update-user-stake-stats (stake-id:string to-pay-stake:decimal)
        @doc "Updates the user's stake stats after unstaking"
        (require-capability (USER_CLAIM_CHECK))
        (update stakes stake-id
          {
            "last-updated": (at "block-time" (chain-data)),
            "balance": (- (at "balance" (read stakes stake-id)) to-pay-stake),
            "last-withdraw": (at "block-time" (chain-data))
          }
        )
      )
      
      
      
         (defun enforce-valid-id ( id:string )
          @doc " Enforce that an account ID meets charset and length requirements. "
          (enforce
            (is-charset ACCOUNT_ID_CHARSET id)
            (format
              "Account ID does not conform to the required charset: {}"
              [id]))
          (let ((accountLength (length id)))
            (enforce
              (>= accountLength ACCOUNT_ID_MIN_LENGTH)
              (format
                "Account ID does not conform to the min length requirement: {}"
                [id]))
            (enforce
              (<= accountLength ACCOUNT_ID_MAX_LENGTH)
              (format
                "Account ID does not conform to the max length requirement: {}"
                [id]))))
      
        (defun enforce-valid-name ( name:string )
          @doc " Enforce that a Pool Name meets charset and length requirements. "
          (enforce
            (is-charset ACCOUNT_ID_CHARSET name)
            (format
              "Pool Name does not conform to the required charset: {}"
              [name]))
          (let ((nameLength (length name)))
            (enforce
              (>= nameLength NAME_MIN_LENGTH)
              (format
                "Pool Name does not conform to the min length requirement: {}"
                [name]))
            (enforce
              (<= nameLength NAME_MAX_LENGTH)
              (format
                "Pool Name does not conform to the max length requirement: {}"
                [name]))))
      
      
        (defun min
          ( num1
            num2
          )
          (if (is-min num1 num2) num1 num2)
        )
      
        (defun is-min
          ( num1
            num2
          )
          (< num1 num2)
        )
      
        (defun get-token-key
          ( tokenA:module{fungible-v2} )
          " Create key from token module"
          (format "{}" [tokenA])
        )
      
      )
      
      
