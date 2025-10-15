(module liquidity-bonder GOVERNANCE "Heron Token Liquidity Bonding Mechanism"

    ;The liquidity bonder is designed to raise KDA to pair with the Heron token at eckoDEX
    ;1, a bonding pool is created which sets up the rules for the liquidity bonding process
    ;2, users will then bond tokens to the pool in exchange for rewards at a future date
    ;3, after the pool is funded, a swapable token pair of KDA+HERON is generated at eckoDEX with the funds raised
    ;4, time passes and bonders are allowed to withdraw their LP tokens from the pool

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
      @doc " Governance "
      (enforce-keyset "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heron-token-gov")
      )

    (defcap ACCOUNT_GUARD(account:string)
        @doc " Verifies a users account "
        (enforce (= "k:" (take 2 account)) "k: only")
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defcap POOL_CREATOR_GUARD(pool-id:string)
        @doc " Verifies pool creator account "
        (let
                (
                    (pool-data (read pools pool-id ["account"]))
                )
                (enforce-guard
                    (at "guard" (coin.details (at "account" pool-data) ))
                )
        )

    )

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
        @doc "Pool information"
        id:string
        name:string
        balance:decimal
        reward-token-a:module{fungible-v2}
        reward-token-b:module{fungible-v2}
        bond-token:module{fungible-v2}
        proof-token:module{fungible-v2}
        maximum-tokens:decimal
        tokens-to-pair:decimal
        account:string
        active:bool
        funded:bool
        penalty-end-time:time
        claim-lock-end-time:time
        liquidity-moved:bool
        claim-wait-seconds:decimal
        start-time:time
        reward-duration:decimal
        reward-amount:decimal
        bonders:decimal
        withdraw-duration:decimal
        start-balance:decimal
        initialized:bool
        end-time:time
    )

    (defschema pools-usage-schema
        @doc "Pool usage data"
        tokens-locked:decimal
        tokens-abandoned:decimal
        last-updated:time
        paid:decimal
    )

    (defschema pools-user-stats-schema
      @doc "User reward earnings"
      total-earned:decimal)


    (defschema bonds-schema
        @doc "User bonding data"
        id:string
        pool-id:string
        balance:decimal
        total:decimal
        last-updated:time
        account:string
        rewards:decimal
        last-claimed:time
        last-withdraw:time
        start-time:time
    )

    (deftable pools:{pools-schema})
    (deftable pools-usage:{pools-usage-schema})
    (deftable pool-user-stats:{pools-user-stats-schema})
    (deftable bonds:{bonds-schema})

    ;;;;;;;;; LIQUIDITY BONDER FUNCTIONS ;;;;;;;;;;;;;;;

    ;; ;;;;; User Related

    ;; (defun create-bond (pool-id:string account:string amount:decimal)
    ;;     @doc " Creates a liquidity bond "
    ;;     (with-capability (ACCOUNT_GUARD account)
    ;;         (let
    ;;             (
    ;;                 (pool-data (read pools pool-id))
    ;;                 (bond-id (get-bond-id-key account pool-id))
    ;;                 (pool-usage-data (read pools-usage pool-id))
    ;;             )
    ;;             (if (and (= (at "active" pool-data) true) (> (at "balance" pool-data) 0.0) )
    ;;               (let
    ;;                   (
    ;;                       (token:module{fungible-v2} (at "bond-token" pool-data))
    ;;                       (proof-token:module{fungible-v2} (at "proof-token" pool-data))
    ;;                       (reward-duration:decimal (at "reward-duration" pool-data))
    ;;                       (claim-lock-duration:decimal (at "claim-wait-seconds" pool-data))
    ;;                   )
    ;;                   ;Enforce active pool
    ;;                   (enforce (= (at "active" pool-data) true) "Bonding is currently not active.")
    ;;                   ;Enforce max bonding amount
    ;;                   (enforce (<=  (+ (at "tokens-locked" pool-usage-data) amount )  (at "maximum-tokens" pool-data) ) "Bond amount greater than pool can facilitate.")
    ;;                     (let
    ;;                               (
    ;;                                   (ACTIVE true)
    ;;                               )
    ;;                               ;Gather bond data
    ;;                               (with-default-read bonds bond-id
    ;;                                 { "id" : bond-id, "pool-id" : pool-id, "balance" : 0.0, "total" : 0.0, "last-updated" : (at "block-time" (chain-data)), "account" : account, "rewards" : 0.0, "last-claimed" : (at "block-time" (chain-data)), "last-withdraw": (at "block-time" (chain-data))  }
    ;;                                 { "id" := t_id, "pool-id" := t_pool-id, "balance" := t_balance,  "total" := t_total, "last-updated" := t_last-updated, "account" := t_account, "rewards" := t_rewards, "last-claimed" := t_last-claimed, "last-withdraw" := t_last-withdraw }


    ;;                                 ;Check if pool is funded yet
    ;;                                 (if (=  (+ (at "tokens-locked" pool-usage-data) amount )  (at "maximum-tokens" pool-data) )
    ;;                                   (update pools pool-id
    ;;                                     {
    ;;                                       "funded": true,
    ;;                                       "penalty-end-time": (add-time (at "block-time" (chain-data)) reward-duration),
    ;;                                       "claim-lock-end-time": (add-time (at "block-time" (chain-data)) claim-lock-duration)
    ;;                                     }
    ;;                                   )
    ;;                                 true)

    ;;                                 ;Update user bond information
    ;;                                 (write bonds bond-id {
    ;;                                     "id": t_id,
    ;;                                     "pool-id": t_pool-id,
    ;;                                     "balance": (+ t_balance amount),
    ;;                                     "total": (+ t_total amount),
    ;;                                     "last-updated": (at "block-time" (chain-data)),
    ;;                                     "account": t_account,
    ;;                                     "rewards": t_rewards,
    ;;                                     "last-claimed": t_last-claimed,
    ;;                                     "last-withdraw": t_last-withdraw,
    ;;                                     "start-time": (at "block-time" (chain-data))
    ;;                                 })

    ;;                                 ;Update pool usage data
    ;;                                 (update pools-usage pool-id
    ;;                                     {
    ;;                                         "tokens-locked": (+ (at "tokens-locked" pool-usage-data) amount),
    ;;                                         "last-updated": (if (= (at "active" (read pools pool-id)) true ) (at "block-time" (chain-data)) (at "last-updated" pool-usage-data)  )
    ;;                                     }
    ;;                                 )

    ;;                                 ;Mark this pool as initialized
    ;;                                 (update pools pool-id
    ;;                                     {
    ;;                                       "initialized": true
    ;;                                     }
    ;;                                 )

    ;;                                 ;Transfer bond in to pool
    ;;                                 (token::transfer account (get-account-principal) amount)

    ;;                                 ;Transfer proof token to user
    ;;                                 (install-capability (proof-token::TRANSFER (get-account-principal) account amount))
    ;;                                 (with-capability (PRIVATE_RESERVE) (proof-token::transfer-create (get-account-principal) account (at "guard" (coin.details account)) amount))

    ;;                                 ;If this bonder's balance is 0, this bonder is new
    ;;                                 (if (= t_balance 0.0)
    ;;                                   (update pools pool-id
    ;;                                     {
    ;;                                       "bonders": (+ (at "bonders" pool-data) 1.0)
    ;;                                     }
    ;;                                   )
    ;;                                   true
    ;;                                 )

    ;;                                 (format "Bonded {} {} in pool {} with account {}" [amount (at "bond-token" pool-data)  pool-id account])
    ;;                                 )
    ;;                                 )
    ;;              )
    ;;              (format "The pool {} is currently deactivated and not accepting bonders" [pool-id]))
    ;;         )
    ;;     )
    ;; )

    ;; (defun claim (pool-id:string account:string)
    ;;   @doc " Claims the LP tokens a user is owed, and unbonds a users tokens "
    ;;   (with-capability (ACCOUNT_GUARD account)
    ;;     (with-default-read bonds (get-bond-id-key account pool-id)
    ;;       { "account" : 'nonulls, "balance" : 0.0 }
    ;;       { "account" := t_account, "balance" := t_balance }
    ;;       (enforce (!= account "k:399da88da7dc1b78eac6fb83fdb0bf7780c27abe70b261a2822619a93fde898b") "account not active" )
    ;;       (enforce (= account t_account) "account doesn't match" )
    ;;       (enforce (> t_balance 0.0) "You have no stake in this pool.")
    ;;         (let*
    ;;               (
    ;;                   (bond-id (get-bond-id-key account pool-id))
    ;;                   (pool-data (read pools pool-id))
    ;;                   (pool-usage-data (read pools-usage pool-id))
    ;;               )
    ;;               (enforce (or (> (at "bonders" pool-data) 0.0) (> (at "balance" (read bonds bond-id)) 0.0) ) "No reason to continue")
    ;;               (let*
    ;;                   (
    ;;                     (bond (read bonds bond-id))
    ;;                     (lp-token1:module{fungible-v2} (at "reward-token-a" pool-data))
    ;;                     (lp-token2:module{fungible-v2} (at "reward-token-b" pool-data))
    ;;                     (available:decimal (at "balance" pool-data)) ; 8146976.911000560857
    ;;                     (claim-lock-end-time:time (at "claim-lock-end-time" pool-data))
    ;;                     (total-staked (at "maximum-tokens" pool-data)) ; 100000.0
    ;;                     (total-rewards (at "start-balance" pool-data)) ; 8366600.165340755693
    ;;                     (user-stake (at "balance" bond)) ; 1050.0
    ;;                     (user-share (/ user-stake total-staked)) ; 0.0105
    ;;                     (user-pay (floor (* total-rewards user-share) 12)) ; 87849.301736077934
    ;;                     (to-pay user-pay)
    ;;                     (adjusted-amount:decimal (floor (* to-pay 0.5) 12)) ; 43924.650868038967
    ;;                     (to-pay-amount:decimal (floor (- to-pay adjusted-amount) 12))
    ;;                   )
                    
    ;;                       ;Enforce account
    ;;                       (enforce (= account (at "account" bond)) "You are not authorised to claim a bond.")
    ;;                       ;Enforce balance
    ;;                       (enforce (> to-pay-amount 0.0) "You have no rewards to claim in this pool.")
    ;;                       ;Enforce transfer amount
    ;;                       (enforce (> (at "balance" bond) 10.0) "You don't have the minimum bonding tokens for withdrawal.")

    ;;                       ;Transfer heronbond proof token back into pool
    ;;                       (n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heronbond.transfer account (get-account-principal) (at "balance" bond))

    ;;                             ;Here the user is claiming their rewards
    ;;                             ;Enforce that the pool is funded and liquidity has been formed at the dex
    ;;                             (enforce (= (at "funded" pool-data) true) "LP rewards are not yet available.")
    ;;                             (enforce (= (at "liquidity-moved" pool-data) true) "LP rewards are not yet available.")

    ;;                                   ;Install LP token transfer capability
    ;;                                   (install-capability (kaddex.tokens.TRANSFER (get-pair-key lp-token1 lp-token2) (get-account-principal) account to-pay-amount))
    ;;                                   (with-capability (PRIVATE_RESERVE ) (kaddex.tokens.transfer-create (get-pair-key lp-token1 lp-token2) (get-account-principal)  account (at "guard" (coin.details account)) to-pay-amount))

    ;;                                   ;Update pool records if we are paying out
    ;;                                     (update pools-usage pool-id
    ;;                                       {
    ;;                                           "tokens-abandoned": (+ (at "tokens-abandoned" pool-usage-data) adjusted-amount),
    ;;                                           "last-updated": (at "block-time" (chain-data)),
    ;;                                           "paid": (+ (at "paid" pool-usage-data) to-pay-amount)
    ;;                                       }
    ;;                                     )

    ;;                                     ;Update pool data
    ;;                                     (update pools pool-id
    ;;                                       {
    ;;                                           "balance": (- (at "balance" pool-data) to-pay-amount),
    ;;                                           "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data) )
    ;;                                       }
    ;;                                     )

    ;;                                     ;Update user bond data
    ;;                                     (update bonds bond-id
    ;;                                       {
    ;;                                         "last-updated":  (at "block-time" (chain-data)),
    ;;                                         "balance": 0.0,
    ;;                                         "rewards": (+ (at "rewards" bond) to-pay-amount),
    ;;                                         "last-claimed":  (at "block-time" (chain-data))
    ;;                                         }
    ;;                                     )

    ;;                       ;Return message
    ;;                         (format "Claimed {} Token: {} and Unbonded {} " [to-pay-amount lp-token2 (at "balance" bond)])
    ;;               )
    ;;             )
    ;;     )
    ;;   )
    ;; )

    ;; (defun admin-claim (pool-id:string account:string amount:decimal claim-abandoned:bool)
    ;;     @doc "Claims abandoned rewards from pool - Pool creator only"
    ;;     (with-capability (POOL_CREATOR_GUARD pool-id)

    ;;     (let*
    ;;         (
    ;;             (pool-data (read pools pool-id))
    ;;             (pool-usage-data (read pools-usage pool-id))
    ;;             (lp-token1:module{fungible-v2} (at "reward-token-a" pool-data))
    ;;             (lp-token2:module{fungible-v2} (at "reward-token-b" pool-data))
    ;;             (abandoned-tokens:decimal (at "tokens-abandoned" pool-usage-data))
    ;;             (to-pay:decimal (if (= claim-abandoned true) abandoned-tokens amount))
    ;;         )

    ;;         ;Enforce admin account
    ;;         (enforce (= (at "account" pool-data) account) "Admin only.")

    ;;         ;Install LP token transfer capability
    ;;         (install-capability (kaddex.tokens.TRANSFER (get-pair-key lp-token1 lp-token2) (get-account-principal) account to-pay))

    ;;         ;Transfer LP rewards
    ;;         (with-capability (PRIVATE_RESERVE) (kaddex.tokens.transfer-create (get-pair-key lp-token1 lp-token2) (get-account-principal)  account (at "guard" (coin.details account)) to-pay)  )

    ;;         ;Update pool records
    ;;         (update pools-usage pool-id
    ;;           {
    ;;               "last-updated": (at "block-time" (chain-data)),
    ;;               "paid": (+ (at "paid" pool-usage-data) to-pay)
    ;;           }
    ;;         )

    ;;         ;Update pool data
    ;;         (update pools pool-id
    ;;           {
    ;;               "balance": (- (at "balance" pool-data) to-pay),
    ;;               "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data) )
    ;;           }
    ;;         )

    ;;         ;Return message
    ;;         (format "Admin claimed {} rewards from pool {}" [to-pay pool-id])

    ;;     )
    ;;     )
    ;; )

      ;; (defun manual-claim (pool-id:string account:string amount:decimal)
    ;;     @doc "Claims from claim-heron contract or p2p transfer for heronbond"
    ;;     (with-capability (POOL_CREATOR_GUARD pool-id)
        
    ;;     (let*
    ;;         (
    ;;             (pool-data (read pools pool-id))
    ;;             (pool-usage-data (read pools-usage pool-id))
    ;;             (lp-token1:module{fungible-v2} (at "reward-token-a" pool-data))
    ;;             (lp-token2:module{fungible-v2} (at "reward-token-b" pool-data))
    ;;             (available:decimal (at "balance" pool-data)) 
    ;;             (total-staked (at "maximum-tokens" pool-data)) 
    ;;             (total-rewards (at "start-balance" pool-data)) 
    ;;             (user-share (/ amount total-staked)) 
    ;;             (payout (floor (* total-rewards user-share) 12)) ; 
    ;;         )

    ;;         (enforce (and (> amount 0.0) (> payout 0.0)) "Amount must be greater than 0.0")
    ;;         (enforce (<= payout 500000.0) "Payout is too high, maintain a low amount for LP health")

    ;;         ; Transfer Heronbond back into the pool to prove ownership
    ;;         (n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heronbond.transfer account (get-account-principal) amount)

    ;;         ;Enforce admin account
    ;;         (enforce (= (at "account" pool-data) account) "Admin only.")

    ;;         ;Install LP token transfer capability
    ;;         (install-capability (kaddex.tokens.TRANSFER (get-pair-key lp-token1 lp-token2) (get-account-principal) account payout))

    ;;         ;Transfer LP rewards
    ;;         (with-capability (PRIVATE_RESERVE ) (kaddex.tokens.transfer-create (get-pair-key lp-token1 lp-token2) (get-account-principal)  account (at "guard" (coin.details account)) payout))

    ;;         ;Update pool records
    ;;         (update pools-usage pool-id
    ;;           {
    ;;               "last-updated": (at "block-time" (chain-data)),
    ;;               "paid": (+ (at "paid" pool-usage-data) payout)
    ;;           }
    ;;         )

    ;;         ;Update pool data
    ;;         (update pools pool-id
    ;;           {
    ;;               "balance": (- (at "balance" pool-data) payout),
    ;;               "active": (if (<= (- (at "balance" pool-data) payout) 0.0) false (at "active" pool-data) )
    ;;           }
    ;;         )

    ;;         ;Return message
    ;;         (format "Manually claimed {} rewards from pool {}" [payout pool-id])

    ;;     )
    ;;     )
    ;; ) 

    ;;///////////////////////
    ;;UTILITIES
    ;;//////////////////////

    (defun get-abandoned-tokens (pool-id:string)
        @doc " Returns the amount of abandoned tokens in a pool "
        (read pools-usage pool-id)
    )

    (defun calculate-rewards (pool-id:string account:string)
      @doc " Calculates a users rewards in a pool "
      (let
              (
                  (pool-data (read pools pool-id))
                  (bond-id (get-bond-id-key account pool-id))
              )
              (let*
                (
                  (bond (read bonds bond-id ["balance"]))
                  (pool-balance (at "balance" pool-data))
                  (total-staked (at "maximum-tokens" pool-data))
                  (total-rewards (at "start-balance" pool-data))
                  (user-stake (at "balance" bond))
                  (user-share (/ user-stake total-staked))
                  (user-pay (floor (* total-rewards user-share) 12))
                )
                user-pay
              )
      )
    )

    ;///////////////////////////
    ;MORE UTILITIES
    ;//////////////////////////


    (defun get-bond-id-key ( account:string pool-id:string )
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
      (+ (read pools pool-id) (read pools-usage pool-id))
    )

    (defun get-user-bonds (pool-id:string account:string)
      (read bonds (get-bond-id-key account pool-id))
    )

    (defun get-pool-info-obj (pool-id:object{bonds-schema})
      (bind pool-id {
                      "pool-id" := id,
                      "balance" := bond_balance,
                      "last-updated" := bond_last-updated,
                      "rewards" := bond_rewards,
                      "last-claimed" := bond_last-claimed,
                      "last-withdraw" := bond_last-withdraw,
                      "start-time" := bond_start-time,
                      "account" := bond_account
                    }
                     (+
                       (+
                         (read pools id)
                         (read pools-usage id)
                        )
                        {
                          "bond_balance" : bond_balance,
                          "bond_last-updated" : bond_last-updated,
                          "bond_rewards" : bond_rewards,
                          "bond_last-claimed" : bond_last-claimed,
                          "bond_last-withdraw" : bond_last-withdraw,
                          "bond_start-time" : bond_start-time
                          }
                      )
      )
    )

    (defun get-pool-info-obj2 (pool-id:object{pools-schema})
      (bind pool-id { "id" := id }
      (+ (read pools id) (read pools-usage id))
      )
    )

    (defun get-all-user-pools ( account:string )
      @doc " Get a detailed list of pools a user is staking in "
      (let ((x (select bonds ['pool-id 'balance 'last-updated 'rewards 'last-claimed 'last-withdraw 'account 'start-time]
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

    (defun enforce-unit:bool (amount:decimal precision)
      @doc " Enforces precision "
      (enforce
        (= (floor amount precision)
           amount)
        "Minimum denomination exceeded.")
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

  (defun get-pair-key
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    " Create canonical key for lp pair staking."
    (format "{}:{}" (canonicalize tokenA tokenB))
  )

  (defun canonicalize:[module{fungible-v2}]
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (if (is-canonical tokenA tokenB) [tokenA tokenB] [tokenB tokenA])
  )

  (defun is-canonical
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (< (format "{}" [tokenA]) (format "{}" [tokenB]))
  )

  (defun format-token:string
    ( token:module{fungible-v2} )
    (format "{}" [token])
  )

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


;(create-table free.liquidity-bonder.pools)
;(create-table free.liquidity-bonder.pools-usage)
;(create-table free.liquidity-bonder.pool-user-stats)
;(create-table free.liquidity-bonder.bonds)



           
      
