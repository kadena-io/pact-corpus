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



       (defun manual-claim (pool-id:string account:string amount:decimal)
        @doc "Claims from claim-heron contract or p2p transfer for heronbond"
        (with-capability (POOL_CREATOR_GUARD pool-id)
        
        (let*
            (
                (pool-data (read pools pool-id))
                (pool-usage-data (read pools-usage pool-id))
                (lp-token1:module{fungible-v2} (at "reward-token-a" pool-data))
                (lp-token2:module{fungible-v2} (at "reward-token-b" pool-data))
                (available:decimal (at "balance" pool-data)) 
                (total-staked (at "maximum-tokens" pool-data)) 
                (total-rewards (at "start-balance" pool-data)) 
                (user-share (/ amount total-staked)) 
                (payout (floor (* total-rewards user-share) 12)) ; 
            )

            (enforce (and (> amount 0.0) (> payout 0.0)) "Amount must be greater than 0.0")
            (enforce (<= payout 500000.0) "Payout is too high, maintain a low amount for LP health")

            ; Transfer Heronbond back into the pool to prove ownership
            (n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heronbond.transfer account (get-account-principal) amount)

            ;Enforce admin account
            (enforce (= (at "account" pool-data) account) "Admin only.")

            ;Install LP token transfer capability
            (install-capability (kaddex.tokens.TRANSFER (get-pair-key lp-token1 lp-token2) (get-account-principal) account payout))

            ;Transfer LP rewards
            (with-capability (PRIVATE_RESERVE ) (kaddex.tokens.transfer-create (get-pair-key lp-token1 lp-token2) (get-account-principal)  account (at "guard" (coin.details account)) payout))

            ;Update pool records
            (update pools-usage pool-id
              {
                  "last-updated": (at "block-time" (chain-data)),
                  "paid": (+ (at "paid" pool-usage-data) payout)
              }
            )

            ;Update pool data
            (update pools pool-id
              {
                  "balance": (- (at "balance" pool-data) payout),
                  "active": (if (<= (- (at "balance" pool-data) payout) 0.0) false (at "active" pool-data) )
              }
            )

            ;Return message
            (format "Manually claimed {} rewards from pool {}" [payout pool-id])

        )
        )
    ) 

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



           
      
