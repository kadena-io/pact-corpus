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

    ;id: id of pool, ex: 'bondng-pool'
    ;name: name of pool, ex: 'Bonding Pool'
    ;balance: total amount of rewards to be distributed by this pool, ex: 8366600.165340755693
    ;reward-token-a: name of fungible-v2 LP pair token 1, ex: coin
    ;reward-token-b: name of fungible-v2 LP pair token 2, ex: HERON
    ;bond-token: name of fungible-v2 token module bonders will bond with, ex: coin
    ;account: pool creator account, ex: 'k:amir'
    ;claim-wait-seconds: minimum number of seconds between a bonder's reward claims, ex: 0 (bonders can claim rewards any time)
    ;reward-duration: number of seconds it takes to distribute reward-amount, ex: 15780000.0 (6 months)
    ;reward-amount: the amount of rewards available each reward-duration, ex: 8366600.165340755693
    ;withdraw-duration: time in seconds a bonder's tokens must be locked before a bonder can unbond, ex 0 (bonders can unbond any time)
    ;maximum-tokens: max amount of KDA tokens this pool will allow bonders to bond, ex: 100000.0
    ;tokens-to-pair: number of tokens to pair with the KDA, ex: 700000000.0

    ;  (defun create-pool (id:string
    ;                      name:string
    ;                      balance:decimal
    ;                      reward-token-a:module{fungible-v2}
    ;                      reward-token-b:module{fungible-v2}
    ;                      bond-token:module{fungible-v2}
    ;                      proof-token:module{fungible-v2}
    ;                      account:string
    ;                      claim-wait-seconds:decimal
    ;                      reward-duration:decimal
    ;                      reward-amount:decimal
    ;                      withdraw-duration:decimal
    ;                      maximum-tokens:decimal
    ;                      tokens-to-pair:decimal )

    ;      @doc " Creates a new liquidity bonding pool "
    ;      (with-capability (ACCOUNT_GUARD account)

    ;          ;Enforce naming rules
    ;          (enforce-pool-id id)
    ;          (enforce-valid-id account)
    ;          (enforce-valid-id id)
    ;          (enforce-valid-name name)

    ;          ;Create liquidity pair at dex
    ;          (kaddex.exchange.create-pair reward-token-a reward-token-b "")

    ;          ;Create vault to hold KDA
    ;          (reward-token-a::create-account (get-account-principal) (create-pool-guard))
    ;          ;Create vault to hold HERON
    ;          (reward-token-b::create-account (get-account-principal) (create-pool-guard))
    ;          ;Create vault to hold HERONBOND
    ;          (proof-token::create-account (get-account-principal) (create-pool-guard))
    ;          ;Transfer heron token into pool
    ;          (reward-token-b::transfer account (get-account-principal) tokens-to-pair)
    ;          ;Transfer heronbond token into pool
    ;          (proof-token::transfer account (get-account-principal) maximum-tokens)
    ;          ;Create LP token account to hold all LP tokens
    ;          (kaddex.tokens.create-account (get-pair-key reward-token-a reward-token-b) (get-account-principal) (create-pool-guard))

    ;          ;Insert pool record
    ;          (insert pools id
    ;              {
    ;                  "id": id,
    ;                  "name": name,
    ;                  "balance": balance,
    ;                  "reward-token-a": reward-token-a,
    ;                  "reward-token-b": reward-token-b,
    ;                  "bond-token": bond-token,
    ;                  "proof-token": proof-token,
    ;                  "account": account,
    ;                  "active": true,
    ;                  "funded": false,
    ;                  "penalty-end-time": (at "block-time" (chain-data)),
    ;                  "claim-lock-end-time": (at "block-time" (chain-data)),
    ;                  "liquidity-moved": false,
    ;                  "claim-wait-seconds": claim-wait-seconds,
    ;                  "start-time": (at "block-time" (chain-data)),
    ;                  "reward-duration": reward-duration,
    ;                  "reward-amount": reward-amount,
    ;                  "maximum-tokens": maximum-tokens,
    ;                  "tokens-to-pair": tokens-to-pair,
    ;                  "bonders": 0.0,
    ;                  "withdraw-duration": withdraw-duration,
    ;                  "start-balance": balance,
    ;                  "initialized": false,
    ;                  "end-time": (at "block-time" (chain-data))
    ;              }
    ;          )
    ;          ;Insert pool usage record
    ;          (insert pools-usage id {
    ;              "tokens-locked": 0.0,
    ;              "tokens-abandoned": 0.0,
    ;              "last-updated": (at "block-time" (chain-data)),
    ;              "paid": 0.0
    ;          })
    ;          ;Return a message
    ;          (format "Created liquidity bonding pool {}" [name])
    ;      )
    ;  )


    ;  (defun move-liquidity (pool-id:string)
    ;      @doc " Moves liquidity to dex after liquidity has been funded "
    ;      (let
    ;          (
    ;              (pool-data (read pools pool-id))
    ;              (pool-usage-data (read pools-usage pool-id))
    ;          )
    ;          (if (and (= (at "active" pool-data) true) (> (at "balance" pool-data) 0.0) )
    ;            (let
    ;                (
    ;                    (lp-token1:module{fungible-v2} (at "reward-token-a" pool-data))
    ;                    (lp-token2:module{fungible-v2} (at "reward-token-b" pool-data))
    ;                    (tokens-locked (at "tokens-locked" pool-usage-data))
    ;                    (tokens-to-pair (at "tokens-to-pair" pool-data))
    ;                    (reward-duration:decimal (at "reward-duration" pool-data))
    ;                    (claim-lock-duration:decimal (at "claim-wait-seconds" pool-data))
    ;                )
    ;                (let
    ;                    (
    ;                        (pair (kaddex.exchange.get-pair lp-token1 lp-token2))
    ;                    )
    ;                    (let
    ;                        (
    ;                            (pair-account (at "account" pair))
    ;                        )

    ;                        (enforce (= (at "funded" pool-data) true) "LP has not yet been fully funded.")
    ;                        (enforce (= (at "liquidity-moved" pool-data) false) "LP has already bonded..")

    ;                        ;Move liquidity
    ;                        (install-capability (lp-token1::TRANSFER (get-account-principal) pair-account tokens-locked))
    ;                        (install-capability (lp-token2::TRANSFER (get-account-principal) pair-account tokens-to-pair))

    ;                        (with-capability (PRIVATE_RESERVE)
    ;                            (kaddex.exchange.add-liquidity lp-token1 lp-token2 tokens-locked tokens-to-pair tokens-locked tokens-to-pair (get-account-principal) (get-account-principal) (create-pool-guard))
    ;                        )

    ;                        ;Update pool records
    ;                        (update pools pool-id
    ;                          {
    ;                            "liquidity-moved": true,
    ;                            "penalty-end-time": (add-time (at "block-time" (chain-data)) reward-duration),
    ;                            "claim-lock-end-time": (add-time (at "block-time" (chain-data)) claim-lock-duration)
    ;                          }
    ;                        )
    ;                    )
    ;                 )
    ;              )

    ;              ;Return
    ;              (format "The pool {} is deactivated" [pool-id])
    ;           )
    ;        )
    ;  )


    ;;;;; User Related

    ;  (defun create-bond (pool-id:string account:string amount:decimal)
    ;      @doc " Creates a liquidity bond "
    ;      (with-capability (ACCOUNT_GUARD account)
    ;          (let
    ;              (
    ;                  (pool-data (read pools pool-id))
    ;                  (bond-id (get-bond-id-key account pool-id))
    ;                  (pool-usage-data (read pools-usage pool-id))
    ;              )
    ;              (if (and (= (at "active" pool-data) true) (> (at "balance" pool-data) 0.0) )
    ;                (let
    ;                    (
    ;                        (token:module{fungible-v2} (at "bond-token" pool-data))
    ;                        (proof-token:module{fungible-v2} (at "proof-token" pool-data))
    ;                        (reward-duration:decimal (at "reward-duration" pool-data))
    ;                        (claim-lock-duration:decimal (at "claim-wait-seconds" pool-data))
    ;                    )
    ;                    ;Enforce active pool
    ;                    (enforce (= (at "active" pool-data) true) "Bonding is currently not active.")
    ;                    ;Enforce max bonding amount
    ;                    (enforce (<=  (+ (at "tokens-locked" pool-usage-data) amount )  (at "maximum-tokens" pool-data) ) "Bond amount greater than pool can facilitate.")
    ;                      (let
    ;                                (
    ;                                    (ACTIVE true)
    ;                                )
    ;                                ;Gather bond data
    ;                                (with-default-read bonds bond-id
    ;                                  { "id" : bond-id, "pool-id" : pool-id, "balance" : 0.0, "total" : 0.0, "last-updated" : (at "block-time" (chain-data)), "account" : account, "rewards" : 0.0, "last-claimed" : (at "block-time" (chain-data)), "last-withdraw": (at "block-time" (chain-data))  }
    ;                                  { "id" := t_id, "pool-id" := t_pool-id, "balance" := t_balance,  "total" := t_total, "last-updated" := t_last-updated, "account" := t_account, "rewards" := t_rewards, "last-claimed" := t_last-claimed, "last-withdraw" := t_last-withdraw }


    ;                                  ;Check if pool is funded yet
    ;                                  (if (=  (+ (at "tokens-locked" pool-usage-data) amount )  (at "maximum-tokens" pool-data) )
    ;                                    (update pools pool-id
    ;                                      {
    ;                                        "funded": true,
    ;                                        "penalty-end-time": (add-time (at "block-time" (chain-data)) reward-duration),
    ;                                        "claim-lock-end-time": (add-time (at "block-time" (chain-data)) claim-lock-duration)
    ;                                      }
    ;                                    )
    ;                                  true)

    ;                                  ;Update user bond information
    ;                                  (write bonds bond-id {
    ;                                      "id": t_id,
    ;                                      "pool-id": t_pool-id,
    ;                                      "balance": (+ t_balance amount),
    ;                                      "total": (+ t_total amount),
    ;                                      "last-updated": (at "block-time" (chain-data)),
    ;                                      "account": t_account,
    ;                                      "rewards": t_rewards,
    ;                                      "last-claimed": t_last-claimed,
    ;                                      "last-withdraw": t_last-withdraw,
    ;                                      "start-time": (at "block-time" (chain-data))
    ;                                  })

    ;                                  ;Update pool usage data
    ;                                  (update pools-usage pool-id
    ;                                      {
    ;                                          "tokens-locked": (+ (at "tokens-locked" pool-usage-data) amount),
    ;                                          "last-updated": (if (= (at "active" (read pools pool-id)) true ) (at "block-time" (chain-data)) (at "last-updated" pool-usage-data)  )
    ;                                      }
    ;                                  )

    ;                                  ;Mark this pool as initialized
    ;                                  (update pools pool-id
    ;                                      {
    ;                                        "initialized": true
    ;                                      }
    ;                                  )

    ;                                  ;Transfer bond in to pool
    ;                                  (token::transfer account (get-account-principal) amount)

    ;                                  ;Transfer proof token to user
    ;                                  (install-capability (proof-token::TRANSFER (get-account-principal) account amount))
    ;                                  (with-capability (PRIVATE_RESERVE) (proof-token::transfer-create (get-account-principal) account (at "guard" (coin.details account)) amount))

    ;                                  ;If this bonder's balance is 0, this bonder is new
    ;                                  (if (= t_balance 0.0)
    ;                                    (update pools pool-id
    ;                                      {
    ;                                        "bonders": (+ (at "bonders" pool-data) 1.0)
    ;                                      }
    ;                                    )
    ;                                    true
    ;                                  )

    ;                                  (format "Bonded {} {} in pool {} with account {}" [amount (at "bond-token" pool-data)  pool-id account])
    ;                                  )
    ;                                  )
    ;               )
    ;               (format "The pool {} is currently deactivated and not accepting bonders" [pool-id]))
    ;          )
    ;      )
    ;  )

    ;; (defun claim (pool-id:string account:string unbond:bool)
;;         @doc " Claims the LP tokens a user is owed, or unbonds a users tokens "
;;         (with-capability (ACCOUNT_GUARD account)
;;         (with-default-read bonds (get-bond-id-key account pool-id)
;;           { "account" : 'nonulls, "balance" : 0.0 }
;;           { "account" := t_account, "balance" := t_balance }
;;           (if (and (= account t_account) (> t_balance 0.0) )
;;             (let
;;                   (
;;                       (bond-id (get-bond-id-key account pool-id))
;;                       (pool-data (read pools pool-id))
;;                       (pool-usage-data (read pools-usage pool-id))
;;                   )
;;                   (if (or (> (at "bonders" pool-data) 0.0) (> (at "balance" (read bonds bond-id)) 0.0) )
;;                   (let*
;;                       (
;;                         (bond (read bonds bond-id))
;;                         (lp-token1:module{fungible-v2} (at "reward-token-a" pool-data))
;;                         (lp-token2:module{fungible-v2} (at "reward-token-b" pool-data))
;;                         (bond-token:module{fungible-v2} (at "bond-token" pool-data))
;;                         (proof-token:module{fungible-v2} (at "proof-token" pool-data))
;;                         (available:decimal (at "balance" pool-data))
;;                         (penalty-end-time:time (at "penalty-end-time" pool-data))
;;                         (claim-lock-end-time:time (at "claim-lock-end-time" pool-data))
;;                         (total-staked (at "maximum-tokens" pool-data))
;;                         (total-rewards (at "start-balance" pool-data))
;;                         (user-stake (at "balance" bond))
;;                         (user-share (/ user-stake total-staked))
;;                         (user-pay (floor (* total-rewards user-share) 12))
;;                       )
;;                       (let*
;;                           (
;;                             (to-pay user-pay)
;;                             (to-pay-bond (if (= unbond true)
;;                                                   (at "balance" bond)
;;                                                   0.0
;;                                           )
;;                             )
;;                             (fee-amount:decimal (floor (* to-pay 0.2) 12))
;;                             (to-pay-plus-penalty:decimal (floor (- to-pay fee-amount) 12))
;;                           )
;;                           ;Enforce account
;;                           (enforce (= account (at "account" bond)) "You are not authorised to claim a bond.")
;;                           ;Enforce balance
;;                           (enforce (>= to-pay 0.0) "You have no rewards to claim in this pool.")
;;                           ;Enforce claim reward duration
;;                           ;(if (or (= unbond true) (= (at "active" pool-data) false) ) true (enforce (> (at "block-time" (chain-data)) claim-lock-end-time) "You must wait the full claim reward duration before claiming rewards.") )
;;                           ;Transfer heronbond proof token back into pool
;;                           (proof-token::transfer account (get-account-principal) (at "balance" bond))

;;                           ;If the user is unbonding or claiming rewards we run different sections of code
;;                           (if (= unbond true)
;;                           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;If the user is UNBODNING ONLY the user runs this code:
;;                             (let
;;                                 (
;;                                   (unbonding:bool true)
;;                                 )
;;                                 ;Here the user is unbonding before they can claim rewards
;;                                 ;Enforce that the pool is not funded, and that liquidity has not been formed at the dex
;;                                 (enforce (= (at "funded" pool-data) false) "LP has bonded- please try and see if your rewards are available.")
;;                                 (enforce (= (at "liquidity-moved" pool-data) false) "LP has bonded- please try and see if your rewards are available.")

;;                                 ;Enforce withdraw timer rules
;;                                 (if (and (= unbond true) (= (at "active" pool-data) true) ) (enforce (>= (diff-time (at "block-time" (chain-data)) (at 'last-withdraw bond)) (at "withdraw-duration" pool-data) ) "You must wait the full withdraw wait duration before claiming your bond.") true )

;;                                 ;Install cap & transfer KDA back
;;                                 (install-capability (bond-token::TRANSFER (get-account-principal) account to-pay-bond))
;;                                 (with-capability (PRIVATE_RESERVE) (bond-token::transfer (get-account-principal) account to-pay-bond))

;;                                 ;if a payment was made lets update pool records and state
;;                                 (if (> to-pay-bond 0.0)
;;                                     [(update bonds bond-id
;;                                       {
;;                                         "total": 0.0
;;                                       }
;;                                     )

;;                                     (update pools-usage pool-id
;;                                       {
;;                                           "last-updated": (at "block-time" (chain-data)),
;;                                           "tokens-locked": (- (at "tokens-locked" pool-usage-data) to-pay-bond)
;;                                       }
;;                                     )

;;                                     ;Update pool data
;;                                     (update pools pool-id
;;                                       {
;;                                           "bonders": (if (> to-pay-bond 0.0) (- (at "bonders" pool-data) 1.0) (at "bonders" pool-data) )
;;                                       }
;;                                     )

;;                                     ;Update user bond data
;;                                     (update bonds bond-id
;;                                       {
;;                                         "last-updated":  (at "block-time" (chain-data)),
;;                                         "balance": 0.0,
;;                                         "last-withdraw": (if (> to-pay-bond 0.0) (at "block-time" (chain-data)) (at "last-withdraw" bond) )
;;                                         }
;;                                     )]
;;                                     true
;;                                 )
;;                               )
;;                               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;If the user is CLAIMNING REWARDS ONLY the user runs this code::
;;                               (let
;;                                 (
;;                                   (claiming:bool true)
;;                                   (has-penalty:bool (< (at "block-time" (chain-data)) penalty-end-time))
;;                                 )
;;                                 ;Here the user is claiming their rewards
;;                                 ;Enforce that the pool is funded and liquidity has been formed at the dex
;;                                 (enforce (= (at "funded" pool-data) true) "LP rewards are not yet available.")
;;                                 (enforce (= (at "liquidity-moved" pool-data) true) "LP rewards are not yet available.")

;;                                 ;If the user is claiming rewards and has a penalty or doesnt have a penalty we run different sections of code:
;;                                 (if (= has-penalty true)
;;                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 ;If the user has a withdraw penalty the user runs this code:
;;                                     (let*
;;                                       (
;;                                         (penalize:bool true)
;;                                       )
;;                                       ;Here the user is claiming rewards early and has a penalty

;;                                       ;Install LP token transfer capability
;;                                       (install-capability (kaddex.tokens.TRANSFER (get-pair-key lp-token1 lp-token2) (get-account-principal) account to-pay-plus-penalty))
;;                                       (with-capability (PRIVATE_RESERVE ) (kaddex.tokens.transfer-create (get-pair-key lp-token1 lp-token2) (get-account-principal)  account (at "guard" (coin.details account)) to-pay-plus-penalty))

;;                                       ;Update pool records if we are paying out
;;                                       (if (> to-pay 0.0)
;;                                         [(update pools-usage pool-id
;;                                           {
;;                                               "tokens-abandoned": (+ (at "tokens-abandoned" pool-usage-data) fee-amount),
;;                                               "last-updated": (at "block-time" (chain-data)),
;;                                               "paid": (+ (at "paid" pool-usage-data) to-pay-plus-penalty)
;;                                           }
;;                                         )

;;                                         ;Update pool data
;;                                         (update pools pool-id
;;                                           {
;;                                               "balance": (- (at "balance" pool-data) to-pay-plus-penalty),
;;                                               "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data) )
;;                                           }
;;                                         )

;;                                         ;Update user bond data
;;                                         (update bonds bond-id
;;                                           {
;;                                             "last-updated":  (at "block-time" (chain-data)),
;;                                             "balance": 0.0,
;;                                             "rewards": (+ (at "rewards" bond) to-pay-plus-penalty),
;;                                             "last-claimed":  (at "block-time" (chain-data))
;;                                             }
;;                                         )]
;;                                         true
;;                                       )
;;                                     )
;;                                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;If the user has no withdraw penalty the user runs the code below:
;;                                     (let
;;                                         (
;;                                           (no-penalty:bool true)
;;                                         )
;;                                         ;Here the user is claiming rewards and has no penalty

;;                                         ;Install LP token transfer capability
;;                                         (install-capability (kaddex.tokens.TRANSFER (get-pair-key lp-token1 lp-token2) (get-account-principal) account to-pay))

;;                                         ;Transfer LP rewards
;;                                         (with-capability (PRIVATE_RESERVE ) (kaddex.tokens.transfer-create (get-pair-key lp-token1 lp-token2) (get-account-principal)  account (at "guard" (coin.details account)) to-pay)  )

;;                                         ;Update pool records if paying out
;;                                         (if (> to-pay 0.0)
;;                                             [(update pools-usage pool-id
;;                                               {
;;                                                   "last-updated": (at "block-time" (chain-data)),
;;                                                   "paid": (+ (at "paid" pool-usage-data) to-pay)
;;                                               }
;;                                             )

;;                                             ;Update pool data
;;                                             (update pools pool-id
;;                                               {
;;                                                   "balance": (- (at "balance" pool-data) to-pay),
;;                                                   "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data) )
;;                                               }
;;                                             )

;;                                             ;Update user bond data
;;                                             (update bonds bond-id
;;                                               {
;;                                                 "last-updated":  (at "block-time" (chain-data)),
;;                                                 "balance": 0.0,
;;                                                 "rewards": (+ (at "rewards" bond) to-pay),
;;                                                 "last-claimed":  (at "block-time" (chain-data))
;;                                                 }
;;                                             )]
;;                                             true
;;                                         )

;;                                     )


;;                                 )

;;                               )

;;                           )

;;                           ;Return message
;;                           (if (= unbond true)
;;                             (format "Unbonded {} {}" [to-pay-bond bond-token])
;;                             (if (= (< (at "block-time" (chain-data)) penalty-end-time) true)
;;                             (format "Awarded {} with {} {} instead of {} for withdrawing before {} " [account (if (< (at "block-time" (chain-data)) penalty-end-time) to-pay-plus-penalty to-pay) (get-pair-key lp-token1 lp-token2) to-pay penalty-end-time])
;;                             (format "Awarded {} with {} {}" [account (if (< (at "block-time" (chain-data)) penalty-end-time) to-pay-plus-penalty to-pay) (get-pair-key lp-token1 lp-token2)])
;;                             )

;;                           )
;;                       )
;;                   )
;;                 (format "{} has no rewards in {}" [account pool-id]))
;;                 )
;;                 (enforce (and (= account t_account) (> t_balance 0.0)) "You have no stake in this pool.")
;;           )
;;         )

;;         )
;;     )

    ;  (defun admin-claim (pool-id:string account:string amount:decimal claim-abandoned:bool)
    ;      @doc "Claims abandoned rewards from pool - Pool creator only"
    ;      (with-capability (POOL_CREATOR_GUARD pool-id)

    ;      (let*
    ;          (
    ;              (pool-data (read pools pool-id))
    ;              (pool-usage-data (read pools-usage pool-id))
    ;              (lp-token1:module{fungible-v2} (at "reward-token-a" pool-data))
    ;              (lp-token2:module{fungible-v2} (at "reward-token-b" pool-data))
    ;              (abandoned-tokens:decimal (at "tokens-abandoned" pool-usage-data))
    ;              (to-pay:decimal (if (= claim-abandoned true) abandoned-tokens amount))
    ;          )

    ;          ;Enforce admin account
    ;          (enforce (= (at "account" pool-data) account) "Admin only.")

    ;          ;Install LP token transfer capability
    ;          (install-capability (kaddex.tokens.TRANSFER (get-pair-key lp-token1 lp-token2) (get-account-principal) account to-pay))

    ;          ;Transfer LP rewards
    ;          (with-capability (PRIVATE_RESERVE ) (kaddex.tokens.transfer-create (get-pair-key lp-token1 lp-token2) (get-account-principal)  account (at "guard" (coin.details account)) to-pay)  )

    ;          ;Update pool records
    ;          (update pools-usage pool-id
    ;            {
    ;                "last-updated": (at "block-time" (chain-data)),
    ;                "paid": (+ (at "paid" pool-usage-data) to-pay)
    ;            }
    ;          )

    ;          ;Update pool data
    ;          (update pools pool-id
    ;            {
    ;                "balance": (- (at "balance" pool-data) to-pay),
    ;                "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data) )
    ;            }
    ;          )

    ;          ;Return message
    ;          (format "Admin claimed {} rewards from pool {}" [to-pay pool-id])

    ;      )
    ;      )
    ;  )

    ;;///////////////////////
    ;;UTILITIES
    ;;//////////////////////

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

