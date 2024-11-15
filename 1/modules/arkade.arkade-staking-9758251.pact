(module arkade-staking GOVERNANCE
    @doc "Arkade staking contract for ARKD and NFT rewards"
    (use arkade.token [ transfer get-balance create-account transfer-create ])

    ; --------------------------------------------------------------------------
    ; Constants
    ; --------------------------------------------------------------------------
    (defconst ARKD_STAKING_BANK "arkd-staking-bank")
    (defconst ARKD_STAKING_ACCOUNT "k:beeb4b1f5aaa492c29d4287e1f9951c71bfb363e63fb6c5940f10f3ec050e58e")
    (defconst ARKD_DEV_ACCOUNT "k:a3ecc7fc15052ea4ffecad3035bad35c8e3b20a70ddb5227e4c35d227e4c0d13")
    (defconst MAX_STAKE_AMOUNT 5000000.0)
    (defconst MIN_STAKE_AMOUNT 100.0)
    (defconst SECONDS_IN_DAY 86400.0)
    (defconst REWARD_INTERVAL 3600.0) ;In seconds 3600=> 1 HOUR
    (defconst DAYS_IN_YEAR 365.0)
    (defconst STAKING_FEE 1.0) ;in KDA
    (defconst EARLY_UNSTAKING_FEE 2.0) ;in KDA
    (defconst DEV_P 0.1)
    (defconst TOTAL_STAKED_TOKENS "total-staked-tokens")
    (defconst TOTAL_REWARDS "total-rewards")
    (defconst CLAIM_FEES "claim-fees") ;In percentage
    (defconst STAKED "staked-count")
    (defconst REWARD_COUNT "reward-count")
    (defconst STAKING "staking-count")
    (defconst CLAIM "claim-count")
    (defconst REWARD_TIME "reward-time")
    (defconst STAKE_TIERS:list ["bronze","silver","gold","diamond","superstar"])
    ; --------------------------------------------------------------------------
    ; Utilities
    ; --------------------------------------------------------------------------
    (defcap STAKE(owner:string amount:decimal)
        @managed
        (compose-capability (PRIVATE))
    )

    (defcap PRIVATE ()
        true
    )

    (defcap GOVERNANCE ()
        (enforce-one "Any Guard passes" [
            (enforce-guard (at "guard" (coin.details "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec")))
        ])
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard 
            (at "guard" (coin.details account))
        )
    )

    (defun create-arkd-account(account:string)
        (create-account account (at "guard" (coin.details account)))
    )

    ; --------------------------------------------------------------------------
    ; Schema
    ; --------------------------------------------------------------------------
    (defschema counts-schema
        @doc "Keeps track of key counts."
        count:integer
    )

    (defschema staking-schema
        @doc "Stores staking information"
        account:string
        amount:decimal
        stake-time:time
        tier:string
        status:bool
        days:integer
        apy:decimal
        nft-reward-number:integer
        rewards:decimal
        staking-id:integer
        claim:object
    )

    (defschema reward-schema
        @doc "Stores reward time information"
        reward-time:time
    )

    (defschema staking-types-schema
        @doc "Stores staking types information"
        tier:string
        lock-days:integer
        apy:decimal
        nft-reward-number:integer
    )

    (defschema price-schema
        @doc "Stores the price different keys"
        price:decimal
    )

    ; --------------------------------------------------------------------------
    ; Tables
    ; --------------------------------------------------------------------------
    (deftable tk-staking-table:{staking-schema})
    (deftable staking-types-table:{staking-types-schema})
    (deftable counts-table:{counts-schema})
    (deftable price-table:{price-schema})
    (deftable admin-reward-table:{reward-schema})

    ; --------------------------------------------------------------------------
    ; Init
    ; --------------------------------------------------------------------------
    (defun init()
      (with-capability (GOVERNANCE)
        (insert counts-table REWARD_COUNT { "count": 0 })
        ;(insert admin-reward-table REWARD_TIME {
            ;'reward-time: (at "block-time" (chain-data))
        ;})
        ;(insert counts-table STAKED { "count": 0 })
        ;(insert counts-table STAKING { "count": 0 })
        ;(insert counts-table CLAIM { "count": 0 })
        ;(insert price-table TOTAL_STAKED_TOKENS { "price": 0.0})
        ;(insert price-table TOTAL_REWARDS { "price": 0.0})
        ;(insert price-table CLAIM_FEES { "price": 5.0}) ;In percentage
      )
    )
    
    ; --------------------------------------------------------------------------
    ; Get information
    ; --------------------------------------------------------------------------
    (defun get-stake-tiers ()
        @doc "Returns the stake tiers"
        STAKE_TIERS
    )

    (defun get-staking-fee ()
        @doc "Returns the staking fee"
        STAKING_FEE
    )

    (defun get-early-unstaking-fee ()
        @doc "Returns early unstaking fee"
        EARLY_UNSTAKING_FEE
    )

    (defun get-dev-fee ()
        @doc "Returns dev fee"
        DEV_P
    )

    (defun get-min-stake-amount ()
        @doc "Returns min stake amount"
        MIN_STAKE_AMOUNT
    )

    (defun get-max-stake-amount ()
        @doc "Returns max stake amount"
        MAX_STAKE_AMOUNT
    )

    (defun get-staking(account:string)
        @doc "Returns staking information"
        (select tk-staking-table (where "account" (= account)))
    )

    (defun get-unlock-time(staking-id:string)
        @doc "Returns unlock time"
        (let*
            (
                (data (at 'status (read tk-staking-table staking-id ["days" "stake-time"])))
            )
            (add-time (at 'stake-time data) (days (at 'days data)))
        )
    )

    (defun get-time-since-tokens-staked (staking-id:string)
        @doc "gets time the stake status changed"
        (diff-time (at "block-time" (chain-data)) (time-locked staking-id))
    )

    (defun get-apy-rewards(staking-id:string)
        (let*
            (
                (data (read tk-staking-table staking-id ["apy" "amount" "status"]))
                (total-d-staked (/ (get-time-since-tokens-staked staking-id) SECONDS_IN_DAY))
                (status (at 'status data))
                (stake-amount (at 'amount data))
                (apy (at 'apy data))
                (d-apy (/ (/ (* apy 100.0) DAYS_IN_YEAR) 100.0))
                (d-reward (* d-apy stake-amount))
                (total-reward (* d-reward total-d-staked))
            ) 
            (enforce (= status true) "You must stake to calculate rewards.")
            (round total-reward 4)   
        )
    )

    (defun get-stakings-total-amount(account:string)
        @doc "Returns total staked amount accross all stakings."
        (let*
            (
                (data (select tk-staking-table ['amount,'status] (where "account" (= account))))
                (total-amount (fold (+) 0.0 (map (get-staking-amount) data)))
            )   
            total-amount 
        )
    )

    (defun get-staking-amount(staking:object)
        (if (= (at 'status staking) true)
            (at 'amount staking)
            0.0
        )
    )

    (defun time-locked(staking-id:string)
        @doc "Returns the time tokens were locked."
        (at 'stake-time (read tk-staking-table staking-id ["stake-time"]))
    )

    (defun get-staked-accounts()
        @doc "Returns all the accounts that have staked tokens."
        (select tk-staking-table (where "status" (= true)))
    )

    (defun get-staking-types ()
        @doc "Returns all the staking types"
        (select staking-types-table (where "tier" (!= "null")))
    )

    (defun get-staking-type (key:string)
        @doc "Returns a staking type info"
        (read staking-types-table key)
    )

    (defun get-count:integer (key:string)
        @doc "Gets the count for a key" 
        (at "count" (read counts-table key ['count]))
    )
    
    (defun increase-count (key:string)
        @doc "Increase the count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )

    (defun decrease-count (key:string)
        @doc "Decrease the count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update counts-table key {"count": (- (get-count key) 1)})
    )

    (defun get-price (price-key:string) 
        @doc "Gets the price for a key"
        (at "price" (read price-table price-key ["price"]))
    )
    
    (defun set-price(key:string value:decimal)
        @doc "Sets the price for a key to store in the price-table"
        (with-capability (GOVERNANCE)
            (update price-table key 
                {"price": value} 
        ))
    )

    (defun check-staking-status(staking-id:string)
        (at "status" (read tk-staking-table staking-id ["status"]))
    )

    ; --------------------------------------------------------------------------
    ; Functions
    ; --------------------------------------------------------------------------
    (defun set-staking-type(tier:string lock-days:integer apy:decimal nft-reward-number:integer)
        @doc "Allows the admin to set staking types"
        (with-capability (GOVERNANCE)
            (insert staking-types-table tier {
                'tier: tier
                ,'lock-days: lock-days
                ,'apy: apy
                ,'nft-reward-number: nft-reward-number
            })
        )
    )

    (defun update-staking-type(tier:string lock-days:integer apy:decimal nft-reward-number:integer)
        @doc "Allows the admin to set staking types"
        (with-capability (GOVERNANCE)
            (update staking-types-table tier {
                'lock-days: lock-days
                ,'apy: apy
                ,'nft-reward-number: nft-reward-number
            })
        )
    )

    (defun lock-tokens(account:string amount:decimal tier:string)
        (with-capability (PRIVATE)
        (with-capability (ACCOUNT_GUARD account)
            (enforce (= true (contains tier STAKE_TIERS)) (format "Tier '{}' does not exist in list {}" [tier STAKE_TIERS]))
            (enforce (>= amount MIN_STAKE_AMOUNT) (format "Min Stake amount is {} ARKD" [MIN_STAKE_AMOUNT]))
            (enforce (<= amount MAX_STAKE_AMOUNT) (format "Max Stake amount is {} ARKD" [MAX_STAKE_AMOUNT]))
            (let*
                (
                    (total-staked (get-price TOTAL_STAKED_TOKENS))
                    (new-total (+ total-staked amount))
                    (package (get-staking-type tier))
                    (balance (try 0.0 (get-balance account)))
                    (dev-p (* STAKING_FEE DEV_P))
                    (arkd-p (- STAKING_FEE dev-p))
                    (total-stakings (get-stakings-total-amount account))
                )
                (enforce (< (+ amount total-stakings) MAX_STAKE_AMOUNT) (format "You've exceeded the total stake amount of {} ARKD. Current total staked {} ARKD." [MAX_STAKE_AMOUNT total-stakings]))
                (if (<= balance 0.0)
                    (enforce (> amount balance) "Insufficient balance.")
                    true
                )
                
                (insert tk-staking-table (int-to-str 10 (+ (get-count STAKING) 1)) {
                    'account: account
                    ,'amount: amount
                    ,'stake-time: (at "block-time" (chain-data))
                    ,'tier: tier
                    ,'status: true
                    ,'rewards: 0.0
                    ,'days: (at 'lock-days package)
                    ,'apy: (at 'apy package)
                    ,'nft-reward-number: (at 'nft-reward-number package)
                    ,'claim: {}
                    ,'staking-id: (+ (get-count STAKING) 1)
                })
            
                (transfer account ARKD_STAKING_ACCOUNT amount)
                (coin.transfer account ARKD_DEV_ACCOUNT dev-p)
                (coin.transfer account ARKD_STAKING_ACCOUNT arkd-p)
                (increase-count STAKING)
                (increase-count STAKED)
                (update price-table TOTAL_STAKED_TOKENS {
                    'price: new-total
                }) 
            )
        ))
    )

    (defun unlock-tokens(account:string staking-id:string)
        (with-capability (ACCOUNT_GUARD account)
        (with-capability (BANK_DEBIT)
        (with-capability (PRIVATE)
            (let*
                (
                    (tk-account (at 'account (read tk-staking-table staking-id)))
                    (staked-amount (at 'amount (read tk-staking-table staking-id)))
                    (stake-time (at 'stake-time (read tk-staking-table staking-id)))
                    (days (at 'days (read tk-staking-table staking-id)))
                    (days-in-seconds (* days SECONDS_IN_DAY))
                    (total-staked (get-price TOTAL_STAKED_TOKENS))
                    (staking-rewards (get-apy-rewards staking-id))
                    (new-total (- total-staked staked-amount))
                    (stake-period-status (get-stake-period stake-time days-in-seconds))
                    (status (check-staking-status staking-id))
                    (dev-p (* EARLY_UNSTAKING_FEE DEV_P))
                    (arkd-p (- EARLY_UNSTAKING_FEE dev-p))
                )
                (enforce (= account tk-account) "Token does not belong to this account")
                (enforce (= status true) "You must lock your ARKD tokens to unlock.")
                (claim-reward account staking-id)
                (if (= stake-period-status false) 
                    [
                        (coin.transfer account ARKD_DEV_ACCOUNT dev-p)
                        (coin.transfer account ARKD_STAKING_ACCOUNT arkd-p)
                    ]
                    true
                )
                (update price-table TOTAL_STAKED_TOKENS 
                    {"price": new-total} 
                )
                (install-capability (arkade.token.TRANSFER ARKD_STAKING_BANK account (+ staking-rewards staked-amount)))
                (transfer ARKD_STAKING_BANK account (+ staking-rewards staked-amount))
                (update tk-staking-table staking-id {
                    'status: false
                })
                (decrease-count STAKED)
                (format "Your staked {} ARKD tokens and {} ARKD staking rewards has been transferred to you account." [staked-amount staking-rewards])
            )
        )))
    )

    (defun claim-reward(account:string staking-id:string)
        (require-capability (PRIVATE))
        (let*
            (
                (rewards (get-apy-rewards staking-id))
                (tk-account (at 'account (read tk-staking-table staking-id)))
            )
            (enforce (> rewards 0.0) "Your tokens must be greater than 0.0")
            (enforce (= account tk-account) "Token does not belong to this account")
            (update tk-staking-table staking-id {
                'rewards: 0.0
                ,'claim: {"reward": (round rewards 2),  "claim-time": (at "block-time" (chain-data))}
            })
            (increase-count CLAIM)
        )
        
    )

    ; --------------------------------------------------------------------------
    ; Utility 
    ; --------------------------------------------------------------------------
    (defun curr-time ()
        (at 'block-time (chain-data))
    )

    (defun get-stake-period (staked-time:time wait-time:decimal)
        (if (>= (diff-time (curr-time) staked-time) wait-time) 
            true
            false
        )
    )

    (defun create-arkd-user-guard (funder:string amount:decimal account:string)
        (with-capability (GOVERNANCE)
            (transfer-create funder account 
                (create-BANK_DEBIT-guard) amount)
        )
    )
    
    (defun require-BANK_DEBIT () 
        (require-capability (BANK_DEBIT))
    )
    
    (defun create-BANK_DEBIT-guard ()
        (create-user-guard (require-BANK_DEBIT))
    )

    (defcap BANK_DEBIT ()
        true
    ) 
)
