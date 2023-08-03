(module mjc-casino GOVERNANCE
    (use util-random)
    (use util-math)
    (use util-lists)
    (use coin [ details transfer ])
    (use free.mjc-nft-pre-sale)
    
    (defconst BANK_KDA_ACCT "mjc-casinobank")
    (defconst BANK_BALANCE "mj-bank-balance")
    (defconst POOL_BALANCE "pool-balance")
    (defconst RESERVE_BALANCE "reserve-balance")
    (defconst STAKED_AMOUNT "staked-amount")

    (defconst CURRENT_ID_COUNT "current-id-count")
    (defconst STAKERS_COUNT "stakers-count")
    (defconst ROULETTE_COUNT "roulette-count")
    (defconst COIN_FLIP_COUNT "coin-flip-count")
    (defconst GAME_COUNT "game-count")
    (defconst BET_COUNT "bet-game-count")
    (defconst WIN_COUNT "win-count")
    (defconst LOSS_COUNT "loss-count")

    (defconst MAX_BET_AMOUNT 20.0)
    (defconst MIN_BET_AMOUNT 1.0)
    (defconst MAX_STAKE_AMOUNT 2000.0)
    (defconst MIN_STAKE_AMOUNT 10.0)

    (defconst RESERVE_FEE 10.0)
    (defconst MJ_FEE 50.0)
    (defconst WITHDRAWAL_FEE 5.0)

    (defconst MJ_BANK:string "k:11d94daa557ddc588a297e7252f42776a07efd5b8f536a843974838c680f95d4")
    (defconst POOL_BANK:string "k:8d8f5f51d5af731e1cd27cc7cedc24179ccc5475a87f1eb585fdad20a303cbb4")

    (defschema counts-schema

        @doc "Keeps track of how many things there are."

        count:integer

    )
    
    (defschema rewards-schema

    	@doc "Stores information about each account that has claimed a reserved token"

    	account:string

    	amount:decimal ;counts up from zero

    )

    (defschema bet-rewards-schema

    	@doc "Stores information about each account that has won a bet."

    	account:string

    	amount:decimal ;counts up from zero

    )

    (defschema staked-schema

    	@doc "Stores total staked amount"

    	amount:decimal ;counts up from zero

    )

    (defschema staking-schema

        @doc "Keeps track of how many things there are."

        account:string

    	amount:decimal

        lockperiod:time

        status:bool

    )

    (defschema withdrawn-schema

        @doc "Keeps track of how many things there are."

        account:string

    	amount:decimal

        date:time

    )

    (defschema liquidity-schema

        @doc "Stores the balance of the liquidity pool"

        balance:decimal

    )

    (defschema mj-bank-schema

        @doc "Stores KDA for MJ."

        balance:decimal

    )

    (defschema reserve-bank-schema

        @doc "Stores KDA for reserve."

        balance:decimal

    )

    (defschema game-schema

        @doc "Stores KDA for reserve."

        name:string

    )

    (defschema game-history-schema

        @doc "Stores KDA for reserve."

        bet_id:integer

        type:string

        amount:decimal

        result:string

        account:string

        bet:string

    )

    (deftable rewards-table:{rewards-schema})
    (deftable bet-rewards-table:{bet-rewards-schema})
    (deftable staked-table:{staked-schema})
    (deftable staking-table:{staking-schema})
    (deftable withdrawn-table:{withdrawn-schema})
    (deftable liquidity-table:{liquidity-schema})
    (deftable mj-bank-table:{mj-bank-schema})
    (deftable reserve-bank-table:{reserve-bank-schema})
    (deftable game-bet-history-table:{game-history-schema})
    (deftable counts-table:{counts-schema})
    (deftable game-table:{game-schema})



    (defun initialize ()
        @doc "Initializes values upon deploy of the contract"
        (with-capability (GOVERNANCE)
            "Hello"
            ;(insert counts-table CURRENT_ID_COUNT { "count": 0 }) 
            ;(insert counts-table STAKERS_COUNT { "count": 0 }) 
            ;(insert counts-table ROULETTE_COUNT { "count": 0 })
            ;(insert counts-table COIN_FLIP_COUNT { "count": 0 })
            ;(insert counts-table GAME_COUNT { "count": 0 }) 
            ;(insert counts-table BET_COUNT { "count": 0 }) 
            ;(insert counts-table WIN_COUNT { "count": 0 })
            ;(insert counts-table LOSS_COUNT { "count": 0 }) 
            ;(insert mj-bank-table BANK_BALANCE { "balance": 0.0 }) 
            ;(insert liquidity-table POOL_BALANCE { "balance": 0.0 }) 
            ;(insert staked-table STAKED_AMOUNT { "amount": 0.0 }) 
            ;(insert reserve-bank-table RESERVE_BALANCE { "balance": 0.0 }) 
        )
    )

    (defun insert-game (name:string)
        @doc "Inserts a new game type"
        (with-capability (GOVERNANCE)

            (insert game-table (int-to-str  10 (+ 1 (get-count GAME_COUNT)))

                {"name": name}

            )

        )
    )

    (defun get-game (id:integer)

        @doc "Get the name of a game."

        (with-default-read game-table id

            { 'name: 0 }

            { 'name:= game-name }

            game-name

        )

    )

    (defun get-bank-balance ()

        @doc "Returns the smart contract balance."

        (with-capability (GOVERNANCE)

            (with-default-read mj-bank-table BANK_BALANCE

                { 'balance: 0 }

                { 'balance:= balance }

                balance

            )

        )

    )

    (defun get-account-balance (account:string)

        @doc "Returns the KDA balance of an account."

        (coin.get-balance account)

    )

    (defun get-reserve-balance ()

        @doc "Returns reserve balance."

        (with-capability (GOVERNANCE)

            (with-default-read reserve-bank-table RESERVE_BALANCE

                { 'balance: 0 }

                { 'balance:= balance }

                balance

            )

        )

    )

    (defun get-pool-balance ()

        @doc "Returns the pool balance."

        (with-default-read liquidity-table POOL_BALANCE

            { 'balance: 0 }

            { 'balance:= balance }

            balance

        )

    )

    (defun get-rewards-balance (account:string)

        @doc "Returns the rewards balance of an account."

        (with-capability (ACCOUNT_GUARD account)

            (with-default-read rewards-table account

                { 'amount: 0 }

                { 'amount:= amount }

                amount

            )

        )

    )

    (defun distribute-rewards (bet-id:string)
        @doc "Calculates and distrubutes the rewards to stakers and the platform"
        ;(require-capability (PRIVATE))
        (with-capability (PRIVATE)

            (let*

                (

                    (result (with-default-read game-bet-history-table bet-id {'result: 0} {'result:= bt-result} bt-result))

                    (prev-reserve-balance (with-default-read reserve-bank-table RESERVE_BALANCE { 'balance: 0 }{ 'balance:= balance }balance))

                    (prev-bank-balance (with-default-read mj-bank-table BANK_BALANCE { 'balance: 0 }{ 'balance:= balance }balance))

                    (bet-amount (with-default-read game-bet-history-table bet-id {'amount: 0} {'amount:= amount} amount))

                    (total-pool-balance (get-pool-balance))

                    (mj-fee-amount (* (/ MJ_FEE 100.0) bet-amount))

                    (reserve-fee-amount (* (/ RESERVE_FEE 100.0) bet-amount))

                    (distributable-amount (- bet-amount (+ mj-fee-amount reserve-fee-amount)))

                    (total-staked (get-total-staked-amount))

                    (stakers (select staking-table ['account, 'amount] (where "status" (= true))))

                )



                (if (= result "loss") 

                    (if (> bet-amount 0.0) 

                        [

                            (update reserve-bank-table RESERVE_BALANCE {"balance": (+ prev-reserve-balance reserve-fee-amount)})

                            (update mj-bank-table BANK_BALANCE {"balance": (+ prev-bank-balance mj-fee-amount)})

                            (update liquidity-table POOL_BALANCE {"balance": (+ bet-amount total-pool-balance)})

                            (if (> total-staked 0.0)

                                (calculate-rewards stakers distributable-amount)

                                "Total staked amount is 0.0"

                            )

                        ]

                        "Bet amount must be greater than 0.0"

                    )

                    "No rewards"

                )

                

            )

        )

        

    )

    (defun calculate-rewards (stakers:list amount:decimal)

        @doc "Calculate user reward"

        (require-capability (PRIVATE))

        (with-capability (PRIVATE)

            (map (calculate-and-distribute-reward amount) stakers)

        )

    )

    (defun distribute-reward (staker:object amount:decimal) 

        (require-capability (PRIVATE))

        (let

            (

                (total-staked (get-total-staked-amount))

                (staked-amount (get-staked-amount (at 'account staker)))

            )   



            (if (> total-staked 0) (if (> staked-amount 0)

                ;;(calculate-reward amount)

            ))

        )

    )

    (defun calculate-and-distribute-reward (amount:decimal staker:object)

        @doc "Calculate the reward of a user and update the rewards table."

        (require-capability (PRIVATE))

        (let

            (

                (percentage (/ (get-staked-amount (at 'account staker)) (get-total-staked-amount)))

            )

            (if (> percentage 0.0)

                (let 

                    (

                        (reward (* percentage amount))

                    )

                    (if (> reward 0.0)

                        (update rewards-table (at 'account staker) {"amount": (+ reward (get-rewards (at 'account staker)))})

                        "Reward is 0.0"

                    )

                )

                "Percentage is 0.0"

            )

        )

    )

    (defun get-rewards (account:string)

        @doc "Get user rewards."

        (with-default-read rewards-table account

            { 'amount: 0 }

            { 'amount:= amount }

            amount

        )

    )

    (defun get-bet-rewards (account:string)

        @doc "Get user bet rewards."

        (with-default-read bet-rewards-table account

            { 'amount: 0 }

            { 'amount:= amount }

            amount

        )

    )

    (defun withdraw-staked-rewards (account:string)

        @doc "Withdraw staked rewards"
        (with-capability (BANK_DEBIT)
        (with-capability (ACCOUNT_GUARD account)
            (let*
                (
                  (rewards (get-rewards account)) 
                )
                (enforce (> rewards 0.0) "Your rewards balance is zero.")

                (install-capability (coin.TRANSFER BANK_KDA_ACCT account (round rewards 5)))
                (coin.transfer BANK_KDA_ACCT account (round rewards 5))

                (update rewards-table account {"amount": 0.0})
    
                (format "You successfully made a withdrawal of {} KDA" [(round rewards 5)])
            )
        ))

    )

    (defun withdraw-bet-rewards (account:string)

        @doc "Withdraw bet rewards"
        (with-capability (BANK_DEBIT)
        (with-capability (ACCOUNT_GUARD account)
            (let*
                (
                  (rewards (get-bet-rewards account)) 
                )
                (enforce (> rewards 1.0) "Your rewards balance must be greater than 1.")

                (install-capability (coin.TRANSFER BANK_KDA_ACCT account rewards))

                (coin.transfer BANK_KDA_ACCT account (round rewards 2))
    
                (update bet-rewards-table account {"amount": 0.0})
    
                (format "You successfully made a withdrawal of {} KDA" [rewards])
            )

        ))

    )

    (defun admin-withdraw-rewards (account:string amount:decimal)

        @doc "Admin withdraw rewards"
        (with-capability (BANK_DEBIT)
        (with-capability (GOVERNANCE)
            (let*
                (
                  (balance (get-bank-balance)) 
                )
                (enforce (>= amount 0.0) "Bank balance is zero.")
                (enforce (>= balance amount) "Insufficient Funds")

                (install-capability (coin.TRANSFER BANK_KDA_ACCT account amount))

                (coin.transfer BANK_KDA_ACCT account (round amount 2))

                (update mj-bank-table BANK_BALANCE {"amount": (- balance amount)})

                (format "You successfully made a withdrawal of {} KDA" [balance])
            )

        ))

    )

    (defun admin-withdraw-reserve (account:string amount:decimal)
        (with-capability (BANK_DEBIT)
        (with-capability (GOVERNANCE)

            (let*
                (
                  (balance (get-reserve-balance)) 
                )
                (enforce (>= amount 0.0) "Reserve Bank balance is zero.")

                (enforce (>= balance  amount) "Insufficient Funds.")
    
                (install-capability (coin.TRANSFER BANK_KDA_ACCT account (round amount 2)))
    
                (coin.transfer BANK_KDA_ACCT account amount)
    
                (update reserve-bank-table RESERVE_BALANCE {"amount": (- balance amount)})
            )

        ))

    )

    (defun get-all-rewards ()

        @doc "Get all rewards."

        (select rewards-table (where "account" (!= "")))

    )

    (defun get-staked-amount (account:string)

        @doc "Get amount staked by a user."

        (with-default-read staking-table account

            { 'amount: 0 }

            { 'amount:= amount }

            amount

        )

    )

    (defun get-total-staked-amount ()

        @doc "Get total staked amount."

        (with-default-read staked-table STAKED_AMOUNT {'amount: 0} {'amount:= amount} amount)

    )

    (defun get-staked-info (account:string)

        @doc "Get account staking information."

        (with-default-read staking-table account

            {

                "account": "",

                "amount": 0.0,

                "status": "",

                "lockperiod": ""

            }

            {

                "account":= staker,

                "amount":= amount,

                "status":= status,

                "lockperiod":= time

            }

            {"staker":staker, "amount":amount, "status":status, "time":time}

        )

    )

    (defun stake (account:string amount:decimal)

        @doc "Stake KDA to earn rewards"   

        (with-capability (ACCOUNT_GUARD account)
            (enforce (<= amount MAX_STAKE_AMOUNT) (format "You have exceeded max stake amount of {} " [MAX_STAKE_AMOUNT]))
            (enforce (>= amount MIN_STAKE_AMOUNT) (format "You stake amount is less than the minimum staking amount of {}." [MIN_STAKE_AMOUNT]))

            (let

                (
                    (status (with-default-read staking-table account {'status: ""} {'status:= status} status))
                    (total-staked-amount (get-total-staked-amount))
                    (total-pool-balance (get-pool-balance))
                    (num-tokens-reserved (mjc-nft-pre-sale.get-mjc-reserved account))
                )   
                (enforce (> num-tokens-reserved num-tokens-reserved) "You must have atleast one Marietta Jane Casino NFT to stake KDA and enjoy benefits.")

                (coin.transfer account POOL_BANK amount)

                (if (= status "")
                    (let*
                        (
                            (status (with-default-read staking-table account {'status: ""} {'status:= status} status))
                        )
                        (insert rewards-table account
                            {
                                "account": account,
                                "amount": 0.0
                            }
                        )

                        (insert staking-table account
                            {
                                "account": account,
                                "amount": amount,
                                "status":true,
                                "lockperiod": (add-time (at 'block-time (chain-data)) (days 30)) 
                            }
                        )

                        (update staked-table STAKED_AMOUNT {"amount": (+ amount total-staked-amount)})
                        (update liquidity-table POOL_BALANCE {"balance": (+ amount total-pool-balance)})
                        (with-capability (PRIVATE)
                            (increase-count STAKERS_COUNT)
                            "Your staking was successful"
                        )

                    )

                    (if (= status true)
                        [
                            (update staking-table account
                                { 
                                    "amount": (+ (get-staked-amount account) amount),
                                    "lockperiod": (add-time (at 'block-time (chain-data)) (days 30))
                                }
                            )

                            (update liquidity-table POOL_BALANCE {"balance": (+ amount total-pool-balance)})
                            (update staked-table STAKED_AMOUNT {"amount": (+ amount total-staked-amount)})
                            "Your staking was updated successfully."
                        ]
                        "Your staking was not updated successfully."
                    )

                )
            )

        )

    )

    (defun unstake (account:string)
        @doc "Unstake account KDA" 
        (with-capability (BANK_DEBIT)
        (with-capability (ACCOUNT_GUARD account)
            (let*
                (
                    (status (with-default-read staking-table account {'status: 0} {'status:= status} status))
                    (staked-amount (with-default-read staking-table account {'amount: 0} {'amount:= amount} amount))
                    (lockperiod (with-default-read staking-table account {'lockperiod: 0} {'lockperiod:= lockperiod} lockperiod))
                    (total-staked-amount (get-total-staked-amount))
                    (total-pool-balance (get-pool-balance))
                )    

                (enforce (= status true) "Please kindly stake to unstake.")
                (enforce (< lockperiod (at 'block-time (chain-data))) (format "You funds will be available for unstaking by {}, please exercise patiance. Thank you." [lockperiod]))
                (coin.transfer BANK_KDA_ACCT account staked-amount)
                (update staking-table account {"status": false})
                (update staked-table STAKED_AMOUNT {"amount": (- total-staked-amount staked-amount)})
                (update liquidity-table POOL_BALANCE {"balance": (- total-pool-balance staked-amount)})
                (update counts-table STAKERS_COUNT {"count": (- (get-count STAKERS_COUNT) 1)})
            )
        ))

    )


    (defun place-coin-flip-bet (account:string type:string amount:decimal bet:string)
        @doc "Place a coin flip bet game"
        (with-capability (ACCOUNT_GUARD account)
            (enforce (<= amount MAX_BET_AMOUNT) (format "Maximum Bet amount is {}" [MAX_BET_AMOUNT]))
            (enforce (>= amount MIN_BET_AMOUNT) (format "Minimum Bet amount is {}" [MIN_BET_AMOUNT]))
            (with-capability (PRIVATE)
                (if (= type "coin-flip")
                    [
                        (enforce (contains bet ["kda" "mjc"]) "Bet must be KDA or MJC")
                        (coin.transfer account MJ_BANK amount)
                        (increase-count COIN_FLIP_COUNT)
                        (let*
                            (
                                (result (coin-flip-bet))
                            )  
                            
                            (if (= result bet)
                                [
                                    (update-bet-history-table amount account type "win" bet)
                                    result
                                ]
                                (if (!= result bet)
                                    [
                                        (update-bet-history-table amount account type "loss" bet)
                                        result
                                    ]
                                    "Something is wrong."
                                )
                            )  
                        )
                    ]
                    "Game type not found."
                )
            )
        )

    )

    (defun place-roulette-bet (account:string type:string amount:decimal bet:string)

        (with-capability (ACCOUNT_GUARD account)
            (enforce (<= amount MAX_BET_AMOUNT) (format "Maximum Bet amount is {}" [MAX_BET_AMOUNT]))
            (enforce (>= amount MIN_BET_AMOUNT) (format "Minimum Bet amount is {}" [MIN_BET_AMOUNT]))
            (enforce (!= bet "") "Bet can not be empty.")
            (with-capability (PRIVATE)
                (coin.transfer account MJ_BANK amount)
                (if (= type "number")
                    (let*
                        (
                            (bet-number (str-to-int 10 bet))
                            (bet-result (random 0 37))
                        )    

                        (if (= bet-result bet-number)
                            [
                                (update-bet-history-table amount account type "win" bet)
                                bet-result
                            ]
                            [
                                (update-bet-history-table amount account type "loss" bet)
                                bet-result
                            ]

                        )

                    )

                    (if (= type "odd_even")
                        [
                            (enforce (contains bet ["odd" "even"]) "Bet must be ODD or EVEN")
                            (increase-count ROULETTE_COUNT)
                            (let*
                                (
                                    (resultbet (roulette-random))
                                    (even (is-even resultbet))
                                    (odd (is-odd resultbet))
                                    (result (if (= even true)
                                                "even"
                                                (if (= odd true)
                                                    "odd"
                                                    "Not valid"
                                                )
                                            )
                                    )
                                )  
                                (if (= result bet)
                                    [
                                        (update-bet-history-table amount account type "win" bet)
                                        resultbet
                                    ]
                                    
                                    (if (!= result bet)
                                        [
                                            (update-bet-history-table amount account type "loss" bet)
                                            resultbet
                                        ]
                                        "Something is wrong."
                                    )
                                )  
                            )
                        ]

                        (if (= type "red_green_black")
                            [
                                (enforce (contains bet ["black" "red" "green"]) "Bet must be RED, BLACK or GREEN")
                                (increase-count ROULETTE_COUNT)
                                (let*
                                    (
                                        (data ["black" "red" "green"])
                                        (result (roulette-string-random data))
                                    )  

                                    (if (= result bet)
                                        [
                                            (update-bet-history-table amount account type "win" bet)
                                            result
                                        ]
                                        (if (!= result bet)
                                            [
                                                (update-bet-history-table amount account type "loss" bet)
                                                result
                                            ]
                                            "Something is wrong."
                                        )
                                    )  
                                )
                            ]

                            (if (= type "oneeighteen")
                                [
                                    (enforce (= bet type) "Bet must be oneeighteen")
                                    (increase-count ROULETTE_COUNT)
                                    (let*
                                        (
                                            (result (int-to-str 10 (roulette-random)))
                                        )  

                                        (if (contains result ["1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18"])
                                            [
                                                (update-bet-history-table amount account type "win" bet)
                                                result
                                            ]
                                            [
                                                (update-bet-history-table amount account type "loss" bet)
                                                result
                                            ]
                                        ) 
                                    )
                                ]

                                (if (= type "nineteenthirtysix")
                                    [
                                        (enforce (= bet type) "Bet must be nineteenthirtysix")
                                        (increase-count ROULETTE_COUNT)
                                        (let*
                                            (
                                                (result (int-to-str 10 (roulette-random)))
                                            )  
                                            (if (contains result ["19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35" "36"])
                                                [
                                                    (update-bet-history-table amount account type "win" bet)
                                                    result
                                                ]
                                                [
                                                    (update-bet-history-table amount account type "loss" bet)
                                                    result
                                                ]
                                            )  
                                        )
                                    ]

                                    (if (= type "zeroes")
                                        [
                                            (enforce (contains bet ["00" "0"]) "Bet must be 0 or 00")
                                            (increase-count ROULETTE_COUNT)
                                            (let*
                                                (
                                                    (data ["00" "0"])
                                                    (result (roulette-string-random data))
                                                )  
        
                                                (if (= result bet)
                                                    [
                                                        (update-bet-history-table amount account type "win" bet)
                                                        result
                                                    ]
                                                    [
                                                        (update-bet-history-table amount account type "loss" bet)
                                                        result
                                                    ]
                                                ) 
                                            )
                                        ]
                                        "Game Roulette type not found."
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

    (defun roulette-number-bet (bet-number:integer limit:integer)

        (require-capability (PRIVATE))

        (enforce (<= bet-number 36) "Maximum Roulette Bet number is 32." )

        (enforce (>= bet-number 0) "Roulette Bet number must be equal or greater than 0." )

        (random bet-number limit)

    )

    (defun roulette-random ()
        (random-int-range 1 36)
    )

    (defun roulette-string-random (data:list)
        (random-choice data)
    )

    (defun coin-flip-bet ()

        (random-choice ["kda" "mjc"])

    )

    (defun random (offset:integer limit:integer)

        (random-int-range offset limit)

    )

    (defun test-random ()

        (random-int-range 0 10)

    )

    (defun update-bet-history-table (amount:decimal account:string type:string result:string bet:string)

        (require-capability (PRIVATE))
        (enforce (!= result "") "Result is empty.")
        (increase-count BET_COUNT)
        (if (= result "win")
            [
                (let*
                    (
                        (total-pool-balance (get-pool-balance))
                        (bet-rewards (get-bet-rewards account))
                        (bet-odd (if (= type "coin-flip")
                                    (get-coin-flip-odd amount)
                                    (get-roulette-odd amount type)
                                  )
                        )
                        (win-amount (* amount bet-odd))
                        (reward-account (with-default-read bet-rewards-table account { 'account: 0 }{ 'account:= account } account))
                    )    

                    (update liquidity-table POOL_BALANCE {"balance": (- total-pool-balance win-amount)})

                    (if (= reward-account 0)
                        (insert bet-rewards-table account
                            {
                                "amount": win-amount,
                                "account": account
                            }
                        )
                        (update bet-rewards-table account {"amount": (+ win-amount bet-rewards)})
                    )


                    (insert game-bet-history-table (int-to-str 10 (get-count BET_COUNT))
                        {
                            "bet_id": (get-count BET_COUNT),
                            "type": type,
                            "amount": win-amount,
                            "result": result,
                            "account": account,
                            "bet": bet
                        }
                    )
                )
            ]



            (if (= result "loss")

                [

                    (insert game-bet-history-table (int-to-str 10 (get-count BET_COUNT))

                        {
                            "bet_id": (get-count BET_COUNT),
                            "type": type,
                            "amount": amount,
                            "result": result,
                            "account": account,
                            "bet": bet

                        }

                    )

                    ;Distribute rewards to all stakers 

                    (distribute-rewards (int-to-str 10 (get-count BET_COUNT)))

                ]

                "Something went wrong"

            )

        )

        ;Bet Result status

        result

    )

    (defun get-account-bet-history (account:string)
        (select game-bet-history-table ['type, 'account, 'amount, 'result, 'bet] (where "account" (= account)))
    )

    (defun get-bet-history (limit:integer)
        (let*
            (
                (offset 
                    (if (<= (get-count BET_COUNT) limit)
                        (get-count BET_COUNT)
                        (- (get-count BET_COUNT) - limit)
                    )
                )
            )
            (select game-bet-history-table ['bet_id, 'type,'account, 'amount, 'result, 'bet] (where 'bet_id (<= offset)))
        )
    )

    (defun get-coin-flip-odd (amount:decimal)
        (if (< amount 2.0) 
            2.0
            (if (< amount 6.0) 
                1.7
                (if (< amount 10.0)
                    1.4
                    (if (<= amount 20.0) 
                        1.25
                        0.0
                    )

                )

            )

        )
    )

    (defun get-roulette-odd (amount:decimal type:string)
        (if (= type "zeroes") 
            (get-coin-flip-odd amount)
            (if (= type "nineteenthirtysix") 
                (get-coin-flip-odd amount)
                (if (= type "oneeighteen") 
                    (get-coin-flip-odd amount)
                    (if (= type "red_green_black") 
                        (get-coin-flip-odd amount)
                        (if (= type "odd_even") 
                            (get-coin-flip-odd amount)
                            (if (= type "number") 
                                10
                                0
                            )
                        )
                    )
                )
            )
        )
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

    (defun create-simple-user-guard (funder:string amount:decimal account:string)

        (coin.transfer-create funder account 

            (create-BANK_DEBIT-guard) amount)

    )

    (defun require-BANK_DEBIT () 

        (require-capability (BANK_DEBIT))

    )

    (defun create-BANK_DEBIT-guard ()

        (create-user-guard (require-BANK_DEBIT))

    )

    (defcap GOVERNANCE ()

        (enforce-guard (keyset-ref-guard "free.mjc-keyset"))

    )

    (defcap PRIVATE ()
        true
    )

    (defcap ACCOUNT_GUARD (account:string) 

        @doc "Verifies account meets format and belongs to caller"

        (enforce-guard 

        (at "guard" (coin.details account))

        )

    )

    (defcap BANK_DEBIT ()
        true
    ) 
)


;; Read the `upgrade` key from transaction data

