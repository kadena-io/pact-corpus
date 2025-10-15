(module finux GOVERNANCE
    @doc "finux is a utility token dedicated to supporting honest and uncensored citizen journalism and a free and informative prediction market, all on finulab: the front-page of finance"
    @model [
        (defproperty conserves-mass (amount:decimal)
            (= (column-delta token-table 'balance) 0.0)
        )
        (defproperty valid-account-id (account-id:string)
            (and
                (>= (length account-id) 3)
                (<= (length account-id) 256)
            )
        )
    ]

    ;; -----------------------------------------------------------------------------------------------------
    ;;  schemas
    (defschema token-schema
        @doc "schema designated to account and maintain token distribution ledger for all accounts"
        @model [
          (invariant (>= balance 0.0))
        ]
        guard:guard
        balance:decimal
    )
    (defschema token-initialization-schema
        @doc "schema designated to ensuring that this contract is only initialized once--see initialize-contract function below"
        initialized:bool
    )

    (defschema crosschain-schema
        @doc "schema designated to facilitate cross-chain transfers; see transfer-crosschain function below"
        @model [
            (invariant (>= amount 0.0))
            (invariant (!= "" receiver))
        ]
        receiver:string
        receiver-guard:guard
        amount:decimal
    )

    (defschema market-schema
        @doc "deprecated, now using y-n-market-schema & c-market-schema"
        @model [
          (invariant (>= quantityYes 0.0))
          (invariant (>= quantityNo 0.0))
        ]
        predictiveQuestion:string
        outcome:string
        quantityYes:decimal
        quantityNo:decimal
        priceYes:decimal
        priceNo:decimal
        endDate:integer
        status:string
        resolved:bool
        resolutionOutcome:string
    )
    (defschema y-n-market-schema
        @doc "schema designated to account and maintain finulab's yes-or-no based prediction market"
        @model [
            (invariant (>= quantityYes 0.0))
            (invariant (>= quantityNo 0.0))
        ]
        predictiveQuestion:string
        outcome:string
        quantityYes:decimal
        quantityNo:decimal
        continuous:bool
        endDate:integer
        status:string
        resolved:bool
        resolutionOutcome:string
    )
    (defschema c-market-schema
        @doc "schema designated to account and maintain finulab's categorical based prediction market"
        @model [
            (invariant (>= quantityOne 0.0))
            (invariant (>= quantityTwo 0.0))
            (invariant (>= quantityThree 0.0))
            (invariant (>= quantityFour 0.0))
        ]
        predictiveQuestion:string
        outcomes:object
        quantityOne:decimal
        quantityTwo:decimal
        quantityThree:decimal
        quantityFour:decimal
        continuous:bool
        endDate:integer
        status:string
        resolved:bool
        resolutionOutcome:string
    )
    (defschema portfolio-schema
        @doc "schema designated to account and maintain each finux account's portfolio by market for yes-or-no based predictions"
        @model [
          (invariant (>= yesQuantity 0.0))
          (invariant (>= noQuantity 0.0))
        ]
        yesQuantity:decimal
        noQuantity:decimal
    )
    (defschema c-portfolio-schema
        @doc "schema designated to account and maintain each finux account's portfolio by market for categorical based predictions"
        @model [
            (invariant (>= quantityOne 0.0))
            (invariant (>= quantityTwo 0.0))
            (invariant (>= quantityThree 0.0))
            (invariant (>= quantityFour 0.0))
        ]
        quantityOne:decimal
        quantityTwo:decimal
        quantityThree:decimal
        quantityFour:decimal
    )
    (defschema collateralized-portfolio-schema
        @doc "schema designated to account and maintain each finux account's sale portfolio by market for yes-or-no based predictions"
        @model [
          (invariant (>= soldYesQuantity 0.0))
          (invariant (>= soldNoQuantity 0.0))
        ]
        soldYesQuantity:decimal
        soldYesCollateral:decimal
        soldNoQuantity:decimal
        soldNoCollateral:decimal
    )
    (defschema c-collateralized-portfolio-schema
        @doc "schema designated to account and maintain each finux account's sale portfolio by market for categorical based predictions"
        @model [
            (invariant (>= soldQuantityOne 0.0))
            (invariant (>= soldQuantityTwo 0.0))
            (invariant (>= soldQuantityThree 0.0))
            (invariant (>= soldQuantityFour 0.0))
        ]
        soldQuantityOne:decimal
        soldCollateralOne:decimal
        soldQuantityTwo:decimal
        soldCollateralTwo:decimal
        soldQuantityThree:decimal
        soldCollateralThree:decimal
        soldQuantityFour:decimal
        soldCollateralFour:decimal
    )

    ;; -----------------------------------------------------------------------------------------------------
    ;;  tables
    (deftable token-table:{token-schema})
    (deftable token-initialization-table:{token-initialization-schema})

    (deftable market-table:{market-schema})
    (deftable y-n-market-table:{y-n-market-schema})
    (deftable c-market-table:{c-market-schema})
    (deftable portfolio-table:{portfolio-schema})
    (deftable c-portfolio-table:{c-portfolio-schema})
    (deftable collateralized-portfolio-table:{collateralized-portfolio-schema})
    (deftable c-collateralized-portfolio-table:{c-collateralized-portfolio-schema})

    ;; -----------------------------------------------------------------------------------------------------
    ;;  constants
    (defconst finulab-bank:string "finulab-bank"
        @doc "treasury account established to initially hold all the tokens allocated to the users of finulab: 66.67% (i.e., 100M / 150M) of the supply. for more on our tokenomics, visit: finux.xyz"
    )
    (defconst finulab-marketing-bank:string "finulab-marketing-bank"
        @doc "treasury account established to initially hold all the tokens allocated to marketing finulab to the public: 5% (i.e., 7.5M / 150M) of the supply. for more on our tokenomics, visit: finux.xyz"
    )
    (defconst finulab-competition-bank:string "finulab-competition-bank"
        @doc "treasury account established to initially hold all the tokens allocated to finulab supported competitions for users: 5% (i.e., 7.5M / 150M) of the supply. for more on our tokenomics, visit: finux.xyz"
    )
    (defconst finulab-account-creation-bonus-bank:string "finulab-account-creation-bonus-bank"
        @doc "treasury account established to initially hold all the tokens allocated to the first users to create a finulab account: 16.67% (i.e., 25M / 150M) of the supply. for more on our tokenomics, visit: finux.xyz"
    )
    (defconst finulab-prediction-market-liquidity-pool:string "finulab-prediction-market-liquidity-pool"
        @doc "pool account established to support finulab's liquidity-sensitive LMSR based automated market maker for prediction markets: 6.63% (i.e., 10M / 150M) of the supply. for more on our tokenomics, visit: finux.xyz"
    )
    (defconst finulab-prediction-market-escrow-account:string "finulab-prediction-market-escrow-account"
        @doc "account established to manage all the balances put into finulab's prediction market"
    )
    (defconst finulab-prediction-market-collateral-account:string "finulab-prediction-market-collateral-account"
        @doc "account established to manage all the collateral posted to conduct sales in finulab's prediction market"
    )
    (defconst finulab-prediction-market-fees:string "finulab-prediction-market-fees"
        @doc "account established to manage a portion of the fees related to purchases on finulab's prediction market"
    )
    (defconst finulab-monetization-vault:string "finulab-monetization-vault"
        @doc "account established to hold and retain locked finux, so that monetized account on finulab can earn on every engagement"
    )
    (defconst finulab-subscriptions:string "finulab-subscriptions"
        @doc "account established to receive subscription fees paid on finulab for privileged accounts"
    )
    (defconst pi:decimal 3.1415926535
        @doc "pi to the 10th decimal; utilized in the delta-function scoped into the initialization of this contract. see initialize-contract function below"
    )
    (defconst token-supply:decimal 150000000.0
        @doc "initial and max supply of the finux token; for more on our tokenomics, visit: finux.xyz"
    )
    
    ;; -----------------------------------------------------------------------------------------------------
    ;;  interfaces
    (implements fungible-v2)
    
    ;; -----------------------------------------------------------------------------------------------------
    ;;  capabilities
    (defcap GOVERNANCE ()
        (enforce-keyset "free.finux-module-admin")
    )
    (defcap DEBIT (sender:string)
        @doc "capability to ensure that the sender is appropriately validated"
        (enforce (!= "" sender) "sender must be specified in order to perform a debit")
        (with-read token-table sender {"guard":=sender-guard}
            (if (contains sender [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
                (and
                    (enforce-guard sender-guard)
                    (enforce-keyset "free.finux-operations-admin")
                )
                (enforce-guard sender-guard)
            )
        )
    )
    (defcap CREDIT (receiver:string)
        @doc "capability to ensure that the receiver is appropriately validated"
        (enforce (!= "" receiver) "receiver must be specified in order to perform a credit")
    )
    (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
        @doc "capability to ensure that the transfer operation is appropriately validated"
        @managed amount TRANSFER-mgr
        
        (enforce (> amount 0.0) "transfer amount must be greater than 0.0")
        (enforce-unit amount)
        (enforce (!= sender receiver) "sender cannot match the receiver in a transfer operation")
        (compose-capability (DEBIT sender))
        (compose-capability (CREDIT receiver))
    )
    (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
        @doc "managed capability to control the transfer amount"
        (let ((new-balance (- managed requested)))
            (enforce (>= new-balance 0.0)
                (format "requested transfer amount has exceeded {}, i.e., amount approved for the transfer" [managed])
            )
            new-balance
        )
    )

    ;; -----------------------------------------------------------------------------------------------------
    ;;  functionality
    (defun validate-account-id (account-id:string)
        @doc "enforce an account name to meet the required format"
        (enforce (!= "" account-id) "account name must be specified, i.e., account name cannot be an empty string")
        (enforce (is-charset CHARSET_LATIN1 account-id) "account name must conform to the character set allowed under ISO-8859-1")
        (let ((account-id-length (length account-id)))
            (and
                (enforce (>= account-id-length 3) "account name must be at least 3 characters long")
                (enforce (<= account-id-length 256) "account name cannot be over 256 characters long")
            )
        )
    )
    (defun validate-account-for-credit-root (account-id:string account-guard:guard)
        @doc "ensure that a root account's guard is never modified in a credit operation"
        (enforce 
            (contains account-id [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
            "account name is invalid for the utilization of this function"
        )
        (if (= account-id finulab-bank)
            (enforce
                (=
                    (format "{}" [account-guard])
                    (format "KeySet {keys: [{}],pred: keys-all}" ["8a6d0f7488699d061fa8f6eb87cca6de9994d867435269987aa68bef88e038c9"])
                )
                "in order to credit a root account, the appropriate guard must be specified"
            )
            (if (= account-id finulab-marketing-bank)
                (enforce
                    (=
                        (format "{}" [account-guard])
                        (format "KeySet {keys: [{}],pred: keys-all}" ["c34808f8a13425eb5e21c0b4a14eadd89dae5558fd2a22affec24ccce7444586"])
                    )
                    "in order to credit a root account, the appropriate guard must be specified"
                )
                (if (= account-id finulab-competition-bank)
                    (enforce
                        (=
                            (format "{}" [account-guard])
                            (format "KeySet {keys: [{}],pred: keys-all}" ["4463abf6b912a677f941a46bad70c988dfc5801452ebb50e29cbc31bb61ac28c"])
                        )
                        "in order to credit a root account, the appropriate guard must be specified"
                    )
                    (if (= account-id finulab-account-creation-bonus-bank)
                        (enforce
                            (=
                                (format "{}" [account-guard])
                                (format "KeySet {keys: [{}],pred: keys-all}" ["e871fa64713c80d2a746942a4bd76c8ace913c3b156e5eea92e8bd50c6b21a54"])
                            )
                            "in order to credit a root account, the appropriate guard must be specified"
                        )
                        (if (= account-id finulab-prediction-market-liquidity-pool)
                            (enforce
                                (=
                                    (format "{}" [account-guard])
                                    (format "KeySet {keys: [{}],pred: keys-all}" ["37a6038bd13db5a53d96e0db0fc180dd93753e9e60f08761bbc97702399e0c95"])
                                )
                                "in order to credit a root account, the appropriate guard must be specified"
                            )
                            (if (= account-id finulab-prediction-market-escrow-account)
                                (enforce
                                    (=
                                        (format "{}" [account-guard])
                                        (format "KeySet {keys: [{}],pred: keys-all}" ["714a2a357b1b417fba468e05ee16fcfe3c2d6200ecb33673e5793b097130f711"])
                                    )
                                    "in order to credit a root account, the appropriate guard must be specified"
                                )
                                (if (= account-id finulab-prediction-market-collateral-account)
                                    (enforce
                                        (=
                                            (format "{}" [account-guard])
                                            (format "KeySet {keys: [{}],pred: keys-all}" ["44818c010cb7075bedd8e32adc61c3c7afe6f7e1da3df6aae07db005b787f159"])
                                        )
                                        "in order to credit a root account, the appropriate guard must be specified"
                                    )
                                    (if (= account-id finulab-prediction-market-fees)
                                        (enforce
                                            (=
                                                (format "{}" [account-guard])
                                                (format "KeySet {keys: [{}],pred: keys-all}" ["33693ab467270fcb86ec63e6313e04a6e403deac0244c45986491b71ae01a702"])
                                            )
                                            "in order to credit a root account, the appropriate guard must be specified"
                                        )
                                        (if (= account-id finulab-monetization-vault)
                                            (enforce
                                                (=
                                                    (format "{}" [account-guard])
                                                    (format "KeySet {keys: [{}],pred: keys-all}" ["d3eab7eb65171d827dc9a3667ee0ac86bba107d7a5331d79b180039541180fb8"])
                                                )
                                                "in order to credit a root account, the appropriate guard must be specified"
                                            )
                                            (enforce
                                                (=
                                                    (format "{}" [account-guard])
                                                    (format "KeySet {keys: [{}],pred: keys-all}" ["b43f3ce060c7b6b25225e01c281b24acb6772a6233b6c2486dd663f8fd5b7de5"])
                                                )
                                                "in order to credit a root account, the appropriate guard must be specified"
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ) 
        )
    )
    (defun validate-account-for-credit-non-root (account-id:string account-guard:guard)
        @doc "ensure that a non-root account's guard is never modified in a credit operation"
        (if (= (take 2 account-id) "k:")
            (let ((afx (drop 2 account-id)))
                (let ((publicKey (drop 15 (drop (- 17) (format "{}" [account-guard])))))
                    (enforce (not (contains "," publicKey)) 
                        "finux token contract does not support multiple keys, an account can only have one key"
                    )
                    (enforce (= afx publicKey)
                        "an account's name must start off with a 'k:', which must then be followed by the account's public key"
                    )
                )
            )
            (enforce (= 1 1) "matrix is broken")
        )
    )
    (defun enforce-unit:bool (amount:decimal)
        @doc "enforce minimum precision allowed for transactions"
        (enforce (= (floor amount 12) amount)
            "transfer amount's fractional part cannot exceed the maximum allowed decimal precision of 12"
        )
    )

    (defun create-account:string (account:string guard:guard)
        @doc "utility to create a new account"
        @model [
            (property (valid-account-id account))
        ]
        
        (validate-account-id account)
        (if (contains account [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
            (validate-account-for-credit-root account guard)
            (validate-account-for-credit-non-root account guard)
        )
        (insert token-table account {"guard":guard, "balance":0.0})
    )
    (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
        @doc "utility to transfer an amount to another account, creating the account if it does not exist"
        @model [
            (property (> amount 0.0))
            (property (!= "" sender))
            (property (!= "" receiver))
            (property (!= sender receiver))
            
            (property (conserves-mass amount))
            (property (valid-account-id sender))
            (property (valid-account-id receiver))
        ]
        
        (enforce (> amount 0.0) "transfer amount must be greater than 0.0")
        (enforce-unit amount)
        (validate-account-id sender)
        (validate-account-id receiver)
        (enforce (!= sender receiver) "sender and receiver cannot match in a transfer operation")
        (if (contains receiver [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
            (validate-account-for-credit-root receiver receiver-guard)
            (validate-account-for-credit-non-root receiver receiver-guard)
        )
        
        (with-capability (TRANSFER sender receiver amount)
            (require-capability (DEBIT sender))
            (with-read token-table sender {"guard":=sender-guard, "balance":=balance}
                (enforce-guard sender-guard)
                (enforce (>= balance amount) "insufficient balance to complete the transfer")
                (update token-table sender {"balance":(- balance amount)})
            )
            
            (require-capability (CREDIT receiver))
            (with-default-read token-table receiver 
                {"guard":receiver-guard, "balance":-1.0}
                {"guard":=retg, "balance":=balance}
                (enforce (= retg receiver-guard) "for a transfer operation, the receiver-guard must match that of the recipient")
                
                (let ((is-new (if (= -1.0 balance) true false)))
                    (write token-table receiver
                        {"guard":retg, "balance":(if is-new amount (+ balance amount))}
                    )
                )
            )
        )
    )
    (defun transfer:string (sender:string receiver:string amount:decimal)
        @doc "utility to transfer an amount to another account, failing if the account does not exist"
        @model [
            (property (> amount 0.0))
            (property (!= "" sender))
            (property (!= "" receiver))
            (property (!= sender receiver))
            
            (property (conserves-mass amount))
            (property (valid-account-id sender))
            (property (valid-account-id receiver))
        ]
        
        (with-read token-table receiver {"guard":=receiver-guard}
            (transfer-create sender receiver receiver-guard amount)
        )
    )
    (defpact transfer-crosschain:string (sender:string receiver:string receiver-guard:guard target-chain:string amount:decimal)
        @doc "utility to transfer an amount to another account cross-chain"
        @model [
            (property (> amount 0.0))
            (property (!= "" sender))
            (property (!= "" receiver))
            
            (property (valid-account-id sender))
            (property (valid-account-id receiver))
        ]
        
        (step 
            (with-capability (DEBIT sender)
                (enforce (> amount 0.0) "transfer amount must be greater than 0.0")
                (enforce-unit amount)
                (validate-account-id sender)
                (validate-account-id receiver)
                (enforce 
                    (contains target-chain ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"])
                    "specified target-chain is invalid, please select a valid chain id (i.e. 0 to 19)"
                )
                (enforce (!= (at 'chain-id (chain-data)) target-chain) "cannot run cross-chain transfers to the same chain")
                (if (contains receiver [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
                    (validate-account-for-credit-root receiver receiver-guard)
                    (validate-account-for-credit-non-root receiver receiver-guard)
                )
                
                ;; step 1: debit sender account on the current chain
                (with-read token-table sender {"guard":=sender-guard, "balance":=balance}
                    (enforce-guard sender-guard)
                    (enforce (>= balance amount) "insufficient balance to complete the transfer")
                    (update token-table sender {"balance":(- balance amount)})
                )
                
                (let 
                    ((crosschain-details:object{crosschain-schema}
                        {
                            "receiver":receiver,
                            "receiver-guard":receiver-guard,
                            "amount":amount
                        }
                    ))
                    (yield crosschain-details target-chain)
                )
            )
        )
        (step
            (resume
                {
                    "receiver":=receiver,
                    "receiver-guard":=receiver-guard,
                    "amount":=amount
                }
                
                ;; step 2: credit receiver account on target-chain
                (with-capability (CREDIT receiver)
                    (with-default-read token-table receiver
                        {"guard":receiver-guard, "balance":-1.0}
                        {"guard":=retg, "balance":=balance}
                        (enforce (= retg receiver-guard) "for a transfer operation, the receiver-guard must match that of the recipient")
                        
                        (let ((is-new (if (= -1.0 balance) true false)))
                            (write token-table receiver
                                {"guard":retg, "balance":(if is-new amount (+ balance amount))}
                            )
                        )
                    )
                )
            )
        )
    )
    
    (defun precision:integer ()
        @doc "returns the maximum allowed decimal precision"
        12
    )
    (defun get-balance:decimal (account:string)
        @doc "get balance for an account, failing if the account does not exist"
        (with-read token-table account {"balance":=balance}
            balance
        )
    )
    (defun details:object{fungible-v2.account-details} (account:string)
        @doc "utility to pull an account's general detail"
        (with-read token-table account {"guard":=account-guard, "balance":=balance}
            {
                "account":account,
                "balance":balance,
                "guard":account-guard
            }
        )
    )
    (defun rotate:string (account:string new-guard:guard)
        @doc "utility to rotate an entire account based on the new provided guard; note that this function will transition entire balance to the newly created account after enforcing the old guard. the new account's format will be of the form: current account's protocol ('k:') followed by the new public key. note that the finux token contract does not support multiple keys, an account can only have one key"
        (enforce
            (not (contains account [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions]))
            (format "{} is not authorized to call this function" [account])
        )
        (with-read token-table account {"guard":=old-guard, "balance":=balance}
            (enforce-guard old-guard)
            (enforce-guard new-guard)
            (let ((account-protocol (take 1 account)))
                (let ((publicKey (drop 15 (drop (- 17) (format "{}" [new-guard])))))
                    (let ((new-account (format "{}:{}" [account-protocol publicKey])))
                        (create-account new-account new-guard)
                        (update token-table new-account {"balance":balance})
                        (update token-table account {"balance":0.0})
                    )
                )
            )
        )
    )
    (defun rotate-root:string (account:string new-guard:guard)
        @doc "utility to rotate the keyset of a root account"
        (enforce-keyset "free.finux-operations-admin")
        (enforce 
            (contains account [finulab-bank finulab-marketing-bank finulab-competition-bank finulab-account-creation-bonus-bank finulab-prediction-market-liquidity-pool finulab-prediction-market-escrow-account finulab-prediction-market-collateral-account finulab-prediction-market-fees finulab-monetization-vault finulab-subscriptions])
            "account name is invalid for the utilization of this function"
        )
        (with-read token-table account {"guard":=old-guard, "balance":=balance}
            (enforce-guard old-guard)
            (enforce-guard new-guard)
            (update token-table account {"guard":new-guard, "balance":balance})
        )
    )

    (defun initialize-contract:string ()
        (with-default-read token-initialization-table "contract" {"initialized":false} {"initialized":=initialized-val}
            (enforce (!= initialized-val true) "initialize-contract function has expired")
            
            (with-capability (GOVERNANCE)
                (create-account finulab-bank (read-keyset "finulab-bank"))
                
                (let ((chain-val (str-to-int (at 'chain-id (chain-data)))))
                    (let ((delta-function-val (floor (exp (- (^ (* (sqrt pi) (- chain-val 3)) 2))))))
                        (update token-table finulab-bank 
                            {"balance":(* delta-function-val token-supply)}
                        )
                    )
                )
                (write token-initialization-table "contract" {"initialized":true})
            )
        )
    )
    
    (defun market-tx-validation (tx-desc:object)
        @doc "validation to ensure all market related transactions are correctly structured"
        (enforce-keyset "free.finux-operations-admin")

        (enforce (> (at "cost" tx-desc) 0.0) "tx cost must be a decimal greater than 0.0")
        (enforce-unit (at "cost" tx-desc))

        (enforce (> (at "quantity" tx-desc) 0.0) "quantity must be a decimal greater than 0.0")
        (enforce-unit (at "quantity" tx-desc))
    )

    (defun y-n-create-market (account:string market-id:string predictive-question:string outcome:string continuous:bool end-date:integer market-desc:object)
        @doc "functionality to create a yes or no market related to a prediction"
        (enforce-keyset "free.finux-operations-admin")

        (enforce (> (at "cost" market-desc) 0.0) "tx cost must be greater than 0.0")
        (enforce-unit (at "cost" market-desc))

        (enforce-unit (at "quantity-no" market-desc))
        (enforce-unit (at "quantity-yes" market-desc))
        (enforce (>= (at "quantity-no" market-desc) 0.0) "quantity-no must be a decimal greater than or equal to 0.0")
        (enforce (>= (at "quantity-yes" market-desc) 0.0) "quantity-yes must be a decimal greater than or equal to 0.0")
        (enforce (> (+ (at "quantity-yes" market-desc) (at "quantity-no" market-desc)) 0.0) "sum of quantity-yes and quantity-no must be greater than 0.0")

        (with-read token-table finulab-prediction-market-escrow-account {"guard":=escrow-guard}
            (transfer-create account finulab-prediction-market-escrow-account escrow-guard (at "cost" market-desc))
        )
        (insert y-n-market-table market-id {"predictiveQuestion":predictive-question, "outcome":outcome, "quantityYes":(at "quantity-yes" market-desc), "quantityNo":(at "quantity-no" market-desc), "continuous":continuous, "endDate":end-date, "status":"live", "resolved":false, "resolutionOutcome":""})
        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (insert portfolio-table portfolio-id {"yesQuantity":(at "quantity-yes" market-desc), "noQuantity":(at "quantity-no" market-desc)})
        )
    )
    (defun c-create-market (account:string market-id:string predictive-question:string outcomes:object continuous:bool end-date:integer market-desc:object)
        @doc "functionality to create a categorical market related to a prediction"
        (enforce-keyset "free.finux-operations-admin")

        (enforce (> (at "cost" market-desc) 0.0) "tx cost must be greater than 0.0")
        (enforce-unit (at "cost" market-desc))

        (enforce (>= (length outcomes) 2) "there must be at least 2 outcomes per a categorical market")
        (enforce (<= (length outcomes) 4) "at most there can only be 4 outcomes per a categorical market")

        (let ((outcomes-length (length outcomes)))
            (if (= outcomes-length 2)
                (and
                    (enforce (contains "outcomeOne" outcomes) "outcomes object must be properly structured")
                    (enforce (contains "outcomeTwo" outcomes) "outcomes object must be properly structured")
                )
                (if (= outcomes-length 3)
                    (and
                        (enforce (contains "outcomeOne" outcomes) "outcomes object must be properly structured")
                        (and
                            (enforce (contains "outcomeTwo" outcomes) "outcomes object must be properly structured")
                            (enforce (contains "outcomeThree" outcomes) "outcomes object must be properly structured")
                        )
                    )
                    (and
                        (enforce (contains "outcomeOne" outcomes) "outcomes object must be properly structured")
                        (and
                            (enforce (contains "outcomeTwo" outcomes) "outcomes object must be properly structured")
                            (and
                                (enforce (contains "outcomeThree" outcomes) "outcomes object must be properly structured")
                                (enforce (contains "outcomeFour" outcomes) "outcomes object must be properly structured")
                            )
                        )
                    )
                )
            )

            (if (= outcomes-length 2)
                (and
                    (enforce (>= (at "quantityOne" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                    (and
                        (enforce (>= (at "quantityTwo" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                        (enforce (> (+ (at "quantityOne" market-desc) (at "quantityTwo" market-desc)) 0.0) "sum of all outcome quantities must be greater than 0.0")
                    )
                )
                (if (= outcomes-length 3)
                    (and
                        (enforce (>= (at "quantityOne" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                        (and
                            (enforce (>= (at "quantityTwo" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                            (and
                                (enforce (>= (at "quantityThree" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                                (enforce (> (+ (at "quantityOne" market-desc) (+ (at "quantityTwo" market-desc) (at "quantityThree" market-desc))) 0.0) "sum of all outcome quantities must be greater than 0.0")
                            )
                        )
                    )
                    (and
                        (enforce (>= (at "quantityOne" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                        (and
                            (enforce (>= (at "quantityTwo" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                            (and
                                (enforce (>= (at "quantityThree" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                                (and
                                    (enforce (>= (at "quantityFour" market-desc) 0.0) "all outcome quantities must be greater than or equal to 0.0")
                                    (enforce (> (+ (at "quantityOne" market-desc) (+ (at "quantityTwo" market-desc) (+ (at "quantityThree" market-desc) (at "quantityFour" market-desc)))) 0.0) "sum of all outcome quantities must be greater than 0.0")
                                )
                            )
                        )
                    )
                )
            )

            (if (= outcomes-length 2)
                (and
                    (enforce-unit (at "quantityOne" market-desc))
                    (enforce-unit (at "quantityTwo" market-desc))
                )
                (if (= outcomes-length 3)
                    (and
                        (enforce-unit (at "quantityOne" market-desc))
                        (and
                            (enforce-unit (at "quantityTwo" market-desc))
                            (enforce-unit (at "quantityThree" market-desc))
                        )
                    )
                    (and
                        (enforce-unit (at "quantityOne" market-desc))
                        (and
                            (enforce-unit (at "quantityTwo" market-desc))
                            (and
                                (enforce-unit (at "quantityThree" market-desc))
                                (enforce-unit (at "quantityFour" market-desc))
                            )
                        )
                    )
                )
            )
        )

        (with-read token-table finulab-prediction-market-escrow-account {"guard":=escrow-guard}
            (transfer-create account finulab-prediction-market-escrow-account escrow-guard (at "cost" market-desc))
        )
        (insert c-market-table market-id {"predictiveQuestion":predictive-question, "outcomes":outcomes, "quantityOne":(at "quantityOne" market-desc), "quantityTwo":(at "quantityTwo" market-desc), "quantityThree":(at "quantityThree" market-desc), "quantityFour":(at "quantityFour" market-desc), "continuous":continuous, "endDate":end-date, "status":"live", "resolved":false, "resolutionOutcome":""})
        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (insert c-portfolio-table portfolio-id {"quantityOne":(at "quantityOne" market-desc), "quantityTwo":(at "quantityTwo" market-desc), "quantityThree":(at "quantityThree" market-desc), "quantityFour":(at "quantityFour" market-desc)})
        )
    )
    
    (defun y-n-portfolio-credit (account:string market-id:string type:string tx-desc:object supportive-desc:object)
        @doc "functionality to purchase yes or no outcomes"
        (market-tx-validation tx-desc)

        (enforce (>= (at "fee" supportive-desc) 0.0) "tx fee must be greater than or equal to 0.0")
        (enforce-unit (at "fee" supportive-desc))
        (if (> (at "fee" supportive-desc) 0.0)
            (with-read token-table finulab-prediction-market-fees {"guard":=fees-guard}
                (transfer-create account finulab-prediction-market-fees fees-guard (at "fee" supportive-desc))
            )
            (enforce (= 1 1) "matrix is broken")
        )

        (enforce (>= (at "creator-fee" supportive-desc) 0.0) "creator's portion of fee must be greater than or equal to 0.0")
        (enforce-unit (at "creator-fee" supportive-desc))
        (if (> (at "creator-fee" supportive-desc) 0.0)
            (with-read token-table (at "creator-address" supportive-desc) {"guard":=creator-guard}
                (transfer-create account (at "creator-address" supportive-desc) creator-guard (at "creator-fee" supportive-desc))
            )
            (enforce (= 1 1) "matrix is broken")
        )

        (with-read token-table finulab-prediction-market-escrow-account {"guard":=escrow-guard}
            (transfer-create account finulab-prediction-market-escrow-account escrow-guard (at "cost" tx-desc))
        )

        (if (= type "yes")
            (with-default-read y-n-market-table market-id
                {"predictiveQuestion":"", "outcome":"", "quantityYes":-1.0, "quantityNo":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                {"predictiveQuestion":=p-q, "outcome":=o, "quantityYes":=q-y, "quantityNo":=q-n, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                (let ((is-new (if (= -1.0 q-y) true false)))
                    (if is-new
                        (write y-n-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcome":(at "outcome" supportive-desc), "quantityYes":(at "quantity" tx-desc), "quantityNo":0.0, "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                        (write y-n-market-table market-id {"predictiveQuestion":p-q, "outcome":o, "quantityYes":(+ q-y (at "quantity" tx-desc)), "quantityNo":q-n, "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                    )
                )
            )
            (with-default-read y-n-market-table market-id
                {"predictiveQuestion":"", "outcome":"", "quantityYes":-1.0, "quantityNo":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                {"predictiveQuestion":=p-q, "outcome":=o, "quantityYes":=q-y, "quantityNo":=q-n, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                (let ((is-new (if (= -1.0 q-n) true false)))
                    (if is-new
                        (write y-n-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcome":(at "outcome" supportive-desc), "quantityYes":0.0, "quantityNo":(at "quantity" tx-desc), "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                        (write y-n-market-table market-id {"predictiveQuestion":p-q, "outcome":o, "quantityYes":q-y, "quantityNo":(+ q-n (at "quantity" tx-desc)), "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                    )
                )
            )
        )
        
        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (if (= type "yes")
                (with-default-read portfolio-table portfolio-id
                    {"yesQuantity":-1.0, "noQuantity":-1.0}
                    {"yesQuantity":=y-q, "noQuantity":=n-q}
                    (let ((is-account-new (if (= -1.0 y-q) true false)))
                        (write portfolio-table portfolio-id {"yesQuantity":(if is-account-new (at "quantity" tx-desc) (+ y-q (at "quantity" tx-desc))), "noQuantity":(if is-account-new 0.0 n-q)})
                    )
                )
                (with-default-read portfolio-table portfolio-id
                    {"yesQuantity":-1.0, "noQuantity":-1.0}
                    {"yesQuantity":=y-q, "noQuantity":=n-q}
                    (let ((is-account-new (if (= -1.0 n-q) true false)))
                        (write portfolio-table portfolio-id {"yesQuantity":(if is-account-new 0.0 y-q), "noQuantity":(if is-account-new (at "quantity" tx-desc) (+ n-q (at "quantity" tx-desc)))})
                    )
                )
            )
        )
    )
    (defun c-portfolio-credit (account:string market-id:string type:string tx-desc:object supportive-desc:object)
        @doc "functionality to purchase categorical outcomes"
        (market-tx-validation tx-desc)

        (enforce (>= (at "fee" supportive-desc) 0.0) "tx fee must be greater than or equal to 0.0")
        (enforce-unit (at "fee" supportive-desc))
        (if (> (at "fee" supportive-desc) 0.0)
            (with-read token-table finulab-prediction-market-fees {"guard":=fees-guard}
                (transfer-create account finulab-prediction-market-fees fees-guard (at "fee" supportive-desc))
            )
            (enforce (= 1 1) "matrix is broken")
        )

        (enforce (>= (at "creator-fee" supportive-desc) 0.0) "creator's portion of fee must be greater than or equal to 0.0")
        (enforce-unit (at "creator-fee" supportive-desc))
        (if (> (at "creator-fee" supportive-desc) 0.0)
            (with-read token-table (at "creator-address" supportive-desc) {"guard":=creator-guard}
                (transfer-create account (at "creator-address" supportive-desc) creator-guard (at "creator-fee" supportive-desc))
            )
            (enforce (= 1 1) "matrix is broken")
        )

        (with-read token-table finulab-prediction-market-escrow-account {"guard":=escrow-guard}
            (transfer-create account finulab-prediction-market-escrow-account escrow-guard (at "cost" tx-desc))
        )

        (if (= type "outcomeOne")
            (with-default-read c-market-table market-id
                {"predictiveQuestion":"", "outcomes":{}, "quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                {"predictiveQuestion":=p-q, "outcomes":=o, "quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                (let ((is-new (if (= -1.0 q-o) true false)))
                    (if is-new
                        (write c-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcomes":(at "outcomes" supportive-desc), "quantityOne":(at "quantity" tx-desc), "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0, "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                        (write c-market-table market-id {"predictiveQuestion":p-q, "outcomes":o, "quantityOne":(+ q-o (at "quantity" tx-desc)), "quantityTwo":q-tw, "quantityThree":q-th, "quantityFour":q-f, "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                    )
                )
            )
            (if (= type "outcomeTwo")
                (with-default-read c-market-table market-id
                    {"predictiveQuestion":"", "outcomes":{}, "quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                    {"predictiveQuestion":=p-q, "outcomes":=o, "quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                    (let ((is-new (if (= -1.0 q-tw) true false)))
                        (if is-new
                            (write c-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcomes":(at "outcomes" supportive-desc), "quantityOne":0.0, "quantityTwo":(at "quantity" tx-desc), "quantityThree":0.0, "quantityFour":0.0, "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                            (write c-market-table market-id {"predictiveQuestion":p-q, "outcomes":o, "quantityOne":q-o, "quantityTwo":(+ q-tw (at "quantity" tx-desc)), "quantityThree":q-th, "quantityFour":q-f, "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                        )
                    )
                )
                (if (= type "outcomeThree")
                    (with-default-read c-market-table market-id
                        {"predictiveQuestion":"", "outcomes":{}, "quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                        {"predictiveQuestion":=p-q, "outcomes":=o, "quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                        (let ((is-new (if (= -1.0 q-th) true false)))
                            (if is-new
                                (write c-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcomes":(at "outcomes" supportive-desc), "quantityOne":0.0, "quantityTwo":0.0, "quantityThree":(at "quantity" tx-desc), "quantityFour":0.0, "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                                (write c-market-table market-id {"predictiveQuestion":p-q, "outcomes":o, "quantityOne":q-o, "quantityTwo":q-tw, "quantityThree":(+ q-th (at "quantity" tx-desc)), "quantityFour":q-f, "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                            )
                        )
                    )
                    (with-default-read c-market-table market-id
                        {"predictiveQuestion":"", "outcomes":{}, "quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0, "continuous":false, "endDate":0, "status":"", "resolved":false, "resolutionOutcome":""}
                        {"predictiveQuestion":=p-q, "outcomes":=o, "quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "continuous":=c, "endDate":=e-d, "status":=s, "resolved":=r, "resolutionOutcome":=r-o}
                        (let ((is-new (if (= -1.0 q-f) true false)))
                            (if is-new
                                (write c-market-table market-id {"predictiveQuestion":(at "predictive-question" supportive-desc), "outcomes":(at "outcomes" supportive-desc), "quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":(at "quantity" tx-desc), "continuous":(at "continuous" supportive-desc), "endDate":(at "end-date" supportive-desc), "status":"live", "resolved":false, "resolutionOutcome":""})
                                (write c-market-table market-id {"predictiveQuestion":p-q, "outcomes":o, "quantityOne":q-o, "quantityTwo":q-tw, "quantityThree":q-th, "quantityFour":(+ q-f (at "quantity" tx-desc)), "continuous":c, "endDate":e-d, "status":s, "resolved":r, "resolutionOutcome":r-o})
                            )
                        )
                    )
                )
            )
        )

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (if (= type "outcomeOne")
                (with-default-read c-portfolio-table portfolio-id
                    {"quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0}
                    {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                    (let ((is-account-new (if (= -1.0 q-o) true false)))
                        (write c-portfolio-table portfolio-id {"quantityOne":(if is-account-new (at "quantity" tx-desc) (+ q-o (at "quantity" tx-desc))), "quantityTwo":(if is-account-new 0.0 q-tw), "quantityThree":(if is-account-new 0.0 q-th), "quantityFour":(if is-account-new 0.0 q-f)})
                    )
                )
                (if (= type "outcomeTwo")
                    (with-default-read c-portfolio-table portfolio-id
                        {"quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0}
                        {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                        (let ((is-account-new (if (= -1.0 q-tw) true false)))
                            (write c-portfolio-table portfolio-id {"quantityOne":(if is-account-new 0.0 q-o), "quantityTwo":(if is-account-new (at "quantity" tx-desc) (+ q-tw (at "quantity" tx-desc))), "quantityThree":(if is-account-new 0.0 q-th), "quantityFour":(if is-account-new 0.0 q-f)})
                        )
                    )
                    (if (= type "outcomeThree")
                        (with-default-read c-portfolio-table portfolio-id
                            {"quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0}
                            {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                            (let ((is-account-new (if (= -1.0 q-th) true false)))
                                (write c-portfolio-table portfolio-id {"quantityOne":(if is-account-new 0.0 q-o), "quantityTwo":(if is-account-new 0.0 q-tw), "quantityThree":(if is-account-new (at "quantity" tx-desc) (+ q-th (at "quantity" tx-desc))), "quantityFour":(if is-account-new 0.0 q-f)})
                            )
                        )
                        (with-default-read c-portfolio-table portfolio-id
                            {"quantityOne":-1.0, "quantityTwo":-1.0, "quantityThree":-1.0, "quantityFour":-1.0}
                            {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                            (let ((is-account-new (if (= -1.0 q-f) true false)))
                                (write c-portfolio-table portfolio-id {"quantityOne":(if is-account-new 0.0 q-o), "quantityTwo":(if is-account-new 0.0 q-tw), "quantityThree":(if is-account-new 0.0 q-th), "quantityFour":(if is-account-new (at "quantity" tx-desc) (+ q-f (at "quantity" tx-desc)))})
                            )
                        )
                    )
                )
            )
        )
    )

    (defun y-n-portfolio-debit (account:string market-id:string type:string tx-desc:object)
        @doc "functionality to sell yes or no outcomes"
        (market-tx-validation tx-desc)
        (with-read y-n-market-table market-id {"quantityYes":=q-y, "quantityNo":=q-n, "status":=status}
            (enforce (= status "live") "market is closed")
            (if (= type "yes")
                (enforce (>= (- q-y (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
                (enforce (>= (- q-n (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
            )
        )

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (if (= type "yes")
                (with-read portfolio-table portfolio-id {"yesQuantity":=y-q}
                    (enforce (>= y-q (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                    (with-read token-table account {"guard":=account-guard}
                        (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                    )
                    (update portfolio-table portfolio-id {"yesQuantity":(- y-q (at "quantity" tx-desc))})
                )
                (with-read portfolio-table portfolio-id {"noQuantity":=n-q}
                    (enforce (>= n-q (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                    (with-read token-table account {"guard":=account-guard}
                        (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                    )
                    (update portfolio-table portfolio-id {"noQuantity":(- n-q (at "quantity" tx-desc))})
                )
            )
        )

        (if (= type "yes")
            (with-read y-n-market-table market-id {"quantityYes":=q-y}
                (update y-n-market-table market-id {"quantityYes":(- q-y (at "quantity" tx-desc))})
            )
            (with-read y-n-market-table market-id {"quantityNo":=q-n}
                (update y-n-market-table market-id {"quantityNo":(- q-n (at "quantity" tx-desc))})
            )
        )
    )
    (defun c-portfolio-debit (account:string market-id:string type:string tx-desc:object)
        @doc "functionality to sell categorical outcomes"
        (market-tx-validation tx-desc)
        (with-read c-market-table market-id {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "status":=status}
            (enforce (= status "live") "market is closed")
            (if (= type "outcomeOne")
                (enforce (>= (- q-o (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
                (if (= type "outcomeTwo")
                    (enforce (>= (- q-tw (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
                    (if (= type "outcomeThree")
                        (enforce (>= (- q-th (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
                        (enforce (>= (- q-f (at "quantity" tx-desc)) 0.0) "insufficient market balance to complete the tx")
                    )
                )
            )
        )

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (if (= type "outcomeOne")
                (with-read c-portfolio-table portfolio-id {"quantityOne":=q-o}
                    (enforce (>= q-o (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                    (with-read token-table account {"guard":=account-guard}
                        (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                    )
                    (update c-portfolio-table portfolio-id {"quantityOne":(- q-o (at "quantity" tx-desc))})
                )
                (if (= type "outcomeTwo")
                    (with-read c-portfolio-table portfolio-id {"quantityTwo":=q-tw}
                        (enforce (>= q-tw (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                        (with-read token-table account {"guard":=account-guard}
                            (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                        )
                        (update c-portfolio-table portfolio-id {"quantityTwo":(- q-tw (at "quantity" tx-desc))})
                    )
                    (if (= type "outcomeThree")
                        (with-read c-portfolio-table portfolio-id {"quantityThree":=q-th}
                            (enforce (>= q-th (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                            (with-read token-table account {"guard":=account-guard}
                                (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                            )
                            (update c-portfolio-table portfolio-id {"quantityThree":(- q-th (at "quantity" tx-desc))})
                        )
                        (with-read c-portfolio-table portfolio-id {"quantityFour":=q-f}
                            (enforce (>= q-f (at "quantity" tx-desc)) "insufficient portfolio balance to complete the tx")
                            (with-read token-table account {"guard":=account-guard}
                                (transfer-create finulab-prediction-market-escrow-account account account-guard (at "cost" tx-desc))
                            )
                            (update c-portfolio-table portfolio-id {"quantityFour":(- q-f (at "quantity" tx-desc))})
                        )
                    )
                )
            )
        )

        (if (= type "outcomeOne")
            (with-read c-market-table market-id {"quantityOne":=q-o}
                (update c-market-table market-id {"quantityOne":(- q-o (at "quantity" tx-desc))})
            )
            (if (= type "outcomeTwo")
                (with-read c-market-table market-id {"quantityTwo":=q-tw}
                    (update c-market-table market-id {"quantityTwo":(- q-tw (at "quantity" tx-desc))})
                )
                (if (= type "outcomeThree")
                    (with-read c-market-table market-id {"quantityThree":=q-th}
                        (update c-market-table market-id {"quantityThree":(- q-th (at "quantity" tx-desc))})
                    )
                    (with-read c-market-table market-id {"quantityFour":=q-f}
                        (update c-market-table market-id {"quantityFour":(- q-f (at "quantity" tx-desc))})
                    )
                )
            )
        )
    )

    (defun y-n-close-market (market-id:string)
        @doc "functionality to close a yes or no market related to a prediction"
        (enforce-keyset "free.finux-operations-admin")
        
        (with-read y-n-market-table market-id {"continuous":=continuous, "status":=status}
            (enforce (= status "live") (format "cannot close a {} market" [status]))
            (enforce (not continuous) "cannot close a continuous market")
            (update y-n-market-table market-id {"status":"ended"})
        )
    )
    (defun c-close-market (market-id:string)
        @doc "functionality to close a categorical market related to a prediction"
        (enforce-keyset "free.finux-operations-admin")
        
        (with-read c-market-table market-id {"continuous":=continuous, "status":=status}
            (enforce (= status "live") (format "cannot close a {} market" [status]))
            (enforce (not continuous) "cannot close a continuous market")
            (update c-market-table market-id {"status":"ended"})
        )
    )
    
    (defun y-n-resolve-market (market-id:string resolution-outcome:string)
        @doc "functionality to resolve a yes or no market related to a prediction"
        (enforce-keyset "free.finux-operations-admin")
        (enforce
            (contains resolution-outcome ["yes" "no"])
            "resolution-outcome is invalid for the utilization of this function"
        )

        (with-read y-n-market-table market-id {"status":=status}
            (enforce (= status "ended") (format "cannot resolve a {} market" [status]))
            (update y-n-market-table market-id {"status":"resolved", "resolved":true, "resolutionOutcome":resolution-outcome})
        )
    )
    (defun c-resolve-market (market-id:string resolution-outcome:string)
        @doc "functionality to resolve a categorical market related to a prediction"
        (enforce-keyset "free.finux-operations-admin")

        (with-read c-market-table market-id {"outcomes":=outcomes, "status":=status}
            (enforce (= status "ended") (format "cannot resolve a {} market" [status]))
            (enforce
                (contains resolution-outcome outcomes)
                "resolution-outcome is invalid for the utilization of this function"
            )
            (update c-market-table market-id {"status":"resolved", "resolved":true, "resolutionOutcome":resolution-outcome})
        )
    )
    
    (defun y-n-resolve-portfolio (account:string market-id:string)
        @doc "functionality to resolve an account's portfolio related to a yes or no market"
        (enforce-keyset "free.finux-operations-admin")

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (with-read portfolio-table portfolio-id {"yesQuantity":=y-q, "noQuantity":=n-q}
                (with-read y-n-market-table market-id {"quantityYes":=q-y, "quantityNo":=q-n, "resolved":=rslvd, "resolutionOutcome":=r-o}
                    (enforce rslvd "market has not yet resolved")

                    (if (= r-o "yes")
                        (if (> y-q 0.0)
                            (with-read token-table account {"guard":=account-guard}
                                (enforce (>= q-y y-q) "insufficient market balance to complete the tx")
                                (transfer-create finulab-prediction-market-escrow-account account account-guard y-q)
                                (update portfolio-table portfolio-id {"yesQuantity":0.0, "noQuantity":0.0})
                            )
                            (update portfolio-table portfolio-id {"yesQuantity":0.0, "noQuantity":0.0})
                        )
                        (if (> n-q 0.0)
                            (with-read token-table account {"guard":=account-guard}
                                (enforce (>= q-n n-q) "insufficient market balance to complete the tx")
                                (transfer-create finulab-prediction-market-escrow-account account account-guard n-q)
                                (update portfolio-table portfolio-id {"yesQuantity":0.0, "noQuantity":0.0})
                            )
                            (update portfolio-table portfolio-id {"yesQuantity":0.0, "noQuantity":0.0})
                        )
                    )

                    (update y-n-market-table market-id {"quantityYes":(- q-y y-q), "quantityNo":(- q-n n-q)})
                )
            )
        )
    )
    (defun c-resolve-portfolio (account:string market-id:string)
        @doc "functionality to resolve an account's portfolio related to a categorical market"
        (enforce-keyset "free.finux-operations-admin")

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (with-read c-portfolio-table portfolio-id {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                (with-read c-market-table market-id {"quantityOne":=mq-o, "quantityTwo":=mq-tw, "quantityThree":=mq-th, "quantityFour":=mq-f, "resolved":=rslvd, "resolutionOutcome":=r-o}
                    (enforce rslvd "market has not yet resolved")

                    (if (= r-o "outcomeOne")
                        (if (> q-o 0.0)
                            (with-read token-table account {"guard":=account-guard}
                                (enforce (>= mq-o q-o) "insufficient market balance to complete the tx")
                                (transfer-create finulab-prediction-market-escrow-account account account-guard q-o)
                                (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                            )
                            (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                        )
                        (if (= r-o "outcomeTwo")
                            (if (> q-tw 0.0)
                                (with-read token-table account {"guard":=account-guard}
                                    (enforce (>= mq-tw q-tw) "insufficient market balance to complete the tx")
                                    (transfer-create finulab-prediction-market-escrow-account account account-guard q-tw)
                                    (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                                )
                                (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                            )
                            (if (= r-o "outcomeThree")
                                (if (> q-th 0.0)
                                    (with-read token-table account {"guard":=account-guard}
                                        (enforce (>= mq-th q-th) "insufficient market balance to complete the tx")
                                        (transfer-create finulab-prediction-market-escrow-account account account-guard q-th)
                                        (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                                    )
                                    (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                                )
                                (if (> q-f 0.0)
                                    (with-read token-table account {"guard":=account-guard}
                                        (enforce (>= mq-f q-f) "insufficient market balance to complete the tx")
                                        (transfer-create finulab-prediction-market-escrow-account account account-guard q-f)
                                        (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                                    )
                                    (update c-portfolio-table portfolio-id {"quantityOne":0.0, "quantityTwo":0.0, "quantityThree":0.0, "quantityFour":0.0})
                                )
                            )
                        )
                    )

                    (update c-market-table market-id {"quantityOne":(- mq-o q-o), "quantityTwo":(- mq-tw q-tw), "quantityThree":(- mq-th q-th), "quantityFour":(- mq-f q-f)})
                )
            )
        )
    )
    
    (defun y-n-read-market (market-id:string)
        @doc "utility to pull a yes or no market's details"
        (with-read y-n-market-table market-id {"predictiveQuestion":=p-q, "outcome":=outcome, "quantityYes":=q-y, "quantityNo":=q-n, "continuous":=cont, "endDate":=e-d, "status":=status, "resolved":=rslvd, "resolutionOutcome":=rslvd-o}
            {
                "predictiveQuestion":p-q,
                "outcome":outcome,
                "quantityYes":q-y,
                "quantityNo":q-n,
                "continuous":cont,
                "endDate":e-d,
                "status":status,
                "resolved":rslvd,
                "resolutionOutcome":rslvd-o
            }
        )
    )
    (defun c-read-market (market-id:string)
        @doc "utility to pull a categorical market's details"
        (with-read c-market-table market-id {"predictiveQuestion":=p-q, "outcomes":=outcomes, "quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f, "continuous":=cont, "endDate":=e-d, "status":=status, "resolved":=rslvd, "resolutionOutcome":=rslvd-o}
            (let ((outcomes-length (length outcomes)))
                (if (= outcomes-length 2)
                    {
                        "predictiveQuestion":p-q,
                        "outcomes":[(at "outcomeOne" outcomes) (at "outcomeTwo" outcomes)],
                        "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw]],
                        "continuous":cont,
                        "endDate":e-d,
                        "status":status,
                        "resolved":rslvd,
                        "resolutionOutcome":rslvd-o
                    }
                    (if (= outcomes-length 3)
                        {
                            "predictiveQuestion":p-q,
                            "outcomes":[(at "outcomeOne" outcomes) (at "outcomeTwo" outcomes) (at "outcomeThree" outcomes)],
                            "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw] [(at "outcomeThree" outcomes) q-th]],
                            "continuous":cont,
                            "endDate":e-d,
                            "status":status,
                            "resolved":rslvd,
                            "resolutionOutcome":rslvd-o
                        }
                        {
                            "predictiveQuestion":p-q,
                            "outcomes":[(at "outcomeOne" outcomes) (at "outcomeTwo" outcomes) (at "outcomeThree" outcomes) (at "outcomeFour" outcomes)],
                            "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw] [(at "outcomeThree" outcomes) q-th] [(at "outcomeFour" outcomes) q-f]],
                            "continuous":cont,
                            "endDate":e-d,
                            "status":status,
                            "resolved":rslvd,
                            "resolutionOutcome":rslvd-o
                        }
                    )
                )
            )
        )
    )

    (defun y-n-read-portfolio (account:string market-id:string)
        @doc "utility to pull a portfolio's details related to a yes or no market"

        (let ((portfolio-id (format "{}-{}" [account market-id])))
            (with-read portfolio-table portfolio-id {"yesQuantity":=y-q, "noQuantity":=n-q}
                {
                    "yesQuantity":y-q, 
                    "noQuantity":n-q
                }
            )
        )
    )
    (defun c-read-portfolio (account:string market-id:string)
        @doc "utility to pull a portfolio's details related to a categorical market"

        (with-read c-market-table market-id {"outcomes":=outcomes}
            (let ((outcomes-length (length outcomes)))
                (let ((portfolio-id (format "{}-{}" [account market-id])))
                    (with-read c-portfolio-table portfolio-id {"quantityOne":=q-o, "quantityTwo":=q-tw, "quantityThree":=q-th, "quantityFour":=q-f}
                        (if (= outcomes-length 2)
                            {
                                "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw]]
                            }
                            (if (= outcomes-length 3)
                                {
                                    "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw] [(at "outcomeThree" outcomes) q-th]]
                                }
                                {
                                    "quantities":[[(at "outcomeOne" outcomes) q-o] [(at "outcomeTwo" outcomes) q-tw] [(at "outcomeThree" outcomes) q-th] [(at "outcomeFour" outcomes) q-f]]
                                }
                            )
                        )
                    )
                )
            )
        )
    )
    
    (defun migrate-market-table (market-id:string)
        @doc "functionality to migrate finulab's prediction market from v1 to v2"
        (enforce-keyset "free.finux-operations-admin")

        (with-read market-table market-id {"predictiveQuestion":=p-q, "outcome":=outcome, "quantityYes":=q-y, "quantityNo":=q-n, "endDate":=e-d, "status":=status, "resolved":=rslvd, "resolutionOutcome":=rslvd-o}
            (insert y-n-market-table market-id {"predictiveQuestion":p-q, "outcome":outcome, "quantityYes":q-y, "quantityNo":q-n, "continuous":false, "endDate":e-d, "status":status, "resolved":rslvd, "resolutionOutcome":rslvd-o})
        )
    )
)
