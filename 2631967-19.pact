(module fin-us GOVERNANCE
    @doc "governing computational token contract implemented across all fin-us, inc. platforms"
    @model [
        (defproperty conserves-mass (amount:decimal)
            (= (column-delta token-table 'total-balance) 0.0)
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
    (defschema account-details-schema
        @doc "schema designated to pull accounts' information in details; see details-in-depth function below"
        @model [
            (invariant (!= "" account))
            (invariant (>= balance-a 0.0))
            (invariant (>= balance-b 0.0))
            (invariant (>= total-balance 0.0))
        ]
        account:string
        guard:guard
        balance-a:decimal
        balance-b:decimal
        total-balance:decimal
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

    (defschema token-schema
        @doc "schema designated to account and maintain token distribution ledger across all accounts"
        @model [
            (invariant (>= balance-a 0.0))
            (invariant (>= balance-b 0.0))
            (invariant (>= total-balance 0.0))
        ]
        guard:guard
        balance-a:decimal
        balance-b:decimal
        total-balance:decimal
    )
    (defschema token-initialization-schema
        @doc "schema designated to determine whether the token infrastructure has been established or not; created with the purpose of ensuring that this token contract is only initialized once--see initialize-contract function below"
        initialized:bool
    )

    ;; -----------------------------------------------------------------------------------------------------
    ;;  tables
    (deftable token-table:{token-schema})
    (deftable token-initialization-table:{token-initialization-schema})

    ;; -----------------------------------------------------------------------------------------------------
    ;;  constants
    (defconst root-co-bank:string "root-co-bank"
        @doc "root treasury account established to initially hold all the tokens allocated to the co - 25%; note that the company is allocated class a tokens, which can only, strictly be used for computation, i.e., class a tokens cannot be exchanged publicly in either exchanges or DEXs. for more information, visit: fin-us.com"
    )
    (defconst root-users-bank:string "root-users-bank"
        @doc "root treasury account established to initially hold all the tokens allocated to the users of the fin-us platforms - 37.5%; note that the users are allocated class b tokens, which can be used for any purpose, i.e., class b tokens can be used for both computational purposes or for general public trading purposes (in either exchanges or DEXs). for more information, visit: fin-us.com"
    )
    (defconst root-miners-bank:string "root-miners-bank"
        @doc "root treasury account established to initially hold all the tokens allocated to the miners across the fin-us platforms - 37.5%; note that similar to users, miners are also allocated class b tokens."
    )
    (defconst root-network-bank:string "root-network-bank"
        @doc "root treasury account established as a middle layer to accept computational fees--furthermore, this account serves to modulate the network's class a and b tokens balances. this is the only account authorized to transition class a tokens to class b and vise versa. for more information, visit: fin-us.com"
    )
    (defconst pi:decimal 3.1415926535
        @doc "pi to the 10th decimal; utilized in the delta-function scoped into the initialization of this contract. see initialize-contract function below"
    )
    (defconst token-supply:decimal 1000000000.0
        @doc "initial and max supply of the fin-us token; for more information, visit: fin-us.com"
    )

    ;; -----------------------------------------------------------------------------------------------------
    ;;  interfaces
    (implements fungible-v2)

    ;; -----------------------------------------------------------------------------------------------------
    ;;  capabilities
    (defcap GOVERNANCE ()
        (enforce-keyset "free.fin-us-module-admin")
    )
    (defcap DEBIT (sender:string)
        @doc "capability to ensure that the sender is appropriately validated"
        (enforce (!= "" sender) "sender must be specified in order to perform a debit")
        (with-read token-table sender {"guard":=sender-guard}
            (if (contains sender [root-co-bank root-users-bank root-miners-bank root-network-bank])
                (and
                    (enforce-guard sender-guard)
                    (enforce-keyset "free.fin-us-operations-admin")
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
            (contains account-id [root-co-bank root-users-bank root-miners-bank root-network-bank])
            "account name is invalid for the utilization of this function"
        )
        (if (= account-id root-co-bank)
            (enforce
                (=
                    (format "{}" [account-guard])
                    (format "KeySet {keys: [{}],pred: keys-all}" ["4f2c5b8be7c148618790cba4c9d4f5497c364d93094dbdcca7ab6d4b1784bf5d"])
                )
                "in order to credit a root account, the appropriate guard must be specified"
            )
            (if (= account-id root-users-bank)
                (enforce
                    (=
                        (format "{}" [account-guard])
                        (format "KeySet {keys: [{}],pred: keys-all}" ["a615f733602430c9eb0cb24338ed90fa79d76401d1d7182dd6d9b76655a8c96d"])
                    )
                    "in order to credit a root account, the appropriate guard must be specified"
                )
                (if (= account-id root-miners-bank)
                    (enforce
                        (=
                            (format "{}" [account-guard])
                            (format "KeySet {keys: [{}],pred: keys-all}" ["f4ce111c7fae913b213cef3008e7f408277f128fee1f470ab4027e1664c56159"])
                        )
                        "in order to credit a root account, the appropriate guard must be specified"
                    )
                    (enforce
                        (=
                            (format "{}" [account-guard])
                            (format "KeySet {keys: [{}],pred: keys-all}" ["8e8244dc83a00422bb4cade79e2e1b3a12c0d853e6608f88d2adb7c81de55339"])
                        )
                        "in order to credit a root account, the appropriate guard must be specified"
                    )
                )
            )
        )
    )
    (defun validate-account-for-credit-non-root (account-id:string account-guard:guard)
        @doc "ensure that a non-root account's guard is never modified in a credit operation"
        (let ((pfx (take 2 account-id)))
            (enforce (contains pfx ["a:" "b:"])
                "an account's name must start off with either 'a:' or 'b:', which must then be followed by the account's public key"
            )
        )
        (let ((afx (drop 2 account-id)))
            (let ((publicKey (drop 15 (drop (- 17) (format "{}" [account-guard])))))
                (enforce (not (contains "," publicKey)) 
                    "fin-us token contract does not support multiple keys, an account can only have one key"
                )
                (enforce (= afx publicKey)
                    "an account's name must start off with either 'a:' or 'b:', which must then be followed by the account's public key"
                )
            )
        )
    )
    (defun enforce-transfer-create (sender:string receiver:string)
        @doc "enforce a dual class (a and b) based transfer protocol"
        (if (contains receiver [root-co-bank root-users-bank root-miners-bank])
            (enforce 
                (= sender root-network-bank)
                (format "{} is not authorized to credit {}" [sender receiver])
            )
            (if (= receiver root-network-bank)
                (enforce 
                    (not (contains sender [root-users-bank root-miners-bank]))
                    (format "{} is not authorized to credit {}" [sender receiver])
                )
                (if (= (take 2 receiver) "a:")
                    (enforce 
                        (or (= sender root-co-bank) (= (take 2 sender) "a:"))
                        (format "{} is not authorized to credit {}" [sender receiver])
                    )
                    (if (= (take 2 receiver) "b:")
                        (enforce
                            (or (contains sender [root-users-bank root-miners-bank]) (= (take 2 sender) "b:"))
                            (format "{} is not authorized to credit {}" [sender receiver])
                        )
                        (enforce false (format "{} is not authorized to credit {}" [sender receiver]))
                    )
                )
            )
        )
    )
    (defun enforce-transfer-crosschain (sender:string receiver:string)
        @doc "enforce a dual class (a and b) based cross-chain transfer protocol"
        (and
            (enforce (!= sender root-network-bank) (format "{} is not authorized to credit {}" [sender receiver]))
            (enforce (!= receiver root-network-bank) (format "{} is not authorized to credit {}" [sender receiver]))
        )
        (if (contains receiver [root-co-bank root-users-bank root-miners-bank])
            (enforce (= sender receiver) (format "{} is not authorized to credit {}" [sender receiver]))
            (if (= (take 2 receiver) "a:")
                (enforce 
                    (or (= sender root-co-bank) (= (take 2 sender) "a:"))
                    (format "{} is not authorized to credit {}" [sender receiver])
                )
                (if (= (take 2 receiver) "b:")
                    (enforce
                        (or (contains sender [root-users-bank root-miners-bank]) (= (take 2 sender) "b:"))
                        (format "{} is not authorized to credit {}" [sender receiver])
                    )
                    (enforce false (format "{} is not authorized to credit {}" [sender receiver]))
                )
            )
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
        (if (contains account [root-co-bank root-users-bank root-miners-bank root-network-bank])
            (validate-account-for-credit-root account guard)
            (validate-account-for-credit-non-root account guard)
        )
        (insert token-table account {"guard":guard, "balance-a":0.0, "balance-b":0.0, "total-balance":0.0})
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
        (if (contains receiver [root-co-bank root-users-bank root-miners-bank root-network-bank])
            (validate-account-for-credit-root receiver receiver-guard)
            (validate-account-for-credit-non-root receiver receiver-guard)
        )
        (enforce-transfer-create sender receiver)
        
        (with-capability (TRANSFER sender receiver amount)
            (require-capability (DEBIT sender))
            (if 
                (or 
                    (= sender root-co-bank)
                    (or
                        (= (take 2 sender) "a:")
                        (and (= sender root-network-bank) (= receiver root-co-bank))
                    )
                )
                (with-read token-table sender {"guard":=sender-guard, "balance-a":=bal-a, "total-balance":=tot-bal}
                    (enforce-guard sender-guard)
                    (enforce (>= bal-a amount) "insufficient class a balance to complete the transfer")
                    (update token-table sender {"balance-a":(- bal-a amount), "total-balance":(- tot-bal amount)})
                )
                (with-read token-table sender {"guard":=sender-guard, "balance-b":=bal-b, "total-balance":=tot-bal}
                    (enforce-guard sender-guard)
                    (enforce (>= bal-b amount) "insufficient class b balance to complete the transfer")
                    (update token-table sender {"balance-b":(- bal-b amount), "total-balance":(- tot-bal amount)})
                )
            )
            
            (require-capability (CREDIT receiver))
            (if 
                (or 
                    (= receiver root-co-bank)
                    (or 
                        (= (take 2 receiver) "a:")
                        (and (= receiver root-network-bank) (or (= sender root-co-bank) (= (take 2 sender) "a:")))
                    )
                )
                (with-default-read token-table receiver
                    {"guard":receiver-guard, "balance-a":-1.0, "balance-b":-1.0, "total-balance":-2.0}
                    {"guard":=retg, "balance-a":=bal-a, "balance-b":=bal-b, "total-balance":=tot-bal}
                    (enforce (= retg receiver-guard) "for a transfer operation, the receiver-guard must match that of the recipient")
                    
                    (let ((is-new (if (= -1.0 bal-a) true false)))
                        (write token-table receiver
                            {"guard":retg, "balance-a":(if is-new amount (+ bal-a amount)), "balance-b":(if is-new 0.0 bal-b), "total-balance":(if is-new amount (+ tot-bal amount))}
                        )
                    )
                )
                (with-default-read token-table receiver
                    {"guard":receiver-guard, "balance-a":-1.0, "balance-b":-1.0, "total-balance":-2.0}
                    {"guard":=retg, "balance-a":=bal-a, "balance-b":=bal-b, "total-balance":=tot-bal}
                    (enforce (= retg receiver-guard) "for a transfer operation, the receiver-guard must match that of the recipient")
                    
                    (let ((is-new (if (= -1.0 bal-a) true false)))
                        (write token-table receiver
                            {"guard":retg, "balance-a":(if is-new 0.0 bal-a), "balance-b":(if is-new amount (+ bal-b amount)), "total-balance":(if is-new amount (+ tot-bal amount))}
                        )
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
                (if (contains receiver [root-co-bank root-users-bank root-miners-bank root-network-bank])
                    (validate-account-for-credit-root receiver receiver-guard)
                    (validate-account-for-credit-non-root receiver receiver-guard)
                )
                (enforce-transfer-crosschain sender receiver)
                
                ;; step 1: debit sender account on the current chain
                (if 
                    (or 
                        (= sender root-co-bank)
                        (= (take 2 sender) "a:")
                    )
                    (with-read token-table sender {"guard":=sender-guard, "balance-a":=bal-a, "total-balance":=tot-bal}
                        (enforce-guard sender-guard)
                        (enforce (>= bal-a amount) "insufficient class a balance to complete the transfer")
                        (update token-table sender {"balance-a":(- bal-a amount), "total-balance":(- tot-bal amount)})
                    )
                    (with-read token-table sender {"guard":=sender-guard, "balance-b":=bal-b, "total-balance":=tot-bal}
                        (enforce-guard sender-guard)
                        (enforce (>= bal-b amount) "insufficient class b balance to complete the transfer")
                        (update token-table sender {"balance-b":(- bal-b amount), "total-balance":(- tot-bal amount)})
                    )
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
                    (if 
                        (or 
                            (= receiver root-co-bank)
                            (= (take 2 receiver) "a:")
                        )
                        (with-default-read token-table receiver
                            {"guard":receiver-guard, "balance-a":-1.0, "balance-b":-1.0, "total-balance":-2.0}
                            {"guard":=retg, "balance-a":=bal-a, "balance-b":=bal-b, "total-balance":=tot-bal}
                            (enforce (= retg receiver-guard) "for a transfer operation, the receiver-guard must match that of the recipient")
                            
                            (let ((is-new (if (= -1.0 bal-a) true false)))
                                (write token-table receiver
                                    {"guard":retg, "balance-a":(if is-new amount (+ bal-a amount)), "balance-b":(if is-new 0.0 bal-b), "total-balance":(if is-new amount (+ tot-bal amount))}
                                )
                            )
                        )
                        (with-default-read token-table receiver
                            {"guard":receiver-guard, "balance-a":-1.0, "balance-b":-1.0, "total-balance":-2.0}
                            {"guard":=retg, "balance-a":=bal-a, "balance-b":=bal-b, "total-balance":=tot-bal}
                            (enforce (= retg receiver-guard) "for a transfer operation, the receiver-guard must match that of the recipient")
                            
                            (let ((is-new (if (= -1.0 bal-a) true false)))
                                (write token-table receiver
                                    {"guard":retg, "balance-a":(if is-new 0.0 bal-a), "balance-b":(if is-new amount (+ bal-b amount)), "total-balance":(if is-new amount (+ tot-bal amount))}
                                )
                            )
                        )
                    )
                )
            )
        )
    )

    (defun network-modulator:string (users-percentage:decimal miners-percentage:decimal)
        @doc "utility to moderate class a and b balances based on available tokens in root-network-bank"
        
        (enforce-keyset "free.fin-us-operations-admin")
        (let ((b-percentage (+ users-percentage miners-percentage)))
            (and
                (enforce (>= 1.0 b-percentage) "specified users and miners' percentages are invalid")
                (and 
                    (enforce (>= users-percentage 0.0) "specified users and miners' percentages are invalid")
                    (enforce (>= miners-percentage 0.0) "specified users and miners' percentages are invalid")
                )
            )
            (with-read token-table root-network-bank {"guard":=network-guard, "total-balance":=tot-bal}
                (enforce-guard network-guard)
                (update token-table root-network-bank {"balance-a":(* tot-bal (- 1.0 b-percentage)), "balance-b":(* tot-bal b-percentage)})
            )
        )
    )
    (defun details-in-depth:object{account-details-schema} (account-id:string)
        @doc "utility to pull an account's detail in depth"
        (with-read token-table account-id {"guard":=account-guard, "balance-a":=bal-a, "balance-b":=bal-b, "total-balance":=tot-bal}
            {
                "account":account-id,
                "guard":account-guard,
                "balance-a":bal-a,
                "balance-b":bal-b,
                "total-balance":tot-bal
            }
        )
    )
    (defun precision:integer ()
        @doc "returns the maximum allowed decimal precision"
        12
    )
    (defun get-balance:decimal (account:string)
        @doc "get balance for an account, failing if the account does not exist"
        (with-read token-table account {"total-balance":=tot-bal}
            tot-bal
        )
    )
    (defun details:object{fungible-v2.account-details} (account:string)
        @doc "utility to pull an account's general detail"
        (with-read token-table account {"guard":=account-guard, "total-balance":=tot-bal}
            {
                "account":account,
                "balance":tot-bal,
                "guard":account-guard
            }
        )
    )
    (defun rotate:string (account:string new-guard:guard)
        @doc "utility to rotate an entire account based on the new provided guard; note that this function will transition entire balance to the newly created account after enforcing the old guard. the new account's format will be of the form: current account's protocol (either 'a:' or 'b:') followed by the new public key. note that the fin-us token contract does not support multiple keys, an account can only have one key"
        (enforce
            (not (contains account [root-co-bank root-users-bank root-miners-bank root-network-bank]))
            (format "{} is not authorized to call this function" [account])
        )
        (with-read token-table account {"guard":=old-guard, "balance-a":=bal-a, "balance-b":=bal-b, "total-balance":=tot-bal}
            (enforce-guard old-guard)
            (let ((account-protocol (take 1 account)))
                (let ((publicKey (drop 15 (drop (- 17) (format "{}" [new-guard])))))
                    (let ((new-account (format "{}:{}" [account-protocol publicKey])))
                        (create-account new-account new-guard)
                        (update token-table new-account {"balance-a":bal-a, "balance-b":bal-b, "total-balance":tot-bal})
                        (update token-table account {"balance-a":0.0, "balance-b":0.0, "total-balance":0.0})
                    )
                )
            )
        )
    )
    (defun rotate-root:string (account:string new-guard:guard)
        @doc "utility to rotate the keyset of a root account"
        (enforce-keyset "free.fin-us-operations-admin")
        (enforce 
            (contains account [root-co-bank root-users-bank root-miners-bank root-network-bank])
            "account name is invalid for the utilization of this function"
        )
        (with-read token-table account {"guard":=old-guard, "balance-a":=bal-a, "balance-b":=bal-b, "total-balance":=tot-bal}
            (enforce-guard old-guard)
            (update token-table account {"guard":new-guard, "balance-a":bal-a, "balance-b":bal-b, "total-balance":tot-bal})
        )
    )

    (defun initialize-contract:string ()
        (with-default-read token-initialization-table "contract" {"initialized":false} {"initialized":=initialized-val}
            (enforce (!= initialized-val true) "initialize-contract function has expired")
            
            (with-capability (GOVERNANCE)
                (create-account root-co-bank (read-keyset "root-co-bank"))
                (create-account root-users-bank (read-keyset "root-users-bank"))
                (create-account root-miners-bank (read-keyset "root-miners-bank"))
                (create-account root-network-bank (read-keyset "root-network-bank"))
                
                (let ((chain-val (str-to-int (at 'chain-id (chain-data)))))
                    (let ((delta-function-val (floor (exp (- (^ (* (sqrt pi) (- chain-val 3)) 2))))))
                        (update token-table root-co-bank 
                            {"balance-a":(* (* 0.25 delta-function-val) token-supply), "total-balance":(* (* 0.25 delta-function-val) token-supply)}
                        )
                        (update token-table root-users-bank 
                            {"balance-b":(* (* 0.375 delta-function-val) token-supply), "total-balance":(* (* 0.375 delta-function-val) token-supply)}
                        )
                        (update token-table root-miners-bank 
                            {"balance-b":(* (* 0.375 delta-function-val) token-supply), "total-balance":(* (* 0.375 delta-function-val) token-supply)}
                        )
                    )
                )
                (write token-initialization-table "contract" {"initialized":true})
            )
        )
    )
)


