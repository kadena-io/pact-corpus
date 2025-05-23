(module heron GOV
      
        @doc "Heron coin is a fun community driven utility memecoin"
      
        @model
          [ (defproperty conserves-mass
              (= (column-delta coin-table 'balance ) 0.0))
      
            (defproperty valid-account (account:string)
              (and
                (>= (length account) 3)
                (<= (length account) 256)))
          ]
      
        ; --------------------------------------------------------------------------
        ; Governance
      
        (defcap GOV ()
          ;; 2. Enforce the keyset you defined above for governance
          (enforce-keyset "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heron-token-gov")
        )
      
        ; --------------------------------------------------------------------------
        ; Tokenomics
      
        ;; 3. Build out the tokenomics initialization for your token
        (defun init-token:[string] (init-data:object)
          @doc "Initialize the tokens for the contract"
      
          (with-capability (GOV)
            (let
              (
                (sum-percent
                  (lambda (curr-percent:decimal account-data:object)
                    (+ curr-percent (at 'percent account-data))
                  )
                )
                (init-token-account
                  (lambda (initial-supply:decimal account-data:object)
                    (let
                      (
                        (account (at 'account account-data))
                        (percent (at 'percent account-data))
                        (guard (at 'guard account-data))
                      )
      
                      (insert coin-table account
                        { "balance" : (* initial-supply percent)
                        , "guard" : guard
                        }
                      )
                    )
                  )
                )
              )
      
              ; Step 1: Validate the percentages
              ; fold iterations
              ; Iteration 1: Sum Percent with 0 and ROOT account -> (0 + 0.8) = 0.8
              ; Iteration 2: Sum Percent with 0.8 and LIQDUIDITY account -> (0.8 + 0.2) = 1.0
              (enforce
                (=
                  (fold
                    (sum-percent)
                    0.0
                    (at "accounts" init-data)
                  )
                  1.0
                )
                "Percentages must add up to 1.0"
              )
      
              ; Step 2: Initialize the token accounts
              (map
                (init-token-account (at "initial-supply" init-data))
                (at "accounts" init-data)
              )
            )
          )
        )
      
        ; --------------------------------------------------------------------------
        ; Implementation
      
        (implements fungible-v2)
        (implements fungible-xchain-v1)
      
        ; --------------------------------------------------------------------------
        ; Schemas and Tables
      
        (defschema coin-schema
          @doc "The coin contract token schema"
          @model [ (invariant (>= balance 0.0)) ]
      
          balance:decimal
          guard:guard)
      
        (defschema exempt-schema
          account:string
          revoked:bool
          )
      
        (deftable coin-table:{coin-schema})
        (deftable exempt:{exempt-schema})
      
      (defun get-exempt ()
      (select exempt (constantly true))
      )
        ; --------------------------------------------------------------------------
        ; Capabilities
      
        (defcap DEBIT (sender:string)
          "Capability for managing debiting operations"
          (enforce-guard (at 'guard (read coin-table sender)))
          (enforce (!= sender "") "valid sender"))
      
        (defcap CREDIT (receiver:string)
          "Capability for managing crediting operations"
          (enforce (!= receiver "") "valid receiver"))
      
        (defcap ROTATE (account:string)
          @doc "Autonomously managed capability for guard rotation"
          @managed
          true)
      
        (defcap TRANSFER:bool
          ( sender:string
            receiver:string
            amount:decimal
          )
          @managed amount TRANSFER-mgr
          (enforce (!= sender receiver) "same sender and receiver")
          (enforce-unit amount)
          (enforce (> amount 0.0) "Positive amount")
          (compose-capability (DEBIT sender))
          (compose-capability (CREDIT receiver))
        )
      
        (defun TRANSFER-mgr:decimal
          ( managed:decimal
            requested:decimal
          )
      
          (let ((newbal (- managed requested)))
            (enforce (>= newbal 0.0)
              (format "TRANSFER exceeded for balance {}" [managed]))
            newbal)
        )
      
        (defcap TRANSFER_XCHAIN:bool
          ( sender:string
            receiver:string
            amount:decimal
            target-chain:string
          )
      
          @managed amount TRANSFER_XCHAIN-mgr
          (enforce-unit amount)
          (enforce (> amount 0.0) "Cross-chain transfers require a positive amount")
          (compose-capability (DEBIT sender))
        )
      
        (defun TRANSFER_XCHAIN-mgr:decimal
          ( managed:decimal
            requested:decimal
          )
      
          (enforce (>= managed requested)
            (format "TRANSFER_XCHAIN exceeded for balance {}" [managed]))
          0.0
        )
      
        (defcap TRANSFER_XCHAIN_RECD:bool
          ( sender:string
            receiver:string
            amount:decimal
            source-chain:string
          )
          @event true
        )
      
        (defcap INTERNAL ()
            @doc "only for internal use"
            true
          )
      
        ; --------------------------------------------------------------------------
        ; Constants
      
        (defconst COIN_CHARSET CHARSET_LATIN1
          "The default coin contract character set")
      
        (defconst MINIMUM_PRECISION 12
          "Minimum allowed precision for coin transactions")
      
        (defconst MINIMUM_ACCOUNT_LENGTH 3
          "Minimum account length admissible for coin accounts")
      
        (defconst MAXIMUM_ACCOUNT_LENGTH 256
          "Maximum account name length admissible for coin accounts")
      
        (defconst VALID_CHAIN_IDS (map (int-to-str 10) (enumerate 0 19))
          "List of all valid Chainweb chain ids")
      
          (defconst ROOT_ACCOUNT_ID:string 'ROOT
          " ID for the account which initially owns all the tokens. ")
      
        ; --------------------------------------------------------------------------
        ; Utilities
      
        (defun enforce-unit:bool (amount:decimal)
        @doc "Enforce minimum precision allowed for coin transactions"
        (enforce
          (= (floor amount MINIMUM_PRECISION)
             amount)
          (format "Amount violates minimum precision: {}" [amount]))
        )
      
        (defun validate-account (account:string)
          @doc "Enforce that an account name conforms to the coin contract                minimum and maximum length requirements, as well as the                   latin-1 character set."
      
          (enforce
            (is-charset COIN_CHARSET account)
            (format
              "Account does not conform to the coin contract charset: {}"
              [account]))
      
          (let ((account-length (length account)))
      
            (enforce
              (>= account-length MINIMUM_ACCOUNT_LENGTH)
              (format
                "Account name does not conform to the min length requirement: {}"
                [account]))
      
            (enforce
              (<= account-length MAXIMUM_ACCOUNT_LENGTH)
              (format
                "Account name does not conform to the max length requirement: {}"
                [account]))
            )
        )
      
        ; --------------------------------------------------------------------------
        ; Coin Implementation
      
        (defun create-account:string (account:string guard:guard)
          @model [ (property (valid-account account)) ]
      
          (validate-account account)
          (enforce-reserved account guard)
      
          (insert coin-table account
            { "balance" : 0.0
            , "guard"   : guard
            })
          )
      
        (defun get-balance:decimal (account:string)
          (with-read coin-table account
            { "balance" := balance }
            balance
            )
          )
      
        (defun details:object{fungible-v2.account-details}
          ( account:string )
          (with-read coin-table account
            { "balance" := bal
            , "guard" := g }
            { "account" : account
            , "balance" : bal
            , "guard": g })
          )
      
        (defun rotate:string (account:string new-guard:guard)
          (with-capability (ROTATE account)
            (with-read coin-table account
              { "guard" := old-guard }
      
              (enforce-guard old-guard)
      
              (update coin-table account
                { "guard" : new-guard }
                )))
          )
      
      
        (defun precision:integer
          ()
          MINIMUM_PRECISION)
      
        (defun transfer:string (sender:string receiver:string amount:decimal)
          @model [ (property conserves-mass)
                   (property (> amount 0.0))
                   (property (valid-account sender))
                   (property (valid-account receiver))
                   (property (!= sender receiver)) ]
      
          (enforce (!= sender receiver)
            "sender cannot be the receiver of a transfer")
      
          (validate-account sender)
          (validate-account receiver)
      
          (enforce (> amount 0.0)
            "transfer amount must be positive")
      
          (enforce-unit amount)
      
          (with-capability (TRANSFER sender receiver amount)
            (debit sender amount)
            (with-read coin-table receiver
              { "guard" := g }
      
              (credit sender receiver g amount))
            )
          )
      
        (defun transfer-create:string
          ( sender:string
            receiver:string
            receiver-guard:guard
            amount:decimal )
      
          @model [ (property conserves-mass) ]
      
          (enforce (!= sender receiver)
            "sender cannot be the receiver of a transfer")
      
          (validate-account sender)
          (validate-account receiver)
      
          (enforce (> amount 0.0)
            "transfer amount must be positive")
      
          (enforce-unit amount)
      
          (with-capability (TRANSFER sender receiver amount)
            (debit sender amount)
            (credit sender receiver receiver-guard amount))
          )
      
        (defun debit:string (account:string amount:decimal)
          @doc "Debit AMOUNT from ACCOUNT balance"
      
          @model [ (property (> amount 0.0))
                   (property (valid-account account))
                 ]
      
          (validate-account account)
      
          (enforce (> amount 0.0)
            "debit amount must be positive")
      
          (enforce-unit amount)
      
          (require-capability (DEBIT account))
          (with-read coin-table account
            { "balance" := balance }
      
            (enforce (<= amount balance) "Insufficient funds")
      
            (update coin-table account
              { "balance" : (- balance amount) }
              ))
          )
      
          (defconst TX_BURN:decimal 0.05
            "Percentage of every transaction burned")
      
          (defconst TX_FEE:decimal 0.02
            "Percentage of every transaction collected")
      
          (defconst FEES_ACCOUNT_ID:string "u:n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heron-fees.require-WALLET:DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g"
            "ID of LP rewards account")
      
          (defconst BURN:string "BURN_ACCOUNT")
      
      
        (defun credit:string (sender:string account:string guard:guard amount:decimal)
          @doc "Credit AMOUNT to ACCOUNT balance"
      
          @model [ (property (> amount 0.0))
                   (property (valid-account account))
                 ]
          (validate-account sender)
          (validate-account account)
          (enforce (> amount 0.0) "credit amount must be positive")
          (enforce-unit amount)
      
          (require-capability (CREDIT account))
          (with-default-read coin-table account
            { "balance" : 0.0, "guard" : guard }
            { "balance" := balance, "guard" := retg }
            ; we don't want to overwrite an existing guard with the user-supplied one
            (enforce (= retg guard)
              "account guards do not match")
      
            (write coin-table account
                { "balance" : (if (or (is-excluded-account account sender) (= account sender))
                (+ balance amount) (+ balance (* (- 1.0 (+ TX_FEE TX_BURN)) amount)))
                , "guard"   : retg
                })
          )
      
            (with-default-read coin-table FEES_ACCOUNT_ID
              { "balance" : 0.0
              , "guard"   : (create-null-guard)
              }
              { "balance" := balance
              , "guard"   := currentGuard
              }
              (enforce (!= currentGuard (create-null-guard)) (format "Please create {} first." [FEES_ACCOUNT_ID]))
              (write coin-table FEES_ACCOUNT_ID
                  { "balance" : (if (or (is-excluded-account account sender) (= account account))
                  (+ balance 0.0) (+ balance (* TX_FEE amount)))
                  , "guard"   : currentGuard
                }
              )
            )
        )
      
        (defun enforce-null:bool
          ()
          false
         )
      
         (defun create-null-guard:guard
          ()
          (create-user-guard (enforce-null))
         )
      
        (defun is-excluded-account:bool (account:string sender:string)
          @doc "Checks if the account or sender is in the list of exempt accounts."
            (with-default-read exempt account {'account: "", 'revoked: false}
                                              {'account:= receiver-account, 'revoked:= revoked-receiver}
              (with-default-read exempt sender {'account: "", 'revoked: false}
                                               {'account:= sender-account, 'revoked:= revoked-sender}
          (or (and (= receiver-account account)(= revoked-receiver false)) (and (= sender-account sender)(= revoked-sender false)))
              )
            )
        )
      
      (defun enforce-reserved:bool (account:string guard:guard)
        "Enforce that a principal account matches to it's guard"
        (if (is-principal account)
            (enforce (validate-principal guard account)
                      (format "Reserved protocol guard violation: {}" [(typeof-principal account)]))
            true)
      )
      
      
        (defschema crosschain-schema
          @doc "Schema for yielded value in cross-chain transfers"
          receiver:string
          receiver-guard:guard
          amount:decimal
          source-chain:string)
      
        (defpact transfer-crosschain:string
          ( sender:string
            receiver:string
            receiver-guard:guard
            target-chain:string
            amount:decimal )
      
          @model [ (property (> amount 0.0))
                   (property (valid-account sender))
                   (property (valid-account receiver))
                 ]
      
          (step
            (with-capability
              (TRANSFER_XCHAIN sender receiver amount target-chain)
      
              (validate-account sender)
              (validate-account receiver)
      
              (enforce (!= "" target-chain) "empty target-chain")
              (enforce (!= (at 'chain-id (chain-data)) target-chain)
                "cannot run cross-chain transfers to the same chain")
      
              (enforce (> amount 0.0)
                "transfer quantity must be positive")
      
              (enforce-unit amount)
      
              (enforce (contains target-chain VALID_CHAIN_IDS)
                "target chain is not a valid chainweb chain id")
      
              ;; step 1 - debit delete-account on current chain
              (debit sender amount)
              (emit-event (TRANSFER sender "" amount))
      
              (let
                ((crosschain-details:object{crosschain-schema}
                  { "receiver" : receiver
                  , "receiver-guard" : receiver-guard
                  , "amount" : amount
                  , "source-chain" : (at 'chain-id (chain-data))
                  }))
                (yield crosschain-details target-chain)
                )))
      
          (step
            (resume
              { "receiver" := receiver
              , "receiver-guard" := receiver-guard
              , "amount" := amount
              , "source-chain" := source-chain
              }
      
              (emit-event (TRANSFER "" receiver amount))
              (emit-event (TRANSFER_XCHAIN_RECD "" receiver amount source-chain))
      
              ;; step 2 - credit create account on target chain
              (with-capability (CREDIT receiver)
                (credit sender receiver receiver-guard amount))
              ))
          )
      
          (defun move-premine:string
              ( receiver:string
                guard:guard
                amount:decimal )
      
              @doc " Admin-only. Move the premine. "
      
              (with-capability (GOV)
                (with-capability (INTERNAL)
                  (install-capability (TRANSFER ROOT_ACCOUNT_ID receiver amount))
                  (transfer-create ROOT_ACCOUNT_ID receiver guard amount)
                )
              )
            )
      
          (defun exempt-accounts:[string] (accounts:[string] revoked:bool)
            @doc "Adds or updates accounts that shouldn't deduct burn or fees"
            (with-capability (GOV)
              (map (lambda (account)
                      (write exempt account
                        {'account: account, 'revoked: revoked})
                    )
                  accounts
              )
            )
          )
      
      )
      
      
