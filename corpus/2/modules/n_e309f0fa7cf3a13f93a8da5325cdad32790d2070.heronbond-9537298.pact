(module heronbond GOVERNANCE
      
         @doc "heronbond - Heronbond token is given to anyone who bonded with the heron liquidity bonder"
      
         @model
          [ (defproperty conserves-mass (amount:decimal)
              (= (column-delta account-table 'balance) 0.0))
      
            (defproperty valid-account-id (accountId:string)
              (and
                (>= (length accountId) 3)
                (<= (length accountId) 256)))
          ]
      
         (implements fungible-v2)
      
        ;--------------------------------------------------------------------------
        ; Schemas and Tables
      
        (defschema account-schema
          @doc " An account, holding a token balance. "
          balance:decimal
          guard:guard
        )
        (deftable account-table:{account-schema})
      
        ; --------------------------------------------------------------------------
        ; Capatilibites
      
        (defcap GOVERNANCE()
             @doc " Give a designated account full access to upgrade the smart contract. "
             (enforce-keyset "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heron-token-gov")
        )
      
        (defcap DEBIT
          ( sender:string )
      
          @doc " Capability to perform debiting operations. "
      
          (enforce-guard (at 'guard (read account-table sender ['guard])))
          (enforce (!= sender "") "Invalid sender.")
        )
      
        (defcap CREDIT
          ( receiver:string )
      
          @doc " Capability to perform crediting operations. "
      
          (enforce (!= receiver "") "Invalid receiver.")
        )
      
        (defcap TRANSFER:bool
          ( sender:string
            receiver:string
            amount:decimal )
      
          @doc " Capability to perform transfer between two accounts. "
      
          @managed amount TRANSFER-mgr
      
          (enforce (!= sender receiver) "Sender cannot be the receiver.")
          (enforce-unit amount)
          (enforce (> amount 0.0) "Transfer amount must be positive.")
          (compose-capability (DEBIT sender))
          (compose-capability (CREDIT receiver))
        )
      
        (defun TRANSFER-mgr:decimal
          ( managed:decimal
            requested:decimal )
      
          (let ((newbal (- managed requested)))
            (enforce (>= newbal 0.0)
              (format "TRANSFER exceeded for balance {}" [managed]))
            newbal
          )
        )
      
        ; --------------------------------------------------------------------------
        ; Constants
      
        (defconst ROOT_ACCOUNT_ID:string 'ADMIN
          " ID for the account which initially receives all the tokens. ")
      
        (defconst INITIAL_SUPPLY:decimal 100000.0
          " Initial supply of tokens.")
      
        (defconst DECIMALS:integer 12
          " Specifies the minimum denomination for token transactions. ")
      
        (defconst ACCOUNT_ID_CHARSET CHARSET_LATIN1
          " Allowed character set for account IDs. ")
      
        (defconst ACCOUNT_ID_PROHIBITED_CHARACTER "$")
      
        (defconst ACCOUNT_ID_MIN_LENGTH 3
          " Minimum character length for account IDs. ")
      
        (defconst ACCOUNT_ID_MAX_LENGTH 256
          " Maximum character length for account IDs. ")
      
      
        ; --------------------------------------------------------------------------
        ; Utilities
      
        (defun validate-account-id
          ( accountId:string )
      
          @doc " Enforce that an account ID meets charset and length requirements. "
      
          (enforce
            (is-charset ACCOUNT_ID_CHARSET accountId)
            (format
              "Account ID does not conform to the required charset: {}"
              [accountId]))
      
          (enforce
            (not (contains accountId ACCOUNT_ID_PROHIBITED_CHARACTER))
            (format "Account ID contained a prohibited character: {}" [accountId]))
      
          (let ((accountLength (length accountId)))
      
            (enforce
              (>= accountLength ACCOUNT_ID_MIN_LENGTH)
              (format
                "Account ID does not conform to the min length requirement: {}"
                [accountId]))
      
            (enforce
              (<= accountLength ACCOUNT_ID_MAX_LENGTH)
              (format
                "Account ID does not conform to the max length requirement: {}"
                [accountId]))
          )
        )
      
        ; --------------------------------------------------------------------------
        ; Fungible-v2 Implementation
      
        (defun transfer-create:string
          ( sender:string
            receiver:string
            receiver-guard:guard
            amount:decimal )
      
          @doc " Transfer to an account, creating it if it does not exist. "
      
          @model [ (property (conserves-mass amount))
                   (property (> amount 0.0))
                   (property (valid-account-id sender))
                   (property (valid-account-id receiver))
                   (property (!= sender receiver)) ]
      
          (with-capability (TRANSFER sender receiver amount)
            (debit sender amount)
            (credit receiver receiver-guard amount)
          )
        )
      
        (defun transfer:string
          ( sender:string
            receiver:string
            amount:decimal )
      
          @doc " Transfer to an account, failing if the account does not exist. "
      
          @model [ (property (conserves-mass amount))
                   (property (> amount 0.0))
                   (property (valid-account-id sender))
                   (property (valid-account-id receiver))
                   (property (!= sender receiver)) ]
      
          (with-read account-table receiver
            { "guard" := guard }
            (transfer-create sender receiver guard amount)
          )
        )
      
        (defun debit
          ( accountId:string
            amount:decimal )
      
          @doc " Decrease an account balance. Internal use only. "
      
          @model [ (property (> amount 0.0))
                   (property (valid-account-id accountId))
                 ]
      
          (validate-account-id accountId)
      
          (enforce (> amount 0.0) "Debit amount must be positive.")
          (enforce-unit amount)
          (require-capability (DEBIT accountId))
      
          (with-read account-table accountId
            { "balance" := balance }
      
            (enforce (<= amount balance) "Insufficient funds.")
      
            (update account-table accountId
              { "balance" : (- balance amount) }
            )
          )
        )
      
        (defun credit
          ( accountId:string
            guard:guard
            amount:decimal )
      
          @doc " Increase an account balance. Internal use only. "
      
          @model [ (property (> amount 0.0))
                   (property (valid-account-id accountId))
                 ]
      
          (validate-account-id accountId)
          (enforce (> amount 0.0) "Credit amount must be positive.")
          (enforce-unit amount)
          (require-capability (CREDIT accountId))
      
          (with-default-read account-table accountId
            { "balance" : -1.0, "guard" : guard }
            { "balance" := balance, "guard" := retg }
      
            (enforce (= retg guard)
              "account guards do not match")
      
            (let ((is-new
                   (if (= balance -1.0)
                       (enforce-reserved accountId guard)
                     false)))
      
              (write account-table accountId
                { "balance" : (if is-new amount (+ balance amount))
                , "guard"   : retg
                }))
            ))
      
        (defun check-reserved:string (accountId:string)
          " Checks ACCOUNT for reserved name and returns type if            found or empty string. Reserved names start with a            single char and colon, e.g. 'c:foo', which would return 'c' as type."
          (let ((pfx (take 2 accountId)))
            (if (= ":" (take -1 pfx)) (take 1 pfx) "")))
      
        (defun enforce-reserved:bool (accountId:string guard:guard)
          @doc " Enforce reserved account name protocols. "
          (let ((r (check-reserved accountId)))
            (if (= "" r) true
              (if (= "k" r)
                (enforce
                  (= (format "{}" [guard])
                     (format "KeySet {keys: [{}],pred: keys-all}"
                             [(drop 2 accountId)]))
                  "Single-key account protocol violation")
                true))))
      
        (defun get-balance:decimal
          ( account:string )
          @doc " Returns the balance for an ACCOUNT. "
          (at 'balance (read account-table account ['balance]))
        )
      
        (defun details:object{fungible-v2.account-details}
          ( account:string )
      
          (with-read account-table account
            { "balance" := balance
            , "guard"   := guard
            }
            { "account" : account
            , "balance" : balance
            , "guard"   : guard
            }
          )
        )
      
        (defun precision:integer ()
        @doc " Returns token decimal precision. "
          DECIMALS
        )
      
        (defun enforce-unit:bool
          ( amount:decimal )
      
          @doc " Enforce the minimum denomination for token transactions. "
      
          (enforce
            (= (floor amount DECIMALS) amount)
            (format "Amount violates minimum denomination: {}" [amount])
          )
        )
      
        (defun create-account:string
          ( account:string
            guard:guard )
      
          @doc " Create a new account. "
      
          @model [ (property (valid-account-id account)) ]
      
          (validate-account-id account)
          (enforce-reserved account guard)
      
          (insert account-table account
            { "balance" : 0.0
            , "guard"   : guard
            }
          )
        )
      
        (defun rotate:string
          ( account:string
            new-guard:guard )
      
            @doc " Rotates an account's guard. "
      
          (with-read account-table account
            { "guard" := oldGuard }
      
            (enforce-guard oldGuard)
            (enforce-guard new-guard)
      
            (update account-table account
              { "guard" : new-guard }
            )
          )
        )
      
      
        (defpact transfer-crosschain:string
          ( sender:string
            receiver:string
            receiver-guard:guard
            target-chain:string
            amount:decimal )
          (step (enforce false "cross chain not supported"))
        )
        ; --------------------------------------------------------------------------
        ; Initialization
      
        (defcap MINT (account:string amount:decimal)
          @doc " Emitted event when tokens are minted "
          @event true
        )
      
        ;Guard for initial pool account that holds the minted tokens
        (defcap INIT_POOL_GUARD
          (id:string)
        true)
      
        ;Enforces the guard for the pool of initial tokens
        (defun enforce-pool-guard:bool
          (id:string)
        (require-capability (INIT_POOL_GUARD id)))
      
        ;Creates a pool guard
        (defun create-pool-guard:guard
          (account:string)
        (create-user-guard (enforce-pool-guard account)))
      
        (defun initialize:string()
          @doc " Initialize the contract and mint the supply.                 Admin-only. Can only run once. "
            (with-capability (GOVERNANCE)
              (create-account ROOT_ACCOUNT_ID (create-pool-guard ROOT_ACCOUNT_ID))
              (update account-table ROOT_ACCOUNT_ID { "balance" : INITIAL_SUPPLY })
              (emit-event (MINT ROOT_ACCOUNT_ID INITIAL_SUPPLY))
              (format "Initialized {} tokens" [INITIAL_SUPPLY])
            )
        )
      
        (defun move-init:string
          ( receiver:string
            rguard:guard
            amount:decimal  )
          @doc " Move the initial supply tokens to the admin after initialization.            Admin-only. "
          (with-capability (GOVERNANCE)
            (with-capability (INIT_POOL_GUARD ROOT_ACCOUNT_ID)
              (install-capability (TRANSFER ROOT_ACCOUNT_ID receiver amount))
              (transfer-create ROOT_ACCOUNT_ID receiver rguard amount)
            )
            (format "Moved {} tokens" [INITIAL_SUPPLY])
          )
        )
      
      )
      
