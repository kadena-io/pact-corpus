(module test-meme-token GOVERNANCE

    @doc "'The meme Token"
  
    @model
      [ (defproperty conserves-mass
          (= (column-delta coin-table 'balance) 0.0))
  
        (defproperty valid-account (account:string)
          (and
            (>= (length account) 3)
            (<= (length account) 256)))
      ]
  
    (implements fungible-v2)
    
    ; --------------------------------------------------------------------------
    ; Schemas and Tables
  
    (defschema coin-schema
      @doc "The coin contract token schema"
      @model [ (invariant (>= balance 0.0)) ]
  
      balance:decimal
      guard:guard)
  
    (deftable coin-table:{coin-schema})

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
  
      (defconst INITIALIZE_CHAIN "3"
          "Chain on whichthe tokens can be initialized")
    
      (defconst ADMIN_ADDRESS "k:f7278eeaa55a4b52c281fa694035f82a43a6711eb547fc1ab900be1ccf9fb409")
  
      (defconst ADMIN_KEYSET (read-keyset 'kitty-kad))

      (defconst INITIAL_SUPPLY 100000000.0)
  
    ; --------------------------------------------------------------------------
    ; Capabilities
    (defcap GOVERNANCE ()
        @doc " Give the admin full access to call and upgrade the module. "
        (enforce-keyset ADMIN_KEYSET)
      )
  
    (defcap COINBASE ()
      "Magic capability to protect miner reward"
      true)
  
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
  
    ; v3 capabilities
    (defcap RELEASE_ALLOCATION
      ( account:string
        amount:decimal
      )
      @doc "Event for allocation release, can be used for sig scoping."
      @event true
    )

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
      @doc "Enforce that an account name conforms to the coin contract \
           \minimum and maximum length requirements, as well as the    \
           \latin-1 character set."
  
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
    ; Coin Contract
  
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
  
          (credit receiver g amount))
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
        (credit receiver receiver-guard amount))
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
  
  
    (defun credit:string (account:string guard:guard amount:decimal)
      @doc "Credit AMOUNT to ACCOUNT balance"
  
      @model [ (property (> amount 0.0))
               (property (valid-account account))
             ]
  
      (validate-account account)
  
      (enforce (> amount 0.0) "credit amount must be positive")
      (enforce-unit amount)
  
      (require-capability (CREDIT account))
      (with-default-read coin-table account
        { "balance" : -1.0, "guard" : guard }
        { "balance" := balance, "guard" := retg }
        ; we don't want to overwrite an existing guard with the user-supplied one
        (enforce (= retg guard)
          "account guards do not match")
  
        (let ((is-new
               (if (= balance -1.0)
                   (enforce-reserved account guard)
                 false)))
  
          (write coin-table account
            { "balance" : (if is-new amount (+ balance amount))
            , "guard"   : retg
            }))
        ))
  
    (defun check-reserved:string (account:string)
      " Checks ACCOUNT for reserved name and returns type if \
      \ found or empty string. Reserved names start with a \
      \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
      (let ((pfx (take 2 account)))
        (if (= ":" (take -1 pfx)) (take 1 pfx) "")))
  
    (defun enforce-reserved:bool (account:string guard:guard)
      @doc "Enforce reserved account name protocols."
      (let ((r (check-reserved account)))
        (if (= "" r) true
          (if (= "k" r)
            (enforce
              (= (format "{}" [guard])
                 (format "KeySet {keys: [{}],pred: keys-all}"
                         [(drop 2 account)]))
              "Single-key account protocol violation")
            (enforce false
              (format "Unrecognized reserved protocol: {}" [r]))))))
  
  
    (defschema crosschain-schema
      @doc "Schema for yielded value in cross-chain transfers"
      receiver:string
      receiver-guard:guard
      amount:decimal)
  
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
        (with-capability (DEBIT sender)
  
          (validate-account sender)
          (validate-account receiver)
  
          (enforce (!= "" target-chain) "empty target-chain")
          (enforce (!= (at 'chain-id (chain-data)) target-chain)
            "cannot run cross-chain transfers to the same chain")
  
          (enforce (> amount 0.0)
            "transfer quantity must be positive")
  
          (enforce-unit amount)
  
          ;; step 1 - debit delete-account on current chain
          (debit sender amount)
  
          (emit-event (TRANSFER sender "" amount))
  
          (let
            ((crosschain-details:object{crosschain-schema}
              { "receiver" : receiver
              , "receiver-guard" : receiver-guard
              , "amount" : amount
              }))
            (yield crosschain-details target-chain)
            )))
  
      (step
        (resume
          { "receiver" := receiver
          , "receiver-guard" := receiver-guard
          , "amount" := amount
          }
          (emit-event (TRANSFER "" receiver amount))
          ;; step 2 - credit create account on target chain
          (with-capability (CREDIT receiver)
            (credit receiver receiver-guard amount))
          ))
      )

    ;; ; --------------------------------------------------------------------------
    ;; ; Custom Functions

    (defun initialize:string()
        @doc " Initialize the contract. Admin-only. Should fail if it has been called before"
        ;  (enforce (!= (at 'chain-id (chain-data))) INITIALIZE_CHAIN)
        (with-capability (GOVERNANCE)
            (create-account ADMIN_ADDRESS ADMIN_KEYSET)
            (update coin-table ADMIN_ADDRESS { "balance" : INITIAL_SUPPLY })
        )
    )
  
  )

  
