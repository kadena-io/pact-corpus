(module zUSD GOVERNANCE
  "Wrapped USDC fungible-v2 token."
  (implements fungible-v2)
  (implements wrapped-mint-burn)
  (implements fungible-xchain-v1)

  ;; coin-v1
  (bless "mfLjYGnmNljVR3CQWTlZiqwyHz-uRXQiihnsyzJ9zsQ")

  (use n_b742b4e9c600892af545afb408326e82a6c0c6ed.bridge)

  ; ---Schemas and tables---
  (defschema zusd-schema
    balance:decimal
    guard:guard)
  (deftable zusd-ledger:{zusd-schema})

  ; ---Capatilibites---
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_b742b4e9c600892af545afb408326e82a6c0c6ed.bridge-admin")))

  (defcap DEBIT (sender:string)
    "Capability for managing debiting operations"
    (enforce-guard (at 'guard (read zusd-ledger sender))))

  (defcap CREDIT (receiver:string)
    "Capability for managing crediting operations"
    (enforce (!= receiver "") "Invalid receiver"))

  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce (!= sender receiver) "same sender and receiver")
    (enforce-unit amount)
    (enforce (> amount 0.0) "Positive amount")
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver)))

  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal))

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

  (defcap ROTATE (account:string)
    @doc "Autonomously managed capability for guard rotation"
    @managed true
  )

  ; ---Constants---
  (defconst MINIMUM_PRECISION 18
    "Minimum allowed precision for zUSD transactions")

  (defconst ERC_ADDRESS (read-msg "erc-address")
    "Address of wrapped ERC-20 contract")
  (defconst VALID_CHAIN_IDS (map (int-to-str 10) (enumerate 0 19))
    "List of all valid Chainweb chain ids")

  (defconst COIN_CHARSET CHARSET_LATIN1
    "The default coin contract character set")

  (defconst MINIMUM_ACCOUNT_LENGTH 3
    "Minimum account length admissible for coin accounts")

  (defconst MAXIMUM_ACCOUNT_LENGTH 256
    "Maximum account name length admissible for coin accounts")

  ; ---Utilities---
  (defun enforce-unit:bool (amount:decimal)
    @doc "Enforce minimum precision allowed for coin transactions"
    (enforce
      (= (floor amount MINIMUM_PRECISION)
         amount)
      (format "Amount violates minimum precision: {}" [amount])))

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

  (defun check-reserved:string (account:string)
    @doc " Checks ACCOUNT for reserved name and returns type if \
    \ found or empty string. Reserved names start with a \
    \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
    (let ((pfx (take 2 account)))
      (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

  (defun enforce-reserved:bool (account:string guard:guard)
    @doc "Enforce reserved account name protocols"
    (if (validate-principal guard account)
        true
        (let ((r (check-reserved account)))
          (if (= r "")
              true
              (enforce false (format "Reserved protocol guard violation: {}" [r]))))))

  ; ---zUSD Contract---
  (defun create-account:string (account:string guard:guard)
    (validate-account account)
    (enforce-reserved account guard)
    (insert zusd-ledger account
      { "balance" : 0.0
      , "guard"   : guard
      }))

  (defun get-balance:decimal (account:string)
    (at 'balance (read zusd-ledger account)))

  (defun details:object{fungible-v2.account-details} (account:string)
    (with-read zusd-ledger account
      { "balance" := bal
      , "guard" := g }
      { "account" : account
      , "balance" : bal
      , "guard": g }))

  (defun rotate:string (account:string new-guard:guard)
    (with-capability (ROTATE account)
      (with-read zusd-ledger account
        { "guard" := old-guard }
        (enforce-guard old-guard)
        (update zusd-ledger account { "guard" : new-guard }))))

  (defun fund:string (account:string amount:decimal)
    (with-capability (GOVERNANCE)
      (with-capability (CREDIT account)
        (credit account
          (at 'guard (read zusd-ledger account))
          amount))))

  (defun precision:integer () MINIMUM_PRECISION)

  (defun transfer:string (sender:string receiver:string amount:decimal)
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")

    (enforce (> amount 0.0)
      "transfer amount must be positive")
    (enforce-unit amount)
    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (with-read zusd-ledger receiver
        { "guard" := g }
        (credit receiver g amount))))

  (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")

    (enforce (> amount 0.0)
      "transfer amount must be positive")
    (enforce-unit amount)
    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (credit receiver receiver-guard amount)))

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
            (credit receiver receiver-guard amount))
          ))
      )


  (defun debit:string (account:string amount:decimal)
    (require-capability (DEBIT account))
    (validate-account account)

    (enforce (> amount 0.0) "debit amount must be positive")
    (enforce-unit amount)

    (with-read zusd-ledger account
      { "balance" := balance }
      (enforce (<= amount balance) "Insufficient funds")
      (update zusd-ledger account { "balance" : (- balance amount) })))

  (defun credit:string (account:string guard:guard amount:decimal)
    (require-capability (CREDIT account))
    (validate-account account)

    (enforce (> amount 0.0) "credit amount must be positive")
    (enforce-unit amount)

    (with-default-read zusd-ledger account
      { "balance" : -1.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      ; we don't want to overwrite an existing guard with the user-supplied one
      (enforce (= retg guard)
        "account guards do not match")
      (let ((is-new (if (= balance -1.0) (enforce-reserved account guard) false)))
        (write zusd-ledger account
          { "balance" : (if is-new amount (+ balance amount))
          , "guard"   : retg
          })
        )))

  ; ---MINT & BURN Capabilities---
  (defcap MINT (account:string amount:decimal)
    (compose-capability (CREDIT account)))

  (defcap BURN:bool (account:string amount:decimal)
    (compose-capability (DEBIT account)))

  (defun mint:bool (account:string guard:guard amount:decimal)
    (with-capability (MINT account amount)
      (enforce-bridge-guard)
      (credit account guard amount)
      (emit-event (TRANSFER "" account amount))))

  (defun burn:bool (account:string amount:decimal)
    (with-capability (BURN account amount)
      (enforce-bridge-guard)
      (debit account amount)
      (emit-event (TRANSFER account "" amount))))
)


