(module prod-hype-coin 'hyper-admin-multi-keyset-prod
  (implements fungible-v2)
  (implements fungible-xchain-v1)
  (use hypercent.prod-fungible-util)

  (defschema entry
    balance:decimal
    guard:guard)
  (deftable ledger:{entry})

  (defconst GENESIS_SUPPLY:decimal 10000000.0)
  (defconst GENESIS_ACCT:string "GENESIS_ACCT")
  (defun genesis-guard () (create-module-guard 'GENESIS_ACCT))
  (defconst HYPE_ALLOW_TRANSFER_DATE (time "2022-04-02T00:00:00Z"))
  (defconst MINIMUM_PRECISION:integer 14)
  (defcap DEBIT (sender:string)
    "Capability for managing debiting operations"
    (enforce-guard (at 'guard (read ledger sender)))
    (enforce (!= sender "") "valid sender"))

  (defcap CREDIT (receiver:string)
    "Capability for managing crediting operations"
    (enforce (!= receiver "") "valid receiver"))

  (defcap GENESIS ()
    "Magic capability constraining genesis transactions"
    true)

  (defcap INT ()
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
    (enforce-valid-transfer sender receiver (precision) amount)
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver))
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

  (defun enforce-unit:bool (amount:decimal)
    (enforce-precision (precision) amount))

  (defun precision:integer ()
    MINIMUM_PRECISION)

  (defun init ()
    (with-capability (GENESIS)
      (mint)
    )
  )

  (defun mint ()
    (require-capability (GENESIS))
    (insert ledger GENESIS_ACCT
      { "balance" : GENESIS_SUPPLY
      , "guard"   : (genesis-guard)
      })
    "Mint succeeded"
    )

  (defun get-balance:decimal (account:string)
      (at 'balance (read ledger account))
    )

  (defun debit:string (account:string amount:decimal)
    (enforce-valid-account account)

    (enforce (> amount 0.0)
      "debit amount must be positive")

    (enforce-unit amount)

    (require-capability (DEBIT account))
    (with-read ledger account
      { "balance" := balance }

      (enforce (<= amount balance) "Insufficient funds")

      (update ledger account
        { "balance" : (- balance amount) }
      ))
    )

  (defun credit:string (account:string guard:guard amount:decimal)
    (enforce-valid-account account)

    (enforce (> amount 0.0) "credit amount must be positive")
    (enforce-unit amount)

    (require-capability (CREDIT account))
    
    (with-default-read ledger account
      {"balance": -1.0, "guard": guard}
      { "balance" := balance, "guard" := retg }
      ; we don't want to overwrite an existing guard with the user-supplied one
      (enforce (= retg guard)
        "account guards do not match")

    (let ((is-new
          (if (= balance -1.0)
              (enforce-reserved account guard)
            false)))

     (write ledger account
       { "balance" : (if is-new amount (+ balance amount))
       , "guard"   : retg
       }))
    
    )
  )

  (defun details:object{fungible-v2.account-details}
    ( account:string )
    (with-read ledger account
      { "balance" := bal
      , "guard" := g }
      { "account" : account
      , "balance" : bal
      , "guard": g })
    )

  (defun create-account:string
    ( account:string
      guard:guard
    )
    (enforce-valid-account account)
    (enforce-reserved account guard)
    (insert ledger account
      { "balance" : 0.0
      , "guard"   : guard
      })

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

  (defun rotate:string (account:string new-guard:guard)
    (require-capability (INT))
    (with-read ledger account
      { "guard" := old-guard }

      (enforce-guard old-guard)

      (update ledger account
        { "guard" : new-guard }))
    )

  (defun can-transfer (sender:string)
    (or (= sender "GENESIS_ACCT") (>= (at 'block-time (chain-data)) HYPE_ALLOW_TRANSFER_DATE))
  )

  (defun transfer:string (sender:string receiver:string amount:decimal)
    (enforce (can-transfer sender) "Only redeeming from Genesis allowed at this time")
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision) amount)

    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (with-read ledger receiver
        { "guard" := g }
        (credit receiver g amount))
      )
    )

  (defun transfer-create:string
      ( sender:string
        receiver:string
        receiver-guard:guard
        amount:decimal )
      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")
      (enforce-valid-transfer sender receiver (precision) amount)
  
      (with-capability (TRANSFER sender receiver amount)
        (debit sender amount)
        (credit receiver receiver-guard amount))
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

    (step
      (with-capability (TRANSFER_XCHAIN sender receiver amount target-chain)

        (enforce-valid-account sender)
        (enforce-valid-account receiver)

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
            , "source-chain" : (at 'chain-id (chain-data))
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
                       (format "Single-key account protocol violation {}" [guard]))
          (enforce false
            (format "Unrecognized reserved protocol: {}" [r]))))))
)


