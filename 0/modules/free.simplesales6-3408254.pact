(module simplesales6 GOVERNANCE

  (implements fungible-v2)
  (use fungible-util)
  (use util.guards)

  (defschema reservation
    account:string
    amount-kda:decimal
    amount-ztrust:decimal)

  (deftable reservations6:{reservation})

  (defschema entry
    balance:decimal
    guard:guard)

  (deftable ledger6:{entry})

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'admin-keyset-peters)))
      (defcap DEBIT (sender:string)
        (enforce-guard (at 'guard (read ledger6 sender))))

  (defcap CREDIT (receiver:string) true)

  (defcap RESERVE
    ( account:string
      amount-kda:decimal)
    " Reserve event for ztrust reservation..."
    @event
    true)

  (defcap FUND () true)

  (defcap REDEEM (account:string)
    (enforce-guard (at 'guard (read ledger6 account)))
  )

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-valid-transfer sender receiver (precision) amount)
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

  (defconst MINIMUM_PRECISION:integer 14)
  (defconst RESERVATION_RATE:decimal 1.0)
  (defconst ZTRUST_BANK:string 'ztrust-bank6)

  (defun ztrust-bank-guard () (create-module-guard 'admin-keyset-peters))

  (defun init ()
    (coin.create-account ZTRUST_BANK (ztrust-bank-guard))
  )

  (defun enforce-unit:bool (amount:decimal)
    (enforce-precision (precision) amount))

  (defun create-account:string
    ( account:string
      guard:guard
    )
    (enforce-valid-account account)
    (insert ledger6 account
      { "balance" : 0.0
      , "guard"   : guard
      })
    )

  (defun get-balance:decimal (account:string)
    (at 'balance (read ledger6 account))
  )

  (defun details:object{fungible-v2.account-details}
    ( account:string )
    (with-read ledger6 account
      { "balance" := bal
      , "guard" := g }
      { "account" : account
      , "balance" : bal
      , "guard": g })
    )

  (defun rotate:string (account:string new-guard:guard)
    (with-read ledger6 account
      { "guard" := old-guard }
      (enforce-guard old-guard)
      (update ledger6 account
        { "guard" : new-guard }))
    )

  (defun fund:string (account:string amount:decimal guard:guard)
    (with-capability (CREDIT account)
      (require-capability (FUND))
        (credit account guard amount))
  )

  (defun reserve:string (account:string amount-kda:decimal)
    (coin.transfer account ZTRUST_BANK amount-kda)
    (let
      ( (tx-id (hash {"account": account, "amount": amount-kda, "salt": (at "block-time" (chain-data))}))
        (amount-ztrust (* amount-kda RESERVATION_RATE))
        (g (at 'guard (coin.details account)))
      )
      (insert reservations6 (format "{}-{}" [account, tx-id])
        { "account"        : account
        , "amount-kda"     : amount-kda
        , "amount-ztrust"  : amount-ztrust
        })
      (with-capability (RESERVE account amount-kda)
        (with-capability (FUND)
          (fund account amount-ztrust g))
      )

    )
  )

  (defun redeem:string (account:string redeem-account:string redeem-guard:guard)
    (with-capability (REDEEM account)
      (with-read ledger6 account
        { "balance" := amount-ztrust
        }
        (let ((amount-kda (floor (/ amount-ztrust RESERVATION_RATE) (coin.precision))))
          (install-capability (coin.TRANSFER ZTRUST_BANK redeem-account amount-kda))
          (coin.transfer-create ZTRUST_BANK redeem-account redeem-guard amount-kda)
          (update ledger6 account {
            "balance" : 0.0
          })
          {
            "account": account,
            "balance": amount-ztrust,
            "redeem-account": redeem-account,
            "redeem-guard": redeem-guard,
            "redeem-kda": amount-kda
          }
          ))))

  (defun precision:integer ()
    MINIMUM_PRECISION)

  (defun transfer:string (sender:string receiver:string amount:decimal)
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision) amount)

    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (with-read ledger6 receiver
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

  (defun debit:string (account:string amount:decimal)
    (require-capability (DEBIT account))
    (with-read ledger6 account
      { "balance" := balance }
      (enforce (<= amount balance) "Insufficient funds")
      (update ledger6 account
        { "balance" : (- balance amount) }
        ))
    )

  (defun credit:string (account:string guard:guard amount:decimal)
    (require-capability (CREDIT account))
    (with-default-read ledger6 account
      { "balance" : 0.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      ; we don't want to overwrite an existing guard with the user-supplied one
      (enforce (= retg guard)
        "account guards do not match")
      (write ledger6 account
        { "balance" : (+ balance amount)
        , "guard"   : retg
        })
      ))

  (defun read-reservations (account:string)
    (select reservations6 (where 'account (= account)))
  )

  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (enforce false "cross chain not supported"))
    )

)


