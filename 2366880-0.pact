(module colorblock-gas-station-test GOVERNANCE

  (implements gas-payer-v1)
  (use coin)
  (use util.guards1)

  (defschema gas
    balance:decimal
    guard:guard)

  (deftable ledger:{gas})

  (defcap GOVERNANCE ()
    @doc " Only support upgrading by admin."
    (enforce-guard (at 'guard (coin.details "colorblock-admin-test")))
  )

  (defcap GAS_PAYER:bool
    ( user:string
      limit:integer
      price:decimal
    )
    (enforce (= "exec" (at "tx-type" (read-msg))) "Inside an exec")
    (enforce (= 1 (length (at "exec-code" (read-msg)))) "Tx of only one pact function")
    (enforce 
      ( or
        (= "(free.colorblock." (take 17 (at 0 (at "exec-code" (read-msg)))))
        (= "(free.colorblock-market." (take 24 (at 0 (at "exec-code" (read-msg)))))
      )
      "only colorblock smart contract"
    )
    (enforce-below-or-at-gas-price 0.000000000001)
    (enforce-below-or-at-gas-limit 800)
    (compose-capability (ALLOW_GAS))
  )

  (defcap ALLOW_GAS () true)

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-payer-guard))
  )

  (defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
  )
)



