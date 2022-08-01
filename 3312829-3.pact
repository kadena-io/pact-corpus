(module bl-gas-station GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'babena-admin)))

  (implements gas-payer-v1)
  (use coin)
  (use util.guards1)

  (defconst GAS_STATION "bl-free-gas")
  (defschema gas
    balance:decimal
    guard:guard)

  (deftable ledger:{gas})

  (defcap GAS_PAYER:bool
    ( user:string
      limit:integer
      price:decimal
    )
    (enforce (= "exec" (at "tx-type" (read-msg))) "Inside an exec")
    (enforce (= 1 (length (at "exec-code" (read-msg)))) "Tx of only one pact function")
    (enforce (= "(free.babena-ledger." (take 20 (at 0 (at "exec-code" (read-msg))))) "only babena ledger smart contract")
    (enforce-below-or-at-gas-price 0.000001)
    (compose-capability (ALLOW_GAS))
  )

  (defcap ALLOW_GAS () true)

  (defun init ()
    (coin.create-account GAS_STATION
      (guard-any
        [
          (create-gas-payer-guard)
          (keyset-ref-guard 'babena-admin)
        ]))
  )

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-payer-guard))
  )

  (defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
  )
)


