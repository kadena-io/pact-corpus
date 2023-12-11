(module hello-world-gas-station GOVERNANCE
  (defcap GOVERNANCE ()
    "makes sure only admin account can update the smart contract"
    (enforce-guard (at 'guard (coin.details "cc21dbb944f9fbc9b02ab72d1635d1b6fba90b91472e39ef34633fde822cf8e4")))
  )

  (implements gas-payer-v1)
  (use coin)

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
    (enforce (= "(free.hello-world." (take 18 (at 0 (at "exec-code" (read-msg))))) "only hello world token smart contract")
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

