(module kdswap-gas-station GOVERNANCE
  (defcap GOVERNANCE ()
    "makes sure only admin account can update the smart contract"
    (enforce-keyset 'kdlaunch-admin)
  )

  (implements gas-payer-v1)
  (use coin)
  (use util.guards1)

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
    (enforce (= "(kdlaunch.kdswap" (take 16 (at 0 (at "exec-code" (read-msg))))) "only kdlaunch.kdswap smart contracts")
    (max-gas-price 0.00000001)
    (max-gas-limit 60000)
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


