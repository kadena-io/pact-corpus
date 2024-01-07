(module kda-skellies-gas GOVERNANCE
    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )

  (implements gas-payer-v1)
  (use coin)
  (defconst ADMIN_KEYSET "free.skellies-admin-keys")
  (defconst SKELLIES_NAMESPACE "(free.kda-skellies.")

  (defschema gas
    balance:decimal
    guard:guard)

  (deftable ledger:{gas})

  (defcap GAS_PAYER:bool
    ( user:string
      limit:integer
      price:decimal
    )
    ;(enforce (= "exec" (at "tx-type" (read-msg))) "Can only be used inside an exec")
    ;(enforce (> (length (at "exec-code" (read-msg))) 0) "Tx at least one pact function")
    ;(enforce (<= (length (at "exec-code" (read-msg))) 2) "Tx has too many pact functions")
	(enforce (= SKELLIES_NAMESPACE (take (length SKELLIES_NAMESPACE) (at 0 (at "exec-code" (read-msg))))) "Only free.kda-skellies smart contract")
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
;(coin.create-account "kda-skellies-gas-payer" (free.kda-skellies-gas.create-gas-payer-guard))
