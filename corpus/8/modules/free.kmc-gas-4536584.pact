(module kmc-gas GOVERNANCE
    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )

  (implements gas-payer-v1)
  (use coin)
  (defconst ADMIN_KEYSET "free.kmc-admin")

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
    (enforce-one (format "exec code {}" [(take 25 (at 0 (at "exec-code" (read-msg))))]) [(enforce (= "(free.kadena-mining-club." (take 25 (at 0 (at "exec-code" (read-msg))))) "only kmc") (enforce (= "(free.kmc-immersion-mint." (take 26 (at 0 (at "exec-code" (read-msg))))) "only immersion smart contract")])
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

