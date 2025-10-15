(module kinesis-gas-station GOVERNANCE

  ;; Imports 
  (implements gas-payer-v1)
  (use coin)
  (use guards1)

  ;; Capabilities
  (defcap GOVERNANCE () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.upgrade-admin"))
  (defcap ONLY_ADMIN () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.bridge-admin"))

  (defcap GAS_PAYER:bool
    ( user:string
      limit:integer
      price:decimal
    )
    (create-user-guard(coin.gas-only))
    (n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.guards1.max-gas-price 0.00000001)
    (n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.guards1.max-gas-limit limit)
    (enforce (= "cont" (at "tx-type" (read-msg))) "Can only be used inside an cont tx")
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
