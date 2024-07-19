(module gas-station GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_b742b4e9c600892af545afb408326e82a6c0c6ed.bridge-admin")))

  (implements gas-payer-v1)

  (use coin)

  (defun chain-gas-price ()
    (at 'gas-price (chain-data))
  )

  (defun chain-gas-limit ()
    (at 'gas-limit (chain-data))
  )

  (defun enforce-below-or-at-gas-price:bool (gasPrice:decimal)
    (enforce (<= (chain-gas-price) gasPrice)
      (format "Gas Price must be smaller than or equal to {}" [gasPrice]))
  )

  (defun enforce-below-or-at-gas-limit:bool (gasLimit:integer)
    (enforce (<= (chain-gas-limit) gasLimit)
      (format "Gas Limit must be smaller than or equal to {}" [gasLimit])))

  (defcap GAS_PAYER:bool
    ( user:string
      limit:integer
      price:decimal
    )
    (enforce (= "exec" (at "tx-type" (read-msg))) "Can only be used inside an exec")
    (enforce (= 2 (length (at "exec-code" (read-msg)))) "Can only be used to call max two pact functions")
    (enforce
      (= "(n_b742b4e9c600892af545afb408326e82a6c0c6ed." (take 43 (at 0 (at "exec-code" (read-msg)))))
      "Module caller is not allowed"
    )
    (enforce-below-or-at-gas-price 0.000001)
    (enforce-below-or-at-gas-limit 2500)
    (compose-capability (ALLOW_GAS))
  )

  (defcap ALLOW_GAS () true)

  (defun create-gas-payer-guard:guard ()
    (create-capability-guard (ALLOW_GAS))
  )

  (defconst GAS_STATION_ACCOUNT (create-principal (create-gas-payer-guard)))

  (defun init ()
    (coin.create-account GAS_STATION_ACCOUNT (create-gas-payer-guard))
  )
)


