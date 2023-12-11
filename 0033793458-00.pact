(module dao-hive-gas GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-keyset "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.admin")
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
    (compose-capability (ALLOW_GAS))
  )

  (defun chain-gas-price ()
    (at 'gas-price (chain-data)))

  (defun chain-gas-limit ()
    (at 'gas-limit (chain-data)))

  (defun enforce-below-or-at-gas-price:bool (gasPrice:decimal)
    (enforce (<= (chain-gas-price) gasPrice)
      (format "Gas Price > {}" [gasPrice])))

  (defun enforce-below-or-at-gas-limit:bool (gasLimit:integer)
    (enforce (<= (chain-gas-limit) gasLimit)
      (format "Gas limit > {}" [gasLimit])))

  (defcap ALLOW_GAS () true)

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-payer-guard))
  )

  (defcap DAO_GAS (msg:object)
    @event true
  )

  (defun gas-payer-guard ()
  (require-capability (GAS))
  (at "miner-keyset" (read-msg))
  (enforce-below-or-at-gas-price 0.000001)
  (enforce-below-or-at-gas-limit 10000)
  )

  (defun init()
    (coin.create-account "swarms-gas-payer" (create-gas-payer-guard))
  )

)

