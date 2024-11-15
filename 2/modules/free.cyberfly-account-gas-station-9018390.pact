(module cyberfly-account-gas-station GOVERNANCE

  (implements gas-payer-v1)
  (use coin)
  (use util.guards1)

  (defschema gas
    balance:decimal
    guard:guard)

  (deftable ledger:{gas})

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard "free.cyberfly_team")))

  (defconst GAS_STATION "cyberfly-account-gas")
  (defcap GAS_PAYER:bool
    ( user:string
      limit:integer
      price:decimal
    )
    (enforce (= "exec" (at "tx-type" (read-msg))) "Inside an exec")
    (enforce (= 1 (length (at "exec-code" (read-msg)))) "Tx of only one pact function")
    (enforce-one "Anyone of these should succeed" [(enforce (= "(coin.create-account" (take 20 (at 0 (at "exec-code" (read-msg))))) "only Coin.create-account function") (enforce (= "(free.cyberfly" (take 14 (at 0 (at "exec-code" (read-msg))))) "Only Cyberfly smart contract")] )
    (enforce-below-or-at-gas-price 0.0000001)
    (enforce-below-or-at-gas-limit 2000)
    (compose-capability (ALLOW_GAS))
  )

  (defcap ALLOW_GAS () true)

  (defun init ()
    (coin.create-account GAS_STATION
      (guard-any
        [
          (create-gas-payer-guard)
          (at 'guard (coin.details "k:f03fb771f7727797291d877e5085803c84e72d06563a6dc704453dde404fca79"))
        ]
      )
    )
  )

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-payer-guard))
  )

  (defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
  )
)


