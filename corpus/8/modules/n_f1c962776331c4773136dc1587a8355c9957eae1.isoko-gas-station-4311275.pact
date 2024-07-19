(module isoko-gas-station GOVERNANCE
  (defcap GOVERNANCE ()
    "makes sure only admin account can update the smart contract"
    (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
  )
  (implements gas-payer-v1)
  (use coin)

  (defconst GAS_STATION "isoko-gas-station")

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
        (enforce (>= 3 (length (at "exec-code" (read-msg)))) "Tx of only one pact function")
        (enforce (= "(n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-orchestrator." (take 63 (at 0 (at "exec-code" (read-msg))))) "only free.isoko smart contract")
        (enforce-below-or-at-gas-price 0.0000002)
        (compose-capability (ALLOW_GAS))
    )
    (defun chain-gas-price ()
      "Return gas price from chain-data"
      (at 'gas-price (chain-data)))
    
    (defun enforce-below-or-at-gas-price:bool (gasPrice:decimal)
      (enforce (<= (chain-gas-price) gasPrice)
        (format "Gas Price must be smaller than or equal to {}" [gasPrice])))

    (defcap ALLOW_GAS () true)

    (defun init ()
      (coin.create-account GAS_STATION
        (guard-any
          [
            (create-gas-payer-guard)
            (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")
          ]))
    )

    (defun create-gas-payer-guard:guard ()
        (create-user-guard (gas-payer-guard))
    )

  (defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
  )

    (defun guard-any:guard (guards:[guard])
      "Create a guard that succeeds if at least one guard in GUARDS is successfully enforced."
      (enforce (< 0 (length guards)) "Guard list cannot be empty")
      (create-user-guard (enforce-guard-any guards)))
    
    (defun enforce-guard-any:bool (guards:[guard])
      "Will succeed if at least one guard in GUARDS is successfully enforced."
      (enforce (< 0
        (length
          (filter
            (= true)
            (map (try-enforce-guard) guards))))
        "None of the guards passed"))
    
    (defun try-enforce-guard (g:guard)
      (try false (enforce-guard g))
    )

)


