(module prod-hypercent-gas-station GOVERNANCE
    (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard "hypercent.hyper-admin-1-prod")))
  
    (implements gas-payer-v1)
    (use coin)
  
    (defun chain-gas-price ()
      "Return gas price from chain-data"
      (at 'gas-price (chain-data)))
  
    (defun enforce-below-or-at-gas-price:bool (gasPrice:decimal)
      (enforce (<= (chain-gas-price) gasPrice)
        (format "Gas Price must be smaller than or equal to {}" [gasPrice])))
  
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
    (defconst GAS_STATION "prod-hype-free-gas")
    (defconst HYPERCENT_NAMESPACE "(hypercent.")
  
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
      (enforce (= HYPERCENT_NAMESPACE (take (length HYPERCENT_NAMESPACE) (at 0 (at "exec-code" (read-msg))))) "only hypercent namespace")
      (enforce-below-or-at-gas-price 0.000001)
      (compose-capability (ALLOW_GAS))
    )
  
    (defcap ALLOW_GAS () true)
  
    (defun init ()
      (coin.create-account GAS_STATION
        (guard-any
          [
            (create-gas-payer-guard)
            (keyset-ref-guard "hypercent.hyper-admin-1-prod")
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
  
  
  


