(module kai-gas-station GOV
    (defcap GOV ()
      @doc "protected by admin keys"
      (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.admin"))
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
      (enforce (<= 4 (length (at "exec-code" (read-msg)))) "Tx up to 4 pact functions")
      (enforce (= "(n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c." (take 44 (at 0 (at "exec-code" (read-msg))))) "Kai smart contracts")
      (compose-capability (ALLOW_GAS))
    )

    (defcap ALLOW_GAS () true)
  
    (defun create-gas-payer-guard:guard ()
      (create-user-guard (gas-payer-guard))
    )
  
    (defun gas-payer-guard ()
      (enforce-below-or-at-gas-price 0.000001)
      (enforce-below-or-at-gas-limit 3500)
      (require-capability (GAS))
      (require-capability (ALLOW_GAS))
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

 (defun init()
 (coin.create-account "kai-gas-payer-1" (create-gas-payer-guard))
 )

)
  

