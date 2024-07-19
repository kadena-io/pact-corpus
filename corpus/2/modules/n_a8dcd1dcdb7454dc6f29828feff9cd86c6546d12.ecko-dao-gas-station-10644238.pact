(module ecko-dao-gas-station GOVERNANCE
    (defcap GOVERNANCE ()
        (enforce-guard
        (keyset-ref-guard "n_a8dcd1dcdb7454dc6f29828feff9cd86c6546d12.eckodao-gov")))

    ; Signal that the module implements the gas-payer-v1 interface
    (implements gas-payer-v1)
  
    ; Import the coin module, we need it to create a KDA account that will be controlled
    ; by the gas station
    (use coin)

    (defun chain-gas-price ()
    "Return gas price from chain-data"
    ; chain-data is a built-in function that returns tx public metadata
    ; we are using it to retrieve the tx gas price
    (at 'gas-price (chain-data)))
  
    (defun enforce-below-or-at-gas-price:bool (gasPrice:decimal)
      (enforce (<= (chain-gas-price) gasPrice)
        (format "Gas Price must be smaller than or equal to {}" [gasPrice])))
  
    (defcap GAS_PAYER:bool
      ( user:string
        limit:integer
        price:decimal
      )
  
      (enforce (= "exec" (at "tx-type" (read-msg))) "Inside an exec")
      (enforce (= 1 (length (at "exec-code" (read-msg)))) "Tx of only one pact function")
      (enforce
        (= "(n_a8dcd1dcdb7454dc6f29828feff9cd86c6546d12.ecko-dao." (take 53 (at 0 (at "exec-code" (read-msg)))))
        "Only n_a8dcd1dcdb7454dc6f29828feff9cd86c6546d12 ecko-dao module calls allowed")

      (enforce-below-or-at-gas-price 0.0000001)
  
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
  
    (defconst GAS_STATION "ecko-dao-gas-station")
  
    (defun init ()
      (coin.create-account GAS_STATION (create-gas-payer-guard))
    )
  )
