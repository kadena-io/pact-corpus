(module de-guestbook-gas-station GOVERNANCE
  (defcap GOVERNANCE ()
    "makes sure only admin account can update the smart contract"
    ;change "Julz" to your account name registered on the chain you are trying to deploy
    (enforce-guard (at 'guard (coin.details "Julz")))
    ; true
  )

  (implements gas-payer-v1)
  (use coin)
  (use util.guards1)

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
    ;change the code below to the name of your module
    ;make sure the number after take is one longer than your contract with the namespace prefix
    ;example: free.de-guestbook is 17 long so I put an 18 there. This enforce checks if your contract is called
    (enforce (= "(free.de-guestbook" (take 18 (at 0 (at "exec-code" (read-msg))))) "only de-guestbook token smart contract")
    ;enforce a gas price and limit. IF transactions fail you may have to increase it.
    (enforce-below-or-at-gas-price 0.00000001)
    (enforce-below-or-at-gas-limit 800)
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
;replace k:c5ddd with an account you control. guestbook-gas-payer with an original account name if it's taken already. change 2.0 to set a different amount to be transferred to the gas station.replace "free.de-guestbook-gas-station with your own contract name"
;(coin.transfer-create "l:c5ddd112e3fe1e062fe576a2de1aff398960ffcc220e3ec5011ebf01618999ba" "guestbook-gas-payer" (free.de-guestbook-gas-station.create-gas-payer-guard) 2.0)
;(create-table ledger)

