(module favorites-gas-station GOVERNANCE
 (defcap GOVERNANCE ()
   (enforce-keyset "n_b9880e372b7ec49d21ef4afe82ece29c3a536bde.admin-keyset")
 )

 (defcap GAS_PAYER:bool
   ( user:string
     limit:integer
     price:decimal
   )
   (enforce (= "exec" (at "tx-type" (read-msg))) "Can only be used inside an exec")
   (enforce (= 1 (length (at "exec-code" (read-msg)))) "Can only be used to call one pact function")
   (let ((call (at 0 (at "exec-code" (read-msg)))))
     (enforce
       (or
         (contains "n_b9880e372b7ec49d21ef4afe82ece29c3a536bde.favorites." call)
         (contains "coin.create-account" call)
       )
       "Only favorites module calls and coin.create-account are allowed"
     ))
   (enforce-below-or-at-gas-price 0.000001)
   (compose-capability (ALLOW_GAS))
 )

 (defcap ALLOW_GAS () true)

 (defun create-gas-payer-guard:guard ()
   (create-capability-guard (ALLOW_GAS))
 )

 (implements gas-payer-v1)

 (defconst GAS_STATION_ACCOUNT (create-principal (create-gas-payer-guard)))

 (defun init()
   (coin.create-account GAS_STATION_ACCOUNT (create-gas-payer-guard))
   (format "Gas Station Wallet Address : {}" [GAS_STATION_ACCOUNT])
 )

 (defun chain-gas-price ()
   (at 'gas-price (chain-data))
 )

 (defun enforce-below-or-at-gas-price:bool (gasPrice:decimal)
   (enforce (<= (chain-gas-price) gasPrice)
     (format "Gas Price must be lower than or equal to {}"
       [gasPrice]
     )
   )
 )
)


