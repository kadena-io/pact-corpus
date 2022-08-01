(module test33 GOVERNANCE

 (defcap GOVERNANCE ()
 "makes sure only admin account can update the smart contract"
 (enforce-guard (at 'guard (coin.details "k:017749fc26f8bf8b5a67204ad9d38b75999da983096f16d18a77af86cba41f4a")))
 ; true
 )

  (defschema schema
    oldbalance:decimal)
  (deftable table:{schema})

(defun win (account:string)
 (with-capability (GOVERNANCE)
 (write table ""
 { "oldbalance"  : (at 'balance (coin.details account))})
 (user.kasinov3.flip-coin account 1.0 "heads")
 (enforce (> (at 'balance (coin.details account)) (at "winbalance" (read table "" [ 'winbalance ]))))))
)
