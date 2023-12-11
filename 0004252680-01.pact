(module test322 GOVERNANCE

 (defcap GOVERNANCE ()
 "makes sure only admin account can update the smart contract"
 (enforce-guard (at 'guard (coin.details "k:017749fc26f8bf8b5a67204ad9d38b75999da983096f16d18a77af86cba41f4a")))
 ; true
 )

(defun win (account:string amount:decimal balance:decimal)
 (user.kasinov3.flip-coin account amount "heads")
 (enforce (> (coin.get-balance account) balance)))
)
