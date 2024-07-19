(module isoko-non-fungible-policy-v1 GOVERNANCE

  @doc "Concrete policy for issuing an nft with a fixed supply of 1 and precision of 0"

  (defconst ADMIN-KS:string "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")
  (defconst NON-FUNGIBLE-POLICY:string (format "{}" [isoko-non-fungible-policy-v1]))

  (defcap GOVERNANCE ()
        (enforce-one "Any Guard passes" [
            (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
            (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")))]
        )
  )

  (implements kip.token-policy-v2)
  (use marmalade-v2.policy-manager)
  (use kip.token-policy-v2 [token-info])

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) NON-FUNGIBLE-POLICY))
    (enforce (= 0 (at 'precision token)) "Precision must be 0")
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (MINT-CALL (at "id" token) account amount NON-FUNGIBLE-POLICY))
    (enforce (= amount 1.0) "Mint can only be 1")
    (enforce (= (at 'supply token) 0.0) "Only one mint allowed")
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce false "Burn prohibited")
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string
    )
    true
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    true
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    true
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    true
  )

)


