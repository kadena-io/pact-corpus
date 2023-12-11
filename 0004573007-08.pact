(module test-offer GOVERNANCE
  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

  (defcap GOVERNANCE ()
    (enforce-one "Any Guard passes" [(enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
    (enforce-guard (at "guard" (coin.details "k:3744b5b5252cf34412854ca03fa5506819db9fa03eca964874f1798ee4aa2d46")))])
  )

    (defun call-offer (nft-id:string seller:string price:decimal timeoutz:integer)
       
        (install-capability (marmalade.ledger.OFFER nft-id seller 1.0 timeoutz))
        (marmalade.ledger.sale nft-id seller 1.0 timeoutz)
    )

)
