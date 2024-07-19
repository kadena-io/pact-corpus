(module sgk-account-all-weapons GOVERNANCE
    @doc "All Weapon policies contract."

    (defcap GOVERNANCE ()
        (enforce-one "Any Guard passes" [
            (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
            (enforce-guard (at "guard" (coin.details "k:a3ecc7fc15052ea4ffecad3035bad35c8e3b20a70ddb5227e4c35d227e4c0d13")))
        ])
    )

    (defun get-all-weapons (account:string)
        (let*
            (
                (c-1-2 (free.sgk-weapons-policy-1-2.get-nfts-by-owner account))
                (c-1-3 (free.sgk-weapons-policy-1-3.get-nfts-by-owner account))
                (c-1-4 (free.sgk-weapons-policy-1-4.get-nfts-by-owner account))
            ) 
            (list c-1-2 c-1-3 c-1-4)
        )
    )
)
