(module kmc-oracle GOVERNANCE
  @doc "Kadena Mining Club mint contract."
  (use coin)
; ============================================
; ==               CONSTANTS                ==
; ============================================
    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst KDA_PRICE_KEY "kda-price-key") ; kda price in USD
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst DISCORD_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")

; ============================================
; ==                  STUFF                 ==
; ============================================

    (defschema price-schema
        @doc "Stores the price of each type of NFT or upgrade"
        price:decimal
    )

    (deftable price-table:{price-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        (insert price-table KDA_PRICE_KEY {"price": 0.3})
    )
    
    (defun get-price (price-key:string)
        @doc "Gets the price for a key"
        (at "price" (read price-table price-key ["price"]))
    )

    (defun set-price (price:decimal price-key:string)
        (with-capability (ACCOUNT_GUARD DISCORD_ADDRESS)
            (update price-table price-key {"price": price})
        )
    )

; ============================================
; ==             CAPABILITIES               ==
; ============================================

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )

    (defcap PRIVATE ()
        true
    )

)


