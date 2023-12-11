(module kmc-immersion-mint GOVERNANCE
  @doc "Kadena Mining Club mint contract."
    (use coin)
    (use marmalade.ledger)
    (use kmc-test3-policy)
    (use kmc-oracle)
; ============================================
; ==               CONSTANTS                ==
; ============================================
    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst MINT_STATUS "mint-status")
        ;can be either "not started" "paused" or "started"
    (defconst WHITELIST_USD_PRICE 1)
    (defconst PUBLIC_USD_PRICE 2)
    
    (defconst MAX_SUPPLY 10000 "The max supply of immersion vials")

    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst DISCORD_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")

; ============================================
; ==            SCHEMA AND TABLES           ==
; ============================================

    (defschema mint-schema
        status:string
    )

    (defschema wl-tracker-schema
        @doc "stores how many whitelist mints one account has remaining"
        wl-mints-remaining:integer
    )

    (deftable wl-remaining-per-account:{wl-tracker-schema})
    (deftable mint-status:{mint-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        (insert mint-status MINT_STATUS {"status": "not started"})
    )

; ============================================
; ==              MINT FUNCTIONS            ==
; ============================================
        
    (defun set-mint-status (status:string)
        @doc "not started, paused, whitelist, public"
        (with-capability (ADMIN)
            (update mint-status MINT_STATUS
                {'status : status }  
            )
        )
    )

    (defun mint (account:string amount:integer)
        (enforce (> amount 0) "You must mint at least 1 NFT, please do not submit your transaction")
            (with-capability (PRIVATE)
                (map (marmalade-mint (coin-account-guard account)) (make-list amount account))
            )
    )

    (defun marmalade-mint (account-guard:guard account:string)
        (require-capability (PRIVATE))
        (let 
            (
                (token-id (kmc-test3-policy.get-token-nft-id (int-to-str 10 (+ (kmc-test3-policy.get-count "mint-count") 1))))
            )
            ; (marmalade.ledger.create-account token-id account account-guard); "")
            (install-capability (marmalade.ledger.MINT token-id account 1.0))
            (marmalade.ledger.mint token-id account account-guard 1.0)
        )
    )

    (defun get-whitelist-details (account:string)
        (with-default-read wl-remaining-per-account account
            {"wl-mints-remaining" : 0}
            {"wl-mints-remaining" := mints-remaining }
            (if (< mints-remaining 0)
                0
                mints-remaining)
        )
    )

    (defun update-whitelist (account:string number-of-mints:integer)
        (enforce-mint-status true) ;true for only whitelist, false everywhere else
        (with-capability (ADMIN)
            (with-default-read wl-remaining-per-account account
                {"wl-mints-remaining" : -1}
                {"wl-mints-remaining" := mints-remaining }
                (if (!= -1 mints-remaining) 
                    (update wl-remaining-per-account account 
                        {'wl-mints-remaining : number-of-mints})
                    (insert wl-remaining-per-account account
                        {'wl-mints-remaining : number-of-mints})
                )
            )
        )
    )
    
    (defun enforce-mint-status (wl:bool) ; wl is set to true if checking for whitelist
        @doc "Functions will not run if this check fails"
        (with-read mint-status MINT_STATUS
            {'status:= status}
            (if (= true wl)
                (enforce (= status "not started") "mint has started, so whitelist cannot be updated")
                (enforce (= status "started") "mint is not running, so no write functions will work")
            )
        status
        )
    )
    
    (defun get-current-nft-price:decimal (isWhitelistTime:bool)
        @doc "returns the price of 1 NFT in terms of kda, \
        \ accounting for whitelist/public and current usd value of kda"
        (let*
            (
                (price-of-kda (kmc-oracle.get-price "kda-price-key"))
                (price (if (= true isWhitelistTime) (/ WHITELIST_USD_PRICE price-of-kda) (/ PUBLIC_USD_PRICE price-of-kda)))
            )
            (round price 2)
        )
    )

    (defun coin-account-guard (account:string)
        @doc "enforces coin account guard"
        (at "guard" (coin.details account))
    )

    (defun get-time ()
        (at "block-time" (chain-data))
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

    (defcap PRIVATE ()
        true
    )


    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )
)



; (create-table counts-table)
; (create-table wl-remaining-per-account)
; (create-table mint-status)
; (initialize)

