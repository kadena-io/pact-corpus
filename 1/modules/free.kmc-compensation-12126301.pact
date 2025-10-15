(module kmc-compensation GOVERNANCE
  @doc "Kadena Mining Club compensation contract."
    (use coin)

; ============================================
; ==               CONSTANTS                ==
; ============================================
    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst COMPENSATION_INCREASED_COUNT "compensation-increased-count")
    (defconst COMPENSATION_WALLET "kmc-comp-wallet")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst DISCORD_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")

; ============================================
; ==            SCHEMA AND TABLES           ==
; ============================================

    (defschema user-data-schema
        @doc "key is the users k:address"
        effective-nfts:decimal
        account:string
        claim-count:integer
    )

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )

    (defschema price-schema
        @doc "Stores decimals"
        price:decimal
    )

    (deftable counts-table:{counts-schema})
    (deftable price-table:{price-schema})
    (deftable user-data-table:{user-data-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        (insert counts-table COMPENSATION_INCREASED_COUNT { "count" : 0 })
    )

; ============================================
; ==              MINT FUNCTIONS            ==
; ============================================

    (defun withdraw-contract-funds (account:string)
    	(with-capability (ACCOUNT_GUARD ADMIN_ADDRESS)
    	(with-capability (BANK_DEBIT)
    		(coin.transfer account ADMIN_ADDRESS (coin.get-balance account))
    	))
    )


    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counts-table key ['count]))
    )
        
    (defun set-price(key:string value:decimal)
        @doc "Sets the price for a key to store in the price-table"
        (with-capability (ADMIN)
            (update price-table key 
                {"price": value} 
            )
        )
    )

    (defun set-count(key:string value:integer)
        @doc "Sets the count for a key to store in the counts-table"
        (with-capability (ADMIN)
            (update counts-table key 
                {"count": value} 
            )
        )
    )

    ; (defun select-all-collection-offers ()
    ;     (select collection-offer-table (where "address" (!= "nope")))
    ; )

    ; (defun update-claim (account:string amount:string)
    ;     @doc "Adds a claim amount to the airdrop table for an already existing account"
    ;     (require-capability (PRIVATE))
    ;     (with-read airdrop-table account
    ;         {'airdrop := airdrop-temp }
    ;         (update airdrop-table account
    ;             {"airdrop": (+ airdrop-temp (make-list 1 {"amount":amount, "tiempo":(at "block-time" (chain-data))})) }
    ;         )
    ;     )
    ; )

    (defun increase-count (key:string)
        ;increase the count of a key in a table by 1
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )
  
; ============================================
; ==           COIN ACCOUNT CHECKS          ==
; ============================================

    (defun create-simple-user-guard (funder:string amount:decimal)
        (coin.transfer-create funder COMPENSATION_WALLET 
          (create-BANK_DEBIT-guard) amount)
    )

    ;; Capability user guard: capability predicate function
    (defun require-BANK_DEBIT () 
        (require-capability (BANK_DEBIT))
    )
    
    ;; Capability user guard: guard constructor
    (defun create-BANK_DEBIT-guard ()
        (create-user-guard (require-BANK_DEBIT))
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

    (defcap DISCORD(account:string)
        @doc "Only allows the discord wallet to send information"
        (enforce (= account DISCORD_ADDRESS) "only the administrator discord wallet can call this function")
        (compose-capability (ACCOUNT_GUARD DISCORD_ADDRESS))
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

    (defcap CLAIM (account:string amount:decimal comp-number)
        @doc "Emitted event when a user claims KDA from the hashrate wallet"
        @event true
    )

    (defcap PRIVATE ()
        true
    )

    (defcap BANK_DEBIT () true)
)

; (create-table activity-table)
; (initialize)


