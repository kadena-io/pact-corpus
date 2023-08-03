(module kmc-immersion-mint GOVERNANCE
  @doc "Kadena Mining Club mint contract."
    (use coin)
    (use marmalade.ledger)
    (use kmc-test-policy)
; ============================================
; ==               CONSTANTS                ==
; ============================================
    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst VIALS_CREATED_COUNT "vials-count")
    (defconst CURRENT_ID_COUNT "current-id-count")
    (defconst GAS_PER_NFT_MINTED "gas")
    (defconst MINT_STATUS "mint-status")
        ;can be either "not started" "paused" or "started"
    (defconst PRICE_KEY "price-key")
    (defconst WHITELIST_PRICE_KEY "wl-price-key")
    (defconst MINT_CHAIN_ID "mint-chain-id")
    (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
    
    (defconst MAX_SUPPLY 10000 "The max supply of immersion vials")

    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst DISCORD_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")

; ============================================
; ==            SCHEMA AND TABLES           ==
; ============================================

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )

    (defschema price-schema
        @doc "Stores the price of each type of NFT or upgrade"
        price:decimal
    )

    (defschema values-schema
        @doc "Schema used for storing basic values"
        value:string
    )

    (defschema mint-schema
        status:string
    )

    (defschema wl-tracker-schema
        @doc "stores how many whitelist mints one account has remaining"
        wl-mints-remaining:integer
    )

    (deftable wl-remaining-per-account:{wl-tracker-schema})
    (deftable counts-table:{counts-schema})
    (deftable price-table:{price-schema})
    (deftable values-table:{values-schema})
    (deftable mint-status:{mint-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        (insert counts-table VIALS_CREATED_COUNT {"count": 0})
        (insert mint-status MINT_STATUS {"status": "not started"})
        ; (insert counts-table MINERS_CREATED_COUNT {"count": 1})
        ; (insert counts-table KDA_MINED_UPDATE_COUNT {"count": 0})
        ; (insert counts-table NFT_OFFER_ID {"count": 0})
        (insert price-table PRICE_KEY {"price": 0.3})
        (insert price-table WHITELIST_PRICE_KEY {"price": 0.2})
        ; (insert values-table MINT_CHAIN_ID {"value": "8"})
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

    (defun set-price(key:string value:decimal)
        @doc "Sets the price for a key to store in the price-table"
        (with-capability (ADMIN)
            (update price-table key 
                {"price": value} 
            )
        )
    )

    (defun mint (account:string amount:integer)
         ; TODO: implement oracle contract
        ;if whitelist time window, check whitelist status for deciding price and number of mints
        ;also update mints remaining
        (enforce (> amount 0) "you must mint at least 1 NFT")
        (let*
            (
                (mint-status (enforce-mint-status false))
                (mints-remaining (get-whitelist-details account))
                (tiempo (get-time))
                (isWhitelistTime (if (and (> tiempo (time "2022-06-14T12:00:00Z")) 
                                (< tiempo (add-time (time "2022-06-30T12:00:00Z") 86400)))
                                true false))
                (price (if (= true isWhitelistTime) (get-price WHITELIST_PRICE_KEY) (get-price PRICE_KEY)))
            )
            (coin.transfer account ADMIN_ADDRESS (* price amount))
            (if (= true isWhitelistTime)
                (with-capability (PRIVATE)
                    (enforce (<= amount mints-remaining) (format "Cannot mint more than your allocated whitelist mints. You have {} remaining. Come back during the public sale in < 24 hours" [mints-remaining]))
                    (update wl-remaining-per-account account 
                        {'wl-mints-remaining : (- mints-remaining amount)})
                )
                ""
            )
            (map (marmalade-mint (coin-account-guard account)) (make-list amount account))
        )
    )

    (defun marmalade-mint (account-guard:guard account:string)
        (require-capability (PRIVATE))
        (let 
            (
                (token-id (kmc-test-policy.get-marmalade-id 0))
            )
            (marmalade.ledger.create-account token-id account account-guard); "")
            (install-capability (marmalade.ledger.MINT token-id account 1.0))
            (marmalade.ledger.mint token-id account account-guard 1.0)
        )
    )

    (defun get-whitelist-details (account:string)
        (with-default-read wl-remaining-per-account account
            {"wl-mints-remaining" : 0}
            {"wl-mints-remaining" := mints-remaining }
            mints-remaining
        )
    )

    (defun update-whitelist (account:string number-of-mints:string)
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

    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counts-table key ['count]))
    )

    (defun set-count(key:string value:integer)
        @doc "Sets the count for a key to store in the counts-table"
        (with-capability (ADMIN)
            (update counts-table key 
                {"count": value} 
            )
        )
    )

    (defun increase-count (key:string)
        ;increase the count of a key in a table by 1
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )

    (defun set-value(key:string value:string)
        @doc "Sets the value for a key to store in a table"
        (with-capability (ADMIN)
            (update values-table key 
                {"value": value} 
            )
        )
    )
    
; ============================================
; ==     NON STATE-MODIFYING FUNCTIONS      ==
; ============================================
 

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
    
    (defun get-price (price-key:string)
        @doc "Gets the price for a key"
        (at "price" (read price-table price-key ["price"]))
    )
    (defun get-value (key:string)
        @doc "Gets value for a key"
        (at "value" (read values-table key ['value]))
    )

    ; (key) inputs are the same as get-count
    (defun id-for-next-key (key:string)
        @doc "returns the next id for a given key"
        (int-to-str 10 (get-count key))
    )

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )

    (defun get-all-miner-owners ()
        (kmc-policy.get-all-owners)
    )
 
; ============================================
; ==           COIN ACCOUNT CHECKS          ==
; ============================================

    (defun enforce-coin-account-exists (account:string)
        (let ((exist (coin-account-exists account)))
            (enforce exist "Account does not exist in coin contract, please send 0.001 KDA to the address on chain 8 and try again."))
    )

    (defun coin-account-exists:bool (account:string)
        (try false
            (let ((ok true))
                (coin.details account)
                ok))
    )

    (defun coin-account-guard (account:string)
        @doc "enforces coin account guard"
        (at "guard" (coin.details account))
    )

    (defun get-coin-guard (account:string)
       (format "{}" [(at "guard" (coin.details account))])
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

    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )

    (defcap OWNER (account:string id:string)
        @doc "Enforces that an account owns the particular miner ID"
        (let
            (
                (nft-owner (kmc-test-policy.get-owner id))
            )
            (enforce (= nft-owner account) "Account is not owner of the NFT")
                (compose-capability (ACCOUNT_GUARD account))
        )
    )

    (defcap TRANSFER (id:string sender:string receiver:string amount:decimal)
        @doc "Allows transferring of NFTs between two accounts"
        @event true
    )

    (defcap MINT_VIAL (id:string account:string)
        @doc "Emitted event when a Vial NFT is purchased"
        @event true
    )

    (defcap PRIVATE ()
        true
    )

    (defcap CALL-POLICY-MODULES () 
        true 
    )

    (defcap BANK_DEBIT () true)
)



; (create-table counts-table)
; (create-table wl-remaining-per-account)
; (create-table price-table)
; (create-table values-table)
; (create-table mint-status)
; (initialize)

