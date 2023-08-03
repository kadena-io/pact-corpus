(module kmc-vial-policy GOVERNANCE
  @doc "Kadena Mining Club Immersion Vial policy"
    (implements kip.token-policy-v1)
    (implements free.kmc-token-policy-v13)
    (use kip.token-policy-v1 [token-info])
    (use coin)
    (use marmalade.ledger)
    (use kmc-oracle)

    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst MINT_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")
    (defconst NFT_COUNT:string "nft-count")
    (defconst MINT_COUNT:string "mint-count")
    (defconst MINT_STATUS "mint-status")
        ;can be either "not started" "paused" or "started"
    (defconst WHITELIST_USD_PRICE 60)
    (defconst PUBLIC_USD_PRICE 85)
    (defconst QUOTE-MSG-KEY "quote"
        @doc "Payload field for quote spec")

    (defcap GOVERNANCE ()
        (enforce-keyset ADMIN_KEYSET)
    )

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap ADMIN_OR_DISCORD (account:string)
        (compose-capability (ACCOUNT_GUARD account))
        (enforce-one "admin or discord" [(enforce (= account ADMIN_ADDRESS) "") (enforce (= account DISCORD_ADDRESS)"")])
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

    (defcap QUOTE:bool
        ( sale-id:string
          token-id:string
          amount:decimal
          price:decimal
          sale-price:decimal
          spec:object{quote-spec}
        )
        @doc "For event emission purposes"
        @event
        true
    )

    (defschema number-to-id-schema
        @doc "Stores the token id:number pairing"
        token-id:string
    )
    
    (defschema status-schema
        @doc "Stores information of the Vial NFT collection"
        staked:bool 
        for-sale:bool
        token-id:string
    )

    (defschema store-guard
        g:guard
    )

    (defschema mint-schema
        status:string
    )

    (defschema wl-tracker-schema
        @doc "stores how many whitelist mints one account has remaining"
        wl-mints-remaining:integer
    )

    (defschema token-schema
        id:string
        number:integer
        owner:string
        supply:decimal
    )

    (defschema quote-spec
        @doc "Quote data to include in payload"
        fungible:module{fungible-v2}
        price:decimal
        recipient:string
        recipient-guard:guard
    )

    (defschema quote-schema
        id:string
        spec:object{quote-spec})

    (deftable quotes:{quote-schema})
    (deftable guard-storage-table:{store-guard})
    (deftable status-table:{status-schema})
    (deftable kmc-tokens:{token-schema})
    (deftable mint-status:{mint-schema})
    (deftable number-to-id-table:{number-to-id-schema})
    (deftable wl-remaining-per-account:{wl-tracker-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        (insert counts-table NFT_COUNT {"count": 0})
        (insert counts-table MINT_COUNT {"count": 0})
        (insert mint-status MINT_STATUS {"status": "not started"})
    )

    (defun update-whitelist-multiple (number-of-mints:integer accounts:list)
        @doc "Allows the admin to update multiple whitelist amounts"
        (with-capability (ADMIN_OR_DISCORD)
        (with-capability (PRIVATE)
            (map (update-whitelist number-of-mints) accounts)
        ))
    )

    (defun get-tokens-owned:list (account:string)
        @doc "returns all tokens that an account owns"
        (select kmc-tokens ['id] (where "owner" (= account)))
    )

    (defun get-token:object (token-id:string)
        (read kmc-tokens token-id)
    )

    (defun get-token-nft-id (nft-id:string)
        @doc "returns the t:token of an nft-id `1`" 
        (at 'token-id (read number-to-id-table nft-id))
    )

    (defun get-all-for-sale:list () 
        (select status-table ['token-id] (where 'for-sale (= true)))
    )

    (defun get-all-tokens ()
        (keys kmc-tokens)
    )

    (defun get-all-owners ()
        (select kmc-tokens ["owner"] (where "owner" (!= "null")))
    )

    (defun get-status:object (token-id:string)
        (read status-table token-id)
    )

    (defun get-owner:string (token-id:string)
        (at 'owner (read kmc-tokens token-id))
    )

    (defun get-policy:object{token-schema} (token:object{token-info})
        (read kmc-tokens (at 'id token))
    )

    (defun update-for-sale:bool (token-id:string sale:bool)
        ; (enforce-kmc) TODO: reinstate
        (update status-table token-id
            { "for-sale": sale }
        )
    )

    (defun update-staked (token-id:string on-off:bool)
        (enforce-kmc)
        (update status-table token-id
            { "staked": on-off }
        )
    )

    (defun enforce-kmc ()
        (with-read guard-storage-table "kmc"  {'g:=g}
            (enforce-guard g)
        )
    )

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )
    (deftable counts-table:{counts-schema})


    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counts-table key ['count]))
    )

    (defun set-count(key:string value:integer)
        @doc "Sets the count for a key to store in the counts-table"
        (with-capability (GOVERNANCE)
            (update counts-table key 
                {"count": value} 
            )
        )
    )

    (defun enforce-crosschain:bool
        ( token:object{token-info}
          sender:string
          guard:guard
          receiver:string
          target-chain:string
          amount:decimal )
        (enforce-ledger)
        (enforce false "Transfer across chains prohibited")
    )

    (defun enforce-offer:bool
        ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
        @doc "Capture quote spec for SALE of TOKEN from message"
        (enforce-ledger)
        (enforce-sale-pact sale-id)
        (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
                (fungible:module{fungible-v2} (at 'fungible spec) )
                (price:decimal (at 'price spec))
                (recipient:string (at 'recipient spec))
                (recipient-guard:guard (at 'recipient-guard spec))
                (recipient-details:object (fungible::details recipient))
                (sale-price:decimal (* amount price)) )
          (fungible::enforce-unit sale-price)
          (enforce (< 0.0 price) "Offer price must be positive")
          (enforce (=
            (at 'guard recipient-details) recipient-guard)
            "Recipient guard does not match")
          (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
          (emit-event (QUOTE sale-id (at 'id token) amount price sale-price spec)))
          true
    )

    (defun enforce-buy:bool
        ( token:object{token-info}
          seller:string
          buyer:string
          buyer-guard:guard
          amount:decimal
          sale-id:string )
        (enforce-ledger)
        (enforce-sale-pact sale-id)
        (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
          (enforce (= qtoken (at 'id token)) "incorrect sale token")
          (bind spec
            { 'fungible := fungible:module{fungible-v2}
            , 'price := price:decimal
            , 'recipient := recipient:string
            }
            (fungible::transfer buyer recipient (* amount price))
          )
        )
        true
    )
    
    (defun enforce-sale-pact:bool (sale:string)
        "Enforces that SALE is id for currently executing pact"
        (enforce (= sale (pact-id)) "Invalid pact/sale id")
    )

    (defun enforce-transfer:bool
        ( token:object{token-info}
          sender:string
          guard:guard
          receiver:string
          amount:decimal )
        (enforce-ledger)
        (with-read status-table (at 'id token)
            { 'staked := staked }
                (update status-table (at 'id token)
                    { "for-sale" : false })
                (enforce (= staked false) "Please turn off your NFT via the Farm before trying to transfer")
        )
        (update kmc-tokens (at 'id token)
            { "owner" : receiver }
        )
        true
    )

    (defun enforce-ledger:bool ()
        (enforce-guard (marmalade.ledger.ledger-guard))
    )

    (defun enforce-mint:bool
        ( token:object{token-info}
          account:string
          guard:guard
          amount:decimal
        )
        (enforce-ledger)
        (enforce (= 1.0 amount) "Invalid mint amount")
        (with-capability (PRIVATE)
            (mint account))
        (with-read kmc-tokens (at 'id token) {
            'supply:= supply
        }
            (enforce (= supply 0.0) "Token has been minted")
        )
        (update kmc-tokens (at 'id token) 
            {"supply": 1.0
            ,"owner": account }
        )
        (let 
            (
                (number (get-count MINT_COUNT))
            )
            (enforce (<= number 10000) "Error: There are not enough NFTs remaining in the mint for your transaction")
            (update counts-table MINT_COUNT {"count": (+ 1 number)})
        )
        true
    )

      (defun enforce-burn:bool
        ( token:object{token-info}
          account:string
          amount:decimal
        )
        (enforce-ledger)
        (enforce false "Burn prohibited")
      )

    (defun register-guard (g)
        (insert guard-storage-table "kmc" {'g:g})
    )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let 
        (
            (number (get-count NFT_COUNT))
        )
        (update counts-table NFT_COUNT {"count": (+ 1 number)})
        (insert kmc-tokens (at 'id token)
          { "id": (at 'id token)
          , "number": (+ 1 number)
          , "owner": MINT_ADDRESS
          , "supply": 0.0
          })
        (insert status-table (at 'id token)
            { "staked":false
            , "for-sale":false
            , "token-id": (at 'id token) })
        (insert number-to-id-table (int-to-str 10 (+ 1 number) )
            { "token-id" : (at 'id token) })
    )
    false
  )

    (defun emit-buy:bool (nft-id:string buyer:string seller:string price:decimal policy:string)
        @doc "emits the buy event from another contract"
        (emit-event (BUY_KMC nft-id buyer seller price policy))
    )

    (defcap BUY_KMC:bool (nft-id:string buyer:string seller:string price:decimal policy:string)
        @doc "Emitted when an NFT is sold"
        @event true
    )

; ============================================
; ==              MINT FUNCTIONS            ==
; ============================================
        
    (defun set-mint-status (status:string)
        @doc "not started, paused, started"
        (with-capability (ADMIN)
            (update mint-status MINT_STATUS
                {'status : status }  
            )
        )
    )

    (defun mint (account:string)
        (require-capability (PRIVATE))
        (let*
            (
                (mint-status (enforce-mint-status false))
                (mints-remaining (get-whitelist-details account))
                (tiempo (get-time))
                (isWhitelistTime (if (and (> tiempo (time "2023-06-31T16:00:00Z")) 
                                (< tiempo (add-time (time "2023-07-01T17:01:00Z") 86400)))
                                true false))
                (price (get-current-nft-price))
            )
            (coin.transfer account ADMIN_ADDRESS price)
            (if (= true isWhitelistTime)
                (with-capability (PRIVATE)
                    (enforce (<= 1 mints-remaining) "Cannot mint more than your allocated whitelist mints.")
                    (write wl-remaining-per-account account 
                        {'wl-mints-remaining : (- mints-remaining 1)})
                )
                ""
            )
        )
    )

    (defun get-whitelist-details (account:string)
        (with-default-read wl-remaining-per-account account
            {"wl-mints-remaining" : 0}
            {"wl-mints-remaining" := mints-remaining }
            (if (< mints-remaining 0)
                0
                (if (> (get-time) (add-time (time "2023-07-01T17:01:00Z") 86400)) 0 mints-remaining)
            )
        )
    )

    (defun update-whitelist (number-of-mints:integer account:string)
        (require-capability (PRIVATE))
        (with-default-read wl-remaining-per-account account
            {"wl-mints-remaining" : -1}
            {"wl-mints-remaining" := mints-remaining }
            (if (!= -1 mints-remaining) 
                (update wl-remaining-per-account account 
                    {'wl-mints-remaining : (+ mints-remaining number-of-mints)})
                (insert wl-remaining-per-account account
                    {'wl-mints-remaining : number-of-mints})
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
    
    (defun get-current-nft-price:decimal ()
        @doc "returns the price of 1 NFT in terms of kda, \
        \ accounting for whitelist/public and current usd value of kda"
        (let*
            (
                (isWhitelistTime (if (and (> (get-time) (time "2023-06-31T16:00:00Z")) 
                                (< (get-time) (add-time (time "2023-07-01T17:01:00Z") 86400)))
                                true false))         
                (price-of-kda (kmc-oracle.get-price "kda-price-key"))
                (price (if (= true isWhitelistTime) (/ WHITELIST_USD_PRICE price-of-kda) (/ PUBLIC_USD_PRICE price-of-kda)))
            )
            (round price 0)
        )
    )

    (defun coin-account-guard (account:string)
        @doc "enforces coin account guard"
        (at "guard" (coin.details account))
    )

    (defun get-time ()
        (at "block-time" (chain-data))
    )
)


