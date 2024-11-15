(module kmc-your-miner GOVERNANCE
  @doc "Kadena Mining Club NFT Your Miner policy. NFTs representing physical ASIC miners"
    (implements kip.token-policy-v1)
    (implements free.kmc-token-policy-v13)
    (use kip.token-policy-v1 [token-info])
    (use marmalade.ledger)

    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst MINT_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")
    (defconst DISCORD_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")
    (defconst NFT_COUNT:string "nft-count")
    (defconst MINT_COUNT:string "mint-count")
    (defconst ASIC_LIST:string "asic-list")
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
        (enforce-one "admin or discord" [(enforce (= account ADMIN_ADDRESS) "") (enforce (= account MINT_ADDRESS)"")])
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
    
    (defschema status-schema
        @doc "Stores information of the Vial NFT collection"
        for-sale:bool
        token-id:string
    )

    (defschema store-guard
        g:guard
    )

    (defschema mint-schema
        status:string
    )

    (defschema project-schema
        @doc "schema for storing information about project-branded ASICs"
        ;keys are the project names
        project-name:string
        asics-remaining:integer ;remaining for sale
        payout-address:string
        associated-asics:list
        num-sold:integer
		active-promotion:bool
    )

    (deftable projects-table:{project-schema})

    (defun get-all-projects-with-stock ()
    	@doc "Returns all the projects with remaining stock, for display on the marketplace, also returns all information pertaining to those projects"
    	(select projects-table ["project-name"] (where "asics-remaining" (> 0)))
    )

    (defun get-all-project-details ()
        (map (get-project-details) (get-all-projects-with-stock))
    )

    (defun get-project-details (project:string)
    	@doc "returns all the details of a project, usually used in conjunction with get-all-projects-with-stock"
    	(read projects-table project)
    )

    (defun add-new-project (project:string asics-remaining:integer payout-address:string associated-asics:list)
        @doc "populates the project table with all information for a new project ASICs on the marketplace"
	    (enforce-kmc)
        (insert projects-table project 
            { "project-name": project
            , "asics-remaining": asics-remaining
            , "payout-address": payout-address
            , "associated-asics": associated-asics
            , "active-promotion": true
            })
    )

    (defun add-new-asics-to-project (project:string associated-asics:list)
    	@doc "Append new ASICs if a project wants to re-up on their ASICs for sale"
    	(enforce-kmc)
    	(with-read projects-table project
    		{ "associated-asics" := previously-associated-asics }
    		(update projects-table project
    			{ "asics-remaining": (+ (length previously-associated-asics) (length associated-asics))
    			, "associated-asics": (+ previously-associated-asics associated-asics) }))
    )

    (defun update-project-details (project:string field:string item) ;item intentionally has no type so it can be anything
    	@doc "allows the admin to update project details such as payout address"
    	(with-capability (GOVERNANCE)
    		(update projects-table project 
    			{ 'field : item })
    	)
    )

    (defschema token-schema
        @doc "Key is the t:token"
        id:string ; t:token
        asic-coin:string ;btc, ltc, doge, kda, etc.
        asic-manufacturer:string ;bitmain, goldshell, etc.
        asic-model:string ;L7, KA3, S19 Pro, etc.
        asic-number:integer ; counts up from 1, all asics share the same counter
        asic-details:object ;{ "hashrate": "166 TH/s", "wattage": "4000W", "affiliation": "project-xxxx"} ;affiliation is just "kmc" if no affiliation
        electric-expiry:time ; 11:59:00 UTC at the end of their 6month/1year
        electric-rate:decimal
        contract-start:time
        watcher-link:string ;pool information
        first-owner:string ;original owner, for royalty purposes
        owner:string
        supply:decimal ;1.0 if minted
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

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )

    (deftable counts-table:{counts-schema})
    (deftable quotes:{quote-schema})
    (deftable guard-storage-table:{store-guard})
    (deftable status-table:{status-schema})
    (deftable kmc-tokens:{token-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        (insert counts-table NFT_COUNT {"count": 0})
        (insert counts-table MINT_COUNT {"count": 0})
    )

    (defun get-tokens-owned:list (account:string)
        @doc "returns all tokens that an account owns"
        (select kmc-tokens ['id] (and? (where 'owner (= account))
              (where 'supply (= 1.0))))
    )

    (defun get-token:object (token-id:string)
        (read kmc-tokens token-id)
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
        (enforce-kmc)
        (update status-table token-id
            { "for-sale": sale }
        )
    )

    (defun extend-electric-contract (extend-by:integer token-id:string)
        @doc "Adds time to the current electric contract, extend-by is number of days"
        (with-capability (ACCOUNT_GUARD MINT_ADDRESS)
        (with-read kmc-tokens token-id
            { "electric-expiry" := old-expiry }
            (update kmc-tokens token-id
                { "electric-expiry": (add-time (time old-expiry) (days extend-by)) })
        ))
    )

    (defun update-watcher-link (token-id:string watcher-link:string)
        (with-capability (ACCOUNT_GUARD MINT_ADDRESS)
            (update kmc-tokens token-id 
                { "watcher-link": watcher-link })
        )
    )

    (defun update-asic-details (token-id:string details:object)
        (with-capability (ADMIN)
            (update kmc-tokens token-id
                { "asic-details": details })
        )
    )

    (defun get-first-owner (token-id:string)
        (at 'first-owner (read kmc-tokens token-id))
    )

    (defun enforce-kmc ()
        (with-read guard-storage-table "kmc"  {'g:=g}
            (enforce-guard g)
        )
    )

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
        (with-read kmc-tokens (at 'id token)
            {"first-owner":= first-owner }
            (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
              (enforce (= qtoken (at 'id token)) "incorrect sale token")
              (bind spec
                { 'fungible := fungible:module{fungible-v2}
                , 'price := price:decimal
                , 'recipient := recipient:string
                }
                (fungible::transfer buyer recipient (* (* amount 0.94) price))
                (fungible::transfer buyer first-owner (* (* amount 0.01) price))
                (fungible::transfer buyer ADMIN_ADDRESS (* (* amount 0.05) price))
              )
            )
        )
        (update kmc-tokens (at 'id token)
            { "owner" : buyer }
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
        (update status-table (at 'id token)
            { "for-sale" : false })
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
        (enforce (= account MINT_ADDRESS) "Only admin can mint these NFTs")
        (enforce (= 1.0 amount) "Invalid mint amount")
        (with-read kmc-tokens (at 'id token) {
            'supply:= supply
        }
            (enforce (= supply 0.0) "Token has been minted")
        )
        (update kmc-tokens (at 'id token) 
            { "supply": 1.0
            , "owner": account }
        )
        (let 
            (
                (number (get-count MINT_COUNT))
            )
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
        (enforce (= 1.0 amount) "Invalid burn amount")
        (with-read kmc-tokens (at 'id token) 
            { 'supply:= supply }
            (enforce (= supply 1.0) "Token has been burned already")
        )
        (update kmc-tokens (at 'id token) 
            { "supply": 0.0
             ,"owner":"null"}
        )
        true
    )

    (defun register-guard (g)
        (insert guard-storage-table "kmc" {'g:g})
    )

    (defun enforce-init:bool
        ( token:object{token-info})
        ; (with-capability (ACCOUNT_GUARD MINT_ADDRESS)
            (enforce-ledger)
            (let*
                (
                    (asic-coin (read-msg "asicCoin"))
                    (asic-model (read-msg "asicModel"))
                    (asic-manufacturer (read-msg "asicManufacturer"))
                    (electric-expiry (time (read-msg "expiry")))
                    (electric-rate (read-msg "electricRate"))
                    (contract-start (time (read-msg "contractStart")))
                    (asic-details (read-msg "asicDetails"))
                    (watcher-link (read-msg "watcherLink"))
                    (first-owner (read-msg "firstOwner"))
                    (number (+ 1 (get-count NFT_COUNT)))
                )
                ""
                (update counts-table NFT_COUNT {"count": number})
                (insert kmc-tokens (at 'id token)
                    { "id": (at 'id token)
                    , "asic-coin": asic-coin
                    , "asic-manufacturer": asic-manufacturer
                    , "asic-model": asic-model
                    , "asic-number": number
                    , "asic-details": asic-details
                    , "electric-expiry": electric-expiry
                    , "electric-rate" : electric-rate
                    , "contract-start" : contract-start
                    , "watcher-link": watcher-link
                    , "first-owner": first-owner
                    , "owner": "null"
                    , "supply": 0.0
                    })
                (insert status-table (at 'id token)
                    { "for-sale": false
                    , "token-id": (at 'id token) })
            )
        ; )
        true
    )

    (defun emit-buy:bool (nft-id:string buyer:string seller:string price:decimal policy:string)
        @doc "emits the buy event from another contract"
        (enforce-kmc)
        (emit-event (BUY_KMC nft-id buyer seller price policy))
    )

    (defcap BUY_KMC:bool (nft-id:string buyer:string seller:string price:decimal policy:string)
        @doc "Emitted when an NFT is sold"
        @event true
    )
)

