(module kmc-test-policy GOVERNANCE

  @doc "Kadena Mining Club Immersion Vial policy"
  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])
  (use coin)

    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst MINT_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")
    (defconst MINT_PAUSED:string "mint-paused")
    (defconst MINT_STARTED:string "mint-started")
    (defconst MINT_COMPLETED:string "mint-completed")
    (defconst NFT_COUNT:string "nft-count")
    
    (defcap GOVERNANCE ()
        (enforce-keyset ADMIN_KEYSET)
    )

    (defschema royalty-details
        royalty-receiever:string
        royalty-rate:decimal
    )

    (defschema vial-status-schema
        @doc "Stores information of the Vial NFT collection"
        staked:bool 
        for-sale:bool
        token-id:string
    )

    (defschema store-guard
        g:guard
    )

    (defschema mint-schema
         tokens-list:[string]
         current-length:integer
         status:string
    )

    (defschema token-schema
        id:string
        marmalade-id:string
        owner:string
        supply:decimal
    )

    (deftable guard-storage-table:{store-guard})
    (deftable vial-status-table:{vial-status-schema})
    (deftable kmc-tokens:{token-schema})
    (deftable mint-status:{mint-schema})


    (defun get-tokens-owned (account:string)
        @doc "returns all tokens that an account owns"
        (select kmc-tokens ['id] (where "owner" (= account)))
    )

    (defun get-token (token-id:string)
        (read kmc-tokens token-id)
    )

    (defun get-token-nft-id (nft-id:string)
        @doc "returns the t:token of an nft-id `1`" 
        (select kmc-tokens ['id, 'number] (where "number" (= (str-to-int nft-id))))
    )

    (defun get-all-for-sale () ; key "t:xxx"
        (select vial-status-table ['token-id] (where 'for-sale (= true)))
    )

    (defun get-all-tokens ()
        (keys kmc-tokens)
    )

    (defun get-all-owners ()
        (select kmc-tokens ["owner"] (where "owner" (!= "null")))
    )

    (defun get-vial-status (token-id:string)
        (read vial-status-table token-id)
    )

    (defun get-owner:string (token-id:string)
        (at 'owner (read kmc-tokens token-id))
    )

    (defun get-policy:object{token-schema} (token:object{token-info})
        (read kmc-tokens (at 'id token))
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
        (with-read vial-status-table (at 'id token)
            { 'staked := staked }
                (update vial-status-table (at 'id token)
                    { "for-sale" : false })
                (enforce (= staked false) "Please turn off your NFT via the Farm before trying to transfer")
        )
        (update kmc-tokens (at 'id token)
            { "owner" : receiver }
        )
        true
    )

    (defun update-for-sale (token-id:string sale:bool)
        (enforce-kmc)
        (update vial-status-table token-id
            { "for-sale": sale }
        )
    )

    (defun update-staked (token-id:string on-off:bool)
        (enforce-kmc)
        (update vial-status-table token-id
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

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        (insert counts-table NFT_COUNT {"count": 0})
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

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

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
    (with-read kmc-tokens (at 'id token) {
        'supply:= supply
    }
        (enforce (= supply 0.0) "Token has been minted")
    )
    (update kmc-tokens (at 'id token) 
        {"supply": 1.0}
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
    (write guard-storage-table "kmc" {'g:g})
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let 
        (
            (id (get-count NFT_COUNT))
        )
        (insert kmc-tokens (int-to-str 10 id)
          { "marmalade-id": (at 'id token)
          , "id": (int-to-str 10 id)
          , "owner": MINT_ADDRESS
          , "supply": 0.0
          })
        
        (update counts-table NFT_COUNT {"count": (+ 1 id)})
    )
    true
  )
)
; (create-table kmc-tokens)
; (create-table counts-table)
; (initialize)
