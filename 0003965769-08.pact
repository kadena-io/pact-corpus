(module kmc-policy GOVERNANCE

  @doc "Kadena Mining Club Gen 1 Miner policy"
  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])
  (use coin)
    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst MINT_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")
    (defconst MINT_PAUSED:string "mint-paused")
    (defconst MINT_STARTED:string "mint-started")
    (defconst MINT_COMPLETED:string "mint-completed")

  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN_KEYSET))

    (defschema royalty-details
        royalty-receiever:string
        royalty-rate:decimal
    )

   (defschema mint-schema
     tokens-list:[string]
     current-length:integer
     status:string
   )

    (defschema collection-schema
        id:string
        royalty-info:[object{royalty-details}] ;accounts which receive the royalty
        collection-size:integer ;total supply of tokens that will ever exist
        provenance-hash:string ;sha256 of combined string
        tokens:[string] ;list of sha256 of the images that will ever exist
        creator-account:string
        creator-guard:guard
        price:decimal
        fungible:module{fungible-v2}
    )

  (defschema token-schema
    id:string
    owner:string
    supply:decimal
  )

  (deftable tokens:{token-schema})
  (deftable mint-status:{mint-schema})
  (deftable collection-info:{collection-schema})

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
    (with-read tokens (at 'id token) {
        'supply:= supply
    }
        (enforce (= supply 0.0) "Token has been minted")
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

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    true
  )

  (defun get-all-tokens ()
    (keys tokens)
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
    (with-read tokens (at 'id token)
        { "owner" := owner }
            (enforce (= sender owner) "you are not the owner of this NFT")
    )
    (update tokens (at 'id token)
        { "owner" : receiver }
    )
    (emit-event (TEST))
    true
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

  (defun get-owner:string (token-id:string)
    (at 'owner (read tokens token-id))
  )

  (defun get-collection:object{collection-schema} (collection-id:string)
    (read collection-info collection-id)
  )

  (defun get-tokens-owned:[string] (account:string)
    @doc "returns all tokens that an account owns"
    (select tokens ['id] (where "owner" (= account)))
  )

  (defun get-token (token-id:string)
    (read tokens token-id)
  )

  (defun get-details:object{collection-schema} (collection:string)
    @doc "Returns the details of the collection"
    (read collection-info collection)
  )

  (defun get-policy:object{token-schema} (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun initialize-collection (
    new-collection:object
    tokens-list:list
    fungible:module{fungible-v2}
    )
    (with-capability (GOVERNANCE)
    (write collection-info (at 'id new-collection) {
      "royalty-info": (at 'royalty-info new-collection),
      "collection-size": (at 'collection-size new-collection),
      "provenance-hash": (at 'provenance-hash new-collection),
      "tokens": tokens-list,
      "creator-account": (at 'creator-account new-collection),
      "creator-guard": (at 'creator-guard new-collection),
      "price": (at 'price new-collection),
      "fungible": fungible
    })
    (write mint-status (at 'id new-collection) {
      "current-length": (length tokens-list),
      "tokens-list": tokens-list,
      "status": MINT_STARTED
    }))
  )

    (defun initialize-tokens-list (new-token:string collection:string)
        @doc "Allows for initializing lists larger than 1,000 tokens"
        (with-read collection-info collection
            { "collection-size" := total-supply 
            , "tokens" := collection-tokens-list }
            ; (with-read mint-status collection
            ;     { "current-length" := old-length
            ;     , "tokens-list" := old-tokens-list
            ;     } 
            ;     (enforce (<= (+ 1 old-length) total-supply ) "The additional tokens exceed the size of the total supply")
            ;     (update mint-status collection
            ;         { "tokens-list": (+ old-tokens-list [new-token])
            ;         , "current-length": (+ (length new-token) old-length)
            ;         }
            ;     )
                
            (update collection-info collection
                { "tokens": (+ collection-tokens-list [new-token]) }
            )
            (insert tokens new-token
              { "id": new-token
              , "owner": MINT_ADDRESS
              , "supply": 0.0
              })
        )
    )

    (defun fix-tokens (token-list:[string])
      (map (insert-token) token-list)
    )

    (defun insert-token (token:string)
        (insert tokens token
          { "id": token
          , "owner": MINT_ADDRESS
          , "supply": 0.0
          })
    )

    (defcap TEST ()
     @event
     true)

)

; (create-table tokens)
; (create-table mint-status)
; (create-table collection-info)