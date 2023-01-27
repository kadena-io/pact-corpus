(module mod-fixed-quote-policy GOVERNANCE

  @doc "Policy for fixed issuance with simple quoted sale."
  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst MINT_STATUS:string "mint-status")
    (defconst COLLECTION_INFO:string "collection-info")
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
     tokens-list:[integer]
     current-length:integer
     status:string
   )

  (defschema collection-schema
    royalty-info:[object{royalty-details}] ;account which receives the royalty
    total-supply:integer ;total supply of tokens that will ever exist
    provenance-hash:string ;sha256 of combined string
    tokens-list:list ;list of sha256 of the images that will ever exist
    creator:string
    max-per-user:integer ;maximum NFT a user can mint
    max-per-wl:integer ;maximum NFT a whitelisted user can mint
    max-per-txn:integer
    price-per-nft:decimal
    whitelist-price:decimal
    creator-guard:guard
    public-mint-time:time
    whitelist-mint-time:time
    mint-end-time:time
    name:string
    symbol:string
    fungible:module{fungible-v2}
  )

  (defschema policy-schema
    mint-guard:guard
    max-supply:decimal
    min-amount:decimal
  )

  (deftable policies:{policy-schema})
  (deftable mint-status:{mint-schema})
  (deftable collection-info2:{collection-schema})

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

  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
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
    (bind (get-policy token)
      { 'mint-guard:=mint-guard:guard
      , 'min-amount:=min-amount:decimal
      , 'max-supply:=max-supply:decimal
      }
      (enforce-guard mint-guard)
      (enforce (>= amount min-amount) "mint amount < min-amount")
      (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
  ))

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
    (let* ( (mint-guard:guard (read-keyset 'mint-guard ))
            (max-supply:decimal (read-decimal 'max-supply ))
            (min-amount:decimal (read-decimal 'min-amount ))
            )
    (enforce (>= min-amount 0.0) "Invalid min-amount")
    (enforce (>= max-supply 0.0) "Invalid max-supply")
    (insert policies (at 'id token)
      { 'mint-guard: mint-guard
      , 'max-supply: max-supply
      , 'min-amount: min-amount })
    true)
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
    (enforce true "Transfer prohibited")
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

  (defun initialize-collection (
    new-collection:object
    tokens-list:list
    fungible:module{fungible-v2}

    )

    (write collection-info2 (at 'name new-collection) {
      "provenance-hash": (at 'provenance new-collection),
      "total-supply": (at 'total-supply new-collection),
      "creator": (at 'creator new-collection),
      "creator-guard": (at 'creator-guard new-collection),
      "max-per-user": (at 'max-per-user new-collection),
      "max-per-txn": (at 'max-per-txn new-collection),
      "max-per-wl": (at 'max-per-wl new-collection),
      "public-mint-time": (at 'public-mint-time new-collection),
      "whitelist-mint-time": (at 'whitelist-mint-time new-collection),
      "mint-end-time": (at 'mint-end-time new-collection),
      "royalty-info": (at 'royalty-info new-collection),
      "price-per-nft": (at 'price-per-nft new-collection),
      "whitelist-price": (at 'whitelist-price new-collection),
      "name": (at 'name new-collection),
      "symbol": (at 'symbol new-collection),
      "fungible": fungible,
      "tokens-list": tokens-list
    })
    (write mint-status MINT_STATUS {
      "current-length": (length tokens-list),
      "tokens-list": (map (str-to-int 64) tokens-list),
      "status": MINT_STARTED
    })
  )

    (defun initialize-tokens-list (new-tokens-list:[string] collection:string)
        @doc "Allows for initializing lists larger than 1,000 tokens"
        (with-read collection-info2 collection
            { "total-supply" := total-supply }
            (with-read mint-status MINT_STATUS
                { "current-length" := old-length
                , "tokens-list" := old-tokens-list
                } 
                (enforce (<= (+ (length new-tokens-list) old-length) total-supply ) "The additional tokens exceed the size of the total supply")
                (format "old list {} and new list {}" [old-tokens-list new-tokens-list])
                (update mint-status MINT_STATUS 
                    { "tokens-list": (+ old-tokens-list new-tokens-list)
                    , "current-length": (+ (length new-tokens-list) old-length)
                    }
                )
                (format "Old length = {}, new additions = {}" [old-length (length new-tokens-list)]) 
            )
        )
    )

    (defun initialize-tokens-list-check (new-tokens-list:[string] collection:string)
        @doc "Allows for initializing lists larger than 1,000 tokens"
        (with-read collection-info2 collection
            { "total-supply" := total-supply }
            (with-read mint-status MINT_STATUS
                { "current-length" := old-length
                , "tokens-list" := old-tokens-list
                } 
                (enforce (<= (+ (length new-tokens-list) old-length) total-supply ) "The additional tokens exceed the size of the total supply")
                (format "old list {} and new list {}" [old-tokens-list new-tokens-list])
                ; (update mint-status MINT_STATUS 
                ;     { "tokens-list": (+ old-tokens-list new-tokens-list)
                ;     , "current-length": (+ (length new-tokens-list) old-length)
                ;     }
                ; )
                ; (format "Old length = {}, new additions = {}" [old-length (length new-tokens-list)]) 
            )
        )
    )

)

; (create-table policies)
; (create-table mint-status)
; (create-table collection-info2)
