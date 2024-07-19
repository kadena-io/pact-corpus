(module prod-nft-sale-royality-policy GOVERNANCE

  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "hypercent.hyper-admin-1-prod")))

  (defcap IS_ADMIN ()
      (enforce-keyset "hypercent.hyper-api-admin-prod"))

  (defcap HYPERCENT_SALE ()
    true)

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])
  
  (defschema traits-schema
    trait-type:string
    value:string
  )

  (defschema holder-schema
    id:string
    account:string  
  )

  (defschema nft-metadata
    name:string
    description:string
    image:string
    image-hash:string
    attributes:[object{traits-schema}]
  )

  (defschema mint-schema
    account:string
    collection:string
    tokens:[string]  
  )

  (defschema policy-schema
    id:string
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    mint-guard:guard
    max-supply:decimal
    min-amount:decimal
    royalty-rate:decimal
    collection:string
  )


  (deftable policies:{policy-schema})

  (deftable holders:{holder-schema})
  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (deftable quotes:{quote-schema})

  (deftable mint:{mint-schema})

  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      royalty-payout:decimal
      creator:string
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
   )

  (defun can-mint (account:string collection:string id:string)
      (contains id (at "tokens" (read mint (+ account collection))))
  ) 

  (defun get-token-collection (token:object{token-info})
    (at "collection" (at "datum" (at 0 (at "data" (at "manifest" token)))))
  )
  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (let (
      (distribution-contract:module{callable-v3} (hypercent.prod-callable-contracts.get "hype-nft-distribution")))
        (enforce-guard (distribution-contract::call "GET_GUARD"))
    )
    (bind (get-policy token)
      { 'mint-guard:=mint-guard:guard
      , 'min-amount:=min-amount:decimal
      , 'max-supply:=max-supply:decimal
      }
        (enforce (>= amount min-amount) "mint amount < min-amount")
        (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
        (write holders (at "id" token) {"account": account, "id": (at "id" token)})
  ))

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (let (
      (distribution-contract:module{callable-v3} (hypercent.prod-callable-contracts.get "hype-nft-distribution")))
        (enforce-guard (distribution-contract::call "GET_GUARD" []))
    )
    (write holders (at "id" token) {"account": "", "id": (at "id" token)})
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (with-capability (IS_ADMIN)
      (enforce-ledger)
        (let* ( (spec:object{policy-schema} (read-msg TOKEN_SPEC))
                (fungible:module{fungible-v2} (at 'fungible spec))
                (creator:string (at 'creator spec))
                (creator-guard:guard (at 'creator-guard spec))
                (mint-guard:guard (at 'mint-guard spec))
                (max-supply:decimal (at 'max-supply spec))
                (min-amount:decimal (at 'min-amount spec))
                (royalty-rate:decimal (at 'royalty-rate spec))
                (creator-details:object (fungible::details creator ))
                )
          (enforce (>= min-amount 0.0) "Invalid min-amount")
          (enforce (>= max-supply 0.0) "Invalid max-supply")
          (enforce (=
            (at 'guard creator-details) creator-guard)
            "Creator guard does not match")
          (enforce (and
            (>= royalty-rate 0.0) (<= royalty-rate 10.0))
            "Invalid royalty rate")
          (insert policies (at 'id token)
            { 'fungible: fungible
            , 'creator: creator
            , 'id: (at 'id token)
            , 'collection: (get-token-collection token)
            , 'creator-guard: creator-guard
            , 'mint-guard: mint-guard
            , 'max-supply: max-supply
            , 'min-amount: min-amount
            , 'royalty-rate: royalty-rate }))
      true
    )
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
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
       ,'royalty-rate:= royalty-rate:decimal
       ,'creator:= creator:string
      }
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price))
            (royalty-payout:decimal (floor (* sale-price royalty-rate) (fungible::precision))) 
            (platform-comission:decimal (if (try false (read-msg "HYPERCENT_SALE")) (floor (* sale-price 0.9) (fungible::precision)) 0.0))
        )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")

      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price (+ royalty-payout platform-comission) creator spec)))
      true
  )
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
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'royalty-rate:= royalty-rate:decimal
      }
      (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          }
          (let* ((sale-price:decimal (* amount price))
                 (royalty-payout:decimal (floor (* sale-price royalty-rate) (fungible::precision)))
                 (platform-comission:decimal (if (try false (read-msg "HYPERCENT_SALE")) (floor (* sale-price 0.9) (fungible::precision)) 0.0))
                 (payout:decimal (- sale-price (+ platform-comission royalty-payout))) )
            (if
              (> royalty-payout 0.0)
              (fungible::transfer buyer creator royalty-payout)
              "No royalty")
            (if
                (> platform-comission 0.0)
                ; todo: transfer to marketplace, not creator here
                (fungible::transfer buyer creator royalty-payout)
                "No comission")  
            (fungible::transfer buyer recipient payout)))
            (write holders (at "id" token) {"account": buyer, "id": (at "id" token)})
            true
        ))
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
    ; (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    ; (enforce false "Transfer prohibited")
  )
  
  (defun get-collection-tokens (collection:string)
      (select policies ['id] (where 'collection (= collection)))
  )

  (defun get-account-tokens (account:string)
      (select holders ['id] (where 'account (= account)))
  )

  (defun allow-mint (account:string collection:string tokens:[string])
    (let (
      (distribution-contract:module{callable-v3} (hypercent.prod-callable-contracts.get "hype-nft-distribution")))
        (enforce-guard (distribution-contract::call "GET_GUARD"))
    )
    (write mint (+ account collection) {"tokens": tokens, "account":account, "collection": collection})
  )

  (defun get-account-minted-nfts (account:string collection:string)
      (at "tokens" (read mint (+ account collection)))
  )

  
)



