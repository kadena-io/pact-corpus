(module kadcars-nft-policy GOVERNANCE
  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."


  (defconst ADMIN_ADDRESS "k:f157854c15e9bb8fb55aafdecc1ce27a3d60973cbe6870045f4415dc06be06f5"
    @doc "admin address which also recieves mint payouts")

  (defconst ROYALTY_ADDRESS "k:f157854c15e9bb8fb55aafdecc1ce27a3d60973cbe6870045f4415dc06be06f5"
    @doc "address for royalty payouts, which ideally will feed into splitter with business rules")
  (defconst MINT_PRICE 10.0
    @doc "base mint price")
  (defconst MINT_PRICE_WL 1.1
    @doc "base mint price")
  (defconst MINT_PRICE_FREE 0.0
    @doc "base mint price")

  (defconst PUBLIC_SALE false
    @doc "flag to indicate public sale has begun")
  (defconst ROYALTY_RATE_CONST
     0.03
    @doc "base mint price")


  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.kc-policy-admin" )))
  (defcap BUY (id:string receiver:string)
   (compose-capability (UPDATE-OWNER id receiver)))

  (defcap MINT()
    (compose-capability (UPDATE_WL))
  )

  (defcap UPDATE_WL ()
      @doc "private cap for update-whitelists"
      true)

  (defcap UPDATE-OWNER (token-id:string new-owner:string)
    true)

  (implements free.universal-token-policy-v1)
  (use free.universal-token-policy-v1 [token-info])

  (defschema account-records-schema
    account:string
    account-guard:guard
    free-mints-remaining:integer
    whitelists-remaining:integer
    minted-total:integer
  )

  (defschema collections-schema
    collection-id:string
    collection-guard:guard
    created-token-list:[string]
    minted-token-list:[string]
    max-unique-supply:integer
  )

  ;; 1. when creating add to created token list
  ;; 2.1 when minting, remove from created-list
  ;; 2.2 when minting, add to minted-token list
  ;;
  (defschema token-policy-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    mint-guard:guard
    royalty-rate:decimal
    collection-id:string
    owner:string
  )

  (defschema effecient-token-policy-schema
    @doc "encapsulate token-policy for all but id"
    token-id:string
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    mint-guard:guard
    royalty-rate:decimal
    collection-id:string
    owner:string
  )

  (deftable tokens:{effecient-token-policy-schema})
  (deftable collections:{collections-schema})
  (deftable account-records:{account-records-schema})


  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

    (defconst COLLECTION_SUPPLY "collection-unique-supply"
      @doc "Max Unique Tokens in a collection")

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

  (defun get-policy:object{token-policy-schema} (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun get-collection:object{collections-schema} (collection-id:string)
    (read collections collection-id)
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
     (enforce-guard (free.universal-ledger.ledger-guard))
   )


  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (with-capability (MINT)
    (enforce-ledger)
      (let* ( (whitelist-info (get-whitelist-info account guard))
              (mint-price (get-mint-price whitelist-info))
              (token-supply (at "supply" token))
              (token-policy-info (get-policy token))
              (collection-id (at 'collection-id token-policy-info))

          )
            (enforce (= token-supply 0.0) "Supply exceeded")
            (enforce-whitelist-info whitelist-info)
            (enforce (= 1.0 amount) "Amount of 1 only allowed for Non fungibles")
            (if (!= account ADMIN_ADDRESS)
                (if (> mint-price 0.0) (coin.transfer account ADMIN_ADDRESS mint-price) true) true
            )
            (bind (get-collection collection-id)
              {
                'created-token-list:= created-tokenList,
                'minted-token-list:= minted-tokenList
              }
              (update collections collection-id

                {'created-token-list: (filter (!= (at 'id token)) created-tokenList),
                'minted-token-list: (+ [(at 'id token)] minted-tokenList)})
            )
      )
    )
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
    (with-capability (GOVERNANCE)
    (enforce-ledger)
    (let* ( (spec:object{token-policy-schema} (read-msg TOKEN_SPEC))
            (fungible:module{fungible-v2} (at 'fungible spec))
            (creator:string (at 'creator spec))
            (creator-guard:guard (at 'creator-guard spec))
            (mint-guard:guard (at 'mint-guard spec))
            (royalty-rate:decimal (at 'royalty-rate spec))
            (collection-identifier:string (at 'collection-id spec))
            (creator-details:object (fungible::details creator ))
            )
      (enforce (= royalty-rate ROYALTY_RATE_CONST) "Invalid Royalty Rate!")
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")

      ;;TODO : COLLECTION ENFORCEMENT
      (enforce-collection collection-identifier creator-guard)
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")
      (insert tokens (at 'id token)
        {'token-id: (at 'id token)
        ,'fungible: fungible
        , 'creator: creator
        , 'owner: creator
        , 'creator-guard: creator-guard
        , 'mint-guard: mint-guard
        , 'collection-id:collection-identifier
        , 'royalty-rate: royalty-rate }
        )
        (bind (get-collection collection-identifier)
          {'created-token-list:=tokenList}
          (update collections collection-identifier
            {'created-token-list: (+ [(at 'id token)] tokenList)}
          )
        )
      )


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
    (enforce false "Transfer prohibited")
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
            (royalty-payout:decimal
               (floor (* sale-price royalty-rate) (fungible::precision))) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout creator spec)))
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
    (with-capability (BUY (at 'id token) buyer)
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
                   (royalty-payout:decimal
                      (floor (* sale-price royalty-rate) (fungible::precision)))
                   (payout:decimal (- sale-price royalty-payout)) )
              (if
                (> royalty-payout 0.0)
                (fungible::transfer buyer ROYALTY_ADDRESS royalty-payout)
                "No royalty")
              (fungible::transfer buyer recipient payout)))
              true
              (update-owner qtoken buyer)
        )))
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
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun update-owner (token-id:string new-owner:string)
      (require-capability (UPDATE-OWNER token-id new-owner))
        (update tokens token-id
          {'owner: new-owner}
        )
  )

  (defun create-collection ()
    (with-capability (GOVERNANCE)
    (let* (
            (creator-guard:guard (read-msg "creator-guard"))
            (collection-identifier:string (read-msg 'collection-id))
            (collection-max-unique-supply:integer (read-integer COLLECTION_SUPPLY))
            )
            (enforce-guard creator-guard)
            ;;TODO : Move collection-id to pull from make of manifest
          (insert collections collection-identifier
              {'collection-id:collection-identifier
              ,'collection-guard:creator-guard
              ,'created-token-list:[]
              ,'minted-token-list:[]
              ,'max-unique-supply:collection-max-unique-supply})
          )
          true
    )
  )

    (defun enforce-collection (collection-id:string guard:guard)

      (bind (get-collection collection-id)
        {'collection-guard:=collection-guard}
        (enforce-guard collection-guard)
      )
    )

    ;;;;;;;;;;;;;; WL Functionality ;;;;;;;;;;;;;;
    (defun get-account-records-info:object{account-records-schema} (account:string)
      (read account-records account)
    )

    (defun add-whitelist:bool (account:string free-mints-remaining:integer whitelists-remaining:integer )
       (with-capability (GOVERNANCE)
         (insert account-records account{
           "account": account,
           "account-guard": (at 'guard (coin.details account)),
           "free-mints-remaining": free-mints-remaining,
           "whitelists-remaining": whitelists-remaining,
           "minted-total":0
         })
         true
       )
    )

    (defun get-whitelist-info:string (account:string guard:guard)
       (let ( (accounts:[string] (keys account-records))
            )
         (if (= false PUBLIC_SALE) (enforce (contains account accounts) "You are not whitelisted") (enforce (= true true)))
         (require-capability (UPDATE_WL))

           (with-read account-records account{
              'account-guard:= g,
              "free-mints-remaining":= free-mints-remaining,
              "whitelists-remaining":= whitelists-remaining,
              'minted-total:= minted-total
             }
             (enforce (= g guard) "Guards doesn't match.")

             (if (> free-mints-remaining 0) (update-free-mints-table account (- free-mints-remaining 1))
                (if (> whitelists-remaining 0) (update-whitelists-table account (- whitelists-remaining 1)) "account not whitelisted, wait for public sale!"))
           )
      )
    )


    (defun update-free-mints-table (account:string new-free-mints:integer)
      (require-capability (UPDATE_WL))
        (update account-records account {
          "free-mints-remaining": new-free-mints
        })
        "free mint granted"
    )

    (defun update-whitelists-table (account:string new-whitelists:integer)
      (require-capability (UPDATE_WL))
        (update account-records account {
          "whitelists-remaining": new-whitelists
        })
        "whitelist granted"
    )

    (defun enforce-whitelist-info (whitelist-info:string)

      (let*
          ((is-allowed:bool (if (= whitelist-info "free mint granted") true
                              (if (= PUBLIC_SALE true) true
                                (if (= whitelist-info "whitelist granted") true false))))

            )
            (enforce (= true is-allowed) "Not allowed to mint!")

        )

    )

    (defun get-mint-price:decimal (whitelist-info:string)
      (if (= whitelist-info "free mint granted") MINT_PRICE_FREE
        (if (= PUBLIC_SALE true) MINT_PRICE
           (if (= whitelist-info "whitelist granted") MINT_PRICE_WL 100000)))
    )


    ;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;

    (defun get-non-minted-tokens-for-collection (collection-id:string)

       (at "created-token-list" (read collections collection-id ["created-token-list"]))
    )

    (defun get-minted-tokens-for-collection (collection-id:string)

       (at "minted-token-list" (read collections collection-id ["minted-token-list"]))
    )

    (defun get-cars-in-collection-by-owner (collection-id:string owner:string)
      @doc "All cars under collections and owner"
      (select tokens ["token-id"](where 'owner (= owner)))
    )

)




