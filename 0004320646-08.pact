(module kryptomerchpolicy-contract MERCH

  (defcap MERCH ()
    (enforce-guard (keyset-ref-guard "free.kryptomerchpolicy-contract-ks")))

    (defcap IS_ADMIN ()
     (compose-capability (MERCH))
    )
    (implements kip.token-policy-v1)
    (use kip.token-policy-v1 [token-info])
    (use util.guards)

    (defcap MINT (account:string)
       (compose-capability (PRIVATE))
     )

     (defcap PRIVATE ()
       true
     )

    (defschema collection-schema
      total-supply:integer ;total supply of tokens that will ever exist
      creator:string
      max-per-txn:integer
      price-per-nft:decimal
      creator-guard:guard
      name:string
      imageUrl:string
    )

    (defschema policy-schema
      id:string
      creator:string
      creator-guard:guard
      owner:string
    )

    (defschema account-schema
      account:string
      minted:integer
    )

    (defschema traits-schema
      trait-type:string
      value:string
    )

    (defschema token-metadata
      name:string
      description:string
      image:string
      image-hash:string
      attributes:[object{traits-schema}]
    )

    (deftable policies:{policy-schema})
    (deftable collection-info:{collection-schema})

    (deftable account-details:{account-schema})
    (deftable quotes:{quote-schema})

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

    (defcap QUOTE:bool
      ( sale-id:string
        token-id:string
        amount:decimal
        price:decimal
        sale-price:decimal
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

    (defschema mf-uri
      scheme:string
      data:string
    )

    (defschema mf-datum
      uri:object{mf-uri}
      hash:string
      datum:object
    )

    (defun enforce-mint:bool
      ( token:object{token-info}
        account:string
        guard:guard
        amount:decimal
      )
       (enforce-ledger)

       (bind token
         {
          "id":=token-id
          }
       (let* ((data (at 'data (marmalade.ledger.get-manifest token-id)))
              (datum (at 'datum (at 0 data)))
              (name:string (at 'collectionName datum))
              (minted:integer (get-account-minted account))
         )

       (with-capability (MINT account)
          (with-read collection-info name
            {
              "creator":=creator,
              "price-per-nft":=price
            }
          (if (= account creator)
            [(format "your tokenId is {}" [token-id]) ]
            (coin.transfer account creator price)
          )
          (update-status
           token-id
           account
           name
           minted
           )
         )
        )
         )
         )
       )

    (defun update-status
      ( token-id:string
        account:string
        name:string
        minted:integer
      )
      (require-capability (PRIVATE))

      (bind (get-details name) {
        'name:= name,
        'creator:= creator,
        'creator-guard:= creator-guard
        }

          (insert policies token-id {
            "id": token-id,
            "owner": account,
            "creator-guard": creator-guard,
            "creator": creator
          })
        (write account-details account {
          'account: account,
          'minted: (+ minted 1)
          })
        token-id
      )
    )

    (defun update-mint-price:string (price:decimal collectionName:string)
      @doc   "Update mint price"
      (enforce (< 0.0 price) "price is not a positive number")
          (with-read collection-info collectionName
          {
            "creator":=creator
          }
          (enforce-guard (at "guard" (coin.details creator)))

          (update collection-info collectionName
            {
              "price-per-nft":price
              }
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
      ( token:object{token-info} )
      (enforce-ledger)
      true
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
         ,'creator:= creator:string
        }
      (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
              (price:decimal (at 'price spec))
              (recipient:string (at 'recipient spec))
              (recipient-guard:guard (at 'recipient-guard spec))
              (recipient-details:object (fungible::details recipient))
              (sale-price:decimal (* amount price)))
        (fungible::enforce-unit sale-price)
        (enforce (< 0.0 price) "Offer price must be positive")
        (enforce (=
          (at 'guard recipient-details) recipient-guard)
          "Recipient guard does not match")
        (insert quotes sale-id { 'id: (at 'id token), 'spec: spec }))
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
        }
        (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
          (enforce (= qtoken (at 'id token)) "incorrect sale token")
          (bind spec
            { 'price := price:decimal
            , 'recipient := recipient:string
            }
            (let* ((sale-price:decimal (* amount price)))
              (fungible::transfer buyer recipient sale-price))
              (update policies qtoken {
                "owner": buyer
                }))
              true
        )
      )
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
      (with-read policies (at 'id token)
        { "owner" := owner }
            (enforce (= sender owner) "you are not the owner of this NFT")
      )
      (update policies (at 'id token)
        { "owner" : receiver }
      )
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
      (enforce false "Transfer prohibited")
    )

    (defun create-nft-collection
      (
        creator:string
        creator-guard:guard
        total-supply:integer
        max-per-txn:integer
        price-per-nft:decimal
        name:string
        imageUrl:string
        )

        (with-capability (IS_ADMIN)
        (insert collection-info name {
          "max-per-txn": max-per-txn,
          "total-supply": total-supply,
          "creator": creator,
          "creator-guard": creator-guard,
          "price-per-nft": price-per-nft,
          "name": name,
          "imageUrl":imageUrl
        })

      )
     )

    ;;
    ;; helpers
    ;;

    (defun get-policy:object{policy-schema} (token:object{token-info})
      (read policies (at 'id token))
    )

    (defun get-account-minted:integer (account:string)
      (with-default-read account-details account
        {"minted": 0}
        {"minted":= minted}
      minted
      )
    )

    (defun get-account-info:object{account-schema} (account:string)
      (read account-details account)
    )

    (defun get-details:object{collection-schema} (collectionName:string)
     (read collection-info collectionName)
    )

    (defun get-total-supply:string (collectionName:string)
     (at 'total-supply (get-details collectionName))
    )

    (defun get-nft-price:decimal (collectionName:string)
     (at 'price-per-nft (get-details collectionName))
    )

    (defun get-owner:string (token-id)
      (at 'owner (read policies token-id))
    )

    (defun get-tokens-owned:[string] (account:string)
      (select policies ['id] (where "owner" (= account)))
    )


  )


  
