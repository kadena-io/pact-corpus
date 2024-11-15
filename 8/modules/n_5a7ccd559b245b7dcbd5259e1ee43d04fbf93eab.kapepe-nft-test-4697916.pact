(module kapepe-nft-test GOVERNANCE
  
  (implements kip.token-policy-v1) 

  (use coin)
  (use marmalade.ledger)
  (use kip.token-policy-v1 [token-info])
  (use kip.token-manifest)

  ; Constants

  (defconst COLLECTION_SUPPLY 1000)
  (defconst COLLECTION_ID "kapepe-nft-test")
  (defconst ITEM_PRICE 10.0)
  (defconst ADMIN_ADDRESS "k:1604706e7486b5abc1c5391a2f83c3d636a4668eb08572a0777286323a2e4c48")
  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  ; Capabilities

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_5a7ccd559b245b7dcbd5259e1ee43d04fbf93eab.kapepe-admin-keyset")))

  (defcap ACCOUNT-GUARD(account:string)
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard
            (at "guard" (coin.details account))
        )
  )

  (defcap CREATE-NFT ()
    @doc "Private capability for creating an NFT."
    true
  )

  (defcap UPDATE-COLLECTION ()
    @doc "Private capability for updating collection information."
    true
  )

  (defcap MINT(token-id:string new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    (compose-capability (UPDATE-OWNER token-id new-owner))
    (compose-capability (INCREMENT-MINTED))
    (compose-capability (UPDATE-WL))
    (compose-capability (ACCOUNT-GUARD new-owner))
  )
  (defcap CREATE ()
    @doc "Private Capability to update WL amounts after mint"
    (compose-capability (INCREMENT-CREATED))
    true)
  (defcap UPDATE-OWNER (token-id:string new-owner:string)
    @doc "Private Capability to update owner of token"
    true)
  (defcap INCREMENT-MINTED ()
    @doc "Private Capability to increment amount minted"
    true)
  (defcap INCREMENT-CREATED ()
    @doc "Private Capability to increment amount created"
    true)
  (defcap UPDATE-WL ()
    @doc "Private Capability to update WL amounts after mint"
    true)

  (defschema collection-schema
    id:string
    non-minted-tokens:[string]
    minted-total:integer
    current-unique-supply:integer
    max-unique-supply:integer
    policy:module{kip.token-policy-v1}
  )

  (deftable collection:{collection-schema})

  (defschema counter-schema
    @doc "schema for integer constants such as TOTAL-MINTED-KEY"
    count:integer
  )

  (deftable counter:{counter-schema})

  (defschema tokens-schema
    owner:string
    token-id:string
  )

  (deftable tokens:{tokens-schema})

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

  ; Creating

  (defun create-collection (policy:module{kip.token-policy-v1})
    (with-capability (GOVERNANCE)
      (insert collection COLLECTION_ID
        {'id: COLLECTION_ID
         ,'non-minted-tokens: []
         ,'minted-total: 0
         ,'current-unique-supply: 0
         ,'max-unique-supply: COLLECTION_SUPPLY
         ,'policy:policy})
      true
    )
  )

    (defun create-nfts-bulk (items:list)
      @doc "Bulk creates marmalade tokens for any collection. Formatted in specs : [{nft-data : {}, uri-data : '', uri-scheme: ''}]"
      (let*
        (
          (collection (get-collection))
        )
        (with-capability (GOVERNANCE)
          (let*
            (
              (token-ids (map (lambda (item) (create-nft-token item)) items))
              (amount (length token-ids))
            )
            (update-non-minted-collection token-ids COLLECTION_ID collection)
            token-ids
          )
        )
      )
    )
    
    (defun create-nft-token (datum:object)
      @doc "Creates a manifest then builds a marmalade token using supplied policy"
      (require-capability (GOVERNANCE))
      (let*
        (
          (policy (at 'policy (get-collection)))
          (datum-uri (at 'uri datum))
          (datum-uri-data:string (at 'data datum-uri))
          (datum-uri-scheme:string (at 'scheme datum-uri))
          (nft-datum:object{mf-datum} (create-datum (uri datum-uri-scheme datum-uri-data) datum))
          (manifest:object{manifest} (create-manifest (uri (at 'scheme datum-uri) (at 'data datum-uri)) [nft-datum]))
          (token-id-suffix (at 'edition datum))
          (token-id (+ (+ COLLECTION_ID "-") token-id-suffix))
        )
        (marmalade.ledger.create-token token-id 0 manifest policy)
        token-id
        )
    )


  (defun update-non-minted-collection (token-ids:list collection:object{collection-schema})
    (require-capability (UPDATE-COLLECTION))
    (bind collection
      { 'non-minted-tokens:= non-minted-tokens:list
        ,'current-unique-supply:= current-supply:integer
      }
      (update collection COLLECTION_ID
        {'non-minted-tokens: (+ token-ids non-minted-tokens)
        ,'current-unique-supply: (+ (length token-ids) current-supply)}
      )
    )
  )

  ; Minting

  (defun mint (token-id:string new-owner:string)
    (require-capability (MINT token-id new-owner))
    (let* (
        (collection (get-collection))
        (non-minted-tokens (at 'non-minted-tokens collection))
        (minted-total (at 'minted-total collection))
      )
      (enforce (contains token-id non-minted-tokens) "Token not available for minting!")
      (coin.transfer new-owner ADMIN_ADDRESS ITEM_PRICE)
      (update collection COLLECTION_ID
        {'non-minted-tokens: (filter (not-in-list token-id) non-minted-tokens)
         ,'minted-total: (+ 1 minted-total)}
      )
      (marmalade.ledger.mint token-id new-owner (keyset-ref-guard new-owner) 1.0)
    )
  )

  (defun not-in-list (element:string in-list:string) (not (= element in-list)))

  ; Enforcements
  
    (defun enforce-ledger:bool ()
        (enforce-guard (marmalade.ledger.ledger-guard))
    )
  
    (defun enforce-mint:bool
        ( token:object{token-info}
          account:string
          guard:guard
          amount:decimal
        )

        (with-capability (MINT (at 'id token) account)
          (enforce-ledger)
          (write counter "nfts-minted" {"count": (+ 1 (get-count "nfts-minted"))})
          (bind (get-token token)
             { 'owner:=owner:string
             }
             (enforce (= (+ amount (at 'supply token)) 1.0) "Exceeds max supply")
             (update-owner (at 'id token) account)
          )
          (if
              (= account ADMIN_ADDRESS)
              true
              (coin.transfer account ADMIN_ADDRESS ITEM_PRICE)
          ))
    )

    (defun enforce-burn:bool
        ( token:object{token-info}
          account:string
          amount:decimal )
        (enforce-ledger)
        (enforce false "Burn prohibited")
    )

    (defun enforce-init:bool
        ( token:object{token-info}
        )
        (enforce-ledger)
        (write counter "nfts-created" {"count": (+ 1 (get-count "nfts-created"))})
        (insert tokens (at 'id token)
          { "token-id": (at 'id token)
          , "owner": ADMIN_ADDRESS
        })
        (enforce false "All tokens initialized")
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
          (insert quotes sale-id { 'id: (at 'id token), 'spec: spec }))
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

    (defun enforce-transfer:bool
        ( token:object{token-info}
          sender:string
          guard:guard
          receiver:string
          amount:decimal )
        (enforce-ledger)
        (update tokens (at 'id token)
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
        (enforce false "Transfer across chains prohibited")
    )

   (defun enforce-sale-pact:bool (sale:string)
        "Enforces that SALE is id for currently executing pact"
        (enforce (= sale (pact-id)) "Invalid pact/sale id")
    )

  ; Setters
  
  (defun update-owner (token-id:string new-owner:string)
    @doc "Updates token with new owner"
    (require-capability (UPDATE-OWNER token-id new-owner))
      (update tokens token-id
        {'owner: new-owner}
      )
  )

  ; Getters
  
    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counter key ['count]))
    )
  
  (defun get-token:object{token-schema} (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun get-collection:object ()
    (read collection COLLECTION_ID)
  )

  (defun get-minted-total:integer ()
    (at 'minted-total (get-collection))
  )

  (defun get-nft-details (token-id:string)
    (marmalade.ledger.get-manifest token-id)
  )

)

; (insert counter "nfts-minted" { 'count: 0 })
; (insert counter "nfts-created" { 'count: 0 })
; (create-table counter)
; (create-table tokens)
; (create-table quotes)
; (create-table collection)
; (create-collection)
; (update collection COLLECTION_ID { 'policy:= module{kip.token-policy-v1}})
