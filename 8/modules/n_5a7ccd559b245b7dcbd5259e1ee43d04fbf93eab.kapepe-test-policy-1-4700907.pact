(module kapepe-test-policy-1 GOVERNANCE
  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_5a7ccd559b245b7dcbd5259e1ee43d04fbf93eab.kapepe-admin-keyset"))
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])
  (use kip.token-manifest)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Constants
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconst COLLECTION_SUPPLY 1000)
  (defconst ITEM_PRICE 10.0)
  (defconst ADMIN_ADDRESS "k:1604706e7486b5abc1c5391a2f83c3d636a4668eb08572a0777286323a2e4c48")
  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Capabilities
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defcap ACCOUNT-GUARD(account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard   
            (at "guard" (coin.details account))
        )
  )
  (defcap MINT(token-id:string new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    (compose-capability (UPDATE-OWNER token-id new-owner))
    (compose-capability (ACCOUNT-GUARD new-owner))
  )

  (defcap MINT-INTERNAL()
    @doc "Capability to wrap all needed caps for minting"
    true
  )

  (defcap CREATE ()
    @doc "Private Capability to update WL amounts after mint"
    true)
  (defcap UPDATE-OWNER (token-id:string new-owner:string)
    @doc "Private Capability to update owner of token"
    true)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Schemas
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defschema counter-schema
    @doc "schema for integer constants such as TOTAL-MINTED-KEY"
    count:integer
  )

  (defschema nfts-schema
    owner:string
    token-id:string
    minted:bool
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


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Tables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (deftable counter:{counter-schema})
  (deftable nfts:{nfts-schema})
  (deftable quotes:{quote-schema})

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Init
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun init()
      (with-capability (GOVERNANCE)
        (create-table counter)
        (create-table nfts)
        (create-table quotes)
        (insert counter "nfts-minted" { 'count: 0 })
        (insert counter "nfts-created" { 'count: 0 })
      )
    )

  (defun get-token:object{nfts-schema} (token:object{token-info})
    (read nfts (at 'id token))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Mint
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
      (defun enforce-init:bool
        ( token:object{token-info}
        )
        (enforce-ledger)
        (write counter "nfts-created" {"count": (+ 1 (get-count "nfts-created"))})
        (insert nfts (at 'id token)
          { "token-id": (at 'id token)
          , "owner": ""
          , "minted": false
        })
    )

    (defun enforce-mint:bool
        ( token:object{token-info}
          account:string
          guard:guard
          amount:decimal
        )

        (with-capability (MINT-INTERNAL)
          (enforce-ledger)
        ;   (enforce (= false (at "minted" (read nfts (at 'id token))) "NFT minted already"))
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
          )
        )
    )

    (defun enforce-burn:bool
        ( token:object{token-info}
          account:string
          amount:decimal )
        (enforce-ledger)
        (enforce false "Burn prohibited")
    )

    (defun enforce-offer:bool
        ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
        @doc "Capture quote spec for SALE of TOKEN from message"
        (enforce-ledger)
        (enforce-minted)
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
        (enforce-minted)
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
        (enforce-minted)
        (update nfts (at 'id token)
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

   (defun enforce-minted:bool (sale:string)
        "Enforces that SALE is id for currently executing pact"
        (enforce (= true (at "minted" (read nfts)) "NFT not minted"))
    )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Utility Setters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun update-owner (token-id:string new-owner:string)
    @doc "Updates token with new owner"
    (require-capability (UPDATE-OWNER token-id new-owner))
      (update nfts token-id
        {'owner: new-owner, 'minted: true}
      )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Utility Getters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defun get-non-minted ()
    (select nfts ["token-id"] (where 'minted (= false)))
  )

    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counter key ['count]))
    )

  (defun get-nft-details (token-id:string)
    (marmalade.ledger.get-manifest token-id)
  )

  (defun get-random:integer (account:string)
    ; (require-capability (RANDOM account))
    (let* (
      (prev-block-hash (at "prev-block-hash" (chain-data)))
      (block-time (at 'block-time (chain-data)))
      (block-hash (hash block-time))
      (total-hash (hash (+ block-hash prev-block-hash)))
      (random (str-to-int 64 (hash (+ total-hash (take 20 account)))))
    )
    random
    )
  )

    (defun get-all-nfts ()
        (keys nfts)
    )

)

