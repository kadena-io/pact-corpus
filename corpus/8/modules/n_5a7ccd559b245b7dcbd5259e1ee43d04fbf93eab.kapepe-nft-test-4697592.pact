(module kapepe-nft-test GOVERNANCE

  (use coin)
  (use marmalade.ledger)
  (use kip.token-policy-v1 [token-info])
  (use kip.token-manifest)

  ; Constants

  (defconst COLLECTION_SUPPLY 1000)
  (defconst COLLECTION_ID "kapepe-nft-test")
  (defconst ITEM_PRICE 10.0)
  (defconst ADMIN_ADDRESS "k:1604706e7486b5abc1c5391a2f83c3d636a4668eb08572a0777286323a2e4c48")

  ; Capabilities

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_5a7ccd559b245b7dcbd5259e1ee43d04fbf93eab.kapepe-admin-keyset")))

  (defcap CREATE-NFT ()
    @doc "Private capability for creating an NFT."
    true
  )

  (defcap UPDATE-COLLECTION ()
    @doc "Private capability for updating collection information."
    true
  )

  (defcap MINT (token-id:string new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    true
  )

  (defschema collection-schema
    id:string
    non-minted-tokens:[string]
    minted-total:integer
    current-unique-supply:integer
    max-unique-supply:integer
    policy:module{kip.token-policy-v1}
  )

  (deftable collection:{collection-schema})

  ; Creating

  (defun create-collection ()
    (with-capability (GOVERNANCE)
      (insert collection COLLECTION_ID
        {'id: COLLECTION_ID
         ,'non-minted-tokens: []
         ,'minted-total: 0
         ,'current-unique-supply: 0
         ,'max-unique-supply: COLLECTION_SUPPLY})
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

  ; Getters

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

; (create-table collection)
; (create-collection)
; (update collection COLLECTION_ID { 'policy:= module{kip.token-policy-v1}})
