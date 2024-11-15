(module mintit-marketplace GOVERNANCE

  ; mintit-marketplace

  (defcap GOVERNANCE ()
    (enforce-guard (read-keyset 'admin-keyset)))


  ; See mintit-api.create-account for documentation.
  (defun create-account
    ( params:object{mintit-api.create-account-params}
    )
    (mintit-api.create-account params)
  )

  ; See mintit-api.create-nft-collection for documentation.
  (defun create-nft-collection
    ( params:object{mintit-api.create-nft-collection-params}
    )
    (mintit-api.create-nft-collection params)
  )

  ; See mintit-api.mint-nft for documentation.
  (defun mint-nft:bool
    ( params:object{mintit-api.mint-nft-params}
    )
    (let 
      (
        (nft (mintit-api.mint-nft params))
      )

    (bind nft 
      { 'name := nft-name  
      , 'description := description 
      , 'content-hash := content-hash
      , 'spec := spec
      , 'collection-name := collection-name
      , 'owner := minter 
      , 'edition := edition 
      , 'mint-time := mint-time 
      , 'content-uri := content-uri
      }

    (bind (mintit-api.get-nft-collection collection-name)
      { 'creator := creator
      , 'provenance-hash := provenance-hash
      }

    (let*
      ( 
        (datum-object 
          { 'name: nft-name 
          , 'description: description 
          , 'coFntent-hash: content-hash
          , 'spec: spec
          , 'creator: creator
          , 'collection-name: collection-name 
          , 'mint-time: mint-time 
          , 'content-uri: content-uri
          , 'edition: edition
          , 'provenance-hash: provenance-hash
          })
        (minter-guard (at 'guard (mintit-api.get-account minter)))
        (datum-uri (kip.token-manifest.uri "pact:schema" "free.mintit-policy.token-metadata"))
        (manifest-datum (kip.token-manifest.create-datum datum-uri datum-object)) ; See NOTE (1)
        (manifest-uri content-uri) 
        (nft-manifest (kip.token-manifest.create-manifest manifest-uri [manifest-datum]))
        (token-id content-hash)
        (token-precision 0)
      )
      (marmalade.ledger.create-token token-id token-precision nft-manifest mintit-policy)
      (marmalade.ledger.create-account token-id minter minter-guard)
      (marmalade.ledger.mint token-id minter minter-guard 1.0)
    ))))
    true
  )


  (defun sale-nft:string 
    ( params:object{mintit-api.sale-nft-params}
    )
    (bind params 
      { 'content-hash := content-hash
      , 'timeout := timeout 
      }
    
    (mintit-api.sale-nft params)
  
    (bind (mintit-api.get-nft content-hash) { 'owner := owner }

    (marmalade.ledger.sale content-hash owner 1.0 timeout)))
  )

  ; Just an object to be passed to offer-nft
  (defschema offer-nft-params
    nft-id:string  
    seller-account-id:string ; TODO can we retrieve the current owner here?
  )

  (defun offer-nft:bool 
    ( params:object{offer-nft-params}
    )
    (bind params 
      { 'nft-id := token-id 
      , 'seller-account-id := seller 
      }
      (marmalade.ledger.offer token-id seller 1.0)
    )
    true
  )

  ; Just an object to be passed to withdraw-nft
  (defschema withdraw-nft-params
    nft-id:string  
    seller-account-id:string ; TODO can we retrieve the current owner here?
  )

  (defun withdraw-nft:string 
    ( params:object{withdraw-nft-params}
    )
    (bind params 
      { 'nft-id := token-id 
      , 'seller-account-id := seller 
      }
      (marmalade.ledger.withdraw token-id seller 1.0)
    )
  )
)
