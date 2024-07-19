(module mintit-api GOVERNANCE

  (bless "u4Xg4GSg4wZ-6E271w8gVrbicTCiGrR5ST_tXcW9PEQ")

  (defcap GOVERNANCE () (enforce-guard (read-keyset 'admin-keyset)))

  (defun enforce-developer-guard ()
    (bind (get-account "mintit-developer")
      { 'guard := guard }
      (enforce-guard guard)
    )
  )

  (defconst EXC_INVALID_COLLECTION_NAME "EXC_INVALID_COLLECTION_NAME")
  (defconst EXC_INVALID_NFT_CONTENT_HASH "EXC_INVALID_NFT_CONTENT_HASH")
  (defconst EXC_INVALID_NFT_MINT_PRICE "EXC_INVALID_NFT_MINT_PRICE")
  (defconst EXC_INVALID_COLLECTION_INIT_PRICE "EXC_INVALID_COLLECTION_INIT_PRICE")
  (defconst EXC_INVALID_TOKEN_AMOUNT "EXC_INVALID_TOKEN_AMOUNT")
  (defconst EXC_INVALID_MINT_GUARD "EXC_INVALID_MINT_GUARD") ; TODO 
  (defconst EXC_COLLECTION_MAXED_OUT "EXC_COLLECTION_MAXED_OUT")
  (defconst EXC_INVALID_COLLECTION_MAX_SIZE "EXC_INVALID_COLLECTION_MAX_SIZE")
  (defconst EXC_INVALID_COLLECTION_TYPE "EXC_INVALID_COLLECTION_TYPE")
  (defconst EXC_NFT_ALREADY_EXISTS "EXC_NFT_ALREADY_EXISTS")
  (defconst EXC_ACCOUNT_NOT_FOUND "EXC_ACCOUNT_NOT_FOUND")
  (defconst EXC_ACCOUNT_ALREADY_EXISTS "EXC_ACCOUNT_ALREADY_EXISTS")
  (defconst EXC_ACCOUNT_NOT_WHITELISTED_FOR_PREMINT "EXC_ACCOUNT_NOT_WHITELISTED_FOR_PREMINT")
  (defconst EXC_TOO_EARLY_TO_MINT "EXC_TOO_EARLY_TO_MINT")
  (defconst EXC_INVALID_PREMINT_ENDS "EXC_INVALID_PREMINT_ENDS")
  (defconst EXC_MINTER_NOT_WHITELISTED "EXC_MINTER_NOT_WHITELISTED")
  
  (defconst EPOCH:time (time "1970-01-01T00:00:00Z"))
  

  (defschema account 
    owner:string ; k:string 
    guard:guard 
    active:bool 
  )

  ; The key is the owner k:account 
  (deftable accounts:{account})

  ; 
  (defun get-account:object{account}
    ( owner:string ; k:account
    )
    (with-default-read accounts owner
      { 'active: false 
      , 'guard: false 
      }
      { 'active := active 
      , 'guard := guard 
      }
      (enforce active EXC_ACCOUNT_NOT_FOUND)
      { 'owner: owner 
      , 'active: active 
      , 'guard: guard 
      })
  )

  (defcap CREATE_ACCOUNT (owner:string)
    @managed
    true
  )

  (defschema create-account-params 
    owner:string ; k:string 
    guard:guard 
  )

  ; 
  (defun create-account:bool
    ( params:object{create-account-params}
    )
    (enforce-developer-guard)
    (bind params 
      { 'owner := owner 
      , 'guard := guard  
      }
    (with-default-read accounts owner
      { 'active: false 
      }
      { 'active := active 
      }
      (enforce (not active) EXC_ACCOUNT_ALREADY_EXISTS)
      (with-capability (CREATE_ACCOUNT owner)
        (enforce-guard guard)
        (write accounts owner 
          { 'owner: owner 
          , 'active: true 
          , 'guard: guard 
          }))))
    true
  )

  ; NFTs are stored in the nfts table.
  ; An NFT is uniquely identified by its content-hash.
  ; name is optional, it can be the empty string.
  ; spec is arbitrary json data, it can contain anything, for example 
  ; the NFT rarity, or a second-content-hash to support image flipping.
  ; The collection-name identifies which collection the NFT is in, it is used 
  ; to find all NFTs inside a given collection.
  ; The owner is the k:account of the current owner. This is originally equal
  ; to creator, and changes every time the NFT has been sold.
  ; creator is the original creator; this field is used by the 
  ; search-nfts-by-creator function and is reduntant as it can be retrieved 
  ; using the collection-name.
  ; Note creator and provenance-hash are duplicated in collection-name
  (defschema nft
    name:string ; INCLUDE
    description:string ; INCLUDE
    content-hash:string ; INCLUDE
    spec:object ; INCLUDE
    creator:string ; INCLUDE k:account 
    collection-name:string ; INCLUDE
    mint-time:time ; INCLUDE
    content-uri:object{kip.token-manifest.mf-uri} ; rename and include
    edition:integer ; INCLUDE 
    owner:string ; k:account 
    creator:string ; k:account 
    block-number:integer
  )    

  ; The key is simply the content-hash
  (deftable nfts:{nft})



  ; Find an NFT by its content hash.
  ; Use the /local endpoint to call this function.
  ; If no NFT exists with the given content-hash, this function will fail with:
  ; read: row not found: {content-hash}
  (defun get-nft:object{nft}
    ( content-hash
    )
    (at 0 (flip-double-sided-nfts [(read nfts content-hash)]))
  )

  ; Given a collection name, find its *minted* NFTs.
  ; Returns an empty list if the collection exists, but zero NFTs 
  ; have been minted.
  ; Use the /local endpoint to call this function.
  (defun search-nfts-by-collection:[object{nft}]
    ( collection-name:string 
    )
    (flip-double-sided-nfts
      (select nfts (where 'collection-name (= collection-name))))
  )

  ; Find the NFTs with the given creator.
  ; If creator does not exist, the empty list is returned.
  ; Use the /local endpoint to call this function.
  (defun search-nfts-by-creator:[object{nft}]
    ( creator:string ; k:account 
    )
    (flip-double-sided-nfts
      (select nfts (where 'creator (= creator))))
  )

  ; Find the NFTs with the given current owner.
  ; If owner does not exist, the empty list is returned.
  ; Use the /local endpoint to call this function.
  (defun search-nfts-by-owner:[object{nft}]
    ( owner:string ; k:account 
    )
    (flip-double-sided-nfts
      (select nfts (where 'owner (= owner))))
  )

  ; Will return all nfts minted during the last provided number of minutes.
  (defun search-latest-nfts:[object{nft}]
    ( mins:integer   
    )
    (let*
      ( 
        (curr-time (at 'block-time (chain-data)))
        (search-time (add-time curr-time (minutes (- mins))))
      )
      (flip-double-sided-nfts
        (select nfts (where 'mint-time (>= search-time)))))
  )

  ; NFT collections are stored in the nft-collections table.
  ; An NFT collection is uniquely identified by its name across all k:accounts.
  ; The creator is the k:account of the original creator.
  ; The name is the collection name, and also its id. 
  ; num-minted is the current number of NFTs that have been minted in this 
  ; collection.
  ; max-size is the total number of NFTs in this collection.
  ; mint-price is the price to mint each NFT in this collection.
  ; mint-guard defines who can mint NFTs in this collection.
  ; mint-royalties define who gets paid when NFTs in this collection are 
  ; minted.
  ; sale-royalties define who gets paid when NFTs in this collection are sold.
  (defschema nft-collection
    creator:string ; k:account 
    description:string  
    name:string 
    type:string 
    provenance-hash:string
    num-minted:integer 
    max-size:integer 
    mint-starts:time 
    premint-ends:time 
    premint-whitelist:[string] ; k:account
    init-price:decimal
    mint-price:decimal 
    block-number:integer
    init-royalties:object{mintit-royalty.royalty-rates}
    mint-royalties:object{mintit-royalty.royalty-rates}
    sale-royalties:object{mintit-royalty.royalty-rates}
  ) 

  ; The key is simply the collection-name
  (deftable nft-collections:{nft-collection})

  
  ; Find an NFT collection by its name.
  ; Use the /local endpoint to call this function.
  ; If no NFT collection exists with the given name, this function will fail with:
  ; read: row not found: {name}
  (defun get-nft-collection:object{nft-collection}
    ( name:string 
    )
    (read nft-collections name)
  ) 

  ; Find the NFT collections with the given creator.
  ; If creator does not exist, the empty list is returned.
  ; Use the /local endpoint to call this function.
  (defun search-nft-collections-by-creator:[object{nft-collection}]
    ( creator:string ; k:account 
    )
    (select nft-collections (where 'creator (= creator)))
  )

  ; When minting an NFT from a collection, royalties will be payed out to
  ; stakeholders. Use this function for help computing the capabilities for 
  ; your request.
  ; Use the /local endpoint to call this function.
  (defun precompute-nft-collection-mint-royalties:object{mintit-royalty.royalty-payouts}
    ( name:string 
      from:string ; k:account
    )
    (bind (get-nft-collection name) 
      { 'mint-royalties := rates
      , 'mint-price := amount 
      }
      (mintit-royalty.create-payouts rates from amount)
    )
  )

  ; When creating an NFT from a collection, royalties will be payed out to
  ; stakeholders. Use this function for help computing the capabilities for 
  ; your request.
  ; Use the /local endpoint to call this function.
  (defun precompute-nft-collection-init-royalties:object{mintit-royalty.royalty-payouts}
    ( name:string 
    )
    (bind (get-nft-collection name) 
      { 'init-royalties := rates
      , 'init-price := amount 
      , 'creator := from 
      }
      (mintit-royalty.create-payouts rates from amount)
    )
  )

  ; When selling an NFT from a collection, royalties will be payed out to
  ; stakeholders. Use this function for help computing the capabilities for 
  ; your request.
  ; Use the /local endpoint to call this function.
  (defun precompute-nft-collection-sale-royalties:object{mintit-royalty.royalty-payouts}
    ( name:string 
      from:string ; k:account
      price:decimal 
    )
    (bind (get-nft-collection name) 
      { 'sale-royalties := rates
      }
      (mintit-royalty.create-payouts rates from price)
    )
  )

  (defschema nft-info
    owner:string ; k:account 
    owner-guard:guard 
    mint-price:decimal 
    init-price:decimal 
    mint-royalties:object{mintit-royalty.royalty-rates}
    sale-royalties:object{mintit-royalty.royalty-rates}
    init-royalties:object{mintit-royalty.royalty-rates}
  ) 
  
  ;
  (defun get-nft-info:object{nft-info}
    ( content-hash:string 
    )
    (bind (get-nft content-hash) 
      { 'owner := owner 
      , 'collection-name := collection-name 
      }
    (bind (get-account owner)
      { 'guard := owner-guard
      }
    (bind (get-nft-collection collection-name)
      { 'mint-price := mint-price 
      , 'init-price := init-price 
      , 'sale-royalties := sale-royalties
      , 'mint-royalties := mint-royalties
      , 'init-royalties := init-royalties
      }
      { 'owner: owner 
      , 'owner-guard: owner-guard 
      , 'mint-price: mint-price 
      , 'init-price: init-price 
      , 'sale-royalties: sale-royalties
      , 'mint-royalties: mint-royalties
      , 'init-royalties: init-royalties
      })))
  )

  (defconst PRIVATE_COLLECTION_TYPE:string "private")
  (defconst PUBLIC_COLLECTION_TYPE:string "public")

  ; The object to be passed to create-nft-collection
  ; Its fields have the same semantics as those in the nft-collection schema. 
  (defschema create-nft-collection-params
    creator:string ; k:account
    description:string 
    name:string 
    type:string 
    provenance-hash:string
    mint-starts:time 
    premint-ends:time 
    premint-whitelist:[string] ; k:account
    max-size:integer 
    mint-price:decimal 
    init-price:decimal 
    mint-royalties:object{mintit-royalty.royalty-rates}
    sale-royalties:object{mintit-royalty.royalty-rates}
    init-royalties:object{mintit-royalty.royalty-rates}
  )
  
  (defcap CREATE_COLLECTION (creator:string)
    @managed
    (bind (get-account creator) { 'guard := guard } (enforce-guard guard))
  )

  ; Create an NFT collection. 
  ; No NFTs will be minted at this stage, but the collection will be inserted
  ; in the nft-collections table, so that it can be retrieved.
  ; Sanity checks will be performed on the params, for example if the 
  ; mint-price is negative, this will throw EXC_INVALID_NFT_MINT_PRICE.
  ; The creator as well as all stakeholders in mint-royalties and 
  ; sale-royalties must be *existing* k:account, or an unhelpful message will 
  ; be thrown by pact. 
  ; You should never need to call this function directly, use 
  ; mintit-marketplace.create-nft-collection instead.
  (defun create-nft-collection:bool
    ( params:object{create-nft-collection-params}
    )
    (bind params  
      { 'creator := creator
      , 'description := description
      , 'name := name
      , 'type := type 
      , 'provenance-hash := provenance-hash 
      , 'mint-starts := mint-starts
      , 'premint-ends := premint-ends
      , 'premint-whitelist := premint-whitelist
      , 'max-size := max-size
      , 'init-price := init-price
      , 'mint-price := mint-price
      , 'init-royalties := init-royalties
      , 'sale-royalties := sale-royalties
      , 'mint-royalties := mint-royalties
      } 

    (enforce-developer-guard)

    (enforce (>= premint-ends mint-starts) EXC_INVALID_PREMINT_ENDS)
    (enforce (>= init-price 0.0) EXC_INVALID_COLLECTION_INIT_PRICE)
    (enforce (>= mint-price 0.0) EXC_INVALID_NFT_MINT_PRICE)
    (enforce (!= "" name) EXC_INVALID_COLLECTION_NAME)
    (enforce (<= (length name) 64) EXC_INVALID_COLLECTION_NAME)
    (enforce (>= max-size 1) EXC_INVALID_COLLECTION_MAX_SIZE)
    (enforce (or? (= PUBLIC_COLLECTION_TYPE) (= PRIVATE_COLLECTION_TYPE) type) EXC_INVALID_COLLECTION_TYPE)

    (mintit-royalty.verify-rates sale-royalties)
    (mintit-royalty.verify-rates mint-royalties)
    (mintit-royalty.verify-rates init-royalties)
    
    (mintit-royalty.create-execute-payouts init-royalties creator init-price)

    (with-capability (CREATE_COLLECTION creator)
      
      (write nft-collections name
        { 'creator: creator
        , 'description: description
        , 'name: name
        , 'type: type
        , 'provenance-hash: provenance-hash 
        , 'mint-starts: mint-starts
        , 'premint-ends: premint-ends
        , 'premint-whitelist: premint-whitelist 
        , 'max-size: max-size
        , 'init-price: init-price
        , 'mint-price: mint-price
        , 'sale-royalties: sale-royalties
        , 'mint-royalties: mint-royalties
        , 'init-royalties: init-royalties
        , 'block-number: (at 'block-height (chain-data))
        , 'num-minted: 0
        })))
    true
  )

  ; The object to be passed to mint-nft.
  ; Its fields have the same semantics as those in the nft schema.
  (defschema mint-nft-params
    minter:string ; k:account
    name:string 
    description:string
    content-hash:string 
    spec:object
    collection-name:string
    content-uri:object{kip.token-manifest.mf-uri}
    edition:integer 
  )

  (defcap MINT 
    ( creator:string ; k:account
      minter:string 
      type:string
    )
    @managed
    (if (= type PRIVATE_COLLECTION_TYPE)
      [ (bind (get-account creator) { 'guard := guard } (enforce-guard guard)) ]
      [ (bind (get-account minter) { 'guard := guard } (enforce-guard guard)) ]
    )
    (enforce-developer-guard)
  )

  ; mint an NFT.
  ; The collection mint guard will be enforced, so add it to your request!
  ; If an NFT with the same content-hash exists, this will throw 
  ; EXC_NFT_ALREADY_EXISTS.
  ; If the maximum number of NFTs has been minted for this collection, this 
  ; will throw EXC_COLLECTION_MAXED_OUT
  ; All mint royalty stakeholders must be existing k:accounts, or unhelpful 
  ; error messages will the thrown by pact.
  ; The correct capabilities must be installed, as there will as many transfers
  ; as there are mint royalty stakeholders. 
  ; Use the function precompute-nft-collection-mint-royalties for that.
  ; The NFT will also be written to the marmalade ledger.
  ; The marmalade ledger uses account-id:token-id as the table key.
  ; The NFT token-id is simply its content-hash.
  ; You should never need to call this function directly, use 
  ; mintit-marketplace.mint-nft instead.
  (defun mint-nft:object{nft}
    ( params:object{mint-nft-params}
    )
    (enforce-developer-guard)

    (bind params 
      { 'minter := minter 
      , 'name := nft-name  
      , 'description := description
      , 'content-hash := content-hash
      , 'spec := spec
      , 'collection-name := collection-name
      , 'content-uri := content-uri 
      , 'edition := edition
      }

    (enforce (!= "" content-hash) EXC_INVALID_NFT_CONTENT_HASH)

    (let
      ( 
        (collection (get-nft-collection collection-name))
        (currtime (at 'block-time (chain-data)))
      )

    (bind collection 
      { 'creator := creator
      , 'num-minted := num-minted 
      , 'max-size := max-size 
      , 'mint-starts := mint-starts
      , 'premint-ends := premint-ends
      , 'premint-whitelist := premint-whitelist
      , 'type := type
      }

    (let 
      (
        (nft { 'name: nft-name 
             , 'content-hash: content-hash 
             , 'spec: spec
             , 'description: description
             , 'owner: minter  
             , 'creator: creator 
             , 'collection-name: collection-name
             , 'mint-time: currtime 
             , 'content-uri: content-uri
             , 'edition: edition
             , 'block-number: (at 'block-height (chain-data))
             })
      )

    (enforce (> max-size num-minted) EXC_COLLECTION_MAXED_OUT)

    (enforce (<= mint-starts currtime) EXC_TOO_EARLY_TO_MINT)
    
    (with-capability (MINT creator minter type)

      (enforce (or (>= currtime premint-ends) (contains minter premint-whitelist)) 
        EXC_MINTER_NOT_WHITELISTED)

      (with-default-read nfts content-hash { 'owner: "" } { 'owner := owner }

        (enforce (= "" owner) EXC_NFT_ALREADY_EXISTS)
        
        (write nfts content-hash nft)
      
        (update nft-collections collection-name { 'num-minted: (+ 1 num-minted) })
      ))
      
    nft))))
  )

  ; 
  (defschema sale-nft-params 
    content-hash:string 
    timeout:integer 
  )

  ;
  (defcap SALE 
    ( content-hash:string 
      owner:string 
    )
    @managed 
    (enforce-guard (at 'guard (get-account owner)))
  )

  (defun sale-nft:bool
    ( params:object{sale-nft-params}
    )
    (bind params 
      { 'content-hash := content-hash
      , 'timeout := timeout 
      }
    (bind (get-nft content-hash)
      { 'owner := owner 
      }
    true))
  )

  ; Some NFTs represent double-sided images.
  ; To indicate this, the NFT must contain a field named `second-content-hash`
  ; in its `spec`.
  ; If this field is present, then the returned NFT object will contain a new 
  ; field in its spec called current-content-hash, which may contain the 
  ; content-hash or the second-content-hash depending on the time of day 
  ; (using the block-time to determine this). 
  ; If the field is not present, then the original NFT is returned instead.
  ; This function is called automatically from other functions in this module.
  ; You should never need to call this function directly.
  (defun flip-double-sided-nfts:[object{nft}]
    ( nfts:[object{nft}]
    )
    (let*
      (
        (block-time (at 'block-time (chain-data)))
      )
      (map (flip-double-sided-nft block-time) nfts)
    )
  )

  ; You should never need to call this function directly.
  (defun flip-double-sided-nft:object{nft}
    ( block-time:time
      nft:object{nft}
    )
    (bind nft
      { 'name := name 
      , 'content-hash := content-hash 
      , 'spec := spec 
      , 'owner := owner 
      , 'creator := creator  
      , 'collection-name := collection-name 
      }

    (let*
      (
        (time-hhmmss:string (format-time "%H:%M:%S" block-time))
        (before-midday:bool (<= time-hhmmss "12:00:00"))
        (midday-spec (try {} (at 'midday-spec spec)))
        (midnight-spec (try {} (at 'midnight-spec spec)))
        (is-double-sided-nft (and (!= midday-spec {}) (!= midday-spec {})))
        (current-spec (if before-midday midnight-spec midday-spec))
        (updated-nft (+ { 'spec: current-spec } nft))
      )
    
      (if is-double-sided-nft updated-nft nft))))
)
    

