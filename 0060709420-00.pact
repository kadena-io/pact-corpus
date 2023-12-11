(module gg01 GOVERNANCE

  (use fungible-util)
  (use util.guards)

  (defcap GOVERNANCE ()
  (with-read radmin "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
  ))

  (defcap INTERNAL ()
    "mark some functions as internal only..."
    true
  )

  (defschema admin
    admin:string
    aguard:guard
  )

  (deftable radmin:{admin})

  (defschema model
    id:string
    name:string
    description:string
    rarity:string
    game:string
    image:string
    maxMint:integer
    minted:integer
  )

  (deftable models:{model})

  (defschema nft
    nftId:string
    modelId:string
    mintedAt:time
    price:decimal
    owner:string
    guard:guard
    forSale:bool
    likes:integer
  )

  (deftable nfts:{nft})

  (defun init ()
    "Set admin...."
    (insert radmin "admin" {
      "admin": (at "sender" (chain-data)),
      "aguard": (read-keyset "keyset")
      } )
  )

  (defun get-time:time ()
    "Get block time"
    (at "block-time" (chain-data))
  )

  (defun insert-model (id name description rarity game image maxMint)
  (with-read radmin "admin"
    { "aguard" := aguard }
        (enforce-guard aguard)
        (insert models id
          {"id":id,
           "name":name,
           "description": description,
           "rarity": rarity,
           "game": game,
           "image": image,
           "maxMint": maxMint,
           "minted": 0
          }
        )
  )
  )

  (defun mint-nft (nftId modelId)
  (with-read radmin "admin"
    { "aguard" := aguard }
        (enforce-guard aguard)
        (with-read models modelId
          {"minted":= minted,
           "maxMint":= maxMint}
        (enforce (< minted maxMint) "Already minted maximum allowed")   
        (insert nfts nftId
          {"nftId":nftId,
           "modelId":modelId,
           "mintedAt": (get-time),
           "price": 0.0,
           "owner":(at "sender" (chain-data)),
           "guard": (read-keyset "keyset"),
           "forSale": false,
           "likes":0
          }
        )
        (update models modelId
          {"minted": (+ minted 1)}
        )
        )
  )
  )

  (defun get-models ()
    "Get all models"
    (select models (constantly true))
  )

  (defun get-nft (nftId)
    "Get nft details"
    (with-read nfts nftId
      {"nftId":= nftId,
       "modelId":= modelId,
       "mintedAt":= mintedAt,
       "price":= price,
       "owner":= owner,
       "forSale":= forSale,
       "likes":= likes}
    (with-read models modelId
      {"id":= modelId,
       "name":= name,
       "description":= description,
       "rarity":= rarity,
       "game":= game,
       "image":= image,
       "maxMint":= maxMint,
       "minted":= minted}
         (format "{},{},{},{},{},{},{},{},{},{}" [nftId name description rarity game image mintedAt price owner forSale])
    )
    )

  )


)
; create-table
;(create-table radmin)
;(create-table models)
;(create-table nfts)

 
