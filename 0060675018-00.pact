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
    minted:time
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

 
)
; create-table
;(create-table radmin)

