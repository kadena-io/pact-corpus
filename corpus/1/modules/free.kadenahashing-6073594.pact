(module kadenahashing 'kadenahashing-keyset
  "A smart contract to share Kadena mining rewards."
  
    ; define catalogue table schema and table
    (defschema catalogue
    tier:integer
    price:decimal
    share:integer
    image:string
    status:string
    owner:string)
    (deftable catalogue-table:{catalogue})

    ; define member table schema and table
    (defschema member
    wallet:string)
    (deftable member-table:{member})

    
    ; create an NFT and store in the catalogue table
    (defun createNFT (
        id:string
        tier:integer 
        price:decimal 
        share:integer
        image:string 
        status:string 
        owner:string
        )
        "create an NFT"
        (enforce-keyset 'kadenahashing-keyset)
        (insert catalogue-table id { "tier":tier, "price":price, "share":share,
                                "image":image, "status":status, "owner":owner
                                }
        )
        (format "Created NFT: {} {} {} {} {} {} {}!" [id, tier, price, share, image, status, owner])
        ;(select catalogue-table)
    )

    ; create an NFT IF the ID doesn't already exist
    (defun checkCreateNFT(
        new_id:string
        new_tier:integer 
        new_price:decimal 
        new_share:integer
        new_image:string 
        new_status:string 
        new_owner:string
        )
        "check if nft with new_id is already created, if not create it"
        (enforce-keyset 'kadenahashing-keyset)
        (with-default-read catalogue-table new_id {"tier":0} { "tier":= tier}
            (if (> tier 0) 
                (format "{} Exists Already" [ new_id]) 
                (createNFT new_id new_tier new_price new_share new_image new_status new_owner)
            )
        )
    )

    ; create a member - storing the wallet as the key and a value
    (defun createMember(wallet:string)
        "create member"
        (enforce-keyset 'kadenahashing-keyset)
    	(insert member-table wallet { "wallet":wallet } )
    )

    ; create member IF does not already exist
    (defun checkCreateMember(new_wallet:string)
        "check if member exists, if not create it"
        (enforce-keyset 'kadenahashing-keyset)
        (with-default-read member-table new_wallet {"wallet":"empty"} { "wallet":= wallet}
            (if (= wallet "empty") 
               (createMember new_wallet) 
               (format "{} Member Already Exists" [ new_wallet])
            )
        )
    )

    ; get list of Sold NFTs
    (defun getCatalogueSold()
        "Get list of Sold NFTs"
        (enforce-keyset 'kadenahashing-keyset)
        (select catalogue-table (where 'status (= "Sold")))
    )



    ; get list of Sold NFTs By Owner
    (defun getCatalogueByOwner(wallet:string)
        "Get NFTs by Owner"
        (select catalogue-table  (and? (where  'status (= "Sold")) (where 'owner (= wallet)) ) )
        
    )
    
     ; change all params 
    (defun updateNFTParams(
    nftid:string
    tier:integer
    price:decimal
    share:integer
    image:string
    status:string
    owner:string
    )
        "Update NFT Params"
        (enforce-keyset 'kadenahashing-keyset)
        (update catalogue-table nftid { 
          
          "tier": tier, "price": price, "share": share,
          "image": image, "status": status, "owner": owner
          })
        (format "{} NFT Updated, Owner {} Tier {} price {} share {} image {}  status {}" [nftid, owner, tier, price, share, image, status]  )
    )
    
    ; get list of Available NFTs
    (defun getCatalogueAvailable()
        "Get list of Available NFTs"
        (select catalogue-table (where 'status (= "Available")))
    )

)


