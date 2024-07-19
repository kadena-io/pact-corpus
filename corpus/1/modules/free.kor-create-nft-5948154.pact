(module kor-create-nft MODULE_ADMIN

    (defcap MODULE_ADMIN () true)

(defcap ADMIN ()
    @doc "makes sure only admin can make changes"
    (enforce-guard 'kor-project-keyset)
  )
    ; The "key" for each entry in the table will be the NFT ID
    ; and the values fetched for each ID will be the owner, value, date
    (defschema owners-schema
        @doc "Stores the owner information for each nft"
        owner-address:string
        nft-value:string
        created-date:integer
        hash-rate:string
        rarityModel:string
        nftid:string
        phone-number:string
        og-badge:string
    )


    (deftable nft-kordata:{owners-schema})

    (defun set-owner(owner-address:string nft-id:string)
        @doc "Set the owner of an NFT - only available for admin"
        (with-capability (MODULE_ADMIN)
     
            (enforce-keyset  (read-keyset "admin-keyset"))
            (insert nft-kordata nft-id {"owner-address": owner-address})
    ))
    (defun set-values(owner-address:string nft-id:string nft-value:string created-date:integer hash-rate:string rarityModel:string)
        @doc "Set the values for NFT"
            (insert nft-kordata nft-id {"owner-address": owner-address,"nft-value": nft-value,"created-date": created-date, "hash-rate": hash-rate, "rarityModel":rarityModel, "nftid" : nft-id})
    )

    (defun get-owner (nft-id:string)
        @doc "Gets the owner of an NFT"
       
        (at "owner-address" (read nft-kordata nft-id ['owner-address] ))
    )
    (defun get-nftvalue (nft-id:string)
        @doc "Gets the value of an NFT"
       
        (at "nft-value" (read nft-kordata nft-id ['nft-value] ))
    )
    (defun get-createddate (nft-id:string)
        @doc "Gets the created date of an NFT"
       
        (at "created-date" (read nft-kordata nft-id ['created-date] ))
    )
    (defun get-hashrate (nft-id:string)
        @doc "Gets the created date of an NFT"
       
        (at "hash-rate" (read nft-kordata nft-id ['hash-rate] ))
    )
    (defun get-allvalues ()
        @doc "Return all values from the table"
       
        (select nft-kordata (constantly true))
    )
(defun get-ownedby (owner-address:string)
        @doc "All Nfts owned by someone"
        (select nft-kordata ["nftid"] (where "owner-address" (= owner-address)))
)

(defun update-kyc (nft-id:string phone-number:string)
        (update nft-kordata nft-id  {
        "phone-number":phone-number
    }
)
)
(defun update-ogbadge (nft-id:string og-badge:string)
        (update nft-kordata nft-id  {
        "og-badge":og-badge
    }
)
)
(defun update-ogdata (nft-id:string og-badge:string phone-number:string)
        (update nft-kordata nft-id  {
        "og-badge":og-badge, "phone-number":phone-number
    }
)
)
(defun get-ogbadge (owner-address:string)
        @doc "Get OG badge details"
        (select nft-kordata ["og-badge"] (where "owner-address" (= owner-address)))
)
    (defun uri:string (id:string)
        @doc
        " Give URI for ID. If not supported, return \"\" (empty string)."

        ; This will take the url of the website you uplaoded your image to
        ; and then add the id and .jpg to the end of it
        (+ "https://koryptoblockchain.com/nft/"
            ; replace .jpg with the provided nft format
            (+ id ".gif")
        )
    )
)
