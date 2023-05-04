(module gen-1-kitty-kad-kitties 'kitty-kad
  "Kitty Kad Kitties Gen1s for the NFT game"

    (defconst KITTIES_MINTED_COUNT_KEY "kitties-minted-count-key")
    (defconst KITTIES_URI_KEY "kitties-uri-key")
    (defconst PRICE_KEY "price-key")
    (defconst ADMIN_KEYSET_NAME "kitty-kad")
    (defconst ADMIN_ADDRESS "k:f7278eeaa55a4b52c281fa694035f82a43a6711eb547fc1ab900be1ccf9fb409")

    (defcap PRIVATE () 
        @doc "can only be called from a private context"
        true
    ) 

    (defcap ACCOUNT_GUARD(account:string) ; Used for admin functions
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard   
            (at "guard" (coin.details account))
        )
    )

    (defcap OWNER (id:string)
        @doc "Enforces that an account owns a kitty kad"
        (let 
            (
                (nft-owner (at "owner" (read nfts id ["owner"])))
            )
            (compose-capability (ACCOUNT_GUARD nft-owner))
        )
    )

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset  ADMIN_KEYSET_NAME)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )


    (defcap MINTED (id:string owner:string)
      @doc "Emitted event when a kitty is created "
      @event true
    )

    (defcap BRED (id:string)
        @doc "Emitted event when a kitty is bred "
        @event true
    )

    (defun initialize ()
        @doc "Initialize the contract the first time its loaded "
        (insert counts KITTIES_MINTED_COUNT_KEY {"count": 0})
        (insert values KITTIES_URI_KEY {"value": "kittykad.com/"})
    )

    ;;;; SCHEMAS AND TABLES ;;;;;

    (defschema nft-main-schema
        @doc "Stores core information about each nft"
        id:string
        parent-1-id:string
        parent-2-id:string
        generation:integer
        birthday:time
        next-breed-time:time
        gene-pairs:list
        item-pairs:list
        name:string
        owner:string
    )

    (defschema counts-schema
        @doc "Basic schema used for counting things"
        count:integer
    )

    (defschema values-schema
        @doc "Basic schema used for storing basic values"
        value:string
    )

    (deftable nfts:{nft-main-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})

    ;;;;; STATE MODIFYING FUNCTIONS 

    (defun increase-count(key:string)
        @doc "Increases count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update counts key 
            {"count": (+ 1 (get-count key))} 
        )
    )

    (defun set-value(key:string value:string)
        @doc "Sets the value for a key to store in a table"
        (with-capability (ADMIN)
            (update values key 
                {"value": value} 
            )
        )
    )

    ;;;;;; KITTY KAD NON STATE MODIFYING HELPER FUNCTIONS ;;;;;;;;;

    (defun get-count (key:string)
        @doc "Gets count for key"
        (at "count" (read counts key ['count] ) )
    )

    (defun get-value (key:string)
        @doc "Gets value for a key"
        (at "value" (read values key ['value] ) )
    )

    (defun enforce-account-guard (account:string)
        @doc "Enforces the user passes the account guard check"
        (let ((user-guard (at "guard" (coin.details account))))
            (enforce-guard (= user-guard account) "account was not found")
        )
    )

    (defun enforce-account-exists (account:string)
        @doc "Enforces that an account exists in the coin table"
        (let ((coin-account (at "account" (coin.details account))))
            (enforce (= coin-account account) "account was not found")
        )
    )

    (defun ids-owned-by (owner:string)
        @doc "All kitties owned by someone, generic name to work with marketplaces"
        (select nfts ["id"] (where "owner" (= owner)))
    )

    (defun ids-of-children (parent-id:string)
        @doc "All children of a kitty"
        (select nfts ["id"]
            (or? 
                (where 'parent-1-id (= parent-id))
                (where 'parent-2-id (= parent-id))
            )
        )
    )

    (defun get-nft-fields-for-ids (fields:list ids:list) 
        @doc "Market interface method for getting fields for a list of ids"
        (map 
            (get-nft-fields-for-id fields)
            ids
        )
    )

    (defun get-nft-fields-for-id (fields:list id:string) 
        @doc "Market interface method for getting fields for an id"
        (if 
            (> (length fields) 0)
            (+ {"id": id} (read nfts id fields))
            (read nfts id)
        )
    )

    (defun uri:string (id:string)
        @doc
        " Give URI for ID. If not supported, return \"\" (empty string)."
        (+ (+ (get-value KITTIES_URI_KEY) id) ".png")
    )

    (defun all-ids ()
        @doc "Returns all the ids"
        (keys nfts)
    )

    ;;;;;; GENERIC HELPER FUNCTIONS ;;;;;;;;;;

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )

)

;  (create-table nfts)
;  (create-table counts)
;  (create-table values)
;  (initialize)
