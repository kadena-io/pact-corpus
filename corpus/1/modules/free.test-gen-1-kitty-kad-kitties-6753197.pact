(module test-gen-1-kitty-kad-kitties 'kitty-kad
  "Kitty Kad Kitties NFTs game"

    (defconst KITTIES_MINTED_COUNT_KEY "kitties-minted-count-key")
    (defconst KITTIES_URI_KEY "kitties-uri-key")
    (defconst PRICE_KEY "price-key")
    (defconst ADMIN_KEYSET_NAME "kitty-kad")
    (defconst ADMIN_ADDRESS "k:f7278eeaa55a4b52c281fa694035f82a43a6711eb547fc1ab900be1ccf9fb409")
    (defconst MARKET_FEE 0.03)

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

    (defun initialize ()
        @doc "Initialize the contract the first time its loaded "
        (insert counts KITTIES_MINTED_COUNT_KEY {"count": 0})
        (insert values KITTIES_URI_KEY {"value": "kittykad.com/"})
        (insert price PRICE_KEY {"price": 1.0})
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

    (defschema price-schema
        @doc "Prices schema"
        price:decimal
    )

    (deftable nfts:{nft-main-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})
    (deftable price:{price-schema})

    (defun mint-kitty (parent-1-id:string parent-2-id:string owner:string)
        @doc "Mints a new kitty"
        (with-capability (ADMIN) 
            ;  (enforce (= (at "chain-id" (chain-data)) "1") "Can only mint on specific chain")
            (let* 
                (
                    (p1-data (free.kitty-kad-kitties.get-nft-fields-for-id [] parent-1-id))
                    (p2-data (free.kitty-kad-kitties.get-nft-fields-for-id [] parent-2-id))
                    (new-id (id-for-new-kitty))
                    (new-kitty-data (free.kitty-kad-kitties-helper-functions.create-new-kitty-data p1-data p2-data new-id owner))
                )
                (insert nfts new-id new-kitty-data)
                ; TODO UPDATE KITTIES BRED TIMES
                ; TODO CHARGE BREEDING FEE

            )
            (increase-count KITTIES_MINTED_COUNT_KEY)
        )
    )

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

    (defun set-price(price-value:decimal)
        @doc "Set the price"
        (with-capability (ADMIN)
            (update price PRICE_KEY {"price": price-value})
        )
    )

    ;;;;;; KITTY KAD NON STATE MODIFYING HELPER FUNCTIONS ;;;;;;;;;

    (defun get-price()
        (at "price" (read price PRICE_KEY ["price"]))
    )

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

    (defun id-for-new-kitty ()
        @doc "Returns an id for a new kitty to be minted"
        (int-to-str 10 (+ 1 (get-count KITTIES_MINTED_COUNT_KEY)))
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


