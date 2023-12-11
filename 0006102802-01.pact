(module kitty-kad-kitties 'kitty-kad
  "Kitty Kad Kitties NFTs game"

    (defconst KITTIES_CREATED_COUNT_KEY "kitties-count-key")
    (defconst KITTIES_ADOPTED_COUNT_KEY "kitties-adopted-count-key")
    (defconst KITTIES_CREATED_TO_ADOPT_COUNT_KEY "kitties-created-to-adopt-count-key")
    (defconst KITTIES_URI_KEY "kitties-uri-key")
    (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
    (defconst PRICE_KEY "price-key")
    (defconst BITS_PER_GENE 30)
    (defconst MAX_GENE_SIZE 1073741823)
    (defconst AMOUNT_OF_GENES 15)
    (defconst AMOUNT_OF_ITEMS 15)
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


    (defcap PUT_ON_SALE (id:string owner:string price:decimal)
      @doc "Emitted event when a Kitty is put on sale "
      @event true
    )

    (defcap REMOVED_FROM_SALE (id:string owner:string)
      @doc "Emitted event when a kitty is removed from sale "
      @event true
    )

    (defcap BOUGHT (id:string new-owner:string original-owner:string price:decimal)
        @doc "Emitted event when a kitty is removed from sale "
        @event true
    )

    (defun initialize ()
        @doc "Initialize the contract the first time its loaded "
        (insert counts KITTIES_CREATED_COUNT_KEY {"count": 0})
        (insert counts KITTIES_ADOPTED_COUNT_KEY {"count": 0})
        (insert counts KITTIES_CREATED_TO_ADOPT_COUNT_KEY {"count": 0})
        (insert values MINT_CHAIN_ID_KEY {"value": "1"})
        (insert values KITTIES_URI_KEY {"value": "kittykad.com/"})
        (insert price PRICE_KEY {"price": 5.0})
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

    (defschema gen-0-adopt-schema
        @doc "Gen 0s for adoption traits"
        gene-pairs:list
        item-pairs:list
        adopted:bool
    )

    (defschema counts-schema
        @doc "Basic schema used for counting things"
        count:integer
    )

    (defschema values-schema
        @doc "Basic schema used for storing basic values"
        value:string
    )

    (defschema wl-schema
        @doc "Basic schema used for WL members, keys are account ids"
        role:string
    )

    (defschema price-schema
        @doc "Prices schema"
        price:decimal
    )

    (defschema marketplace-schema
        @doc "Schema for marketplace information, ID is the nft id"
        id:string
        for-sale:bool
        price:decimal
        updated-at:time
        owner:string
    )

    (deftable nfts:{nft-main-schema})
    (deftable gen-0-adopt:{gen-0-adopt-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})
    (deftable wl:{wl-schema})
    (deftable price:{price-schema})
    (deftable marketplace:{marketplace-schema})

    ;;; STATE MODIFYING FUNCTIONS, REQUIRE CAPABILITIES OR ENFORCE USERS ;;;

    (defun put-id-for-sale (id:string price:decimal)
        @doc "Puts a kitty up for sale"
        (with-capability (OWNER id)
            (enforce (> price 0.0) "Price must be positive")
            (let* 
                (
                    (owner (at "owner" (get-kitty-fields-for-id ["owner"] id )))
                )
                (write marketplace id {
                    "id": id,
                    "for-sale": true, 
                    "price": price, 
                    "updated-at": (at "block-time" (chain-data)),
                    "owner": owner
                })
                (emit-event (PUT_ON_SALE id owner price))
            )
        )
    )

    (defun remove-id-from-sale (id:string)
        @doc "Removes a kitty up from selling"
        (with-capability (OWNER id)
            (let* 
                (
                    (owner (at "owner" (get-kitty-fields-for-id ["owner"] id )))
                )
                (write marketplace id {
                    "id": id,
                    "for-sale": false, 
                    "price": -1.0, 
                    "updated-at": (at "block-time" (chain-data)),
                    "owner": owner
                })
                (emit-event (REMOVED_FROM_SALE id owner))
            )
        )
    )

    (defun buy-id-on-sale (id:string curr-user:string)
        @doc "Buys a kitty that was put up for sale"
        (with-capability (ACCOUNT_GUARD curr-user)
            (enforce-kitty-on-sale id)
            (let* 
                (
                    (kitty-data (get-kitty-fields-for-id [] id ))
                    (original-owner (at "owner" kitty-data))
                    (price (at "price" (read marketplace id ["price"])))
                    (fee (get-market-fee-from-price price))
                    (to-seller-amount (get-to-seller-amount-from-price price))

                )
                (coin.transfer curr-user original-owner to-seller-amount)
                (coin.transfer curr-user ADMIN_ADDRESS fee)
                (write marketplace id {
                    "id": id,
                    "for-sale": false, 
                    "price": -1.0,  ; Invalid price so kitty can't be sold
                    "updated-at": (at "block-time" (chain-data)),
                    "owner": curr-user
                })
                (update nfts id (+ {"owner": curr-user} kitty-data ))
                (emit-event (BOUGHT id curr-user original-owner price))
            )
        )
    )

    (defun mint-kitty (id:string data:object)
        @doc "Mints a new kitty"
        (require-capability (PRIVATE))
        (let ((mint-chain-id (get-value MINT_CHAIN_ID_KEY)))
            (enforce (= (curr-chain-id) mint-chain-id) "Can only mint on specific chain")
        )
        (enforce-genes-and-items (at "gene-pairs" data) (at "item-pairs" data))
        (let ((id (id-for-new-kitty)))
            (insert nfts id data)
        )
        (increase-count KITTIES_CREATED_COUNT_KEY)
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

    (defun transfer:string
        ( id:string
          sender:string
          receiver:string
          amount:decimal
        )
        @doc " Transfer to an account, failing if the account to account does not exist. "
        (enforce (= 1.0 amount) "Only 1 kitty kad can be transferred")
        (enforce-account-exists receiver)
        ;  (enforce (= sender ADMIN_ADDRESS) "Can only send from admin account for now")
        ;  (with-capability (ADMIN)
        (with-capability (OWNER id)
            (update nfts id {"owner": receiver})
            (write marketplace id {
                "id": id,
                "for-sale": false, 
                "price": -1.0, 
                "updated-at": (at "block-time" (chain-data)),
                "owner": receiver
            })
        )
        ;  )
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

    (defun enforce-kitty-on-sale (id:string)
        @doc "Enforces the kitty for the ID is on sale"
        (let* 
            (
                ; Get the owner for the kitty
                (current-owner (at "owner" (get-kitty-fields-for-id ["owner"] id)))
                (marketplace-data (read marketplace id ["owner", "for-sale"]))
            )
            (enforce (= current-owner (at "owner" marketplace-data))
                "The person who is the current NFT owner isn't the one that put it on sale")
            (enforce (= true (at "for-sale" marketplace-data))
                "The nft is not listed for sale")
        )
    )

    (defun enforce-account-guard (account:string)
        @doc "Enforces the user passes the account guard check"
        (let ((user-guard (at "guard" (coin.details account))))
            (enforce-guard (= user-guard account) "account was not found")
        )
    )

    (defun enforce-genes-and-items (gene-pairs:list item-pairs:list)
        @doc "Enforces that the genes and items lengths are correct"
        (enforce (= (length gene-pairs) AMOUNT_OF_GENES) "Genes length is incorrect")
        (enforce (= (length item-pairs) AMOUNT_OF_ITEMS) "Items length is incorrect")
        (enforce-bits gene-pairs)
        (enforce-bits item-pairs)
    )

    (defun enforce-account-exists (account:string)
        @doc "Enforces that an account exists in the coin table"
        (let ((coin-account (at "account" (coin.details account))))
            (enforce (= coin-account account) "account was not found")
        )
    )

    (defun enforce-bits (pairs:list)
        @doc "Enforces that all the bits in the list are of valid size"
        (map (enforce-gene-length) pairs)
    )

    (defun enforce-gene-length (pair:list)
        @doc "Enforces that all the bits in the list are of valid size"
        (enforce (< (at 0 pair) MAX_GENE_SIZE) "A gene is larger than expected")
        (enforce (< (at 1 pair) MAX_GENE_SIZE) "A gene is larger than expected")

    )

    (defun kitties-owned-by (owner:string)
        @doc "All kitties owned by someone"
        (select nfts ["id"] (where "owner" (= owner)))
    )

    (defun ids-owned-by (owner:string)
        @doc "All kitties owned by someone, generic name to work with marketplaces"
        (kitties-owned-by owner)
    )


    (defun get-market-fee-from-price (price:decimal)
        @doc "Market fee cost for kitty sold at a given price"
        (* price MARKET_FEE)
    )

    (defun get-to-seller-amount-from-price (price:decimal)
        @doc "Amount that goes to a seller when kitty sold at a given price"
        (* price (- 1 MARKET_FEE))
    )

    (defun get-kitty-fields-for-ids (fields:list ids:list) 
        @doc "Return fields for a list of ids"
        (map 
            (get-kitty-fields-for-id fields)
            ids
        )
    )

    (defun get-nft-fields-for-ids (fields:list ids:list) 
        @doc "Market interface method for getting fields for a list of ids"
        (get-kitty-fields-for-ids fields ids)
    )

    (defun get-kitty-fields-for-id (fields:list id:string )
        @doc "Return the fields for a given id"
        (if 
            (> (length fields) 0)
            (+ {"id": id} (read nfts id fields))
            (read nfts id)
        )
    )

    (defun get-nft-fields-for-id (fields:list id:string) 
        @doc "Market interface method for getting fields for an id"
        (get-kitty-fields-for-id fields id)
    )

    (defun uri:string (id:string)
        @doc
        " Give URI for ID. If not supported, return \"\" (empty string)."
        (+ (+ (get-value KITTIES_URI_KEY) id) ".png")
    )

    (defun id-for-new-kitty ()
        @doc "Returns an id for a new kitty to be minted"
        (+ (+ (curr-chain-id) ":") (int-to-str 10 (get-count KITTIES_CREATED_COUNT_KEY)))
    )

    (defun all-kitties ()
        @doc "Returns all the ids"
        (keys nfts)
    )


    (defun all-ids ()
        @doc "Returns all the ids (different name to be compatible with bulls)"
        (all-kitties)
    )

    (defun get-marketplace-fields-for-ids (fields:list ids:list) 
        @doc "Return a list of marketplace fields for ids"
        (map 
            (get-marketplace-fields-for-id fields)
            ids
        )
    )

    (defun get-marketplace-fields-for-id (fields:list id:string )
        @doc "Return the fields for a given id"
        (if 
            (> (length fields) 0)
            (+ {"id": id} (read marketplace id fields))
            (read marketplace id)
        )
    )


    (defun get-all-on-sale ()
        @doc "Returns all ids on sale"
        (select marketplace (where "for-sale" (= true)))
    )

    (defun get-names-for-ids (ids:list)
        @doc "Returns user friendly names for ids"
        (map (get-name-for-id) ids)
    )

    (defun get-name-for-id (id:string)
        @doc "Returns the user friendly for ids"
        ; Drop the first two values (i.e. the 1: from the ids)
        ; Convert to int, add one, then back to string
        (int-to-str 10 (+ 1 (str-to-int (drop 2 id))))
    )

    ;;;;;; GENERIC HELPER FUNCTIONS ;;;;;;;;;;

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )
)

;  (create-table nfts)
;  (create-table gen-0-adopt)
;  (create-table counts)
;  (create-table values)
;  (create-table wl)
;  (create-table price)
;  (create-table marketplace)
;  (initialize)
