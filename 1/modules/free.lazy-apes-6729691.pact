(module lazy-apes GOV
  "Lazy Apes Mint"

    (defconst LAZY_APES_CREATED_COUNT_KEY "lazy-apes-count-key")
    (defconst LAZY_APES_MINTED_COUNT_KEY "lazy-apes-minted-count-key")
    (defconst LAZY_APES_CREATED_TO_MINT_COUNT_KEY "lazy-apes-created-to-mint-count-key")
    (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
    (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
    (defconst CURR_WL_ROLE_KEY "curr-wl-role-key")
    (defconst WL_ROLE "whitelist")
    (defconst FREE_MINT_ROLE "free-mint")
    (defconst MAX_WL_AMOUNT 10)
    (defconst PRICE_KEY "price-key")
    (defconst ADMIN_KEYSET "free.admin-arkade")
    (defconst ADMIN_ADDRESS "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec")
    (defconst CREATOR_ADDRESS "k:bb7cf7ab6508ede363f7a713f6a9b80baa74eba858c47cfed70e42751cf0f172")
    (defconst ROYALTY_FEE 0.10)

    (defcap GOV () 
        (enforce-keyset ADMIN_KEYSET)
    ) 

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
        @doc "Enforces that an account owns a lazy ape"
        (let 
            (
                (nft-owner (at "owner" (read minted-nfts id ["owner"])))
            )
            ; (enforce (= nft-owner account) "Account is not owner of the NFT")
            (compose-capability (ACCOUNT_GUARD nft-owner))
        )
    )

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset  ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap PUT_ON_SALE (id:string owner:string price:decimal)
      @doc "Emitted event when an NFT is put on sale "
      @event true
    )

    (defcap REMOVED_FROM_SALE (id:string owner:string)
      @doc "Emitted event when an NFT is removed from sale "
      @event true
    )

    (defcap BOUGHT (id:string new-owner:string original-owner:string price:decimal)
        @doc "Emitted event when an NFT is removed from sale "
        @event true
    )

    (defun initialize ()
        @doc "Initialize the contract the first time its loaded "
        (insert counts LAZY_APES_CREATED_COUNT_KEY {"count": 0.0})
        (insert counts LAZY_APES_MINTED_COUNT_KEY {"count": 0.0})
        (insert counts LAZY_APES_CREATED_TO_MINT_COUNT_KEY {"count": 0.0})
        (insert counts TOTAL_VOLUME_KEY {"count": 0.0})
        (insert values MINT_CHAIN_ID_KEY {"value": "1"})
        (insert price PRICE_KEY {"price": 5.0})
        (insert values CURR_WL_ROLE_KEY {"value": WL_ROLE})
    )

    ;;;; SCHEMAS AND TABLES ;;;;;

    (defschema nft-main-schema
        @doc "Stores core information about each nft"
        id:string
        date_minted:time
        owner:string
        item:object
        price:decimal
        for_sale:string
        creators:list
        updated_at:time
        seller_fee_basis_points:integer
    )

    (defschema not-minted-nfts-schema
        @doc "Gen 0s for mint traits"
        item:object
    )


    (defschema marketplace-schema
        @doc "Schema for marketplace information, ID is the nft id"
        id:string
        for-sale:bool
        price:decimal
        updated-at:time
        owner:string
    )

    (defschema counts-schema
        @doc "Basic schema used for counting things"
        count:decimal
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

    (deftable minted-nfts:{nft-main-schema})
    (deftable not-minted-nfts:{not-minted-nfts-schema})
    (deftable new-not-minted-nfts:{not-minted-nfts-schema})
    (deftable marketplace:{marketplace-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})
    (deftable wl:{wl-schema})
    (deftable price:{price-schema})

    ;;; STATE MODIFYING FUNCTIONS, REQUIRE CAPABILITIES ;;;

    (defun create-multiple-lazy-ape-for-mint (items:list)
        @doc "Take a list of items, create lazy-apes for mints"
        (with-capability (ADMIN)
            (map 
                (create-lazy-ape-for-mint)
                items
            )
        )
    )

    (defun create-lazy-ape-for-mint (item:object)
        (require-capability (ADMIN))
        (let 
            ((id (int-to-str 10 (at "edition" (at "item" item)))))
            (insert new-not-minted-nfts id
                item
            )
        )
        (increase-count LAZY_APES_CREATED_TO_MINT_COUNT_KEY 1.0)
    )

    ; (defun buy-lazy-apes-bulk (owner:string amount:integer)
    ;     @doc "Buys a lazy ape"
    ;     ; (enforce-mint-wl-role owner)
    ;     ; (enforce-max-wl-mint owner amount MAX_WL_AMOUNT)
    ;     (coin.transfer owner CREATOR_ADDRESS (* 0.93 (* (get-price) amount)))
    ;     (coin.transfer owner ADMIN_ADDRESS (* 0.07 (* (get-price) amount)))
    ;     (with-capability (ACCOUNT_GUARD owner)
    ;         (with-capability (PRIVATE)
    ;             (map
    ;                 (buy-lazy-ape owner) 
    ;                 (make-list amount 1)
    ;             )
    ;         )
    ;     )
    ; )

    (defun buy-lazy-ape (owner:string number:integer)
        @doc "Buys a lazy ape"
        (require-capability (PRIVATE))
        (require-capability (ACCOUNT_GUARD owner))
        (let (
                (data (get-latest-lazy-ape-to-mint-data) )
            )
            (mint-lazy-ape (int-to-str 10 (at "edition" (read-table-item data))) {
                "id": (int-to-str 10 (at "edition" (read-table-item data))),
                "item": (read-table-item data),
                "date_minted": (at "block-time" (chain-data)),
                "owner": owner,
                "for_sale": "false",
                "price": 0.0,
                "creators": [{ address: CREATOR_ADDRESS, share: 100 }],
                "updated_at": (at "block-time" (chain-data)),
                "seller_fee_basis_points": 500
            })
        )
        (increase-count LAZY_APES_MINTED_COUNT_KEY 1.0)
    )

    (defun mint-lazy-ape (id:string data:object)
        @doc "Mints a new lazy ape"
        (require-capability (PRIVATE))
        
        (insert minted-nfts id data)

        (increase-count LAZY_APES_CREATED_COUNT_KEY 1.0)
    )

    (defun giveaways (owner:string amount:integer)
        @doc "Mints a new lazy ape as admin for giveaways"
        (with-capability (ADMIN)
            (with-capability (ACCOUNT_GUARD owner)
            (with-capability (PRIVATE)
                (map
                    (buy-lazy-ape owner) 
                    (make-list amount 1)
                )
            )
            )
        )
        
    )

    (defun increase-count(key:string amount:decimal)
        @doc "Increases count of a key in a table by amount"
        (require-capability (PRIVATE))
        (update counts key 
            {"count": (+ amount (get-count key))} 
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

    (defun add-to-wl-bulk (role:string accounts:list)
        @doc "Adds wl users with a role"
        (with-capability (ADMIN)
            (map (add-to-wl role) accounts)
        )
    )

    (defun add-to-wl (role:string account:string)
        @doc "Adds a user to a wl"
        (require-capability (ADMIN))
        (insert wl account {"role": role})
    )

    (defun update-user-wl-role (role:string account:string)
        @doc "Updates a user's wl role"
        (with-capability (ADMIN)
            (update wl account {"role": role})
        )
    )

    (defun set-price(price-value:decimal)
        @doc "Set the price"
        (with-capability (ADMIN)
            (update price PRICE_KEY {"price": price-value})
        )
    )

    ;;;;;; MARKETPLACE FUNCTIONS ;;;;;;

    (defun transfer-bulk:string
        (ids:list
          sender:string
          receiver:string
        )
        @doc "(Admin only) Transfer multiple NFTs to an account."
        (with-capability (ADMIN)
            (map 
                (transfer sender receiver)
                ids
            )
        )
    )

    (defun transfer:string (receiver:string id:string)
        @doc "Transfer an NFT to an account."
        (enforce-account-exists receiver)
        (with-capability (OWNER id)
            (write marketplace id {
                        "id": id,
                        "for-sale": false, 
                        "price": -1.0, 
                        "updated-at": (at "block-time" (chain-data)),
                        "owner": receiver
            })
            (update minted-nfts id {"owner": receiver})
        )
    )

    (defun put-id-for-sale(id:string price:decimal)
        @doc "Puts an NFT up for sale"
        (with-capability (OWNER id)
            (enforce (> price 0.0) "Price must be positive")
            (let* 
                (
                    (owner (at "owner" (get-nft-fields-for-id ["owner"] id )))
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
        @doc "Removes an NFT from sale"
        (with-capability (OWNER id)
            (let* 
                (
                    (owner (at "owner" (get-nft-fields-for-id ["owner"] id )))
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
        @doc "Buys an NFT that was put up for sale"
        (with-capability (ACCOUNT_GUARD curr-user)
            (enforce-id-on-sale id)
            (let* 
                (
                    (nft-data (get-nft-fields-for-id [] id ))
                    (original-owner (at "owner" nft-data))
                    (price (at "price" (read marketplace id ["price"])))
                    (fee (get-market-fee-from-price price))
                    (to-seller-amount (get-to-seller-amount-from-price price))

                )
                (coin.transfer curr-user original-owner to-seller-amount)
                (coin.transfer curr-user CREATOR_ADDRESS fee)
                (write marketplace id {
                    "id": id,
                    "for-sale": false, 
                    "price": -1.0,  ; Invalid price so NFT can't be sold
                    "updated-at": (at "block-time" (chain-data)),
                    "owner": curr-user
                })
                (update minted-nfts id (+ {"owner": curr-user} nft-data ))
                (with-capability (PRIVATE)
                    (increase-count TOTAL_VOLUME_KEY price)
                )
                (emit-event (BOUGHT id curr-user original-owner price))
            )
        )
    )

    (defun all-ids ()
        @doc "Returns all the ids"
        (keys minted-nfts)
    )

    (defun get-market-fee-from-price (price:decimal)
        @doc "Market fee cost for id sold at a given price"
        (* price ROYALTY_FEE)
    )

    (defun get-to-seller-amount-from-price (price:decimal)
        @doc "Amount that goes to a seller when nft sold at a given price"
        (* price (- 1 ROYALTY_FEE))
    )

    (defun get-marketplace-fields-for-ids (fields:list ids:list) 
        @doc "Return fields for a list of ids"
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
        @doc "Returns all items on sale"
        (select marketplace ["id", "price", "updated-at"] (where "for-sale" (= true)))
    )

    (defun get-owner-items-on-sale (owner:string)
        @doc "Returns a specific owner's items on sale"
        (select marketplace ["id", "price", "updated-at", "owner"] (and? (where "for-sale" (= true)) (where "owner" (= owner))))
    )

    ;;;;;; LAZY APES NON STATE MODIFYING HELPER FUNCTIONS ;;;;;;;;;

    (defun get-price()
        (at "price" (read price PRICE_KEY ["price"]))
    )

    (defun get-wl-role (account:string)
        @doc "Gets current wl role for  the user"
        (try
            ""
            (at "role" (read wl account ["role"]))
        )
    )

    (defun get-count (key:string)
        @doc "Gets count for key"
        (at "count" (read counts key ['count]))
    )

    (defun get-value (key:string)
        @doc "Gets value for a key"
        (at "value" (read values key ['value]))
    )

    (defun get-latest-lazy-ape-to-mint-data ()
        (let 
            (
                (minted-count (get-count LAZY_APES_MINTED_COUNT_KEY))
                (created-to-mint-count (get-count LAZY_APES_CREATED_TO_MINT_COUNT_KEY))
            )
            (enforce (< 0.0 created-to-mint-count) "No lazy-apes have been put up for mint")
            (enforce 
                (< minted-count created-to-mint-count)
                 "All lazy apes put up for mint have already been minted, please check later"
            )
            (let 
                (
                  (data (at (- (floor minted-count) 454) (keys new-not-minted-nfts)))
                )
                data
            )
        )
    )

    (defun enforce-mint-wl-role (account:string)
        @doc "Enforce the account has a role that allows them to mint lazy-apes"
        (let 
            (
                (curr_wl_role (get-value CURR_WL_ROLE_KEY))
                (user_wl_role (get-wl-role account))
            )
            (if 
                (= curr_wl_role WL_ROLE)
                (enforce (= user_wl_role WL_ROLE) "Only premium WL members allowed")
                true
            )
        )    
    )

    (defun enforce-account-exists (account:string)
        @doc "Enforces that an account exists in the coin table"
        (let ((coin-account (at "account" (coin.details account))))
            (enforce (= coin-account account) "account was not found")
        )
    )

    (defun enforce-max-wl-mint (account:string amount:integer max:integer)
        @doc "Enforces wl member only mints max amount"
        (let 
            (
                (owned-count (length (ids-owned-by account)))
            )
            (enforce (<= (+ owned-count amount) max) "You have minted your max for whitelist round")
        )

    )

    (defun enforce-id-on-sale (id:string)
        @doc "Enforces the NFT for the ID is on sale"
        (let* 
            (
                ; Get the owner for the id
                (current-owner (at "owner" (get-nft-fields-for-id ["owner"] id)))
                (marketplace-data (read marketplace id ["owner", "for-sale"]))
            )
            (enforce (= current-owner (at "owner" marketplace-data))
                "The person who is the current NFT owner isn't the one that put it on sale")
            (enforce (= true (at "for-sale" marketplace-data))
                "The nft is not listed for sale")
        )
    )

    (defun ids-owned-by (owner:string)
        @doc "All lazy-apes owned by someone"
        (select minted-nfts ["id"] (where "owner" (= owner)))
    )

    (defun get-nft-fields-for-ids (fields:list ids:list) 
        @doc "Return fields for a list of ids"
        (map 
            (get-nft-fields-for-id fields)
            ids
        )
    )

    (defun get-nft-fields-for-id (fields:list id:string )
        @doc "Return the fields for a given id"
        (+ {"id": id} (read minted-nfts id fields))
    )

    (defun id-for-new-lazy-ape ()
        @doc "Returns an id for a new lazy ape to be minted"
        (int-to-str 10 (floor (get-count LAZY_APES_CREATED_COUNT_KEY)))
    )


    (defun all-lazy-apes ()
        @doc "Returns all the ids"
        (with-capability (ADMIN)
            (keys not-minted-nfts)
        )
    )

    (defun new-all-lazy-apes ()
        @doc "Returns all the ids"
        (with-capability (ADMIN)
            (keys new-not-minted-nfts)
        )
    )

    ;;;;;; GENERIC HELPER FUNCTIONS ;;;;;;;;;;

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )

    (defun read-table-item(id)
        (at "item" (read new-not-minted-nfts id ['item] ))
    )

    (defun patch-count (num:integer key:string)
        (with-capability (ADMIN)
            (update counts key {"count": num})
        )
    )

    (defun backfill-items (items:list)
        @doc "Backfills table"
        (with-capability (ADMIN)
            (map 
                (backfill-item)
                items
            )
        )
    )

    (defun backfill-item (item:string)
        (require-capability (ADMIN))
        (update minted-nfts item 
            {
                "updated_at": (at "block-time" (chain-data))
            }
        )
    )

)

;  (create-table minted-nfts)
;  (create-table not-minted-nfts)
 
