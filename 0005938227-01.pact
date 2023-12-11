(module komodos GOV
  "Komodos Mint"

    (defconst KOMODOS_CREATED_COUNT_KEY "komodos-count-key")
    (defconst KOMODOS_MINTED_COUNT_KEY "komodos-minted-count-key")
    (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
    (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
    (defconst CURR_WL_ROLE_KEY "curr-wl-role-key")
    (defconst WL_ROLE "whitelist")
    (defconst FREE_MINT_ROLE "free-mint")
    (defconst MAX_WL_AMOUNT 10)
    (defconst STATUS_KEY "status-key")
    (defconst PRICE_KEY "price-key")
    (defconst ADMIN_KEYSET "free.admin-arkade")
    (defconst ADMIN_ADDRESS "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec")
    (defconst CREATOR_ADDRESS "k:4983f6d4168234fd89434ff62aecff45033a58e39c710d259f0836a786bc7d15")
    (defconst ROYALTY_FEE 0.05)

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
        @doc "Enforces that an account owns a komodo"
        (let 
            (
                (nft-owner (at "owner" (read nfts id ["owner"])))
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
        (insert counts KOMODOS_CREATED_COUNT_KEY {"count": 1500.0})
        (insert counts KOMODOS_MINTED_COUNT_KEY {"count": 0.0})
        (insert counts TOTAL_VOLUME_KEY {"count": 0.0})
        (insert values MINT_CHAIN_ID_KEY {"value": "1"})
        (insert price PRICE_KEY {"price": 1.0})
        (insert status STATUS_KEY {"status": "pending"})
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

    (defschema status-schema
        @doc "Status schema"
        status:string
    )

    (deftable nfts:{nft-main-schema})
    (deftable marketplace:{marketplace-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})
    (deftable wl:{wl-schema})
    (deftable price:{price-schema})
    (deftable status:{status-schema})

    ;;; STATE MODIFYING FUNCTIONS, REQUIRE CAPABILITIES ;;;

    ; (defun buy-komodos-bulk (owner:string amount:integer)
    ;     @doc "Buys a komodo"
    ;     ; (enforce (= (get-status) "live") "Mint is not live yet")
    ;     ; (enforce-mint-wl-role owner)
    ;     ; (enforce-max-wl-mint owner amount MAX_WL_AMOUNT)
    ;     (coin.transfer owner PROJECT_ADDRESS (* 0.93 (* (get-price) amount)))
    ;     (coin.transfer owner ADMIN_ADDRESS (* 0.07 (* (get-price) amount)))
    ;     (with-capability (ACCOUNT_GUARD owner)
    ;         (with-capability (PRIVATE)
    ;             (map
    ;                 (buy-komodo owner) 
    ;                 (make-list amount 1)
    ;             )
    ;         )
    ;     )
    ; )

    (defun buy-komodo (owner:string number:integer)
        @doc "Buys a komodo"
        (require-capability (PRIVATE))
        (require-capability (ACCOUNT_GUARD owner))
        (let (
                (id (get-latest-komodo-to-mint))
            )
            (mint-komodo id {
                "id": id,
                "item": { "edition": id },
                "date_minted": (at "block-time" (chain-data)),
                "owner": owner,
                "for_sale": "false",
                "price": 0.0,
                "creators": [{ address: PROJECT_ADDRESS, share: 100 }],
                "updated_at": (at "block-time" (chain-data)),
                "seller_fee_basis_points": 500
            })
        )
        (increase-count KOMODOS_MINTED_COUNT_KEY 1.0)
    )

    (defun mint-komodo (id:string data:object)
        @doc "Mints a new komodo"
        (require-capability (PRIVATE))
        
        (insert nfts id data)

        (increase-count KOMODOS_CREATED_COUNT_KEY 1.0)
    )

    (defun giveaways (owner:string amount:integer)
        @doc "Mints a new komodo as admin for giveaways"
        (with-capability (ADMIN)
            (with-capability (ACCOUNT_GUARD owner)
            (with-capability (PRIVATE)
                (map
                    (buy-komodo owner) 
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

    (defun set-status(status-value:string)
        @doc "Set the sale status"
        (with-capability (ADMIN)
            (update status STATUS_KEY {"status": status-value})
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
            (update nfts id {"owner": receiver})
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
                (update nfts id (+ {"owner": curr-user} nft-data ))
                (with-capability (PRIVATE)
                    (increase-count TOTAL_VOLUME_KEY price)
                )
                (emit-event (BOUGHT id curr-user original-owner price))
            )
        )
    )

    (defun all-ids ()
        @doc "Returns all the ids"
        (keys nfts)
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


    ;;;;;; KOMODOS NON STATE MODIFYING HELPER FUNCTIONS ;;;;;;;;;

    (defun get-price()
        (at "price" (read price PRICE_KEY ["price"]))
    )

    (defun get-status()
        (at "status" (read status STATUS_KEY ["status"]))
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

    (defun get-latest-komodo-to-mint ()
        (let 
            (
                (minted-count (get-count KOMODOS_MINTED_COUNT_KEY))
                (created-count (get-count KOMODOS_CREATED_COUNT_KEY))
            )
            (enforce (< 0.0 created-count) "No komodos have been put up for mint")
            (enforce 
                (< minted-count created-count)
                 "All komodos put up for mint have already been minted, please check later"
            )
            (let 
                (
                  (id (int-to-str 10 (+ (floor minted-count) 1)))
                )
                id
            )
        )
    )

    (defun enforce-mint-wl-role (account:string)
        @doc "Enforce the account has a role that allows them to mint komodos"
        (let 
            (
                (curr_wl_role (get-value CURR_WL_ROLE_KEY))
                (user_wl_role (get-wl-role account))
            )
            (if 
                (= curr_wl_role WL_ROLE)
                (enforce (= user_wl_role WL_ROLE) "Only whitelist members allowed")
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
                (owned-count (length (komodos-owned-by account)))
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

    (defun komodos-owned-by (owner:string)
        @doc "All komodos owned by someone"
        (select nfts ["id"] (where "owner" (= owner)))
    )

    (defun get-komodo-fields-for-ids (fields:list ids:list) 
        @doc "Return fields for a list of ids"
        (map 
            (get-komodo-fields-for-id fields)
            ids
        )
    )

    (defun get-komodo-fields-for-id (fields:list id:string )
        @doc "Return the fields for a given id"
        (+ {"id": id} (read nfts id fields))
    )

    (defun id-for-new-komodo ()
        @doc "Returns an id for a new komodo to be minted"
        (int-to-str 10 (floor (get-count KOMODOS_CREATED_COUNT_KEY)))
    )

    (defun all-minted-komodos ()
        @doc "Returns all the ids"
        (with-capability (ADMIN)
            (keys nfts)
        )
    )

    (defun ids-owned-by (owner:string)
        @doc "All ids owned by someone"
        (select nfts ["id"] (where "owner" (= owner)))
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
        (+ {"id": id} (read nfts id fields))
    )

    (defun all-nfts ()
        @doc "Returns all the ids"
        (with-capability (ADMIN)
            (keys nfts)
        )
    )

    ;;;;;; GENERIC HELPER FUNCTIONS ;;;;;;;;;;

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
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
        (update nfts item 
            {
                "updated_at": (at "block-time" (chain-data))
            }
        )
    )

)

;  (create-table nfts)
 
