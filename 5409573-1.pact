(module KadenaZombies 'kadena-zombies                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      'project-kz
  
  
    (defconst KZ_CREATED_COUNT_KEY "kz-count-key")
    (defconst KZ_MINTED_COUNT_KEY "kz-minted-count-key")
    (defconst KZ_CREATED_TO_MINT_COUNT_KEY "kz-created-to-mint-count-key")
    (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
    (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
    (defconst CURR_WL_ROLE_KEY "curr-wl-role-key")
    (defconst WL_ROLE "whitelist")
    (defconst PRICE_KEY "price-key")
    (defconst ADMIN_KEYSET (read-keyset 'kadena-zombies))
    (defconst ADMIN_ADDRESS "k:38f1438799eb096df315d670af49c2c73ca4ab7afd6725ff7f29b6f51be8cca5")
    (defconst KZ_URI_KEY "https://kadenazombies.com/")
  
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

    (defcap OWNER (account:string id:string)
        @doc "Enforces that an account owns a Kadena Zombie"
        (let 
            (
                (nft-owner (at "owner" (read minted-nfts id ["owner"])))
            )
            (enforce (= nft-owner account) "Account is not owner of the NFT")
            (compose-capability (ACCOUNT_GUARD account))
        )
    )

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset  ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap KZ_LIST (id:string owner:string price:decimal for_sale:string)
      @doc "Emitted event when a Kadena Zombie is sold "
      @event true
    )

    (defcap KZ_BUY (id:string buyer:string seller:string price:decimal)
      @doc "Emitted event when a Kadena Zombie is purchased "
      @event true
    )

    (defun initialize ()
        @doc "Initialize the contract the first time its loaded "
        (insert counts KZ_CREATED_COUNT_KEY {"count": 0})
        (insert counts KZ_MINTED_COUNT_KEY {"count": 0})
        (insert counts KZ_CREATED_TO_MINT_COUNT_KEY {"count": 0})
        (insert counts TOTAL_VOLUME_KEY {"count": 0})
        (insert values MINT_CHAIN_ID_KEY {"value": "1"})
        (insert price PRICE_KEY {"price": 20.0})
        (insert values CURR_WL_ROLE_KEY {"value": WL_ROLE})
    )

    ;;;; SCHEMAS AND TABLES ;;;;;

    (defschema nft-main-schema
        @doc "Stores core information about each nft"
        id:string
        date_minted:time
        owner:string
        traits:object
        price:decimal
        for_sale:string
        creators:list
        updated_at:time
        seller_fee_basis_points:integer
    )

    (defschema not-minted-nfts-schema
        @doc "Gen 0s for mint traits"
        traits:object
        minted:bool
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

    (deftable minted-nfts:{nft-main-schema})
    (deftable not-minted-nfts:{not-minted-nfts-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})
    (deftable wl:{wl-schema})
    (deftable price:{price-schema})

    ;;; STATE MODIFYING FUNCTIONS, REQUIRE CAPABILITIES ;;;

    (defun create-multiple-kz-for-mint (object-list:list)
        @doc "Take a list of traits, create kzs for mints"
        (with-capability (ADMIN)
        (map 
                (create-kz-for-mint)
                object-list
            )
        )
    )

    (defun create-kz-for-mint (traits-list:object)
         (require-capability (ADMIN))
        (let 
            ((id (int-to-str 10(get-count KZ_CREATED_TO_MINT_COUNT_KEY))))
            (insert not-minted-nfts id
                {"traits": (at "traits" traits-list),
                 "minted": false
                }
            )
        )(with-capability (PRIVATE)
        (increase-count KZ_CREATED_TO_MINT_COUNT_KEY 1)
    )
)
    (defun buy-kzs-bulk (owner:string amount:integer)
        @doc "Buys a Kadena Zombie"
        (enforce-mint-wl-role owner)
       (if 
            (!= owner ADMIN_ADDRESS)
            (coin.transfer owner ADMIN_ADDRESS (* (get-price) amount))
            "Admin account"
        )
         (with-capability (ACCOUNT_GUARD owner)
            (with-capability (PRIVATE)
                (map
                    (buy-kz owner) 
                    (make-list amount 1)
                )
            )
         )
    )

    (defun buy-kz (owner:string number:integer)
        @doc "Buys a Kadena Zombie"
        (require-capability (PRIVATE))
         (require-capability (ACCOUNT_GUARD owner))
    (let (
                    (id (id-for-new-kz))
                )
                (mint-kz id {
                    "id": id,
                    "traits": (read-table-traits id),
                    "date_minted":  (at "block-time" (chain-data)),
                    "owner": owner,
                    "for_sale": "false",
                    "price": 0.0,
                    "creators": [{ address: ADMIN_ADDRESS, share: 100 }],
                    "updated_at": (at "block-time" (chain-data)),
                    "seller_fee_basis_points": 500
                })
            
        )
        (increase-count KZ_MINTED_COUNT_KEY 1)
    )

    (defun mint-kz (id:string data:object)
        @doc "Mints a new Kadena Zombie"
        (require-capability (PRIVATE))
        
        (insert minted-nfts id data)

        (increase-count KZ_CREATED_COUNT_KEY 1)
    )

    (defun increase-count(key:string amount:integer)
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

    (defun transfer:string
        (id:string
          sender:string
          receiver:string
          amount:decimal
        )
        @doc " Transfer to an account, failing if the account to account does not exist. "
         (enforce (= 1.0 amount) "Only 1 Kadena Zombie can be transferred")
        (enforce-account-exists receiver)
         (enforce (= sender ADMIN_ADDRESS) "Can only send from admin account for now")
        (with-capability (ADMIN)
             (with-capability (OWNER sender id)
                (update minted-nfts id {"owner": receiver})
             )
        )
    )

    ; (defun list-my-kz-on-market(id:string owner:string price:decimal for_sale:string)
    ;     @doc "Put a Kadena Zombie on the Market or update one already for sale "
    ;     (with-read minted-nfts id
    ;       { 'id := l_id, 
    ;       'owner := l_owner }
    ;       ;Enforce rules
    ;       (with-capability (ACCOUNT_GUARD l_owner)
    ;         (enforce (= owner l_owner) "Account Owners dont match.")
    ;         (enforce (>= price 0.0)  "Positive decimal sell prices only." )
    ;         (update minted-nfts id 
    ;             { 
    ;                 "for_sale": for_sale,
    ;                 "price": price,
    ;                 "updated_at": (at "block-time" (chain-data))
    ;             }
    ;         )
    ;         (emit-event (KZ_LIST id owner price for_sale))
    ;         (if (= for_sale "true") (format "Kadena Zombie with ID {} is now for sale for {}" [id price]) (format "Kadena Zombie with ID {} is no longer for sale" [id]))
    ;       )
    ;     )
    ; )
    
    ; (defun buy-kz-off-market
    ;     ( id:string
    ;       buyer:string
    ;       price:decimal )
    ;     @doc " Buy a Kadena Zombie off the Market "
    ;     (with-read minted-nfts id
    ;       { 'id := m_id, 
    ;       'owner := seller,
    ;       'for_sale := m_for_sale,
    ;       'price := m_price }
    ;         (enforce (= m_for_sale "true")  "You can only purchase a Kadena Zombie that is for sale." )
    ;         (enforce (= price m_price) "Insufficient funds.")
    ;         (enforce (!= buyer seller) "You cannot buy your own Kadena Zombie.")
    ;         (enforce (= "k:" (take 2 buyer)) "Only k: Accounts.")
    ;         (coin.transfer buyer seller (round (* 0.97 m_price) 2))
    ;         (coin.transfer buyer ADMIN_ADDRESS (round (* 0.05 m_price) 2))
    ;         (with-capability (PRIVATE)
    ;             (increase-count TOTAL_VOLUME_KEY m_price)
    ;         )
    ;         (update minted-nfts id 
    ;             {
    ;                 "owner": buyer,
    ;                 "for_sale": "false",
    ;                 "price": 0.0,
    ;                 "updated_at": (at "block-time" (chain-data))
    ;             }
    ;         )
    ;         (emit-event (KZ_BUY id buyer seller price))
    ;         (format "Purchased a Kadena Zombie with the ID {} for {} KDA " [id price])
    ;     )
    ; )

    ;;;;;; Kadena ZombieS NON STATE MODIFYING HELPER FUNCTIONS ;;;;;;;;;

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

    (defun get-latest-kz-to-mint-data ()
         (let 
             (
                 (minted-count (get-count KZ_MINTED_COUNT_KEY))
                 (created-to-mint-count (get-count KZ_CREATED_TO_MINT_COUNT_KEY))
             )
             (enforce (< 0 created-to-mint-count) "No kzs have been put up for mint")
             (enforce 
                 (< minted-count created-to-mint-count)
                  "All Kadena Zombies put up for mint have already been minted, please check later"
             )
            
    
         )
       )

    (defun enforce-mint-wl-role (account:string)
        @doc "Enforce the account has a role that allows them to mint kzs"
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
                (owned-count (length (kzs-owned-by account)))
            )
            (enforce (<= (+ owned-count amount) max) "You have minted your max for whitelist round")
        )

    )

    (defun kzs-owned-by (owner:string)
        @doc "All kzs owned by someone"
        (select minted-nfts ["id"] (where "owner" (= owner)))
    )

    (defun get-kz-fields-for-ids (fields:list ids:list) 
        @doc "Return fields for a list of ids"
        (map 
            (get-kz-fields-for-id fields)
            ids
        )
    )

    (defun get-kz-fields-for-id (fields:list id:string )
        @doc "Return the fields for a given id"
        (+ {"id": id} (read minted-nfts id fields))
    )

    (defun id-for-new-kz ()
        @doc "Returns an id for a new Kadena Zombie to be minted"
        (int-to-str 10 (get-count KZ_CREATED_COUNT_KEY))
    )

    (defun all-minted-kzs ()
        @doc "Returns all the ids"
        (with-capability (ADMIN)
            (keys minted-nfts)
        )
    )

    (defun uri:string (id:string)
        @doc
        " Give URI for ID. If not supported, return \"\" (empty string)."
        (+ (get-value KZ_URI_KEY) 
        (+ id ".gif")
        )
        )

    (defun all-kzs ()
        @doc "Returns all the ids"
        (with-capability (ADMIN)
            (keys not-minted-nfts)
        )
    )

    ;;;;;; GENERIC HELPER FUNCTIONS ;;;;;;;;;;

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )

    (defun read-table-traits(id)
        (at "traits" (read not-minted-nfts id ['traits 'minted] ))
    )

    (defun patch-count (num:integer key:string)
        (with-capability (ADMIN)
            (update counts key {"count": num})
        )
    )

    (defun backfill-trait (traits-list:list)
        @doc "Backfills table"
        (with-capability (ADMIN)
            (map 
                (backfill-traits)
                traits-list
            )
        )
    )

    (defun backfill-traits (traits:string)
        (require-capability (ADMIN))
        (update minted-nfts traits 
            {
                "updated_at": (at "block-time" (chain-data))
            }
        )
    )

)


