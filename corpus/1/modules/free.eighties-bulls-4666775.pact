(module eighties-bulls 'bulls-keyset
  "80s Bulls"

    (defconst BULLS_CREATED_COUNT_KEY "bulls-count-key")
    (defconst BULLS_MINTED_COUNT_KEY "bulls-minted-count-key")
    (defconst BULLS_CREATED_TO_MINT_COUNT_KEY "bulls-created-to-mint-count-key")
    (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
    (defconst CURR_WL_ROLE_KEY "curr-wl-role-key")
    (defconst WL_PREMIUM_ROLE "whitelist")
    (defconst PRICE_KEY "price-key")
    (defconst ADMIN_KEYSET (read-keyset 'bulls-keyset))
    (defconst ADMIN_ADDRESS "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec")

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
        @doc "Enforces that an account owns a pixel bull"
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

    (defun initialize ()
        @doc "Initialize the contract the first time its loaded "
        (insert counts BULLS_CREATED_COUNT_KEY {"count": 0})
        (insert counts BULLS_MINTED_COUNT_KEY {"count": 0})
        (insert counts BULLS_CREATED_TO_MINT_COUNT_KEY {"count": 0})
        (insert values MINT_CHAIN_ID_KEY {"value": "1"})
        (insert price PRICE_KEY {"price": 20.0})
        (insert values CURR_WL_ROLE_KEY {"value": WL_PREMIUM_ROLE})
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
    )

    (defschema not-minted-nfts-schema
        @doc "Gen 0s for mint traits"
        item:object
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

    (defschema stats-schema
        @doc "Prices schema"
        total_volume:decimal
    )

    (deftable minted-nfts:{nft-main-schema})
    (deftable not-minted-nfts:{not-minted-nfts-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})
    (deftable wl:{wl-schema})
    (deftable price:{price-schema})
    (deftable stats:{stats-schema})


    ;;; STATE MODIFYING FUNCTIONS, REQUIRE CAPABILITIES ;;;

    (defun create-multiple-80s-bull-for-sale (items:list)
        @doc "Take a list of items, create 80s bulls for mints"
        (with-capability (ADMIN)
            (map 
                (create-80s-bull-for-sale)
                items
            )
        )
    )

    (defun create-80s-bull-for-sale (item:object)
        (require-capability (ADMIN))
        (let 
            ((id (int-to-str 10 (at "edition" (at "item" item)))))
            (insert not-minted-nfts id
                item
            )
        )
        (increase-count BULLS_CREATED_TO_MINT_COUNT_KEY)
    )

    ; (defun buy-80s-bulls-bulk (owner:string amount:integer )
    ;     @doc "Buys a 80s bull"
    ;     ; (enforce-mint-wl-role owner)
    ;     ; (enforce-max-wl-mint owner)
    ;     (if 
    ;         (!= owner ADMIN_ADDRESS)
    ;         (coin.transfer owner ADMIN_ADDRESS (* (get-price) amount))
    ;         "Admin account"
    ;     )
    ;     (with-capability (ACCOUNT_GUARD owner)
    ;         (with-capability (PRIVATE)
    ;             (map
    ;                 (buy-80s-bull owner) 
    ;                 (make-list amount 1)
    ;             )
    ;         )
    ;     )
    ; )

    (defun buy-80s-bull (owner:string number:integer)
        @doc "Buys a 80s bull"
        (require-capability (PRIVATE))
        (require-capability (ACCOUNT_GUARD owner))
        (let (
                (data (get-latest-80s-bull-to-mint-data) )
            )
            (mint-bull (int-to-str 10 (at "edition" (read-table-item data))) {
                "id": (int-to-str 10 (at "edition" (read-table-item data))),
                "item": (read-table-item data),
                "date_minted": (at "block-time" (chain-data)),
                "owner": owner
            })
        )
        (increase-count BULLS_MINTED_COUNT_KEY)
    )

    (defun mint-bull (id:string data:object)
        @doc "Mints a new bull"
        (require-capability (PRIVATE))
        
        (insert minted-nfts id data)

        (increase-count BULLS_CREATED_COUNT_KEY)
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
        ; (enforce (= 1.0 amount) "Only 1 pixel bull can be transferred")
        (enforce-account-exists receiver)
        ; (enforce (= sender ADMIN_ADDRESS) "Can only send from admin account for now")
        (with-capability (ADMIN)
            ; (with-capability (OWNER sender id)
                (update minted-nfts id {"owner": receiver})
            ; )
        )
    )

    (defun list-my-bull-on-market(id:string owner:string price:decimal for_sale:string)
        @doc "Put a Bull on the Market or update one already for sale "
        (with-read minted-nfts id
          { 'id := l_id, 
          'owner := l_owner }
          ;Enforce rules
          (with-capability (ACCOUNT_GUARD l_owner)
            (enforce (= owner l_owner) "Account Owners dont match.")
            (enforce (>= price 0.0)  "Positive decimal sell prices only." )
            (update minted-nfts id 
                {
                    "for_sale": for_sale,
                    "price": price
                }
            )
            (if (= for_sale "true") (format "Bull with ID {} is now for sale for {}" [id price]) (format "Bull with ID {} is no longer for sale" [id]))
          )
        )
    )
    
    (defun buy-bull-off-market
        ( id:string
          owner:string
          amount:decimal )
        @doc " Buy a Bull off the Market "
        (with-read minted-nfts id
          { 'id := m_id, 
          'owner := m_owner,
          'for_sale := m_for_sale,
          'price := m_price }
            (enforce (= m_for_sale "true")  "You can only purchase a Bull that is for sale." )
            (enforce (= amount m_price) "Insufficient funds.")
            (enforce (!= owner m_owner) "You cannot buy your own Bull.")
            (enforce (= "k:" (take 2 owner)) "Only k: Accounts.")
            (coin.transfer owner m_owner m_price)
            (coin.transfer owner ADMIN_ADDRESS (* 0.02 m_price))
            (transfer id owner m_owner m_price)
            (update minted-nfts id 
                {
                    "for_sale": "false",
                    "price": 0.0
                }
            )
            (format "Purchased a Bull with the ID {} for {} KDA " [id amount])
        )
    )

    ;;;;;; BULL NON STATE MODIFYING HELPER FUNCTIONS ;;;;;;;;;

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

    (defun get-latest-80s-bull-to-mint-data ()
        (let 
            (
                (minted-count (get-count BULLS_MINTED_COUNT_KEY))
                (created-to-mint-count (get-count BULLS_CREATED_TO_MINT_COUNT_KEY))
            )
            (enforce (< 0 created-to-mint-count) "No bulls have been put up for mint")
            (enforce 
                (< minted-count created-to-mint-count)
                 "All bulls put up for mint have already been minted, please check later"
            )
            (let 
                (
                    (data (at minted-count (keys not-minted-nfts)))
                )
                data
            )
        )
    )

    (defun enforce-mint-wl-role (account:string)
        @doc "Enforce the account has a role that allows them to mint bulls"
        (let 
            (
                (curr_wl_role (get-value CURR_WL_ROLE_KEY))
                (user_wl_role (get-wl-role account))
            )
            (if 
                (= curr_wl_role WL_PREMIUM_ROLE)
                (enforce (= user_wl_role WL_PREMIUM_ROLE) "Only premium WL members allowed")
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

    (defun enforce-max-wl-mint (account:string)
        @doc "Enforces wl member only mints max amount"
        (let 
            (
                (owned-count (length (bulls-owned-by account)))
            )
            (enforce (<= owned-count 5) "You have minted your max for whitelist round")
        )

    )

    (defun bulls-owned-by (owner:string)
        @doc "All bulls owned by someone"
        (select minted-nfts ["id"] (where "owner" (= owner)))
    )

    (defun get-bull-fields-for-ids (fields:list ids:list) 
        @doc "Return fields for a list of ids"
        (map 
            (get-bull-fields-for-id fields)
            ids
        )
    )

    (defun get-bull-fields-for-id (fields:list id:string )
        @doc "Return the fields for a given id"
        (+ {"id": id} (read minted-nfts id fields))
    )

    (defun id-for-new-bull ()
        @doc "Returns an id for a new bull to be minted"
        (int-to-str 10 (get-count BULLS_CREATED_COUNT_KEY))
    )

    (defun all-minted-bulls ()
        @doc "Returns all the ids"
        (with-capability (ADMIN)
            (keys minted-nfts)
        )
    )

    (defun all-bulls ()
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

    (defun read-table-item(id)
        (at "item" (read not-minted-nfts id ['item] ))
    )

    (defun patch-minted-count (num)
        (with-capability (ADMIN)
            (update counts BULLS_MINTED_COUNT_KEY {"count": num})
        )
    )

)

;  (create-table minted-nfts)
;  (create-table not-minted-nfts)
;  (create-table counts)
;  (create-table values)
;  (create-table wl)
;  (create-table price)
 
