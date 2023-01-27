(module pact-rats GOV
    "Pact Rats Mint"
    
      (defconst ADMIN_KEYSET "free.admin-arkade")
      (defconst NFTS_TO_MINT_COUNT 666.0)  
      (defconst MINT_PRICE 10.0)
      (defconst MAX_WL_MINT 10)
      (defconst MINT_START_TIME "2022-10-07T20:00:00Z")
      (defconst MINT_END_TIME "2022-10-08T20:00:00Z")
      (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
      (defconst NFTS_MINTED_COUNT_KEY "nfts-minted-count-key")
      (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
      (defconst CURR_WL_ROLE_KEY "curr-wl-role-key")
      (defconst WL_PREMIUM_ROLE "whitelist")
      (defconst PRICE_KEY "price-key")
      (defconst ADMIN_ADDRESS "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec")
      (defconst CREATOR_ADDRESS "k:e4dcb5cfd692521478cf6550f776aa5f70a35dec15ac90cda8f8a99ca6844fd1")
      (defconst ROYALTY_FEE 0.05)
  
      (defcap GOV () 
          (enforce-keyset ADMIN_KEYSET)
      ) 
  
      (defcap PRIVATE () 
          @doc "can only be called from a private context"
          true
      ) 
  
      (defcap ACCOUNT_GUARD(account:string) 
          @doc "Verifies account meets format and belongs to caller"
          (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
          (enforce-guard   
              (at "guard" (coin.details account))
          )
      )
  
      (defcap OWNER (id:string)
          @doc "Enforces that an account owns a pixel nft"
          (let 
              (
                  (nft-owner (at "owner" (read nfts id ["owner"])))
              )
              (compose-capability (ACCOUNT_GUARD nft-owner))
          )
      )
  
      (defcap ADMIN() 
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
          (insert counts NFTS_MINTED_COUNT_KEY {"count": 0.0})
          (insert counts TOTAL_VOLUME_KEY {"count": 0.0})
          (insert values MINT_CHAIN_ID_KEY {"value": (at "chain-id" (chain-data))})
          (insert price PRICE_KEY {"price": MINT_PRICE})
          (insert values CURR_WL_ROLE_KEY {"value": "whitelist"})
      
      )
  
      ;;;;; SCHEMAS AND TABLES ;;;;;
  
      (defschema nft-main-schema
          @doc "Stores core information about each nft"
          id:string
          date-minted:time
          owner:string
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
  
      (deftable nfts:{nft-main-schema})
      (deftable marketplace:{marketplace-schema})
      (deftable counts:{counts-schema})
      (deftable values:{values-schema})
      (deftable wl:{wl-schema})
      (deftable price:{price-schema})
  
      ;;;;;; MINT FUNCTIONS ;;;;;;
  
      (defun mint-nfts-bulk (owner:string amount:integer)
         @doc "Mints NFTs bulk"
          (enforce-mint-live)
          (coin.transfer owner CREATOR_ADDRESS (* (get-mint-price) amount))
          (with-capability (ACCOUNT_GUARD owner)
             (with-capability (PRIVATE)
                 (map
                     (mint-nft owner) 
                     (make-list amount 1)
                 )
             )
         )
      )
  
      (defun mint-nft (owner:string amount:integer)
          @doc "Mints an NFT"
          (require-capability (PRIVATE))
          (require-capability (ACCOUNT_GUARD owner))

          (let (
                  (id (get-latest-nft-to-mint-data) )
              )
              (insert nfts id {
                  "id": id,
                  "item": { "edition": id },
                  "date-minted": (at "block-time" (chain-data)),
                  "owner": owner
              })
          )
          (with-capability (PRIVATE)
              (increase-count TOTAL_VOLUME_KEY (* (get-mint-price) amount))
          )   
          (increase-count NFTS_MINTED_COUNT_KEY 1.0)
      )
  
      (defun mint-nfts-free (owner:string amount:integer)
          @doc "Mints nfts as admin for free"
          (with-capability (ADMIN)
              (with-capability (ACCOUNT_GUARD owner)
              (with-capability (PRIVATE)
                  (map
                      (mint-nft owner) 
                      (make-list amount 1)
                  )
              )
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
          (enforce-marketplace-live)
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
          (enforce-marketplace-live)
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
  
      ;;;;;; OTHER FUNCTIONS ;;;;;;
  
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
  
      ;;;;; ENFORCEMENTS ;;;;;
  
      (defun enforce-mint-wl-role (account:string)
          @doc "Enforce the account has a role that allows them to mint nfts"
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
                  (owned-count (length (ids-owned-by account)))
              )
              (enforce (<= owned-count MAX_WL_MINT) "You have minted the max")
          )
      )
  
      (defun enforce-mint-live ()
          @doc "Enforces mint is live"
          (enforce (>= (at "block-time" (chain-data)) (time MINT_START_TIME)) "Mint is not live.")
          (enforce (<= (at "block-time" (chain-data)) (time MINT_END_TIME)) "Mint has ended.")
      )
  
      (defun enforce-marketplace-live ()
          @doc "Enforces mint has ended and marketplace is live"
          (enforce (>= (at "block-time" (chain-data)) (time MINT_END_TIME)) "Mint is not live.")
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
      
  
      ;;;;;; HELPER FUNCTIONS ;;;;;;;;;
  
      (defun get-mint-price()
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
  
      (defun get-latest-nft-to-mint-data ()
          (let 
              (
                  (minted-count (get-count NFTS_MINTED_COUNT_KEY))
                  (created-to-mint-count NFTS_TO_MINT_COUNT)
              )
              (enforce (< 0.0 created-to-mint-count) "No nfts have been put up for mint")
              (enforce 
                  (< minted-count created-to-mint-count)
                   "All nfts put up for mint have already been minted, please check later"
              )
              (let 
                  (
                    (id (int-to-str 10 (+ (floor minted-count) 1)))
                  )
                  id
              )
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
  
  )
  
    ; (create-table nfts)
    ; (create-table marketplace)
    ; (create-table counts)
    ; (create-table values)
    ; (create-table wl)
    ; (create-table price)
    ; (initialize)
  
