(module brawler-bears GOV
    "Brawler Bears NFTs"
  
    (defconst NFTS_TO_MINT_COUNT 2323.0)  
    (defconst WL_MINT_PRICE 0.0)
    (defconst PUBLIC_MINT_PRICE 11.0)
    (defconst WL_MINT_START_TIME "2023-03-02T19:00:00Z")
    (defconst PUBLIC_MINT_START_TIME "2023-03-02T19:00:00Z")
    (defconst MINT_END_TIME "2023-03-13T20:00:00Z")
    (defconst TRADING_START_TIME "2023-02-13T20:00:00Z")
    (defconst MAX_WL_MINT 1)
    (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
    (defconst NFTS_MINTED_COUNT_KEY "nfts-minted-count-key")
    (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
    (defconst CURR_WL_ROLE_KEY "curr-wl-role-key")
    (defconst WL_PREMIUM_ROLE "wl")
    (defconst URI_KEY "uri-key")
    (defconst PRICE_KEY "price-key")
    (defconst OFFERS_COUNT_KEY "offers-count-key")
    (defconst OFFERS_BANK:string "brawler-bears-offers-bank")
    (defconst ADMIN_KEYSET "free.arkade-admin")
    (defconst ADMIN_ADDRESS "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec")
    (defconst CREATOR_ADDRESS "k:3abb60b658d2754bf14c6c15a8178a590efa36a1d420772174eb92d7a143b021")
    (defconst ROYALTY_FEE 0.11)
    (defconst IMAGES_URI "https://arkade-prod.s3.us-east-1.amazonaws.com/brawler-bears/")
  
      (defcap GOV () 
          (enforce-keyset ADMIN_KEYSET)
      ) 
  
      (defcap PRIVATE () 
          @doc "Can only be called from a private context"
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
          @doc "Enforces that an account owns an NFT"
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
        @doc "Emitted event when an NFT is put on sale"
        @event true
      )
  
      (defcap REMOVED_FROM_SALE (id:string owner:string)
        @doc "Emitted event when an NFT is removed from sale"
        @event true
      )
  
      (defcap BOUGHT (id:string new-owner:string original-owner:string price:decimal)
          @doc "Emitted event when an NFT is removed from sale"
          @event true
      )
  
      (defun initialize ()
          @doc "Initializes the contract the first time its loaded"
          (insert counts NFTS_MINTED_COUNT_KEY {"count": 0.0})
          (insert counts TOTAL_VOLUME_KEY {"count": 0.0})
          (insert values MINT_CHAIN_ID_KEY {"value": (at "chain-id" (chain-data))})
          (insert price PRICE_KEY {"price": WL_MINT_PRICE})
          (insert values CURR_WL_ROLE_KEY {"value": WL_PREMIUM_ROLE})
          (insert counts OFFERS_COUNT_KEY {"count": 0.0})
          (insert values URI_KEY {"value": IMAGES_URI})
          (coin.create-account OFFERS_BANK (create-BANK-guard))
      )
  
      ;;;;; SCHEMAS AND TABLES ;;;;;
  
      (defschema nft-main-schema
          @doc "Stores core information about each NFT"
          id:string
          date-minted:time
          owner:string
          item:object
      )
  
      (defschema marketplace-schema
          @doc "Schema for marketplace information"
          id:string
          for-sale:bool
          price:decimal
          updated-at:time
          owner:string
      )

      (defschema offers-schema
          @doc "Schema for marketplace offers"
          id:string
          nft-id:string
          buyer:string
          owner:string
          created-at:time
          expires-at:time
          amount:decimal
          withdrawn:bool
          status:string
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
      (deftable offers:{offers-schema})
      (deftable counts:{counts-schema})
      (deftable values:{values-schema})
      (deftable wl:{wl-schema})
      (deftable price:{price-schema})
  
      ;;;;;; MINT FUNCTIONS ;;;;;;
      
     (defun mint-nfts-bulk (owner:string amount:integer)
         @doc "Mints NFTs bulk"
          (enforce-mint-live)
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
        ;   (require-capability (ACCOUNT_GUARD owner))

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

      (defun airdrop (owners:list)
          @doc "Airdrops NFTs to list of address"
          (with-capability (ADMIN)
              (map (lambda (owner) (mint-nft owner 1)) owners)
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
      
      (defun get-latest-nft-to-mint-data ()
          (let 
              (
                  (minted-count (get-count NFTS_MINTED_COUNT_KEY))
                  (created-to-mint-count NFTS_TO_MINT_COUNT)
              )
              (enforce (< 0.0 created-to-mint-count) "No NFTs are available for mint.")
              (enforce 
                  (< minted-count created-to-mint-count)
                   "All NFTs put up for mint have already been minted."
              )
              (let 
                  (
                    (id (int-to-str 10 (+ (floor minted-count) 1)))
                  )
                  id
              )
          )
      )
  
      ;;;;;; MARKETPLACE FUNCTIONS ;;;;;;
  
      (defun transfer-bulk:string
          (ids:list
            receiver:string
          )
          @doc "(Admin only) Transfer multiple NFTs to an account."
          (with-capability (ADMIN)
              (map 
                  (transfer receiver)
                  ids
              )
          )
      )
  
      (defun transfer:string (receiver:string id:string)
          @doc "Transfer an NFT to an account."
        ;   (enforce-marketplace-live)
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
  
    (defun buy-id-on-sale (id:string buyer:string)
        @doc "Buys an NFT that was put up for sale"
        (with-capability (ACCOUNT_GUARD buyer)
            (enforce-id-on-sale id)
            (let* 
                (
                    (nft-data (get-nft-fields-for-id [] id ))
                    (original-owner (at "owner" nft-data))
                    (price (at "price" (read marketplace id ["price"])))
                    (fee (get-market-fee-from-price price))
                    (to-seller-amount (get-to-seller-amount-from-price price))

                )
                (coin.transfer buyer original-owner to-seller-amount)
                (coin.transfer buyer CREATOR_ADDRESS fee)
                (write marketplace id {
                    "id": id,
                    "for-sale": false, 
                    "price": -1.0,  ; Invalid price so NFT can't be sold
                    "updated-at": (at "block-time" (chain-data)),
                    "owner": buyer
                })
                (update nfts id (+ {"owner": buyer} nft-data ))
                (with-capability (PRIVATE)
                    (increase-count TOTAL_VOLUME_KEY price)
                )
                (emit-event (BOUGHT id buyer original-owner price))
            )
        )
    )

    (defun make-offer (id:string buyer:string duration:integer amount:decimal)
        @doc "Make an offer for an NFT"
        (with-capability (ACCOUNT_GUARD buyer)
        (let (
            (original-owner (at "owner" (read nfts id ["owner"])))
            (new-offer-id (int-to-str 10 (floor (get-count OFFERS_COUNT_KEY))))
        )
            (enforce-id-on-sale id)
            (enforce (> amount 0.0) "Amount must be greater then zero")
            (enforce (> duration 0) "Duration must be at least 1 day")
            (enforce (!= original-owner buyer) "The buyer can't be the owner")
            (coin.transfer buyer OFFERS_BANK amount)
            (insert offers new-offer-id {
              "id": new-offer-id,
              "nft-id": id,
              "buyer": buyer,
              "owner": original-owner,
              "amount": amount,
              "withdrawn": false,
              "status": "pending",
              "created-at": (at "block-time" (chain-data)),
              "expires-at": (add-time (at "block-time" (chain-data)) (days duration))
            })
            (with-capability (PRIVATE)
              (increase-count OFFERS_COUNT_KEY 1.0)
            )
            )
        )
    )

    (defun cancel-offer (id:string)
      @doc "Cancel NFT offer"
      (with-read offers id
        {
          "buyer" := buyer,
          "expires-at" := expires-at,
          "amount" := amount,
          "withdrawn" := withdrawn
        }
        (with-capability (ACCOUNT_GUARD buyer)
          (enforce (= withdrawn false) "Cannot withdraw twice")
          (with-capability (PRIVATE)
            (install-capability (coin.TRANSFER OFFERS_BANK buyer amount))
            (coin.transfer OFFERS_BANK buyer amount)
            (update offers id { "withdrawn": true, "status": "canceled" })
          )
        )
      )
    )

    (defun accept-offer (id:string)
        @doc "Accept an offer"
        (with-read offers id
        {
            "nft-id" := nft-id,
            "buyer" := buyer,
            "created-at" := created-at,
            "expires-at" := expires-at,
            "amount" := amount,
            "withdrawn" := withdrawn
        }
            (with-capability (OWNER nft-id)
                (enforce-id-on-sale nft-id)
                (enforce (= withdrawn false) "Cannot withdraw twice")
                (enforce (>= expires-at (at "block-time" (chain-data))) "Offer expired")
                (let* 
                    (
                        (nft-data (get-nft-fields-for-id [] nft-id ))
                        (original-owner (at "owner" nft-data))
                        (fee (get-market-fee-from-price amount))
                        (to-seller-amount (get-to-seller-amount-from-price amount))

                    )
                    (with-capability (PRIVATE)(coin.transfer OFFERS_BANK original-owner to-seller-amount))
                    (with-capability (PRIVATE)(coin.transfer OFFERS_BANK CREATOR_ADDRESS fee))
                    
                    (update offers id { "withdrawn": true, "status": "accepted" })
                    (write marketplace nft-id {
                        "id": nft-id,
                        "for-sale": false, 
                        "price": -1.0, 
                        "updated-at": (at "block-time" (chain-data)),
                        "owner": buyer
                    })
                    (update nfts nft-id (+ {"owner": buyer} nft-data ))
                    (with-capability (PRIVATE)
                        (increase-count TOTAL_VOLUME_KEY amount)
                    )
                    (emit-event (BOUGHT nft-id buyer original-owner amount))
                )
            )
        )
    )

    (defun get-offers-for-id (id:string)
      @doc "Get all offers for a single NFT"
      (select offers (and?
        (where "nft-id" (= id))
        (where "withdrawn" (= false))
      ))
    )

    (defun get-offers-for-buyer (buyer:string)
      @doc "Get all offers made by a single buyer"
      (select offers (and?
        (where "status" (!= "canceled"))
        (where "buyer" (= buyer))
      ))
    )

    (defun get-offers-for-owner (owner:string)
      @doc "Get all offers received from owner"
      (select offers (and?
        (where "owner" (= owner))
        (where "withdrawn" (= false))
     ))
    )
  
      (defun all-ids ()
          @doc "Returns all the IDs"
          (keys nfts)
      )
  
      (defun get-market-fee-from-price (price:decimal)
          @doc "Market fee cost for ID sold at a given price"
          (* price ROYALTY_FEE)
      )
  
      (defun get-to-seller-amount-from-price (price:decimal)
          @doc "Amount that goes to a seller when NFT sold at a given price"
        (* price (- 1 ROYALTY_FEE))
      )
  
      (defun get-marketplace-fields-for-ids (fields:list ids:list) 
          @doc "Return fields for a list of IDs"
          (map 
              (get-marketplace-fields-for-id fields)
              ids
          )
      )
  
      (defun get-marketplace-fields-for-id (fields:list id:string )
          @doc "Return the fields for a given ID"
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
      
      (defun get-nft-fields-for-ids (fields:list ids:list) 
          @doc "Return fields for a list of IDs"
          (map 
              (get-nft-fields-for-id fields)
              ids
          )
      )
  
      (defun get-nft-fields-for-id (fields:list id:string )
          @doc "Return the fields for a given ID"
          (+ {"id": id} (read nfts id fields))
      )
  
      ;;;;; ENFORCEMENTS ;;;;;
  
      (defun enforce-mint-wl-role (account:string)
          @doc "Enforce the account has a role that allows them to mint NFTs"
          (if (< (at "block-time" (chain-data)) (time PUBLIC_MINT_START_TIME))
              (let 
                  (
                      (curr_wl_role (get-value CURR_WL_ROLE_KEY))
                      (user_wl_role (get-wl-role account))
                  )
                  (if 
                      (= curr_wl_role WL_PREMIUM_ROLE)
                      (enforce (= user_wl_role WL_PREMIUM_ROLE) "Only whitelist members allowed")
                      true
                  )
              )    
              true
          )
      )
  
      (defun enforce-account-exists (account:string)
          @doc "Enforces that an account exists in the coin table"
          (let ((coin-account (at "account" (coin.details account))))
              (enforce (= coin-account account) "account was not found")
          )
      )
  
      (defun enforce-max-wl-mint (account:string amount:integer)
          @doc "Enforces WL member only mints max amount"
          (if (< (at "block-time" (chain-data)) (time PUBLIC_MINT_START_TIME))
              (let 
                  (
                      (owned-count (length (ids-owned-by account)))
                  )
                  (enforce (< amount MAX_WL_MINT) "You have minted the max")
                  (enforce (< owned-count MAX_WL_MINT) "You have minted the max")
              )
              true
         )
      )
  
      (defun enforce-mint-live ()
          @doc "Enforces mint is live"
          (enforce (>= (at "block-time" (chain-data)) (time WL_MINT_START_TIME)) "Mint is not live.")
          (enforce (<= (at "block-time" (chain-data)) (time MINT_END_TIME)) "Mint has ended.")
      )
  
      (defun enforce-marketplace-live ()
          @doc "Enforces mint has ended and marketplace is live"
          (enforce (>= (at "block-time" (chain-data)) (time TRADING_START_TIME)) "Trading is not live.")
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

    (defun create-BANK-guard ()
        (create-user-guard (require-PRIVATE))
    )

    (defun require-PRIVATE ()
        (require-capability (PRIVATE))
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
  
      (defun get-count (key:string)
          @doc "Gets count for key"
          (at "count" (read counts key ['count]))
      )
  
      (defun get-value (key:string)
          @doc "Gets value for a key"
          (at "value" (read values key ['value]))
      )      

      (defun get-mint-price()
          (if (< (at "block-time" (chain-data)) (time PUBLIC_MINT_START_TIME)) WL_MINT_PRICE PUBLIC_MINT_PRICE)
      )
  
      (defun get-wl-role (account:string)
          @doc "Gets current wl role for the user"
          (try
              ""
              (at "role" (read wl account ["role"]))
          )
      )
  
      (defun ids-owned-by (owner:string)
          @doc "All IDs owned by someone"
          (select nfts ["id"] (where "owner" (= owner)))
      )

    (defun get-image-uri (id:string)
        @doc
        "Give URI for ID. If not supported, return \"\" (empty string)."
        (+ (get-value URI_KEY) id)
    )
  
  )

; (free.brawler-bears.add-to-wl-bulk "wl" [
;     "k:609466382bc22b6c19f030acddaacba0d5f2aeb299dca4694d3bc104e34df654",
;     "k:d53408e4c09673a57a38e2b1bcc3035c7848ba9e97b5ab757eede0f60d2d464a",
;     "k:d9a08f27c9473c80caf394231309dc43467659dd55fea341e49785ffb1a3683c",
;     "k:9d1567b8547a288bea6b09cac2e6cd818d511012078594b264ccab4d3a28a812",
;     "k:2c12b4f9d2d2f1b68a8cf1e21a127ec2fb195433db19566e638e9cf0477bad50",
;     "k:18236e667ca1cb2f2ad2b15afd75ee8a57bd9966693767605bdda3fe7d547659",
;     "k:7b969dc9711f17e3f813a2b025e1a9405edc0bfab603a54b8ffd37f376ee1c2a",
;     "k:6151a59541a4d411644d01ad07566f16ddf1ef53e0f3e24269a3141f5df0647e",
;     "k:283428c768e917cf731f12fad4f4ac074c325c70e4be7976c005ba811e2ac993",
;     "k:d6ff662f8a4828611c80d43029f59fd5902ff8b87a2ccb8908f6323dd671501a",
;     "k:814d1d97a63afa9f5a520813d0c3c9aae7dfe6f2476372b6aba431316e479038",
;     "k:b76d29a210b96df2658a888a791a402488429470a0c678b9a33c3507a2deec30",
;     "k:9a61add16c7107b3dbab6d3aaaff1c0339f9828cd48037749dfd79762c719fea",
;     "k:651a15c7dddba583e3a32f5a1845867bfe8f60060b9c17e11bff191102575f31",
;     "k:f35ff98bb9620420b98bef509cc1c0dd3f5c463011834b7233e769300dd792d3",
;     "k:dd06a5a398c2e9571f966586109d878c1cfaee578167839e916ab1024482f05b",
;     "k:fae896b9086120d0a7e0702f6395d2aba026b50e177db5fb0010ee2c7aa53d50",
;     "k:afe2b39cf4633290281d52015bf0dfd1700043b1b36424aebe4fb2b07a37566b",
;     "k:99cc0e10f2cc3df46538cc9f9e64dd6ff694cf00d2b26b36953e5d253d07ee1e",
;     "k:a671b0de6fd269348b68dfcd277549bc885e781791c46edf0905fdf23193a0a9",
;     "k:78a622687acad287ae369d7aafad83ccad0c97f126539db2cd27122008a77e39",
;     "k:645e6a405c6c81292bf121684f0ee10e83037564b1f3355346b6915d3cdbc609",
;     "k:cdaa0f2d434bc3e30080fb8f2eba1f395b691cb47dbf078c167372f627441553",
;     "k:fcd4e847adbf45fc4e4b05b58afe15843395e567b432e4dd6c9b315b5d2ec49a",
;     "k:0761af28a9f283c1b4ed62bf5ddbe138c33f6899c6e2753278b1b7464bbd75bd",
;     "k:94d08e5349676632ca64ff882fb23d60804eacb7960aa40084395adcd0c611f9",
;     "k:99c3949903f0e89275f1f35944d706eeaea4b4a8051826ea4c81319753cd5ac2",
;     "k:4496205014a34f909ce27dd6a7417477f1a5a2df9ac3fcb77feb9c23a7251e1a",
;     "k:d09865f0340c6d2e1ba353bab368968407586c7a9c5ec4f6b1cb943acc496a5d",
;     "k:37877711df8d761e807d28061a6c54ebc1181ffecf3b40bbbf2c034e59e17a19",
;     "k:48704163cc65e8eea903b9ff6b48a8d905a2aa6c7e9d512607c84f7dc98cfbd2",
;     "k:3de54c45b72dd96587587aee4bff9e4bd4580c9d628102b74daa9ddbea05dce7",
;     "k:ae0e225378fe41ffe4f639bea0bb48ba039b511163d5ce9b1efc63bc5b1fb9ef",
;     "k:76821f628c5d257ee32b9f4527490fd41847cb9267c6ba5611d7981616e4ed17",
;     "k:36d44a7f6aeb69cbd049c3dfb86259daac5e31ca945731b2b0b362070beb3749",
;     "k:80509be08447b048382c23ad198fffb6f3ae8520a3c004f3312f87409cb8adc9",
;     "k:544f3fb37ff2e9f746288727a5fd1830b4bb8a3fbb6fe3b1439a4c7ccc67e797",
;     "k:dde578acd8253b8e904e2096738b8fefb698ef1200047d23d77fce34a8ff46e2",
;     "k:05792732afb5519cb23e161a31de117acc157198632c37bde0faf28a61261a9f",
;     "k:9b51bbab5051d22f2fc39ad30530c163f61dd419edbdb7c3a1c88ec77b6d4ed7",
;     "k:612847ff468f83dfec55b1e26af066764ef2444196ec15c30ab23839dae24994",
;     "k:8c682aaff14b6e3692d0aa66e7ec9eb6435af95e67b2349166af0bc9fb35ae97",
;     "k:21a1a0f77858bbfd98e7ef696646afda65a247a05cb624270881aaadfedd5963",
;     "k:0e091bae582399a881b4f9036e1fba9026b76e561afa08ca436bd73e43b3a5af",
;     "k:b29d6068ff2bb117d91ba571b334ba643744cebfb7a7ae02b9fb6af609ac38c7",
;     "k:36929105fa4f03adcfdd7ce31024db2374e8e0ce5225fb9504de3b63cfe270f4",
;     "k:7202fa2368f7aec83a9030c9d00b123f21e6c1898b7d52630b8a67fff18819af",
;     "k:6a599963c2dc099c6ef90dfa9788c8a226771edb38b3d6755c406c64d400f706",
;     "k:88edd9fd5479ba5c1eaa350b1663167c9a25e896c4d5d2662225aaf47c4e50a0",
;     "k:b3d8a12b7bf2e6eb162fccdf416f7be67ddf27460e00d41e51307219ed17867e",
;     "k:07d6a3ebaeae0232a8ec4217d582803b72ee96a61d53dba8e717938da4928a9f",
;     "k:060015bb5d51b0e7bbba4f46b16217ce76902badf948055c8ae5e8c597fae7a2",
;     "k:cfb5fcb3a4021ebb844fd8ff4fdb428124d875a0b61821bd03a16cce93edb16a",
;     "k:c44101df95280bf4de9b2b8ef0ba32375fdf34bf041e15eb478933d0dc7e01c0",
;     "k:cfe62e7eac9e6110211dabf09da712fa16b3c6bbe64a6a2f8f841f6e06b248da",
;     "k:7c7f0b1da427f34c78dff50c67595062aa77ef3986ba633e1b0a267f1b58b421",
;     "k:a49e7e84bdbb8af3b5a02cc51ce05f741454800e8c77225c5fa8f7ab051e2cda",
;     "k:be7083f46a49e8d3976d147f6e863df08006325b4112e6eae715ebe9fba15b0b",
;     "k:7b1267de65072c8be77cf0f693ab22821d22412e52dd35e279fa701998e2a526",
;     "k:aca5a0612c6733be08987ed8358298270ba747df9e532bdc30a956ae75b96b04",
;     "k:ca23d02bad504b9aa754207d7d435f1427476e5d654fdb16bc531774b8871547",
;     "k:2b959bf1a69f2f1ecef082ff3cb40b1393a07a44e5d3f89cbe4bbd6e4ef87520",
;     "k:1df63dc4612cbcfcbc4704d8265c1daadc73c292d121d819a3b65d4b11017e79",
;     "k:4a12d5c6b4d86709cdd94d908ccd3d781682bdbafaa2f55b705383833046a466",
;     "k:1880fe4cdc5db6acbab1a1303832dab8269b8cf30d6330b6cf4ed193f87490e0",
;     "k:d909b172a456cc66db9646dbefc85c5aaed7fce74a902bde0f32f23f676c4e6b",
;     "k:461ae9f3c9c255112ac3797f6b15699c656c9bc44ed089551a0f792085ef9504",
;     "k:b65b32558c4295ed6eee958fd9fc1ab2c3e2ece5ca17d4567660a3edb481459d",
;     "k:b8dbe0b6c7fe80b92ab648d5e758882e750114f4ffac4188ddf52e5bfc1a9d9c",
;     "k:1de196801c03efa9fa98fb2e5e945f899dda7174eb490ccd3bcfe60789e6250d",
;     "k:5d32b48b70c4318e54d1f03e5a009f4f93671594ef5309a2a3755b2177fc3261",
;     "k:a1b052f8754d68772ffd28f648f4358631e3b739758385a5a4cc04905f6491b3",
;     "k:c1f76e2ba323cec34b46a33d8b4bb346566a7b4b31db512cf04802048084cf27",
;     "k:9b539e93c8557532bb5ac73f6f5e2dbbf51b3e7dca45c5bb1e71343280050a36",
;     "k:3643552d871dc197e35978d35dcc8875839290b0a85d159a911995df1f694194",
;     "k:afe002eb61bb9e7958c307a8dc9579bd032f8c72f8c777207fc9108ac93a1e2b",
;     "k:7318e639cc4d33e7bc096314b9635e383b91118e755236e2e86f02c5e276f348",
;     "k:84e85499a12b1b49d7c87daff870602a5eba72bd5a7a42c6b8bba1c294c6ccbe",
;     "k:4bac24f2622cf073a26d0222da0d0867caeaa4a56f6445635423c4be6b29895b",
;     "k:57e7f5c4b2076f74add99cfef2bbc463089bd9e2df8adefdcb0f6c50ba5ef992",
;     "k:cdea5e697845f449cc42d7341c1f80a22640e2eb162b017307083e2088b13ddd",
;     "k:ad492dcaf8a75f779b9e4efcc6fca19e9683f7f98ceb81efe6ea871ec00deb40",
;     "k:3e47dfae31b756ce2277682b3e9ea31f9eaa823cdc1efd279237efc37848bbc3",
;     "k:a54b9408ab47572bb3a8e403ead1502a23755bc24bbbda3ccaea0b7a292e336d",
;     "k:51e79fda425dac781a8834b1e8f89c99a8dcbdb97c7e787e9701161df1031669",
;     "k:ab5f922b8fe4ffa4ce155066051e35461f11b1648a3fe8d798f71272d18d1637",
;     "k:a527bec40c53410620ddac37a3c4dedb9fc76912ab584e7436188b9a483b161b",
;     "k:e4dcb5cfd692521478cf6550f776aa5f70a35dec15ac90cda8f8a99ca6844fd1",
;     "k:b2bcf61fc8a948ba390840b3cd83916e9ce999e458ec8261f8ef6f91ef9e6c59",
;     "k:0c1c95a98efcfb03df6999957285991cca1218c6cf7a24641b19ca802ead2a5e",
;     "k:a6977fb3fbf6b3973d8c4c27173de44abfb364dd9d837240aa8cf5c0fac17124",
;     "k:82411be4b9d206dfc72d7fc3de9db12181a065fbafeb6b61ef387a860a3b98e6",
;     "k:5d69d1258f9d4a747f933e766aa62b6b4cf570586fa92725608382e22c02210f",
;     "k:8586be00d8341cc12a43cc5445bb38aef878a1fff411f96da7de6427329d387a",
;     "k:5486a1f77b37d92142046d82f935f1af251dc2b3c770e24f03740e8939de8e49",
;     "k:b13054cf2357725085178d7ddd584b2c1575facee161c97b89e90b99771a3e99",
;     "k:0da4a89d97ab5932e5ef1d330ce8352328dd887b784502934c1c787abe226e2c",
;     "k:8ddcb1b0fc5cff6842ef19901d03b3b7c6ab7ebfae48ed20752439ccbdbc8185",
;     "k:669d2ac485cd419043ffa36b69dd5fa76c853f162c65891a6d224aa191e8d138",
;     "k:5862698f413251dd243aef618aec693d92ec306e87e148aa509d86ee6c7d545d",
;     "k:f1543e958b7f1343abbd49b558b6b66f0c5e006eddaa868e9570f3db1ed2dac3",
;     "k:18bc2704b68d15cff89d9df164fec9167b9f5bc29c16a82924ef0f994f2b92bc",
;     "k:4334f6933a8217697f9490ad406c8ecba50cf016c461b58179a5e7d1cebffb1c",
;     "k:1b173b2b586bf8bea6155ba1d8bd4487e6a2f17fec90032863a7a04b02b910a3",
;     "k:ec81c5a5b6dced74b82219b91e4e1afde05dd498c216ce3e779da49f368a78ba",
;     "k:cf415c73edb4666a967933bddc2e6c4a6e13b8ec0566e612b9f3cbe4a4d8506e",
;     "k:4aaa6a6dd6b120d9811270bbb8451a308f6f7a3d9a7582d55ed39df76e4a00d3",
;     "k:9eb0aceeee1af8b933c9a301ffeee0c9b18e3c5b6a49962c93b59f895462309a",
;     "k:d088bd5182ebe8e7a6c7bf42512798dbeaade3b7b422857216ce6a0ff40667e8",
;     "k:c9a66f5456c13a241a91305fd5820cf0535356ad8d16320008672bac4b87d0c4",
;     "k:fcf0141cffe9b0c107aba77befa483e159b2f4dc234aeb7870f9b005d806dc87",
;     "k:45262b148d82c6b8e1ecb8ae18a32ab8abc782d96501e2d5b9cde25fd3ca155c",
;     "k:49c5b89b8d981ad5f344b57ebe911275d7386451f42a86118e826e3769fbc409",
;     "k:c354da2fddc83669d8be072e8a71affe6e82ebf32787c6e1f97be3389a1ba3d8",
;     "k:f7d5634a249dabcb2719eeacf5ef3577f18f54658c715b78dbc37fdfce66288c",
;     "k:7753bd9cd1330c28e4f89bdb25a33c697a0a39158ea35740e007f89aa03c5bd1",
;     "k:4c6ac26e7879f9be49b8911e51a7cbe3433ce57dbeb1192f84dba3e026cac300",
;     "k:c0c2df87041ad2a804397c831dad1a61cec6ae8f7110c9a3c23b118eaad5b07d",
;     "k:481984ae50104f321e2b02aaae2280c9253aa6de3b291baaebdaec2962b83b43",
;     "k:71dd378fe862641c0d9b89f3ec059ba7911b0e6b201e57e85e2c144756c243f8",
;     "k:d1fe9082aec5e7228f21be04ad41dcd74ef993ec90cfc392e96d5cec9a57cd22",
;     "k:4abf574479b5a028dc9f8604da54aba36ca696e8cd76b9b06d684a075921a284",
;     "k:11825e7937a68e21ed58051e375460a27fb130dc101638ab5865420ba5ce56f4",
;     "k:6335715794e205d44ed9f2fa0649151ab99cb53488cd620903cecbe912de091c",
;     "k:6d0a6aaee16d56c0648b6f0f28eb6d9bc5a227c8e88db61c951f188640e8587c",
;     "k:ae40c3b5bd5df98e77a620178d1f805c252ccf7c1a9e06cdc00acdeaf27a2ffc",
;     "k:31262b61fc21b89b5ded718550059443de4595f33b417338d9f69e67aea95106",
;     "k:1e266f8d6f456e3644bc7c9ad2172e32347cc1f9ea748f3d88e7bcd6daedb5ce",
;     "k:0fd542419866c179d5fc91f7f2c0352e5daf26e5f7a761c17524e660613218fe",
;     "k:8dc01727f7a1cf83ea80c9dabb7422115f8ad8762390b243ede7619987f48872",
;     "k:7d78f50db150e2f75825085461c1bad4cc904603e7714a1901bc3766928dc8d8",
;     "k:0e3aa620854606481864ade05973cf30b2dfafc1ab40ce890c5201cee7a73911",
;     "k:363536b93f6ad1195e7eae94ebf9692fdb49cecffc3555ff0df7b19ac3939eab",
;     "k:ceebd08b23db0a716f7048b942062be282ee18c6f290d5e5fdc97053a518ebf0",
;     "k:519267d78fa9a0c6df4158cb73ed630d030f33ea4babff3fe2b372589a033cd2",
;     "k:18358c598fa4d467b6718566b8377dbeba3a63b3a4d26e41ddd0c1d5d325cf89",
;     "k:94922948af50bdfbe20e76bc0e01c9bd963f593a91d8608af20af03ebd6f5c17",
;     "k:4a16d384d24e7035281e33a36e09ce6a8193ddbf4b4e5ee7540134801bc079a8",
;     "k:392fdeffb980aaefedbc600349a7dc929e3ec63e0e3ce230333876c4c4a7a052",
;     "k:140add11eebc63e1cf886a97e7a555a2e221106409bdf3f7e9696d8c1ae0e36f",
;     "k:a4cf6923c421cf5eed5c0de560d1b7be1ed57d320887c88c52465d92e546aa8e",
;     "k:c36783db13c9ca2d3e4bea14256cae7a7c7d9d29f345e69f687c122d8336bef8",
;     "k:ef5f46e64824ebd12621dd5fb3821a78dda5cb8ec1ce7fc25cfecd104556e304",
;     "k:919f700c764d1ed425cd19332c9100995d37fd602b37c62ffdc41209eb638c69",
;     "k:5d7c1ace523b117ae70920d29d3347696fa93e7ffd2e3a8c8b789ada80d49c17",
;     "k:87d4ca6b8bfaea152241ce89c5e9ac8225488de671a7fe9c9a5de3bbbddc1ef4",
;     "k:fa80dc77539995a178379ec2b8bbb65594371cb777e2516112b29976a441d1f4",
;     "k:aeecd476ad8a4842ec84f3fbdad39b73fe7329fb4feaa3ea4367314a29a7e42b",
;     "k:033a7091e8ee4eb808061c12413513454b75f3440897db7ef67f15c0d9b14507",
;     "k:dac13be5b140294bf13530ece90bf097065c1d5fef5fbf7b7ed6c8ec2f612846",
;     "k:c695031536b121ae64e31de0b16e5f5263dea69700a92c7d18ea01bf591b20d8",
;     "k:893a0785d43c4bcbe4e22f8ef83c8c8c61c7375783ae3116ef44bbe3c0d48e9c",
;     "k:bf409cddcc4b33ac2609b5557aa86d4a06e5690ed9df857e4bd586afe126453a",
;     "k:cdfcdff857b143e13776c4e226aabb99cf02bf17ea308f97589201026f421c5d",
;     "k:028e68cbd11d4d54d777ca2f5411e315a814b73f27e240be45242c74a62463ac",
;     "k:b8ef8b0bca0ad98e8b470ba8a6e3e7f0c7762983a9fb50a32b38ff721de9004f",
;     "k:d44529c975c0be5e787dfb9731b584e512f44124d99712c5c5314b52407a9b49",
;     "k:36cae538e0d8d3ad781d4fcc5f73ca8e36d31426612834489fb921a9de17e39b",
;     "k:e2deb09228a802a62938b7d775ef537d4462a8f373729440dcca997ee8dfdbfd",
;     "k:66aa1a597aca27add8e93a625865edfa45c964374423e0b219882fc2e5833f8a",
;     "k:43fdf3ff24e68c0cec69108d8fbac48fa3a4175748f5a5e023ac7fd053a30821",
;     "k:300f485fa1aa9e37043bfc9cb698d114cefa981ffe217f4df08dee512b66ea1d",
;     "k:41c5407b043ac418b1c023e8b89624de4d248c787ccd3abc63bb80758c18889e",
;     "k:9c5277121272775fb7f73dc35144557d02baa647ca24cdf205e78588663d5f3d",
;     "k:34459aa6530625a19730f4405f013f48b8946fef31c5ea1d86bc77ea530fc224",
;     "k:b0eeaa696cf11164063525472360ab62b069a365a0dc4163803e82251c57ebc8",
;     "k:180089b4aa262fc7a8c4289aec77ff0d0af5d7161163306ed7387588d5730dab",
;     "k:85a689846413d7643329a4050ec41e99d6c8bb656e31a68456d2964d860fc9fd",
;     "k:99aedabdcc4cfa2bd4a4a345309d78f468611d9f0dc9002dccfa0a7754f2213c",
;     "k:098face1034192d285459799a2ea7dbff946e7bd242ffce3c2859aa408d37fca",
;     "k:5453470443b860aa0c85d06ce9d7eb74855e5ad680786fde67e3e02eb593ddd5",
;     "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec",
;     "k:7daabcfb733d29b520c9ede529709c21e8307f5cf0256b6f44d9e7bf30ae32d5",
;     "k:7f1af34808e3921148cf42e4167b11a587c10758d5bef814292292b787c1234f",
;     "k:6bcbe84aa48fbee1313559bf33e4254293be261471472204382ffb4aa7ce467e",
;     "k:a2a1cc8efc4519f4a2d3822ed638aaac61f2d3e9211ade0092967130bfefd909",
;     "k:4c65e9b761ae74dd68baa0cf652f61222292a08f1c505d6fd092867b6572f8f9",
;     "k:f7278eeaa55a4b52c281fa694035f82a43a6711eb547fc1ab900be1ccf9fb409",
;     "k:3a079b606ba63c7bad90c18438183a8f45197918884b5c5b0f37b02d8387d6d5",
;     "k:c7017f9cfd6373044ca2bd994cc79797facd66000c8b480da8fdeb767daaa4b9",
;     "k:9875fbcf35062be60c91c20e7bfe513c0f8bc697e0878db07a0580d18dc5c648",
;     "k:5db331117da2b561b09c9b02197a5ec3eeed8b63172beb5da694a10c026b2342",
;     "k:5c71bcab4b63ce78a343b858fbb7b4cf14917b66dd3181babfd73879ffa981b6",
;     "k:cf5510a9f9c9563618c99b71408e74dea6a64f23da3204a15c0dbc61fca0385a",
;     "k:1e5de535ad65fb81986758e9c1018412d2742c8c6bbfde58fa2d50c4bae78cd0",
;     "k:abcbee0e4351ca46e112c07d9d512a8459631ab1a84d67cfec5c7ede09affe81",
;     "k:76c94c0ba65c0a5afc462a50096a02288a4abfdd9ebedbf543c52e5a56604811",
;     "k:bd206c8f2ff143afe55e46788668066bdf563d29cd3903641104964521adf8b4",
;     "k:cf5c17ccec3e9e9ec21007e6ced425f7408bc3acbe8fa3869ac6d0ea0c81910b",
;     "k:cf4d1643460d9ad99bef8ef4522def699d87e6abec435b78479c8e18566d2032",
;     "k:4a9ca1abb96c7f9cb5d226a93749705a5839071f630e56b9bf05e94f2af0699f",
;     "k:3c5a0c339a1ea7baa438ec58697ad265184182c1c1b00aa2eaaca5310b3a9fb2",
;     "k:989fdc4817c9260f50271e73bc98ee6569ddb4735092ad90bb1cfc0c426a02f9",
;     "k:077d0fc5d7c44cfce470db985c6010a8ea60301ed01df27828b361a97a035465",
;     "k:ac112630bf62e406b2e7a2f9de6f907bb6dab83811a777b93eb45a0737f962ed",
;     "k:964a6fd85f380f3cd647fc3b32e74729aa1b1f1c3d27e61c79963c88b69180d6",
;     "k:59c26562a516a365428d557fcad29f8b27a22e670152fa4abc04d95265d75ec4",
;     "k:c0f94195225318c3d7b1a4d654a59f6abba2120645b742b33ca2aa647ad162ce",
;     "k:d5f1ea7f062f07146b5640648a5dc31661aaf4e1278fc4e340106a50ef87a117",
;     "k:5615a32e3c4e178cf64a7b483b2c6ca6647bc788d29c29667fd2c4657d2deaca",
;     "k:ccb60c4491caaa743748cce4b3ce78800e222276de5a0e30646a022996086887",
;     "k:bff3e3ce89ab451ea10a30beb6f7faaa304bc72ddc2a762c68633a8de072e02c",
;     "k:5d8a4c30546f71017455ca53cf42446c9b79a02552d5c47ec51c642c44443535",
;     "k:13f295e409375d48c4c9ccd9e910288b7498f1d4506105809c1c82f3fc7f614e",
;     "k:cc570689c805975715be5f9e85e26aa6696f6298c85e9a51a1a3bf028e145cae",
;     "k:d8b12a09b50e9959384a1d4a0784ae87defa25e0dad68b5253a2d6d66511ade4",
;     "k:986df9e6465fa3b4168cdef4d1846387c20dec2b91e7f5068ddec1977344386f",
;     "k:114a9cad6fbe55cba7b4412b8cde064cc5a084875003c105bfb6c93269f25b14",
;     "k:8ccfd527854ab1a1f5a233458c726fed0f6c90bc9e10a8cbd303fea1e445a187",
;     "k:d7ef4a8f8991885b7aab4490a001f43b9812251c869a875499261d84398e0b9c",
;     "k:dd8bd56f199adab696ce64410f3b2afd6b9bf40f67aeb3eb9f52c063ec39d181",
;     "k:609f68cb2494d8478639d786b2804c50659111d5c8dbc22a12b5d351d419be59",
;     "k:5475870bc41fea9395de6dfcc8820b1ac3c1b7bcc8163db8cb10878fabf4c8d5",
;     "k:109433d67db71bbfd191ab98eca85b50a3363f5782695c1ae9cb69a8ebefd462",
;     "k:919ba497c362e9051292ff7bbba63c4055094c03ab5b10026f3a45d0c7d2ae93",
;     "k:8b7d7e8ed1d87b03436aef486ff7c7642cd7eb70b9bec250ff8d09638085c6c9",
;     "k:5e03142d894800592db1768e154b7937729d3d6c22d357761708b210c2c2f13e",
;     "k:44730eeab49ced70aa153d56c8238ca1a665f6e08a53124739136ee1d5aa14f0",
;     "k:ec17c90108ab962331ff89d04ff4f1f235f611d16a2e0e40d406697c4c45261f",
;     "k:a1aed314bfaa40fb908ede808aa9c1f904b0e258505c5691e03df9b1810fad5d",
;     "k:282e2c21b134fbeda22f6909bdfaf94953e9937e94c72c2c61320be8cd24399d",
;     "k:c38e2873ff6ca85791d9d456bfbaebafdf12f4963a38b77d83ac5a96830a6ffb",
;     "k:c43a6ea3ee38029bb7ab463e3de2f7c9dfc74b4d3104ec99c5436806d8e1e407",
;     "k:ffde189affa7da42d9f202ad6e13ce90b63ed8be8cc746a9bd8cc7a780325887",
;     "k:404910cb38fee4559d4c59c39a0e1534a974fe23c33d41b2b1f3e50a6a63f7e5",
;     "k:8d20ea851d2c2f5f1ab80adcabd18e510bd13f043b10bb45598bb4de660e79cf",
;     "k:0987f9b1f03dbeb8003d0efca3961e920824b2f4a645ffe07f2fc392a36c55d1",
;     "k:9d48dd2ee4f6b042868393983838c7f8ab11169307b8bda0a9ebe465039b680e",
;     "k:3aca39fe131e9e0aafa8ad8b057f5891bf609ba29f93e5cee2a603a97e16a49b",
;     "k:f5544e915ba538dcb75abb140efd7c71996d8645137eea4a8b3bf0b62ce28518",
;     "k:446e57825e02f9b7d0b7f087ee9646b3b4d7fbf313ace9a2caa52a19e3c45db9",
;     "k:6c8350eaf5eaa1b3f6858540e100867b70372e7017737c1fc039a0504f286274",
;     "k:10c91dd66ac4fa87e6b98c6a0866fe92a69d2208941596ae8b2b1c8637b085be",
;     "k:b2756a2598b70fce6786dc59e96c6ae4f011b9fe4a23b8c1131d7282b0c1900b",
;     "k:eac5965fd358e052080afe8ab45ea6fe6e6cd9a47629b6fbb55a5c2d788aca7c",
;     "k:56b4a294d304be6c45e48f4e94a2a07e1273774548068b19ae8e11f2ac4b4ff8",
;     "k:06aecaa117e76a89e6987d892d3f1cf750f1ac7df6608cb495d2b0104bd5ed56",
;     "k:d7b1598b97d942966c210791bf970e1d97c9e4acbbf07b818dad7b4f45498ebc",
;     "k:b869dba07c668909b01602d6ed0c357f85d47ddcaea2ac839b0a3c763c3d6d40",
;     "k:8220d05dc9587383f87263d00a0acc9f7cc740c8d8c03b53483c795bc275d52f",
;     "k:8f6d60de738337c299f7c152a52e4a353df1d94982c75d04d3c9315c47e4e99f",
;     "k:840cbbc507f40e79d23c50cd292e2a5c736e8a3c005d15a43efcb756d35bfc91",
;     "k:28383586ea869c07679dddf6fd8ca9cb90df4ef021b15b130809e3be284b92fa",
;     "k:eac513824906042db444b2aba1d596553218c858e63b6dd19d7f66638193c8c0",
;     "k:d54ac29bd3c4d54d6cfcc27c45317393d773783e7b97c904d2265c8bf0e44ad6",
;     "k:e1d5e2759454a3897acbc63cb3148298563cb66af677083c22bed1339864f78f",
;     "k:be03a29f21a7840df3f556541ad11888e0302d95daf139aec6698dd916247d7c",
;     "k:8c58608379f41105c4e14916621f971ea18b42eecc2a5406d0831b4b150a5fa5",
;     "k:2281c5f798d9c2c179f9b463db4aaa996907799ce415966327ff679bb0e51593",
;     "k:bb580f6d2bc0de67629914c35cd9e81623f39e5ecff1e7a33533b1f74cc00358"
; ])

; (free.brawler-bears.add-to-wl-bulk "wl" [
;     "k:42ddfa91882e41f4ee4f4b216de52460a1f5a67d00ae3db90dda6c208469e995",
;     "k:4a045ed4a3692cc9111074929eb85e008a27f0d2318a7c5621e9fb760bda94df",
;     "k:3e657d1f4f2728289201cbc94c89d5f7057d25bebc6f88d5bf5e90720f98ab83",
;     "k:f6ff406caa0b7c10f98e7b6fe547f6e9fea2d9710ff9833c480f395cce3b4623",
;     "k:a983fed9ca16ab5c048e88de9a6bade8a799dbf7f06720ccb4c7afaa30643f11",
;     "k:0c77552ba8cdbcf86e7e9beb5f2af7114dd63683f2247134b731c83f39925aeb",
;     "k:1b20cfb593645ad9036f080c055a3536b9de88c94aa61faa1791d5782266c469",
;     "k:33af8142f044164cdf1bc7a8b4feab3c7c4881ca1532f23422ae80a509a4bd49",
;     "k:50d365419fde91aef94cafe9a0f6374b866efab561a9988f1f866e5bb2533ef2",
;     "k:b4a8854ce74759b2980e12b0b4a01afb9097d15c81249bf0143019c63f6eea7f",
;     "k:78ea727bc2f1d56761479d521244aa083454e50a8cc1885eaa34c355ec407d30",
;     "k:7146559dbfbe849913187a4d176147b6908264b2941890e6fe8040341fb5a9ab",
;     "k:aa9c909cb28e5ae792e8ac3c110b3d76cde962fdc08702e8f47e197022e5c14f",
;     "k:f9efaef83476f681fc4839829bef0f805d973f2cf809b23a56e7d8c336379c0c",
;     "k:8d387d55dd343c1bfe617468947c0c15dbbdf9e2f439f17544205df3577e6f89",
;     "k:47a7c028e30fae2bb090aeaf69ea72c290c304b23c5e17581b6784bf16449152",
;     "k:5478ff12efbb64dd0c14f8906a8c8d42ad9655d5ae63ec4b5fd1d6810f2b7dc0",
;     "k:fb867f6b3d68cd2eeb78111f2a6a5b809150e59d7ff955a07d8f62352be0e62f",
;     "k:6a11e0774f1406b9c03e6c1fc82c0ddbce5134a4e7947705942ee92936ee74aa",
;     "k:898f47657292eac2bfb8b65c63cbd28580b115a90fb7c442eec22c0d50e7b533",
;     "k:e47dafb8520588c86f6cf82b5e5d24f6f043288f59b00010dc9f488ecb398f3c",
;     "k:90bbe15846a389379e0535f068c6b7e2cd319ee7a292c9334458d07ad15cf376",
;     "k:1b2bfb3b612ffbe567876fa1d18a268b84cd175b9e340b9e866a8c6859493eab",
;     "k:9bd32681d3566471f1f9283d6f237aac023ecb629cdace47204891df364bd7ed",
;     "k:92e4864f47b0e0dcb4b84549309f854a16fc2256a7a534ad3b68a3c15a330cc2",
;     "k:bd336d28676f9134c7602325da44e1cb992d530ba061bd9dfd105ccce61ee289",
;     "k:0a0535c9523e9d144268f310a4722e62826cb1091b9869744127d2682620197c",
;     "k:28249e3735d64b4e9c17067251749ccc20581b1724aff1f7dabe90d444372e28",
;     "k:10d77510b05d20d17ba98e8fbe76db9acc6a79567ecf65f57d2c1f9b92b792c4",
;     "k:a86d03880d2b8273f55b2b6a0268e217f1b586109a00d504be33b88cdbc443d2",
;     "k:7d363fe52d821839ed800db6e319f92609d385c7a8285a37b1c8dba3d3e558c4",
;     "k:542fb5101121cdf19e3f1a8dc4e6fff26c1537c7d76b8f0e3df3d941c4c81cd8",
;     "k:bdc10da41cea6830a6a36cea495bf5dfc9924c1ab8e42894b92278431ad8f772",
;     "k:49119ebee1423469b8b4580470e6f0328a1a49c7a88aba0aa129c7e21a751f04",
;     "k:bf1599ec65aa2a0d137c95f6944042dde8f6046e8fdbd86c5db921ad32d917fa",
;     "k:0f81296423dbe328d783dc3896f05e6cd736aeb5bcff42433b01eeb62dfbcc74",
;     "k:a5ae288167d3a0428d8d847a3a4a11f4f3f78ee7839e87e519839be85d0705d0",
;     "k:2555c6318fb73e9bb2eadfe6269d3b1739237606c295cdf5ca4b0550b969ac26",
;     "k:8087e5cd7422769a6fb38482ca7a3c4c4ab9e9d4eba8cf8ddec3ac1958a16103",
;     "k:dcc495cccda5fd0fb5adca0d14af90c48ba88ee041d3b48109feac6a87fe06bb",
;     "k:1888fdfa1421c64cea59ea7fbfa3c7778ca2f74bb2cac320aeb3880e5cc31138",
;     "k:17170ac65e726048b9752adf38ec20c108d5a9be4ef1cbedc2999494ae2eea4f",
;     "k:aa649522377b0ed9a2cce8a856c817b2369f02322179bfc0be80092e9bb2be60",
;     "k:825cec5ccc8afd752e64ea0a077e961defd4c871fa213d86441cc3c3e51fc7c6",
;     "k:8b4cc32fc0a8d5527e782047492f3fa8e8fba8d40119f124db0743c4bcce1370",
;     "k:1c20da0b90b5bdd7436ecac07ad3130d8e5305b4113aca5a6d402efcd3efd12a",
;     "k:96c7afa210da83253138cb06bfdfb2965f68c9a294aa1f14f889f431612230a7",
;     "k:60b833c386bd56e96f719513a2c22867bcecab6dce38b4207792841d5521c5b8",
;     "k:308f41bfb3f2d2548a8a3146c726746a00de1e48f0a4566f3befbb436ebed715",
;     "k:9bd2c12bf3e221b61b01753d9a9b88dc12dcf8b14fb7fd8fcc3529bae739e9c4",
;     "k:7b1dee46ad233367e63c895d1508299b87a2f14c33d3b3f04ba77ebe0ad3bb92",
;     "k:3d3d73f01fdd7ced5cfbb546ab66f7c2692853b2d07a8a38b365c0cededbe0a6",
;     "k:c923a4caa2b9b52f23a6f860d3fcd8d9cc44ff8f579c1eb9eecd70af7dbe51d1",
;     "k:0b374765de163ce6d3370f39aa93f07b0c767d0f354e0b8320edb6a26338636b",
;     "k:b6d375036687c4d5fc38921f6ba680988b58ce32f543329563283b3e6db5c8cf",
;     "k:4923fc6713ec16d3d21b08d44e236a3663a0442797ed46c5c7f759a8519bd1d1",
;     "k:b391c89be63ad04583c830e3533050aaa2e044593b12d6abf4224fecc1854449",
;     "k:0ed8f4ed1f6f9595836476b14422b854c7002134a825339f95e7adc731f6b164",
;     "k:28dc4635afaed7b652c03a411619467f5bee5cf3bf90fe3989a577813bf44bc8",
;     "k:5c241edfd265bc57fb284ffb080af6386ce9b31d8d29c109bbe5860c71fc8ae0",
;     "k:e0490ac0de01cb1e6fc08b1154a639481edb154d83228776cef6fb9d9ef4cd5c",
;     "k:117d892628c9e6ca6d8123403b4b7962d8ab4400d37893897b2aca12665bf2ec",
;     "k:43f478defa774d71f44b09c43f3febdbeeb9f80e333cc07609612121fb977eac",
;     "k:37aaec708b0c03cffb654a5f0e0abc21ee2eaa223a8425a48ce6bf13286e655a",
;     "k:8ffd243f24324a24a665069ade9ce786c99163a75701bf63e96466110df825a0",
;     "k:d7db0bcb55bf115fac308f65c2780ab8707361139b483eac6547126e9549df94",
;     "k:498213b16b029394c707a691b14204d9e83b5206d3e552a190c61894ca21e656",
;     "k:1e62c85e3dc0d47fb5c2d7f016e39d0a55763f4ef76eecf5222ead304d5238d2",
;     "k:d0d0e5e151c4c2f0b9dfad6b20e39ff10b880cbb8e82a6cc71afd85688a9d609",
;     "k:01341f802fe5e90259f46bb084a161c17c3241fbc508a54b6c929c8812ba3d81",
;     "k:fb3144c18cd8454cd827b82b5cbdc63d100fffeae9aee4d211e1cf61dd7bff56",
;     "k:0203412571887ea1200388ead2752d1d7eaf00ef0bd52597dc04d52c1284e272",
;     "k:b689469494e29bf9c609748d419468ec733c6bfab56b13ac262ff9646bd763a9",
;     "k:13206a333de700804b18fda283f7a4f349d5ba8c80137effa3e2654d3fe14029",
;     "k:0fbe5cc68a8b1d8a617c8069e09dd8e99989cb0986d05bb5459016498650a145",
;     "k:5f35f7dc5945bbe6e1839975cb0006174559d7eb9fce4d473a98fa627bc947e8",
;     "k:55935120e47f57fa9e58aeef491e4d2f83232aa6018d7db658d4cc3c399bbafe",
;     "k:ff9b402acb99bafce4613a207a748f842d49102bc3098b9f5dae46b07eb2a1b2",
;     "k:a225f2a6b20f2d1ee44fc3942b9daddbf881a2e4e620586c8534bda22259425f",
;     "k:ddf8a1638259936d272a6a6f3381b68df1a9ffc789c24c066a0ae8ba0eee7eb4",
;     "k:ad0be6bb7db1f3c7f34fc25312abefb6f782940c71639a411a7698d43cc71754",
;     "k:348a2f373c7261e5110a9c6050dfd868d02c3281846ffd33e0eeae51d6f82f8c",
;     "k:c4399f94f83247860a06188eaf9e8360c47fca539b4b4878ee6f583653bcf48d",
;     "k:9e1f73e011d4752fe63e9fefc03b9adcf53467328633d6d32c765bab921d2b56",
;     "k:20d8bdd0d92987ba83dccd41cfe8128681d49abb588df51372b85683270f0759",
;     "k:74efd90cd9edf78783060403c23e3b3c1073b4be87d5ab27152e1f26106f56b7",
;     "k:f6e0e9910a79380309fda898a39021c9d85cdbfba65c318dbce931f29af10337",
;     "k:357842093e8c54a6321f9581d867c0abf42e38ff50000ec3ce438e29bce2852f",
;     "k:1ec43dc13c2726450d6afb26ee962d61ce60cfb8b5882e03da040435f0da7cac",
;     "k:6eb9af0db3028577cd8204112c58a8c31dd7b417c58913c133f12bc754f103df",
;     "k:fa545532c746d983814c5923ffadb89cee963d549e093652e645653aa6081eec",
;     "k:1fed4953ee5cedb29c47703580763559033d021aa3a3971ea05fb76f55773c9a",
;     "k:7c2d8e21004cacb5341463039f994b618ff6cdf4942ee76822432718da95725d",
;     "k:1184f45f4d4e47cb79be50e57372dd6b09d12979844e9f9245bc5b859a39ba91",
;     "k:87973f4eced61d09e95b8b86d7eb293d75c1f68c6ada6fa41a508ab4710e882d",
;     "k:63bc08c1e32d6ff7d40267866d5cae0ebdcd8127a439e3badd485516cbc9c351",
;     "k:cfb783f267f8bad6bbb96994115af6552d59253bb10c10fa7b164cb7374ed74c",
;     "k:86934b8e220129ece187f0ba73601354a42e981fd1daad32a3bc743be7640943",
;     "k:b8992048eeab8fbda0f54605e75135565d4389cb8fbc291a41748c36d5a10be4",
;     "k:71200cc92a5824e9d265b0da9d100c81ca3db513407b93336f850dd1dec4f45f",
;     "k:abda88bd0c9e0585e4e895ca02dc4a0160654ae57c48b9972020489f2d051e86",
;     "k:cf5c0ec2b807777cfc8f4627869193a0b314f6e01c289918faf4be1ba86b64b3",
;     "k:aa92eefb3e1e2e468a671fce4ff7ab224f4a0dfdfb9192ba9a1df906f1effa65",
;     "k:1d6166af5a98f89ea260606c90156b56dc7cf33dd9b93eb2d4f276f826da1f42",
;     "k:2dbe0cd83c02cfd9bce6a4116845beeef1ab4946480b763023af9df99d313d97",
;     "k:1a851a6cb77bff93168e3e781203e63de8362865fc2cece387da0876e246b116",
;     "k:12fccd037cf0782063676450996901c8087d34fd0e876eb57dcde54da1c9986d",
;     "k:45fa9c6ed964b429d3ed4c7ad713eed4f4d7b481aef411e40fa632cc4df388d9",
;     "k:b9f796d2a4606d808f418e694d48d895c721d2893aab491dd953d5631a1fa530",
;     "k:385ace80f603e989376b30203c277a2674f2c72c834fe36231c4e018f088c855",
;     "k:0628b99d30fbd9c7acd8133e2ce23925ad18f24129bee57b440971f73cc86595",
;     "k:4ef38c4084aa7454cd593387c8448469ec698139708be34f28ef1888933f04a5",
;     "k:f146cb38410fda9f3f62c6cc58ccfa257d019276f0c7a36a5c7130bedfe05964",
;     "k:63289db9c255833f486b126be26afc48183fa15762ee09b245528fb02718fec3",
;     "k:a03becdbad1a5da4f8db3f7a8d135a005a9c93b2aa091fe3cfb8dc4e79a1ef31",
;     "k:f5f25a2903a192f082c262d3373057aa5978636d0a9b939b93d2a4e2369db3cc",
;     "k:287c5194a56a324a6d7785ff690f253f3142384295e88e2b275e7f17a2f8b922",
;     "k:effca1216c6861bc3b5e2139df1403359fdc59ca36e90af891a18669a387bfff",
;     "k:064d83d0c21123a3a4d8dda4d2f45e64b1f90cd3cead7df05919931052cf2b26",
;     "k:fc07793e4f2541a4c521a1eede44532f3746a0438fcf13c4f21868465ec0e254",
;     "k:9c4b1c4cc937b17d9d9f6c0f470b4d86d0a45f9bdab6af52a89ec462c18d51e7",
;     "k:4576d6d23a5421c6bcf3eeae7d3be86ebb0f2dac3b04064052283b7e42319d2e",
;     "k:c6d7a342ef6778e1a15b569e80f0fb508adcb522e325a7ac4366b9464caebc3b",
;     "k:da403a206fddb55a9e1283f8839c431031f7b94db1dd8899bd9e791265189713",
;     "k:5622bdc9da695ebd88d4098e60aee3d3d741a90c0c635469a8cfbc94c736d2ea",
;     "k:96a82ab7a15883264ec294f43a2b81ba5c26da04d8ced3b121b2d569e2f6b475",
;     "k:8f3fcacddac8dbc214672c0c855d04497963869eba105e2b4ca72269985b908e",
;     "k:d74965cca26d9f27e5f7980e3650e6e45a13b24a861b3fee07651752700a29d7",
;     "k:d6791d672560d034cab59fcba1474bb45b406b38537d65d73102a008ef76da4d",
;     "k:39561990b7b1f50e52faa3a266daf57a8ea9c9972a3a122f2ce5e844b26d8604",
;     "k:cbb499fba5cf85b073f5c9336a374bf1fa7cd1e0efa6c82b2abd8dc2a948b6a6",
;     "k:db81275c69a9eef0e2f4e17c40baed765a9d9debd78e5d99109b1f293729a111",
;     "k:435f954c80e7d9d576b18c4bb02571db3d207f39918409108f4cdf593a7f7c6c",
;     "k:2e04890308b7a1fc79a5987c6e68849bb5e8044cd7a4d5fa8e8dee1e2be57abd",
;     "k:43457cb388242f9e3ab0f6eefc8cd0546996396481c5925a136a1682f34f3fb3",
;     "k:7a8ceab609ae334075a680eadaab1cf354f96753b44c7b3a5c29ba1e8d73c06f",
;     "k:5d487e75525992036105a8087893a067a892ebf3c3ec1d38091cf2dcbaab8598",
;     "k:b896a46c1ade5ecbf7b5376bef02d2003a41355c3582167f41762d13cb4f617c",
;     "k:0f988812eb7ba083454e27e79ad02d8ea3c409735e2a9f0f04cfc68b06e1fb3a",
;     "k:31988ee96418d73eeedef1fe1622d3e2acf91335e2d16b77ce48c76d0bfdf801",
;     "k:4a0104f1b29c0e5aae5213bc848ac32aa8cbde005f19504b1469c6ab5e49d5e4",
;     "k:1e61b45273597d03e0d7fa5a9703da62a9cb4490f3f77299e454996f1bbbe6d6",
;     "k:a666a347cfc93e75758015e6f9dcf79734b7d65f79a06dc53da8036107e9cd9f",
;     "k:137ae1530c42757ee7672994624c1909d0b9eb80ef560436898a1c0667637a41",
;     "k:41c65ce556a0ac400480b7fdfd58ff122c3aa1ad4fa8352187be549d9802b55f",
;     "k:ab65056e23df533ecca172a61a057193acf2894eed57b107c451029d973cb83a",
;     "k:42061dcf075919be6ab376d95a766365f357e5dcca702c2beb125a27361a3ad0",
;     "k:f7ffa37888b0eac3c632e7c5a85466ab554080e39d58208258f5149a2e3152f2",
;     "k:1953867a2f1e7b1e2c3b9700891cbbec4b7907d0c0f693b19e43c9b3493fb97e",
;     "k:c0b72a1ccbd8eecda480175834c533185d1ed4e741568dcc805b3c21f5a8958d",
;     "k:95ed4f4f2047af0e78c620516afe2779a33d89829cfc5faf05936d6d414d72a4",
;     "k:9a0e7d14f9470a72d5aa5c3283b4aa505f9e8ab2bada69bf0000224ea3ca09fb",
;     "k:4ce351944a065b17abed174108a1d428e03bea539d7b05e36779da075ec0f5a6",
;     "k:4749fd6c4c61d75b2ddf55023a06125ee4975571e2783e4f19b9bcb9b536c798",
;     "k:9bdaaa08fdb1584b6fb00f670bf9fdd902a051810bbbc947ca9a52849c1e31eb",
;     "k:96061f9683fb75d1bc892671ef2faed2830f868e69f78f3d527ae30ec2698a1a",
;     "k:3217f7272e3d202902722c8a3f432c462df6650a7dd39d9dd2ebabc8f271aee3",
;     "k:8d62a4a1382b9f767ade2363b621519f66ef225f583307dd8a88c56898e52eac",
;     "k:33b8e4f92d68e4dfc992cf736f214227cb227b35411c58a9878d39e17565506b",
;     "k:fad6e0155c6328feb171a891d77dfa29cbd1c944a256b8b3f62a9075619ea03f",
;     "k:6a2c5072b6c644e3e8d8def30bda3322168fb67262bc3426416be6f388e57e93",
;     "k:1dcdce84018cff475eab887e050cfb02685179453692cb64bb21e667bd9a0526",
;     "k:db59f36107adf6bd9624b7d6d5f0839f335047043367b21b5d5d76ed161ab404",
;     "k:51440bb6509e732e1a52fdd4ea356ae6fb1a0214aa4ab33b753eb8a8652a8afd",
;     "k:bfc9c3c2eb7e08b6689524f96f58ad8b05988be26200081e707dedc9d6abdf48",
;     "k:cc1bb1d5f3d99e39ac405fb5696ea8287061ef4bf772e363557af576c2b83b72",
;     "k:3bed738cf57706c6b7342e41e66ca354790fed453b11c6a4713998947fc52b5c",
;     "k:9b22246b384b2bd67db472e246894bfab780376daa3cf653a2ce4583388373b7",
;     "k:fb8d2580f3a9f527ca2de29ecf2d8b03221f6ae807ad5756a52ecc8af5350c8a",
;     "k:e8d85faccab1ecc676ac104e9a10d6f6ba705e5dc67bbc732c7a50885b771653",
;     "k:a1434eb6e58ad43966770a5b8a8738e7f458daa8502f398bc0384730c973c0a8",
;     "k:75403ea16e57a7477d304770e7eb1018faa5164cd592178d06b574210036663e",
;     "k:f0cab5abe720f1d1622d259efb1bf98e2b767002619bc327bd86fc80936f01dd",
;     "k:9ca5f80cad4c82d397ff4b67c9d229fa0629284c414e47db58879462a80a2b2d",
;     "k:42824edcee034930e2e5645d9b760996ff97473a6233f3ffcbb5615d60e31961",
;     "k:8d99a7c82723b5101f04bb73f3ad08646f64a3e9d17ba9efa24855c0da707eac",
;     "k:0e4b9dfb1dd8886dc010577c965bcb0d8830051cca53339a31a8b99740bcf4c1",
;     "k:1dc56c942383f37edb4990cf7422c3fc7818faf28f9714f96534ec070aede1c3",
;     "k:56ffcaf186699343855c3620f2a1f6662b92c65f44667bc746010ac102699013",
;     "k:bdb2ebf164d4cdbdcd2b2f26f5ee239538d836e4c643e6935e3306bc9428626a",
;     "k:43918883ca78d469a9e936e7fe0ab22bf94beb9142f1a990afc88bf6e11203a3",
;     "k:0ccce1eada7a8ee67a29e6a8e7d4cd60ef6e81d685c300689b429f85d6890a62",
;     "k:49f8c6b548d816c469f919f40c67a513f7e7b47ff620ed4a3322ae96dfb8cabc",
;     "k:51fedb60930e42d428be19fba87c91012b0aeb2bafb270a2c9f5cc9ede02d1e5",
;     "k:61a4cec86e362a1737d2723101f758db4ae96447bc5046387be4f302d7bfc47e",
;     "k:51e631774d78e9d4f4ba47ac8796264220d86c60649e40b54e3dd14c67902958",
;     "k:b2eed2c05a2df779a101e9a8dc87db1f859989be31f7f694894f43050782da4b",
;     "k:dbe119e0b054675a4fd87c636d705e27f6559634518f6600648edaa3d6d6253e",
;     "k:82bc52834b676550fcd4c7c08ab3b35e7f88a434f4ae82e27573ec7fc5f8f222",
;     "k:da65af4f1c8db6b08ea4383c9cf4454f892376a55223c8f46168966069cd0e02",
;     "k:6e8fdd3da0a734cd9a382236f60084fbe4c9730751c3bdf57bf636a80fc1b4cd",
;     "k:9ae210ee10272648c42e6a56f2090d014936b36cc985e0fd1b5618d4e253f2d7",
;     "k:4af8f1e4fafb0eb1544c0fa043b8229123cc17d4c35558e21e29d1c9e10bcb2b",
;     "k:c6d0e23edc63a131b72921e7465ea13bb2e79c3e58a6753edb39f7b7e21207a3",
;     "k:e9d90e9c00f419d7904c5fa6891504ec385c87e9c908fbafd06475ac3d635acb",
;     "k:92f2d8ea053172469f5b2784e294977240426f6e80dd075bfe825f1868a5b0b5",
;     "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad",
;     "k:bda9a99f2ae934a322126aedb88e3d76810c04d8f08ab156079b14d781e54b90",
;     "k:896a8c744852d89bf8289d74852b8bec996a12b1df1f97785a6361cda1505721",
;     "k:d23297bd0b53ee344780397c6355a0b61970709eb4a9da7ae6f8d6e8385968fd",
;     "k:cb5365b6cec5d8a056f4de8d1c280aaa01f8c3dadcff04e449fd5d1e8c7ced81",
;     "k:5a2905f168c3ecd2abb33ef84a3075db78dfbe8e15990afab6b1ea9771ad56a3",
;     "k:b2a5ff7f901f78b5f361efcb034c622c50179beb3f24a779039d9d01106096e0",
;     "k:80711e64bc0435dcf891dfdeda010c902210b77764a236ac9341f1c5f0483f99",
;     "k:237f8ae6f79e8b7a7f7542fb6462ad08cfc54131312c6eee8ce94eef0be5a5cc",
;     "k:ffddb622111626eb3fefc9a6c9e6528b32e8bc3cb96abbe198d00f8813b3e83a",
;     "k:f453b83deb134c0b3a24b9e2182d315c91768cf69358179ddbb262dcd3f56af3",
;     "k:c60ff959741f5baa4c4a5ca9b6720a0de2db9955aa1df4c46dd06ed2fb24d294",
;     "k:9c705e1a209556400dbd78267f065f8e4fe1c4069aa6ea46563362b8f3e72813",
;     "k:ce73a4090fab9660ca6be0822043b2b56f18f40964c70cfe756c461d64cd090b",
;     "k:d0f08e2d2fcc99aae9230e127d0503696515c3b8e34941540c2d6c9493e1ded0",
;     "k:da101ceba1f891a74826f6c185ceb27167e4871ec6d75157ca88a5f21f2f50e7",
;     "k:793552155e1c21a204762b662d3a3f2d434dde4aa71039718e4d977826deaf5c",
;     "k:bde91e8583cdba11d332192bd5e588b7cb722f9507fb355fb1ebbbc3266b092d",
;     "k:70d6e632d45e03ed54f1207c06f1ae7a662973d57b3b6892124f24e45ba9fef4",
;     "k:92a031438f60af87a1c2cfef9633d4448632dd901df053261bf0a45294a1dd77",
;     "k:de7f9922a147c2b4aed472614e1ee72d1c36f8c1cd97822dd73b5026292f9c70",
;     "k:da98bd41c8e37347236fdd40f1534a4781230362c99418a17c0b8332318b974a",
;     "k:dca12d9eb690fde7883ef42d3323d5c1cd8f417b00af82939e39a2bea0027f1d",
;     "k:f45c5fadf0ffae3cefe10ddeab39ee785c0a587e38006c1f6d9d5610172eda41",
;     "k:731fddc64e047a010ac65f4e7d338d92bc804789110c64c4097181911f3d875b",
;     "k:83b4397adac0c3600e124f84135d0ccd6eec5db27dc4f658f0ad400b22a1b056",
;     "k:bcc9752b2aeda00a55000d41d03330f526d4216abd696ec2edeef2b603cef712",
;     "k:7240a14ed19ece4a8333ca3b07469c6321495c17ce71addea89c736337673581",
;     "k:39f538e7e77788ab6306083d920b5e79fc97d05409febb99e7de3be4cc172cf4",
;     "k:e1f166ddb801ca56e79afdf56e5dbf82c759d9accd6ae7da3ca650bdc34eb79d",
;     "k:f0ee5cb981e6133acfd7e604829f351ef530042c31cc7aac78327bedbfbe8c9a",
;     "k:6963b07d737868c73399cc7c135d2d6f7f38d942b4fc89c5df767aa06ba92a2e",
;     "k:706b80bf4402630e6da8d37b4e66faec19f3ff5b6c503725e78a95ed33f2e14a",
;     "k:f141a0a824d6dfc4a679638b8db87afb54c271d5da2bb13af49ce70268920d06",
;     "k:e61108b15a8c5b45d7593ebdd1e66c1bd0d9f40cb4190d2e68c922355c0bb932",
;     "k:7c2b45ca3cc2de600673cc9012f4bcdc055119589799a61ac152b0fc8ae300f9",
;     "k:072826b3bbfe225a7e26edb4c7e1514fb22cbc08430685919a791c0423f573bc",
;     "k:76fed161f3465e74f6e72b328263d5f3c9fc57f717d2e6006060f15baa19ee5a",
;     "k:cb0b1715db8b68b53237ffa8b2d84147bca567407deb5754f914dd3f0758c29b",
;     "k:660390a3cd7cddcae92063e460e2daea46feaeb73e37dfcc693f4e0eb5b0148a",
;     "k:0e63fb7781834f61bee8d80bbf63173f4b6ebec128533591bac28335b290d1c1",
;     "k:50acde59513c708b0001a9f979319277635464c255a1abbba0ed3da9b0ef80d0",
;     "k:f990ae3994b64e463df0fedfd600e166f38d153656c51483e284bfee8b77cfa1",
;     "k:467aad0672810625157f4330ccd08d03dc3fb0dd0863349c0ed35ec63ec7ca7d",
;     "k:d3451d98f75bd0a25f9c5f2ff965759a696f081997c089856f4ef985d302449f",
;     "k:46287bfd08b2e39c4f43b97442d48e339bcb57ba942dacbcc2614879c23c7c48",
;     "k:2ab0e68a0491570aaa3ea4c39eb1dbc9921decffd3bfcbdf026d9f9880a8cfef",
;     "k:3e7abdc05408ded96b83754c5fab456049af7a1ee5c06420794ae58899819c0f",
;     "k:42799e7135f7a504ac82b6795f2aa77463c7667f5d27a112589664602a0c685e",
;     "k:7500836ca8094755b154aff21233440c048b11d80c8d52dd2dbf0e78781a1ce3",
;     "k:ff996648154058cf90cd8774c27816d1d762fdac0b0c19d4823fe1f52e8d45f7",
;     "k:1b79a30cd2cda7b394b15da79536b259a35310aa4b2c621b17337ad78b7558ac",
;     "k:1f54efca51b2c084ff369e5d4e43ac1eb65875779d11a1cdec946ec719e6577d",
;     "k:e6576db09995c4f63e012b6bf56a7e36491e7c70f4bea60239ab2b5b793905d9"
; ])
  
; (free.brawler-bears.add-to-wl-bulk "wl" [
;     "k:cf45c0dedadd3616b19ca1ff7ceccaaa657ff679d275f8a69e14d3c40e931e49",
;     "k:8fe49bbc9ce462ce1e1ce20f0878e09b42ec9c2548836f7587f7a1767689e8fb",
;     "k:f4b05cb02f449ff6a2c21d1c9f0aee1854a85f59e64f30264856a8c5ec0f52f0",
;     "k:e8d149f3cf7cd01e6bd0fb056538ddc49657b2896a35cffaeaf65eff7374d801",
;     "k:4f95c7bffed719b6cd0a57b21b76468c32b46ea8f46886ed879bb1e81172a7da",
;     "k:0000000000000000000000000000000000000000000000000000000000000000",
;     "k:eb4adc3cf8e69e26a285215e16aee264e0e70a8153f8f7c7d2ddf11046213448",
;     "k:1b9f848efa172201985deb428c5f603f1b95fc4285f7ce10865f6e1c6344f6c2",
;     "k:3652c9b85f7f03b5af3caa664db3951ac3a7d91f7ce79a6cd7d2084abc700917",
;     "k:b564e5a6276f8cd3097ab778e3d1c77c625de9ecf2e510b7959158c9bac2f33d",
;     "k:b78e8b0cb99105bba985403ec86f42001df6069fc7933d520b1f2407d0a09aca",
;     "k:cf92846520bf254bb966f6c7b88a9185e8689e3701a3a0573b8cdecd2f16cdb2",
;     "k:e4710a204803b7927f3c9c4f407e7d7690f44a09d5ba4da1034e4526150eaf6f",
;     "k:5eefaafa7f41f238c168196b7e574ee0bd050fc180c682811ae7c95c5769be99",
;     "k:e94a7cc28f2eea04027771e86801a1b878d10b9182f84df2ac59668defb46a66",
;     "k:bf53ba90aa4e76f091e0dd6f25e66e39f1a7e0a06d6c931194825d4545a46fd7",
;     "k:279176bc553dc28ec5948f3f21ab4bb6c3367a5b5d23f8f91656f8216b83890a",
;     "k:8a4c46fa2457246ae5de1129bb6a24570f96cf4fc6392b9ac02e30273c303187",
;     "k:39b6efe6bc1f3b56e9a75343fbbb8e8e177ab0c941c7f53e9b256116d9bc2cd8",
;     "k:6e59ce8ee65032145b3b65928730fb559861e7842e67649de1d79806fa561336",
;     "k:f83944631c1f1b802880cb4592eb82f675113cf0e63da1bd57aeae513b373e41",
;     "k:151547a3f0b2cce979c8d348cbeb28cae07fb658782f5e34b0c5f80fa7622672",
;     "k:f53ea8fa48c0d0f35dc861e49bcc3bfdf57db30f613d705304319cc291b9bade",
;     "k:fa264de39a5a8b6686646fe2a470352a3cf46069d90cae722fad4b21a1a9cf44",
;     "k:7031df2b61501f8433f6ea9a45e0c3da913f36a86d18c69a371322310682d935",
;     "k:e55caea429403e774879d10ffd2c8d307d28655630e8542891480c74cd692d7f",
;     "k:4b89f4597a2c916611f4041d5626077fc6ba212fb62b93755d80216e9566e14b",
;     "k:22ce975d3132bfd480191b1827904c7914faa3e9a6a230b3c8e3ac947f2df560",
;     "k:efc25ff8f0033dbdbc2489d207c96d0707b7aad4500429b295b1e1fb0f536c78",
;     "k:caa07b63ae96bde71a7a11cc73b15b50ebd299d939f39a53aa041db1cf036b93",
;     "k:4cd8628733c96a29fafa7590735b400e93231d294f1c657614cec750219fb288",
;     "k:4ae77377140f260ed6d43e1f5ab6661ec01d1841a7366ae776b05c2bf500c88a",
;     "k:609f06e293b58a87769d0a4be3d5eb7efcbdb2826d6ce27bac16810f74453ebf",
;     "k:c4e2bd6019aa2a09a40bebf691581bc95220cb1baccea0a6884c3165939eef2b",
;     "k:0ec85cf5e5389ef47a4907edbf4985c70a40e0dd93211c254cb6ae32ae07e9c2",
;     "k:e44e06b09a09d0be76d4d3ae12adf1707296d942bcde91d0e067097e0d6a31b8",
;     "k:e34bc3692632c4e62aa349f82f4a1f57a1c04f85374d4bc9199a0e73207a8649",
;     "k:496abccf006c29971b146188b69035eeede2a4cdc7317dbf9246e4b519aef17b",
;     "k:10ad0a6c18322bfcef643a7cfcb8e7a15c59a0e0ec4b77bc34612f22267494fa",
;     "k:d0e8cf1f196d433b86cf889fba6cc46b44fbba38966753613af0b1c4e977745a",
;     "k:9c3f22f38a0f14f668cab1bcfc0d631c3046cb9def1b173b7bd2ce36f52d0af5",
;     "k:81352a0b73f29776edce53cd8c5c925f88061dbb7b052d1560241100f1b33266",
;     "k:9c2a5c328bbc6629e8b355e04e62fdd4a312687431b3e291fec68d6166ce44ed",
;     "k:c7d27a84a355497e5a13adff2d2de8d69227c31d172b0584ed8278c991bd6068",
;     "k:3b88382c601bcae12cb2ffd2cde5435b381f7ec204469d9cebe6b5cec5879b70",
;     "k:a2af94eea976b4457370aba66618fb9b9392e2a0a0bd3a90115109affacd9e78",
;     "k:51d78b0ebeae53b5139215b8188f9cfd7c632a279e993e61c948dcff2cab8f50",
;     "k:a2c0fe6f872d27d7cfb9255dc65af762925c06e27c58fd8e684b2f9d5bc5210a",
;     "k:461d8cfef2b2899d9f50b29022467877028880c4be527dc3bcb624c2b8ec2222",
;     "k:3ab114faea8b2b67f69035c302da81b3099ae79213969ebcd6fde69861ed7403",
;     "k:df514f2658011d4f12021c5fbf08a2f3f55385c062a8d8257d1cfff3feb74080",
;     "k:a5096a2196b80dd88390755e6e57ebf80c13c40721cd4ad1e381b9580add5641",
;     "k:0d569699c52f85be0dd6476e859ebf11f16feb7ed00aa1de75b2efe612e87bdb",
;     "k:0e98221c53ec867b3d6f25b98dd39fc19cba3fbd2670227b25f100bd24c3278f",
;     "k:b16cb0dfbc9ae52345ab35e4d7e6246c4d02453a6ff15a1c191ba070e0ff6442",
;     "k:60622a7c80044faa1a5f53ca4dbdd3a357350321461453f56503fab5fbc3f1d7",
;     "k:8edb2e0be6314f9232e3400d4e42e85f4ae56539c77d4acad04d9b815ae92b6f",
;     "k:0bdedaf1f655f234f48bdb4e48c399244df277009583a587ed371f4109a7499c",
;     "k:030b4c5322dbaa42fef2541302cd29cce1b1a6dd9a0a1755fd7f3da9c2487546",
;     "k:ea406207ea0bce272c4504799df06a540a6c22de6ede0ad4177a8d69e4ad01f3",
;     "k:6b839a681ca19d3dd011739a80f75eb63476f7b88ba0da380ec6300193688eb7",
;     "k:6b9375503533077542264b91a4ccbeff24b3b26c3f94a7be7618659948e164f2",
;     "k:42bd114c678d96731c168bf5115df6f8cfc75fc84968e5adf87b0638aa3a5fe9",
;     "k:65b94ce29e17ff28132ecab8df947a421ccb922c784e9053d024b2046db20136",
;     "k:d3107c2fd20e7529932b114cd2818a404ed090e863d3c1f4242115480c60784e",
;     "k:608a676057807e959e4527dc003e146532717eb6591f5122f679b8f77384f7c0",
;     "k:a5d014f8a9755b7d6680a6e75cd55f56eda00c573e03dd371a9f9187e3e53308",
;     "k:daa20df95c201ddae138d619cbbe1193374ba34e71bfb7c8b92367f41e03cb1f",
;     "k:c1d8b23bffcfaea3ea27b7d4f219b529acc46419aa7eb5b3835bfdcd0e1e6cbb",
;     "k:28cc2eea3337ab199e76b337d10ca30fa4eb5c291922ccb82bbcebbf007f0c4d",
;     "k:1e086cdf2ff0e95dbccd8bb27c628f77755f36957c3b8420e62dd6c70db82267",
;     "k:347195ceb08c0b7cccd8f350b8de114e4260117b7c28eaebbe22d63a2326decf",
;     "k:e8811d60a44cbd0f11bfe92093cb5ad77719916c7d2480470c59a9ade0a39908",
;     "k:e9a965def5e7d637883f792bfe9e057573654cf0cc4d2e9a7ac331e98f6701b1",
;     "k:1d6fa3eca35d64ac6ec9739a508d8a6128fab3f2c07979fc8fe13edb3b93a0cc",
;     "k:f9d4700954bb7befcdd1741103d123e3da4b04cfd1f3642f96dc03e37accb932",
;     "k:803368b8a064eb8f40cd7004ccadfdc0500b0423d1736885f4fc1576478d5c22",
;     "k:c07835009a5cddaedf30f0d740b7e3dbc27224d17c66ad8c56ae68291900215e",
;     "k:a68c556c191aa93670c09d930eb7ead38c90a8df5185a0ade747f803e81fc921",
;     "k:4055cccadefec4f9de99c30135f3d7268d5e21664d3632127612d4031060f11c",
;     "k:a23cd579e8759b6ca071aed4e3072bb50eb8cf852025475d9783b0a7d4d350c9",
;     "k:c5ddd112e3fe1e062fe576a2de1aff398960ffcc220e3ec5011ebf01618999ba",
;     "k:51ffb129bfabf2f011f623f4d52d880db481d665995f32685630e280457418b9",
;     "k:cdfd5014a06b30f8cb5be7e5d001ad7ab54d69fd3ce304ea1d472865e5e65a37",
;     "k:c7e8eed701865b96bf95bde11e7a9726960a0e1d894a76aae1dd4decfb43f9e7",
;     "k:b1c26f948bb0ee669aa19b26902fb0ae2eb2029b18fc3f36a3c0c642a6d652d6",
;     "k:5df7fc29df3b3a582fffc8e440331fb084f8f4bb390d92cdfde6dbbf0d7898fa",
;     "k:2feb222cff0338e94f8e3e310d285ee89e70f2e5ddae906f242d8f89bd83cf51",
;     "k:00ba6898d51288592102ffbc2188e8a256ab56f6e6f74df8d50080739b70f8d4",
;     "k:e6dfc20b784ef349fc80abe1671a9b3be24013cf8d8206da61ca42b169f94e63",
;     "k:ab27dcf2217761df3724f3e2c24bc1abbbd667a1204b8ea1166c406dbd65e190",
;     "k:37608dfabe26b47a0d179597ec06f6683f6a4635b9d1c60c062257d6da749807",
;     "k:01154326f515d341f22e299e6dbf8383b3931d7604940092c8d91fd59aeedb8c",
;     "k:d9793310be765a36be98441bf6ab4c3c28e876a220971037f0a0f3e7d4e0021f",
;     "k:aaeda5f42e73c41f02d0a86fb9a55fb375be1b4ea61154868697d9b9cb8d56ea",
;     "k:33962615311d7f15c2599c624e433f4a7f4ba46713e01f677256af13adefbb6d",
;     "k:c0767946329d962ed6b3ba0ce8d35d91dfbf9d9bdd9349385f585266c3185bab",
;     "k:4d708ac79db0bffc4a452c21113a1ca9bf3355066f23c8b2dcb288f7bcfc04c0",
;     "k:c09e83c412e52ca06c997ad2d95cdf0cdfa4eff99e822d2913eeb297604948ca",
;     "k:af5b6b569365ca39d4ae5d7528d0527f2f58560d2b84cf4ce705d25802c89cff",
;     "k:4cc79791f0ec1bb25a86af02effd2dc22b0aac7ce8a6a0494549add8506ed956",
;     "k:94e95ec030215b79ed2744e3d2d0ae1b2a274d23e2f090f3219d5919b751cb8f",
;     "k:0470da7c25eb6ff0a55c93ca2d8967e3ae604378cb1f1da02b15a3b0a3ae5f41",
;     "k:8e20d9bb99570eec4207f5fd26a7025494cd968b7e4e02b0feb1c9164d2fa0dc",
;     "k:371bc786a091a335fa47746ab7ce98063a82ef99801e7314866b27769bdda0fb",
;     "k:e0a3593a361ca506a989cc4a68c472822dcdd62b3c3e1d9a20573d2a78e271c6",
;     "k:7edace7af7c07fc72c469c108b98d04e1a7128fe5bab972c2f85c46fd2211b06",
;     "k:745c5fa28469ec6cc153f195ff8110e7d16a6cbe7b822a5a19b51d5504fe2eea",
;     "k:962970b0fe4fb48a3d1b1261fdd0242827c2eb961fd557a23222c81217f735b7",
;     "k:31beaffaa63c1aeec6871259c33cf47d4d9353535a3cd5af4a27a883f68e3b0e",
;     "k:be98734f9710b2b8dfc2c700989daf29dc7f44037706b6b941ebf374363087f8",
;     "k:981204029ddf8e22fbbbc1e2dacb3573baafaee4b952a44b45d6debb9f7413ba",
;     "k:3cf03618c887e7ead7059eb12df37b720484f1db30985f8351af34ae550704bd",
;     "k:dfb883d99b2f483a9d04eb130107fae4d82725f5536009ce910d384c72ff9a57",
;     "k:613736f5d97c15f790238bd102baecac3b656cb7bcbed38ed36a3977906ec8c9",
;     "k:58fe9765290b2bacb4df1c3cb3a0f25c08ea062fafdb5fbdcef4fc73d37c0953",
;     "k:5f43107e147417cd213bc4558068914c3d2b3eaf9008ae93dded473bfc52a0d2",
;     "k:1e018adbed3369f152abcb417887ec9fddafebc897d5a0d222b6900d9860f8d0",
;     "k:c07d4002cab93047a195a397293020cb40594ba137558a99d794cc9e70416f3f",
;     "k:e27c0d365f14f16aea5e0b711ba5c0b1d9ddc2dab78ed276796b65f6e8eec61b",
;     "k:ceddda8245bd3ce1d37a2122f0b7b10cf96a77fc46a74d60391e3703a06a2189",
;     "k:ea32b026ff1e127abcc5e6f7b9c5cc82a34ccf9a919ee62e81f1948d5171b8c8",
;     "k:b2ffb98e0d7ed5e9063ec05cc2389ca761e4e7d0652128ff9717a8b7f1831f69",
;     "k:f507cad88e2778606f50fdeb5fc0b349c672a3c978ca4a40b70b26ec259b0ef5",
;     "k:e7333e2e7caa341debea33ce392dbf818c9146ec777af37a34ee188d7cfb637c",
;     "k:38504b0c85d00d40d57cf1db327e45b75f002f667fa4b87136507913c1644cb7",
;     "k:01b6620b5d3e5f9380457ea358599834219dad46f80754adf38a97ef1e9eae5f",
;     "k:e69ae8244e9289a593f3239e2e00b9ce478723886eaa36c182bb3867b45d2106",
;     "k:ced3322b069803f4df576b1bba4d1581fd7faa19fb3f5144f30ed1e082881f59",
;     "k:264724dd448e1444a5937defee80f429c92396400ba7c9b1f1bc4febd9052ce2",
;     "k:c4dc399d5f569c1de22bb7621535db0a7c04c401aa1af6bedca7a73f046aa65e",
;     "k:c39f1e22d4584ec8c5f71e705c929e7eb4724be25a35259a8bc787a728d21f2e",
;     "k:69be6e647f7681a836a44be13d799a057b7e703a89c32c15b46be48a936729ff",
;     "k:126768ca89a180bb5449b08e6866dd104f99c9ba8ace6dc5f2b4e6beaaf304da",
;     "k:679285a35ae98a98cbf7b4187e855e33a5e24721c68d8d6260f54201efe92977",
;     "k:818d0958df629912bcb58e17470e0690ff9d3d1707ff5baf5664d9853311d1f6",
;     "k:ba0bd65a927ca745f04f6162182b5d4e04747ce07167be0652339447b54f3997",
;     "k:8b4cb923461cfc514467ac2238bb41d6c56ec71cfd438ef5109cfe7eca94e0d2",
;     "k:9d46d6b6ba34ef14378b11c28bde3d23abf02b07973f21efb4d34106f6434d18",
;     "k:d7c28cc81187266bdc2fb014ff8ef3c179eb1dbf53bd18201fbf68394e33701c",
;     "k:b351d2b0717d4b0afe27305eda4160840a88ee74d18e22a4fe2526a30ce39d1e",
;     "k:9417a4108d891d10b46d2ad1424330e586b2687bf77556d75290cc0c282131cc",
;     "k:d5c5668fb9917a8ab80f7a567887b251c73b0878a282d284840bd31604a797d3",
;     "k:e348cf12ac5806d1fb0c818f7aaffd7491f4cb3f8e01a51b232e3f4a8ad3bd1c",
;     "k:35bb15997dba4fc963ca592d68d7c9910edd9920e520ef1f6223cb72ff0c5b9c",
;     "k:0366b65f41b3c6498a4df92194313393c3581e4c30143a8f6ce750394c141812",
;     "k:6d16b2a6cfd8fc658c332b14072e67fb05aac89035cddae69d4e5cdfbbc8068c",
;     "k:81f9b9fe9b03792cb75471b0574b8efdd217c81784d4e4def4e3bf9d3491640a",
;     "k:7403db3854a575cc9572d99c7f4104e91756080494c1372555161cc3f5dc118a",
;     "k:364528349ab30d2ec34f42ae6b93a0253a0932901c2e494b8b4398b4d93451c4",
;     "k:df8fde6727744604e55f52d33f4fef8560397d3aefe7589f84289e5eef93700f",
;     "k:cab336600b583bb76caef7e951088878f8afe1027fa709ae3ea797fdcf41e605",
;     "k:cc136ef83675e50c69426d84468cde91d5f4612a74eaa7aeb313bd7e943948f6",
;     "k:f9cf054e796e1ccee71159ace00bd6f3a98c8c04cfc998fd423969da255151d4",
;     "k:e0d123b5476efccde600e677232b8e55593f7cb78973c67c08824979fdd0c2f4",
;     "k:dc2e58c9426ed81525a864d846f6bbec90f3ecf4a4582b4fc69a0e215e50a0cb",
;     "k:e1e37bc81ac2691b8d2fb98a9b8076df2bb66b47f5e7d4e34c18a2dfbd4955d8",
;     "k:e64cb7a6d16e168d4ddbf6dff38155de76ddb8f3f6f44123159c05a2ad76fe34",
;     "k:83899c071fa6bb04e35171c79d5435ceb7a603af8303e8f19a52ecc57b946d07",
;     "k:91e4896ec6bf6a0f5df565d90d7a2131a8a28582ae5381ed31f722cdcb2571ed",
;     "k:ec78da4a4563b7ed4245adad4d457b73ce8ce00b828f2a205bf2ab6a1672c43d",
;     "k:d264bfc0a22e2f5d1e38e0e7e9dbb0347fa5beb359d1a51dd0c99a53bbe565f7",
;     "k:c08d28082dec0dcf0a255dad79bcc7cba2f73763df5e789be4ccc3b98e638bd7",
;     "k:3317d361c8630d424786c1c9c9e567c08e65563121f7a528b3c5f97b55e2b546",
;     "k:1f8300cc25bf3e5f0a5cae3aff070d5d7660ede6678f690302bfed6c9bc1b397",
;     "k:0ab011825bca9e86c99313404e5352eeb790234c3cbdf04314da12c016574c9e",
;     "k:bf9dc54402dc72fe6c2e34b6ab03cff2cc221b1132e28d55ddf2646ea306beac",
;     "k:258b003da16b0eef1a936b3520c894ff6411aa09cc71a34551ed9b776ce69650",
;     "k:ce4bf6b70b85cce9cd7032c9b43308256db3b123a81c1e4ee1c0b0a94c8a6167",
;     "k:802d5ccbc769e6299a351201019f8f2e2fec0555cccfa1767cd184a1c72b1c3d",
;     "k:a9ee84119255537bbabfd44fc5de9a7449d3e515c013c9e1be513d1f0f314d67",
;     "k:49fd53ae2e468e74fd2190af4db0702639805085ab94a6b600c13a96e303c6a3",
;     "k:a2f45ebbe8bc0d85454946158d5ab1fead8f5e2b41850963e7ed059354344c1b",
;     "k:89bbcf49dffd11d1f2fe1551247bd1ddc4a98448bcf927ce2c374f67c4292480",
;     "k:44b41b684554e33c422c1fb91d5cb9a9378df02c6cb240ab5a2f1b67aeb039ff",
;     "k:2c477cf53ea46076e7158826800dcd17729500a5c28a7d055bddce0a237b870e",
;     "k:f208c93260f6091fd8abf382512899b2796f3b1a068985d9036978d490c6946d",
;     "k:e10fd94cdc79cd498e8c40e16141703366f1f1e89490522681f68965964d738a",
;     "k:0b72b1c68f6f22cc71eb43bfb220b3db865b776a7d3ba2eee4e5a9c28f8bdd26",
;     "k:6f2f5ccb39372a86a85176be0850669139a0f35faaab9467085d9d6f755c4a0f",
;     "k:57577f5ea2c7ff0ca7e85f1337b87ac78a03b37dcbc04fc190be99aa0bae2966",
;     "k:018c6a1b314ac29b3f081aac4529ba940f833f5d951c22a6302d3e52f198f94e",
;     "k:52a8476e4f7cbe49a1534aa110a856ad8ab162031e2cb59cf3a1d5b77f79f0c6",
;     "k:f870828d1f6e115d3240781414aa3adae8430f5a7fd064cc8aa38bb1a488e083",
;     "k:6f607f54b99605abfb15fe3b3a9c711491def222010c0d26d7b598887b80c720",
;     "k:77480c73f3b0368024e7bc7785a97ca41d35b5ac10a1b5e659676c1c15b57818",
;     "k:cd4b0bfd53854368b3eaf52cc4cda168372725284d5fa5c996af6d1d9e1578ec",
;     "k:3cdc42be0c9603d9692a3539790b002e5e758369efbe0b12a135e1460ff53cba",
;     "k:bf9e804d7c1687fbbe3e768fcd75fc2e467391b7d91502d727d8f8ca0655a84b",
;     "k:067e33d99ea117425622a9774a6b550a6473eacd1343729980543a668d90aec5",
;     "k:60781100557c4a5c86575469d993eb4c8afc3548302e4fa50088bc01ddceccf0",
;     "k:22a18cc06eedadccadb7b38d8a83e6c90a4ab8150d0b8d9f2eccfdfea483d7bd",
;     "k:e5e4c95c672028d12dabf468c5f542cb6253205edf7018613a651e57a912f303",
;     "k:6c10ee736cedd860d3fd3b14f0185f85cef260dbbd9e7e1876ddcfb11b882195",
;     "k:9e09e14125fa1c75d68517e85f5e42d440b7bcb36d4243185b5ab16c11c0642e",
;     "k:46c071c0ffc0d0e318cf7b5da36c43a37e28bbed6b373e16eeade1b811dfeca6",
;     "k:d80eb59f00dda6ab14b87f803d7bddd509497f7bbeefec65a43e9235775927aa",
;     "k:375117b0781e139c8e5145c3e000c8e7bfa19bedf7089850362b3af3aa592eb9",
;     "k:58a5cf804f9f0e80067e36e5c99d1109312869b13580ff3e711da399e95a0daa",
;     "k:e764bdca1f2d841b317991fef6546e76981cdb45850307d88ac71f34cb781dfe",
;     "k:c51be90b089e25cecddad2f44c702658169bdc9de6a2df16b5cb80e7679f9482",
;     "k:49aaff9a8454773662f8d1e9fcfbd037bf68a0d3e39c84c686a79f203c847b80",
;     "k:987c15a06824f2dc3fda3e27c3aeef0825081ed49aae9e29f6599c3b637d2646",
;     "k:8e7a967bb7ee89d3c2a4bba6159e91cd3eb2c6d70359294793301648d88c22b6",
;     "k:cd87f4fb135e026f6f702fbda09963b3686d3229eb2aa1481c3b647a92948e17",
;     "k:f19e634fd8156e681daa23eca39e959a8d107e5601b22e0668bc6d1f7fbc1b36",
;     "k:7e2df9087863ec9fbfc56737bb44c88e1870c542ba349fefc9b9c196c8b00d70",
;     "k:c276ae3dcba7bab0a17d72f2734e63d026c99c1db04cdaac876a7d9f77fd2bd1",
;     "k:3bd53397e32a9eb9be3d65bf8ddcf7ebf322bae0ce9200080c68d4f62c982e44",
;     "k:eab2621b47aadf5452116e726cea15e8c370e98bf12a24c7d53b44a7efcaef17",
;     "k:0b71c4a2dc3f5f577391cb7b0bbc97a70e67a5c3f828dd54d6df111500b2873e",
;     "k:7c98a4d80453e3a0fcfb6f7417dc24723010b5825718489fad8cec7cc6ff7b15",
;     "k:9bb718ceb8992c858c1b974232bef7e5674008a3b03fa30dd3a0dcae03b35472",
;     "k:4867ac3d9f9a2726fed84a2a366b01a895c9b236c368af18bb592cfca6b055a6",
;     "k:d35229717965097969bb7a0119cd56da2faa9e664d303ef02c8b1a798bb8cc6e",
;     "k:4233194b80b02f1e800477e14ffb58150f5df9e677326b7f6de8f583ba670970",
;     "k:5164b9205e6ee109682247f7f1e0ac61b902dd6df3b68a7600b3fa0bdfcf6660",
;     "k:f48def15ecdc8d2729a7be1eabbf67d9e08c5f36764ced6db0f19f9ba83adbc4",
;     "k:89aa218f65e2339c775dce1e45147332ecdfc3c98813052110230dec7b77bdbf",
;     "k:ea4dc5c595e3052b410d97a1624fa5f83e40c6cc746bfafa42bb876fc0078298",
;     "k:72f35346443cc703b68044ae5df59c8541c12d534bdb494e896d1e70017be7a4",
;     "k:f07f27d0ba1c86f4ce5099aaf28f45551f382b193c290534ff7b1d740971cc74",
;     "k:db27e0789cf4b99af64d8f97a7d55c1b6648bfb08337ff38a59feb26dac85816",
;     "k:fe004b4ea40bd9788bf325ef3ecf7de4ae1795b1a4c33b1fc847a692aff3fd71",
;     "k:dfb6bcf72c51bc75b54fbe3d2593e8a5c565632b368359aba25ebb578095ea8f",
;     "k:ab2157fee90665ee72a7d23609fd530a2fa5054bf95c9e144496621ee9cd58e7",
;     "k:9f6aa42136677dccdbcb0d3e3d4e605d35ea7a695a7635b7eadc3b5027d7bd8c",
;     "k:fec558689a8149fd77316f55e85ab881665e9eeac85e090a3c5f8f665b739107",
;     "k:1d665c3b54cbd6bfc351b0e9e859ec80faa60c0d8c13bc6679ff8dd161cc10c4",
;     "k:bcfcc244604712a461d9857463fc03c6a9f5ae1fd638b412dcaa17c3cb9b4b72",
;     "k:d20632f40ca3e194b1fba716165fc0eacea6bc25607d359e3f18e14b755f293d",
;     "k:314d9f7bb475baaec7d7ab434f5b1dfba81226fcbd3d12f57400f0d410db5f61",
;     "k:6966367caa3ff752279a646fa17130f9164f54b48d7efb135587e96a482cb63f",
;     "k:c8d0bb751e7fb86922fcd37a97cb5902db4b1f691504ad7b13d5e9d22a9d70fd",
;     "k:32f427b0fd143897aa9300b5e8c5ad2dce76fe42f3826d9f8c69439bcd363357",
;     "k:a0a60c73af6af190b5ae0987b8ab826792b081e7e53a756b7f85520b66d64a84",
;     "k:a7b5b8405f06c2bd78a4912683e21e22a8c7000ba19537b54257087ae6a43416",
;     "k:31f6dccfec161018e2a01d11b1d579b18660eb4669372425d07211c9d2230139",
;     "k:8005af80a82f9e3d63ec779afd8a4db5553ab4cd2ade95b1f54dc8cdce998410",
;     "k:cd970f88da2208addd967bedf4326d10d6dd347379adbd2d9b49d9132c604427",
;     "k:3071be44840838dcab7c5cf2cdd9108da6dda7be980196c5c49153b2d567635a",
;     "k:28161f59e201f80eafae3f525ae1c57d2fde388935f5a6608467b251cfcd91a5",
;     "k:c1af0c6427c642ed793e26aff7b19265faba627a69b41b6d34c52ddfeade74b1",
;     "k:0e7aca9eb45fa0f64d170a434be86c882c17a3a31ffd4cb4a62226105bbf109b",
;     "k:d3f84d5fc4802cc2a05d94d8c1617c4b77b8a216935f6c06ca4c60253c75cd0a",
;     "k:d866e943163d642007b665ed7a059ca54ec5c20abaf931adb90f34a0163edaff",
;     "k:f8c251c6c283ecd900e4385ccece124f6f87324a411db3be3c6cebd3d50b58a2",
;     "k:592bef9ea19e769dc649532a34aba274ec9a3532b20f59cd332a7bc7c1f3a845",
;     "k:636f3de4389767a3ac975d858e86d2d683b5089613caf884738456b978ccc172",
;     "k:9303878c1895b994c30160ccd4c63b6bdb33b3cc5fcb992bfbe3d408d6af2bf8"
; ])
  
; (free.brawler-bears.add-to-wl-bulk "wl" [
;     "k:432499ac1545256382397a9d8ed6eb21dac02a2711226338c786ed2ee7670309",
;     "k:cf40e514290d78e0aa720c297d7133fec115f47e2f12458a70e3173d8cdb52d9",
;     "k:43e9d0ddc2578034d5d2a78872be14ad63cd9467f8e756cdd4f3de9faa38aa87",
;     "k:b53956e8280a528ba0fc87e64dc2365628539523726be8825cfba1b21ea3feea",
;     "k:2efcc23e4a2f2584ebe4907e948c3f0f7942eb7fa163634aceaefc46459148f6",
;     "k:7c2671395a74a5e0de0789a4a3404ca32cd29f8381c1f3483704fbe825061fcb",
;     "k:8631dc0d8b7270bcf648d1e757fae36e9de8ebfa367bfc4bbf34c9889f6a7497",
;     "k:f43ca1c1682e545630bf3cfb0ddfb0ca67228418ae41a85d54c0bb0ab324cb22",
;     "k:b08074ff4267b3037c4ebbd12dbefa18ee1241c7418bf29d64440b2ad86f55b5",
;     "k:60cca43f4b6719756c98b807b1d44039e0a0ccf3a2b06afa68bb5de440615d43",
;     "k:bd45b38e33ffefb059aaa905e2b0df4004784b4a5e4a2129df482e69e6c68376",
;     "k:937455ca313f70332980211edab5021e259abc36eb55812d3d0ff5d43a5ddd98",
;     "k:face4d3364491183c58399191062d98c74bcf2ed1c82ebf2cecd918d0920098d",
;     "k:87bd5791f0d3dc6eeb019e75ed8f3193275b510d34f3483eebe5c426c2e23cc2",
;     "k:e003745050767be8259a7292d19b71163e7efaef8f1e5c8f00619e883dbd9960",
;     "k:f73913bf316847cb09b11b2e4a3f3038e9b602defea26bec3c3c5319f62b30ad",
;     "k:eadee7ef326be610051ed7b721d0b8239ab1dff7161a0bc7982cef8125f0eb4d",
;     "k:048fb834bf7d93a173120258a5dff263f0e43eaffd347751815b2c1daec00b06",
;     "k:170ee907cd377a9d5bf55473b42a45aed19a0155dca8b67afb80335accfb8658",
;     "k:3af35cc7ac1815890d1e0aabacb5b1ee5dfe6dcd205223d5bd2be550d0db4cc9",
;     "k:b18e7e0014536c14b46b777d42295695e34563061c23b0f81611c7ef7e353eeb",
;     "k:43a91183da24104c63f516c683b090293296aa424966f55dec762186f4feff99",
;     "k:1870dc8bf9302f8714ef23fcbcffd8836e52e1c388a7ad55761c8190c2862c16",
;     "k:133d6233a632a4d54639810a58ea43bceb78cfc83e10625b8a80e7464c34d748",
;     "k:aa7faa58dcb9f3ba187289b1965d748062de91cfa98aa5c06b1f59d493dfac89",
;     "k:dd9f846007299c3cf215573a4c8532b137293d4b567ca0bd63af53231a31c536",
;     "k:8d015880a0f80cd2c3611249668e114d91bcf753716d2b56bed6d8ae0e1ed731",
;     "k:446640aaee1cd9920c890e8dd327c5a96aada196ebf092ee0d2213917aa4cd0a",
;     "k:434f399ddd3a80a62471873db2bd0edaf30ae6775d3d6e10cf813b82f5d1c6bb",
;     "k:d2197ae518da9b083611ba0c55bfc39a22781de87e3e1307a9b05b49dd849610",
;     "k:f28d41ef706303a1202de2fc0077fd56f3c4eb3dedfc8087d231a50a3a94ad6c",
;     "k:05680067f963282db072dad5c97eb52f75dcf5038d34cd7aba2812540af83904",
;     "k:89d9604f29a7f471fe0134ab7eee0af708d2736c9dccf52d6748673a4afd0eb4",
;     "k:ecb12b014798052c5c6ae628e4635c66babc5db116aaca2a52789ad002112d1f",
;     "k:c2e47c9aa5bf3803537f50c1ead9d97a9dcf0c3dca771f38ad7efd0a7eec6881",
;     "k:f2ee21b4cb90fd627351bf169e49d870bac3bdd7db2e3a81334e89f2a2498269",
;     "k:74c6a2b3a35c5f08cf078b95ad34d5b613258ecd7a54ad1fa13b7eec139d83fa",
;     "k:26c23987caa9418daeeaffd7f8c5384c8c2ed4248e1820f4a637a56288afd479",
;     "k:3776850988be45ae9b730b3f5e500b32e3b4a9b6f50ae7eff1cf789aa87086ba",
;     "k:e8d2dbdb09aa90a71dcf029742f5e2bc3fdd2b743fefe82a564e82b0e474f05c",
;     "k:93b2db64de479b4b18b7f993dd3407b3f3e4d3e7b7ef7988a474e99c203dc5ae",
;     "k:8b1748996e6867030a4f7a771b79b70d9286f454add00009e78c1fb33c8dcfb7",
;     "k:ecf4d37f4159cb667e53342e5f329c0afabb8d5a8088e42496161c83e960f9f9",
;     "k:200f72953a51dfa8187d53e1c790ab53f0242396c8f20f1799d0389685c77abd",
;     "k:7eebb153ce0e9c781f5f3af33df84245ae62149a24b0fef41001c8fa821733a1",
;     "k:10920a3f98d76ad26efcca2e15110ffa52ccce5dbbf5e1ab50654ef0d9596dda",
;     "k:63706f379ede50f9aea61e119e16a91805743593d53ec38cb4aeb7486ee60ffc",
;     "k:47a9ac09618da19cb4414d80f228b5557fcd300f43084271b0b87ee03c1866b3",
;     "k:f23f873b791c39f0f696a951fca9bcd1e1dcb2e5c41db7170dbe43bd3593d7bc",
;     "k:2d44d6e009feabf31a6cd72d33b7ab3cfa98397277f59801088d9d925b436aa1",
;     "k:542d0f27069e3a6d4b7bc990f419d9750c421734de7dbb7e6fe3d9ed550b351b",
;     "k:2061ff265823f8c40407c56d578edcb77c354ba1b74cca3d0c16554066967cd4",
;     "k:cedb63c57aca5666f636939b8702c884958045e0552c5c440b7d612a769879f7",
;     "k:195e7c347fc971eaa0a48b1074c6306c7563dd1e22396a6b83403cae29efcd6a",
;     "k:6014f00d63b9377bf1a521d6bcde75425cd41db4005739bc6cb79cda78970f36",
;     "k:f78066fac635fd555466fab7193621c17d5f402ca1066d4684ad98b1be454b03",
;     "k:83894f2e2bf8d8d4457fceae0a94405bd0768cfa5afba81f02123a803121c8e3",
;     "k:21acaa3074b553212e97c71ff311065649af195d05a1177d1bde7cde9bcf8c50",
;     "k:d6b941ce41ca4d9210ee444d4b5a858ce0b4dee4b846a74f8627f400315895f0",
;     "k:2f9b97330771b8c327afb6d52dbae34e88a97c7d90c95032d1c00a6f0d94640f",
;     "k:0a6dd61d490ac8547a0bd43d8478c31ec5804cae5d91a17f58f417f80ee20c4d",
;     "k:1d0c1f8c62e33b4d92233c1d4d65a896c1a1c1fc51a0925938b58dd70935b185",
;     "k:7cf745e5509372775840b987e64b1e501bcf4ee5db731e1a0e34a529271efdac",
;     "k:e58bee5ee0f2904a8ccd3a7f9ffeba498714a380051e08d4fd090fe6683407dc",
;     "k:b9b5affeb2abc259800ff5c837a2d700eed387241bed58c014667d592c5aa743",
;     "k:9d1f207284f8381a87e71a8a1e7bb635588e193baa1106878b9d1ee6d83809e3",
;     "k:66a71bd7062ecb9e1d0a1f1eb57f34c5e62da8c4c190a51dc29da1da94c26e05",
;     "k:7b19c4b4dfd1bb35b5c940649430926dc1019afd5f548606cad75a8e1806dd39",
;     "k:e2fdf4dabb47e77ef7e45379f6caa3f4bd7659cce26377949c623faee85db74b",
;     "k:0a1d86d1736c237607d68a1b0387aa7b064abf1e97c1251e004eeea589d2f075",
;     "k:18c4db6e70be55be5c261a8cc045db300104aea872727821c3c3f464e74ac64e",
;     "k:0b6daf2deae8b66b3fe861076ad3f079f29c314641cc0119cbf2f59fdd012ccc",
;     "k:2b53d2f9536a7a485a16ce6eaf3ed389109921498c558b192ec4ba6faef15692",
;     "k:fd7b8c30bd038a33fc86774777b457aa767d4f923f45adda3568ea5142cabe3f",
;     "k:76c8a2165123f570c8fc85b175cf3b45a31d19b390e07afbcd3b95c324967bff",
;     "k:75689a28265e4ac1c9296831f32808e6cce24e72b054711e74f56bb5e58a04d3",
;     "k:04269eed03daf349327e71350f33decec9e52c1a04c40bf8155a2718f57802f1",
;     "k:d7eff8622b6acccc6b09bb3a966d00cda152418e27190e59db88d9880ac0a6fb",
;     "k:fed3cd3e88554db7f65cf6b63329d405a1a0462f679e68e301eb06b90c963542",
;     "k:9a13afdf62a46ef1a8ff461fc3924771bf89baa7cc76c3cd6c826d741f3f49d8",
;     "k:1acea4d3a369734957b2631cd3aadaebd3dd6a54027173f351583f32d65d29f0",
;     "k:41b0cf729593693a9eef096c420e93fb6a67cce0a88ada54d85e8304263e014c",
;     "k:df88f1b9288e53936844b75545d4f19d5b7fba9a3adf4d54eec609a8d622b1ff",
;     "k:5a54d10344f428629a7aba8900f39a1b511bbcbcb77a5d5e7eb248b5e6ef50a0",
;     "k:45933bdea1e4fabfe6571b89bfe8f9f7e73872678a91712fe5272dbef5008076",
;     "k:85f173d895157fd393beb2e47fc218be413d4bb19527e96a0956356bb6ca33f7",
;     "k:504e2bfc5808d6ac43d49d35a14c054d92279f220cdee456176d7544080f750e",
;     "k:faf8812ac8fc16ae45b4b524ea616434646967bcc4a05bb7c3ddb9904430980a",
;     "k:3257bde06e9ccff5ca5f2bf34baf40662fc52f554d081a77c9c9acf60b477187",
;     "k:ca38ad9b6519fbd6907d6f522b07b8c5bc9bbd11c984d53972ba0d6a6c9ac565",
;     "k:6cfc58cfd570928bde036b8515ada94a49f441a2f4326fde4945f746ccabcc2d",
;     "k:7e21b052b25c7343fa6906047c238c2de1cc145beac8f5923483a620aec6f0e6",
;     "k:bc53e350b07a304b1fb4346a2cdc90917ee6e6e4af721b29eb575edf27415201",
;     "k:b776ae01fbe31b6a5aa73e3b293b5559cf1adce55119c284f1ec398413b96dfe",
;     "k:81636e11671e842e11452cecc32a36068e27fd64c6cade57487f7da83462b021",
;     "k:c6c31de2b8ccd6ba736e8996c9551353b17ad2613e03024f934e48cf447d9384",
;     "k:74ffcb4c4a8230e4bf5d79c748fbfa923d312eb8c2243348decad5941673de77",
;     "k:edd10e1ea732432606536fb3b885d38c006a8515c9644c35f796fe53ded03115",
;     "k:7aec9dd3a41476832130ba08cbfbfc22ec6cd6e3212e7b1a0d63556b4d44cb74",
;     "k:7113bfacd62bbfb6e065856a9aadc97aaa07c87435e431d1d1072309770afc13",
;     "k:0f9a6d831bd07622e8dc7d8f8ecccf15fa721b1a3bb61b54569380dc6ef1b559",
;     "k:e77f7095c2b39a4b280b45a636890f662c41d019445f1f9f2dba91c50d3f56aa",
;     "k:38a5204ca52f5c22d12dd4f70b0dc24c090e56ebfe706d1816190782a492ebd0",
;     "k:607f963c78b06a21c15aeb30b7c225f40096dc7d55bf2fd609481d1a7166b7b2",
;     "k:85bf96d114bdad39d8514ca18e7c52fa64c2290e5f9debce6772c813ed04bac5",
;     "k:6c6346194e6f9a6904ffc7eceeacd64b3a4f70a31ebd10862bb2f9a389abd170",
;     "k:dfe3035bb03d9a9d17299f76d6cec2cb2251b5cf5575bf0e7bb66109041a362a",
;     "k:a03041ddf050c295d9547ed5e750de3d4c5a538c019e4e5b0c8ee1b227c44909",
;     "k:e44ba0e45f7707d3c6f00ce6d169b4e7fb4d4955fe156b7e691a05ebcb0a7ea1",
;     "k:5f5e7bde4c550415721d7c5b0094fe3de2d6441ffae24452da90c9b3d2a0581f",
;     "k:2ff62e254b2fd298a4fbe593eaf64ba889ec6ba535428ce7f402da1e26315858",
;     "k:8d4dc88b89a5437211631c5ec3c1ea0c3941b18c060554986c60a4c7d452b99b",
;     "k:9950c46a812c16cbd71d63b4d07ae0f9d0b8b3ced74a48af6c373d6bd3114056",
;     "k:faf5fdf15a1730408e21c7264de1a4d3810a498ec15ec66a92b265825e410db3",
;     "k:8dcd10bce374c6e1259927c7792ed116cb7ddae5b04ceacce8705b2574857620",
;     "k:2633de45ccc9d11d9989f5c36f56150af9556b455ba8cc1a00f68a3eb0ee99cc",
;     "k:fc691e0d21b58312eb610d2dd93c734f1b206356393ec02784a21a000540227a",
;     "k:fa31e6e6d5fc9ff4e0b6a3cf76ea43733204c23dfc3c5d450e22fcac98cc1d91",
;     "k:16dbe89668995bc0a5d41de566794e6554111214730d4212409b46d56036f222",
;     "k:f867b78eb950ba4215eaba27ce22a11a3e1cfdd82e1323b24d1482532e4b72ae",
;     "k:ad7ba2c5080ee439000c82c21ab3be9e187e7a71074eb4d8582f2ea7aca621ff",
;     "k:b8d24bc1894c88beaea114a914f4bce218dd4bbcc6fedaf155675c7677a84084",
;     "k:68457bb88f335c3b5680d9c3f8bce0887be929d44924b70f3651906e75f8ee6d",
;     "k:1cd8e43aefd3d3a00173e246fe6c726f791b20a7ae6fd80cbdb32b91a61370e0",
;     "k:d5589b1ccd8e42cf7dbef4b2c33966554be54c56912a63476870524d0ef99d7a",
;     "k:204ef37cb9766c7779245d45c7a90cfdf5239e7d643dde0c39af3ec6cc629cf0",
;     "k:736cc659155cc0c7391c550d739adfd1eedadd73969a3fe0b6f85fce04095728",
;     "k:f0d28e6b2533a7b76b8cdf8709c2a99c4af6b1553e37f00bc24c7fd7550736de",
;     "k:8e1715449b0eb6acc2852da6bd431bb274f7b2cf714567474a4cad84a2ec17aa",
;     "k:0d19b682e5d46e019882f39b3f05764463dd78f7707237629f3161ff258117ea",
;     "k:3e81f4e7971a2ebb87c217d734ba5e25cf2cf0acf74c4cfd402c9173be520f19",
;     "k:41f39d5744a6d8de52f5c1d2dc331b4effed96446103339baf4bf70dbf7b7990",
;     "k:a7e4e6f08ce3534aebd9be8a74eeba696275171969a7159662dee20fb488a4d0",
;     "k:7b100f632fba1e50c56d8a613a49d9d335a5837d08cb91ecea03915dfb155a41",
;     "k:0d2273de7971cd331f91e79d301b663db179bd4d2089d22e518021d4a85099c9",
;     "k:cc451d98b81c72ccc8a1f5cb611b52a48364e3f97b3a3b3acc17d1181424b996",
;     "k:ec1233e61efc955c27a86ea74269224d50d57f5b91d575854169ba6c377d48d6",
;     "k:3a3e3fd94abf6347be0bea15a270d796f97907a881c479ae5bd68ae1afe2ad79",
;     "k:652f889c613b58e8e5df9c93b9c1335a82aabe008031715a8086bdb10c492af3",
;     "k:cb09e52de81d5b4a7bac89939f5c60cf4d1340e421990b3df2672fbf26c33912",
;     "k:46ec5b314b54819d4074f2dd327ba2e994a78ff0e1dd21bc61df28d704c596fe",
;     "k:8ad962750a1efa04a6981aaa8568da34555a83674dbabdcbb3f1bed3df37df88",
;     "k:bea0aa7a012770b88aab0288b7a0ef1056d0ec5eb80c095f0796273e20435855",
;     "k:8880968716827e85fe7f6d0002d0010462e9a048e100a0961d6355a740e126ac",
;     "k:dd96ea399d1c20e97958f5d05c4844d251cd3c123f4e863fc2bc00750ee436e1",
;     "k:96be58fcb950c90d74d32998b0b74bc22ce8f824e44b411b0442bd05cea1fe08",
;     "k:d72579b878a99f59469dc9be3d0384f58a52d995588c39fc59fdd648eba14f47",
;     "k:d9c5f19727dfd0b6b968e961a825a6f607b0769a461de1d2fae1162e928cb483",
;     "k:90752476bd8100db2823a5a42c99ce1579c548ad011659635494be38de34545c",
;     "k:571333fe9d4a8c7ca2cb743f3f76be3fa2d58d9dce058e7c0f48e71d1ad54bd6",
;     "k:6ff42f0929ab4607e3ad99fe19c2a69684bdd2d2c21cac766a53111d48b26f9e",
;     "k:72ac8102d98bacc00ac1f9498ac1618a2651f83556e197212219f3e5f81afda7",
;     "k:cf4868ae08e127e00666bb6e9b43a5888d3bfac337043faf628fe6d4dc45b488",
;     "k:bc75a4ba5c79bef0986e802a5a7e0b0e757ed6cf0f165d516694e60482f4d830",
;     "k:cabb959756969fcc67d9d85d39ee7ffe3810a180d451d345b2a1eaa7c9988878",
;     "k:50e80aa7def66445ef6d0db0552aef3202feab7e74a11599ed055ea0a1fa71ec",
;     "k:91f0baa0c99cdee6cb152cd09d8198e7c03c464277e8aa907e9a339cb093aef3",
;     "k:9f67c1d1cac5b2ad19568c0d9bb64b6e0f9eb9e05523f698888f37ae269e7d78",
;     "k:c4af5f7976bbb6633577b521e8639374a15063fbeba76b6c097e8ddd2fdeae32",
;     "k:d5c4927d60e0641b4eafbf819aaaa8f5137492575cae0ec3573b69b0bdb2837d",
;     "k:8fb4b265d28eb783c6bc9c7f0b5fb361b926fd7f156e3f395ddad785424a95e1",
;     "k:2861dd423326ad4d454fbf9d56094757082177ee4c425235c9d8e2e202d80437",
;     "k:467ff4d70d3fd8bd58b1ec766b516814489250c31c490cd6d44e25942638b769",
;     "k:d9085b1a14171db2aba2ba5b8de32682e4e173d7e4824b7056fac24bdef48644",
;     "k:2b103c9e1687bd0fa22b848825115e433c580b32050a0fb050095983ba16049d",
;     "k:5849271bc77aeefd5e731496c0f28efb86c93baa6a0e722c6621e25616a37543",
;     "k:7d07fe905cae72595d97e303b64e66405e30161253fbd7f27edb8eb4a8f3d902",
;     "k:32dfbc8b73d4e203c4a21e04dfaf1c947121d45ab1fb9ea9b7c7bc5fba748150",
;     "k:6b5788fa4df6be7896e1328ce469ebe954fe50719b04ec8d88e9d6676c188ab6",
;     "k:dc25105f756541c449abae180a18174267f7c1fe7714088a5204ec0ccaade531",
;     "k:f380af92046e31271af1a2b9e9adf47d782b4b9093c40e381d341313f41c4603",
;     "k:3e84c7a7a21e69e666a82f8a38f55fe79049fa6b675860681f11f514d92ae6f5",
;     "k:a827bfda89ac7dd08e12505ce637a9339839e1c0aef56409aa5798841c29d0e6",
;     "k:c5ddf59998e3717ded64905b9eecb222f5776aca9f8fd44dfd585d59ce07fcd2",
;     "k:2d12f598269e38035df7a68f82cc0344c48050d11fc10c0aa0eb04a17a6f5ff8",
;     "k:4ca8a9cf09fe22a349873c81516a6f72e7f565e450e7e674a1b1d23515b788c3",
;     "k:866a63340aff35a595f55d90df9c03083ae2571b6492b6957d5c7c3b55119d4d",
;     "k:208feb0eb68731ad952b76d299052537a7be38c25a14916afd153805ff62bdf1",
;     "k:7c224066d6b35cacd94a635791703be5ff85dd1c29331843a26c268d3f5f410d",
;     "k:ba3d4b74b64f17e0e2bcecd3494aab1ec4c3f72be6215f13b304aff3f06182cb",
;     "k:eb1467afd1ac2a8b24e02ba25c162a186262bb302e373e3abe94ef0a06f857a4",
;     "k:71ed97afcec5698f827a47a4a168d73775e934f2ce4dc5d12aee99ea9bc668d1",
;     "k:db71aca7f7c4364346217cac6c35cc92a44ab8fbd8848be1b068768636961997",
;     "k:154fce0a7d0007a3b0939ba33bc9cd4508444165251e64ae8858a328a08fbd4e",
;     "k:355b7903790e02672e00b9965acdbddc3618fd9a92e61cfe8d0c4fe51979c2d8",
;     "k:a7f8f07ef440868840f24533e6a6e4bc6633a0640209baa75df86d30e246d77f",
;     "k:9af202b8d6a11cfa602d6d095fd0cbfce5ed8c73a06912a2f8c659d220c8ef41",
;     "k:522f254a6208ca9db280e36397ecf995774cbc343f1e8d32f400a253f5ba4e6f",
;     "k:dfec24531f49146aa4b09116aae17fd138e29fce7c888b101b2a485417259294",
;     "k:177a16b1c5bbd05ad6b054803c7c9811cf4ec82c3d308bc99162d68503bf9bd4",
;     "k:1fa348baca08853d47e25c592c6d713a309c8489145ab3f9bce439c0ea8fc02f",
;     "k:7b4afbb8d39a5f8efcded0ffcd674d13939cec8ebde8e3ee16fb32e1c3d49b1e",
;     "k:d4bd1e3e54a3d52c09772802de277b61c422ccc832d465995cf296d4dc27cc34",
;     "k:e563cbed081979c6f57b7d8f375303aba48ecd79a10da4511a5a06ac605f6ba5",
;     "k:c67dfa1f8c6423c18e39b71928b0eed8253145b658ede003cff0aad96f1c4adf",
;     "k:4a6c7bcf28e5a04b5503f270646e37a40af38140351ca6d378ab42cdde146c08",
;     "k:5793400e63d9de53562987d2e77c93f7d823a26ae61d3321560fff782f616f1d",
;     "k:ef7e55b7153e1fda59fd19a4de8d6fac553f32e0aba68ea485364bdaa8a1ad46",
;     "k:e9cd5044825657221cc351c8a6e617bd37f257568afe7e6a51a34de47d9ced8e",
;     "k:eafce37fef91784cf60e668084a6050673ec34b23cba8761db27cd653e9f9c01",
;     "k:7b9d9e15b10ba6c456e0b793c2763076d81ee04b5a1e36c24df3b85a5202bcf4",
;     "k:81d1a4dc2ccff17fcf6154e51435510fb7287e22a83dd487f039be2f2216ad79",
;     "k:008e60b9117675b233475d41f2ed3c548291f542e6bfbf90912c07aaa2393de6",
;     "k:3e5011e520daa04dc738f85c4aa3edfb34d0b83996601c5611246283ba83af09",
;     "k:99670323c72f4ab785c79fa2ad35b4b2242a1435561ece95d37cd4d02d391e2c",
;     "k:3d0620bde4605e756517604c557c59b92e017583373a20455c7817b3c0b2a915",
;     "k:1dbef7ef8142ff926b5ebb822f01235b02f9ad182c273313844e255b836b5d30",
;     "k:bcb19a26b0d15057d6e61d2afc9bd6200e21ab3ae0428314e5abe4e2cf3fe2e6",
;     "k:90669b875b4ee8243174f7262213e46e401ec20c941a5e4d007ff104d898db0c",
;     "k:7f8c92bcf9a7e05587688a7e63a1089666dbe86ab9977ff7e5a997bdaeb766f9",
;     "k:76cb7cea2abcec686b1b37a67faf8758b384bf1f51ff59c279e02a3a37bf6621",
;     "k:93db0f291c17689285afb1ea4f3cd9843259ad8f6e2b4553f9a0c826ace56d85"
; ])
  
  
;  (create-table nfts)
;  (create-table marketplace)
;  (create-table offers)
;  (create-table counts)
;  (create-table values)
;  (create-table wl)
;  (create-table price)
;  (initialize)

          
