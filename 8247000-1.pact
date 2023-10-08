(module kda-skellies GOV
    "Kadena Skellies Mint"
  
      (defconst NFTS_TO_MINT_COUNT 1250.0)  
      (defconst WL_MINT_PRICE 0.0)
      (defconst PUBLIC_MINT_PRICE 10.0)
      (defconst WL_MINT_START_TIME "2022-10-30T20:00:00Z")
      (defconst PUBLIC_MINT_START_TIME "2022-10-31T20:00:00Z")
      (defconst MINT_END_TIME "2022-11-01T20:00:00Z")
      (defconst MAX_WL_MINT 2)
      (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
      (defconst TOTAL_REWARDS_CLAIMED_KEY "total-rewards-claimed-count-key")
      (defconst NFTS_MINTED_COUNT_KEY "nfts-minted-count-key")
      (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
      (defconst CURR_WL_ROLE_KEY "curr-wl-role-key")
      (defconst WL_PREMIUM_ROLE "whitelist")
      (defconst URI_KEY "uri-key")
      (defconst PRICE_KEY "price-key")
      (defconst OFFERS_COUNT_KEY "offers-count-key")
      (defconst OFFERS_BANK:string "skellies-offers-bank")
      (defconst STAKING_BANK:string "skellies-staking-bank")
      (defconst ADMIN_KEYSET "free.skellies-admin-keys")
      (defconst ADMIN_ADDRESS "k:471957f94679ce95351ff042789ddda969b2c51788ee0a9b468bfae5c2506664")
      (defconst CREATOR_ADDRESS "k:3de54c45b72dd96587587aee4bff9e4bd4580c9d628102b74daa9ddbea05dce7")
      (defconst ROYALTY_FEE 0.02)
      (defconst MINTER_ROYALTY_FEE 0.04)
      (defconst FUNDED_STAKING_BANK_BALANCE 1500.00)
      (defconst IMAGES_URI "https://arkade-prod.s3.us-east-1.amazonaws.com/kadena-skellies")
	  (defconst IS_STAKING_LIVE false)
  
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
    (defcap STAKE_NFT (id:string owner:string)
        @event true
    )

    (defcap UNSTAKE_NFT (id:string owner:string)
        @event true
    )
  
     (defcap REWARD_FROM_STAKE (account:string amount:decimal)
        @event true
    )											  	 
  
      (defun initialize ()
          @doc "Initializes the contract the first time its loaded "
          (insert counts NFTS_MINTED_COUNT_KEY {"count": 0.0})
          (insert counts TOTAL_VOLUME_KEY {"count": 0.0})
          (insert values MINT_CHAIN_ID_KEY {"value": (at "chain-id" (chain-data))})
          (insert price PRICE_KEY {"price": WL_MINT_PRICE})
          (insert values CURR_WL_ROLE_KEY {"value": "whitelist"})
          (insert values URI_KEY {"value": "https://arkade-prod.s3.us-east-1.amazonaws.com/kadena-skellies/"})
      )
  
      ;;;;; SCHEMAS AND TABLES ;;;;;
  
      (defschema nft-main-schema
          @doc "Stores core information about each NFT"
          id:string
          date-minted:time
          owner:string
          minted-by:string
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

        (defschema staked-schema
            nft-id:string
            account:string
            timestamp:time
            staked:bool
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
      (deftable staked-table:{staked-schema})					   
      (deftable counts:{counts-schema})
      (deftable values:{values-schema})
      (deftable wl:{wl-schema})
      (deftable price:{price-schema})
  
      ;;;;;; MINT FUNCTIONS ;;;;;;
  
      (defun mint-nfts-bulk (owner:string amount:integer)
         @doc "Mints NFTs bulk"
          (enforce-mint-live)
          (enforce-mint-wl-role owner)
          (enforce-max-wl-mint owner amount)
          (coin.transfer owner CREATOR_ADDRESS (* 0.90 (* (get-mint-price) amount)))
          (coin.transfer owner ADMIN_ADDRESS (* 0.10 (* (get-mint-price) amount)))
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
                  "owner": owner,
                  "minted-by": owner
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
          ;(enforce-marketplace-live)
        (let (
                (data (get-nft-staked id))
            )
            (enforce (= (at "staked" data) false) "Cannot tranfser NFT that is staked")
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
			    (write staked-table id {
                  "nft-id": id,
                  "staked": false,
                  "account": receiver,
                  "timestamp": (at "block-time" (chain-data))
                })
          ))
       )
      ;)
  
      (defun put-id-for-sale(id:string price:decimal)
          @doc "Puts an NFT up for sale"
          ;(enforce-marketplace-live)
        (let (
                (data (get-nft-staked id))
            )
            (enforce (= (at "staked" data) false) "Cannot list NFT that is staked")			  
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
                (write staked-table id {
                  "nft-id": id,
                  "staked": false,
                  "account": owner,
                  "timestamp": (at "block-time" (chain-data))
                })										
                  (emit-event (PUT_ON_SALE id owner price))
              )
          ))
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
                      (minted-by (at "minted-by" nft-data))
                      (price (at "price" (read marketplace id ["price"])))
                      (minter-fee (get-minter-fee-from-price price))
                      (fee (get-market-fee-from-price price))
                      (to-seller-amount (get-to-seller-amount-from-price price))
  
                  )
                ;   (coin.transfer buyer original-owner to-seller-amount)
                ;   (coin.transfer buyer CREATOR_ADDRESS fee)
                ;   (coin.transfer buyer minted-by minter-fee)
                  (if 
                      (= original-owner minted-by)
                      (coin.transfer buyer minted-by (+ minter-fee to-seller-amount)) ; combined
                      true
                  )
                  (if 
                      (!= original-owner minted-by)
                      (coin.transfer buyer minted-by minter-fee) ; minted-by != seller
                      true
                  )
                  (if 
                      (!= original-owner minted-by)
                      (coin.transfer buyer original-owner to-seller-amount) ; seller != minted-by
                      true
                  )
                  (coin.transfer buyer CREATOR_ADDRESS fee)
                  (write marketplace id {
                      "id": id,
                      "for-sale": false, 
                      "price": -1.0,  ; Invalid price so NFT can't be sold
                      "updated-at": (at "block-time" (chain-data)),
                      "owner": buyer
                  })
                  (update nfts id (+ {"owner": buyer} nft-data ))
                (write staked-table id {
                  "nft-id": id,
                  "staked": false,
                  "account": buyer,
                  "timestamp": (at "block-time" (chain-data))
                })										
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
            (staked-data (get-nft-staked id))
            (original-owner (at "owner" (read nfts id ["owner"])))
            (new-offer-id (int-to-str 10 (floor (get-count OFFERS_COUNT_KEY))))
        )
            (enforce-id-on-sale id)
            (enforce (= (at "staked" staked-data) false) "Cannot make offer on NFT that is staked")
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
                    (write staked-table nft-id {
                      "nft-id": id,
                      "staked": false,
                      "account": buyer,
                      "timestamp": (at "block-time" (chain-data))
                    })
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

      (defun get-minter-fee-from-price (price:decimal)
          @doc "Market fee cost for id sold at a given price"
          (* price MINTER_ROYALTY_FEE)
      )
  
      (defun get-market-fee-from-price (price:decimal)
          @doc "Market fee cost for ID sold at a given price"
          (* price ROYALTY_FEE)
      )
  
      (defun get-to-seller-amount-from-price (price:decimal)
          @doc "Amount that goes to a seller when NFT sold at a given price"
          (* price (- 1 (+ ROYALTY_FEE MINTER_ROYALTY_FEE)))
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

      ;;;;; STAKE FUCNTIONS ;;;;;

    (defun enforce-staking-live ()
        @doc "Enforces staking is live"
        (enforce (= IS_STAKING_LIVE true) "Staking is not live.")
          
    )
	
    (defun stake-bulk (items:list)
		enforce(enforce-staking-live)
        (map
            (stake-item)
            items
        )
    )

    (defun stake-item (item:object)
		enforce(enforce-staking-live)
        (let (
                (nft-id (at "id" item))
                (owner (at "owner" item))
            )
            (stake nft-id owner)
        )
    )


    (defun stake (nft-id:string owner:string)
		enforce(enforce-staking-live)
        ; (let (
        ;         (data (read marketplace nft-id))
        ;     )
        ;     (enforce (= (at "for-sale" data) false) "Cannot stake NFT that is listed")
        (with-capability (OWNER nft-id)
            (with-default-read staked-table nft-id
                {"staked": false}
                {"staked":= staked}
                (enforce (= staked false) "This NFT is already staked")
            )
                (write marketplace nft-id {
                    "id": nft-id,
                    "for-sale": false, 
                    "price": -1.0,  ; Invalid price so NFT can't be sold
                    "updated-at": (at "block-time" (chain-data)),
                    "owner": owner
                })
            (write staked-table nft-id
                {"nft-id": nft-id,
                "account": owner,
                "timestamp": (at "block-time" (chain-data)),
                "staked": true}
            )
            (emit-event (STAKE_NFT nft-id owner))
        )
        ; )
    )

    (defun claim-unstake-bulk (items:list)
		enforce(enforce-staking-live)
        (map
            (claim-unstake-item)
            items
        )
    )

    (defun claim-unstake-item (item:object)
		enforce(enforce-staking-live)
        (let (
                (nft-id (at "id" item))
                (owner (at "owner" item))
            )
            (unstake nft-id owner)
        )
    )

    (defun unstake (nft-id:string owner:string)
		enforce(enforce-staking-live)
        (let ((data (get-nft-staked nft-id)))
            (enforce (= (at "staked" data) true) "NFT already unstaked")
            (with-capability (OWNER nft-id)
                (with-read staked-table nft-id
                    {"timestamp":= stakedTime,
                    "account":= owner
                    "nft-id":=nft-id
                    "staked":=staked}
                    (update staked-table nft-id
                        {"staked": false}
                    )
                    (with-capability (PRIVATE)
                        (reward-from-stake owner stakedTime)
                    )
                )
                (emit-event (UNSTAKE_NFT nft-id owner))
            )
        )
    )

    (defun claim-without-unstake-bulk (items:list)
		enforce(enforce-staking-live)
        ; (let ((items (ids-staked-by owner)))
            (map
                (claim-without-unstake-item)
                items
            )
        ; )
    )

    (defun claim-without-unstake-item (item:object)
		enforce(enforce-staking-live)
        (let (
                (nft-id (at "id" item))
                (owner (at "owner" item))
            )
            (claim-without-unstake nft-id owner)
        )
    )

    (defun claim-without-unstake (nft-id:string owner:string)
		enforce(enforce-staking-live)
        (with-capability (OWNER nft-id)
            (with-read staked-table nft-id
                {"timestamp":= stakedTime,
                "account":= owner
                "nft-id":=nft-id
                "staked":=staked}
                (with-capability (PRIVATE)
                    (reward-from-stake owner stakedTime)
                )
                (update staked-table nft-id
                    {"timestamp": (at "block-time" (chain-data))}
                )
            )
        )
    )

    (defun reward-from-stake (account:string stakedTime:time)
		enforce(enforce-staking-live)
        ;(require-capability (PRIVATE))

          (let
              (
                (reward (calculate-reward stakedTime))
            )
            (install-capability (coin.TRANSFER STAKING_BANK account reward))
			(coin.transfer STAKING_BANK account reward)

            (emit-event (REWARD_FROM_STAKE account reward))
            (increase-count TOTAL_REWARDS_CLAIMED_KEY reward)
          )
        
    )

    (defun calculate-reward (stakedTime:time)
        (ceiling (* (/ (diff-time (at "block-time" (chain-data)) stakedTime) 86400) 0.02) 2)
    )

    ; (defun get-max-reward-per-day ()
    ;     (let 
    ;         ((minted-count (get-count NFTS_MINTED_COUNT_KEY))
    ;     )
    ;     (cond ((< minted-count 500) 200)
    ;         ((<= minted-count 1000) 100)
    ;         50)
    ;     )
    ; )

    (defun ids-staked-by (owner:string)
        @doc "All IDs staked by an owner"
        (select staked-table ["nft-id", "account", "timestamp", "staked"] (where "account" (= owner)))
    )

    (defun get-nft-staked (nft-id:string)
        (read staked-table nft-id)
    )

    (defun get-total-staked ()
        (length (keys staked-table))
    )


    (defun get-owner-unclaimed-amount (owner:string)
        (let ((items (ids-staked-by owner)))
			(let ((rewards
                (map (lambda (item)
                    (if (= (at "staked" item) true)
						(let ((days (/ (diff-time (at "block-time" (chain-data)) (at "timestamp" item)) 86400)))
                            (* 25 days))
							0))
                        items)))
          (fold (lambda (reward sum)
                  (+ reward sum))
                0
                rewards))))

    (defun get-items-unclaimed-amount (items:list)
            (let ((rewards
                   (map (lambda (item)
                          (if (= (at "staked" item) true)
                              (let ((days (/ (diff-time (at "block-time" (chain-data)) (time (at "timestamp" item))) 86400)))
                                (* 25 days))
                              0))
                        items)))
          (fold (lambda (reward sum)
                  (+ reward sum))
                0
                rewards)))								 	 
  
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
          @doc "Enforces wl member only mints max amount"
          (if (< (at "block-time" (chain-data)) (time PUBLIC_MINT_START_TIME))
              (let 
                  (
                      (owned-count (length (ids-owned-by account)))
                  )
                  (enforce (<= amount MAX_WL_MINT) "You have minted the max")
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
          (if (< (at "block-time" (chain-data)) (time PUBLIC_MINT_START_TIME)) WL_MINT_PRICE PUBLIC_MINT_PRICE)
)
    (defun create-BANK-guard ()
        (create-user-guard (require-PRIVATE))
    )

    (defun require-PRIVATE ()
        (require-capability (PRIVATE))
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
	  
    (defun get-image-uri (id:string)
        @doc
        "Give URI for ID. If not supported, return \"\" (empty string)."
        (+ (get-value URI_KEY) id)
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
  
   
  ;(create-table marketplace)
  ;(create-table nfts)			 
  ;(create-table counts)
  ;(create-table values)
  ;(create-table wl)
  ;(create-table price)
  ;(initialize)
  
