(module brawler-bears GOV
    "Brawler Bears NFTs"
    
    (defconst NFTS_TO_MINT_COUNT 2323.0)  
    (defconst WL_MINT_PRICE 0.0)
    (defconst PUBLIC_MINT_PRICE 11.0)
    (defconst WL_MINT_START_TIME "2023-03-02T19:00:00Z")
    (defconst PUBLIC_MINT_START_TIME "2023-03-02T19:00:00Z")
    (defconst MINT_END_TIME "2023-03-08T19:00:00Z")
    (defconst TRADING_START_TIME "2023-03-08T19:00:00Z")
    (defconst MAX_WL_MINT 1)
    (defconst TOTAL_BATTLES_KEY "total-battles-count-key")
    (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
    (defconst NFTS_MINTED_COUNT_KEY "nfts-minted-count-key")
    (defconst TOTAL_REWARDS_CLAIMED_KEY "total-rewards-claimed-count-key")
    (defconst TOTAL_BATTLE_REWARDS_KEY "total-battle-rewards-count-key")
    (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
    (defconst CURR_WL_ROLE_KEY "curr-wl-role-key")
    (defconst WL_PREMIUM_ROLE "wl")
    (defconst URI_KEY "uri-key")
    (defconst PRICE_KEY "price-key")
    (defconst OFFERS_COUNT_KEY "offers-count-key")
    (defconst OFFERS_BANK:string "brawler-bears-offers-bank")
    (defconst STAKING_BANK:string "brawler-bears-staking-bank")
    (defconst BATTLE_BANK:string "brawler-bears-battle-bank")
    (defconst ADMIN_KEYSET "free.arkade-admin")
    (defconst ADMIN_ADDRESS "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec")
    (defconst CREATOR_ADDRESS "k:334e2e7959a2c5d204d106f2969b600f92f005887b33289334b406a78146bf1a")
    (defconst ROYALTY_FEE 0.11)
    (defconst FUNDED_STAKING_BANK_BALANCE 20000000.00)
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
          (enforce-guard (at "guard" (coin.details account)))
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
          @doc "Initializes the contract the first time its loaded"
          (insert counts NFTS_MINTED_COUNT_KEY {"count": 0.0})
          (insert counts TOTAL_VOLUME_KEY {"count": 0.0})
          (insert counts TOTAL_REWARDS_CLAIMED_KEY {"count": 0.0})
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
          level:integer
          max-health-power:integer
          current-health-power:integer
          attacks:list
          total-games-played:integer
          total-wins:integer
          total-losses:integer
          total-damage-given:integer
          total-damage-taken:integer
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

        (defschema staked-schema
            nft-id:string
            account:string
            timestamp:time
            staked:bool
        )

        (defschema battles-schema
          id:string
          turn:string
          playerOneAddress:string
          playerOneNftId:string
          playerOneLastAttack:string
          playerTwoAddress:string
          playerTwoNftId:string
          playerTwoLastAttack:string
          winnerAddress:string
          amount:decimal
          created-at:time
          updated-at:time
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
      (deftable battles-table:{battles-schema})
      (deftable counts:{counts-schema})
      (deftable values:{values-schema})
      (deftable whitelist:{wl-schema})
      (deftable price:{price-schema})
  
      ;;;;;; MINT FUNCTIONS ;;;;;;
      
     (defun mint-nfts-bulk (owner:string amount:integer)
         @doc "Mints NFTs bulk"
          (enforce-mint-live)
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
                  "owner": owner
              })
            (insert marketplace id {
                "id": id,
                "for-sale": false, 
                "price": -1.0, 
                "updated-at": (at "block-time" (chain-data)),
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
          (insert whitelist account {"role": role})
      )
  
      (defun update-user-wl-role (role:string account:string)
          @doc "Updates a user's wl role"
          (with-capability (ADMIN)
              (update whitelist account {"role": role})
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
          (enforce-marketplace-live)
          (enforce-account-exists receiver)
        ; (let (
        ;         (data (get-nft-staked id))
        ;     )
            ; (enforce (= (at "staked" data) false) "Cannot tranfser NFT that is staked")
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
          )
        ; )
      )
  
      (defun put-id-for-sale(id:string price:decimal)
          @doc "Puts an NFT up for sale"
          (enforce-marketplace-live)
        ; (let (
        ;         (data (get-nft-staked id))
        ;     )
        ;     (enforce (= (at "staked" data) false) "Cannot list NFT that is staked")
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
          )
            ; )
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
                    ; (staked-data (get-nft-staked id))
                    (nft-data (get-nft-fields-for-id [] id ))
                    (original-owner (at "owner" nft-data))
                    (price (at "price" (read marketplace id ["price"])))
                    (fee (get-market-fee-from-price price))
                    (to-seller-amount (get-to-seller-amount-from-price price))

                )
                ; (enforce (= (at "staked" staked-data) false) "Cannot buy NFT that is staked")
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
            (enforce (= (at "staked" staked-data) false) "Cannot make offer on NFT that is staked")
            (enforce-id-on-sale id)
            (enforce (> amount 0.0) "Amount must be greater then zero")
            (enforce (> duration 0) "Duration must be at least 1 day")
            (enforce (= original-owner buyer) "The buyer can't be the owner")
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

      ;;;;; STAKE FUCNTIONS ;;;;;


    (defun stake-bulk (items:list)
        (map
            (stake-item)
            items
        )
    )

    (defun stake-item (item:object)
        (let (
                (nft-id (at "id" item))
                (owner (at "owner" item))
            )
            (stake nft-id owner)
        )
    )


    (defun stake (nft-id:string owner:string)
        (let (
                (data (read marketplace nft-id))
            )
            (enforce (= (at "for-sale" data) false) "Cannot stake NFT that is listed")
        (with-capability (OWNER nft-id)
            (with-default-read staked-table nft-id
                {"staked": false}
                {"staked":= staked}
                (enforce (= staked false) "This NFT is already staked")
            )
            (write staked-table nft-id
                {"nft-id": nft-id,
                "account": owner,
                "timestamp": (at "block-time" (chain-data)),
                "staked": true}
            )
            (emit-event (STAKE_NFT nft-id owner))
        ))
    )

    (defun claim-unstake-bulk (items:list)
        (map
            (claim-unstake-item)
            items
        )
    )

    (defun claim-unstake-item (item:object)
        (let (
                (nft-id (at "id" item))
                (owner (at "owner" item))
            )
            (unstake nft-id owner)
        )
    )

    (defun unstake (nft-id:string owner:string)
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
        ; (let ((items (ids-staked-by owner)))
            (map
                (claim-without-unstake-item)
                items
            )
        ; )
    )

    (defun claim-without-unstake-item (item:object)
        (let (
                (nft-id (at "id" item))
                (owner (at "owner" item))
            )
            (claim-without-unstake nft-id owner)
        )
    )

    (defun claim-without-unstake (nft-id:string owner:string)
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
        (require-capability (PRIVATE))

          (let
              (
                (reward (calculate-reward stakedTime))
            )
            ; (install-capability (arkade.token.TRANSFER STAKING_BANK account reward))
            (arkade.token.transfer-create STAKING_BANK account (at 'guard (coin.details account)) reward)

            (emit-event (REWARD_FROM_STAKE account reward))
            (increase-count TOTAL_REWARDS_CLAIMED_KEY reward)
          )
        
    )

    (defun calculate-reward (stakedTime:time)
        (ceiling (* (/ (diff-time (at "block-time" (chain-data)) stakedTime) 86400) 200) 2)
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
                                (* 200 days))
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
                                (* 200 days))
                              0))
                        items)))
          (fold (lambda (reward sum)
                  (+ reward sum))
                0
                rewards)))


    ;;;;; BATTLE FUCNTIONS ;;;;;
      
    (defun create-battle (nft-id:string amount:decimal)
        @doc "Create a new battle with the specified NFT"
        (with-capability (OWNER nft-id)
            (let*
                (
                    (data (read marketplace nft-id))
                    (nft (read nfts nft-id))
                    (staked-data (get-nft-staked nft-id))
                    (battle-id (int-to-str 10 (+ (floor (get-count TOTAL_BATTLES_KEY)) 1)))
                    (owner (at 'owner nft))
                )
                (enforce (> (amount 1000.0)) "Amount must be greater than 1,000 ARKD")
                (arkade.token.transfer-create owner BATTLE_BANK (at 'guard (coin.details owner)) amount)
                (enforce (= (at "for-sale" data) false) "Cannot battle with NFT that is listed")
                (enforce (= (at "staked" staked-data) false) "Cannot battle NFT that is staked")
                (insert battles-table battle-id {
                  "id": battle-id,
                  "playerOneAddress": owner,
                  "playerOneNftId": nft-id,
                  "playerTwoLastAttack": "",
                  "playerTwoAddress": "",
                  "playerTwoNftId": "",
                  "playerTwoLastAttack": "",
                  "winnerAddress": "",
                  "turn": owner,
                  "amount": amount,
                  "created-at": (at "block-time" (chain-data)),
                  "updated-at": (at "block-time" (chain-data))

                  })
                  (update nfts nft-id {
                        "total-games-played": (+ (at "total-games-played" nft) 1)
                  })
                
                (increase-count TOTAL_BATTLES_KEY 1.0)
            )
            )
        )

    (defun join-battle (battle-id:string nft-id:string)
        @doc "Join an existing battle with the specified NFT"
        (with-capability (OWNER nft-id)
            (let*
                (
                    (data (read marketplace nft-id))
                    (staked-data (get-nft-staked nft-id))
                    (battle (read battles-table battle-id))
                    (amount (at 'amount battle))
                    (nft (read nfts nft-id))
                    (owner (at 'owner nft))
                )
                    (arkade.token.transfer-create owner BATTLE_BANK (at 'guard (coin.details owner)) amount)
                    (enforce (= (at "for-sale" data) false) "Cannot battle with NFT that is listed")
                    (enforce (= (at "staked" staked-data) false) "Cannot battle NFT that is staked")
                    (enforce  (!= (at 'playerOneAddress battle) owner) "You cannot join your own battle")
                    (enforce (= (at 'playerTwoAddress battle) "") "The battle is already full")
                    (enforce (> (at 'current-health-power nft) 0) "Cannot battle with fainted NFT")
                    
                    (update battles-table battle-id {
                      "amount": (* 2 (at 'amount battle)),
                      "playerTwoAddress": owner,
                      "playerTwoNftId": nft-id,
                      "updated-at": (at "block-time" (chain-data))
                    })
                    (update nfts nft-id {
                        "total-games-played": (+ (at "total-games-played" nft) 1)
                    })
                )
            )
        )

    (defun perform-attack (nft-id:string battle-id:string attack-name:string)
      @doc "Perform an attack from the attacker NFT to the defender NFT and update their health accordingly"
      (with-capability (OWNER nft-id)
        (let*
          (
            (data (read marketplace nft-id))
            (nft (read nfts nft-id))
            (owner (at 'owner nft))
            (battle (read battles-table battle-id))
            (amount (at 'amount battle))
            (playerOne (at 'playerOneAddress battle))
            (playerTwo (at 'playerTwoAddress battle))
            (playerTurn (at 'turn battle))
            (attacker-nft-id (if (= playerOne owner) (at 'playerOneNftId battle) (at 'playerTwoNftId battle)))
            (defender-nft-id (if (= playerOne owner) (at 'playerTwoNftId battle) (at 'playerOneNftId battle)))
            (attacker (at 'owner (read nfts attacker-nft-id)))
            (defender (at 'owner (read nfts defender-nft-id)))
            (attack (find-attack attack-name nft-id))
            (damage (at "occurrence" attack))
          )
          (enforce (= (at "for-sale" data) false) "Cannot battle with NFT that is listed")
          (enforce (= (at 'winnerAddress battle) "") "Battle has already ended")
          (enforce (= playerTurn owner) "Not your turn")
          (enforce (> (at 'current-health-power attacker) 0) "Cannot battle with fainted NFT")
          (let
            (
              (new-defender-health-power (calculate-health (at 'current-health-power defender) damage))
            )
            (update nfts defender-nft-id {
              "current-health-power": new-defender-health-power,
              "damage-taken": (+ (at "damage-taken" defender) damage),
              "total-losses": (if (= new-defender-health-power 0) (+ (at "total-losses" defender) 1) (at "total-losses" defender))
            })
            (update nfts attacker-nft-id {
              "damage-given": (+ (at "damage-given" attacker) damage),
              "total-wins": (if (= new-defender-health-power 0) (+ (at "total-wins" attacker) 1) (at "total-wins" attacker))
            })
            (if (= new-defender-health-power 0) 
              (update battles-table battle-id {
                "turn": defender,
                "playerOneLastAttack": (if (= owner playerOne) attack (at 'playerOneLastAttack battle)),
                "playerTwoLastAttack": (if (= owner playerTwo) attack (at 'playerTwoLastAttack battle)),
                "updated-at": (at "block-time" (chain-data))
              })
              (update battles-table battle-id {
                "turn": defender,
                "winnerAddress": (if (= new-defender-health-power 0) attacker ""),
                "playerOneLastAttack": (if (= owner playerOne) attack (at 'playerOneLastAttack battle)),
                "playerTwoLastAttack": (if (= owner playerTwo) attack (at 'playerTwoLastAttack battle)),
                "updated-at": (at "block-time" (chain-data))
              })
            )
            (if (= new-defender-health-power 0) 
                (install-capability (coin.TRANSFER OFFERS_BANK attacker amount))
                (coin.transfer OFFERS_BANK attacker amount)
                (increase-count TOTAL_BATTLE_REWARDS_KEY amount)
             )
          )
        )
      )
    )

    (defun find-attack (attack-name:string nft-id:string)
      @doc "Find an attack by name from the list of attacks"
      (let
        (
          (attacks (at 'attacks (at 'attributes (read nfts nft-id))))
        )
        (fold (lambda (result attack)
                (if (and (= "" (at 'name result)) (= attack-name (at 'name attack)))
                  attack
                  result))
              ""
              attacks)))

    (defun calculate-health
      (current-health:integer damage:integer)
      @doc "Calculate the new health after taking damage"
      (if (< (- current-health damage) 0)
        0
        (- current-health damage)))


    (defun quit-game (battle-id:string nft-id:string)
        @doc "Forfeit the battle, allowing the other player to win"
        (with-capability (OWNER nft-id)
            (let*
              (
                (battle (read battles-table battle-id))
                (nft (read nfts nft-id))
                (owner (at 'owner nft))
                (amount (at 'amount battle))
                (playerTurn (at 'turn battle))
                (playerOne (at 'playerOneAddress battle))
                (playerTwo (at 'playerTwoAddress battle))
                (last-updated (at 'updated-at battle))
                (current-time (at "block-time" (chain-data)))
              )
              (enforce (= (at 'winnerAddress battle) "") "Battle has already ended")
              (enforce (or (= owner playerOne) (= owner playerTwo)) "You are not part of this battle")
              (enforce (= playerTurn owner) "Not your turn")
              (enforce (!= (at 'turn battle) "") "Battle has not started yet")
                (update battles-table battle-id {
                  "winnerAddress": (if (= owner playerOne) playerTwo playerOne),
                  "playerOneLastAttack": (if (= owner playerOne) "Forfeit" (at 'playerOneLastAttack battle)),
                  "playerTwoLastAttack": (if (= owner playerTwo) "Forfeit" (at 'playerTwoLastAttack battle)),
                  "updated-at": (at "block-time" (chain-data))
                })
                (let*
                    (
                      (winner (if (= owner playerOne) playerTwo playerOne))
                      (loser (if (= owner playerOne) playerOne playerTwo))
                    )
                    (update nfts (at (if (= owner playerOne) 'playerTwoNftId 'playerOneNftId) battle) {
                      "total-wins": (+ (at "total-wins" (at (if (= owner playerOne) 'playerTwoNftId 'playerOneNftId) battle)) 1)
                    })
                    (update nfts (at (if (= owner playerOne) 'playerOneNftId 'playerTwoNftId) battle) {
                      "total-losses": (+ (at "total-losses" (at (if (= owner playerOne) 'playerOneNftId 'playerTwoNftId) battle)) 1)
                    })
                    (install-capability (coin.TRANSFER OFFERS_BANK winner amount))
                    (coin.transfer OFFERS_BANK winner amount)
                    (increase-count TOTAL_BATTLE_REWARDS_KEY amount)
                )
            )
        )
    )

    (defun end-game (battle-id:string nft-id:string)
        @doc "End the game and allow the other player to win if it's been 2 days since the last player made a move"
        (with-capability (OWNER nft-id)
            (let*
                (
                    (battle (read battles-table battle-id))
                    (nft (read nfts nft-id))
                    (owner (at 'owner nft))
                    (amount (at 'amount battle))
                    (playerTurn (at 'turn battle))
                    (playerOne (at 'playerOneAddress battle))
                    (playerTwo (at 'playerTwoAddress battle))
                    (last-updated (at 'updated-at battle))
                    (current-time (at "block-time" (chain-data)))
                )
                (enforce (= (at 'winnerAddress battle) "") "Battle has already ended")
                (enforce (or (= owner playerOne) (= owner playerTwo)) "You are not part of this battle")
                (enforce (!= playerTurn owner) "Cannot end a game when it's your turn")
                (enforce (!= (at 'turn battle) "") "Battle has not started yet")
                (enforce (not (is-two-days-or-more last-updated current-time)) "2 days have not passed since the last move")
    
                (update battles-table battle-id {
                  "winnerAddress": owner,
                  "playerOneLastAttack": (if (= owner playerOne) "Timed Out" (at 'playerOneLastAttack battle)),
                  "playerTwoLastAttack": (if (= owner playerTwo) "Timed Out" (at 'playerTwoLastAttack battle)),
                  "updated-at": (at "block-time" (chain-data))
                })
                (let*
                    (
                      (loser (if (= owner playerOne) playerTwo playerOne))
                      (winner (if (= owner playerOne) playerOne playerTwo))
                    )
                    (update nfts (at (if (= owner playerOne) 'playerTwoNftId 'playerOneNftId) battle) {
                      "total-wins": (+ (at "total-wins" (at (if (= owner playerOne) 'playerTwoNftId 'playerOneNftId) battle)) 1)
                    })
                    (update nfts (at (if (= owner playerOne) 'playerOneNftId 'playerTwoNftId) battle) {
                      "total-losses": (+ (at "total-losses" (at (if (= owner playerOne) 'playerOneNftId 'playerTwoNftId) battle)) 1)
                    })
                    (install-capability (coin.TRANSFER OFFERS_BANK winner amount))
                    (coin.transfer OFFERS_BANK winner amount)
                    (increase-count TOTAL_BATTLE_REWARDS_KEY amount)
                )
            )
        )
    )

    (defun heal (nft-id:string heal-level:string)
        @doc "Heal an NFT"
      (with-capability (OWNER nft-id)
        (let*
          (
            (data (read marketplace nft-id))
            (staked-data (get-nft-staked nft-id))
            (nft (read nfts nft-id))
            (account (at 'owner nft))
            (max-health-power (at 'max-health-power (at 'attributes nft)))
            (cost (cond
                ((= "quarter" heal-level) 750.0)
                ((= "half" heal-level) 1400.0)
                2000.0)
            )
            (new-health-power (cond
                ((= "quarter" heal-level) max-health-power / 4)
                ((= "half" heal-level) max-health-power / 2)
                max-health-power)
            )
            (current-health-power (+ (at 'current-health-power nft) new-health-power))
          )
          (arkade.token.transfer-create account ADMIN_ADDRESS (at 'guard (coin.details account)) cost)
          (enforce (= (at "staked" staked-data) false) "Cannot heal an NFT that is staked")
          (enforce (= (at "for-sale" data) false) "Cannot heal NFT that is listed")
          (if (> current-health-power max-health-power)
              (update nfts nft-id {"current-health-power": max-health-power})
              (update nfts nft-id {"current-health-power": current-health-power})
          )
        )
      )
    )



    (defun pending-battles (address:string)
          @doc "All pending battles waiting for opponent"
          (select battles-table 
            ["id", "playerOneAddress", "playerOneNftId", "playerTwoAddress", "playerTwoNftId", "created-at", "turn"] 
            (where "playerTwoAddress" (= "")))
    )

    (defun active-battles (address:string)
          @doc "All active battles"
          (select battles-table
            ["id", "playerOneAddress", "playerOneNftId", "playerTwoAddress", "playerTwoNftId", "updated-at", "created-at", "turn"]
            (where "turn" (> 0)))
    )

    (defun ended-battles (address:string)
          @doc "All ended battles"
          (select battles-table
            ["id", "playerOneAddress", "playerOneNftId", "playerTwoAddress", "playerTwoNftId", "created-at", "updated-at", "turn"]
            (where "winnerAddress" (!= "")))
    )

    (defun battles-won-by (address:string)
          @doc "All battles won by someone"
          (select battles-table 
            ["id", "winnerAddress", "turn"] 
            (where "winnerAddress" (= address)))
    )

    (defun is-even (num:integer)
      @doc "Check if the given integer is even"
      (= 0 (mod num 2))
    )

    (defun is-two-days-or-more (start-time:time end-time:time)
      @doc "Check if two or more days have passed between two times"
      (>= (diff-time end-time start-time) (* 2 24 60 60))
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
              (at "role" (read whitelist account ["role"]))
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

      (defun backfill-mp-bulk (items:list)
          @doc "Backfill bulk"
          (with-capability (ADMIN)
              (map (lambda (item) (backfill-mp-item item)) items)
          )
      )

      (defun backfill-mp-item (item:object)
          @doc "Backfill"
          (require-capability (PRIVATE))

          (let (
                  (id (at "id" item) )
                (owner (at "owner" item) )
              )
            (insert marketplace id {
                "id": id,
                "for-sale": false, 
                "price": -1.0, 
                "updated-at": (at "block-time" (chain-data)),
                "owner": owner
            })
          )
      )

        (defun backfill-metadata-bulk (items:list)
          @doc "Backfill bulk"
          (with-capability (ADMIN)
              (map (lambda (item) (backfill-mp-item item)) items)
          )
      )

      (defun backfill-metadata-item (item:object)
          @doc "Backfill"
          (require-capability (PRIVATE))

          (let (
                (id (at "edition" item) )
                (level (at "level" item) )
                (maxHealth (at "maxHealth" item) )
                (attacks (at "attacks" item) )
              )
            (update nfts id {
                "id": id,
                "level": level, 
                "max-health-power": maxHealth,
                "current-health-power": maxHealth,
                "attacks": attacks
            })
          )
      )

  
  )


    
