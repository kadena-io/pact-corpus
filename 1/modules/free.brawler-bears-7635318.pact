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
          total-games-played:integer
          total-wins:integer
          total-losses:integer
          total-damage-given:integer
          total-damage-taken:integer
          total-earnings:decimal
          current-battle-id:string
          default-attack:object
          attack-one:string
          attack-two:string
          attack-three:string
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
        (ceiling (* (/ (diff-time (at "block-time" (chain-data)) stakedTime) 86400) 50) 2)
    )

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
                                (* 50 days))
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
                                (* 50 days))
                              0))
                        items)))
          (fold (lambda (reward sum)
                  (+ reward sum))
                0
                rewards)))


    ;;;;; BATTLE FUCNTIONS ;;;;;

    (defun equip:string (bear-id:string attack-id:string)
        @doc "Equip an attack to an NFT."
        (let* (
                ; (mp-data (read marketplace bear-id))
               (bear (read nfts bear-id))
               (owner (at 'owner bear))
               (current-battle-id (at 'current-battle-id bear))
               (attack-one (at 'attack-one bear))
               (attack-two (at 'attack-two bear))
               (attack-three (at 'attack-three bear)))
        ;   (enforce (= (at "for-sale" mp-data) false) "Cannot equip to a Bear that is listed")
          (enforce (= current-battle-id "") "Cannot equip to a Bear in a battle")
          (with-capability (OWNER bear-id)
            (write marketplace bear-id {
              "id": bear-id,
              "for-sale": false, 
              "price": -1.0, 
              "updated-at": (at "block-time" (chain-data)),
              "owner": owner
            })
            (if (= attack-one "")
              (update nfts bear-id {"attack-one": attack-id})
              (if (= attack-two "")
                (update nfts bear-id {"attack-two": attack-id})
                (if (= attack-three "")
                  (update nfts bear-id {"attack-three": attack-id})
                  (enforce false "All attack slots are occupied")))))))
    

    (defun unequip:string (bear-id:string attack-id:string)
        @doc "Unequip an attack from an NFT."
        (let* ((bear (read nfts bear-id))
                (current-battle-id (at 'current-battle-id bear))
                (attack-one (at 'attack-one bear))
                (attack-two (at 'attack-two bear))
                (attack-three (at 'attack-three bear)))
        (enforce (= current-battle-id "") "Cannot unequip from a Bear in a battle")
        (with-capability (OWNER bear-id)
            (if (= attack-one attack-id)
            (update nfts bear-id {"attack-one": ""})
            (if (= attack-two attack-id)
                (update nfts bear-id {"attack-two": ""})
                (if (= attack-three attack-id)
                (update nfts bear-id {"attack-three": ""})
                (enforce false "Attack not found on this bear")))))))
            
      
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
                (enforce (> amount 1000.0) "Amount must be greater than 1,000 ARKD")
                (with-capability (PRIVATE)(arkade.token.transfer owner BATTLE_BANK amount))
                (enforce (= (at "for-sale" data) false) "Cannot battle with NFT that is listed")
                (enforce (= (at "staked" staked-data) false) "Cannot battle NFT that is staked")
                (insert battles-table battle-id {
                  "id": battle-id,
                  "playerOneAddress": owner,
                  "playerOneNftId": nft-id,
                  "playerOneLastAttack": "",
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
                        "total-games-played": (+ (at "total-games-played" nft) 1),
                        "current-battle-id": battle-id
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
                        ,"current-battle-id": battle-id
                    })
                )
            )
        )

    (defun perform-attack (nft-id:string battle-id:string attack-id:string default:bool)
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
            (attack (if (= default true) (at "default-attack" nft) (free.brawler-bears-attacks.get-nft-fields-for-id [] attack-id)))
            (damage (at "damage" attack))
            (power (at "power" attack))
          )
          (enforce (> power 0) "This attack is out of power")
          (enforce (> power (- power 1)) "This attack is out of power")
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
              "total-losses": (if (= new-defender-health-power 0) (+ (at "total-losses" defender) 1) (at "total-losses" defender)),
              "current-battle-id": (if (= new-defender-health-power 0) "" battle-id)

            })
            (update nfts attacker-nft-id {
              "damage-given": (+ (at "damage-given" attacker) damage),
              "total-wins": (if (= new-defender-health-power 0) (+ (at "total-wins" attacker) 1) (at "total-wins" attacker)),
              "total-earnings": (if (= new-defender-health-power 0) (+ (at "total-earnings" attacker) amount) (at "total-earnings" attacker)),
              "current-battle-id": (if (= new-defender-health-power 0) "" battle-id)
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
                (install-capability (arkade.token.TRANSFER BATTLE_BANK attacker amount))
                (arkade.token.transfer BATTLE_BANK attacker amount)
                (increase-count TOTAL_BATTLE_REWARDS_KEY amount)
             )
            (if (= default true) (at "default-attack" nft)
            (update nfts nft-id {
                "default-attack": {
                    "power": (- power 1)
                }
            }) 
            (free.brawler-bears-attacks.perform-attack attack-id))
          )
        )
      )
    )

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
                      "total-wins": (+ (at "total-wins" (at (if (= owner playerOne) 'playerTwoNftId 'playerOneNftId) battle)) 1),
                      "total-earnings": (+ (at "total-earnings" (at (if (= owner playerOne) 'playerTwoNftId 'playerOneNftId) battle)) amount),
                      "current-battle-id": ""
                    })
                    (update nfts (at (if (= owner playerOne) 'playerOneNftId 'playerTwoNftId) battle) {
                      "total-losses": (+ (at "total-losses" (at (if (= owner playerOne) 'playerOneNftId 'playerTwoNftId) battle)) 1),
                      "current-battle-id": ""
                    })
                    (install-capability (arkade.token.TRANSFER BATTLE_BANK winner amount))
                    (arkade.token.transfer BATTLE_BANK winner amount)
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
                      "total-wins": (+ (at "total-wins" (at (if (= owner playerOne) 'playerTwoNftId 'playerOneNftId) battle)) 1),
                      "total-earnings": (+ (at "total-earnings" (at (if (= owner playerOne) 'playerTwoNftId 'playerOneNftId) battle)) amount),
                      "current-battle-id": ""
                    })
                    (update nfts (at (if (= owner playerOne) 'playerOneNftId 'playerTwoNftId) battle) {
                      "total-losses": (+ (at "total-losses" (at (if (= owner playerOne) 'playerOneNftId 'playerTwoNftId) battle)) 1),
                      "current-battle-id": ""
                    })
                    (install-capability (arkade.token.TRANSFER BATTLE_BANK winner amount))
                    (arkade.token.transfer BATTLE_BANK winner amount)
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
 
  
      (defun enforce-account-exists (account:string)
          @doc "Enforces that an account exists in the coin table"
          (let ((coin-account (at "account" (coin.details account))))
              (enforce (= coin-account account) "account was not found")
          )
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
              (map (lambda (item) (backfill-metadata-item item)) items)
          )
      )

      (defun backfill-metadata-item (item:object)
          @doc "Backfill"
          (require-capability (PRIVATE))

          (let (
                (id (at "edition" item) )
                (level (at "level" item) )
                (maxHealth (at "maxHealth" item) )
                (defaultAttack (at "defaultAttack" item) )
              )
            (update nfts id {
                "id": id,
                "level": level, 
                "max-health-power": maxHealth,
                "current-health-power": maxHealth,
                "default-attack": defaultAttack,
                "attack-one": "",
                "attack-two": "",
                "attack-three": "",
                "total-games-played": 0,
                "total-wins": 0,
                "total-losses": 0,
                "total-damage-given": 0,
                "total-damage-taken": 0,
                "total-earnings": 0.0,
                "current-battle-id": ""
            })
          )
      )

  
  )





