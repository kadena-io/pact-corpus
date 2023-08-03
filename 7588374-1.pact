(module wiz-equipment ADMIN
  "Wizards Arena P2E Equipment NFTs"

  (use coin)
  (implements wizequipment-interface-v1)

  ; Constants
 ; --------------------------------------------------------------------------

  (defconst MINTED_POST_COUNT_KEY "minted-post-count-key")
  (defconst MINTED_COUNT_KEY "minted-count-key")
  (defconst NFTS_COUNT_KEY "nfts-count-key")
  (defconst VOLUME_PURCHASE_COUNT "volume_purchase_count")
  (defconst PRICE_KEY 4.0)
  (defconst FEE_KEY 2)
  (defconst ADMIN_KEYSET "free.wizequipment-keyset")
  (defconst ADMIN_ADDRESS "k:90f45921e0605560ace17ca8fbbe72df95ba7034abeec7a8a7154e9eda7114eb")
  (defconst MINT_START "mint_start")
  (defconst ITEMS_OFFERS_COUNT_KEY "items-offers-count-key")

  (defconst WIZ_EQUIPMENT_OFFERS_BANK:string "wiz-equipment-offers-bank" "Account holding offers")
    
  (defconst WIZ_EQUIPMENT_FUSED:string "wiz-equipment-fused" "account holding fused rings")
  (defconst FORGED_ID "forged-id")
  ; Capabilities
  ; --------------------------------------------------------------------------

  (defcap PRIVATE ()
      @doc "can only be called from a private context"
      true
  )

  ;; checks that the transaction owner
  (defcap ACCOUNT_GUARD(account:string)
      @doc "Verifies account meets format and belongs to caller"
      (enforce (is-principal account) "")
      (enforce-guard (at "guard" (coin.details account)))
  )

  ;; checks the owner of the nft
  (defcap OWNER (account:string id:string)
      @doc "Enforces that an account owns the nft"
      (let
          (
              (nft-owner (at "owner" (read equipment id ["owner"])))
          )
          (enforce (= nft-owner account) "Account is not owner of the NFT")
          (compose-capability (ACCOUNT_GUARD account))
      )
  )

  (defcap ADMIN() ; Used for admin functions
      @doc "Only allows admin to call these"
      (enforce-keyset ADMIN_KEYSET)
      (compose-capability (PRIVATE))
      (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
  )

    (defun create-BANK-guard ()
      (create-user-guard (require-PRIVATE))
  )

  (defun require-PRIVATE ()
      (require-capability (PRIVATE))
  )

  (defcap EQUIPMENT_MINTED (id:string owner:string)
      @doc "Emitted event when an Equipment is minted"
      @event true
  )

  (defcap EQUIPMENT_BUY (id:string buyer:string seller:string price:decimal)
      @doc "Emitted event when an Equipment is purchased"
      @event true
  )

    (defcap MAKE_ITEM_OFFER (itemtype:string from:string amount:decimal duration:integer)
      @event true
  )

  (defcap WITHDRAW_OFFER (idoffer:string from:string amount:decimal)
      @event true
  )

    (defcap FORGE_ITEM (recipe:string ingredients:list account:string)
      @event true
    )

  ;;;; SCHEMAS AND TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defschema creation-schema
      @doc "Initial equipment creation"
      url:string
      bonus:string
      name:string
  )

  (defschema equip-main-schema
      @doc "Stores core information about each equipment"
      id:string
      created:time
      url:string
      bonus:string
      name:string
      owner:string
      listed:bool
      price:decimal
      equipped:bool
      equippedToId:string
      type:string
  )

  (defschema equipped-schema
      @doc "keep track of equipped items"
      id:string
      url:string
      bonus:string
      name:string
      equipped:bool
  )

  (defschema counts-schema
      @doc "Basic schema used for counting things"
      count:integer
  )

  (defschema values-schema
      @doc "Basic schema used for storing basic values"
      value:string
  )

  (defschema volume-schema
      @doc "Basic schema used for counting volume"
      count:decimal
  )

  (defschema offers-schema
      @doc "schema for offers on marketplace"
      id:string
      buyer:string
      itemtype:string
      timestamp:time
      expiresat:time
      amount:decimal
      withdrawn:bool
      status:string
  )

  (defschema token-schema
      balance:decimal
      guard:guard
  )

    (defschema recipe-book-schema
      url:string
      bonus:string
      name:string
      wiza:decimal
      level:integer
      type:string
  )

  (defschema forge-level-schema
      xp:integer
  )

  (deftable equipment:{equip-main-schema})
  (deftable creation:{creation-schema})
  (deftable equipped:{equipped-schema})
  (deftable counts:{counts-schema})
  (deftable values:{values-schema})
  (deftable volume:{volume-schema})

  (deftable offers:{offers-schema})
  (deftable token-table:{token-schema})

    (deftable recipe-book:{recipe-book-schema})
  (deftable forge-level:{forge-level-schema})

  ; Can only happen once
  ; --------------------------------------------------------------------------

  (defun initialize ()
      @doc "Initialize the contract the first time its loaded "
      ;(insert counts MINTED_POST_COUNT_KEY {"count": 0})
      ;(insert counts MINTED_COUNT_KEY {"count": 0})
      ;(insert counts NFTS_COUNT_KEY {"count": 0})
      ;(insert volume VOLUME_PURCHASE_COUNT {"count": 0.0})
    ;(insert values MINT_START {"value": "0"})
    
    ;(insert counts ITEMS_OFFERS_COUNT_KEY {"count": 0})

     ; (coin.create-account WIZ_EQUIPMENT_OFFERS_BANK (create-BANK-guard))
     ; (create-account WIZ_EQUIPMENT_OFFERS_BANK (create-BANK-guard))
     
     (insert counts FORGED_ID {"count": 100000})
      (coin.create-account WIZ_EQUIPMENT_FUSED (create-BANK-guard))
      (create-account WIZ_EQUIPMENT_FUSED (create-BANK-guard))
  )

    ; --------------------------------------------------------------------------
   ; STATE MODIFYING FUNCTIONS, REQUIRE CAPABILITIES
   ; --------------------------------------------------------------------------

     (defun create-account:string (account:string guard:guard)
       @doc "create new account"
       (enforce-reserved account guard)
       (insert token-table account {
           "balance": 0.0,
           "guard": guard
       })
     )

     (defun enforce-reserved:bool
     ( accountId:string
       guard:guard )
     @doc "Enforce reserved account name protocols."
     (let ((r (check-reserved accountId)))
       (if (= "" r) true
         (if (= "k" r)
           (enforce
             (= (format "{}" [guard])
                (format "KeySet {keys: [{}],pred: keys-all}"
                        [(drop 2 accountId)]))
             "Single-key account protocol violation")
           (enforce false
             (format "Unrecognized reserved protocol: {}" [r]))))))

     (defun check-reserved:string (accountId:string)
         " Checks ACCOUNT for reserved name and returns type if \
         \ found or empty string. Reserved names start with a \
         \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
         (let ((pfx (take 2 accountId)))
           (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

     (defun enforce-account-exists (account:string)
         @doc "Enforces that an account exists in the coin table"
         (let ((coin-account (at "account" (coin.details account))))
             (enforce (= coin-account account) "account was not found")
         )
     )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;; MINT Equipment  ;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun get-equipment-1 (owner:string amount:integer)
      @doc "Mint part 1"
      (enforce (>= amount 1) "Must mint at least 1 item")
      (let (
              (equipment-minted (get-count MINTED_COUNT_KEY))
              (equipment-created (get-count NFTS_COUNT_KEY))
              (mint-price PRICE_KEY)
              (mint-start (get-value MINT_START))
          )
          (enforce (= mint-start "1") "the chests are still empty")
          (enforce (<= (+ equipment-minted amount) equipment-created) "Tried to mint more items then available! Please reduce the amount")
          (install-capability (coin.TRANSFER owner ADMIN_ADDRESS (* mint-price amount)))
          (coin.transfer owner ADMIN_ADDRESS (* mint-price amount))
          (with-capability (ACCOUNT_GUARD owner)
              (with-capability (PRIVATE)
                  (map
                      (get-equipment-2 owner)
                      (make-list amount 1)
                  )
              )
          )
      )
  )

  (defun get-equipment-2 (owner:string number:integer)
      @doc "Mint part 2"
      (enforce (= number 1) "Number enforced to be 1 to avoid confusion but allow mapping to work")
      (require-capability (PRIVATE))
      (require-capability (ACCOUNT_GUARD owner))
      (let (
              (id (id-for-new-equipment))
          )
          (let (
                  (data (get-latest-equipment-data id))
              )
              (mint-equipment id {
                  "id": id,
                  "created": (at "block-time" (chain-data)),
                  "url": (at "url" data),
                  "bonus": (at "bonus" data),
                  "name": (at "name" data),
                  "owner": owner,
                  "listed": false,
                  "price": 0.0,
                  "equipped": false,
                  "equippedToId": "",
                  "type":""
              })
              (emit-event (EQUIPMENT_MINTED id owner))
          )
      )
      (increase-count MINTED_COUNT_KEY)
  )

  (defun mint-equipment (id:string data:object)
      @doc "Mint part 3"
      (require-capability (PRIVATE))
      (insert equipment id data)
      (increase-count MINTED_POST_COUNT_KEY)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; MARKTEPLACE FUN ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun list-equipment (sender:string id:string price:decimal)
      @doc "list an equipment on marketplace"
      (enforce (>= price 1.0) "amount must be equal or greater then 1")
      (let (
              (data (get-equipment-fields-for-id id))
          )
          (enforce (= (at "equipped" data) false) "You can't list an equipped item")
      )
      (with-capability (OWNER sender id)
          (update equipment id {"listed": true, "price": price})
      )
  )

  (defun delist-equipment (sender:string id:string)
      @doc "delist a wizard on marketplace"
      (let (
              (data (get-equipment-fields-for-id id))
          )
          (enforce (= (at "listed" data) true) "this item is not listed")
      )
      (with-capability (OWNER sender id)
          (update equipment id {"listed": false, "price": 0.0})
      )
  )

  (defun buy-equipment (id:string newowner:string m:module{wiza1-interface-v2})
      @doc "buy an equipment from marketplace"
      (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
      (let (
              (data (get-equipment-fields-for-id id))
          )
          (enforce (= (at "listed" data) true) "this equipment is not listed")
          (enforce (> (at "price" data) 0.0) "the price is not valid")
          (enforce (!= (at "owner" data) newowner) "the buyer can't be the owner")
          (let (
                  (fee (/ (* FEE_KEY (at "price" data)) 100))
                  (owner-guard (at "guard" (coin.details (at "owner" data))))
                  (admin-guard (at "guard" (coin.details ADMIN_ADDRESS)))
              )
              (with-capability (ACCOUNT_GUARD newowner)
                (install-capability (m::TRANSFER newowner (at "owner" data) (- (at "price" data) fee)))
                (m::transfer-create newowner (at "owner" data) owner-guard (- (at "price" data) fee))
                (install-capability (m::TRANSFER newowner ADMIN_ADDRESS fee))
                (m::transfer-create newowner ADMIN_ADDRESS admin-guard fee)
                (update equipment id {
                  "owner": newowner,
                  "price": 0.0,
                  "listed": false
                })
                (emit-event (EQUIPMENT_BUY id newowner (at "owner" data) (at "price" data)))
                (with-capability (PRIVATE)
                    (increase-volume-by VOLUME_PURCHASE_COUNT (at "price" data))
                )
              )
          )
      )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;; EQUIP/UNEQUIP  ;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun equip-item:object (iditem:string owner:string idnft:string m:module{wizarena-interface-v2})
    (enforce (= (format "{}" [m]) "free.wiz-arena") "not allowed, security reason")
    (with-capability (OWNER owner iditem)
        (let (
                (item-data (get-equipment-fields-for-id iditem))
                (nft-data (m::get-wizard-fields-for-id (str-to-int idnft)))
            )
            (enforce (= (at "owner" nft-data) owner) "you are not the owner of this wizard")
            (enforce (= (at "listed" nft-data) false) "can't equip a listed wizard")
            (enforce (= (at "listed" item-data) false) "can't equip a listed item")
            (enforce (= (at "equipped" item-data) false) "can't equip an already equipped item")
            (if
                (= "ring" (at "type" item-data))
                (let (
                        (nft-equipped-data (get-equipped-fields-for-id idnft))
                    )
                    (enforce (= (at "equipped" nft-equipped-data) false) "Can't equip an already equipped wizard")
                    (write equipped idnft {
                        "id": iditem,
                        "url": (at "url" item-data),
                        "bonus": (at "bonus" item-data),
                        "name": (at "name" item-data),
                        "equipped": true
                    })
                )
                (let (
                        (nft-equipped-data (get-equipped-fields-for-id (+ idnft (at "type" item-data))))
                    )
                    (enforce (= (at "equipped" nft-equipped-data) false) "Can't equip an already equipped wizard")
                    (write equipped (+ idnft (at "type" item-data)) {
                        "id": iditem,
                        "url": (at "url" item-data),
                        "bonus": (at "bonus" item-data),
                        "name": (at "name" item-data),
                        "equipped": true
                    })
                )
            )
            (update equipment iditem {
                "equipped": true,
                "equippedToId": idnft
            })
        )
    )
  )

  (defun unequip-item:object (iditem:string owner:string idnft:string ma:module{wizarena-interface-v2} mw:module{wiza1-interface-v2})
    (enforce (= (format "{}" [ma]) "free.wiz-arena") "not allowed, security reason")
    (enforce (= (format "{}" [mw]) "free.wiza") "not allowed, security reason")
    (with-capability (OWNER owner iditem)
        (let (
                (item-data (get-equipment-fields-for-id iditem))
                (nft-data (ma::get-wizard-fields-for-id (str-to-int idnft)))
            )
            (enforce (= (at "owner" nft-data) owner) "you are not the owner of this wizard")
            (enforce (= (at "equipped" item-data) true) "this item is not equipped")
            (mw::spend-wiza (floor (+ (/ (calculate-level nft-data) 5) 0.0) 1) owner)
            (if
                (= "ring" (at "type" item-data))
                (write equipped idnft {
                    "id": iditem,
                    "url": (at "url" item-data),
                    "bonus": (at "bonus" item-data),
                    "name": (at "name" item-data),
                    "equipped": false
                })
                (write equipped (+ idnft (at "type" item-data)) {
                    "id": iditem,
                    "url": (at "url" item-data),
                    "bonus": (at "bonus" item-data),
                    "name": (at "name" item-data),
                    "equipped": false
                })
            )
            (update equipment iditem {
                "equipped": false,
                "equippedToId": ""
            })
        )
    )
  )

    (defun transfer-equipment (id:string sender:string receiver:string)
      @doc "Transfer nft to an account"
      (enforce-account-exists receiver)
      (with-capability (OWNER sender id)
          (let (
                  (data (get-equipment-fields-for-id id))
              )
              (enforce (= (at "equipped" data) false) "You can't transfer an equipped item")
                (enforce (= (at "listed" data) false) "You can't transfer a listed item")
          )
          (update equipment id {"owner": receiver})
      )
  )

    (defun calculate-level (data:object)
      (let (
              (hp (at "hp" data))
              (def (at "defense" data))
              (atk (at "attack" data))
              (dmg (at "damage" data))
              (speed (at "speed" data))
          )
          (round(+ (+ (+ (+ hp (* def 4.67)) (* atk 4.67)) (* dmg 2.67)) (* speed 2.67)))
      )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;; OFFERS ;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
    (defun make-offer (buyer:string itemtype:string duration:integer amount:decimal m:module{wiza1-interface-v2})
      @doc "make an offer for an item type"
      (enforce (> amount 0.0) "Amount must be greater then zero")
      (enforce (> duration 0) "Duration must be at least 1 day")
      (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
      (let (
          (new-offer-id (int-to-str 10 (get-count ITEMS_OFFERS_COUNT_KEY)))
          (bank-guard (at "guard" (coin.details WIZ_EQUIPMENT_OFFERS_BANK)))
        )
        (with-capability (ACCOUNT_GUARD buyer)
            (install-capability (m::TRANSFER buyer WIZ_EQUIPMENT_OFFERS_BANK amount))
            (m::transfer-create buyer WIZ_EQUIPMENT_OFFERS_BANK bank-guard amount)

          (insert offers new-offer-id {
            "id": new-offer-id,
            "buyer": buyer,
            "itemtype": itemtype,
            "timestamp": (at "block-time" (chain-data)),
            "expiresat": (add-time (at "block-time" (chain-data)) (days duration)),
            "amount": amount,
            "withdrawn": false,
            "status": "pending"
          })
          (with-default-read token-table WIZ_EQUIPMENT_OFFERS_BANK
            {"balance": 0.0}
            {"balance":= oldbalance }
            (update token-table WIZ_EQUIPMENT_OFFERS_BANK {"balance": (+ oldbalance amount)})
          )
          (with-capability (PRIVATE)
            (increase-count ITEMS_OFFERS_COUNT_KEY)
          )
          (emit-event (MAKE_ITEM_OFFER itemtype buyer amount duration))
        )
      )
  )

  (defun accept-offer (idoffer:string iditem:string owner:string m:module{wiza1-interface-v2})
    (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
    (with-read offers idoffer
      {
        "buyer":=buyer,
        "itemtype":=itemtype,
        "timestamp":=timestamp,
        "expiresat":=expiresat,
        "amount":=amount,
        "withdrawn":=iswithdrew
      }
      (let (
            (data (get-equipment-fields-for-id iditem))
          )
          (enforce (= (at "equipped" data) false) "You can't sell an equipped item")
          (enforce (= (at "name" data) itemtype) "The item is not what was requested")
          (enforce (= iswithdrew false) "Cannot accept twice")
          (enforce (>= expiresat (at "block-time" (chain-data))) "Offer expired.")

          (with-capability (OWNER (at "owner" data) iditem)
              (let (
                  (fee (/ (* FEE_KEY amount) 100))
                  (owner-guard (at "guard" (coin.details (at "owner" data))))
                  (admin-guard (at "guard" (coin.details ADMIN_ADDRESS)))
                )
                (with-capability (PRIVATE)
                    (install-capability (m::TRANSFER WIZ_EQUIPMENT_OFFERS_BANK (at "owner" data) (- amount fee)))
                    (m::transfer-create WIZ_EQUIPMENT_OFFERS_BANK (at "owner" data) owner-guard (- amount fee))

                    (install-capability (m::TRANSFER WIZ_EQUIPMENT_OFFERS_BANK ADMIN_ADDRESS fee))
                    (m::transfer-create WIZ_EQUIPMENT_OFFERS_BANK ADMIN_ADDRESS admin-guard fee)

                    (update offers idoffer { "withdrawn": true, "status": "accepted" })
                    (update equipment iditem {
                      "owner": buyer,
                      "price": 0.0,
                      "listed": false
                    })
                    (with-default-read token-table WIZ_EQUIPMENT_OFFERS_BANK
                      {"balance": 0.0}
                      {"balance":= oldbalance }
                      (update token-table WIZ_EQUIPMENT_OFFERS_BANK {"balance": (- oldbalance amount)})
                    )
                    (increase-volume-by VOLUME_PURCHASE_COUNT amount)
                )
                (emit-event (EQUIPMENT_BUY iditem buyer (at "owner" data) amount))
              )
          )
      )
    )
  )

  (defun cancel-offer (idoffer:string m:module{wiza1-interface-v2})
    @doc "cancel offer"
    (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
    (with-read offers idoffer
      {
        "buyer" := buyer,
        "expiresat" := expiresat,
        "amount" := amount,
        "withdrawn" := iswithdrew
      }
      ; enforce some rules
      (let (
            (buyer-guard (at "guard" (coin.details buyer)))
          )
          (with-capability (ACCOUNT_GUARD buyer)
            (enforce (= iswithdrew false) "Cannot withdraw twice")
            (enforce (<= expiresat (at "block-time" (chain-data))) "Cannot cancel offer yet.")
            (with-capability (PRIVATE)
              (install-capability (m::TRANSFER WIZ_EQUIPMENT_OFFERS_BANK buyer amount))
              (m::transfer-create WIZ_EQUIPMENT_OFFERS_BANK buyer buyer-guard amount)

              (update offers idoffer { "withdrawn": true, "status": "canceled" })
              (with-default-read token-table WIZ_EQUIPMENT_OFFERS_BANK
                {"balance": 0.0}
                {"balance":= oldbalance }
                (update token-table WIZ_EQUIPMENT_OFFERS_BANK {"balance": (- oldbalance amount)})
              )
            )
            (emit-event (WITHDRAW_OFFER idoffer buyer amount))
          )
      )

    )
  )

    (defun get-active-offers ()
    @doc "Get all active offers"
    (select offers (where "withdrawn" (= false)))
  )

  (defun get-offers-for-buyer (buyer:string)
    @doc "Get all offers made by a single buyer"
    (select offers (and?
                          (where "status" (!= "canceled")) ;se già ritirata non la prendiamo
                          (where "buyer" (= buyer))))
  )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;; FORGE  ;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defun forge (recipe:string ingredients:list owner:string m:module{wiza1-interface-v2})
    (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
    (map
        (check-ownership owner)
        ingredients
    )
    (with-capability (OWNER owner (at 0 ingredients))
        (let (
                (discount (get-discount-level owner))
            )
            (with-read recipe-book recipe
                {"url":=url,
                "bonus":=bonus,
                "name":=name,
                "wiza":=wiza,
                "level":=level,
                "type":=type}
                (let (
                        (final-cost (- wiza (/ (* wiza discount) 100)))
                        (id (id-for-forged-equipment))
                        (forge-level (get-forge-level owner))
                    )
                    (enforce (>= forge-level level) "the forge level is not high enough for this recipe")

                    (m::spend-wiza final-cost owner)

                    (with-capability (PRIVATE)
                        (update-forge-level owner (round final-cost))
                        (map
                            (melt-item)
                            ingredients
                        )
                        (forge-final id {
                                "id": id,
                                "created": (at "block-time" (chain-data)),
                                "url": url,
                                "bonus": bonus,
                                "name": name,
                                "owner": owner,
                                "listed": false,
                                "price": 0.0,
                                "equipped": false,
                                "equippedToId": "",
                                "type":type
                            }
                        )
                        (emit-event (FORGE_ITEM recipe ingredients owner))
                    )
                )
            )
        )
    )

  )

  (defun check-ownership (owner:string id:string)
    (let (
            (data (get-equipment-fields-for-id id))
        )
        (with-capability (OWNER owner id)
            (enforce (= (contains (at "bonus" data) ",") false) "can't melt this item")
            (enforce (= (at "equipped" data) false) "You can't melt an equipped item")
            (enforce (= (at "listed" data) false) "You can't melt a listed item")
        )
    )
  )

  (defun get-discount-level (owner:string)
    (with-default-read forge-level owner
        {"xp":0}
        {"xp":=xp}
        (cond
            (
                (and (>= xp 2000) (< xp 5000))
                10
            )
            (
                (and (>= xp 5000) (< xp 10000))
                15
            )
            (
                (and (>= xp 10000) (< xp 20000))
                20
            )
            (
                (>= xp 20000)
                25
            )
        1)
    )
  )

  (defun get-forge-xp (owner:string)
    (with-default-read forge-level owner
        {"xp":0}
        {"xp":=xp}
        xp
    )
  )

  (defun get-forge-level (owner:string)
    (with-default-read forge-level owner
        {"xp":0}
        {"xp":=xp}
        (cond
            (
                (and (>= xp 2000) (< xp 5000))
                2
            )
            (
                (and (>= xp 5000) (< xp 10000))
                3
            )
            (
                (and (>= xp 10000) (< xp 20000))
                4
            )
            (
                (>= xp 20000)
                5
            )
        1)
    )
  )

  (defun update-forge-level (owner:string xpgained:integer)
    (require-capability (PRIVATE))
    (with-default-read forge-level owner
        {"xp":0}
        {"xp":=xp}
        (write forge-level owner {"xp": (+ xp xpgained)})
    )
  )

  (defun melt-item (id:string)
    (require-capability (PRIVATE))
    (update equipment id
        {"owner": WIZ_EQUIPMENT_FUSED}
    )
  )

  (defun forge-final (id:string data:object)
    (require-capability (PRIVATE))
    (insert equipment id data)
    (increase-count FORGED_ID)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;; UTILS  ;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defun forge-combination (key:string)
     (read recipe-book key)
  )

  (defun equipment-owned-by (owner:string)
      @doc "all ids equipment from owner"
      (select equipment (where "owner" (= owner)))
  )

  (defun equipment-equipped ()
      @doc "get items equipped"
      (select equipment (where "equipped" (= true)))
  )

  (defun get-equipment-fields-for-ids (ids:list)
      @doc "Return fields for a list of ids"
      (map
          (get-equipment-fields-for-id)
          ids
      )
  )

  (defun get-equipment-fields-for-id:object (id:string)
      @doc "Return the fields for a given id"
      (let (
              (max-reveal (get-count MINTED_COUNT_KEY))
          )
          (cond
              (
                  (< (str-to-int 10 id) max-reveal)
                  (let (
                          (info (read equipment id))
                      )
                      info
                  )
              )
              (
                  (>= (str-to-int 10 id) 100000)
                  (let (
                          (info (read equipment id))
                      )
                      info
                  )
              )
          "")
      )
  )

  (defun get-equipped-fields-for-ids (ids:list)
      @doc "Return fields for a list of ids"
      (map
          (get-equipped-fields-for-id)
          ids
      )
  )

  (defun get-equipped-fields-for-id:object (idnft:string)
      (with-default-read equipped idnft
            {"id": "",
            "url": "",
            "bonus": "",
            "name": "",
            "equipped": false}
            {"id":= id,
            "url":= url,
            "bonus":= bonus,
            "name":= name,
            "equipped":= equipped}

            {"id":id,
            "url": url,
            "bonus":bonus,
            "name":name,
            "equipped":equipped,
            "equippedToId":idnft}
      )
  )


  (defun get-latest-equipment-data (id:string)
      (require-capability (PRIVATE))
      (let (
              (minted-count (get-count MINTED_COUNT_KEY))
              (created-count (get-count NFTS_COUNT_KEY))
          )
          (enforce (< 0 created-count) "no equipment created")
          (enforce (< minted-count created-count) "all equipment minted")
          (let (
                  (data (read creation id ['url 'bonus 'name]))
              )
              data
          )
      )
  )

  (defun id-for-new-equipment ()
      @doc "Returns an id for a new equipment to be minted"
      (int-to-str 10 (get-count MINTED_POST_COUNT_KEY))
  )

  (defun get-count (key:string)
      @doc "Gets count for key"
      (at "count" (read counts key ['count]))
  )

  (defun increase-count(key:string)
      @doc "Increases count of a key in a table by 1"
      (require-capability (PRIVATE))
      (update counts key
          {"count": (+ 1 (get-count key))}
      )
  )

  (defun increase-volume-by (key:string amount:decimal)
      (require-capability (PRIVATE))
      (update volume key
          {"count": (+ amount (get-volume))}
      )
  )

  (defun all-items ()
      @doc "Returns all the ids"
      (keys equipment)
  )

  (defun get-volume ()
      @doc "get volume of purchase"
      (at "count" (read volume VOLUME_PURCHASE_COUNT ['count]))
  )

    (defun get-value (key:string)
      @doc "Gets value for a key"
      (at "value" (read values key ['value]))
  )

  (defun set-value(key:string value:string)
      @doc "Sets the value for a key to store in a table"
      (with-capability (ADMIN)
          (update values key
              {"value": value}
          )
      )
  )

  (defun id-for-forged-equipment ()
      @doc "Returns an id for a new equipment to be minted"
      (int-to-str 10 (get-count FORGED_ID))
  )

  (defun set-recipe-book-value (key:string url:string bonus:string name:string wiza:decimal level:integer type:string)
      @doc "Sets the value for a key to store in a table"
      (with-capability (ADMIN)
          (insert recipe-book key
            {"url": url,
            "bonus":bonus,
            "name":name,
            "wiza":wiza,
            "level":level,
            "type":type}
          )
      )
  )

)

;(create-table offers)
 ;   (create-table token-table)
 
 
