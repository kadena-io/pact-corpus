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
  (defconst PRICE_KEY 5.0)
  (defconst FEE_KEY 2)
  (defconst ADMIN_KEYSET "free.wizequipment-keyset")
  (defconst ADMIN_ADDRESS "k:90f45921e0605560ace17ca8fbbe72df95ba7034abeec7a8a7154e9eda7114eb")

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

  (defcap EQUIPMENT_MINTED (id:string owner:string)
      @doc "Emitted event when an Equipment is minted"
      @event true
  )

  (defcap EQUIPMENT_BUY (id:string buyer:string seller:string price:decimal)
      @doc "Emitted event when an Equipment is purchased"
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

  (deftable equipment:{equip-main-schema})
  (deftable creation:{creation-schema})
  (deftable equipped:{equipped-schema})
  (deftable counts:{counts-schema})
  (deftable values:{values-schema})
  (deftable volume:{volume-schema})

  ; Can only happen once
  ; --------------------------------------------------------------------------

  (defun initialize ()
      @doc "Initialize the contract the first time its loaded "
      (insert counts MINTED_POST_COUNT_KEY {"count": 0})
      (insert counts MINTED_COUNT_KEY {"count": 0})
      (insert counts NFTS_COUNT_KEY {"count": 0})
      (insert volume VOLUME_PURCHASE_COUNT {"count": 0.0})
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;:;;;;;;; EQUIP CREATION , ADMIN ONLY ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun create-all-equipment (objects-list:list)
      @doc "take a list of multiple wizards, and create each wizard"
      (with-capability (ADMIN)
          (map
              (create-equipment)
              objects-list
          )
      )
  )

  (defun create-equipment (item-list:object)
      @doc "take a list of traits, to create a wizard"
      (require-capability (ADMIN))
      (let
          (
              (id (int-to-str 10(get-count NFTS_COUNT_KEY)))
          )
          (insert creation id
              {"url": (at "url" item-list),
              "bonus": (at "bonus" item-list),
              "name": (at "name" item-list)}
          )
      )
      (increase-count NFTS_COUNT_KEY)
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
          )
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
                  "equippedToId": ""
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
                (data (get-equipment-fields-for-id iditem))
                (nft-data (m::get-wizard-fields-for-id (str-to-int idnft)))
                (nft-equipped-data (get-equipped-fields-for-id idnft))
            )
            (enforce (= (at "owner" nft-data) owner) "you are not the owner of this wizard")
            (enforce (= (at "listed" nft-data) false) "can't equip a listed wizard")
            (enforce (= (at "listed" data) false) "can't equip a listed item")
            (enforce (= (at "equipped" nft-equipped-data) false) "Can't equip an already equipped wizard")
            (enforce (= (at "equipped" data) false) "can't equip an already equipped item")
            (write equipped idnft {
                "id": iditem,
                "url": (at "url" data),
                "bonus": (at "bonus" data),
                "name": (at "name" data),
                "equipped": true
            })
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
                (data (get-equipment-fields-for-id iditem))
                (nft-data (ma::get-wizard-fields-for-id (str-to-int idnft)))
            )
            (enforce (= (at "owner" nft-data) owner) "you are not the owner of this wizard")
            (enforce (= (at "equipped" data) true) "this item is not equipped")
            (mw::spend-wiza 120.0 owner)
            (write equipped idnft {
                "id": iditem,
                "url": (at "url" data),
                "bonus": (at "bonus" data),
                "name": (at "name" data),
                "equipped": false
            })
            (update equipment iditem {
                "equipped": false,
                "equippedToId": ""
            })
        )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;; UTILS  ;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (if
              (< (str-to-int 10 id) max-reveal)
              (let (
                      (info (read equipment id))
                  )
                  info
              )
              ""
          )
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
            "equipped":equipped}
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
)


