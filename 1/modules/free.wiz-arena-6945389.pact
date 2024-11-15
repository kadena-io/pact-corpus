(module wiz-arena ADMIN
  "Wizards Arena NFTs"

    (use coin)
    (implements wizarena-interface-v2)
 ; Constants

    (defconst MINTED_POST_COUNT_KEY "minted-post-count-key")
    (defconst MINTED_COUNT_KEY "minted-count-key")
    (defconst NFTS_COUNT_KEY "nfts-count-key")
    (defconst VOLUME_PURCHASE_COUNT "volume_purchase_count")
    (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
    (defconst PRICE_KEY "price-key")
    (defconst BUYIN_KEY "buyin-key")
    (defconst FEE_KEY "fee-key")
    (defconst FEE_TOURNAMENT_KEY "fee-tournament-key")
    (defconst ADMIN_KEYSET "free.wizarena-keyset")
    (defconst ADMIN_ADDRESS "k:90f45921e0605560ace17ca8fbbe72df95ba7034abeec7a8a7154e9eda7114eb")
    (defconst CLERIC_MINT_ADDRESS "k:9ca8b0b628eb386edafcb66cb90cfd79f349433502e1c1dece1fa097f6801250")
    (defconst WIZ_BANK:string "wiz-bank" "Account holding prizes")

    (defconst TOURNAMENT_OPEN "tournament_open")

    (defconst LEVEL_CAP 300)

    (defconst MINT_PHASE "mint-phase")

    (defconst PVP_OPEN "pvp-open")
    (defconst PVP_WEEK "pvp-week")

    (defconst ID_REVEAL "id-reveal")

    (defconst WIZARDS_OFFERS_COUNT_KEY "wizards-offers-count-key")
    (defconst WIZARDS_OFFERS_BANK:string "wizards-offers-bank" "Account holding offers")

    ;tournament in wiza
    (defconst BUYIN_WIZA_KEY "buyin-wiza-key")
    (defconst FEE_TOURNAMENT_WIZA_KEY "fee-tournament-wiza-key")
    (defconst TOURNAMENT_WIZA_OPEN "tournament_wiza_open")
    (defconst TOURNAMENT_WIZA_LEVEL_CAP 200)

    (defconst TOURNAMENT_NAME "tournament-name")
    (defconst TOURNAMENT_WIZA_NAME "tournament-wiza-name")
; Capabilities


    (defcap PRIVATE ()
        @doc "can only be called from a private context"
        true
    )

    ;; checks that the transaction owner
    (defcap ACCOUNT_GUARD(account:string)
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard (at "guard" (coin.details account)))
    )

    ;; checks the owner of the nft
    (defcap OWNER (account:string id:string)
        @doc "Enforces that an account owns the nft"
        (let
            (
                (nft-owner (at "owner" (read nfts id ["owner"])))
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

    (defcap WIZ_BUY (id:string buyer:string seller:string price:decimal)
        @doc "Emitted event when a Wizard is purchased"
        @event true
    )

    (defcap TOURNAMENT_SUBSCRIPTION (id:string tournament:string)
        @doc "Emitted event when a Wizard signs up for the tournament"
        @event true
    )

    (defcap TOURNAMENT_WIZA_SUBSCRIPTION (id:string tournament:string)
        @doc "Emitted event when a Wizard signs up for the tournament"
        @event true
    )

    (defcap PVP_SUBSCRIPTION (idnft:string pvpweek:string wiza:decimal)
        @doc "Emitted event when a Wizard signs up for the pvp week"
        @event true
    )

    (defcap WITHDRAW_PRIZE (winner:string prize:decimal)
        @event true
    )

    (defcap BURN_NFT (id:string)
        @event true
    )

    (defcap BUY_UPGRADE (id:string stat:string increment:integer wiza-cost:decimal owner:string)
        @event true
    )

    (defcap BUY_UPGRADE-WITH-AP (id:string stat:string increment:integer ap-cost:integer owner:string)
        @event true
    )

    (defcap MAKE_OFFER (id:string from:string amount:decimal duration:integer)
        @event true
    )

    (defcap WITHDRAW_OFFER (idoffer:string from:string amount:decimal)
        @event true
    )

    ;;;; SCHEMAS AND TABLES ;;;;;
    (defschema nft-main-schema
        @doc "Stores core information about each nft"
        id:string
        created:time
        traits:list
        owner:string
        name:string
        imageHash:string
        nickname:string
    )

    (defschema nft-listed-schema
        @doc "stores info about list and price of nft"
        id:string
        listed:bool
        price:decimal
    )

    (defschema creation-schema
        @doc "Initial nft creation"
        traits:list
        name:string
        imageHash:string
    )

    (defschema counts-schema
        @doc "Basic schema used for counting things"
        count:integer
    )

    (defschema volume-schema
        @doc "Basic schema used for counting volume"
        count:decimal
    )

    (defschema values-schema
        @doc "Basic schema used for storing basic values"
        value:string
    )

    (defschema token-schema
        balance:decimal
        guard:guard
    )

    (defschema tournament-sub-schema
        round:string
        idnft:string
        address:string
    )

    (defschema values-tournament-schema
        @doc "Buyin schema"
        value:decimal
    )

    (defschema stats-schema
        id:string
        attack:integer
        damage:integer
        weakness:string
        defense:integer
        element:string
        fights:list
        hp:integer
        medals:object
        resistance:string
        spellSelected:object
        spellbook:list
        ap:integer
        speed:integer
    )

    (defschema upgrade-stat-values-schema
        value:decimal
    )

    (defschema burning-queue-schema
        burned:bool
        confirmBurn:bool
        idnft:string
        account:string
        timestamp:time
    )

    (defschema free-mint-list-schema
        amount:integer
    )

    (defschema wl-mint-schema
        amount:integer
    )

    (defschema account-free-minted-druids-schema
        @doc "keeps track of how many druids an account has minted for free"
        minted:integer
    )

    (defschema account-minted-druids-schema
        @doc "keeps track of how many druids an account has minted"
        minted:integer
    )

    (defschema price-schema
        @doc "Prices schema"
        price:decimal
    )

    (defschema pvp-subscribers-schema
        @doc "PVP subscribers schema"
        pvpweek:string
        idnft:string
        address:string
        spellSelected:object
        rounds:integer
    )

    (defschema offers-schema
        @doc "schema for offers on marketplace"
        id:string
        refnft:string
        buyer:string
        owner:string
        timestamp:time
        expiresat:time
        amount:decimal
        withdrawn:bool
        status:string
        level:integer
    )

    (defschema potions-schema
        @doc "schema for potion bought"
        potionEquipped:string
        potionBought:bool
    )

    (defschema wizards-base-stats-schema
        @doc "schema for wizards basic stats"
        id:string
        hp:integer
        defense:integer
        attack:integer
        damage:integer
        speed:integer
    )

    (deftable nfts:{nft-main-schema})
    (deftable nfts-market:{nft-listed-schema})
    (deftable creation:{creation-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})
    (deftable volume:{volume-schema})
    (deftable token-table:{token-schema})
    (deftable tournaments:{tournament-sub-schema})
    (deftable values-tournament:{values-tournament-schema})
    (deftable stats:{stats-schema})
    (deftable upgrade-stat-values:{upgrade-stat-values-schema})
    (deftable burning-queue-table:{burning-queue-schema})

    (deftable free-mint-table-druids:{free-mint-list-schema})
    (deftable wl-mint-table-druids:{wl-mint-schema})
    (deftable account-free-minted-druids:{account-free-minted-druids-schema})
    (deftable account-minted-druids:{account-minted-druids-schema})
    (deftable price:{price-schema})

    (deftable pvp-subscribers:{pvp-subscribers-schema})

    (deftable offers-table:{offers-schema})

    (deftable potions-table:{potions-schema})

    (deftable wizards-base-stats:{wizards-base-stats-schema})

        ; --------------------------------------------------------------------------
  ; Can only happen once
  ; --------------------------------------------------------------------------

    (defun initialize ()
        @doc "Initialize the contract the first time its loaded "
        (insert counts MINTED_POST_COUNT_KEY {"count": 0})
        (insert counts MINTED_COUNT_KEY {"count": 0})
        (insert counts NFTS_COUNT_KEY {"count": 0})
        (insert volume VOLUME_PURCHASE_COUNT {"count": 0.0})
        (insert values MINT_CHAIN_ID_KEY {"value": "1"})
        (insert values-tournament BUYIN_KEY {"value": 4.0})
        (insert values-tournament FEE_KEY {"value": 7.0})
        (insert values-tournament FEE_TOURNAMENT_KEY {"value": 20.0})

        (insert values TOURNAMENT_OPEN {"value": "0"})

        (insert values MINT_PHASE {"value": "-1"})
        (insert price PRICE_KEY {"price": 10.0})

        (insert values PVP_OPEN {"value":"0"})
        (insert values PVP_WEEK {"value":"w1"})

        (coin.create-account WIZ_BANK (create-module-guard "wiz-holdings"))
        (create-account WIZ_BANK (create-module-guard "wiz-holdings"))

        (insert values ID_REVEAL {"value":"1024"})

        (insert values-tournament BUYIN_WIZA_KEY {"value": 100.0})
        (insert values-tournament FEE_TOURNAMENT_WIZA_KEY {"value": 11.0})
        (insert values TOURNAMENT_WIZA_OPEN {"value": "0"})
        (insert values TOURNAMENT_NAME {"value": "t14"})

        (insert counts WIZARDS_OFFERS_COUNT_KEY {"count": 0})
        (coin.create-account WIZARDS_OFFERS_BANK (create-BANK-guard))
        (create-account WIZARDS_OFFERS_BANK (create-BANK-guard))

    )

    (defun insertValuesUpgrade ()
        (insert upgrade-stat-values "hp" {"value": 61.0})
        (insert upgrade-stat-values "defense" {"value": 19.0})
        (insert upgrade-stat-values "attack" {"value": 9.0})
        (insert upgrade-stat-values "damage" {"value": 7.0})
        (insert upgrade-stat-values "hp_base_cost" {"value": 150.0})
        (insert upgrade-stat-values "defense_base_cost" {"value": 700.0})
        (insert upgrade-stat-values "attack_base_cost" {"value": 700.0})
        (insert upgrade-stat-values "damage_base_cost" {"value": 400.0})

        (insert upgrade-stat-values "hp_ap_cost" {"value": 1.0})
        (insert upgrade-stat-values "defense_ap_cost" {"value": 4.0})
        (insert upgrade-stat-values "attack_ap_cost" {"value": 4.0})
        (insert upgrade-stat-values "damage_ap_cost" {"value": 2.0})

        (insert upgrade-stat-values "speed" {"value": 1.0})
        (insert upgrade-stat-values "speed_base_cost" {"value": 400.0})
    )

    (defun init4()

        (insert values TOURNAMENT_WIZA_NAME {"value": "t1001"})

    )

  ; STATE MODIFYING FUNCTIONS

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


    ;:;NFT CREATION , ADMIN ONLY ;;;;

    (defun create-all-wizards (objects-list:list)
        @doc "take a list of multiple wizards, and create each wizard"
        (with-capability (ADMIN)
            (map
                (create-wizards)
                objects-list
            )
        )
    )

    (defun create-wizards (item-list:object)
        @doc "take a list of traits, to create a wizard"
        (require-capability (ADMIN))
        (let
            (
                (id (int-to-str 10(get-count NFTS_COUNT_KEY)))
                (wizcount (get-count NFTS_COUNT_KEY))
            )
            (insert creation id
                {"traits": {},
                "name": (at "name" item-list),
                "imageHash": (at "imageHash" item-list)}
            )
        )
        (increase-count NFTS_COUNT_KEY)
    )

    (defun add-traits (objects-list:list)
        (with-capability (ADMIN)
            (map
                (add-trait)
                objects-list
            )
        )
    )

    (defun add-trait (item-list:object)
        (require-capability (ADMIN))
        (let (
                (id (at "id" item-list))
            )
            (update nfts id {
                "traits": (at "attributes" item-list)
            })
        )
    )

    (defun add-stats (objects-list:list)
        (with-capability (ADMIN)
            (map
                (add-stat)
                objects-list
            )
        )
    )

    (defun add-stat (item:object)
        (require-capability (ADMIN))
        (let
            (
                (id (at "id" item))
            )
            (insert stats id
                {"id": id,
                "attack": (at "attack" item),
                "damage": (at "damage" item),
                "weakness": (at "weakness" item),
                "defense": (at "defense" item),
                "element": (at "element" item),
                "fights": (at "fights" item),
                "hp": (at "hp" item),
                "medals": (at "medals" item),
                "resistance": (at "resistance" item),
                "spellSelected": (at "spellSelected" item),
                "spellbook": (at "spellbook" item),
                "ap":(at "ap" item),
                "speed": (at "speed" item)}
            )
        )
    )

    (defun add-base-stats (objects-list:list)
        (with-capability (ADMIN)
            (map
                (add-base-stat)
                objects-list
            )
        )
    )

    (defun add-base-stat (item:object)
        (require-capability (ADMIN))
        (let
            (
                (id (at "id" item))
            )
            (insert wizards-base-stats id
                {"id": id,
                "attack": (at "attack" item),
                "damage": (at "damage" item),
                "defense": (at "defense" item),
                "hp": (at "hp" item),
                "speed": (at "speed" item)}
            )
        )
    )

    (defun update-spellbooks (objects-list:list)
        (with-capability (ADMIN)
            (map
                (update-spellbook)
                objects-list
            )
        )
    )

    (defun update-spellbook (item:object)
        (require-capability (ADMIN))
        (let
            (
                (id (at "id" item))
            )
            (update stats id
                {"id": id,
                "spellbook": (at "spellbook" item)}
            )
        )
    )

    (defun update-fights-medals (objects-list:list)
        (with-capability (ADMIN)
            (map
                (update-fight-medal)
                objects-list
            )
        )
    )

    (defun update-fight-medal (item:object)
        (require-capability (ADMIN))
        (let
            (
                (id (at "id" item))
            )
            (with-default-read stats id
                {"fights":[],
                "medals": {}}
                {"fights":=fights,
                "medals":=medals}
                (update stats id
                    {"fights": (+ fights (at "fights" item)),
                    "medals": (at "medals" item)}
                )
            )
        )
    )

    (defun update-aps (objects-list:list)
        (with-capability (ADMIN)
            (map
                (update-ap)
                objects-list
            )
        )
    )

    (defun update-ap (item:object)
        (require-capability (ADMIN))
        (let
            (
                (id (at "id" item))
            )
            (with-default-read stats id
                {"ap":0}
                {"ap":=ap}
                (update stats id
                    {"id": id,
                    "ap": (+ ap (at "ap" item))}
                )
            )
        )
    )

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;; MINT CLERICS FUN ;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defun get-druids (owner:string amount:integer)
        @doc "Mint part 1"
        (enforce (>= amount 1) "Must mint at least 1 wizard")
        (let (
                (wiz-minted (get-count MINTED_COUNT_KEY))
                (wiz-created (get-count NFTS_COUNT_KEY))
                (mint-phase (get-value MINT_PHASE))
                (max-per-mint-phase (get-max-items-druids owner (get-value MINT_PHASE)))
                (mint-price (get-mint-price))
                (minted (get-minted (get-value MINT_PHASE) owner))
            )
            (enforce (!= mint-phase "-1") "Too early to mint")
            (enforce (<= (+ wiz-minted amount) wiz-created) "Tried to mint more wiz then available! Please reduce the amount")
            (enforce (>= (- max-per-mint-phase amount) minted) "Exceed max mint per wallet")
            ; se è fase 1 o 2, fase 0 = free mint
            (if
                (!= mint-phase "0")
                [
                  (install-capability (coin.TRANSFER owner CLERIC_MINT_ADDRESS (* mint-price amount)))
                  (coin.transfer owner CLERIC_MINT_ADDRESS (* mint-price amount))
                ]
                "Admin address"
            )
            ;l'update su quanti ne hai mintati lo facciamo solo nella fase 1 ovvero WL, perché nella prima fase free, minti tutti quelli che hai
            ; in public non importa quanti ne hai
            (if
                (= mint-phase "0")
                (with-default-read account-free-minted-druids owner
                  {"minted": 0}
                  {"minted":= minted }
                  (write account-free-minted-druids owner {"minted": (+ minted amount)})
                )
                ""
            )
            (if
                (= mint-phase "1")
                (with-default-read account-minted-druids owner
                  {"minted": 0}
                  {"minted":= minted }
                  (write account-minted-druids owner {"minted": (+ minted amount)})
                )
                ""
            )
            (with-capability (ACCOUNT_GUARD owner)
                (with-capability (PRIVATE)
                    (increase-volume-by VOLUME_PURCHASE_COUNT (* mint-price amount))
                    (map
                        (get-druid owner)
                        (make-list amount 1)
                    )
                )
            )
        )
    )

    (defun get-druid (owner:string number:integer)
        @doc "Mint part 2"
        (enforce (= number 1) "Number enforced to be 1 to avoid confusion but allow mapping to work")
        (require-capability (PRIVATE))
        (require-capability (ACCOUNT_GUARD owner))
        (let (
                (id (id-for-new-wizard))
            )
            (let (
                    (data (get-latest-wizard-data id))
                )
                (mint-druid id {
                    "id": id,
                    "created": (at "block-time" (chain-data)),
                    "traits": (at "traits" data),
                    "owner": owner,
                    "name": (at "name" data),
                    "imageHash": (at "imageHash" data),
                    "nickname": ""
                })
            )
        )
        (increase-count MINTED_COUNT_KEY)
    )

    (defun mint-druid (id:string data:object)
        @doc "Mint part 3"
        (require-capability (PRIVATE))
        (insert nfts id data)
        (insert nfts-market id {
            "id": id,
            "price": 0.0,
            "listed": false
        })
        (increase-count MINTED_POST_COUNT_KEY)
    )

    (defun get-max-items-druids (address:string phase:string)
        (cond
            (
                (= phase "0")
                (at "amount" (read free-mint-table-druids address ['amount]))
            )
            (
                (= phase "1")
                (at "amount" (read wl-mint-table-druids address ['amount]))
            )
            (
                (= phase "2")
                200
            )
        "")
    )

    (defun get-minted (phase:string owner:string)
        (cond
            (
                (= phase "0")
                (with-default-read account-free-minted-druids owner
                    {"minted": 0}
                    {"minted":= minted }
                    minted
                )
            )
            (
                (= phase "1")
                (with-default-read account-minted-druids owner
                    {"minted": 0}
                    {"minted":= minted }
                    minted
                )
            )
            (
                (= phase "2")
                0
            )
        "")
    )

    (defun add-users-free-mint (accounts:list)
        (with-capability (ADMIN)
            (map
                (add-user-free-mint)
                accounts
            )
        )
    )

    (defun add-user-free-mint (data:object)
        (require-capability (ADMIN))
        (let (
                (address (at "address" data))
                (amount (at "amount" data))
            )
            (write free-mint-table-druids address { "amount": amount })
        )
    )

    (defun add-users-wl-mint (accounts:list)
        (with-capability (ADMIN)
            (map
                (add-user-wl-mint)
                accounts
            )
        )
    )

    (defun add-user-wl-mint (data:object)
        (require-capability (ADMIN))
        (let (
                (address (at "address" data))
                (amount (at "amount" data))
            )
            (write wl-mint-table-druids address { "amount": amount })
        )
    )



    ;;;; MARKTEPLACE FUN ;;;;

    (defun list-wizard (sender:string id:string price:decimal m:module{wiza1-interface-v1} mequip:module{wizequipment-interface-v1})
        @doc "list a wizard on marketplace"
        (enforce (>= price 1.0) "amount must be equal or greater then 1")
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (= (format "{}" [mequip]) "free.wiz-equipment") "not allowed, security reason")
        (let (
                (data (get-wizard-fields-for-id (str-to-int id)))
                (is-staked (check-is-staked id m))
                (has-equip (at "equipped" (check-is-equipped id mequip)))
            )
            (enforce (= is-staked false) "You can't list a staked wizard")
            (enforce (= has-equip false) "You can't list an equipped wizard")
            (enforce (= (at "confirmBurn" data) false) "You can't list a wizard in burning queue")
        )
        (with-capability (OWNER sender id)
            (update nfts-market id {"listed": true, "price": price})
        )
    )

    (defun delist-wizard (sender:string id:string)
        (let (
                (data (get-wizard-fields-for-id (str-to-int id)))
            )
            (enforce (= (at "listed" data) true) "this wizard is not listed")
        )
        (with-capability (OWNER sender id)
            (update nfts-market id {"listed": false, "price": 0.0})
        )
    )

    (defun buy-wizard (id:string newowner:string)
        (let (
                (currentowner (read nfts id ['owner]))
                (market-data (read nfts-market id ['listed 'price]))
            )
            (enforce (= (at "listed" market-data) true) "this wizard is not listed")
            (enforce (> (at "price" market-data) 0.0) "the price is not valid")
            (enforce (!= (at "owner" currentowner) newowner) "the buyer can't be the owner")
            (let (
                    (fee (/ (* (get-value-tournament FEE_KEY) (at "price" market-data)) 100))
                )
                (with-capability (ACCOUNT_GUARD newowner)
                  (install-capability (coin.TRANSFER newowner (at "owner" currentowner) (- (at "price" market-data) fee)))
                  (coin.transfer newowner (at "owner" currentowner) (- (at "price" market-data) fee))
                  (install-capability (coin.TRANSFER newowner ADMIN_ADDRESS fee))
                  (coin.transfer newowner ADMIN_ADDRESS fee)
                  (update nfts id {
                    "owner": newowner
                  })
                  (update nfts-market id {
                    "price": 0.0,
                    "listed": false
                  })
                  (emit-event (WIZ_BUY id newowner (at "owner" currentowner) (at "price" market-data)))
                  (with-capability (PRIVATE)
                      (increase-volume-by VOLUME_PURCHASE_COUNT (at "price" market-data))
                  )
                )
            )
        )
    )

    (defun make-offer (refnft:string buyer:string duration:integer amount:decimal)
        @doc "make an offer for a nft"
        (enforce (> amount 0.0) "Amount must be greater then zero")
        (enforce (> duration 0) "Duration must be at least 1 day")
        (let (
            (currentowner (at "owner" (read nfts refnft ["owner"])))
            (new-offer-id (int-to-str 10 (get-count WIZARDS_OFFERS_COUNT_KEY)))
            (current-level (calculate-level refnft))
          )
          (enforce (!= currentowner buyer) "the buyer can't be the owner")
          (with-capability (ACCOUNT_GUARD buyer)
            (install-capability (coin.TRANSFER buyer WIZARDS_OFFERS_BANK amount))
            (coin.transfer buyer WIZARDS_OFFERS_BANK amount)
            (insert offers-table new-offer-id {
              "id": new-offer-id,
              "refnft": refnft,
              "buyer": buyer,
              "owner": currentowner,
              "timestamp": (at "block-time" (chain-data)),
              "expiresat": (add-time (at "block-time" (chain-data)) (days duration)),
              "amount": amount,
              "withdrawn": false,
              "status": "pending",
              "level": current-level
            })
            (with-default-read token-table WIZARDS_OFFERS_BANK
              {"balance": 0.0}
              {"balance":= oldbalance }
              (update token-table WIZARDS_OFFERS_BANK {"balance": (+ oldbalance amount)})
            )
            (with-capability (PRIVATE)
              (increase-count WIZARDS_OFFERS_COUNT_KEY)
            )
            (emit-event (MAKE_OFFER refnft buyer amount duration))
          )
        )
    )

    (defun cancel-offer (idoffer:string)
      @doc "cancel offer"

      (with-read offers-table idoffer
        {
          "buyer" := buyer,
          "expiresat" := expiresat,
          "amount" := amount,
          "withdrawn" := iswithdrew
        }
        ; enforce some rules
        (with-capability (ACCOUNT_GUARD buyer)
          (enforce (= iswithdrew false) "Cannot withdraw twice")
          (enforce (<= expiresat (at "block-time" (chain-data))) "Cannot cancel offer yet.")
          (with-capability (PRIVATE)
            (install-capability (coin.TRANSFER WIZARDS_OFFERS_BANK buyer amount))
            (coin.transfer WIZARDS_OFFERS_BANK buyer amount)

            (update offers-table idoffer { "withdrawn": true, "status": "canceled" })
            (with-default-read token-table WIZARDS_OFFERS_BANK
              {"balance": 0.0}
              {"balance":= oldbalance }
              (update token-table WIZARDS_OFFERS_BANK {"balance": (- oldbalance amount)})
            )
          )
            (emit-event (WITHDRAW_OFFER idoffer buyer amount))
        )
      )
    )

    (defun accept-offer (idoffer:string m:module{wiza1-interface-v1} mequip:module{wizequipment-interface-v1})
      @doc "Accept an offer"
      (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (= (format "{}" [mequip]) "free.wiz-equipment") "not allowed, security reason")
      (with-read offers-table idoffer
        {
          "refnft" := refnft,
          "buyer" := buyer,
          "timestamp" := timestamp,
          "expiresat" := expiresat,
          "amount" := amount,
          "withdrawn" := iswithdrew,
          "level" := level
        }
        (let (
                (data (get-wizard-fields-for-id (str-to-int refnft)))
                (is-staked (check-is-staked refnft m))
                (has-equip (at "equipped" (check-is-equipped refnft mequip)))
                (current-level (calculate-level refnft))
            )
            (enforce (= is-staked false) "You can't accept offers if a wizard is staked")
            (enforce (= has-equip false) "You can't accept offers for an equipped wizard")
            (enforce (= (at "confirmBurn" data) false) "You can't accept offers if a wizard is in burning queue")
            (enforce (= iswithdrew false) "Cannot withdraw twice")
            (enforce (>= expiresat (at "block-time" (chain-data))) "Offer expired.")
            (enforce (>= current-level level) "You cannot accept this offer because this wizard level is lower than the level when the offer was made")

            (with-capability (OWNER (at "owner" data) refnft)
                (let (
                    (fee (/ (* (get-value-tournament FEE_KEY) amount) 100))
                  )
                  (install-capability (coin.TRANSFER WIZARDS_OFFERS_BANK (at "owner" data) (- amount fee)))
                  (with-capability (PRIVATE)(coin.transfer WIZARDS_OFFERS_BANK (at "owner" data) (- amount fee)))

                  (install-capability (coin.TRANSFER WIZARDS_OFFERS_BANK ADMIN_ADDRESS fee))
                  (with-capability (PRIVATE)(coin.transfer WIZARDS_OFFERS_BANK ADMIN_ADDRESS fee))

                  (update offers-table idoffer { "withdrawn": true, "status": "accepted" })

                  (with-default-read token-table WIZARDS_OFFERS_BANK
                    {"balance": 0.0}
                    {"balance":= oldbalance }
                    (update token-table WIZARDS_OFFERS_BANK {"balance": (- oldbalance amount)})
                  )
                  (update nfts refnft {
                    "owner": buyer
                  })
                  (update nfts-market refnft {
                    "price": 0.0,
                    "listed": false
                  })

                )
                (with-capability (PRIVATE)
                    (emit-event (WIZ_BUY refnft buyer (at "owner" data) amount))
                    (increase-volume-by VOLUME_PURCHASE_COUNT amount)
                )
            )
        )
      )
    )

    (defun increase-volume-by (key:string amount:decimal)
        (require-capability (PRIVATE))
        (update volume key
            {"count": (+ amount (get-volume))}
        )
    )

    (defun get-offers-for-id (id:string)
      @doc "Get all offers for a single nft"
      (select offers-table (and?
                            (where "refnft" (= id))
                            (where "withdrawn" (= false))
                            ))
    )

    (defun get-offers-for-buyer (buyer:string)
      @doc "Get all offers made by a single buyer"
      (select offers-table (and?
                            (where "status" (!= "canceled")) ;se già ritirata non la prendiamo
                            (where "buyer" (= buyer))))
    )

    (defun get-offers-for-owner (owner:string)
      @doc "Get all offers received from owner"
      (select offers-table (and?
                            (where "owner" (= owner))
                            (where "withdrawn" (= false))))
    )

    ;;;;;; TOURNAMENT ;
    (defun subscribe-tournament-mass (subscribers:list address:string)
        (let (
                (buyin (* (get-value-tournament BUYIN_KEY) (length subscribers)))
                (feebuyin (/ (* (* (get-value-tournament BUYIN_KEY) (length subscribers)) (get-value-tournament FEE_TOURNAMENT_KEY)) 100))
                (tournament-open (get-value TOURNAMENT_OPEN))
            )
            (enforce (= tournament-open "1") "Tournament registrations are closed")
            (with-capability (ACCOUNT_GUARD address)
                (install-capability (coin.TRANSFER address ADMIN_ADDRESS feebuyin))
                (coin.transfer address ADMIN_ADDRESS feebuyin)
                (install-capability (coin.TRANSFER address WIZ_BANK (- buyin feebuyin)))
                (coin.transfer address WIZ_BANK (- buyin feebuyin))
                (with-default-read token-table WIZ_BANK
                  {"balance": 0.0}
                  {"balance":= oldbalance }
                  (update token-table WIZ_BANK {"balance": (+ oldbalance (- buyin feebuyin))})
                )
            )
            (with-capability (PRIVATE)
                (map
                    (nft-to-subscribe "kda")
                    subscribers
                )
            )
        )
    )

    (defun nft-to-subscribe (type:string subscriber:object)
        (require-capability (PRIVATE))
        (let (
                (id (at "id" subscriber))
                (round (at "round" subscriber))
                (idnft (at "idnft" subscriber))
                (address (at "address" subscriber))
                (spellSelected (at "spellSelected" subscriber))
                (tournament-name (get-value TOURNAMENT_NAME))
                (tournament-wiza-name (get-value TOURNAMENT_WIZA_NAME))
            )
            (if
                (= type "kda")
                (enforce (= tournament-name round) "can't subscribe to the tournament")
                (enforce (= tournament-wiza-name round) "can't subscribe to the tournament")
            )
            (with-capability (OWNER address idnft)
                (subscribe-tournament id round idnft address spellSelected type)
            )
        )
    )

    (defun subscribe-tournament (id:string round:string idnft:string address:string spellSelected:object type:string)
        @doc "Subscribe a wizard to tournament"
        (require-capability (PRIVATE))
        (let (
                (data-wiz (get-wizard-fields-for-id (str-to-int idnft)))
                (current-level (calculate-level idnft))
            )
            (enforce (= (at "confirmBurn" data-wiz) false) "You can't subscribe a wizard in burning queue")
            (if
                (= type "wiza")
                (enforce (<= current-level TOURNAMENT_WIZA_LEVEL_CAP) "you can't subscribe this wizard")
                ""
            )
        )
        (subscribe-last id round idnft address spellSelected)
        (if
            (= type "kda")
            (emit-event (TOURNAMENT_SUBSCRIPTION idnft round))
            (emit-event (TOURNAMENT_WIZA_SUBSCRIPTION idnft round))
        )
    )

    (defun send-prizes (winners:list)
        (with-capability (ADMIN)
            (map
                (send-prize)
                winners
            )
        )
    )

    (defun send-prize (item:object)
        (require-capability (ADMIN))
        (let (
              (address (at "address" item))
              (prize (at "prize" item))
            )
            (enforce (> prize 0.0) "prize must be greater than 0")
            (install-capability (coin.TRANSFER WIZ_BANK address prize))
            (coin.transfer WIZ_BANK address prize)

            (with-default-read token-table WIZ_BANK
              {"balance": 0.0}
              {"balance":= wizbalance }
              (update token-table WIZ_BANK {"balance": (- wizbalance prize)})
            )
            (emit-event (WITHDRAW_PRIZE address prize))
        )
    )

    (defun get-subscriptions (ids:list)
        @doc "Check if id is subscribed for tournament"
        (map
            (get-subscription)
            ids
        )
    )

    (defun get-subscription (id:string)
        @doc "Check if id is subscribed for tournament"
        (with-default-read tournaments id
            {"address": "",
            "idnft": "",
            "round": ""}
            {"address":=address,
            "idnft":=idnft,
            "round":=round}
            {"address": address, "idnft":idnft, "round": round}
        )
    )

    (defun get-all-subscription-for-tournament (idtournament:string)
        (select tournaments (where "round" (= idtournament)))
    )

    ; tournament wiza
     (defun subscribe-tournament-wiza-mass (subscribers:list address:string m:module{wiza1-interface-v1})
        (let (
                (buyin (* (get-value-tournament BUYIN_WIZA_KEY) (length subscribers)))
                (tournament-open (get-value TOURNAMENT_WIZA_OPEN))
            )
            (enforce (= tournament-open "1") "Tournament registrations are closed")
            (with-capability (ACCOUNT_GUARD address)
                (spend-wiza buyin address m)
            )
            (with-capability (PRIVATE)
                (map
                    (nft-to-subscribe "wiza")
                    subscribers
                )
            )
        )
    )

    (defun subscribe-last (id:string round:string idnft:string address:string spellSelected:object)
        (require-capability (PRIVATE))
        (with-default-read tournaments id
            {"idnft": ""}
            {"idnft":= idnft }
            (enforce (= (length idnft) 0) "Already subscribed to this tournament")
        )
        (insert tournaments id {
            "round": round,
            "idnft": idnft,
            "address": address
        })
        (update stats idnft {
          "spellSelected": spellSelected
        })
    )

    ;;;;;; PVP ;;;;;;;;;

     (defun subscribe-pvp-mass (subscribers:list address:string m:module{wiza1-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (let (
                (pvp-open (get-value PVP_OPEN))
                (buyin (* 1.0 (length subscribers)))
            )
            (enforce (= pvp-open "1") "Pvp week registrations are closed")
            (with-capability (ACCOUNT_GUARD address)
                (install-capability (coin.TRANSFER address CLERIC_MINT_ADDRESS buyin))
                (coin.transfer address CLERIC_MINT_ADDRESS buyin)
            )
            (with-capability (PRIVATE)
                (map
                    (nft-to-subscribe-pvp m)
                    subscribers
                )
            )
        )
    )

    (defun nft-to-subscribe-pvp (m:module{wiza1-interface-v1} subscriber:object)
        (require-capability (PRIVATE))
        (let (
                (id (at "id" subscriber))
                (week (at "week" subscriber))
                (idnft (at "idnft" subscriber))
                (address (at "address" subscriber))
                (spellSelected (at "spellSelected" subscriber))
                (wizaAmount (at "wizaAmount" subscriber))
            )
            (with-capability (OWNER address idnft)
                (subscribe-pvp id week idnft address spellSelected wizaAmount m)
            )
        )
    )

    (defun subscribe-pvp (id:string week:string idnft:string address:string spellSelected:object wiza:integer m:module{wiza1-interface-v1})
        @doc "Subscribe a wizard to pvp arena"
        (require-capability (PRIVATE))
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (>= wiza 30) "You must send at least 30 wiza to participate")
        (let (
                (data-wiz (get-wizard-fields-for-id (str-to-int idnft)))
            )
            (enforce (= (at "confirmBurn" data-wiz) false) "You can't subscribe a wizard in burning queue")
        )
        (with-default-read pvp-subscribers id
            {"idnft": ""}
            {"idnft":= idnft }
            (enforce (= (length idnft) 0) "Already subscribed to this pvp week")
        )
        (with-capability (OWNER address idnft)
            (spend-wiza (+ wiza 0.0) address m)
            (insert pvp-subscribers id {
                "pvpweek": week,
                "idnft": idnft,
                "address": address,
                "spellSelected": spellSelected,
                "rounds": wiza
            })
            (emit-event (PVP_SUBSCRIPTION idnft week (+ wiza 0.0)))
        )
    )

    (defun add-rounds-pvp (id:string wiza:decimal m:module{wiza1-interface-v1})
        (enforce (>= wiza 30.0) "You must send at least 30 wiza to increment max rounds")
        (with-default-read pvp-subscribers id
            {"idnft": "",
            "rounds": 0}
            {"idnft":= idnft,
            "rounds":= rounds}
            (enforce (> (length idnft) 0) "Not subscribed to this pvp week")
            (let (
                    (data-wiz (get-wizard-fields-for-id (str-to-int idnft)))
                )
                (with-capability (OWNER (at "owner" data-wiz) idnft)
                    (spend-wiza wiza (at "owner" data-wiz) m)
                    (update pvp-subscribers id {
                        "rounds": (+ rounds (round wiza))
                    })
                )
            )
        )
    )

    (defun change-spell-pvp (id:string idnft:string address:string spellSelected:object)
        (with-capability (OWNER address idnft)
            (update pvp-subscribers id {
                "spellSelected": spellSelected
            })
        )
    )

    (defun get-pvp-subscription (id:string)
        @doc "Check if id is subscribed for pvp week"
        (read pvp-subscribers id)
    )

    (defun get-all-subscription-for-pvpweek (idweek:string)
        @doc "Get all subscribers for a pvp week"
        (select pvp-subscribers (where "pvpweek" (= idweek)))
    )


    ;; UPGRADE ;;

    (defun buy-upgrades (account:string idnft:string stat:string increase:integer m:module{wiza1-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")

        (with-capability (OWNER account idnft)
            (let (
                    (current-stat (at stat (get-wizard-fields-for-id (str-to-int idnft))))
                    (new-level (calculate-new-level-mass idnft stat increase))
                )
                (enforce (<= new-level LEVEL_CAP) "Wizard's level cannot exceed the level cap")
                (let (
                        (array-levels-to (drop -1 (enumerate current-stat (+ current-stat increase))))
                    )
                    (let (
                            (wiza-cost (fold (+) 0 (map (calculate-wiza-cost-from stat) array-levels-to)))
                        )
                        (spend-wiza wiza-cost account m)
                        (cond
                            (
                                (= stat "hp")
                                (update stats idnft {
                                    "hp": (+ current-stat increase)
                                })
                            )
                            (
                                (= stat "defense")
                                (update stats idnft {
                                    "defense": (+ current-stat increase)
                                })
                            )
                            (
                                (= stat "attack")
                                (update stats idnft {
                                    "attack": (+ current-stat increase)
                                })
                            )
                            (
                                (= stat "damage")
                                (update stats idnft {
                                    "damage": (+ current-stat increase)
                                })
                            )
                        "")
                        (emit-event (BUY_UPGRADE idnft stat increase wiza-cost account))
                    )
                )
            )
        )
    )

    (defun buy-upgrades-ap (account:string idnft:string stat:string increase:integer)
        (with-capability (OWNER account idnft)
            (let (
                    (current-stat (at stat (get-wizard-fields-for-id (str-to-int idnft))))
                    (new-level (calculate-new-level-mass idnft stat increase))
                    (base-cost (at "value" (read upgrade-stat-values (+ stat "_ap_cost") ['value])))
                )
                (enforce (<= new-level LEVEL_CAP) "Wizard's level cannot exceed the level cap")
                (with-default-read stats idnft
                    {"ap": 0}
                    {"ap":=ap}
                    (enforce (> ap 0) "You have no AP available")
                    (let (
                            (ap-cost (round (* increase base-cost)))
                        )
                        (enforce (>= ap ap-cost) "You don't have enough AP")
                        (update stats idnft {
                            "ap": (- ap ap-cost)
                        })
                        (cond
                            (
                                (= stat "hp")
                                (update stats idnft {
                                    "hp": (+ current-stat increase)
                                })
                            )
                            (
                                (= stat "defense")
                                (update stats idnft {
                                    "defense": (+ current-stat increase)
                                })
                            )
                            (
                                (= stat "attack")
                                (update stats idnft {
                                    "attack": (+ current-stat increase)
                                })
                            )
                            (
                                (= stat "damage")
                                (update stats idnft {
                                    "damage": (+ current-stat increase)
                                })
                            )
                        "")
                        (emit-event (BUY_UPGRADE-WITH-AP idnft stat increase ap-cost account))
                    )
                )
            )
        )
    )

    (defun calculate-wiza-cost-from (stat:string from:integer)
        (let (
                (max-value (at "value" (read upgrade-stat-values stat ['value])))
                (base-cost (at "value" (read upgrade-stat-values (+ stat "_base_cost") ['value])))
            )
            (if
                (= (- max-value from) max-value)
                (let (
                        (last-part (/ base-cost 100))
                        (diff (- max-value 1))
                    )
                    (round (- base-cost (* (* 100 (/ diff max-value)) last-part )) 2)
                )
                (let (
                        (last-part (/ base-cost 100))
                        (diff (- max-value from))
                    )
                    (round (- base-cost (* (* 100 (/ diff max-value)) last-part )) 2)
                )
            )
        )
    )

    (defun calculate-new-level-mass (idnft:string stat:string increase:integer)
        (let (
                (data (get-wizard-fields-for-id (str-to-int idnft)))
            )
            (let (
                    (hp (at "hp" data))
                    (def (at "defense" data))
                    (atk (at "attack" data))
                    (dmg (at "damage" data))
                )
                (cond
                    (
                        (= stat "hp")
                        (round (+ (+ (+ (+ hp increase) (* def 4.67)) (* atk 4.67)) (* dmg 2.67)))
                    )
                    (
                        (= stat "defense")
                        (round (+ (+ (+ hp (* (+ def increase) 4.67)) (* atk 4.67)) (* dmg 2.67)))
                    )
                    (
                        (= stat "attack")
                        (round (+ (+ (+ hp (* def 4.67)) (* (+ atk increase) 4.67)) (* dmg 2.67)))
                    )
                    (
                        (= stat "damage")
                        (round (+ (+ (+ hp (* def 4.67)) (* atk 4.67)) (* (+ dmg increase) 2.67)))
                    )
                "")
            )
        )
    )

    (defun buy-potions (account:string idnft:string key:string potion:string m:module{wiza1-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")

        (with-capability (OWNER account idnft)
            (let (
                    (current-level (calculate-level idnft))
                    (tournament-open (get-value TOURNAMENT_OPEN))
                    (wiza-cost (* (get-wiza-value) 2.1))
                )
                (enforce (= tournament-open "1") "You can't buy vial now")
                (with-default-read potions-table key
                    {"potionEquipped":"",
                    "potionBought":false}
                    {"potionEquipped":=potionEquipped,
                    "potionBought":=potionBought}
                    (enforce (= potionBought false) "Already bought a potion for this tournament")
                    (spend-wiza wiza-cost account m)
                    (write potions-table key {
                        "potionEquipped": potion,
                        "potionBought": true
                    })
                )
            )
        )
    )

    (defun get-potion-for-tournament-mass (keys:list)
        (map
            (get-potion-for-tournament)
            keys
        )
    )

    ;key = tournamentname_idnft e.g t8_342
    (defun get-potion-for-tournament (key:string)
        (with-default-read potions-table key
            {"potionEquipped": ""}
            {"potionEquipped":= potionEquipped}
            {"key":key, "potionEquipped": potionEquipped}
        )
    )

    (defun calculate-level (idnft:string)
        (let (
                (data (get-wizard-fields-for-id (str-to-int idnft)))
            )
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
    )

    (defun spend-ap:object (amount:integer account:string idnft:string)
        (with-capability (OWNER account idnft)
            (with-default-read stats idnft
                {"ap": 0}
                {"ap":=ap}
                (enforce (> ap 0) "You have no AP available")
                (enforce (>= ap amount) "You don't have enough AP")
                (update stats idnft {
                    "ap": (- ap amount)
                })
            )
        )
    )

    (defun spend-wiza (amount:decimal account:string m:module{wiza1-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (m::spend-wiza amount account)
    )

    ;;; DOWNGRADE

    (defun retrain (idnft:string owner:string stat:string amount:integer m:module{wiza1-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (with-capability (OWNER owner idnft)
            (let (
                    (base-stats (get-base-stats idnft))
                    (current-data (get-wizard-fields-for-id (str-to-int idnft)))
                )
                (enforce (>= (- (at stat current-data) amount) (at stat base-stats)) "can't retrain this stat so much")
                (cond
                    (
                        (= stat "hp")
                        (let (
                                (ap-gained (/ amount 2))
                            )
                            (with-read stats idnft
                                {"ap":=ap,
                                "hp":=hp}
                                (update stats idnft
                                    {"ap": (+ ap ap-gained),
                                    "hp": (- hp amount)}
                                )
                            )
                        )
                    )
                    (
                        (= stat "defense")
                        (let (
                                (ap-gained (* amount 2))
                            )
                            (with-read stats idnft
                                {"ap":=ap,
                                "defense":=defense}
                                (update stats idnft
                                    {"ap": (+ ap ap-gained),
                                    "defense": (- defense amount)}
                                )
                            )
                        )
                    )
                    (
                        (= stat "attack")
                        (let (
                                (ap-gained (* amount 2))
                            )
                            (with-read stats idnft
                                {"ap":=ap,
                                "attack":=attack}
                                (update stats idnft
                                    {"ap": (+ ap ap-gained),
                                    "attack": (- attack amount)}
                                )
                            )
                        )
                    )
                    (
                        (= stat "damage")
                        (let (
                                (ap-gained amount)
                            )
                            (with-read stats idnft
                                {"ap":=ap,
                                "damage":=damage}
                                (update stats idnft
                                    {"ap": (+ ap ap-gained),
                                    "damage": (- damage amount)}
                                )
                            )
                        )
                    )
                    (
                        (= stat "speed")
                        (let (
                                (ap-gained amount)
                            )
                            (with-read stats idnft
                                {"ap":=ap,
                                "speed":=speed}
                                (update stats idnft
                                    {"ap": (+ ap ap-gained),
                                    "speed": (- speed amount)}
                                )
                            )
                        )
                    )
                "")
                (spend-wiza (+ (* amount 5) 0.0) owner m)
            )
        )
    )

    (defun get-base-stats (idnft:string)
        (read wizards-base-stats idnft)
    )

    ;;; BURN

    (defun add-to-burning-queue (idnft:string account:string m:module{wiza1-interface-v1} mequip:module{wizequipment-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (= (format "{}" [mequip]) "free.wiz-equipment") "not allowed, security reason")
        (let (
                (data (get-wizard-fields-for-id (str-to-int idnft)))
                (is-staked (check-is-staked idnft m))
                (has-equip (at "equipped" (check-is-equipped idnft mequip)))
            )
            (enforce (= (at "listed" data) false) "You can't burn a listed wizard")
            (enforce (= is-staked false) "You can't burn a staked wizard")
            (enforce (= has-equip false) "You can't burn an equipped wizard")
        )
        (with-capability (OWNER account idnft)
            (write burning-queue-table idnft {
                "burned":false,
                "confirmBurn":true,
                "idnft":idnft,
                "account":account,
                "timestamp": (at "block-time" (chain-data))
            })
        )
    )

    (defun remove-from-burning-queue (idnft:string account:string)
        (with-capability (OWNER account idnft)
            (update burning-queue-table idnft {
                "confirmBurn":false
            })
        )
    )

    (defun burn-nft (idnft:string)
        (with-capability (ADMIN)
            (update burning-queue-table idnft {
                "burned":true,
                "confirmBurn":true,
                "account": WIZ_BANK
            })
            (update nfts idnft {
              "owner": WIZ_BANK
            })
            (update nfts-market idnft {
              "price": 0.0,
              "listed": false
            })
            (emit-event (BURN_NFT idnft))
        )
    )

    (defun get-burning-queue ()
        (select burning-queue-table (and?
            (where 'burned (= false))
            (where 'confirmBurn (= true))
        ))
    )

    (defun get-nft-in-burning-queue (idnft:string)
        (read burning-queue-table idnft)
    )

    (defun check-nft-in-burning-queue (idnft:string)
        (with-default-read burning-queue-table idnft
            {"confirmBurn":false}
            {"confirmBurn":=confirmBurn}
            confirmBurn
        )
    )

    (defun check-is-staked (idnft:string m:module{wiza1-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (m::check-nft-is-staked idnft)
    )

    (defun check-is-equipped (idnft:string m:module{wizequipment-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiz-equipment") "not allowed, security reason")
        (m::get-equipped-fields-for-id idnft)
    )

    ;GENERIC FUN ;;

    (defun update-nickname (id:string address:string nickname:string m:module{wiza1-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (let (
                (wiza-cost (* (get-wiza-value) 1.4))
            )
            (with-capability (OWNER address id)
                (update nfts id {
                  "nickname": nickname
                })
                (spend-wiza wiza-cost address m)
            )
        )
    )

    (defun transfer-wizard (id:string sender:string receiver:string m:module{wiza1-interface-v1} mequip:module{wizequipment-interface-v1})
        @doc "Transfer nft to an account"
        (enforce-account-exists receiver)
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (= (format "{}" [mequip]) "free.wiz-equipment") "not allowed, security reason")
        (with-capability (OWNER sender id)
            (let (
                    (data (get-wizard-fields-for-id (str-to-int id)))
                    (is-staked (check-is-staked id m))
                    (has-equip (at "equipped" (check-is-equipped id mequip)))
                )
                (enforce (= (at "listed" data) false) "A listed wizard cannot be transferred")
                (enforce (= is-staked false) "You can't transfer a staked wizard")
                (enforce (= (at "confirmBurn" data) false) "You can't transfer a wizard in burning queue")
                (enforce (= has-equip false) "You can't transfer an equipped wizard")
            )
            (update nfts id {"owner": receiver})
        )
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

    (defun set-price(value:decimal)
        (with-capability (ADMIN)
            (update price PRICE_KEY
                {"price": value}
            )
        )
    )

    (defun write-new-value(key:string value:string)
        (with-capability (ADMIN)
            (write values key
                {"value": value}
            )
        )
    )

    (defun set-value-tournament(key:string value:decimal)
        (with-capability (ADMIN)
            (update values-tournament key {"value": value})
        )
    )

    ;;;;;; HELPER FUNCTIONS ;;;
    (defun get-wiza-value ()
        (free.wiz-dexinfo.get-wiza-value)
    )

    (defun get-value-tournament(key:string)
        (at "value" (read values-tournament key ['value]))
    )

    (defun get-count (key:string)
        @doc "Gets count for key"
        (at "count" (read counts key ['count]))
    )

    (defun get-value (key:string)
        @doc "Gets value for a key"
        (at "value" (read values key ['value]))
    )

    (defun get-latest-wizard-data (id:string)
        (require-capability (PRIVATE))
        (let (
                (minted-count (get-count MINTED_COUNT_KEY))
                (created-count (get-count NFTS_COUNT_KEY))
            )
            (enforce (< 0 created-count) "no wizard created")
            (enforce (< minted-count created-count) "all wizard minted")
            (let (
                    (data (read creation id ['traits 'name 'imageHash]))
                )
                data
            )
        )
    )

    (defun id-for-new-wizard ()
        @doc "Returns an id for a new wizard to be minted"
        (int-to-str 10 (get-count MINTED_POST_COUNT_KEY))
    )

    (defun wizard-owned-by (owner:string)
        @doc "all ids wizard from owner"
        (select nfts ['id] (where "owner" (= owner)))
    )

    (defun get-wizard-fields-for-ids (ids:list)
        @doc "Return fields for a list of ids"
        (map
            (get-wizard-fields-for-id)
            ids
        )
    )

    (defun get-wizard-fields-for-id:object (id:integer)
        @doc "Return the fields for a given id"
        (let (
                (info-market (read nfts-market (int-to-str 10 id)))
                (info-stat (read stats (int-to-str 10 id)))
                (confirmBurn (check-nft-in-burning-queue (int-to-str 10 id)))
                (max-reveal (str-to-int (get-value ID_REVEAL)))
            )
            ; per fare il reveal vediamo quale id ha la richeista, se è più di 1023 allora è un cleric
            (if
                (< id max-reveal)
                (let (
                        (info (read nfts (int-to-str 10 id)))
                    )
                    (+ (+ (+ info info-market) info-stat) {"confirmBurn":confirmBurn})
                )
                (let (
                        (info (read nfts (int-to-str 10 id) ['created 'owner 'name 'id 'imageHash]))
                    )
                    (+ (+ info info-market) {"confirmBurn":confirmBurn})
                )
            )
        )
    )

    (defun all-wizards ()
        @doc "Returns all the ids"
        (keys nfts)
    )

    (defun get-all-on-sale ()
        (select nfts-market (where "listed" (= true)))
    )

    (defun get-volume ()
        (at "count" (read volume VOLUME_PURCHASE_COUNT ['count]))
    )

    (defun get-prize ()
        (at "balance" (read token-table WIZ_BANK ['balance]))
    )

    (defun get-mint-price()
        (at "price" (read price PRICE_KEY ["price"]))
    )

    (defun check-your-account (account:string)
        (with-capability (ACCOUNT_GUARD account)
            true
        )
    )
)

;(create-table nfts)
  ;  (create-table nfts-market)
  ;  (create-table creation)
  ;  (create-table counts)
  ;  (create-table values)
  ;  (create-table volume)
  ;  (create-table token-table)
  ;  (create-table tournaments)
  ;  (create-table values-tournament)
  ;  (create-table prizes)


  ;  (create-table stats)
  ;  (create-table upgrade-stat-values)

  ;  (create-table burning-queue-table)

  ;  (initialize)
  ;  (insertValuesUpgrade)
  ;(create-table potions-table)

  ;(create-table free-mint-table-druids)
   ; (create-table wl-mint-table-druids)
    ;(create-table account-free-minted-druids)
    ;(create-table account-minted-druids)

