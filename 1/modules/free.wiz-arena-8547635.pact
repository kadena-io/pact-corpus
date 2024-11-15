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
    (defconst DEV_ADDRESS "k:aa7903581556284374b67a395ca5d825765f2cb9c709230100669327cc67b3f1")
    (defconst WIZ_BANK:string "wiz-bank" "Account holding prizes")

    (defconst TOURNAMENT_OPEN "tournament_open")

    (defconst LEVEL_CAP 375)

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
    (defconst TOURNAMENT_WIZA_LEVEL_CAP 275)

    (defconst TOURNAMENT_NAME "tournament-name")
    (defconst TOURNAMENT_WIZA_NAME "tournament-wiza-name")

    (defconst TOURNAMENT_ELITE_NAME "tournament-elite-name")
    (defconst TOURNAMENT_ELITE_OPEN "tournament_elite_open")
    (defconst TOURNAMENT_FARMERS_MAX_LEVEL 175)
    (defconst BUYIN_ELITE_KEY "buyin-elite-key")

    (defconst WIZARDS_CHALLENGES_COUNT_KEY "wizards-challenges-count-key")

    (defconst AUTO_TOURNAMENTS_ID "auto-tournaments-id")
    (defconst WIZ_AUTO_TOURNAMENTS_BANK:string "wiz-auto-tournaments-bank" "Account holding auto tournaments buyins")

    (defconst CONQUEST_BUYIN "conquest_buyin")
    (defconst CONQUEST_OPEN "conquest_open")

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

    (defcap DEV () ; Used for admin functions
        @doc "Only allows dev to call these"
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD DEV_ADDRESS))
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

    (defcap WIZ_TRANSFER (id:string from:string to:string)
        @doc "Emitted event when a Wizard is transferred"
        @event true
    )

    (defcap WIZ_LISTED (id:string owner:string price:decimal)
        @doc "Emitted event when a Wizard is listed"
        @event true
    )

    (defcap WIZ_DELISTED (id:string owner:string)
        @doc "Emitted event when a Wizard is delisted"
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

    (defcap TOURNAMENT_ELITE_SUBSCRIPTION (id:string tournament:string)
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

    (defcap UPGRADE_SPELL (idnft:string spellname:string)
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
        hp:integer
        medals:object
        resistance:string
        spellSelected:object
        spellbook:list
        ap:integer
        speed:integer
        downgrades:integer
        tournaments:object
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

    (defschema wallet-xp-schema
        xp:integer
    )

    (defschema challenges-schema
        id:string
        timestamp:time
        expiresat:time
        amount:decimal
        wiz1id:string
        wiz1owner:string
        wiz1level:integer
        wiz2id:string
        wiz2owner:string
        wiz2level:integer
        status:string
        withdrawn:bool
        fightId:string
        winnerId:string
        blockTime:time
        coin:string
    )

    (defschema spells-schema
        element:string
        atkBase:integer
        dmgBase:integer
        condition:object
        name:string
    )

    (defschema auto-tournaments-schema
        id:string
        createdBy:string
        createdAt:time
        completedAt:time
        players:list
        wallets:list
        buyin:decimal
        maxLevel:integer
        completed:bool
        fights:object
        nPlayers:integer
        name:string
        winners:integer
    )

    (defschema fights-db-schema
        id:string
        tournament:string
        fightId:string
        winner:string
    )

    (defschema upgrade-spells-schema
        attack:integer
        damage:integer
    )

    (defschema conquest-schema
        idnft:string
        season:string
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

    (deftable wallet-xp:{wallet-xp-schema})

    (deftable challenges:{challenges-schema})

    (deftable spells:{spells-schema})

    (deftable auto-tournaments:{auto-tournaments-schema})

    (deftable fights-db:{fights-db-schema})
    (deftable upgrade-spells:{upgrade-spells-schema})

    (deftable conquest-table:{conquest-schema})

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

        (insert values TOURNAMENT_WIZA_NAME {"value": "t1001"})

        (insert counts WIZARDS_OFFERS_COUNT_KEY {"count": 0})
        (coin.create-account WIZARDS_OFFERS_BANK (create-BANK-guard))
        (create-account WIZARDS_OFFERS_BANK (create-BANK-guard))

        (insert values-tournament BUYIN_ELITE_KEY {"value": 300.0})
        (insert values TOURNAMENT_ELITE_OPEN {"value": "0"})
        (insert values TOURNAMENT_ELITE_NAME {"value": "t3000"})

        (insert counts WIZARDS_CHALLENGES_COUNT_KEY {"count": 0})

        (insert counts AUTO_TOURNAMENTS_ID {"count": 0})
        (coin.create-account WIZ_AUTO_TOURNAMENTS_BANK (create-BANK-guard))
        (create-account WIZ_AUTO_TOURNAMENTS_BANK (create-BANK-guard))

        (insert values-tournament CONQUEST_BUYIN {"value": 6.0})
        (insert values CONQUEST_OPEN {"value": "1"})
    )

    (defun insertValuesUpgrade ()
        (insert upgrade-stat-values "hp" {"value": 61.0})
        (insert upgrade-stat-values "defense" {"value": 19.0})
        (insert upgrade-stat-values "attack" {"value": 9.0})
        (insert upgrade-stat-values "damage" {"value": 7.0})
        (insert upgrade-stat-values "speed_ap_cost" {"value": 2.0})

        (insert upgrade-stat-values "hp_ap_cost" {"value": 1.0})
        (insert upgrade-stat-values "defense_ap_cost" {"value": 4.0})
        (insert upgrade-stat-values "attack_ap_cost" {"value": 4.0})
        (insert upgrade-stat-values "damage_ap_cost" {"value": 2.0})
        (insert upgrade-stat-values "speed" {"value": 1.0})

        (write upgrade-stat-values "hp_base_cost" {"value": 1.87})
        (write upgrade-stat-values "defense_base_cost" {"value": 8.75})
        (write upgrade-stat-values "attack_base_cost" {"value": 8.75})
        (write upgrade-stat-values "damage_base_cost" {"value": 5.0})
        (write upgrade-stat-values "speed_base_cost" {"value": 2.5})
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

    ;;; NFT CREATION

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
                {"name": (at "name" item-list),
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
                "tournaments": {},
                "hp": (at "hp" item),
                "medals": (at "medals" item),
                "resistance": (at "resistance" item),
                "spellSelected": (at "spellSelected" item),
                "spellbook": (at "spellbook" item),
                "ap":(at "ap" item),
                "speed": (at "speed" item),
                "downgrades": 32}
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

    (defun mint-elders-1 (amount:integer)
        (with-capability (ADMIN)
            (map
                (mint-elder-2 "k:461ae9f3c9c255112ac3797f6b15699c656c9bc44ed089551a0f792085ef9504")
                (make-list amount 1)
            )
        )
    )

    (defun mint-elder-2 (owner:string number:integer)
        (enforce (= number 1) "Number enforced to be 1 to avoid confusion but allow mapping to work")
        (require-capability (PRIVATE))

        (let (
                (id (id-for-new-wizard))
            )
            (let (
                    (data (get-latest-wizard-data id))
                )
                (mint-elder-3 id {
                    "id": id,
                    "created": (at "block-time" (chain-data)),
                    "traits": [],
                    "owner": owner,
                    "name": (at "name" data),
                    "imageHash": (at "imageHash" data),
                    "nickname": ""
                })
            )
        )
        (increase-count MINTED_COUNT_KEY)
    )

    (defun mint-elder-3 (id:string data:object)
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

    (defun add-spells (objects-list:list)
        (with-capability (ADMIN)
            (map
                (add-spell)
                objects-list
            )
        )
    )

    (defun add-spell (item:object)
        (require-capability (ADMIN))
        (let
            (
                (name (at "name" item))
            )
            (insert spells name
                {"name": name,
                "atkBase": (at "atkBase" item),
                "dmgBase": (at "dmgBase" item),
                "element": (at "element" item),
                "condition": (at "condition" item)}
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
                (update-fight-medal-dev)
                objects-list
            )
        )
    )

    (defun update-fights-medals-dev (objects-list:list)
        (with-capability (DEV)
            (map
                (update-fight-medal-dev)
                objects-list
            )
        )
    )

    (defun update-fight-medal-dev (item:object)
        (require-capability (PRIVATE))
        (let
            (
                (id (at "id" item))
                (key (format "{}_{}" [(at "id" item) (at "tournament" item)]))
            )
            (with-default-read stats id
                {"medals": {}}
                {"medals":=medals}
                (update stats id
                    {"medals": (+ (at "medals" item) medals)}
                )
            )
            (write fights-db key
                {
                    "id": id,
                    "tournament": (at "tournament" item),
                    "fightId": (at "fightId" item),
                    "winner": (at "winner" item)
                }
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

    ;;;; MARKTEPLACE FUN ;;;;

    (defun list-wizard (sender:string id:string price:decimal m:module{wiza1-interface-v3} mequip:module{wizequipment-interface-v1})
        @doc "list a wizard on marketplace"
        (enforce (>= price 1.0) "amount must be equal or greater then 1")
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (= (format "{}" [mequip]) "free.wiz-equipment") "not allowed, security reason")
        (let (
                (data (get-wizard-fields-for-id (str-to-int id)))
                (is-staked (check-is-staked id m))
                (has-ring (at "equipped" (check-is-equipped id mequip)))
                (has-pendant (at "equipped" (check-is-equipped (+ id "pendant") mequip)))
            )
            (enforce (= is-staked false) "You can't list a staked wizard")
            (enforce (= has-ring false) "You can't list an equipped wizard")
            (enforce (= has-pendant false) "You can't list an equipped wizard")
            (enforce (= (at "confirmBurn" data) false) "You can't list a wizard in burning queue")
        )
        (with-capability (OWNER sender id)
            (update nfts-market id {"listed": true, "price": price})
            (emit-event (WIZ_LISTED id sender price))
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
            (emit-event (WIZ_DELISTED id sender))
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

    (defun accept-offer (idoffer:string m:module{wiza1-interface-v3} mequip:module{wizequipment-interface-v1})
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
                (has-ring (at "equipped" (check-is-equipped refnft mequip)))
                (has-pendant (at "equipped" (check-is-equipped (+ refnft "pendant") mequip)))
                (current-level (calculate-level refnft))
            )
            (enforce (= is-staked false) "You can't accept offers if a wizard is staked")
            (enforce (= has-ring false) "You can't accept offers for an equipped wizard")
            (enforce (= has-pendant false) "You can't accept offers for an equipped wizard")
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

    (defun decline-offer (idoffer:string)
        (with-read offers-table idoffer
            {
                "expiresat" := expiresat,
                "refnft" := refnft
            }
            (enforce (>= expiresat (at "block-time" (chain-data))) "Offer expired.")
            (let
                (
                    (data (get-wizard-fields-for-id (str-to-int refnft)))
                )
                (with-capability (OWNER (at "owner" data) refnft)
                    (update offers-table idoffer { "expiresat": (at "block-time" (chain-data)) })
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

    (defun change-spell-tournament (idnft:string address:string spellSelected:object)
        (with-capability (OWNER address idnft)
            (update stats idnft {
              "spellSelected": spellSelected
            })
        )
    )

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

    (defun subscribe-tournament-mass-dev (subscribers:list)
        (with-capability (DEV)
            (map
                (nft-to-subscribe-dev "chaos")
                subscribers
            )
        )
    )

    (defun nft-to-subscribe-dev (type:string subscriber:object)
        (require-capability (DEV))
        (let (
                (id (at "id" subscriber))
                (round (at "round" subscriber))
                (idnft (at "idnft" subscriber))
                (address (at "address" subscriber))
                (spellSelected (at "spellSelected" subscriber))
            )
            (subscribe-tournament id round idnft address spellSelected type)
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
                (tournament-elite-name (get-value TOURNAMENT_ELITE_NAME))
            )
            (cond
                (
                    (= type "kda")
                    (enforce (= tournament-name round) "can't subscribe to the tournament")
                )
                (
                    (= type "wiza")
                    (enforce (= tournament-wiza-name round) "can't subscribe to the tournament")
                )
                (
                    (= type "elite")
                    (enforce (= tournament-elite-name round) "can't subscribe to the tournament")
                )
            "")
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
            (if
                (= type "elite")
                (enforce (<= current-level TOURNAMENT_FARMERS_MAX_LEVEL) "you can't subscribe this wizard")
                ""
            )
        )
        (subscribe-last id round idnft address spellSelected)
        (cond
            (
                (= type "kda")
                (emit-event (TOURNAMENT_SUBSCRIPTION idnft round))
            )
            (
                (= type "wiza")
                (emit-event (TOURNAMENT_WIZA_SUBSCRIPTION idnft round))
            )
            (
                (= type "elite")
                (emit-event (TOURNAMENT_ELITE_SUBSCRIPTION idnft round))
            )
        "")
        (add-xp-to-wallet address 3)
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
     (defun subscribe-tournament-wiza-mass (subscribers:list address:string m:module{wiza1-interface-v3})
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

    ;tournament elite
    (defun subscribe-tournament-elite-mass (subscribers:list address:string m:module{wiza1-interface-v3})
        (let (
                (buyin (* (get-value-tournament BUYIN_ELITE_KEY) (length subscribers)))
                (tournament-open (get-value TOURNAMENT_ELITE_OPEN))
            )
            (enforce (= tournament-open "1") "Tournament registrations are closed")
            (with-capability (ACCOUNT_GUARD address)
                (spend-wiza buyin address m)
            )
            (with-capability (PRIVATE)
                (map
                    (nft-to-subscribe "elite")
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

    ;;;;;; AUTO TOURNAMENTS ;;;;;;;;;
    (defun create-tournament (idnft:string account:string buyin:decimal maxLevel:integer name:string winners:integer nPlayers:integer m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (> buyin 0.0) "Buyin must be greater than 0")
        (enforce (> maxLevel 0) "Max Level must be greater than 0")
        (enforce (contains winners [1 2]) "invalid number of winners")
        (enforce (contains nPlayers [2 4 8 16]) "invalid number of players")
        (with-capability (OWNER account idnft)
            (let (
                    (current-level (calculate-level idnft))
                    (new-tournament-id (int-to-str 10 (get-count AUTO_TOURNAMENTS_ID)))
                    (playerlist (make-list 1 idnft))
                    (wallets (make-list 1 account))
                    (bank-guard (at "guard" (coin.details WIZ_AUTO_TOURNAMENTS_BANK)))
                )
                (enforce (<= current-level maxLevel) "Your wizard has a level greater than the max level")
                (write auto-tournaments new-tournament-id
                    {"id": new-tournament-id,
                    "createdBy": account,
                    "createdAt": (at "block-time" (chain-data)),
                    "completedAt": (at "block-time" (chain-data)),
                    "players": playerlist,
                    "wallets": wallets,
                    "buyin":buyin,
                    "maxLevel": maxLevel,
                    "completed": false,
                    "fights":{},
                    "nPlayers": nPlayers,
                    "name": name,
                    "winners": winners}
                )
                (install-capability (m::TRANSFER account WIZ_AUTO_TOURNAMENTS_BANK buyin))
                (m::transfer-create account WIZ_AUTO_TOURNAMENTS_BANK bank-guard buyin)
                (with-capability (PRIVATE)
                  (increase-count AUTO_TOURNAMENTS_ID)
                )
            )
        )
    )

    (defun join-tournament (tournamentid:string idnft:string account:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (with-capability (OWNER account idnft)
            (let (
                    (current-level (calculate-level idnft))
                    (playerlist (make-list 1 idnft))
                    (wallets (make-list 1 account))
                    (bank-guard (at "guard" (coin.details WIZ_AUTO_TOURNAMENTS_BANK)))
                )
                (with-read auto-tournaments tournamentid
                    {
                        "players":=subscribers,
                        "wallets":=subscriberwallets,
                        "buyin":=buyin,
                        "maxLevel":=maxLevel,
                        "completed":=completed,
                        "nPlayers":=nPlayers
                    }
                    (enforce (= completed false) "Can't join a completed tournament")
                    (enforce (> nPlayers 0) "Tournament has been cancelled")
                    (enforce (< (length subscribers) nPlayers) "Tournament is full")
                    (enforce (<= current-level maxLevel) "Your wizard has a level greater than the max level")
                    (enforce (= (contains idnft subscribers) false) "This wizard is already subscribed")
                    (enforce (= (contains account subscriberwallets) false) "You cannot have multiple wizards in the same tournament")
                    (update auto-tournaments tournamentid
                        {
                            "players": (+ subscribers (make-list 1 idnft)),
                            "wallets": (+ subscriberwallets (make-list 1 account))
                        }
                    )
                    (install-capability (m::TRANSFER account WIZ_AUTO_TOURNAMENTS_BANK buyin))
                    (m::transfer-create account WIZ_AUTO_TOURNAMENTS_BANK bank-guard buyin)
                )
            )
        )
    )

    (defun complete-tournament (tournamentid:string winner:string winner2:string fights:object m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (with-capability (DEV)
            (with-read auto-tournaments tournamentid
                {
                    "buyin":=buyin,
                    "nPlayers":=nPlayers,
                    "wallets":=wallets
                }
                (if
                    (= (length winner2) 0)
                    [
                        (let (
                                (prize (* buyin nPlayers))
                                (fee (/ (* (* buyin nPlayers) 5) 100))
                                (winner-guard (at "guard" (coin.details winner)))
                                (admin-guard (at "guard" (coin.details ADMIN_ADDRESS)))
                            )
                            (install-capability (m::TRANSFER WIZ_AUTO_TOURNAMENTS_BANK winner (- prize fee)))
                            (m::transfer-create WIZ_AUTO_TOURNAMENTS_BANK winner winner-guard (- prize fee))
                            (install-capability (m::TRANSFER WIZ_AUTO_TOURNAMENTS_BANK ADMIN_ADDRESS fee))
                            (m::transfer-create WIZ_AUTO_TOURNAMENTS_BANK ADMIN_ADDRESS admin-guard fee)
                        )
                    ]
                    [
                        (let (
                                (prize1 (/ (* (* buyin nPlayers) 70) 100))
                                (prize2 (/ (* (* buyin nPlayers) 30) 100))
                                (fee (/ (* (* buyin nPlayers) 5) 100))
                                (winner-guard (at "guard" (coin.details winner)))
                                (winner2-guard (at "guard" (coin.details winner2)))
                                (admin-guard (at "guard" (coin.details ADMIN_ADDRESS)))
                            )
                            (install-capability (m::TRANSFER WIZ_AUTO_TOURNAMENTS_BANK winner (- prize1 (/ fee 2))))
                            (m::transfer-create WIZ_AUTO_TOURNAMENTS_BANK winner winner-guard (- prize1 (/ fee 2)))

                            (install-capability (m::TRANSFER WIZ_AUTO_TOURNAMENTS_BANK winner2 (- prize2 (/ fee 2))))
                            (m::transfer-create WIZ_AUTO_TOURNAMENTS_BANK winner2 winner2-guard (- prize2 (/ fee 2)))

                            (install-capability (m::TRANSFER WIZ_AUTO_TOURNAMENTS_BANK ADMIN_ADDRESS fee))
                            (m::transfer-create WIZ_AUTO_TOURNAMENTS_BANK ADMIN_ADDRESS admin-guard fee)
                        )
                    ]
                )
                (update auto-tournaments tournamentid
                    {
                        "completed":true,
                        "fights":fights,
                        "completedAt": (at "block-time" (chain-data))
                    }
                )
            )
        )
    )


    (defun cancel-tournament (tournamentid:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (with-capability (DEV)
            (with-read auto-tournaments tournamentid
                {
                    "wallets":=subscriberwallets,
                    "buyin":=buyin
                }
                (map (refund-single-wallet buyin m) subscriberwallets)
                (update auto-tournaments tournamentid
                    {
                        "nPlayers": 0
                    }
                )
            )
        )
    )

    (defun refund-single-wallet (buyin:decimal m:module{wiza1-interface-v3} account:string)
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (require-capability (PRIVATE))
        (let (
                (account-guard (at "guard" (coin.details account)))
            )
            (install-capability (m::TRANSFER WIZ_AUTO_TOURNAMENTS_BANK account buyin))
            (m::transfer-create WIZ_AUTO_TOURNAMENTS_BANK account account-guard buyin)
        )
    )

    (defun get-pending-auto-tournaments ()
        (select auto-tournaments (and?
            (where "completed" (= false))
            (where "nPlayers" (< 0))
        ))
    )

    (defun get-completed-auto-tournaments ()
        (select auto-tournaments (where "completed" (= true)))
    )

    ;;;;;;;;;;;;;CONQUEST;;;;;;;;;;;;;;;;;;;

    (defun subscribe-to-lord-season-mass (address:string season:string subscribers:list)
        (let (
                (is-open (get-value CONQUEST_OPEN))
                (buyin (* (get-value-tournament CONQUEST_BUYIN) (length subscribers)))
            )
            (enforce (= is-open "1") "Conquest season registrations are closed")
            (with-capability (ACCOUNT_GUARD address)
                (install-capability (coin.TRANSFER address ADMIN_ADDRESS buyin))
                (coin.transfer address ADMIN_ADDRESS buyin)
            )
            (with-capability (PRIVATE)
                (map
                    (subscribe-to-lord-season address season)
                    subscribers
                )
            )
        )
    )

    (defun subscribe-to-lord-season (address:string season:string idnft:string)
        (require-capability (PRIVATE))
        (let (
                (data-wiz (get-wizard-fields-for-id (str-to-int idnft)))
            )
            (enforce (= (at "confirmBurn" data-wiz) false) "You can't subscribe a wizard in burning queue")
        )
        (with-default-read conquest-table (+ idnft season)
            {"idnft": ""}
            {"idnft":= idnft }
            (enforce (= (length idnft) 0) "Already subscribed to this season")
        )
        (with-capability (OWNER address idnft)
            (insert conquest-table (+ idnft season) {
                "season": season,
                "idnft": idnft
            })
        )
        (add-xp-to-wallet address 5)
    )

    (defun get-all-subscription-for-season (season:string)
        (select conquest-table ['idnft] (where "season" (= season)))
    )

    ;;;;;; PVP ;;;;;;;;;

     (defun subscribe-pvp-mass (subscribers:list address:string m:module{wiza1-interface-v3})
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

    (defun nft-to-subscribe-pvp (m:module{wiza1-interface-v3} subscriber:object)
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

    (defun subscribe-pvp (id:string week:string idnft:string address:string spellSelected:object wiza:integer m:module{wiza1-interface-v3})
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
        (add-xp-to-wallet address 3)
    )

    (defun add-rounds-pvp (id:string wiza:decimal m:module{wiza1-interface-v3})
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

    (defun get-all-subscription-for-pvpweek-full (idweek:string)
        (let (
                (subs (select pvp-subscribers (where "pvpweek" (= idweek))))
            )
            (map (get-info-nft-pvp) subs)
        )
    )

    (defun get-info-nft-pvp (item:object)
        (let
            (
                (stat (read stats (at "idnft" item)))
            )
            (+ item stat)
        )
    )


    ;; UPGRADE ;;

    (defun buy-upgrades (account:string idnft:string stat:string increase:integer m:module{wiza1-interface-v3})
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
                            (
                                (= stat "speed")
                                (update stats idnft {
                                    "speed": (+ current-stat increase)
                                })
                            )
                        "")
                        (emit-event (BUY_UPGRADE idnft stat increase wiza-cost account))
                    )
                )
            )
        )
        (with-capability (PRIVATE)
            (add-xp-to-wallet account (* increase 2))
        )
    )

    (defun buy-upgrades-ap (account:string idnft:string stat:string increase:integer m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
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
                            (array-levels-to (drop -1 (enumerate current-stat (+ current-stat increase))))
                        )
                        (enforce (>= ap ap-cost) "You don't have enough AP")
                        (let (
                                (wiza-cost (round (/ (* (fold (+) 0 (map (calculate-wiza-cost-from stat) array-levels-to)) 12) 100) 2))
                            )
                            (spend-wiza wiza-cost account m)
                        )
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
                            (
                                (= stat "speed")
                                (update stats idnft {
                                    "speed": (+ current-stat increase)
                                })
                            )
                        "")
                        (emit-event (BUY_UPGRADE-WITH-AP idnft stat increase ap-cost account))
                    )
                )
            )
        )
        (with-capability (PRIVATE)
            (add-xp-to-wallet account increase)
        )
    )

    (defun calculate-wiza-cost-from (stat:string from:integer)
        (let (
                (max-value (at "value" (read upgrade-stat-values stat ['value])))
                (base-cost (at "value" (read upgrade-stat-values (+ stat "_base_cost") ['value])))
                (wiza-value (get-wiza-value))
            )
            (if
                (= (- max-value from) max-value)
                (let (
                        (last-part (/ base-cost 100))
                        (diff (- max-value 1))
                    )
                    (round (* (- base-cost (* (* 100 (/ diff max-value)) last-part )) wiza-value) 2)
                )
                (let (
                        (last-part (/ base-cost 100))
                        (diff (- max-value from))
                    )
                    (round (* (- base-cost (* (* 100 (/ diff max-value)) last-part )) wiza-value) 2)
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
                    (speed (at "speed" data))
                )
                (cond
                    (
                        (= stat "hp")
                        (round (+ (+ (+ (+ (+ hp increase) (* def 4.67)) (* atk 4.67)) (* dmg 2.67)) (* speed 2.67)))
                    )
                    (
                        (= stat "defense")
                        (round (+ (+ (+ (+ hp (* (+ def increase) 4.67)) (* atk 4.67)) (* dmg 2.67)) (* speed 2.67)))
                    )
                    (
                        (= stat "attack")
                        (round (+ (+ (+ (+ hp (* def 4.67)) (* (+ atk increase) 4.67)) (* dmg 2.67)) (* speed 2.67)))
                    )
                    (
                        (= stat "damage")
                        (round (+ (+ (+ (+ hp (* def 4.67)) (* atk 4.67)) (* (+ dmg increase) 2.67)) (* speed 2.67)))
                    )
                    (
                        (= stat "speed")
                        (round (+ (+ (+ (+ hp (* def 4.67)) (* atk 4.67)) (* (+ speed increase) 2.67)) (* dmg 2.67)))
                    )
                "")
            )
        )
    )

    (defun buy-potions (account:string idnft:string key:string potion:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")

        (with-capability (OWNER account idnft)
            (let (
                    (current-level (calculate-level idnft))
                    (tournament-open (get-value TOURNAMENT_OPEN))
                    (wiza-cost (* (get-wiza-value) 1.4))
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
        (with-capability (PRIVATE)
            (add-xp-to-wallet account 4)
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
                (data (read stats idnft ['attack 'damage 'defense 'hp 'speed] ))
            )
            (let (
                    (hp (at "hp" data))
                    (def (at "defense" data))
                    (atk (at "attack" data))
                    (dmg (at "damage" data))
                    (speed (at "speed" data))
                )
                (round(+ (+ (+ (+ (+ hp (* def 4.67)) (* atk 4.67)) (* dmg 2.67)) (* speed 2.67)) 0.000001))
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

    (defun spend-wiza (amount:decimal account:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (m::spend-wiza amount account)
    )

    ;;; DOWNGRADE

    (defun retrain (idnft:string owner:string stat:string amount:integer m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (with-capability (OWNER owner idnft)
            (let (
                    (base-stats (get-base-stats idnft))
                    (current-data (get-wizard-fields-for-id (str-to-int idnft)))
                    (ap-gained (ap-gained-per-stat stat amount))
                )
                (enforce (>= (- (at stat current-data) amount) (at stat base-stats)) "Can't retrain this stat so much")
                ;(enforce (> (at "downgrades" current-data) 0) "This wizard can no longer do downgrades")
                (if
                    (> ap-gained (at "downgrades" current-data))
                    (update stats idnft
                        {"ap": (+ (at "ap" current-data) (at "downgrades" current-data)),
                        "downgrades": 0}
                    )
                    (update stats idnft
                        {"ap": (+ (at "ap" current-data) ap-gained),
                        "downgrades": (- (at "downgrades" current-data) ap-gained)}
                    )
                )
                (cond
                    (
                        (= stat "hp")
                        (update stats idnft
                            {"hp": (- (at "hp" current-data) amount)}
                        )
                    )
                    (
                        (= stat "defense")
                        (update stats idnft
                            {"defense": (- (at "defense" current-data) amount)}
                        )
                    )
                    (
                        (= stat "attack")
                        (update stats idnft
                            {"attack": (- (at "attack" current-data) amount)}
                        )
                    )
                    (
                        (= stat "damage")
                        (update stats idnft
                            {"damage": (- (at "damage" current-data) amount)}
                        )
                    )
                    (
                        (= stat "speed")
                        (update stats idnft
                            {"speed": (- (at "speed" current-data) amount)}
                        )
                    )
                "")
                (spend-wiza (+ (* amount 15) 0.0) owner m)
            )
        )
    )

    (defun ap-gained-per-stat (stat:string amount:integer)
        (cond
            (
                (= stat "hp")
                (/ amount 2)
            )
            (
                (= stat "defense")
                (* amount 2)
            )
            (
                (= stat "attack")
                (* amount 2)
            )
            (
                (= stat "damage")
                amount
            )
            (
                (= stat "speed")
                amount
            )
        0)
    )

    (defun get-base-stats (idnft:string)
        (read wizards-base-stats idnft)
    )

    ;;;; swap spell

    (defun swap-spell (idnft:string account:string oldspell:string newspell:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (let (
                (data (get-wizard-fields-for-id (str-to-int idnft)))
                (account-xp (get-wallet-xp account))
                (oldspell-info (get-spell-info oldspell))
                (newspell-info (get-spell-info newspell))
                (wiza-cost (* (get-wiza-value) 14))
            )
            (enforce (= (length (at "spellbook" data)) 4) "The wizard must have 4 spells to do the swap")
            (enforce (>= account-xp 250) "You must have at least 250 xp to do the swap")
            (enforce (= (at "element" data) (at "element" newspell-info)) "You cannot learn this spell")
            (let (
                    (contain-oldspell (check-spellbook-contain-spell (at "spellbook" data) oldspell-info))
                    (contain-newspell (check-spellbook-contain-spell (at "spellbook" data) newspell-info))
                    (newspellbook (create-new-spellbook (at "spellbook" data) oldspell-info newspell-info))
                )
                (enforce (= contain-oldspell true) "This wizard does not have the old spell")
                (enforce (= contain-newspell false) "You already know this spell")
                (with-capability (OWNER account idnft)
                    (update stats idnft
                        {"spellbook": newspellbook }
                    )
                    (spend-wiza wiza-cost account m)
                )
            )
        )
    )

    (defun check-spellbook-contain-spell (spellbook:list spell:object)
        (contains spell spellbook)
    )

    (defun create-new-spellbook (spellbook:list oldspell:object newspell:object)
        (let (
                (newspellbook (+ (filter (where 'name (!= (at "name" oldspell))) spellbook) (make-list 1 newspell)))
            )
            newspellbook
        )
    )

    (defun get-spell-info (spell:string)
        (read spells spell)
    )

    ;;; BURN

    (defun add-to-burning-queue (idnft:string account:string m:module{wiza1-interface-v3} mequip:module{wizequipment-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (= (format "{}" [mequip]) "free.wiz-equipment") "not allowed, security reason")
        (let (
                (data (get-wizard-fields-for-id (str-to-int idnft)))
                (is-staked (check-is-staked idnft m))
                (has-ring (at "equipped" (check-is-equipped idnft mequip)))
                (has-pendant (at "equipped" (check-is-equipped (+ idnft "pendant") mequip)))
            )
            (enforce (= (at "listed" data) false) "You can't burn a listed wizard")
            (enforce (= is-staked false) "You can't burn a staked wizard")
            (enforce (= has-ring false) "You can't burn an equipped wizard")
            (enforce (= has-pendant false) "You can't burn an equipped wizard")
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

    (defun check-is-staked (idnft:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (m::check-nft-is-staked idnft)
    )

    (defun check-is-equipped (idnft:string m:module{wizequipment-interface-v1})
        (enforce (= (format "{}" [m]) "free.wiz-equipment") "not allowed, security reason")
        (m::get-equipped-fields-for-id idnft)
    )

    ;;; CHALLENGES ;;;

    (defun send-challenge (wiz1id:string wiz2id:string amount:decimal coin:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (> amount 0.0) "Amount must be greater then zero")
       (let (
               (wiz1data (get-wizard-fields-for-id (str-to-int wiz1id)))
               (owner2 (at "owner" (read nfts wiz2id ["owner"])))
               (wiz1level (calculate-level wiz1id))
               (wiz2level (calculate-level wiz2id))
               (new-challenge-id (int-to-str 10 (get-count WIZARDS_CHALLENGES_COUNT_KEY)))
               (bank-guard (at "guard" (coin.details WIZ_AUTO_TOURNAMENTS_BANK)))
           )
           (enforce (= (at "listed" wiz1data) false) "You cannot launch the challenge if your wizard is listed")
           (enforce (= (at "confirmBurn" wiz1data) false) "You cannot launch the challenge if your wizard is in Burning queue")
           (with-capability (OWNER (at "owner" wiz1data) wiz1id)
               (if
                   (= coin "kda")
                   [
                       (install-capability (coin.TRANSFER (at "owner" wiz1data) WIZARDS_OFFERS_BANK amount))
                       (coin.transfer (at "owner" wiz1data) WIZARDS_OFFERS_BANK amount)
                       (with-default-read token-table WIZARDS_OFFERS_BANK
                         {"balance": 0.0}
                         {"balance":= oldbalance }
                         (update token-table WIZARDS_OFFERS_BANK {"balance": (+ oldbalance amount)})
                       )
                   ]
                   [
                       (install-capability (m::TRANSFER (at "owner" wiz1data) WIZ_AUTO_TOURNAMENTS_BANK amount))
                       (m::transfer-create (at "owner" wiz1data) WIZ_AUTO_TOURNAMENTS_BANK bank-guard amount)
                   ]
               )

               (insert challenges new-challenge-id {
                   "id": new-challenge-id,
                   "timestamp": (at "block-time" (chain-data)),
                   "expiresat": (add-time (at "block-time" (chain-data)) (days 3)),
                   "amount":amount,
                   "withdrawn": false,
                   "status": "pending",
                   "wiz1id": wiz1id,
                   "wiz1owner":(at "owner" wiz1data),
                   "wiz1level": wiz1level,
                   "wiz2id": wiz2id,
                   "wiz2owner": owner2,
                   "wiz2level": wiz2level,
                   "fightId": "",
                   "blockTime": (at "block-time" (chain-data)),
                   "coin":coin,
                   "winnerId": ""
               })
               (with-capability (PRIVATE)
                 (increase-count WIZARDS_CHALLENGES_COUNT_KEY)
               )
           )
       )
   )

   (defun accept-challenge (challengeid:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (with-read challenges challengeid
            {
                "expiresat":=expiresat,
                "amount":=amount,
                "withdrawn":=iswithdrew,
                "wiz1id":=wiz1id,
                "wiz2id":=wiz2id,
                "wiz1level":=wiz1levelold,
                "wiz2level":=wiz2levelold,
                "coin":=coin
            }
            (enforce (= iswithdrew false) "Cannot accept twice")
            (enforce (>= expiresat (at "block-time" (chain-data))) "Challenge expired.")
            (let (
                    (owner2 (at "owner" (read nfts wiz2id ["owner"])))
                    (wiz1level (calculate-level wiz1id))
                    (wiz2level (calculate-level wiz2id))
                    (bank-guard (at "guard" (coin.details WIZ_AUTO_TOURNAMENTS_BANK)))
                )
                (enforce (= wiz1level wiz1levelold) "the level of Wizard 1 does not match that of the sent challenge")
                (enforce (= wiz2level wiz2levelold) "the level of Wizard 2 does not match that of the sent challenge")
                (with-capability (OWNER owner2 wiz2id)
                    (if
                        (= coin "kda")
                        [
                            (install-capability (coin.TRANSFER owner2 WIZARDS_OFFERS_BANK amount))
                            (coin.transfer owner2 WIZARDS_OFFERS_BANK amount)
                            (with-default-read token-table WIZARDS_OFFERS_BANK
                              {"balance": 0.0}
                              {"balance":= oldbalance }
                              (update token-table WIZARDS_OFFERS_BANK {"balance": (+ oldbalance amount)})
                            )
                        ]
                        [
                            (install-capability (m::TRANSFER owner2 WIZ_AUTO_TOURNAMENTS_BANK amount))
                            (m::transfer-create owner2 WIZ_AUTO_TOURNAMENTS_BANK bank-guard amount)
                        ]
                    )
                    (update challenges challengeid {
                        "status": "accepted",
                        "blockTime": (at "block-time" (chain-data))
                    })
                )
            )
        )
    )

    (defun complete-challenge (challengeid:string winner:string fight:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (with-read challenges challengeid
            {
                "amount":=amount,
                "withdrawn":=iswithdrew,
                "wiz1id":=wiz1id,
                "wiz2id":=wiz2id,
                "coin":=coin
            }
            (enforce (= iswithdrew false) "Challenge already completed")
            (let (
                    (wiz1owner (at "owner" (get-wizard-fields-for-id (str-to-int wiz1id))))
                    (wiz2owner (at "owner" (get-wizard-fields-for-id (str-to-int wiz2id))))
                    (fee (/ (* 4 amount) 100))
                )
                (with-capability (DEV)
                    (if
                        (= winner wiz1id)
                        (send-prize-challenge wiz1owner amount fee coin m)
                        (send-prize-challenge wiz2owner amount fee coin m)
                    )
                    (update challenges challengeid {
                        "withdrawn": true,
                        "fightId": fight,
                        "winnerId": winner
                    })
                )
            )
        )
    )

    (defun send-prize-challenge (account:string amount:decimal fee:decimal coin:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (require-capability (DEV))
        (if
            (= coin "kda")
            [
                (install-capability (coin.TRANSFER WIZARDS_OFFERS_BANK account (- (* amount 2.0) fee)))
                (with-capability (PRIVATE)(coin.transfer WIZARDS_OFFERS_BANK account (- (* amount 2.0) fee)))

                (install-capability (coin.TRANSFER WIZARDS_OFFERS_BANK ADMIN_ADDRESS fee))
                (with-capability (PRIVATE)(coin.transfer WIZARDS_OFFERS_BANK ADMIN_ADDRESS fee))

                (with-default-read token-table WIZARDS_OFFERS_BANK
                  {"balance": 0.0}
                  {"balance":= oldbalance }
                  (update token-table WIZARDS_OFFERS_BANK {"balance": (- oldbalance (* amount 2))})
                )
            ]
            [
                (let (
                        (winner-guard (at "guard" (coin.details account)))
                        (admin-guard (at "guard" (coin.details ADMIN_ADDRESS)))
                    )
                    (install-capability (m::TRANSFER WIZ_AUTO_TOURNAMENTS_BANK account (- (* amount 2.0) fee)))
                    (m::transfer-create WIZ_AUTO_TOURNAMENTS_BANK account winner-guard (- (* amount 2.0) fee))

                    (install-capability (m::TRANSFER WIZ_AUTO_TOURNAMENTS_BANK ADMIN_ADDRESS fee))
                    (m::transfer-create WIZ_AUTO_TOURNAMENTS_BANK ADMIN_ADDRESS admin-guard fee)
                )
            ]
        )
    )

    (defun cancel-challenge (challengeid:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (with-read challenges challengeid
            {
                "wiz1owner":=wiz1owner,
                "expiresat":=expiresat,
                "amount":=amount,
                "withdrawn":=iswithdrew,
                "coin":=coin
            }
            (with-capability (ACCOUNT_GUARD wiz1owner)
                (enforce (= iswithdrew false) "Cannot withdraw twice")
                (enforce (<= expiresat (at "block-time" (chain-data))) "Cannot cancel challenge yet.")
            )
            (with-capability (PRIVATE)
                (if
                    (= coin "kda")
                    [
                        (install-capability (coin.TRANSFER WIZARDS_OFFERS_BANK wiz1owner amount))
                        (coin.transfer WIZARDS_OFFERS_BANK wiz1owner amount)
                        (with-default-read token-table WIZARDS_OFFERS_BANK
                          {"balance": 0.0}
                          {"balance":= oldbalance }
                          (update token-table WIZARDS_OFFERS_BANK {"balance": (- oldbalance amount)})
                        )
                    ]
                    [
                        (let (
                                (account-guard (at "guard" (coin.details wiz1owner)))
                            )
                            (install-capability (m::TRANSFER WIZ_AUTO_TOURNAMENTS_BANK wiz1owner amount))
                            (m::transfer-create WIZ_AUTO_TOURNAMENTS_BANK wiz1owner account-guard amount)
                        )
                    ]
                )

                (update challenges challengeid
                    { "withdrawn": true,
                    "status": "canceled"}
                )
            )
        )
    )

    (defun get-sent-challenges (account:string)
        (select challenges
            (and?
                (where "wiz1owner" (= account))
                (where "status" (!= "canceled"))
            ))
    )

    (defun get-received-challenges (account:string)
        (select challenges
            (and?
                (where "wiz2owner" (= account))
                (where "status" (!= "canceled"))
            ))
    )

    (defun get-accepted-challenges ()
        (select challenges
            (and?
                (where "withdrawn" (= false))
                (where "status" (= "accepted"))
            ))
    )

    ; IMPROVE SPELL

    (defun improve-spell (idnft:string address:string stat:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (contains stat ["attack" "damage"]) "invalid stat")
        (with-capability (OWNER address idnft)
            (let* (
                    (data (get-wizard-fields-for-id (str-to-int idnft)))
                    (spellName (at "name" (at "spellSelected" data)))
                    (key (format "{}_{}" [idnft spellName]))
                    (wiza-cost (get-wiza-cost-for-improve-spell key))
                )
                (check-upgrades-spell-limit key)
                (with-default-read stats idnft
                    {"ap": 0}
                    {"ap":=ap}
                    (enforce (> ap 0) "You have no AP available")
                    (enforce (>= ap 10) "You don't have enough AP")
                    (update stats idnft {
                        "ap": (- ap 10)
                    })
                )
                (spend-wiza wiza-cost address m)
                (with-default-read upgrade-spells key
                    {"attack":0, "damage":0}
                    {"attack":=attack, "damage":=damage}
                    (if
                        (= stat "attack")
                        (write upgrade-spells key {"attack": (+ attack 1), "damage": damage})
                        (write upgrade-spells key {"damage": (+ damage 1), "attack": attack})
                    )
                )
                (emit-event (UPGRADE_SPELL idnft spellName))
            )
        )
    )

    (defun get-upgrades-spell (idnft:string spell:string)
        (let (
                (key (format "{}_{}" [idnft spell]))
            )
            (with-default-read upgrade-spells key
                {"attack":0, "damage":0}
                {"attack":=attack, "damage":=damage}
                {"attack": attack, "damage":damage}
            )
        )
    )

    (defun get-wiza-cost-for-improve-spell (key:string)
        (with-default-read upgrade-spells key
            {"attack":0, "damage":0}
            {"attack":=attack, "damage":=damage}
            (let* (
                    (base-cost (round (* (get-wiza-value) 2.2) 2))
                    (mod (* (+ attack damage) 60))
                    (mod2 (round (/ (* base-cost mod) 100) 2))
                    (wiza-cost (+ base-cost mod2))
                )
                wiza-cost
            )
        )
    )

    (defun check-upgrades-spell-limit (key:string)
        (with-default-read upgrade-spells key
            {"attack":0, "damage":0}
            {"attack":=attack, "damage":=damage}
            (let (
                    (total-upgrades (+ attack damage))
                )
                (enforce (< total-upgrades 12) "You have reached the limit for this spell")
            )
        )
    )

    (defun remove-all-upgrades-spell (idnft:string address:string)
        (with-capability (OWNER address idnft)
            (let* (
                    (data (get-wizard-fields-for-id (str-to-int idnft)))
                    (spellName (at "name" (at "spellSelected" data)))
                    (key (format "{}_{}" [idnft spellName]))
                )
                (with-default-read upgrade-spells key
                    {"attack":0, "damage":0}
                    {"attack":=attack, "damage":=damage}
                    (let (
                            (ap-to-reimburse (* (+ attack damage) 5))
                        )
                        (update upgrade-spells key
                            {"attack":0, "damage":0}
                        )
                        (with-default-read stats idnft
                            {"ap":0}
                            {"ap":=ap}
                            (update stats idnft
                                {"id": idnft,
                                "ap": (+ ap ap-to-reimburse)}
                            )
                        )
                    )
                )
            )
        )
    )

    ;GENERIC FUN ;;

    (defun update-nickname (id:string address:string nickname:string m:module{wiza1-interface-v3})
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (let (
                (wiza-cost (round (* (get-wiza-value) 0.6) 2))
            )
            (with-capability (OWNER address id)
                (update nfts id {
                  "nickname": nickname
                })
                (spend-wiza wiza-cost address m)
            )
        )
    )

    (defun transfer-wizard (id:string sender:string receiver:string m:module{wiza1-interface-v3} mequip:module{wizequipment-interface-v1})
        @doc "Transfer nft to an account"
        (enforce-account-exists receiver)
        (enforce (= (format "{}" [m]) "free.wiza") "not allowed, security reason")
        (enforce (= (format "{}" [mequip]) "free.wiz-equipment") "not allowed, security reason")
        (with-capability (OWNER sender id)
            (let (
                    (data (get-wizard-fields-for-id (str-to-int id)))
                    (is-staked (check-is-staked id m))
                    (has-ring (at "equipped" (check-is-equipped id mequip)))
                    (has-pendant (at "equipped" (check-is-equipped (+ id "pendant") mequip)))
                )
                (enforce (= (at "listed" data) false) "A listed wizard cannot be transferred")
                (enforce (= is-staked false) "You can't transfer a staked wizard")
                (enforce (= (at "confirmBurn" data) false) "You can't transfer a wizard in burning queue")
                (enforce (= has-ring false) "You can't transfer an equipped wizard")
                (enforce (= has-pendant false) "You can't transfer an equipped wizard")
            )
            (update nfts id {"owner": receiver})
            (emit-event (WIZ_TRANSFER id sender receiver))
        )
    )

    (defun add-xp-to-wallet (account:string newxp:integer)
       (require-capability (PRIVATE))
       (with-default-read wallet-xp account
           {"xp": 0}
           {"xp":=xp}
           (write wallet-xp account {
               "xp": (+ xp newxp)
           })
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

    (defun set-value-dev(key:string value:string)
        @doc "Sets the value for a key to store in a table"
        (with-capability (DEV)
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
        (let (
                 (wiza-value (free.wiz-dexinfo.get-wiza-value))
             )
             (if
                 (> wiza-value 50.0)
                 50.0001
                 wiza-value
             )
         )
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

    (defun wizard-owned-by (owner:string)
        @doc "all ids wizard from owner"
        (select nfts ['id] (where "owner" (= owner)))
    )

    (defun id-for-new-wizard ()
        @doc "Returns an id for a new wizard to be minted"
        (int-to-str 10 (get-count MINTED_POST_COUNT_KEY))
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
                    (data (read creation id ['name 'imageHash]))
                )
                data
            )
        )
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
                (confirmBurn (check-nft-in-burning-queue (int-to-str 10 id)))
                (max-reveal (str-to-int (get-value ID_REVEAL)))
                (level (calculate-level (int-to-str 10 id)))
            )
            ; per fare il reveal vediamo quale id ha la richeista, se è più di 1023 allora è un cleric
            (if
                (< id max-reveal)
                (let* (
                        (info (read nfts (int-to-str 10 id)))
                        (info-stat (read stats (int-to-str 10 id) ['attack 'damage 'weakness 'defense 'element 'hp 'medals 'resistance 'spellSelected 'spellbook 'ap 'speed 'downgrades] ))
                        (spellSelectedName (at "name" (at "spellSelected" info-stat)))
                        (upgrades-spell (get-upgrades-spell (int-to-str 10 id) spellSelectedName))
                    )
                    (+ (+ (+ (+ (+ info info-market) info-stat) {"confirmBurn":confirmBurn}) {"level":level}) {"upgrades-spell":upgrades-spell})
                )
                (let (
                        (info (read nfts (int-to-str 10 id) ['created 'owner 'name 'id 'imageHash]))
                    )
                    (+ (+ info info-market) {"confirmBurn":confirmBurn})
                )
            )
        )
    )

    (defun get-fights-wizards (ids:list)
        (map (get-fights-wizard) ids)
    )

    (defun get-fights-wizard (id:string)
        (select fights-db (where "id" (= id)))
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

    (defun check-your-account (account:string)
        (with-capability (ACCOUNT_GUARD account)
            true
        )
    )

    (defun get-wallet-xp (account:string)
        (at "xp" (read wallet-xp account ['xp]))
    )

    (defun get-wallets-xp-1 ()
        (map
            (get-wallets-xp-2)
            (keys wallet-xp)
        )
    )

    (defun get-wallets-xp-2 (account:string)
        {"xp": (at "xp" (read wallet-xp account ['xp])),
        "account": account}
    )
)


