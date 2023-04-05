(module kadcars-nft-policy GOVERNANCE
  @doc "Policy for Kadcar NFT issuance with royalty and quoted sale in specified fungible."
  (defconst ADMIN_ADDRESS "k:f157854c15e9bb8fb55aafdecc1ce27a3d60973cbe6870045f4415dc06be06f5"
    @doc "admin address which also recieves mint payouts")
  (defconst ROYALTY_ADDRESS "k:f157854c15e9bb8fb55aafdecc1ce27a3d60973cbe6870045f4415dc06be06f5"
    @doc "address for royalty payouts, which ideally will feed into splitter with business rules")
  (defconst ROYALTY_RATE_CONST 0.03 @doc "base royalty rate")
  (defconst REDUCED_PRICE_ROYALTY_RATE_CONST 0.05 @doc "adjusted royalty rate for selling below base mint price")
  (defconst CHAMPION_MINT_TIME "2023-02-11T00:00:00Z")
  (defconst PUBLIC_MINT_TIME "2023-02-11T06:00:00Z")
  (defconst FREE_MINT_ROLE "free mint granted")
  (defconst CHAMPION_MINT_ROLE "Champion mint granted")
  (defconst WL_MINT_ROLE "whitelist granted")
  (defconst PUBLIC_ROLE "account not whitelisted.")
  (defconst KILL_SWITCH_KEY "KILL_SWITCH")
;;DEPRECATE
  (defconst MINT_PRICE 10.0  @doc "base mint price")
  (defconst PUBLIC_SALE false @doc "flag to indicate public sale has begun")
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.kc-policy-admin" )))
  (defcap BUY (id:string receiver:string)
   (compose-capability (UPDATE-OWNER id receiver)))
  (defcap MINT(token-id:string new-owner:string)
    (compose-capability (UPDATE_WL))
    (compose-capability (UPDATE-OWNER token-id new-owner))
  )
  (defcap UPDATE_WL ()
      @doc "private cap for update-whitelists"
      true)

  (defcap UPDATE-OWNER (token-id:string new-owner:string)
    true)

  (implements free.universal-token-policy-v2)
  (use free.universal-token-policy-v2 [token-info])

  (defschema account-wl-info-schema
    account:string
    free-mints-remaining:integer
    free-mints-granted:integer
    whitelists-remaining:integer
    minted-total:integer
    is-champion:bool
  )
  (defschema collections-schema
    collection-id:string
    collection-guard:guard
    created-token-list:[string]
    minted-token-list:[string]
    max-unique-supply:integer
    total-created:integer
  )

  ;; 1. when creating add to created token list
  ;; 2.1 when minting, remove from created-list
  ;; 2.2 when minting, add to minted-token list
  ;;
  (defschema token-policy-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    royalty-rate:decimal
    collection-id:string
    owner:string
  )

  (defschema bool-consts-schema
    val:bool
  )

  (defschema effecient-token-policy-schema
    @doc "encapsulate token-policy for all but id"
    token-id:string
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    royalty-rate:decimal
    collection-id:string
    owner:string
  )

  (deftable tokens:{effecient-token-policy-schema})
  (deftable collections:{collections-schema})
  (deftable account-wl-info:{account-wl-info-schema})
  (deftable bool-consts:{bool-consts-schema})

  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

    (defconst COLLECTION_SUPPLY "collection-unique-supply"
      @doc "Max Unique Tokens in a collection")

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (deftable quotes:{quote-schema})

  (defun get-policy (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun get-collection:object{collections-schema} (collection-id:string)
    (read collections collection-id)
  )

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      royalty-payout:decimal
      creator:string
      spec:object{quote-spec}
      timeout:integer
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (free.universal-ledger.ledger-guard))
   )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (with-capability (MINT (at 'id token) account)
    (enforce-ledger)
      (let* (
              (token-supply (at "supply" token))
              (token-policy-info (get-policy token))
              (collection-id (at 'collection-id token-policy-info))
              (collection (get-collection collection-id))
              (minted-tokenList (at 'minted-token-list collection))
              (created-tokenList (at 'created-token-list collection))
              (minted-tokens-length (length minted-tokenList))
              (account-wl-info:object{account-wl-info-schema} (get-account-whitelist-info account))
              (whitelist-indicator:string (get-whitelist-indicator account account-wl-info))
              (mint-price-multiplier (get-mint-multiplier whitelist-indicator))
              (base-mint-price (get-mint-price-by-threshold minted-tokens-length))
              (mint-price-precise (* base-mint-price mint-price-multiplier))
              (mint-price (round mint-price-precise 4))
              (token-id (at 'id token))

              (wl-live:bool (enforce-mint-live whitelist-indicator account-wl-info ))
          )
            (enforce (= true wl-live) "mint is not live")
            (enforce (= token-supply 0.0) "Supply exceeded")
            (enforce (= 1.0 amount) "Amount of 1 only allowed for Non fungibles")

            ;;enforcing guard here is gas optimization...
            (if (!= account ADMIN_ADDRESS)
                (if (> mint-price 0.0) (coin.transfer account ADMIN_ADDRESS mint-price) (enforce-guard (get-user-guard account guard)))
                    (enforce-guard (keyset-ref-guard "free.kc-policy-admin"))
            )

            (update collections collection-id
              {'created-token-list: (filter (!= token-id) created-tokenList),
              'minted-token-list: (
                + [token-id] minted-tokenList)})
            (update-owner token-id account)
      )
    )
  )

  (defun get-user-guard (account:string supplied-guard:guard)
    (let*
      (
        (guard (at 'guard (coin.details account)))
        )
        (enforce (= guard supplied-guard) "Supplied Guard does not match")
        guard
      )
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (with-capability (GOVERNANCE)
    (enforce-ledger)
    (let* ( (spec:object{token-policy-schema} (read-msg TOKEN_SPEC))
            (fungible:module{fungible-v2} (at 'fungible spec))
            (creator:string (at 'creator spec))
            (creator-guard:guard (at 'creator-guard spec))
            (royalty-rate:decimal (at 'royalty-rate spec))
            (collection-identifier:string (at 'collection-id spec))
            (collection (get-collection collection-identifier))
            (max-supply (at 'max-unique-supply collection))
            (collection-guard:guard (at 'collection-guard collection))
            (creator-details:object (fungible::details creator ))
            (tokenList (at 'created-token-list collection))
            (total-created (at 'total-created collection))

            (token-id (at 'id token))
            )
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (=
        collection-guard creator-guard)
        "Creator guard does not match")
      (enforce (<= ( + 1 total-created) max-supply) "exceeded supply")

      (insert tokens token-id
        {'token-id: token-id
        ,'fungible: fungible
        , 'creator: creator
        , 'owner: creator
        , 'creator-guard: creator-guard
        , 'collection-id:collection-identifier
        , 'royalty-rate: royalty-rate }
        )
      (update collections collection-identifier
          {'created-token-list: (+ [token-id] tokenList),
          'total-created:(+ 1 total-created)
        }
      )
    )
      true
    )
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
      timeout:integer
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce false "Sale not active wait for mint please.")
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
       ,'royalty-rate:= royalty-rate:decimal
       ,'creator:= creator:string
      }
      (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
              (price:decimal (at 'price spec))
              (recipient:string (at 'recipient spec))
              (recipient-guard:guard (at 'recipient-guard spec))
              (recipient-details:object (fungible::details recipient))
              (sale-price:decimal (* amount price))
              (adjusted-royalty-rate:decimal (get-royalty-rate sale-price royalty-rate ))
              (royalty-payout:decimal
                 (floor (* sale-price adjusted-royalty-rate) (fungible::precision))) )
        (fungible::enforce-unit sale-price)
        (enforce (< 0.0 price) "Offer price must be positive")
        (enforce (=
          (at 'guard recipient-details) recipient-guard)
          "Recipient guard does not match")
        (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
        (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout creator spec timeout)))
        true
      )
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (with-capability (BUY (at 'id token) buyer)
      (enforce-sale-pact sale-id)
      (bind (get-policy token)
        { 'fungible := fungible:module{fungible-v2}
        , 'creator:= creator:string
        , 'royalty-rate:= royalty-rate:decimal
        }
        (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
          (enforce (= qtoken (at 'id token)) "incorrect sale token")
          (bind spec
            { 'price := price:decimal
            , 'recipient := recipient:string
            }
            (let* ((sale-price:decimal (* amount price))
                   (adjusted-royalty-rate (get-royalty-rate sale-price royalty-rate))
                   (royalty-payout:decimal
                      (floor (* sale-price adjusted-royalty-rate) (fungible::precision)))
                   (payout:decimal (- sale-price royalty-payout)) )
              (if
                (> royalty-payout 0.0)
                (fungible::transfer buyer ROYALTY_ADDRESS royalty-payout)
                "No royalty")
              (fungible::transfer buyer recipient payout)))
              true
              (update-owner qtoken buyer)
        )))
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce (= 1.0 amount) "Invalid amount for single supply NFT Collection")
    (update-owner (at 'id token) receiver)
    (enforce false "Transfer prohibited")
  )

  (defun enforce-mod:bool
    (token:object{token-info})
    (enforce false "Modification prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun update-owner (token-id:string new-owner:string)
      (require-capability (UPDATE-OWNER token-id new-owner))
        (update tokens token-id
          {'owner: new-owner}
        )
  )

  (defun create-collection ()
    (with-capability (GOVERNANCE)
    (let* (
            (creator-guard:guard (read-msg "creator-guard"))
            (collection-identifier:string (read-msg 'collection-id))
            (collection-max-unique-supply:integer (read-integer COLLECTION_SUPPLY))
            )
            (enforce-guard creator-guard)
            ;;TODO : Move collection-id to pull from make of manifest
          (insert collections collection-identifier
              {'collection-id:collection-identifier
              ,'collection-guard:creator-guard
              ,'created-token-list:[]
              ,'minted-token-list:[]
              ,'total-created:0
              ,'max-unique-supply:collection-max-unique-supply})
          )
          true
    )
  )

    ;;;;;;;;;;;;;; WL Functionality ;;;;;;;;;;;;;;

    (defun bulk-add-whitelist ()

      (let* (
          (whitelist-info:[object:{account-wl-info-schema}] (read-msg "wls"))
        )
        (map (add-whitelist-wrapper) whitelist-info))
    )

    (defun add-whitelist-wrapper(wl:object{account-wl-info-schema})
      (let*
          (
            (account (at "account" wl))
            (free-mints-granted (at "free-mints-granted" wl))
            (whitelists-remaining (at "whitelists-remaining" wl))
            (minted-total (at "minted-total" wl))
            (is-champion (at "is-champion" wl))
            )
            (add-whitelist account free-mints-granted whitelists-remaining is-champion minted-total)
        )
    )

    (defun add-whitelist:bool (account:string
      free-mints-granted:integer
      whitelists-remaining:integer
      is-champ:bool
      minted-total:integer)
       (with-capability (GOVERNANCE)
         (insert account-wl-info account {
           "account": account,
           "free-mints-remaining": free-mints-granted,
           "free-mints-granted": free-mints-granted,
           "whitelists-remaining": whitelists-remaining,
           "minted-total": minted-total,
           "is-champion": is-champ
         })
         true
       )
    )

    (defun add-to-whitelists-table (account:string additional-whitelists:integer additional-freemints:integer)
      (with-capability (GOVERNANCE)
        (bind (get-account-whitelist-info account)
          {
            'free-mints-remaining:=free-mints-remaining,
            'free-mints-granted:=free-mints-granted,
            'whitelists-remaining:=whitelists
          }
          (update account-wl-info account {
            "whitelists-remaining": (+ whitelists additional-whitelists),
            "free-mints-remaining": (+ free-mints-remaining additional-freemints),
            "free-mints-granted": (+ free-mints-granted additional-freemints)
          })
          "new WL Spots added by Admin"
        )
      )
    )

    (defun update-as-champion (account:string is-champ:bool)
      (with-capability (GOVERNANCE)
        
          (update account-wl-info account {
            "is-champion": is-champ
          })
          "Champ status modified by Admin"
        
      )
    )

    (defun get-whitelist-indicator:string (account:string account-wl-info:object{account-wl-info-schema})
         (require-capability (UPDATE_WL))
           (let* (
             (free-mints-granted (at 'free-mints-granted account-wl-info))
             (free-mints-remaining (at 'free-mints-remaining account-wl-info))
             (minted-total (at 'minted-total account-wl-info))
             (whitelists-remaining (at 'whitelists-remaining account-wl-info))
             (is-champion (at 'is-champion account-wl-info))
             )
               (cond
                 ;;Free mints
                 ((> free-mints-remaining 0) (update-free-mints-table account (- free-mints-remaining 1) (+ 1 minted-total)))
                 ;; Champions First Non Free Mint
                 ((and (= free-mints-granted minted-total) is-champion) (update-champion-minted-total-table account (+ 1 minted-total)))
                 ;; WhiteListMint
                 ((> whitelists-remaining 0) (update-whitelists-table account (- whitelists-remaining 1) (+ 1 minted-total)))
                 ;; PublicMint
                 ((= true true) (update-minted-total-table account (+ 1 minted-total) free-mints-granted is-champion))
                    PUBLIC_ROLE
                )
            )
    )

    (defun get-account-whitelist-info:{account-wl-info-schema} (account:string)
      (with-default-read account-wl-info account
        {
          "free-mints-remaining": 0,
          "free-mints-granted": 0,
          "whitelists-remaining": 0,
          'minted-total: 0,
          'is-champion: false,
          'account: account
        }
        {
         "free-mints-granted":= free-mints-granted,
         "free-mints-remaining":= free-mints-remaining,
         "whitelists-remaining":= whitelists-remaining,
         'minted-total:= minted-total,
         'is-champion:= is-champion,
         'account:= account
        }
        {
         "free-mints-granted": free-mints-granted,
         "free-mints-remaining": free-mints-remaining,
         "whitelists-remaining": whitelists-remaining,
         'minted-total: minted-total,
         'is-champion: is-champion,
         'account: account
        }
      )
    )

    (defun update-free-mints-table (account:string new-free-mints:integer minted-total:integer)
      (require-capability (UPDATE_WL))
        (update account-wl-info account {
          "free-mints-remaining": new-free-mints
          ,'minted-total: minted-total
        })
        FREE_MINT_ROLE
    )

    (defun update-champion-minted-total-table (account:string minted-total:integer)
      (require-capability (UPDATE_WL))
        (update account-wl-info account {
          'minted-total: minted-total
        })
        CHAMPION_MINT_ROLE
    )

    (defun update-whitelists-table (account:string new-whitelists:integer minted-total:integer)
      (require-capability (UPDATE_WL))
        (update account-wl-info account {
          "whitelists-remaining": new-whitelists
          ,'minted-total: minted-total
        })
        WL_MINT_ROLE
    )

    (defun update-minted-total-table (account:string
      new-minted-total:integer
      freemintsgranted:integer
      ischamp:bool)
      (require-capability (UPDATE_WL))
      (write account-wl-info account
        {     'minted-total: minted-total,
              "free-mints-remaining": 0,
              "free-mints-granted": freemintsgranted,
              "whitelists-remaining": 0,
              'minted-total: new-minted-total,
              'is-champion: ischamp,
              'account: account

        })
        PUBLIC_ROLE
    )


    (defun get-mint-price-by-threshold:decimal (minted-tokens-length:integer)
      (let*
        (
          (base-price (get-base-price-by-supply (+ 1 minted-tokens-length)))
          )
          base-price
      )
    )

    (defun get-mint-multiplier:decimal (whitelist-info:string)

      (cond
        ((= whitelist-info FREE_MINT_ROLE) 0.0)
        ((= whitelist-info CHAMPION_MINT_ROLE) 0.70)
        ((= whitelist-info WL_MINT_ROLE) 0.85)
        1.0
      )
    )

    (defun get-base-price-by-supply:decimal (supply:integer)
        (cond
          ((<= supply 1000) 25.0)
          ((<= supply 1500) 29.0)
          ((<= supply 2000) 33.0)
          ((<= supply 2500) 36.0)
          ((<= supply 3000) 39.0)
          ((<= supply 3500) 44.0)
          ((<= supply 4000) 49.0)
          ((<= supply 4500) 55.0)
          ((<= supply 5000) 59.0)
          ((< supply 5338) 65.0)
          65.0
          )
    )
    ;;;;;;;;;;;;;; ROYALTY ;;;;;;;;;;;;;;

    (defun get-royalty-rate (sale-price:decimal configured-royalty-rate:decimal)
        (if (< sale-price MINT_PRICE) REDUCED_PRICE_ROYALTY_RATE_CONST configured-royalty-rate)
    )
    ;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;
    (defun get-non-minted-tokens-for-collection (collection-id:string)

       (at "created-token-list" (read collections collection-id ["created-token-list"]))
    )
    (defun get-minted-tokens-for-collection (collection-id:string)
       (at "minted-token-list" (read collections collection-id ["minted-token-list"]))
    )
    (defun get-cars-in-collection-by-owner (collection-id:string owner:string)
      @doc "All cars under collections and owner"
      (select tokens ["token-id"](and? (where 'owner (= owner)) (where 'collection-id (= collection-id))))
    )

   (defun get-all-k2s ()
      @doc "get all K:2s"
      (select tokens ["token-id"](and (where 'collection-id (= "k:2"))))
    )

    (defun enforce-mint-live(whitelist-info:string account-whitelist-info:object{account-wl-info-schema})
      @doc "Enforces mint is live"
      (let* (
          (kill_switch:bool (get-kill))
          (is-champ:bool (at 'is-champion account-whitelist-info))
        )
        (cond
          ((= kill_switch true) false)
          ((= true is-champ) (enforce-champ-mint-live))
          ((= FREE_MINT_ROLE whitelist-info) (enforce-champ-mint-live))
          ((= true (enforce-public-mint-live)) true)
          false)
      )
      ;;(enforce (<= (at "block-time" (chain-data)) (time MINT_END_TIME)) "Mint has OR is Paused.")
    )
    (defun enforce-champ-mint-live:bool ()
      @doc "Enforces champ/free mint is live"
      (enforce (>= (at "block-time" (chain-data)) (time CHAMPION_MINT_TIME)) "Mint is NOT live.")
      true
    )
    (defun enforce-public-mint-live:bool ()
      @doc "Enforces mint is live"
      (enforce (>= (at "block-time" (chain-data)) (time PUBLIC_MINT_TIME)) "Mint is NOT Live for you. chill pls...")
      true
    )

    (defun add-bool-consts:bool (key:string val:bool)
      @doc "Adds entry to boolean constants"
      (with-capability (GOVERNANCE)
        (insert bool-consts key
          {'val: val}
        )
      )
    )
    (defun get-kill:bool()
      (let* (
          (kill:bool (get-kill-switch))
        )
        kill
        )
    )
    (defun get-kill-switch:bool ()
      (with-default-read bool-consts "KILL_SWITCH"
        { 'val : true }
        { 'val := s }
        s)
    )

    (defun testing-filter(token-id)
        (with-capability (GOVERNANCE)
            (let* (
                    (collection (get-collection "k:2"))
                    (tokenList (at 'created-token-list collection))
                    )
                
                (filter (!= "Kadcars#K:2:2181") tokenList)
            )

        )
    )

    (defun testing-filters()
        (with-capability (GOVERNANCE)
            (let* (
                    (collection (get-collection "k:2"))
                    (tokenList (at 'created-token-list collection))
                    (minted-list ["a" "c" "b" "f"])
                    )
                
                (filter (in-list minted-list) tokenList)
            )

        )
    )
    
    ;;;;;;;;;; BELOW ARE GAS TESTING FUNCTIONS PURELY FOR TEST AND NOT TO BE USED IN PROD! DONT YOU DARE
    (defun in-list (in-list:list element:string)
        (not (contains element in-list))
    )

    (defun testing-add(token-id)
        (with-capability (GOVERNANCE)
            (let* (
                    (collection (get-collection "k:2"))
                    (tokenList (at 'created-token-list collection))
                    )

                 (+ ["kcccc"] tokenList)
            )

        )
    )

    (defun enforce-mint-bulk:bool
    ( token-list:[object{token-info}]
      account:string
      guard:guard
    )
    (with-capability (GOVERNANCE)

    (enforce-ledger)
      (let* (
              (collection-id "k:2")
              (collection (get-collection collection-id))
              (minted-tokenList (at 'minted-token-list collection))
              (created-tokenList (at 'created-token-list collection))
              (token-ids (map (get-token-id) token-list))
          )
            (map (mint-bulk-single account guard 1.0) token-list)


            (update collections collection-id
              {'created-token-list: (filter (in-list token-ids) created-tokenList),
              'minted-token-list: (
                + token-ids minted-tokenList)})
      )
    )
  )
    (defun get-token-id (token:object{token-info})
        (at 'id token)
    )

    (defun mint-bulk-single:bool
    (
      account:string
      guard:guard
      amount:decimal
      token:object{token-info}
    )
    (with-capability (GOVERNANCE)

    (with-capability (MINT (at 'id token) account)
      (let* (
              (token-supply (at "supply" token))
              (token-policy-info (get-policy token))
              (collection-id (at 'collection-id token-policy-info))
              (collection (get-collection collection-id))
              (minted-tokenList (at 'minted-token-list collection))
              (minted-tokens-length (length minted-tokenList))
              (account-wl-info:object{account-wl-info-schema} (get-account-whitelist-info account))
              (whitelist-indicator:string (get-whitelist-indicator account account-wl-info))
              (mint-price-multiplier (get-mint-multiplier whitelist-indicator))
              (base-mint-price (get-mint-price-by-threshold minted-tokens-length))
              (mint-price-precise (* base-mint-price mint-price-multiplier))
              (mint-price (round mint-price-precise 4))
              (token-id (at 'id token))

              (wl-live:bool (enforce-mint-live whitelist-indicator account-wl-info ))
          )
            (enforce (= true wl-live) "mint is not live")
            (enforce (= token-supply 0.0) "Supply exceeded")
            (enforce (= 1.0 amount) "Amount of 1 only allowed for Non fungibles")

            ;;enforcing guard here is gas optimization...
            (if (!= account ADMIN_ADDRESS)
                (if (> mint-price 0.0) (coin.transfer account ADMIN_ADDRESS mint-price) (enforce-guard (get-user-guard account guard)))
                    (enforce-guard (keyset-ref-guard "free.kc-policy-admin"))
            )
            (update-owner token-id account)
      )
    )
  ))
)
