(module kmp-policy GOVERNANCE

  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "babena-admin")))

  (implements free.babena-token-policy-v1)

  (use free.babena-token-policy-v1 [token-info])
  (use util.guards)

  (defcap MINT (account:string)
    (compose-capability (PRIVATE))
  )

  (defcap PRIVATE ()
    true
  )

  (defcap WHITELIST ()
    (compose-capability (GOVERNANCE))
    (compose-capability (PRIVATE))
  )

  (defschema collection-schema
    royalty-receiver:string ;account which receives the royalty
    royalty-rate:decimal
    total-supply:integer ;total supply of tokens that will ever exist
    provenance-hash:string ;sha256 of combined string
    tokens-list:[string] ;list of sha256 of the images that will ever exist
    creator:string
    max-per-user:integer ;maximum NFT a user can mint
    max-per-wh:integer ;maximum NFT a whitelisted user can mint
    max-per-txn:integer
    price-per-nft:decimal
    whitelist-price:decimal
    creator-guard:guard
    public-mint-time:time
    whitelist-mint-time:time
    mint-end-time:time
    name:string
    symbol:string
    fungible:module{fungible-v2}
  )

   (defschema mint-schema
     tokens-list:[integer]
     current-length:integer
     status:string
   )

  (defschema whitelist-schema
    account:string
    guard:guard
    claimed:integer
    type:string
  )

  (defschema wl-limits-schema
    limit:integer
  )

  (defschema policy-schema
    id:string
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    royalty-rate:decimal
    owner:string
  )

  (defschema account-schema
    account:string
    minted:integer
  )

  (defschema traits-schema
    trait-type:string
    value:string
  )

  (defschema add-whitelist-params
    account:string
    type:string
  )

  (defschema token-metadata
    name:string
    description:string
    image:string
    image-hash:string
    attributes:[object{traits-schema}]
  )

  (deftable policies:{policy-schema})
  (deftable collection-info:{collection-schema})
  (deftable mint-status:{mint-schema})
  (deftable account-details:{account-schema})
  (deftable whitelists:{whitelist-schema})
  (deftable wl-limits:{wl-limits-schema})

  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defconst MINT_STATUS:string "mint-status")
  (defconst COLLECTION_INFO:string "collection-info")
  (defconst MINT_PAUSED:string "mint-paused")
  (defconst MINT_STARTED:string "mint-started")
  (defconst MINT_COMPLETED:string "mint-completed")
  (defconst WL-TYPES:[string] ["ROYAL" "ROYAL-BABENA" "EPIC" "EPIC-BABENA"])

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (deftable quotes:{quote-schema})
; (define-keyset "free.kmp-admin" (read-keyset 'kmp-admin))

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      royalty-payout:decimal
      creator:string
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (babena-ledger.ledger-guard))
  )

  (defun enforce-whitelist:bool (account:string guard:guard)
    (with-read whitelists account {
     'guard:= g,
     'claimed:= claimed,
     'type:= wl-type
    }
      (enforce (= g guard) "Guards doesn't match.")
      (let ((limit:integer (get-wl-limit account)))
        (enforce (< claimed limit)
          (format "You can Mint only {} tokens during whitelist" [limit]))
      )
    )
  )

  (defun enforce-bulk-mint:bool (account:string count:integer)
    (enforce-ledger)
    (let* ( (minted:integer (get-account-minted account))
            (mint-count:integer (+ count minted))
            (whitelist-enabled:bool (check-whitelist))
            (collection-info:object{collection-schema} (get-details))
            (max-per-user:integer (at 'max-per-user collection-info))
            (max-per-txn:integer (at 'max-per-txn collection-info))
            (total-minted:integer (get-total-minted))
            (total-supply:integer (get-total-supply))
          )

        (with-read mint-status MINT_STATUS {
          'status:= status
          }
          (enforce (= status MINT_STARTED) "MINT is paused or completed, can't mint now")
         )
         (enforce (<= (+ total-minted count) total-supply)
            (format "Total supply of {} reached" [total-supply]))
        (cond
           (whitelist-enabled
               [(let* ( (wl-info:object{whitelist-schema} (get-whitelist-info account))
                        (claimed:integer (at 'claimed wl-info))
                        (wl-type:string (at 'type wl-info))
                        (wh-mint-count:integer (+ count claimed))
                        (limit:integer (get-wl-limit account)))
                        (enforce (<= wh-mint-count limit)
                          (format "You can Mint only {} tokens during whitelist mint" [limit]))
               )]
           )
           ((check-public)
            [
              (enforce (<= count max-per-txn)
                (format "You can mint only {} per transaction" [max-per-txn]))
              (enforce (<= mint-count max-per-user)
                (format "You can Mint only {} tokens per wallet" [max-per-user]))
            ]
           )
          ["Mint Ended"]
        )
    )
  )

  (defun enforce-mint:string
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
     (enforce-ledger)
     (let ( (total-minted:integer (get-total-minted))
           (total-supply:integer (get-total-supply))
          )
       (enforce (< total-minted total-supply) (format "Total supply of {} reached" [total-supply]))
     )
     (enforce (= 1.0 amount) "Amount must always be 1.0 for 1 for 1 NFTs")

     (with-capability (MINT account)
       (let* ( (whitelist-enabled:bool (check-whitelist))
               (random:integer (get-random account))
               (current-length:integer (get-current-length))
               (index:integer (mod random current-length))
               (available-tokens:[integer] (at 'tokens-list (read mint-status MINT_STATUS)))
               (sha256:string (int-to-str 64 (at index available-tokens)))
               (token-id:string (hash sha256))
               (minted:integer (get-account-minted account))
               (collection-info:object{collection-schema} (get-details))
               (creator:string (at 'creator collection-info))
            )
       (cond
         (whitelist-enabled
             [(enforce-whitelist account guard)
              (let ( (claimed:integer (at 'claimed (get-whitelist-info account))))
                      (update whitelists account {
                        "claimed": (+ claimed 1)
                      })
             )]
          )
          ((check-public)
            true
          )
          ["Mint Ended"]
        )
        (update-status
         token-id
         account
         available-tokens
         current-length
         minted
         (str-to-int 64 sha256))
        )
       )
     )

  (defun get-random:integer (account:string)
    (require-capability (PRIVATE))
      (let* ( (prev-block-hash (at "prev-block-hash" (chain-data)))
              (random (str-to-int 64 (hash (+ prev-block-hash (take 20 account)))))
            )
         random
      )
  )

  (defun update-status
    ( token-id:string
      account:string
      available-tokens:[integer]
      current-length:integer
      minted:integer
      minted-token:integer
    )
    (require-capability (PRIVATE))
    (update mint-status MINT_STATUS {
      'tokens-list: (filter (!= minted-token) available-tokens),
      'current-length: (- current-length 1)
      })
    (bind (get-details) {
      'symbol:= symbol,
      'creator:= creator,
      'creator-guard:= creator-guard,
      'royalty-rate:= royalty-rate,
      'fungible:= fungible
      }
      (let ((id:string (format "{}:{}" [symbol token-id])))
        (insert policies id {
          "id": id,
          "owner": account,
          "creator-guard": creator-guard,
          "creator": creator,
          "fungible": fungible,
          "royalty-rate": royalty-rate
        })
      (write account-details account {
        'account: account,
        'minted: (+ minted 1)
        })
      id
    ))
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
    ( token:object{token-info} )
    (enforce-ledger)
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    (with-read mint-status MINT_STATUS {
      'status:= status
      }
      (enforce (= status MINT_COMPLETED) "Pre-mint is not yet completed")
    )
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
            (royalty-payout:decimal
              (floor (* sale-price royalty-rate) (fungible::precision))) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout creator spec)))
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
                 (royalty-payout:decimal
                  (floor (* sale-price royalty-rate) (fungible::precision)))
               (payout:decimal (- sale-price royalty-payout)) )
          (if
            (> royalty-payout 0.0)
            (fungible::transfer buyer creator royalty-payout)
            "No royalty")
            (fungible::transfer buyer recipient payout))
            (update policies qtoken {
              "owner": buyer
              }))
            true
      )
    )
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
    (enforce false "Transfer prohibited")
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

  (defun add-whitelist (account-info:object{add-whitelist-params})
    (require-capability (PRIVATE))
    (bind account-info {
      'account:= account,
      'type:= type
      }
      (enforce (contains type WL-TYPES) "Invalid Whitelist Type")
      (insert whitelists account{
        "account": account,
        "guard": (at 'guard (coin.details account)),
        "claimed": 0,
        "type": type
      })
    )
  )

  (defun add-whitelists (account-infos:[object{add-whitelist-params}])
    (with-capability (WHITELIST)
      (map (add-whitelist) account-infos)
    )
  )

  (defun add-wl-limit (wl-limit)
    (require-capability (PRIVATE))
    (bind wl-limit {
      'limit:= limit,
      'type:= type
      }
      (insert wl-limits type{
        "limit": limit
      })
    )
  )

  (defun add-wl-limits (limits:[integer])
    (with-capability (WHITELIST)
      (map (add-wl-limit) (zip (lambda (x y) { 'type:x, 'limit:y  }) WL-TYPES limits))
    )
  )

  ;;
  ;; helpers
  ;;

  (defun check-whitelist:bool ()
    (let* ( (collection:object{collection-schema} (get-details))
          (public-mint-time:time (at 'public-mint-time collection))
          (whitelist-mint-time:time (at 'whitelist-mint-time collection))
          (total-supply:integer (at 'total-supply collection))
          (chain-time (at 'block-time (chain-data)))
        )
        (and (<= chain-time public-mint-time) (>= chain-time whitelist-mint-time))
    )
  )

  (defun check-public:bool ()
    (let* ( (collection:object{collection-schema} (get-details))
          (public-mint-time:time (at 'public-mint-time collection))
          (mint-end-time:time (at 'mint-end-time collection))
          (total-supply:integer (at 'total-supply collection))
          (chain-time (at 'block-time (chain-data)))
        )
        (enforce (and (<= chain-time mint-end-time) (>= chain-time public-mint-time)) "Mint has not yet started")
    )
  )

  (defun get-current-length:integer ()
    (with-read mint-status MINT_STATUS {
      'current-length:= current-length
    }
    current-length
    )
  )

  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )

  (defun get-account-minted:integer (account:string)
    (with-default-read account-details account
      {"minted": 0}
      {"minted":= minted}
    minted
    )
  )

  (defun get-total-minted:integer ()
    (with-read mint-status MINT_STATUS {
      'current-length:= current-length
    }
    (- (at 'total-supply (get-details)) current-length)
    )
  )

  (defun get-mint-status ()
    (read mint-status MINT_STATUS)
  )

  (defun get-account-info:object{account-schema} (account:string)
    (read account-details account)
  )

  (defun get-whitelist-info:object{whitelist-schema} (account:string)
    (read whitelists account)
  )

  (defun get-details:object{collection-schema} ()
   (read collection-info COLLECTION_INFO)
  )

  (defun get-total-supply:string ()
   (at 'total-supply (get-details))
  )

  (defun get-public-mint-time:time ()
    (with-read collection-info COLLECTION_INFO {
      'public-mint-time:= public-mint-time
      }
      public-mint-time
    )
  )

  (defun get-owner:string (token-id)
    (at 'owner (read policies token-id))
  )

  (defun get-tokens-owned:[string] (account:string)
    (select policies ['id] (where "owner" (= account)))
  )

  (defun get-limit-by-type:integer (type:string)
    (with-default-read wl-limits type
      {"limit": 0}
      {"limit":= limit}
      limit
    )
  )

  (defun get-wl-limit:integer (account:string)
    (with-default-read whitelists account
      {'type: "public"}
      {'type:= type}
      (get-limit-by-type type)
    )
  )

  ;returns maxmium mint limit for a user including whitelist limit
  (defun get-max-per-user (account:string)
    ; (with-default-read whitelists account
    ;   {'claimed: 0}
    ;   {'claimed:= claimed}
    ;   (+ (at 'max-per-user (get-details)) claimed)
    ; )
    (at 'max-per-user (get-details))
  )

  ;;
  ;; Mint state
  ;;

  (defun pause-mint ()
    (with-capability (GOVERNANCE)
      (update mint-status MINT_STATUS {
        'status: MINT_PAUSED
      })
    )
  )

  (defun resume-mint ()
    (with-capability (GOVERNANCE)
      (update mint-status MINT_STATUS {
        'status: MINT_STARTED
      })
    )
  )

  (defun end-mint ()
    (with-capability (GOVERNANCE)
      (update mint-status MINT_STATUS {
        'status: MINT_COMPLETED
      })
    )
  )

  (defun update-end-time (end-time:time)
  ;;will be removed after mint
    (with-capability (GOVERNANCE)
      (update collection-info COLLECTION_INFO {
        'mint-end-time: end-time
      })
    )
  )

  (defun initialize (
    provenance:string
    tokens-list:[string]
    creator:string
    creator-guard:guard
    total-supply:integer
    max-per-user:integer
    max-per-wh:integer
    max-per-txn:integer
    public-mint-time:time
    whitelist-mint-time:time
    mint-end-time:time
    royalty-receiver:string
    royalty-rate:decimal
    price-per-nft:decimal
    whitelist-price:decimal
    name:string
    symbol:string
    fungible:module{fungible-v2})
    (enforce (= (length tokens-list) total-supply) "Total-supply and tokens-list length does not match")
    (let ( (creator-details:object (fungible::details creator ))
            )
      (fungible::enforce-unit royalty-rate)
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")
    )

    (write collection-info COLLECTION_INFO {
      "provenance-hash": provenance,
      "max-per-user": max-per-user,
      "max-per-txn": max-per-txn,
      "max-per-wh": max-per-wh,
      "total-supply": total-supply,
      "tokens-list": tokens-list,
      "creator": creator,
      "creator-guard": creator-guard,
      "public-mint-time": public-mint-time,
      "whitelist-mint-time": whitelist-mint-time,
      "mint-end-time": mint-end-time,
      "royalty-receiver": royalty-receiver,
      "royalty-rate": royalty-rate,
      "price-per-nft": price-per-nft,
      "whitelist-price": whitelist-price,
      "name": name,
      "symbol": symbol,
      "fungible": fungible
    })
    (write mint-status MINT_STATUS {
      "current-length": (length tokens-list),
      "tokens-list": (map (str-to-int 64) tokens-list),
      "status": MINT_STARTED
    })
  )

)


