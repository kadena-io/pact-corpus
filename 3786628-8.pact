(module dbcooper-policy COOPER

  (defcap COOPER ()
    (enforce-guard (keyset-ref-guard "free.kryptomerchdbcooper-policy-admin-ks")))
    (implements free.db-cooper-policy-v1)

    (use free.db-cooper-policy-v1 [token-info])
    (use util.guards)

    (defcap MINT (account:string)
       (compose-capability (PRIVATE))
     )

     (defcap PRIVATE ()
       true
     )

     (defcap PRIORITY ()
       (compose-capability (COOPER))
       (compose-capability (PRIVATE))
     )

    (defcap WHITELIST ()
      (compose-capability (COOPER))
      (compose-capability (PRIVATE))
    )

    (defschema collection-schema
      total-supply:integer ;total supply of tokens that will ever exist
      provenance-hash:string ;sha256 of combined string
      tokens-list:[string] ;list of sha256 of the images that will ever exist
      creator:string
      max-per-txn:integer
      price-per-nft:decimal
      whitelist-price:decimal
      creator-guard:guard
      public-mint-time:time
      whitelist-mint-time:time
      mint-end-time:time
      name:string
      fungible:module{fungible-v2}
    )

     (defschema mint-schema
       tokens-list:[integer]
       current-length:integer
       status:string
       public-minted:decimal
     )

     (defschema whitelist-schema
       account:string
       guard:guard
       claimed:integer
     )

     (defschema priority-schema
       account:string
       guard:guard
       claimed:bool
     )

    (defschema policy-schema
      id:string
      fungible:module{fungible-v2}
      creator:string
      creator-guard:guard
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
    (deftable priority:{priority-schema})
    (deftable quotes:{quote-schema})

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

    (defconst MINT_STATUS "mint-status")
    (defconst COLLECTION_INFO "collection-info")
    (defconst MINT_PAUSED "mint-paused")
    (defconst MINT_STARTED "mint-started")
    (defconst MINT_COMPLETED "mint-completed")

    (defschema quote-schema
      id:string
      spec:object{quote-spec})

    (defcap QUOTE:bool
      ( sale-id:string
        token-id:string
        amount:decimal
        price:decimal
        sale-price:decimal
        creator:string
        spec:object{quote-spec}
      )
      @doc "For event emission purposes"
      @event
      true
    )

    (defun enforce-ledger:bool ()
      (enforce-guard (db-cooper.ledger-guard))
    )

    (defun printclaim:bool (account:string)
      (with-read priority account{
        'claimed:= claimed
      }claimed)
    )

    (defun enforce-priority (account:string guard:guard)
       (with-read priority account{
        'guard:= g,
        'claimed:= claimed
       }
        (enforce (= g guard) "Guards doesn't match.")
      )
    )

     (defun enforce-whitelist:bool (account:string guard:guard)
        (with-read whitelists account{
         'guard:= g,
         'claimed:= claimed
        }
         (enforce (= g guard) "Guards doesn't match.")
       )

     )

    (defun enforce-bulk-mint:bool (account:string count:integer)
      (enforce-ledger)
      (let* ( (minted:integer (get-account-minted account))
              (mint-count:integer (+ count minted))
              (whitelist-enabled:bool (check-whitelist))
              (collection-info:object{collection-schema} (get-details))
              (max-per-txn:integer (at 'max-per-txn collection-info))
              (priority-sale-users:[string] (get-priority-users))
              (total-minted:integer (get-minted))
              (total-supply:integer (get-total-supply))
              (public-minted:decimal (at 'public-minted (get-mint-status)))
              (public-limit:integer (get-total-supply) )
            )

          (with-read mint-status MINT_STATUS {
            'status:= status
            }
            (enforce (= status MINT_STARTED) "MINT is paused or completed, can't mint now")
           )
           (enforce (<= count max-per-txn)
              (format "You can mint only {} per transaction" [max-per-txn]))
           (enforce (<= (+ total-minted count) total-supply)
              (format "Total supply of {} reached" [total-supply]))
           (cond
             (
               (contains account priority-sale-users)
               [(let* ( (claimed:bool(printclaim account))
                      )
                      (if (= claimed false) "" "you can mint only one free nft")
               )])
             (whitelist-enabled
                 []
             )
             ((check-public)
              []
             )
            ["Mint Ended"]
          )
      )
    )


    (defun enforce-public-mint ()
      (let ( (public-minted:decimal (at 'public-minted (get-mint-status)))
             (public-limit:integer  (get-total-supply) )
           )
        (update mint-status MINT_STATUS {
          'public-minted: (+ public-minted 1.0)
        })
      )
    )

    (defun enforce-mint:string
      ( token:object{token-info}
        account:string
        guard:guard
        amount:decimal
      )
       (enforce-ledger)
       (let ( (total-minted:integer (get-minted))
             (total-supply:integer (get-total-supply))
            )
         (enforce (< total-minted total-supply) (format "Total supply of {} reached" [total-supply]))
       )
       (with-read mint-status MINT_STATUS {
          'status:= status
          }
          (enforce (= status MINT_STARTED) "PRE-MINT is paused or completed, can't mint now")
         )
       (enforce (= 1.0 amount) "Amount must always be 1.0 for 1 for 1 NFTs")

       (with-capability (MINT account)
         (let* (
                 (random:integer (get-random account))
                 (current-length:integer (get-current-length))
                 (index:integer (mod random current-length))
                 (available-tokens:[integer] (at 'tokens-list (read mint-status MINT_STATUS)))
                 (sha256:string (int-to-str 64 (at index available-tokens)))
                 (token-id:string (hash sha256))
                 (minted:integer (get-account-minted account))
                 (priority-sale-users:[string] (get-priority-users))
              )
         (cond

           (
             (contains account priority-sale-users)
             [(let((claim:bool (printclaim account)))
              (if (= claim false)
                (mint-free-nft guard token-id account available-tokens current-length minted sha256)
                (sale guard token-id account available-tokens current-length minted sha256)
              )
             )]
            )

           [(sale guard token-id account available-tokens current-length minted sha256)]
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

    (defun sale
      ( guard:guard
        token-id:string
        account:string
        available-tokens:[integer]
        current-length:integer
        minted:integer
        sha256:string
        )
        (let* (
                (whitelist-enabled:bool (check-whitelist))
                (collection-info:object{collection-schema} (get-details))
                (creator:string (at 'creator collection-info))

             )
        (cond
          (whitelist-enabled
              [
              (enforce-whitelist account guard)
               (enforce-public-mint)
              (let  ( (claimed:integer (at 'claimed (get-whitelist-info account)))
                      (price:decimal (at 'whitelist-price collection-info)))
                      (coin.transfer account creator price)
                      (update whitelists account{
                       "claimed": (+ claimed 1)
                       })
               )]
           )
           [(check-public)(enforce-public-mint) (coin.transfer account creator (at 'price-per-nft collection-info))]
         )
      )
    )

    (defun mint-free-nft
      ( guard:guard
        token-id:string
        account:string
        available-tokens:[integer]
        current-length:integer
        minted:integer
        sha256:string
      )
        (enforce-priority account guard)
        (update priority account{
              "claimed": true
              })
        (free.priority-pass.burn account)

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
        'name:= name,
        'creator:= creator,
        'creator-guard:= creator-guard,
        'fungible:= fungible
        }
        (let ((id:string (format "{}:{}" [name token-id])))
          (insert policies id {
            "id": id,
            "owner": account,
            "creator-guard": creator-guard,
            "creator": creator,
            "fungible": fungible
          })
        (write account-details account {
          'account: account,
          'minted: (+ minted 1)
          })
        id
      ))
    )

    (defun update-mint-price:string (price:decimal type:string)
      @doc   "Update mint price"
      (enforce (< 0.0 price) "price is not a positive number")
        (with-capability (COOPER)
          (with-read collection-info COLLECTION_INFO {
            "price-per-nft":=old-nft-price,
            "whitelist-price":=old-wh-price
          }
            (cond
              ((= type "whitelist")
                [ (update collection-info COLLECTION_INFO {"whitelist-price":price})
                  (format "Updated Whitelist Mint price from {} to {}" [old-wh-price price])])
              ((= type "public")
                [ (update collection-info COLLECTION_INFO {"price-per-nft":price})
                  (format "Updated Public Mint price from {} to {}" [old-nft-price price])])
              ["Conditions not met"]
            )
          )
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
         ,'creator:= creator:string
        }
      (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
              (price:decimal (at 'price spec))
              (recipient:string (at 'recipient spec))
              (recipient-guard:guard (at 'recipient-guard spec))
              (recipient-details:object (fungible::details recipient))
              (sale-price:decimal (* amount price)))
        (fungible::enforce-unit sale-price)
        (enforce (< 0.0 price) "Offer price must be positive")
        (enforce (=
          (at 'guard recipient-details) recipient-guard)
          "Recipient guard does not match")
        (insert quotes sale-id { 'id: (at 'id token), 'spec: spec }))
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
        }
        (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
          (enforce (= qtoken (at 'id token)) "incorrect sale token")
          (bind spec
            { 'price := price:decimal
            , 'recipient := recipient:string
            }
            (let* ((sale-price:decimal (* amount price)))
              (fungible::transfer buyer recipient sale-price))
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

    (defun add-priority-user (account:string)
      (with-capability (PRIVATE)
      (insert priority account{
        "account": account,
        "guard": (at 'guard (coin.details account)),
        "claimed": false
      }))
    )

    (defun add-priority-users (accounts:[string])
      (with-capability (PRIORITY)
        (map (add-priority-user) accounts)
      )
    )

    (defun add-whitelist (account:string)
      (require-capability (PRIVATE))
      (insert whitelists account{
        "account": account,
        "guard": (at 'guard (coin.details account)),
        "claimed": 0
      })
    )

    (defun add-whitelists (accounts:[string])
      (with-capability (WHITELIST)
        (map (add-whitelist) accounts)
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
        'public-minted:= public-minted
        }
        ( public-minted)
      )
    )

    (defun get-minted:integer ()
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

    (defun get-priority-users ()
      (keys priority)
    )

    (defun get-whitelist-info:object{whitelist-schema} (account:string)
      (read whitelists account)
    )

    (defun get-priority-info:object{priority-schema} (account:string)
      (read priority account)
    )

    (defun get-details:object{collection-schema} ()
     (read collection-info COLLECTION_INFO)
    )

    (defun get-total-supply:string ()
     (at 'total-supply (get-details))
    )

    (defun get-nft-price:decimal ()
     (at 'price-per-nft (get-details))
    )

    (defun get-wl-price:decimal ()
     (at 'whitelist-price (get-details))
    )

    (defun get-pp-limit (account:string)
      (at 'max-per-pp (get-details))
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

    (defun update-mint-time
      (
      public-mint-time:time
      whitelist-mint-time:time
      mint-end-time:time
      )
      (with-capability (COOPER)
      (update collection-info COLLECTION_INFO
        {
          "public-mint-time": public-mint-time,
          "whitelist-mint-time": whitelist-mint-time,
          "mint-end-time": mint-end-time
          }
      )
      )
    )

    ;;
    ;; Mint state
    ;;

    (defun pause-mint ()
      (with-capability (COOPER)
        (update mint-status MINT_STATUS {
          'status: MINT_PAUSED
        })
      )
    )

    (defun resume-mint ()
      (with-capability (COOPER)
        (update mint-status MINT_STATUS {
          'status: MINT_STARTED
        })
      )
    )

    (defun end-mint ()
      (with-capability (COOPER)
        (let ( (public-minted:decimal (at 'public-minted (get-mint-status)))
               (public-limit:decimal  (get-total-supply) )
             )
          (enforce (= public-minted public-limit) "Tokens not sold completely, can't end mint now")
        )
        (update mint-status MINT_STATUS {
          'status: MINT_COMPLETED
        })
      )
    )

    (defun update-tokens-list ()
        ;;will be removed after mint
      (with-capability (COOPER)
        (update mint-status MINT_STATUS {
          'tokens-list: (map (str-to-int 64) (at 'tokens-list (get-mint-status)))
        })
      )
    )

    (defun update-end-time (end-time:time)
    ;;will be removed after mint
      (with-capability (COOPER)
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
      max-per-txn:integer
      public-mint-time:time
      whitelist-mint-time:time
      mint-end-time:time
      price-per-nft:decimal
      whitelist-price:decimal
      name:string
      fungible:module{fungible-v2})
      (enforce (= (length tokens-list) total-supply) "Total-supply and tokens-list length does not match")
      (let ( (creator-details:object (fungible::details creator ))
          )
        (enforce (=
          (at 'guard creator-details) creator-guard)
          "Creator guard does not match")
        )
      (insert collection-info COLLECTION_INFO {
        "provenance-hash": provenance,
        "max-per-txn": max-per-txn,
        "total-supply": total-supply,
        "tokens-list": tokens-list,
        "creator": creator,
        "creator-guard": creator-guard,
        "public-mint-time": public-mint-time,
        "whitelist-mint-time": whitelist-mint-time,
        "mint-end-time": mint-end-time,
        "whitelist-price": whitelist-price,
        "price-per-nft": price-per-nft,
        "name": name,
        "fungible": fungible
      })
      (write mint-status MINT_STATUS {
        "current-length": (length tokens-list),
        "tokens-list": (map (str-to-int 64) tokens-list),
        "status": MINT_STARTED,
        "public-minted": 0.0
      })
    )
  )


  
