(module prioritypass-policy PASSPOLICY

    @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

    (defcap PASSPOLICY ()
      (enforce-guard (keyset-ref-guard "kryptomerch-passpolicy-ks" )))

    (implements free.kryptomerch-v1)

    (use free.kryptomerch-v1 [token-info])
    (use util.guards)

    (defcap MINT (account:string)
       (compose-capability (PRIVATE))
     )

     (defcap PRIVATE ()
       true
     )

    (defschema collection-schema
      total-supply:integer ;total supply of tokens that will ever exist
      provenance-hash:string ;sha256 of combined string
      tokens-list:[string] ;list of sha256 of the images that will ever exist
      creator:string
      max-per-txn:integer
      price-per-nft:decimal
      creator-guard:guard
      name:string
     fungible:module{fungible-v2}
    )

     (defschema mint-schema
       tokens-list:[integer]
       current-length:integer
       public-minted:decimal
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

    (deftable policies-table:{policy-schema})
    (deftable collection-info:{collection-schema})
    (deftable mint-status:{mint-schema})
    (deftable account-details:{account-schema})

    (defconst TOKEN_SPEC "token_spec"
      @doc "Payload field for token spec")

    (defconst MINT_STATUS "mint-status")

    (defconst QUOTE-MSG-KEY "quote"
      @doc "Payload field for quote spec")

    (defschema quote-spec
      @doc "Quote data to include in payload"
      price:decimal
      recipient:string
      recipient-guard:guard
      )

    (defconst COLLECTION_INFO "collection-info")


    (defschema quote-schema
      id:string
      spec:object{quote-spec})

    (deftable quotes-table:{quote-schema})

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
      (enforce-guard (priority-pass.ledger-guard))
    )

    (defun enforce-bulk-mint:bool (account:string count:integer)
      (enforce-ledger)
      (let* ( (minted:integer (get-account-minted account))
              (collection-info:object{collection-schema} (get-details))
              (max-per-txn:integer (at 'max-per-txn collection-info))
              (total-minted:integer (get-minted))
              (total-supply:integer (get-total-supply))
              (public-minted:decimal (at 'public-minted (get-mint-status)))
            )
           (enforce (<= count max-per-txn)
              (format "You can mint only {} per transaction" [max-per-txn]))
           (enforce (<= (+ total-minted count) total-supply)
              (format "Total supply of {} reached" [total-supply]))
      )
    )

    (defun get-mint-status ()
      (read mint-status MINT_STATUS)
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
         (enforce (<= total-minted total-supply) (format "Total supply of {} reached" [total-supply]))
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
                 (collection-info:object{collection-schema} (get-details))
                 (creator:string (at 'creator collection-info))
              )
             (coin.transfer account creator (at 'price-per-nft collection-info))
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
        'name:= name,
        'creator:= creator,
        'creator-guard:= creator-guard,
        'fungible:= fungible
        }
        (let ((id:string (format "{}:{}" [name token-id])))
          (insert policies-table id {
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

    (defun update-pass-price:string (price:decimal)
      @doc   "Update mint price"
      (enforce (< 0.0 price) "price is not a positive number")
        (with-capability (PASSPOLICY)
          (with-read collection-info COLLECTION_INFO {
            "price-per-nft":=old-nft-price
          }
          (update collection-info COLLECTION_INFO {"price-per-nft":price})

          )
        )
    )

    (defun enforce-burn:bool
      ( token:object{token-info}
        account:string
        amount:decimal
      )
      (enforce-ledger)
      ;(enforce false "Burn prohibited")
    )

    (defun enforce-init:bool
      ( token:object{token-info} )
      (enforce-ledger)
      true
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

    ;;
    ;; helpers
    ;;

    (defun get-current-length:integer ()
      (with-read mint-status MINT_STATUS {
        'current-length:= current-length
      }
      current-length
      )
    )

    (defun get-policy:object{policy-schema} (token:object{token-info})
      (read policies-table (at 'id token))
    )

    (defun get-account-minted:integer (account:string)
      (with-default-read account-details account
        {"minted": 0}
        {"minted":= minted}
      minted
      )
    )

    (defun get-minted:integer ()
      (with-read mint-status MINT_STATUS {
        'current-length:= current-length
        }
        (- (at 'total-supply (get-details)) current-length)
      )
    )

    (defun get-price:decimal()
     (at 'price-per-nft (get-details))
    )

    (defun get-account-info:object{account-schema} (account:string)
      (read account-details account)
    )

    (defun get-details:object{collection-schema} ()
     (read collection-info COLLECTION_INFO)
    )

    (defun get-total-supply:string ()
     (at 'total-supply (get-details))
    )

    (defun get-owner:string (token-id)
      (at 'owner (read policies-table token-id))
    )

    (defun get-tokens-owned:[string] (account:string)
      (select policies-table ['id] (where "owner" (= account)))
    )

    ;;
    ;; Mint state
    ;;


    ;;SALE

    (defun get-nft (token-id:string)
      (read policies-table token-id)
    )

    (defschema sale-nft-params
    token-id:string
    timeout:integer
  )

  (defun sale-nft:bool
    ( params:object{sale-nft-params}
    )
    (bind params
      { 'token-id := token-id
      , 'timeout := timeout
      }
    (bind (get-nft token-id)
      { 'owner := owner
      }
    true))
  )


;;-------------------------------------------
(defun printtokenlist:integer()
(with-read mint-status MINT_STATUS
  {
    "current-length":=current-length

    }
    current-length
    )
)
    (defun insert-new-tokens-list (tokens-list:[string])
      (with-capability (PASSPOLICY)
        (insert collection-info COLLECTION_INFO {
          "tokens-list": tokens-list
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
      price-per-nft:decimal
      name:string
      fungible:module{fungible-v2}
    )
      ; (coin.create-account "kryptomerch-bank" (read-keyset "kryptomerch-passpolicy-ks") )
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
        "price-per-nft": price-per-nft,
        "name": name,
        "fungible": fungible
      })
      (write mint-status MINT_STATUS {
        "current-length": (length tokens-list),
        "tokens-list": (map (str-to-int 64) tokens-list),
        "public-minted": 0.0
      })
    )


  )


  
