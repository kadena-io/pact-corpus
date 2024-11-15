(module kryptomerch-contract MERCH

  (use util.fungible-util)
  (use kip.token-manifest)
  (implements kip.poly-fungible-v2)
  (use kip.poly-fungible-v2 [account-details sender-balance-change receiver-balance-change])
  (use kip.token-policy-v1)

   ; ============================================
   ; ==           TABLES AND SCHEMAS         ==
   ; ============================================


   (defschema token-schema
    id:string
    manifest:object{manifest}
    precision:integer
    supply:decimal
    policy:module{kip.token-policy-v1}
   )

  (defschema collection-schema
    total-supply:integer
    name:string
    symbol:string
    creator:string
    policy:module{kip.token-policy-v1}
    guard:guard
    price:decimal
    imageUrl:string
    royaltyRate:decimal
    royaltyAccount:string
   )


    (defschema indexSchema
        index:integer
    )

   (deftable collections-table:{collection-schema})
   (deftable ledger-table:{account-details})
   (deftable tokens-table:{token-schema})
   (deftable indexTable:{indexSchema})


  (defschema policy-info
    policy:module{kip.token-policy-v1}
    token:object{kip.token-policy-v1.token-info}
  )


  ;;
  ;; Capabilities
  ;;

  (defcap MERCH ()
    (enforce-guard (keyset-ref-guard "free.kryptomerch-contract-ks"))
       (compose-capability (PRIVATE))
  )

  (defcap IS_ADMIN ()
       (compose-capability (MERCH))
  )

    (defcap CREATE-COLLECTION (name:string symbol:string)
      @managed ;; one-shot for a given amount
      (compose-capability (PRIVATE))
    )

  (defcap TRANSFER:bool
      ( id:string
        sender:string
        receiver:string
        amount:decimal
      )
      @managed amount TRANSFER-mgr
      (enforce-unit id amount)
      (enforce (> amount 0.0) "Amount must be positive")
      (compose-capability (DEBIT id sender))
      (compose-capability (CREDIT id receiver))
  )

  (defcap XTRANSFER:bool
      ( id:string
        sender:string
        receiver:string
        target-chain:string
        amount:decimal
      )
      @managed amount TRANSFER-mgr
      (enforce false "cross chain not supported")
  )

  (defun TRANSFER-mgr:decimal
      ( managed:decimal
        requested:decimal
      )
      (let ((newbal (- managed requested)))
        (enforce (>= newbal 0.0)
          (format "TRANSFER exceeded for balance {}" [managed]))
        newbal)
  )

    (defcap SUPPLY:bool (id:string supply:decimal)
      @doc " Emitted when supply is updated, if supported."
      @event true
    )

    (defcap TOKEN:bool (id:string precision:integer supply:decimal policy:module{kip.token-policy-v1})
      @event
      true
    )

    (defcap RECONCILE:bool
      ( token-id:string
        amount:decimal
        sender:object{sender-balance-change}
        receiver:object{receiver-balance-change}
      )
      @doc " For accounting via events. \
           \ sender = {account: '', previous: 0.0, current: 0.0} for mint \
           \ receiver = {account: '', previous: 0.0, current: 0.0} for burn"
      @event
      true
    )

    (defcap ACCOUNT_GUARD:bool (id:string account:string guard:guard)
      @doc " Emitted when ACCOUNT guard is updated."
      @event
      true
    )

    ;;
    ;; Implementation caps
    ;;

    (defcap ROTATE (id:string account:string)
      @doc "Autonomously managed capability for guard rotation"
      @managed
      true)

    (defcap DEBIT (id:string sender:string)
      (enforce-guard (account-guard id sender))
    )

    (defun account-guard:guard (id:string account:string)
      (with-read ledger-table (key id account) { 'guard := guard } guard)
    )

    (defcap CREDIT (id:string receiver:string) true)

    (defcap UPDATE_SUPPLY ()
      "private cap for update-supply"
      true)


    (defcap MINT (account:string amount:decimal)
      @managed ;; one-shot for a given amount
      (compose-capability (PRIVATE))
      (compose-capability (CREDIT-MERCH account))
      (compose-capability (UPDATE_SUPPLY))
    )

    (defcap PUT_ON_SALE (id:string owner:string price:decimal)
      @doc "Emitted event when an NFT is put on sale "
      @event true
    )

    (defcap REMOVED_FROM_SALE (id:string owner:string)
      @doc "Emitted event when an NFT is removed from sale "
      @event true
    )

    (defcap BOUGHT (id:string new-owner:string original-owner:string price:decimal)
        @doc "Emitted event when an NFT is removed from sale "
        @event true
    )

    (defcap PRIVATE ()
      true
    )

    (defcap CREDIT-MERCH (receiver:string) true)

    (defcap MINTED (token-id:string account:string)
      @event
      true
    )

    (defcap COLLECTION_CREATED (
            name:string
            symbol:string
            total-supply:integer
            policy:module{kip.token-policy-v1}
            guard:guard
            creator:string
          )
            @event
            true
      )

    (defcap BURN (id:string account:string amount:decimal)
      @managed ;; one-shot for a given amount
      (enforce (< 0.0 amount) "Amount must be positive")
      (compose-capability (DEBIT id account))
      (compose-capability (UPDATE_SUPPLY))
    )

    (defun ledger-guard:guard ()
      @doc "Ledger module guard for policies to be able to validate access to policy operations."
      (create-module-guard "dbc-ledger-table-guard")
    )

    (defun create-account:bool
        ( id:string
          account:string
          guard:guard
        )

        (enforce-valid-account account)

        (insert ledger-table (key id account)
          { "balance" : 0.0
          , "guard"   : guard
          , "id" : id
          , "account" : account
          })
        (emit-event (ACCOUNT_GUARD id account guard))
      )

      (defun total-supply:decimal (id:string)
        (with-default-read tokens-table id
          { 'supply : 0.0 }
          { 'supply := s }
          s)
      )

      (defun create-token:bool
        ( id:string
          precision:integer
          manifest:object{manifest}
          policy:module{kip.token-policy-v1}
          collection-name:string
        )
        (enforce-verify-manifest manifest)
        (policy::enforce-init
          { 'id: id, 'supply: 1.0, 'precision: precision, 'manifest: manifest })
          (insert tokens-table id {
            "id": id,
            "precision": precision,
            "manifest": manifest,
            "supply": 1.0,
            "policy": policy
          })
          (emit-event (TOKEN id precision 1.0 policy))
      )

      (defschema nft-collection-info
          name:string
          symbol:string
          total-supply:integer
          guard:guard
          creator:string
          tokens-list:[string]
          price:decimal
          policy:module{kip.token-policy-v1}
          imageUrl:string
          royaltyRate:decimal
          royaltyAccount:string
        )


      (deftable collections-request-table:{nft-collection-info})

      (defun nft-collection-request
        ( name:string
          symbol:string
          total-supply:integer
          guard:guard
          creator:string
          tokens-list:[string]
          price:decimal
          policy:module{kip.token-policy-v1}
          imageUrl:string
          royaltyRate:decimal
          royaltyAccount:string
        )

        (insert collections-request-table name
          {
            "name": name,
            "symbol": symbol,
            "total-supply": total-supply,
            "guard": guard,
            "creator":creator,
            "tokens-list":tokens-list,
            "price":price,
            "policy":policy,
            "imageUrl":imageUrl,
            "royaltyRate":royaltyRate,
            "royaltyAccount":royaltyAccount
          }
         )
        )

      (defun get-all-collections-request ()
        (keys collections-request-table)
      )

      (defun get-collection-info (name:string)
        (read collections-request-table name)
      )


      (defun launch-nft-collection:bool
         (
           name:string
         )
        (with-capability (IS_ADMIN)
        (with-read collections-request-table name
          {
            "name":= name,
            "symbol":= symbol,
            "total-supply":= total-supply,
            "guard":= guard,
            "creator":=creator,
            "tokens-list":=tokens-list,
            "price":=price,
            "policy":=policy,
            "imageUrl":=imageUrl,
            "royaltyRate":=royaltyRate,
            "royaltyAccount":=royaltyAccount
          }

        (insert collections-table name
          {
            "name": name,
            "symbol": symbol,
            "total-supply": total-supply,
            "guard": guard,
            "creator":creator,
            "price":price,
            "policy":policy,
            "imageUrl":imageUrl,
            "royaltyRate":royaltyRate,
            "royaltyAccount":royaltyAccount
          }
        )

        (insert indexTable name
          {
            "index":0
            }
         )
        (add-tokens total-supply tokens-list name)
        (free.kryptomerchpolicy-contract.create-nft-collection  creator guard total-supply 1 price name imageUrl)
        (emit-event (COLLECTION_CREATED name symbol total-supply policy guard creator ))
        )
       )
      )

      (defun truncate:decimal (id:string amount:decimal)
        (floor amount (precision id))
      )

      (defun rotate:bool (id:string account:string new-guard:guard)
        (with-capability (ROTATE id account)
          (enforce-transfer-policy id account account 0.0)
          (with-read ledger-table (key id account)
            { "guard" := old-guard }
            (update ledger-table (key id account)
              { "guard" : new-guard })
            (emit-event (ACCOUNT_GUARD id account new-guard)))))

      (defun transfer:bool
        ( id:string
          sender:string
          receiver:string
          amount:decimal
        )
        (enforce (!= sender receiver)
          "sender cannot be the receiver of a transfer")
        (enforce-valid-transfer sender receiver (precision id) amount)
        (with-capability (TRANSFER id sender receiver amount)
          (with-read ledger-table (key id receiver)
            { "guard" := g }
            (let
              ( (sender (debit id sender amount))
                (receiver (credit id receiver g amount))
              )
              (emit-event (RECONCILE id amount sender receiver))
            )
          )
        )
      )


      (defun enforce-transfer-policy
        ( id:string
          sender:string
          receiver:string
          amount:decimal
        )
        (bind (get-policy-info id)
          { 'policy := policy:module{kip.token-policy-v1}
          , 'token := token }
          (policy::enforce-transfer token sender (account-guard id sender) receiver amount))
      )

      (defun enforce-collection:bool (name:string)
        (with-read collections-table name
          { 'guard:= collection-guard }
          (enforce-one "Only Admin or Collection creator can create tokens-table"
            [(enforce-guard collection-guard)
             (enforce-guard (keyset-ref-guard "kryptomerch-contract-ks"))])
        )
      )

      (defun transfer-create:bool
        ( id:string
          sender:string
          receiver:string
          receiver-guard:guard
          amount:decimal
        )
        (enforce (!= sender receiver)
          "sender cannot be the receiver of a transfer")
        (enforce-valid-transfer sender receiver (precision id) amount)

        (with-capability (TRANSFER id sender receiver amount)
          (let
            (
              (sender (debit id sender amount))
              (receiver (credit id receiver receiver-guard amount))
            )
            (emit-event (RECONCILE id amount sender receiver))
          ))
      )

      ; ============================================
      ; ==           FOR TOKEN-ID                 ==
      ; ============================================


      (defschema nft-info
        owner:string
        token-id:string
        collection-name:string
        index:integer
      )

      (deftable nfts-info-by-id:{nft-info})
      (deftable nfts-info-by-owner:{nft-info})
      (defschema token-record-schema
        tokens-list:[integer]
        current-length:integer
      )

      (deftable token-record:{token-record-schema})

      (defun add-tokens (
        total-supply:integer
        tokens-list:[string]
        collectionName:string
        )
        (require-capability (PRIVATE))

        (enforce (= (length tokens-list) total-supply) "Total-supply and tokens-list length does not match")
        (insert token-record collectionName {
          "current-length": (length tokens-list),
          "tokens-list": (map (str-to-int 64) tokens-list)
          })
      )

      (defun updatetokenlist:bool
        (tokens-list:[string]
          collectionName:string)
          (with-read collections-table collectionName
            {
              "creator":= creator
              }
          (enforce-guard (at "guard" (coin.details creator)))
          (let (
            (tkns:[integer](map (str-to-int 64) tokens-list) )
            )

            (with-read token-record collectionName
              {
                "tokens-list" := tkn-list
            }
            (let (
                (tkn:[integer](+  tkn-list tkns))
                )

                (update token-record collectionName
                  {
                    "current-length": (+ (length tokens-list) (get-current-length collectionName)),
                    "tokens-list": tkn
                  }
                )
                (update collections-table collectionName
                  {
                    "total-supply": (length tkn)
                  }
                  )
              )
            )
          )
         )
        )

      (defun get-nft-info-by-token-id (token-id:string)
        (read nfts-info-by-id token-id)
      )

      (defun get-random:integer (account:string)
        (require-capability (PRIVATE))
        (let* ( (prev-block-hash (at "prev-block-hash" (chain-data)))
              (random (str-to-int 64 (hash (+ prev-block-hash (take 20 account)))))
          )
         random
        )
      )

    (defconst TOKENS "token-record")

    (defun get-current-length:integer (collectionName:string)
      (with-read token-record collectionName
          {
            'current-length:= current-length
          }
          current-length
      )
    )

    (defun get-total-supply:string (collectionName:string)
     (at 'total-supply (get-collection collectionName))
    )

    (defun get-minted:integer (name:string)
      (with-read token-record name {
        'current-length:= current-length
      }
      (- (at 'total-supply (get-collection name)) current-length)
      )
    )

    (defun get-current-index(collectionName:string)
      (with-read indexTable collectionName
        {
          "index":=index
        }
        index
        )
    )



    (defun update-price:string (price:decimal collectionName:string)
      @doc   "Update mint price"
      (enforce (< 0.0 price) "price is not a positive number")
          (with-read collections-table collectionName
          {
            "creator":=creator
          }
          (enforce-guard (at "guard" (coin.details creator)))

          (update collections-table collectionName
            {
              "price":price
              }
            )
          )
        (free.kryptomerchpolicy-contract.update-mint-price price collectionName)

    )

; ============================================

      (defun mint:bool
        ( account:string
          guard:guard
          amount:decimal
          collectionName:string
          count:integer

        )
        (let ( (total-minted:integer (get-minted collectionName))
              (total-supply:integer (get-total-supply collectionName))
              (indx:integer (get-current-index collectionName))
             )

          (enforce (<=  indx total-supply) "We are unable to produce additional images as the current supply has been depleted.")
          (enforce (< total-minted total-supply) (format "Total supply of {} reached" [total-supply]))
        )
        (enforce (= 1.0 amount) "Amount must always be 1.0 for 1 for 1 NFTs")

        (with-capability (MINT account amount)

        (let* (
               (collection-info:object{collection-schema} (get-collection collectionName))
               (creator:string (at 'creator collection-info))
               (imageUrl:string (at 'imageUrl collection-info))
               (policy:module{kip.token-policy-v1} (at 'policy collection-info))

          )
        (let*
              (
                (random:integer (get-random account))
                (current-length:integer (get-current-length collectionName))
                (index:integer (mod random current-length))
                (available-tokens:[integer] (at 'tokens-list (read token-record collectionName)))
                (sha256:string (int-to-str 64 (at index available-tokens)))
                (tkn-id:string (hash sha256))
                (token-id:string (format "{}:{}" [collectionName tkn-id]))
                (imageIndex:integer (+ 1 (get-current-index collectionName)))
                (minted-token:integer (str-to-int 64 sha256))
                (imgUrl:string (format "{}/{}.jpg" [imageUrl imageIndex]))
                (datum-object
                  {
                    'creator: creator
                    , "hash":sha256
                   , 'collectionName: collectionName
                   , "tokenId":tkn-id
                   , "imageIndex":imageIndex
                   , "imageUrl":imgUrl
                  })
                (datum-uri (kip.token-manifest.uri "pact:schema" "free.free-policy.token-metadata"))
                (manifest-datum (kip.token-manifest.create-datum datum-uri datum-object)) ; See NOTE (1)
                (manifest-uri datum-uri)
                (nft-manifest (kip.token-manifest.create-manifest manifest-uri [manifest-datum]))
                (token-precision 0)
              )
              (marmalade.ledger.create-token token-id token-precision nft-manifest policy)
              (marmalade.ledger.create-account token-id account guard)
              (install-capability (marmalade.ledger.MINT token-id account 1.0))
              (marmalade.ledger.mint token-id account guard 1.0)

              (insert nfts-info-by-id token-id
                {
                  "owner":account,
                  "token-id":token-id,
                  "collection-name":collectionName,
                  "index":imageIndex
                  })

              (update indexTable collectionName
                {
                  "index":imageIndex
              })

              (write nfts-info-by-owner account
                 {
                    "owner":account,
                    "token-id":token-id,
                    "collection-name":collectionName,
                    "index":imageIndex
                      })
              (update token-record collectionName {
                'tokens-list: (filter (!= minted-token) available-tokens),
                'current-length: (- current-length 1)
                })

              )
          )
          )
        )

            (defun printtokenlist:integer(collectionName:string)
              (with-read token-record collectionName
                {
                  "tokens-list":=tokens-list
                }
                  tokens-list
              )
            )

      (defun ids-owned-by (owner:string)
         @doc "All punks owned by someone"
        (select nfts-info-by-id ["token-id"] (where "owner" (= owner)))
      )

      (defun burn:bool
        ( id:string
          account:string
          amount:decimal
        )
        (with-capability (BURN id account amount)
          (bind (get-policy-info id)
            { 'policy := policy:module{kip.token-policy-v1}
            , 'token := token }
            (policy::enforce-burn token account amount))
          (let
            (
              (sender (debit id account amount))
              (receiver:object{receiver-balance-change}
                {'account: "", 'previous: 0.0, 'current: 0.0})
            )
            (emit-event (RECONCILE id amount sender receiver))
            (update-supply id (- amount))
          ))
      )

      (defun debit:object{sender-balance-change}
        ( id:string
          account:string
          amount:decimal
        )

        (require-capability (DEBIT id account))

        (enforce-unit id amount)

        (with-read ledger-table (key id account)
          { "balance" := old-bal }

          (enforce (<= amount old-bal) "Insufficient funds")

          (let ((new-bal (- old-bal amount)))
            (update ledger-table (key id account)
              { "balance" : new-bal }
              )
            {'account: account, 'previous: old-bal, 'current: new-bal}
          ))
      )

      (defun credit:object{receiver-balance-change}
        ( id:string
          account:string
          guard:guard
          amount:decimal
        )
        @doc "Credit AMOUNT to ACCOUNT balance"

        @model [ (property (> amount 0.0))
                 (property (valid-account account))
               ]
        (enforce-valid-account account)
        (enforce-unit id amount)
        (with-default-read ledger-table (key id account)
          { "balance" : -1.0, "guard" : guard }
          { "balance" := old-bal, "guard" := retg }
          (enforce (= retg guard)
            "account guards do not match")

          (let* ((is-new
                   (if (= old-bal -1.0)
                       (enforce-reserved account guard)
                     false))
                  (new-bal (if is-new amount (+ old-bal amount)))
                )

          (write ledger-table (key id account)
            { "balance" : new-bal
            , "guard"   : retg
            , "id"   : id
            , "account" : account
            })
            (if is-new (emit-event (ACCOUNT_GUARD id account retg)) true)
            {'account: account, 'previous: (if is-new 0.0 old-bal), 'current: new-bal}
          ))
      )

      (defun credit-account:object{receiver-balance-change}
        ( id:string
          account:string
          amount:decimal
        )
        @doc "Credit AMOUNT to ACCOUNT"
        (credit id account (account-guard id account) amount)
      )

      (defun update-supply:bool (id:string amount:decimal)
        (require-capability (UPDATE_SUPPLY))
        (with-default-read tokens-table id
          { 'supply: 0.0 }
          { 'supply := s }
          (let ((new-supply (+ s amount)))
          (update tokens-table id {'supply: new-supply })
          (emit-event (SUPPLY id new-supply))))
      )

      (defun enforce-unit:bool (id:string amount:decimal)
        (let ((p (precision id)))
        (enforce
          (= (floor amount p)
             amount)
          "precision violation"))
      )

      (defun precision:integer (id:string)
        (with-default-read tokens-table id
          {'precision: 0}
          {'precision:= p}
        p)
      )

      (defpact transfer-crosschain:bool
        ( id:string
          sender:string
          receiver:string
          receiver-guard:guard
          target-chain:string
          amount:decimal )
        (step (format "{}" [(enforce false "cross chain not supported")]) false))


      (defun get-ledger-table-entry (key:string)
        (read ledger-table key))

      (defun format-token-id:string (collection-name:string index:string)
        (format "{}:{}" [collection-name index])
      )

      ;;
      ;; sale
      ;;

      (defcap SALE:bool
        (id:string seller:string amount:decimal timeout:integer sale-id:string)
        @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until TIMEOUT block height."
        @event
        (enforce (> amount 0.0) "Amount must be positive")
        (compose-capability (OFFER id seller amount timeout))
      )

      (defcap OFFER:bool
        (id:string seller:string amount:decimal timeout:integer)
        @doc "Managed cap for SELLER offering AMOUNT of token ID until TIMEOUT."
        @managed
        (compose-capability (DEBIT id seller))
      )

      (defcap WITHDRAW:bool
        (id:string seller:string amount:decimal timeout:integer sale-id:string)
        @doc "Withdraws offer SALE from SELLER of AMOUNT of token ID after timeout."
        @event
        (compose-capability (CREDIT id seller))
      )

      (defcap BUY:bool
        (id:string seller:string buyer:string amount:decimal timeout:integer sale-id:string)
        @doc "Completes sale OFFER to BUYER."
        @managed
        (compose-capability (CREDIT id buyer))
      )

      (defcap SALE_PRIVATE:bool (sale-id:string) true)

      (defpact sale:bool
        ( id:string
          seller:string
          amount:decimal
          timeout:integer
        )
        (step (format "{}" [(enforce false "sale not supported")]) false)
      )
      (defun sale-account:string ()
        (format "sale-{}" [(pact-id)])
      )

      (defun offer:bool
        ( id:string
          seller:string
          amount:decimal
        )
        @doc "Initiate sale with by SELLER by escrowing AMOUNT of TOKEN until TIMEOUT."
        (require-capability (SALE_PRIVATE (pact-id)))
        (bind (get-policy-info id)
          { 'policy := policy:module{kip.token-policy-v1}
          , 'token := token }
          (policy::enforce-offer token seller amount (pact-id)))
        (let
          (
            (sender (debit id seller amount))
            (receiver (credit id (sale-account) (create-pact-guard "SALE") amount))
          )
          (emit-event (TRANSFER id seller (sale-account) amount))
          (emit-event (RECONCILE id amount sender receiver)))
      )

      (defun withdraw:bool
        ( id:string
          seller:string
          amount:decimal
        )
        @doc "Withdraw offer by SELLER of AMOUNT of TOKEN before TIMEOUT"
        (require-capability (SALE_PRIVATE (pact-id)))
        (let
          (
            (sender (debit id (sale-account) amount))
            (receiver (credit-account id seller amount))
          )
          (emit-event (TRANSFER id (sale-account) seller amount))
          (emit-event (RECONCILE id amount sender receiver)))
      )


      (defun buy:bool
        ( id:string
          seller:string
          buyer:string
          buyer-guard:guard
          amount:decimal
          sale-id:string
        )
        @doc "Complete sale with transfer."
        (require-capability (SALE_PRIVATE (pact-id)))
        (bind (get-policy-info id)
          { 'policy := policy:module{kip.token-policy-v1}
          , 'token := token }
          (policy::enforce-buy token seller buyer buyer-guard amount sale-id))
        (let
          (
            (sender (debit id (sale-account) amount))
            (receiver (credit id buyer buyer-guard amount))
          )
          (emit-event (TRANSFER id (sale-account) buyer amount))
          (emit-event (RECONCILE id amount sender receiver)))
      )



    ;;
    ;; ACCESSORS
    ;;

    (defun key:string ( id:string account:string )
      @doc "DB key for ledger-table account"
      (format "{}:{}" [id account])
    )

    (defun get-manifest:object{manifest} (id:string)
      (at 'manifest (read tokens-table id)))

    (defun get-token:object{token-schema} (id:string)
      "Read token"
      (read tokens-table id)
    )

    (defun get-collection:object{collection-schema} (name:string)
      "Read Collection"
      (read collections-table name)
    )

    (defun get-token-uri:object{kip.token-manifest.mf-uri} (id:string)
      (at 'uri (get-manifest id))
    )

    (defun get-policy-info:object{policy-info} (id:string)
      (with-read tokens-table id
        { 'policy := policy:module{kip.token-policy-v1}
        , 'supply := supply
        , 'precision := precision
        , 'manifest := manifest
        }
        { 'policy: policy
        , 'token:
          { 'id: id
          , 'supply: supply
          , 'precision: precision
          , 'manifest: manifest
          } } )
    )


    (defun get-balance:decimal (id:string account:string)
      (at 'balance (read ledger-table (key id account)))
    )

    (defun details:object{account-details}
      ( id:string account:string )
      (read ledger-table (key id account))
    )

    (defun get-all-collection()

      (keys collections-table )
    )


    (defun get-collection-creator(collectionName:string)
        (with-read collections-table collectionName
          {
            "creator":=creator
            }
            creator
            )
      )


      (defun get-collection-price(collectionName:string)
        (with-read collections-table collectionName
          {
            "price":=price
            }
            price
          )
        )

    (defun get-imageUrl (tokenId:string)
      (let* ((data (at 'data (marmalade.ledger.get-manifest tokenId)))

              (datum (at 'datum (at 0 data)))
              (imageUrl:string (at 'imageUrl datum)))
        imageUrl))

        (defun resetIndex(collectionName:string index:integer)
          (with-read collections-table collectionName
            {
              "creator":=creator
              }
          (enforce-guard (at "guard" (coin.details creator)))
          (update indexTable collectionName
            {
              "index":index
            }
          )
          )
        )

)





  
