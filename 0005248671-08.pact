(module km-pass-contract PASS

    (use util.fungible-util)
    (use util.guards)
    (use kip.token-manifest)
    (use kip.token-policy-v1)
    (implements kip.poly-fungible-v2)
    (use kip.poly-fungible-v2 [account-details sender-balance-change receiver-balance-change])

    (defcap PASS ()
     (enforce-guard (keyset-ref-guard "free.km-pass-ks")))

     (defcap IS_ADMIN ()
          (compose-capability (PASS))
     )
     ; ============================================
     ; ==           TABLES AND SCHEMAS         ==
     ; ============================================


    (defschema passbalance-schema
        @doc "schema for holding the balance of a pass"
          balance:integer
    )

    (defschema token-schema
      id:string
      manifest:object{manifest}
      precision:integer
      supply:decimal
      policy:module{kip.token-policy-v1}
     )

     (defschema policy-info
       policy:module{kip.token-policy-v1}
       token:object{kip.token-policy-v1.token-info}
      )

     (defschema collection-schema
       total-supply:integer
       name:string
       symbol:string
       creator:string
       policy:module{kip.token-policy-v1}
       guard:guard
     )

    (defschema indexSchema
        index:integer
       )
    (defschema nft-info
         owner:string
         tokenId:string
         collectionName:string
         index:integer
       )

    (defschema token-record-schema
         tokens-list:[integer]
         current-length:integer
       )

     (deftable collections_table:{collection-schema})
     (deftable ledger_table:{account-details})
     (deftable tokens_table:{token-schema})
     (deftable passbalance_table:{passbalance-schema})
     (deftable indexTable:{indexSchema})
     (deftable nfts-info-by-id:{nft-info})
     (deftable nfts-info-by-owner:{nft-info})
     (deftable token-record:{token-record-schema})





     ; ============================================
     ; ==           poly-fungible-v2 caps        ==
     ; ============================================


     (defcap TRANSFER:bool
       ( id:string sender:string receiver:string amount:decimal )
       @managed amount TRANSFER-mgr
       (enforce-unit id amount)
       (enforce (> amount 0.0) "Amount must be positive")
       (compose-capability (DEBIT id sender))
       (compose-capability (CREDIT id receiver))
     )

     (defcap XTRANSFER:bool
       ( id:string sender:string receiver:string target-chain:string amount:decimal  )
       @managed amount TRANSFER-mgr
       (enforce false "cross chain not supported")
     )

     (defun TRANSFER-mgr:decimal
       ( managed:decimal requested:decimal )
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
       ( token-id:string amount:decimal sender:object{sender-balance-change} receiver:object{receiver-balance-change} )
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

     ; ============================================
     ; ==           Implementation caps          ==
     ; ============================================
       ;;
       ;;
       ;;

       (defcap ROTATE (id:string account:string)
         @doc "Autonomously managed capability for guard rotation"
         @managed
         true)

       (defcap DEBIT (id:string sender:string)
         (enforce-guard (account-guard id sender))
       )

       (defun account-guard:guard (id:string account:string)
         (with-read ledger_table (key id account) { 'guard := guard } guard)
       )

       (defcap CREDIT (id:string receiver:string) true)

       (defcap MINT-PASS (account:string amount:decimal)
         @managed ;; one-shot for a given amount
         (compose-capability (PRIVATE))
         (compose-capability (CREDIT-PASS account))
       )

       (defcap CREATE-COLLECTION (name:string symbol:string)
         @managed ;; one-shot for a given amount
         (compose-capability (PRIVATE))
       )

       (defcap PRIVATE ()
         true
       )


        (defcap CREDIT-PASS (receiver:string) true)

        (defcap MINTED (token-id:string account:string)
          @event
          true
        )

        (defcap COLLECTION_CREATED
          ( name:string symbol:string total-supply:integer policy:module{kip.token-policy-v1} guard:guard creator:string )
          @event
          true
        )

        (defun ledger-guard:guard ()
          @doc "Ledger module guard for policies to be able to validate access to policy operations."
          (create-module-guard "ledger_table-guard")
        )

        (defun create-account:bool
          ( id:string account:string guard:guard )
          (enforce-valid-account account)
          (enforce-reserved account guard)
          (insert ledger_table (key id account)
            { "balance" : 0.0
            , "guard"   : guard
            , "id" : id
            , "account" : account
            })
          (emit-event (ACCOUNT_GUARD id account guard))
        )

        (defun get-current-index()
          (with-read indexTable INDEX
            { "index":=index }
            index
          )
        )

        (defun total-supply:decimal (id:string)
          (with-default-read tokens_table id
            { 'supply : 0.0 }
            { 'supply := s }
            s)
        )

        (defun create-token:bool
          ( id:string precision:integer manifest:object{manifest} policy:module{kip.token-policy-v1} collection-name:string )
          (enforce-verify-manifest manifest)
          (policy::enforce-init
            { 'id: id, 'supply: 1.0, 'precision: precision, 'manifest: manifest })
            (insert tokens_table id {
              "id": id,
              "precision": precision,
              "manifest": manifest,
              "supply": 1.0,
              "policy": policy
            })
            (emit-event (TOKEN id precision 1.0 policy))
        )

      (defun create-collection:bool
         ( name:string symbol:string total-supply:integer policy:module{kip.token-policy-v1} guard:guard creator:string tokens-list:[string] )
        (with-capability (IS_ADMIN)
        (with-capability (CREATE-COLLECTION name symbol)
        (insert collections_table name
          {
            "name": name,
            "symbol": symbol,
            "total-supply": total-supply,
            "policy": policy,
            "guard": guard,
            "creator":creator
          }
        )
        (add-tokens total-supply tokens-list name)
        (emit-event (COLLECTION_CREATED name symbol total-supply policy guard creator))
        )
        )
      )

        (defun truncate:decimal (id:string amount:decimal)
          (floor amount (precision id))
        )

        (defun rotate:bool (id:string account:string new-guard:guard)
          (with-capability (ROTATE id account)
            (enforce-transfer-policy id account account 0.0)
            (with-read ledger_table (key id account)
              { "guard" := old-guard }

              (enforce-guard old-guard)
              (update ledger_table (key id account)
                { "guard" : new-guard })
              (emit-event (ACCOUNT_GUARD id account new-guard)))))

        (defun transfer:bool
          ( id:string sender:string receiver:string amount:decimal )
          (enforce (!= sender receiver)
            "sender cannot be the receiver of a transfer")
          (enforce-valid-transfer sender receiver (precision id) amount)
          (with-capability (TRANSFER id sender receiver amount)
            (enforce-transfer-policy id sender receiver amount)
            (with-read ledger_table (key id receiver)
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
          ( id:string sender:string receiver:string amount:decimal )
          (bind (get-policy-info id)
            { 'policy := policy:module{kip.token-policy-v1}
            , 'token := token }
            (policy::enforce-transfer token sender (account-guard id sender) receiver amount))
            )

        (defun enforce-collection:bool (name:string)
          (with-read collections_table name
            { 'guard:= collection-guard }
            (enforce-one "Only Admin or Collection creator can create tokens_table"
              [(enforce-guard collection-guard)
               (enforce-guard (keyset-ref-guard "km-pass-ks"))])
          )
        )

        (defun transfer-create:bool
          ( id:string sender:string receiver:string receiver-guard:guard amount:decimal )
          (enforce (!= sender receiver)
            "sender cannot be the receiver of a transfer")
          (enforce-valid-transfer sender receiver (precision id) amount)

          (with-capability (TRANSFER id sender receiver amount)
            (enforce-transfer-policy id sender receiver amount)
            (let
              (
                (sender (debit id sender amount))
                (receiver (credit id receiver receiver-guard amount))
              )
              (emit-event (RECONCILE id amount sender receiver))
            ))
        )

        (defun get-priority-users ()
          (keys nfts-info-by-owner)
        )

        (defun get-tokenid-minted ()
          (keys nfts-info-by-id)
        )

        (defun mint-pass:bool
          ( account:string guard:guard amount:decimal collection:string count:integer )
          (with-capability (MINT-PASS account amount)
          (let ((indx:integer (get-current-index))
                (ts:integer (at 'total-supply (read collections_table collection)))
                )
           (enforce (<=  indx ts) "We are unable to produce additional images as the current supply has been depleted.")
          (let* (
                 (collection-info:object{collection-schema} (get-collection collection))
                 (creator:string (at 'creator collection-info))
                 (policy:module{kip.token-policy-v1} (at 'policy collection-info))
                )
          (let*
                (
                  (pass-holder:[string] (get-priority-users))
                  (random:integer (get-random account))
                  (current-length:integer (get-current-length collection))
                  (index:integer (mod random current-length))
                  (available-tokens:[integer] (at 'tokens-list (read token-record collection)))
                  (sha256:string (int-to-str 64 (at index available-tokens)))
                  (tkn-id:string (hash sha256))
                  (token-id:string (format "{}:{}" [collection tkn-id]))
                  (minted-token:integer (str-to-int 64 sha256))
                  (imageIndex:integer (+ 1 (get-current-index)))
                  (imageUrl:string (format "{}/{}.jpg" ["https://emerald-glamorous-parrot-699.mypinata.cloud/ipfs/QmYhqC7bvAtgEnDZTH38DmQdgd2pQLdmqSkcw5Adj1pXte" imageIndex]))
                  (datum-object
                    {
                     'creator: creator
                    , 'collectionName: collection
                    , "hash":sha256
                    , "tokenId":tkn-id
                    , "imageUrl":imageUrl
                    , "imageIndex":imageIndex
                    })
                  (datum-uri (kip.token-manifest.uri "pact:schema" "free.pass-policy.token-metadata"))
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
                    "tokenId":token-id,
                    "collectionName":collection,
                    "index":imageIndex
                    })

                (update indexTable INDEX
                    { "index":imageIndex })
                (write nfts-info-by-owner account
                   {
                      "owner":account,
                      "tokenId":token-id,
                      "collectionName":collection,
                      "index":imageIndex
                    })
                (update token-record collection {
                  'tokens-list: (filter (!= minted-token) available-tokens),
                  'current-length: (- current-length 1)
                  })

                  (cond
                    (
                      (contains account pass-holder)
                      [(let((bal:integer (get-passbalance account)))
                        (update passbalance_table account
                          { "balance": (+ bal 12) })
                      )]
                     )
                     [ (write passbalance_table account { "balance": 12 }) ]
                  )
                )
            )))
        )

        (defun ids-owned-by (owner:string)
         @doc "All punks owned by someone"
          (select nfts-info-by-id ["tokenId"] (where "owner" (= owner)))
        )

        ; ============================================
        ; ==           FOR TOKEN-ID                 ==
        ; ============================================

        (defun add-tokens
          ( total-supply:integer tokens-list:[string] collectionName:string )
          (require-capability (PRIVATE))
          (enforce (= (length tokens-list) total-supply) "Total-supply and tokens-list length does not match")
          (insert token-record collectionName {
            "current-length": (length tokens-list),
            "tokens-list": (map (str-to-int 64) tokens-list)
            })
        )

        (defun updatetokenlist:bool
          ( tokens-list:[string] collection-name:string )
            (with-capability (IS_ADMIN)
            (let (
              (tkns:[integer](map (str-to-int 64) tokens-list) )
              )

              (with-read token-record collection-name
                { "tokens-list" := tkn-list }

            (let* ((tkn:[integer](+  tkn-list tkns)))

                (update token-record collection-name
                  {
                    "current-length": (+ (length tokens-list) (get-current-length collection-name)),
                    "tokens-list": tkn
                  })

                  (update collections_table collection-name
                    { "total-supply": (length tkn) } )
              )
               )))
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

      (defun get-current-length:integer (collection:string)
        (with-read token-record collection
          { 'current-length:= current-length }
                current-length )
      )

      (defun transfer-passbalance (sender:string receiver:string)
          (let ((passHolder:[string] (get-priority-users)))
             (cond
                (
                  (contains receiver passHolder)
                   [""]
                )
                [ (write nfts-info-by-owner receiver
                  { "owner":receiver,
                    "tokenId":"",
                    "collectionName":"",
                    "index":0
                  }
                  )
                  (write passbalance_table receiver
                    {
                      "balance":0
                      })
                  ]
               )
             (let ((b:integer (get-passbalance sender))
                    (pass-holder:[string] (get-priority-users)))
              (if (<= b 12)
              [
                (update passbalance_table sender
                  {"balance": 0})
                (cond
                  (
                    (contains receiver pass-holder)
                    [(let((bal:integer (get-passbalance receiver)))
                      (update passbalance_table receiver
                      {"balance": (+ bal b)})
                      )]
                    )
                    [
                    (write passbalance_table receiver
                      {"balance": b})
                      ]
                    )
              ]
              [
                (update passbalance_table sender
                {"balance": (- b 12)})

                (cond
                  (
                    (contains receiver pass-holder)
                    [(let((bal:integer (get-passbalance receiver)))
                      (update passbalance_table receiver
                      {"balance": (+ 12 bal)})
                      )]
                    )
                    [ (write passbalance_table receiver {"balance": 12}) ]
                  )
              ]

              )))
            )

;;---------------------------------


        (defun key:string ( id:string account:string )
          @doc "DB key for ledger_table account"
          (format "{}:{}" [id account])
        )

        (defun burn:integer (account:string)
             (let ((b:integer (get-passbalance account)))
               (update passbalance_table account
                 {"balance": (- b 1 )})
             )
         )


        (defun get-policy-info:object{policy-info} (id:string)
          (with-read tokens_table id
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

        (defun debit:object{sender-balance-change}
          ( id:string account:string amount:decimal )

          (require-capability (DEBIT id account))
          (enforce-unit id amount)
          (with-read ledger_table (key id account)
            { "balance" := old-bal }

            (enforce (<= amount old-bal) "Insufficient funds")
            (let ((new-bal (- old-bal amount)))
              (update ledger_table (key id account)
                { "balance" : new-bal }
                )
              {'account: account, 'previous: old-bal, 'current: new-bal}
            ))
        )

        (defun credit:object{receiver-balance-change}
          ( id:string account:string guard:guard amount:decimal )
          @doc "Credit AMOUNT to ACCOUNT balance"

          @model [ (property (> amount 0.0))
                   (property (valid-account account))
                 ]
          (enforce-valid-account account)
          (enforce-unit id amount)

          (with-default-read ledger_table (key id account)
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

            (write ledger_table (key id account)
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
          ( id:string account:string amount:decimal )
          @doc "Credit AMOUNT to ACCOUNT"
          (credit id account (account-guard id account) amount)
        )

        (defun enforce-unit:bool (id:string amount:decimal)
          (let ((p (precision id)))
          (enforce
            (= (floor amount p)
               amount)
            "precision violation"))
        )

        (defun precision:integer (id:string)
          (with-default-read tokens_table id
            {'precision: 0}
            {'precision:= p}
          p)
        )

        (defpact transfer-crosschain:bool
          ( id:string sender:string receiver:string receiver-guard:guard target-chain:string amount:decimal )
          (step (format "{}" [(enforce false "cross chain not supported")]) false))

        (defun get-balance:decimal (id:string account:string)
            (at 'balance (read ledger_table (key id account)))
        )

        (defun details:object{account-details}
          ( id:string account:string )
           (read ledger_table (key id account))
        )

        (defun get-passbalance (account:string)
          (with-read passbalance_table account { 'balance := balance } balance)
        )

        (defun get-collection:object{collection-schema} (name:string)
            "Read Collection"
          (read collections_table name)
        )

        (defun get-all-collection()
          (keys collections_table )
        )

        (defun get-manifest:object{manifest} (id:string)
          (at 'manifest (read tokens_table id)))

        (defun get-token-uri:object{kip.token-manifest.mf-uri} (id:string)
              (at 'uri (get-manifest id)))

        (defun sale-active:bool (timeout:integer)
              @doc "Sale is active until TIMEOUT block height."
              (< (at 'block-height (chain-data)) timeout)
        )

        (defun sale-account:string ()
            (create-principal (create-pact-guard "SALE")))

        (defcap SALE_PRIVATE:bool (sale-id:string) true)

        (defcap BUY:bool
           (id:string seller:string buyer:string amount:decimal timeout:integer sale-id:string)
            @doc "Completes sale OFFER to BUYER."
            @managed
            (enforce (sale-active timeout) "BUY: expired")
            (compose-capability (DEBIT id (sale-account)))
            (compose-capability (CREDIT id buyer))
            (compose-capability (SALE_PRIVATE sale-id))
          )

        (defcap SALE:bool
            (id:string seller:string amount:decimal timeout:integer sale-id:string)
              @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until TIMEOUT block height."
              @event
              (enforce (> amount 0.0) "Amount must be positive")
              (compose-capability (OFFER id seller amount timeout))
              (compose-capability (SALE_PRIVATE sale-id))
          )

        (defcap OFFER:bool
            (id:string seller:string amount:decimal timeout:integer)
              @doc "Managed cap for SELLER offering AMOUNT of token ID until TIMEOUT."
              @managed
              (enforce (sale-active timeout) "SALE: invalid timeout")
              (compose-capability (DEBIT id seller))
              (compose-capability (CREDIT id (sale-account)))
          )

          (defcap WITHDRAW:bool
            (id:string seller:string amount:decimal timeout:integer sale-id:string)
            @doc "Withdraws offer SALE from SELLER of AMOUNT of token ID after timeout."
            @event
            (enforce (not (sale-active timeout)) "WITHDRAW: still active")
            (compose-capability (DEBIT id (sale-account)))
            (compose-capability (CREDIT id seller))
            (compose-capability (SALE_PRIVATE sale-id))
          )

            (defpact sale:bool
              ( id:string seller:string amount:decimal timeout:integer)
              (step-with-rollback
                (with-capability (SALE id seller amount timeout (pact-id))
                  (offer id seller amount))
                (with-capability (WITHDRAW id seller amount timeout (pact-id))
                  (withdraw id seller amount))
              )
              (step
                (let ( (buyer:string (read-msg "buyer"))
                       (buyer-guard:guard (read-msg "buyer-guard")) )
                  (with-capability (BUY id seller buyer amount timeout (pact-id))
                    (buy id seller buyer buyer-guard amount (pact-id)))))
            )

            (defun offer:bool
              ( id:string seller:string amount:decimal )
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
              ( id:string seller:string amount:decimal )
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
              ( id:string seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string )
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

            (defun get-datum (tokenId:string)
              (let* ((data (at 'data (marmalade.ledger.get-manifest tokenId)))
                      (datum (at 'datum (at 0 data))) )
                datum))

            (defun get-imageUrl (tokenId:string)
                (let* ((data (at 'data (marmalade.ledger.get-manifest tokenId)))
                        (datum (at 'datum (at 0 data)))
                        (imageUrl:string (at 'imageUrl datum)))
                    imageUrl))

            (defun init()
                (insert indexTable INDEX
                    { "index":0 } )
            )

            (defun resetIndex()
               (update indexTable INDEX
                    { "index":0 }
                )
              )

            (defconst INDEX:string "index")

  )

      ; (create-table ledger_table)
      ; (create-table tokens_table)
      ; (create-table collections_table)
      ; (create-table passbalance_table)
      ; (create-table nfts-info-by-id)
      ; (create-table nfts-info-by-owner)
      ; (create-table indexTable)
      ; (create-table token-record)
      ; (free.km-pass-contract.init)

