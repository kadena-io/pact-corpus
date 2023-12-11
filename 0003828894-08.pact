(module dbcooper COOPER

  (use util.fungible-util)
  (use kip.token-manifest)
  (implements free.db-cooper-fungible)
  (implements free.marketplace-v1)
  (use free.db-cooper-fungible [account-details sender-balance-change receiver-balance-change])
  (use free.db-cooper-policy-v1)

   ;;
   ;;Tables/Schemass
   ;;

   (deftable ledger-table:{account-details})

   (defschema token-schema
    id:string
    manifest:object{manifest}
    precision:integer
    supply:decimal
    policy:module{free.db-cooper-policy-v1}
   )

  (deftable tokens-table:{token-schema})

  (defschema collection-schema
    total-supply:integer
    name:string
    symbol:string
    creator:string
    policy:module{free.db-cooper-policy-v1}
    guard:guard
    token:object{free.db-cooper-policy-v1.token-info}
   )

  (deftable collections-table:{collection-schema})

  (defschema policy-info
    policy:module{free.db-cooper-policy-v1}
    token:object{free.db-cooper-policy-v1.token-info}
  )

  (defschema nft-main-schema
      @doc "Stores core information about each nft"
      id:string
      date_minted:time
      owner:string
      price:decimal
      for_sale:string
      updated_at:time
  )
  (defschema marketplace-schema
      @doc "Schema for marketplace information, ID is the nft id"
      id:string
      for-sale:bool
      price:decimal
      updated-at:time
      owner:string
      on_auction:bool
  )
  (deftable marketplace:{marketplace-schema})
  (deftable nfts:{nft-main-schema})

  ;;
  ;; Capabilities
  ;;

  (defcap COOPER ()
    (enforce-guard (keyset-ref-guard "free.kryptomerch-dbcooper-ks")))


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

    (defcap TOKEN:bool (id:string precision:integer supply:decimal policy:module{free.db-cooper-policy-v1})
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

    (defcap OWNER (owner:string id:string guard:guard)
        @doc "Enforces that an account owns a lazy ape"
            (compose-capability (ACCOUNT_GUARD_MP owner guard))
    )

    (defcap ACCOUNT_GUARD:bool (id:string account:string guard:guard)
      @doc " Emitted when ACCOUNT guard is updated."
      @event
      true
    )

    (defcap ACCOUNT_GUARD_MP(account:string g:guard) ; Used for admin functions
        @doc "Verifies account meets format and belongs to caller"
        (let
          (
            (grd(at "guard" (coin.details account)))
            )
        (enforce (= grd g) "guard doesnot match")
        )
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

    (defcap MINT (id:string account:string amount:decimal)
      @managed ;; one-shot for a given amount
      (enforce (< 0.0 amount) "Amount must be positive")
      (compose-capability (CREDIT id account))
      (compose-capability (UPDATE_SUPPLY))
    )

    (defcap MINT-COOPER (account:string amount:decimal)
      @managed ;; one-shot for a given amount
      (compose-capability (PRIVATE))
      (compose-capability (CREDIT-COOPER account))
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

    (defcap CREDIT-COOPER (receiver:string) true)

    (defcap MINTED (token-id:string account:string)
      @event
      true
    )

    (defcap COLLECTION ( name:string
            symbol:string
            total-supply:integer
            policy:module{free.db-cooper-policy-v1}
            guard:guard
            creator:string
            token:object{token-info}
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
          policy:module{free.db-cooper-policy-v1}
          collection-name:string
        )
        (enforce-verify-manifest manifest)
        ;(enforce-collection collection-name)
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

      (defun create-collection:bool
         ( name:string
           symbol:string
           total-supply:integer
           policy:module{free.db-cooper-policy-v1}
           guard:guard
           creator:string
           token:object{token-info}
         )
        (with-capability (COOPER)
        (insert collections-table name
          {
            "name": name,
            "symbol": symbol,
            "total-supply": total-supply,
            "policy": policy,
            "guard": guard,
            "creator":creator,
            "token": token
          }
        )
        (emit-event (COLLECTION name symbol total-supply policy guard creator token))
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
          { 'policy := policy:module{free.db-cooper-policy-v1}
          , 'token := token }
          (policy::enforce-transfer token sender (account-guard id sender) receiver amount))
      )

      (defun enforce-collection:bool (name:string)
        (with-read collections-table name
          { 'guard:= collection-guard }
          (enforce-one "Only Admin or Collection creator can create tokens-table"
            [(enforce-guard collection-guard)
             (enforce-guard (keyset-ref-guard "kryptomerch-dbcooper-ks"))])
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

      (defun mint:bool
        ( id:string
          account:string
          guard:guard
          amount:decimal
        )
        (with-capability (MINT id account amount)
          (bind (get-policy-info id)
            { 'policy := policy:module{free.db-cooper-policy-v1}
            , 'token := token }
            (policy::enforce-mint token account guard amount))
          (let
            (
              (receiver (credit id account guard amount))
              (sender:object{sender-balance-change}
                {'account: "", 'previous: 0.0, 'current: 0.0})
            )
            (emit-event (RECONCILE id amount sender receiver))
            (update-supply id amount)
          ))
      )

      (defun mint-cooper:bool
        ( account:string
          guard:guard
          amount:decimal
          collection:string
          count:integer
        )
        (require-capability (PRIVATE))
          (let* ( (collection-info:object{collection-schema} (get-collection collection))
                  (policy:module{free.db-cooper-policy-v1} (at 'policy collection-info))
                  (token:object{free.db-cooper-policy-v1.token-info} (at 'token collection-info))
                  (id:string (policy::enforce-mint token account guard amount))
            )
            (let
              (
                (receiver (credit id account guard amount))
                (sender:object{sender-balance-change}
                  {'account: "", 'previous: 0.0, 'current: 0.0})
              )
              (emit-event (RECONCILE id amount sender receiver))
              (insert nfts id
                {
                  "id":id,
                  "date_minted": (at "block-time" (chain-data)),
                  "owner":account,
                  "for_sale": "false",
                  "price":0.0,
                  "updated_at": (at "block-time" (chain-data))

                  })

            )
            )
      )

      (defun mint-bulk-cooper
        ( account:string
          guard:guard
          amount:decimal
          collection:string
          count:integer
        )
        (with-capability (MINT-COOPER account amount)
        (let ( (policy:module{free.db-cooper-policy-v1} (at 'policy (get-collection collection)))
               (account-g:guard (at 'guard (coin.details account))))
          (enforce (= guard account-g) "Guards doesn't match")
          (policy::enforce-bulk-mint account count)
          (map (mint-cooper account guard amount collection) (make-list count 1))
        )
        )
      )

      (defun burn:bool
        ( id:string
          account:string
          amount:decimal
        )
        (with-capability (BURN id account amount)
          (bind (get-policy-info id)
            { 'policy := policy:module{free.db-cooper-policy-v1}
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
          { 'policy := policy:module{free.db-cooper-policy-v1}
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
          { 'policy := policy:module{free.db-cooper-policy-v1}
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
        { 'policy := policy:module{free.db-cooper-policy-v1}
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


    ;;marketplace features

    (defun transfer-nft:bool (owner:string receiver:string id:string guard:guard)
      @doc "Transfer an NFT to an account."
          (with-capability (OWNER owner id guard)
            (write marketplace id {
                "id": id,
                "for-sale": false,
                "price": -1.0,
                "updated-at": (at "block-time" (chain-data)),
                "owner": receiver,
                "on_auction":true
                  }
              )
                  (update nfts id {"owner": receiver})
              )
      )

        (defun put-id-for-sale:bool(owner:string id:string price:decimal guard:guard on_auction:bool)
          @doc "Puts an NFT up for sale"
            (with-capability (OWNER owner id guard)
              (enforce (> price 0.0) "Price must be positive")
                (let*
                    (
                      (owner (get-nft-owner id ))
                    )
                    (write marketplace id {
                          "id": id,
                          "for-sale": true,
                          "price": price,
                          "updated-at": (at "block-time" (chain-data)),
                          "owner": owner,
                          "on_auction":on_auction
                      })
                      (emit-event (PUT_ON_SALE id owner price))
                  )
              )
          )

          (defun remove-id-from-sale:bool (owner:string id:string guard:guard )
              @doc "Removes an NFT from sale"
              (with-capability (OWNER owner id guard)
                  (let*
                      (
                          (owner (get-nft-owner id ))
                      )
                      (write marketplace id {
                          "id": id,
                          "for-sale": false,
                          "price": -1.0,
                          "updated-at": (at "block-time" (chain-data)),
                          "owner": owner,
                          "on_auction":true
                      })
                      (emit-event (REMOVED_FROM_SALE id owner))
                  )
              )
          )

          (defun enforce-id-on-sale (id:string)
              @doc "Enforces the NFT for the ID is on sale"
              (let*
                  (
                      (current-owner (get-nft-owner id ))
                      (marketplace-data (read marketplace id ["owner", "for-sale"]))
                  )
                  (enforce (= current-owner (at "owner" marketplace-data))
                      "The person who is the current NFT owner isn't the one that put it on sale")
                  (enforce (= true (at "for-sale" marketplace-data))
                      "The nft is not listed for sale")
              )
          )

          (defun get-nft-owner:string (id:string)
            (with-read nfts id
              {
                "owner":=owner
              }
              owner
            )
          )

              (defun buy-id-on-sale:bool (id:string curr-user:string user_guard:guard)
                  @doc "Buys an NFT that was put up for sale"
                  (with-read marketplace id
                    {
                      "on_auction":=on_auction
                    }
                  (enforce (!= true on_auction) "Nft on auction on marketplace")
                  (with-capability (ACCOUNT_GUARD_MP curr-user user_guard)
                      (enforce-id-on-sale id)
                      (let*
                          (
                              (original-owner (get-nft-owner id ))
                              (price (at "price" (read marketplace id ["price"])))
                          )
                          (coin.transfer curr-user original-owner price)
                          (write marketplace id {
                              "id": id,
                              "for-sale": false,
                              "price": -1.0,  ; Invalid price so NFT can't be sold
                              "updated-at": (at "block-time" (chain-data)),
                              "owner": curr-user,
                              "on_auction":true
                          })
                          (update nfts id
                            {
                              "owner":curr-user
                              })

                          (emit-event (BOUGHT id curr-user original-owner price))
                      )
                  )
                  )
              )

              (defun all-ids ()
                  @doc "Returns all the ids"
                  (keys nfts)
              )

              (defun get-all-on-sale ()
                  @doc "Returns all items on sale"
                  (select marketplace ["id", "price", "updated-at"] (where "for-sale" (= true)))
              )

              (defun ids-owned-by (owner:string)
                  @doc "All punks owned by someone"
                  (select nfts ["id"] (where "owner" (= owner)))
              )

)





  
