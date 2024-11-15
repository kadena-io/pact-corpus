(module priority-pass PASS

    (use util.fungible-util)
    (use util.guards)
    (use kip.token-manifest)
    (use free.kryptomerch-v1)
    (implements free.kryptomerch-fungible)
    (use free.kryptomerch-fungible [account-details sender-balance-change receiver-balance-change])

    (defcap PASS ()
      (enforce-guard (keyset-ref-guard "free.kryptomerch-pass-admin-ks")))

      (deftable ledger_table:{account-details})

      (defschema passbalance
        @doc "schema for holding the balance of a pass"

          balance:integer
      )
      (deftable pbalance_table:{passbalance})

      (defschema token-schema
        id:string
        manifest:object{manifest}
        precision:integer
        supply:decimal
        policy:module{free.kryptomerch-v1}
     )

     (deftable tokens_table:{token-schema})

     (defschema policy-info
       policy:module{free.kryptomerch-v1}
       token:object{free.kryptomerch-v1.token-info}
      )
     (defschema collection-schema
       total-supply:integer
       name:string
       symbol:string
       creator:string
       policy:module{free.kryptomerch-v1}
       guard:guard
       token:object{free.kryptomerch-v1.token-info}
     )

     (deftable collections_table:{collection-schema})

     ;;
     ;; poly-fungible-v2 caps
     ;;

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

     (defcap TOKEN:bool (id:string precision:integer supply:decimal policy:module{free.kryptomerch-v1})
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
         (with-read ledger_table (key id account) { 'guard := guard } guard)
       )

       (defcap CREDIT (id:string receiver:string) true)


       (defcap MINT (id:string account:string amount:decimal)
         @managed ;; one-shot for a given amount
         (enforce (< 0.0 amount) "Amount must be positive")
         (compose-capability (CREDIT id account))
       )

       (defcap MINT-PASS (account:string amount:decimal)
         @managed ;; one-shot for a given amount
         (compose-capability (PRIVATE))
         (compose-capability (CREDIT-PASS account))
       )

       (defcap PRIVATE ()
         true
       )


        (defcap CREDIT-PASS (receiver:string) true)

        (defcap MINTED (token-id:string account:string)
          @event
          true
        )

        (defcap COLLECTION ( name:string
          symbol:string
          total-supply:integer
          policy:module{free.kryptomerch-v1}
          guard:guard
          creator:string
          token:object{token-info}
        )
          @event
          true
        )

        (defun ledger-guard:guard ()
          @doc "Ledger module guard for policies to be able to validate access to policy operations."
          (create-module-guard "ledger_table-guard")
        )

        (defun create-account:bool
          ( id:string
            account:string
            guard:guard
          )
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

        (defun total-supply:decimal (id:string)
          (with-default-read tokens_table id
            { 'supply : 0.0 }
            { 'supply := s }
            s)
        )

        (defun create-token:bool
          ( id:string
            precision:integer
            manifest:object{manifest}
            policy:module{free.kryptomerch-v1}
            collection-name:string
          )
          (enforce-verify-manifest manifest)
          ;(enforce-collection collection-name)
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
         ( name:string
           symbol:string
           total-supply:integer
           policy:module{free.kryptomerch-v1}
           guard:guard
           creator:string
           token:object{token-info}
         )
        (insert collections_table name
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
          ( id:string
            sender:string
            receiver:string
            amount:decimal
          )
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
          ( id:string
            sender:string
            receiver:string
            amount:decimal
          )
          (bind (get-policy-info id)
            { 'policy := policy:module{free.kryptomerch-v1}
            , 'token := token }
            (policy::enforce-transfer token sender (account-guard id sender) receiver amount))
            )

        (defun enforce-collection:bool (name:string)
          (with-read collections_table name
            { 'guard:= collection-guard }
            (enforce-one "Only Admin or Collection creator can create tokens_table"
              [(enforce-guard collection-guard)
               (enforce-guard (keyset-ref-guard "kryptomerch-pass-admin-ks"))])
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
            (enforce-transfer-policy id sender receiver amount)
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
              { 'policy := policy:module{free.kryptomerch-v1}
              , 'token := token }
              (policy::enforce-mint token account guard amount))
            (let
              (
                (receiver (credit id account guard amount))
                (sender:object{sender-balance-change}
                  {'account: "", 'previous: 0.0, 'current: 0.0})
              )
              (emit-event (RECONCILE id amount sender receiver))
            ;  (update-supply id amount)
            ))
        )

        (defun mint-pass:bool
          ( account:string
            guard:guard
            amount:decimal
            collection:string
            count:integer
          )
          (require-capability (PRIVATE))
            (let* ( (collection-info:object{collection-schema} (get-collection collection))
                    (policy:module{free.kryptomerch-v1} (at 'policy collection-info))
                    (token:object{free.kryptomerch-v1.token-info} (at 'token collection-info))
                    (id:string (policy::enforce-mint token account guard amount))
              )
              (let
                (
                  (receiver (credit id account guard amount))
                  (sender:object{sender-balance-change}
                    {'account: "", 'previous: 0.0, 'current: 0.0})

                )
                (emit-event (RECONCILE id amount sender receiver))
                ; (update-supply id amount)

                (write pbalance_table account
                  {
                    "balance": 12
                  })
              )
              )
        )


        (defun mint-bulk-pass
          ( account:string
            guard:guard
            amount:decimal
            collection:string
            count:integer
          )
          (with-capability (MINT-PASS account amount)
          (let ( (policy:module{free.kryptomerch-v1} (at 'policy (get-collection collection)))
                 (account-g:guard (at 'guard (coin.details account))))
            (enforce (= guard account-g) "Guards doesn't match")
            (policy::enforce-bulk-mint account count)
            (map (mint-pass account guard amount collection) (make-list count 1))
          )
          )
        )

        (defun key:string ( id:string account:string )
          @doc "DB key for ledger_table account"
          (format "{}:{}" [id account])
        )

        (defun burn:integer (account:string)
             (let ((b:integer (get-passbalance account)))
               (update pbalance_table account
                 {"balance": (- b 1 )})
             )
         )

        (defun get-policy-info:object{policy-info} (id:string)
          (with-read tokens_table id
            { 'policy := policy:module{free.kryptomerch-v1}
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
          ( id:string
            account:string
            amount:decimal
          )

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
          ( id:string
            account:string
            amount:decimal
          )
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
          ( id:string
            sender:string
            receiver:string
            receiver-guard:guard
            target-chain:string
            amount:decimal )
          (step (format "{}" [(enforce false "cross chain not supported")]) false))

          (defun get-balance:decimal (id:string account:string)
            (at 'balance (read ledger_table (key id account)))
          )

          (defun details:object{account-details}
            ( id:string account:string )
            (read ledger_table (key id account))
          )

          (defun get-passbalance (account:string)
            (with-read pbalance_table account { 'balance := balance } balance)
          )

          (defun get-collection:object{collection-schema} (name:string)
            "Read Collection"
            (read collections_table name)
          )

          (defun get-manifest:object{manifest} (id:string)
            (at 'manifest (read tokens_table id)))

            (defun get-token-uri:object{kip.token-manifest.mf-uri} (id:string)
              (at 'uri (get-manifest id))
            )

  )

      
