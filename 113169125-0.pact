(module cETH GOVERNANCE
;; Don't forget to implement cancel request...
  @doc "Fungible token for Wrapped ETH"
  @model [

    ;; conserves-mass
    (property
      (= (column-delta ledger 'balance ) 0.0)
      { 'except:
        [ burn                 ;; burn-role-guard
        , mint                 ;; mint-role-guard
        , debit                ;; PRIVATE
        , credit               ;; PRIVATE
        , credit-account       ;; PRIVATE
        , transfer-crosschain  ;; xchain decomposition
        , xchain-send          ;; PRIVATE
        , xchain-receive       ;; PRIVATE
        ] } )

    ;; role-write-guard
    (property
      (forall (k:string)
        (when (row-written roles k)
          (row-enforced roles 'guard "module-admin")))
      { 'except:
        [ init-roles            ;; install only
          transfer-crosschain   ;; xchain decomposition
      ] } )

    ;; ledger-write-guard
    (property
      (forall (k:string)
        (when (row-written ledger k)
          (row-enforced ledger 'guard k)))
      { 'except:
        [ mint                 ;; mint-role-guard
          burn                 ;; burn-role-guard
          revoke               ;; revoke-role-guard
          manage-restriction   ;; restrict-role-guard
          create-account       ;; insert only
          transfer             ;; account-guard-enforced sender
          transfer-create      ;; account-guard-enforced sender
          transfer-crosschain  ;; xchain decomposition
          debit                ;; PRIVATE
          credit               ;; PRIVATE
          credit-account       ;; PRIVATE
          xchain-receive       ;; PRIVATE
        ]
      })

    ;; burn-role-guard
    (property (row-enforced roles 'guard "burner")
      { 'only: [burn] })
    ;; mint-role-guard
    (property (row-enforced roles 'guard "minter")
      { 'only: [mint] })
    ;; revoke-role-guard
    (property (row-enforced roles 'guard "revoker")
      { 'only: [revoke] })
    ;; restrict-role-guard
    (property (row-enforced roles 'guard "restrict")
      { 'only: [manage-restriction] })

    (defproperty account-guard-enforced (account:string)
      (when (row-written ledger account)
        (row-enforced ledger 'guard account)))

  ]

  (implements fungible-v2)
  (implements free.wrapped-token-adapt-v1)
  (use free.wrapped-token-adapt-v1
    [ ROLE_MODULE_ADMIN ROLE_BURNER ROLE_MINTER
      ROLE_REVOKER ROLE_RESTRICT ])
  (use free.wrapped-util-adapt01)

  ;;
  ;; tables/schemas
  ;;

  (defschema role-schema
    guard:guard)

  (deftable roles:{role-schema})

  (defschema account-schema
    balance:decimal
    guard:guard
    restricted:bool
    oracle: bool
  )

  (deftable ledger:{account-schema})

  (defschema txnDetails-schema
     saddress: string
     txnId: string
     amount: decimal
     exFee: decimal
     myfee: decimal
  )

  (defschema request-schema
     kaddress: string
     oaddress: string
     key: string
     amount: decimal
     actual: decimal
     eaddress: string
     direction: string
     createdAt: time
     actualAt: time
     actualed: bool
     mbAt: time
     mbd: bool
     cancelled: bool
     txnDetails:[object{txnDetails-schema}]
  )    

  (deftable requests3:{request-schema})

  (defschema eaddress-schema
     eaddress: string
     oaddress: string ;;oracle address
     busy: bool ;;External account is busy while serving a request
     balance: decimal ;;external account balance
  )    

  (deftable eaddresses1:{eaddress-schema})

  ;;
  ;; capabilities
  ;;

  (defcap GOVERNANCE ()
    @doc "Module admin capability"
    (compose-capability (ROLE ROLE_MODULE_ADMIN))
  )

  (defcap ROLE (role:string)
    @doc "Composable capability for role enforcement"
    (enforce-guard (at 'guard (read roles role)))
  )

  (defcap UPDATE_ROLE:bool (role:string)
    @doc "Managed cap/event for updating role"
    @managed
    (compose-capability (ROLE ROLE_MODULE_ADMIN))
  )

  (defcap ACCOUNT_GUARD (account:string)
    (enforce-guard (at 'guard (read ledger account)))
  )

  (defcap DEBIT (sender:string)
    @doc "Capability for managing debiting operations"
    (enforce-valid-account sender)
  )

  (defcap XCHAIN () "Private cap for crosschain" true)

  (defcap XCHAIN_DEBIT (sender:string)
    @doc "Capability for managing debiting operations"
    (compose-capability (DEBIT sender))
    (compose-capability (ACCOUNT_GUARD sender))
    (compose-capability (UNRESTRICTED sender))
  )

  (defcap CREDIT (receiver:string)
    @doc "Capability for managing crediting operations"
    (enforce-valid-account receiver)
    (compose-capability (UNRESTRICTED receiver))
  )

  (defcap MANAGE_RESTRICTION:bool (account:string restricted:bool)
    @doc "Managed cap/event for managing account restriction"
    @managed
    (compose-capability (ROLE ROLE_RESTRICT))
  )

  (defcap REVOKE:bool (account:string revoke-account:string amount:decimal)
    @doc "Revocation administrative action"
    @managed
    (enforce-valid-transfer account revoke-account (precision) amount)
    (compose-capability (ROLE ROLE_REVOKER))
    (compose-capability (DEBIT account))
    (compose-capability (CREDIT revoke-account))
    (enforce-restriction account true) ;; account must be restricted to revoke
  )

  (defcap UNRESTRICTED (account:string)
    @doc "Enforce account not restricted"
    (enforce-restriction account false)
  )

  (defcap ROTATE (account:string)
    @doc "Managed cap/event for user account rotation"
    @managed
    (compose-capability (ACCOUNT_GUARD account))
  )

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-valid-transfer sender receiver (precision) amount)
    (compose-capability (DEBIT sender))
    (compose-capability (ACCOUNT_GUARD sender))
    (compose-capability (CREDIT receiver))
    (compose-capability (UNRESTRICTED sender))
  )

  (defcap INTERNAL:bool ()
    true
  )    

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    (enforce-transfer-manager managed requested)
  )

  (defcap MINT:bool (recipient:string amount:decimal)
    @doc "Managed cap/event for minting"
    @managed
    (compose-capability (ROLE ROLE_MINTER))
    (compose-capability (CREDIT recipient))
    (enforce-valid-amount (precision) amount)
  )

  (defcap BURN:bool (sender:string amount:decimal)
    @doc "Managed cap/event for burning"
    @managed
    (compose-capability (ROLE ROLE_BURNER))
    (compose-capability (DEBIT sender))
    (enforce-valid-amount (precision) amount)
  )

  ;;
  ;; wrapped-token-v1 functionality
  ;;
  
  (defun init-module-admin-role ()
    (insert roles ROLE_MODULE_ADMIN
      { 'guard: (read-keyset ROLE_MODULE_ADMIN) })
  )

  (defun init-roles ()
    (insert roles ROLE_BURNER
      { 'guard: (read-keyset ROLE_BURNER) })
    (insert roles ROLE_MINTER
      { 'guard: (read-keyset ROLE_MINTER) })
    (insert roles ROLE_REVOKER
      { 'guard: (read-keyset ROLE_REVOKER) })
    (insert roles ROLE_RESTRICT
      { 'guard: (read-keyset ROLE_RESTRICT) })
  )

  (defun update-role:string (role:string guard:guard)
    "Update role, guarded by UPDATE_ROLE"
    (with-capability (UPDATE_ROLE role)
      (update roles role { 'guard: guard }))
  )

  (defun update-oracle:string (address:string)
    "Update oracle role, guarded by GOVERNANCE"
    (with-capability (GOVERNANCE)
      (update ledger address { 'oracle: true }))
  )

  (defun get-role:object{role-schema} (role:string)
    (read roles role))

  (defun manage-restriction:string
    ( account:string
      restricted:bool
    )
    (with-capability (MANAGE_RESTRICTION account restricted)
      (update ledger account { 'restricted: restricted }))
  )

  (defun is-restricted:bool (account:string)
    (with-default-read ledger account
      { 'restricted: false } { 'restricted:= r }
      r)
  )

  (defun enforce-restriction (account:string restriction:bool)
    "Enforce ACCOUNT restriction is RESTRICTION"
    (let ((r (is-restricted account)))
      (enforce (= r restriction)
        (if r "Account Restricted" "Account Unrestricted")))
  )

  (defun revoke:string
    ( account:string
      revoke-account:string
      amount:decimal
    )
    @doc "Administrative revocation action"
    (with-capability (REVOKE account revoke-account amount)
      (emit-event (TRANSFER account revoke-account amount))
      (debit account amount)
      (credit-account revoke-account amount)
    )
  )

  (defun mint-for-request (key)
    (with-read requests3 key
        {"actualed":= actualed,
         "kaddress":= kaddress,
         "eaddress":= eaddress,
         "actual":= actual,
         "direction":= direction,
         "mbd":= mbd,
         "cancelled":= cancelled}
        (enforce (= actualed true) "Not even actualed")
        (enforce (= cancelled false) "Request cancelled")
        (enforce (> actual 0.0) "Can't do zero amount")
        (enforce (= mbd false) "Already minted")
        (enforce (= direction "in") "Wrong direction")
        (with-read ledger kaddress
            {"guard":= nguard}
            (with-capability (MINT kaddress actual)
              (credit kaddress nguard actual)
              (emit-event (TRANSFER "" kaddress actual))
            )
            (update requests3 key 
                {"mbAt": (get-time),
                 "mbd": true
                }
            )
            (update eaddresses1 eaddress 
                {"busy": false}
            )
        )
    )
  )

  (defun mint:string
    ( recipient:string
      recipient-guard:guard
      amount:decimal
    )
    @doc "Administrative mint action..."
    (require-capability (INTERNAL))
    (with-capability (MINT recipient amount)
      (credit recipient recipient-guard amount)
      (emit-event (TRANSFER "" recipient amount))
      "Mint succeeded")
  )

  (defun burn-for-request (key)
    (with-read requests3 key
        {"actualed":= actualed,
         "kaddress":= kaddress,
         "amount":= amount,
         "direction":= direction,
         "mbd":= mbd,
         "cancelled":= cancelled}
        ; (enforce (= actualed true) "Not even actualed") "This comes after
        (enforce (> amount 0.0) "Can't do zero amount")
        (enforce (= mbd false) "Already burned")
        (enforce (= cancelled false) "Cancelled")
        (enforce (= direction "out") "Wrong direction")
        (with-read ledger kaddress
            {"guard":= nguard}
            (with-capability (BURN kaddress amount)
              (debit kaddress amount)
              (emit-event (TRANSFER kaddress "" amount)))
            (update requests3 key 
                {"mbAt": (get-time),
                 "mbd": true
                }
            )
        )
    )
  )

  (defun burn:string
    ( sender:string
      amount:decimal
    )
    @doc "Administrative burn action"
    (require-capability (INTERNAL))
    (with-capability (BURN sender amount)
      (debit sender amount)
      (emit-event (TRANSFER sender "" amount))
      "Burn succeeded")
  )

  ;;
  ;; fungible-v2 functionality
  ;;

  (defun update-request-eaddress 
    (key
     eaddress
    )
    (enforce-guard (at 'guard (read ledger (at "sender" (chain-data)) ['guard])))
    (with-read ledger (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce oracle "Must be an oracle")
        (with-read requests3 key
          {"direction":= direction,
           "oaddress":= oaddress,
           "eaddress":= eaddressDb,
           "actualed":= actualed,
           "mbd":= mbd,
           "cancelled":= cancelled}
            (enforce (= direction "in") "Must be inbound")
            (enforce (= oaddress "") "Cannot have an oracle yet")
            (enforce (= eaddressDb "") "Cannot have an an assigned eaddress")
            (enforce (= actualed false) "Cannot be actualed")
            (enforce (= mbd false) "Cannot be minted")
            (enforce (= cancelled false) "Cancelled")
            (with-read eaddresses1 eaddress ;;External address must already exist
                {"oaddress":= oaddressDb,
                 "busy":= busy}
                (enforce (= busy false) "External account busy")
                (enforce (= oaddressDb (at "sender" (chain-data))) "Not your eaddress")
                (update eaddresses1 eaddress 
                    {"busy": true} ;; eaddress busy until request concludes
                )
                (update requests3 key 
                    {
                    "oaddress": (at "sender" (chain-data)), 
                    "eaddress": eaddress
                    }
                )
            )
        )
    )
  )

  (defun create-eaddress (
    eaddress
  )
    (enforce-guard (at 'guard (read ledger (at "sender" (chain-data)) ['guard])))
    (with-read ledger (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce oracle "Must be an oracle")
        (insert eaddresses1 eaddress
            {"eaddress": eaddress,
             "oaddress": (at "sender" (chain-data)),
             "busy": false,
             "balance": 0.0}
        )
    )
  )
  
  (defun get-my-eaddresses (
  )
    (select eaddresses1 (where 'oaddress (= (at "sender" (chain-data)))))
  )
  
  (defun create-request-in (
    amount ; need to know possible exposure of oracle    
    )
    (enforce-guard (at 'guard (read ledger (at "sender" (chain-data)) ['guard])))
    (enforce (>= amount 0.001) "Can't do too small")
    (insert requests3 (compound-key2 (at "sender" (chain-data)) (get-time-string)) 
        {"kaddress": (at "sender" (chain-data)),
        "oaddress": "", 
        "key": (compound-key2 (at "sender" (chain-data)) (get-time-string)),
        "amount": amount,
        "actual": 0.0,
        "eaddress": "",
        "direction": "in",
        "createdAt": (get-time),
        "actualAt": (get-epoch),
        "actualed": false,
        "mbAt": (get-epoch),
        "mbd": false,
        "cancelled": false,
        "txnDetails": []
        }
    )
  )

  (defun cancel-request 
    (
      key
    )
    (enforce-guard (at 'guard (read ledger (at "sender" (chain-data)) ['guard])))
    (with-read ledger (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce oracle "Must be an oracle")
        (with-read requests3 key
            {"actualed":= actualed,
             "direction":= direction,
             "oaddress":= oaddress,
             "eaddress":= eaddress,
             "mbd":= mbd,
             "cancelled":= cancelled}
            (enforce (= actualed false) "Already actualed")
            (enforce (= cancelled false) "Already cancelled")
            (enforce (= mbd false) "Already minted/burned")
            (enforce (= oaddress (at "sender" (chain-data))) "Not its oracle")
            (update requests3 key 
                {
                "cancelled": true
                }
            )
        )
    )
  )

  (defun create-request-out 
    (
    eaddress
    amount:decimal
    )
    (enforce-guard (at 'guard (read ledger (at "sender" (chain-data)) ['guard])))
    (enforce (>= amount 0.001) "Can't do too small")
    (let ((balance (get-balance (at "sender" (chain-data)))))
        (enforce (< amount balance) "Not enough funds")
        (insert requests3 (compound-key2 (at "sender" (chain-data)) (get-time-string)) 
            {"kaddress": (at "sender" (chain-data)),
            "oaddress": "", 
            "key": (compound-key2 (at "sender" (chain-data)) (get-time-string)),
            "amount": amount,
            "actual": 0.0,
            "eaddress": "",
            "direction": "out",
            "createdAt": (get-time),
            "actualAt": (get-epoch),
            "actualed": false,
            "mbAt": (get-epoch),
            "mbd": false,
            "cancelled": false,
            "txnDetails": []
            }
        )
    )
  )

  (defun update-request-actual-in 
    ( key 
      actual
      txnDetails:[object:{txnDetails-schema}]
    )
    (enforce-guard (at 'guard (read ledger (at "sender" (chain-data)) ['guard])))
    (enforce (>= actual 0.001) "Can't do too small")
    (with-read ledger (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce oracle "Must be an oracle")
        (with-read requests3 key
            {"actualed":= actualed,
             "direction":= direction,
             "eaddress":= eaddress,
             "mbd":= mbd,
             "cancelled":= cancelled}
            (enforce (= actualed false) "Already actualed")
            (enforce (= cancelled false) "Already cancelled")
            (enforce (= direction "in") "Wrong direction")
            (update requests3 key 
                {
                ; "oaddress": (at "sender" (chain-data)),
                "actual": actual,
                "actualAt": (get-time),
                "actualed": true,
                "txnDetails": txnDetails
                }
            )
            (with-read eaddresses1 eaddress
              {"balance":= balance}
               (update  eaddresses1 eaddress
                    {"balance": (+ balance actual)}
               )
            )
        )
    )
  )

  (defun update-request-actual-out
    ( key 
      actual
      txnDetails:[object:{txnDetails-schema}]
    )
    (enforce-guard (at 'guard (read ledger (at "sender" (chain-data)) ['guard])))
    (enforce (>= actual 0.001) "Can't do too small")
    (with-read ledger (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce oracle "Must be an oracle")
        (with-read requests3 key
            {"actualed":= actualed,
             "direction":= direction,
             "eaddress":= eaddress,
             "amount":= amount,
             "mbd":= mbd,
             "cancelled":= cancelled}
            (enforce (= actualed false) "Already actualed")
            (enforce (= cancelled false) "Already cancelled")
            (enforce (= mbd true) "Not burned yet")
            (enforce (= direction "out") "Wrong direction")
            (update requests3 key 
                {"oaddress": (at "sender" (chain-data)),
                "actual": actual, ;This is what was transferred back (minus fees)
                "actualAt": (get-time),
                "actualed": true,
                "txnDetails": txnDetails
                }
            )
            ; eaddress here is the target address. eaddress balance should be calculated based on txnDetails source address and amount 
            ; (with-read eaddresses1 eaddress
            ;   {"balance":= balance}
            ;   (update  eaddresses1 eaddress
            ;         {"balance": (- balance amount)} ;This inludes actually transferred plus external gas fees 
            ;   )
            ; )
        )
    )
  )

  (defconst MINIMUM_PRECISION 8)

  (defun precision:integer ()
    MINIMUM_PRECISION)

  (defun enforce-unit:bool (amount:decimal)
    (enforce-precision (precision) amount))

  (defun create-account:string
    ( account:string
      guard:guard
    )
    (enforce-valid-account account)
    (enforce-reserved account guard)
    (insert ledger account
      { "balance" : 0.0
      , "guard"   : guard
      , "restricted" : false
      , "oracle" : false
      })
  )

  (defun get-balance:decimal (account:string)
    (at 'balance (read ledger account))
  )

  (defun details:object{fungible-v2.account-details}
    ( account:string )
    (with-read ledger account
      { "balance" := bal
      , "guard" := g }
      { "account" : account
      , "balance" : bal
      , "guard": g })
  )

  (defun rotate:string (account:string new-guard:guard)
    (with-capability (ROTATE account)
      (update ledger account
        { "guard" : new-guard }))
  )

  (defun transfer:string
    ( sender:string
      receiver:string
      amount:decimal
    )

    @model [(property (account-guard-enforced sender))]

    (enforce-valid-interparty-transfer sender receiver (precision) amount)
    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (credit-account receiver amount))
  )

  (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal )

    @model [(property (account-guard-enforced sender))]

    (enforce-valid-interparty-transfer sender receiver (precision) amount)
    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (credit receiver receiver-guard amount))
  )

  (defun debit:string (account:string amount:decimal)
    (require-capability (DEBIT account))
    (update ledger account
      { 'balance:
        (compute-debit
          (get-balance account) amount) })
  )

  (defun credit-account (account:string amount:decimal)
    (require-capability (CREDIT account))
    (credit account (at 'guard (read ledger account)) amount)
  )


  (defun credit:string (account:string guard:guard amount:decimal)
    (require-capability (CREDIT account))
    (with-default-read ledger account
      { "balance" : -1.0, "guard" : guard, "restricted" : false, "oracle" : false }
      { "balance" := balance
      , "guard" := retg
      , "restricted" := restricted
      , "oracle" := oracle
      }

      (let ((is-new
               (if (= balance -1.0)
                   (enforce-reserved account guard)
                 false)))

        (enforce (= retg guard) "account guards must match")
        (write ledger account
          { "balance" : (if is-new amount (+ balance amount))
          , "guard"   : retg
          , "restricted" : restricted
          , "oracle" : oracle
          })))
  )

  (defschema xinfo source-chain:string)
  (defun xyield:object{xinfo} ()
    { 'source-chain: (current-chain-id) })

  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )



    (step (with-capability (XCHAIN)
      (xchain-send sender receiver target-chain amount)))

    (step
      (resume { 'source-chain:= sc }
        (with-capability (XCHAIN)
          (xchain-receive sc receiver receiver-guard amount))))

  )

  (defun xchain-send:string
    ( sender:string
      receiver:string
      target-chain:string
      amount:decimal )
    (require-capability (XCHAIN))
    (with-capability (XCHAIN_DEBIT sender)
      (enforce-valid-xchain-transfer
        target-chain sender receiver (precision) amount)

      (debit sender amount)
      (emit-event (TRANSFER sender "" amount))
      (yield (xyield) target-chain))
      "Send successful"
  )

  (defun xchain-receive:string
    ( source-chain:string
      receiver:string
      receiver-guard:guard
      amount:decimal )
    (require-capability (XCHAIN))
    (with-capability (CREDIT receiver)
      (emit-event (TRANSFER "" receiver amount))
      (credit receiver receiver-guard amount))
  )

  (defun get-time:time ()
    "Get block time"
    (at "block-time" (chain-data))
  )

  (defun get-time-string:string ()
    "Get block time"
    (format-time "%Y-%m-%dT%H:%M:%S%N" (at "block-time" (chain-data)))
  )

  (defun get-epoch:time ()
    "Get epoch"
    (time "1970-01-01T00:00:00Z")
  )

  (defun compound-key2:string
    (part1 part2)
    (format "{}{}{}" [part1 "/" part2])
  )

  (defun get-requests ()
    (select requests3 (constantly true))
  )

  (defun get-eaddresses ()
    (select eaddresses1 (constantly true))
  )

  (defun get-ledger ()
    (select ledger (constantly true))
  )

; Adapted from 
; https://github.com/wrappedfi/wrapped_token_pact
; MIT License

; Copyright (c) 2022 wrapped.com

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

)

; (create-table roles)
; (create-table ledger)
; (create-table requests3)
; (create-table eaddresses1)


