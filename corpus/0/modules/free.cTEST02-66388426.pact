(module cTEST02 GOVERNANCE

  @doc "Fungible token for Wrapped test..."
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
  )

  (deftable ledger:{account-schema})

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

  (defun mint:string
    ( recipient:string
      recipient-guard:guard
      amount:decimal
    )
    @doc "Administrative mint action"
    (with-capability (MINT recipient amount)
      (credit recipient recipient-guard amount)
      (emit-event (TRANSFER "" recipient amount))
      "Mint succeeded")
  )

  (defun burn:string
    ( sender:string
      amount:decimal
    )
    @doc "Administrative burn action"
    (with-capability (BURN sender amount)
      (debit sender amount)
      (emit-event (TRANSFER sender "" amount))
      "Burn succeeded")
  )

  ;;
  ;; fungible-v2 functionality
  ;;

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
      { "balance" : -1.0, "guard" : guard, "restricted" : false }
      { "balance" := balance
      , "guard" := retg
      , "restricted" := restricted
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


