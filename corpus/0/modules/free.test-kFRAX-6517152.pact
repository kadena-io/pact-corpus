(module test-kFRAX GOVERNANCE

  @doc "Fungible token for testing"
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
               , xchain-receive]})       ;; PRIVATE
          ;; TODO: I believe this fails with transfer now (since it calls out to mint/burn)
          ;; (but verification is kinda broken already due to the prefix account stuff...)

          ;; role-write-guard
          (property
           (forall (k:string)
                   (when (row-written roles k)
                     (row-enforced roles 'guard "module-admin")))
           { 'except:
             [ init-roles            ;; install only
               transfer-crosschain]})   ;; xchain decomposition

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
               manage-bridge        ;; bridge-role-guard
               create-account       ;; insert only
               transfer             ;; account-guard-enforced sender
               transfer-create      ;; account-guard-enforced sender
               transfer-crosschain  ;; xchain decomposition
               debit                ;; PRIVATE
               credit               ;; PRIVATE
               credit-account       ;; PRIVATE
               xchain-receive]})       ;; PRIVATE

          ;; TODO/NOTE: these were removed, and the BURN/MINT caps are now private
          ;; ;; burn-role-guard
          ;; (property (row-enforced roles 'guard "burner")
          ;;           { 'only: [burn] })
          ;; ;; mint-role-guard
          ;; (property (row-enforced roles 'guard "minter")
          ;;           { 'only: [mint] })
          ;; revoke-role-guard
          (property (row-enforced roles 'guard "revoker")
                    { 'only: [revoke]})
          ;; restrict-role-guard
          (property (row-enforced roles 'guard "restrict")
                    { 'only: [manage-restriction]})
          ;; bridge-role-guard
          (property (row-enforced roles 'guard "bridge")
                    { 'only: [manage-bridge]})

          (defproperty account-guard-enforced (account:string)
            (when (row-written ledger account)
              (row-enforced ledger 'guard account)))]

  (implements fungible-v2)

  (defconst ROLE_MODULE_ADMIN "module-admin")
  (defconst ROLE_REVOKER "revoker")
  (defconst ROLE_RESTRICT "restrict")
  (defconst ROLE_BRIDGES "bridge")

  (defconst FAKE_BRIDGE_BALANCE 999999999999.0)

  (defun all-roles ()
    [ ROLE_MODULE_ADMIN
      ROLE_REVOKER
      ROLE_RESTRICT
      ROLE_BRIDGES])

  (defun validate-role (role:string)
    (enforce (contains role (all-roles)) "Invalid role"))

  (defun enforce-valid-amount
      ( precision:integer
       amount:decimal)

    (enforce (> amount 0.0) "Positive non-zero amount")
    (enforce-precision precision amount))

  (defun enforce-valid-account (account:string)
    (enforce (> (length account) 2) "minimum account length")
    (enforce (< (length account) 256) "minimum account length")
    (enforce (is-charset CHARSET_LATIN1 account) "valid charset"))

  (defun enforce-precision
      ( precision:integer
       amount:decimal)

    (enforce
     (= (floor amount precision) amount)
     "precision violation"))

  (defun enforce-valid-transfer
      ( sender:string
       receiver:string
       precision:integer
       amount:decimal)
    (enforce-valid-amount precision amount)
    (enforce-valid-account sender)
    (enforce-valid-account receiver))

  (defun enforce-valid-interparty-transfer
      ( sender:string
       sender-bridge:bool
       receiver:string
       receiver-bridge:bool
       precision:integer
       amount:decimal)
    (enforce-valid-transfer sender receiver precision amount)
    (enforce (!= sender receiver) "Transfer to same account prohibited")
    (if sender-bridge
        (enforce (not receiver-bridge) "Transfer from bridge to bridge prohibited")
        true))

  (defun enforce-valid-xchain-transfer
      ( target-chain:string
       sender:string
       receiver:string
       precision:integer
       amount:decimal)
    (enforce-valid-transfer sender receiver precision amount)
    (enforce (!= "" target-chain) "empty target-chain")
    (enforce (!= (current-chain-id) target-chain)
             "cannot run cross-chain transfers to the same chain"))

  (defun enforce-transfer-manager:decimal
      ( managed:decimal
       requested:decimal)

    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
               (format "TRANSFER exceeded for amount {}" [managed]))
      newbal))

  (defun compute-debit:decimal (balance:decimal amount:decimal)
    (enforce (<= amount balance) "Insufficient funds")
    (- balance amount))

  (defun current-chain-id:string ()
    (at 'chain-id (chain-data)))

  (defun check-reserved:string (account:string)
    @doc " Checks ACCOUNT for reserved name and returns type if \
         \ found or empty string. Reserved names start with a \
         \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
    (let ((pfx (take 2 account)))
      (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

  (defun enforce-reserved:bool (account:string guard:guard)
    @doc "Enforce reserved account name protocols."
    (let ((r (check-reserved account)))
      (if (= "" r) true
          (if (= "k" r)
              (enforce
               (= (format "{}" [guard])
                  (format "KeySet {keys: [{}],pred: keys-all}"
                          [(drop 2 account)]))
               "Single-key account protocol violation")
              (enforce false
                       (format "Unrecognized reserved protocol: {}" [r]))))))
  ;; end wrapped-util

  ;;
  ;; tables/schemas
  ;;

  ;; key is role
  (defschema role-schema
      guard:guard)

  (deftable roles:{role-schema})

  ;; key is 'minted or 'burned
  (defschema mint-tracking-schema
      total:decimal)

  (deftable mint-tracking:{mint-tracking-schema})

  ;; key is account name
  (defschema account-schema
      balance:decimal
    guard:guard
    restricted:bool
    bridge:bool)

  (deftable ledger:{account-schema})

  ;;
  ;; capabilities
  ;;

  (defcap GOVERNANCE ()
    @doc "Module admin capability"
    (compose-capability (ROLE ROLE_MODULE_ADMIN)))

  (defcap ROLE (role:string)
    @doc "Composable capability for role enforcement"
    (enforce-guard (at 'guard (read roles role))))

  (defcap UPDATE_ROLE:bool (role:string)
    @doc "Managed cap/event for updating role"
    @managed
    (compose-capability (ROLE ROLE_MODULE_ADMIN)))

  (defcap ACCOUNT_GUARD (account:string)
    (enforce-guard (at 'guard (read ledger account))))

  (defcap DEBIT (sender:string)
    @doc "Capability for managing debiting operations"
    (enforce-valid-account sender))

  (defcap XCHAIN () "Private cap for crosschain" true)

  (defcap XCHAIN_DEBIT (sender:string)
    @doc "Capability for managing debiting operations"
    (compose-capability (DEBIT sender))
    (compose-capability (ACCOUNT_GUARD sender))
    (compose-capability (UNRESTRICTED sender)))

  (defcap CREDIT (receiver:string)
    @doc "Capability for managing crediting operations"
    (enforce-valid-account receiver)
    (compose-capability (UNRESTRICTED receiver)))

  (defcap MANAGE_RESTRICTION:bool (account:string restricted:bool)
    @doc "Managed cap/event for managing account restriction"
    @managed
    (compose-capability (ROLE ROLE_RESTRICT)))

  (defcap MANAGE_BRIDGE:bool (account:string bridge:bool)
    @doc "Managed cap/event for updating bridge accounts"
    @managed
    (compose-capability (ROLE ROLE_BRIDGES)))

  (defcap REVOKE:bool (account:string revoke-account:string amount:decimal)
    @doc "Revocation administrative action"
    @managed
    (enforce-valid-transfer account revoke-account (precision) amount)
    (compose-capability (ROLE ROLE_REVOKER))
    (compose-capability (DEBIT account))
    (compose-capability (CREDIT revoke-account))
    (enforce-restriction account true)) ;; account must be restricted to revoke

  (defcap UNRESTRICTED (account:string)
    @doc "Enforce account not restricted"
    (enforce-restriction account false))

  (defcap ROTATE (account:string)
    @doc "Managed cap/event for user account rotation"
    @managed
    (compose-capability (ACCOUNT_GUARD account)))

  (defcap TRANSFER:bool
      ( sender:string
        receiver:string
        amount:decimal)

    @managed amount TRANSFER-mgr
    (enforce-valid-transfer sender receiver (precision) amount)
    (compose-capability (DEBIT sender))
    (compose-capability (ACCOUNT_GUARD sender))
    (compose-capability (CREDIT receiver))
    (compose-capability (UNRESTRICTED sender)))


  (defun TRANSFER-mgr:decimal
      ( managed:decimal
       requested:decimal)

    (enforce-transfer-manager managed requested))

  (defcap MINT:bool (sender:string recipient:string amount:decimal)
    @doc "Private capability for minting from a bridge"
    (compose-capability (TRANSFER sender recipient amount))
    (enforce-bridge sender true)
    (enforce-bridge recipient false))

  (defcap BURN:bool (sender:string recipient:string amount:decimal)
    @doc "Private capability for burning to a bridge"
    (compose-capability (TRANSFER sender recipient amount))
    (enforce-bridge sender false)
    (enforce-bridge recipient true))

  ;;
  ;; wrapped-token-v1 functionality
  ;;

  (defun init-roles ()
    (insert roles ROLE_REVOKER
            { 'guard: (read-keyset ROLE_REVOKER)})
    (insert roles ROLE_RESTRICT
            { 'guard: (read-keyset ROLE_RESTRICT)})
    (insert roles ROLE_BRIDGES
            { 'guard: (read-keyset ROLE_BRIDGES)})
    (insert roles ROLE_MODULE_ADMIN
            { 'guard: (read-keyset ROLE_MODULE_ADMIN)}))

  (defun update-role:string (role:string guard:guard)
    @doc "Update role, guarded by UPDATE_ROLE"
    (with-capability (UPDATE_ROLE role)
      (update roles role { 'guard: guard})))

  (defun get-role:object{role-schema} (role:string)
         (read roles role))

  (defun manage-restriction:string (account:string restricted:bool)
    (with-capability (MANAGE_RESTRICTION account restricted)
      (update ledger account { 'restricted: restricted})))

  (defun is-restricted:bool (account:string)
    (with-default-read ledger account
      { 'restricted: false } { 'restricted := r }
      r))

  ;; TODO/NOTE: probably want to remove this from the real deploy
  (defun restricted-accounts:[string] ()
         (filter (is-restricted) (keys ledger)))

  (defun manage-bridge:string (account:string bridge:bool)
    (with-capability (MANAGE_BRIDGE account bridge)
      (update ledger account { 'bridge: bridge})))

  (defun is-bridge:bool (account:string)
    (with-default-read ledger account
      { 'bridge: false } { 'bridge := b }
      b))

  (defun bridge-accounts:[string] ()
         (filter (is-bridge) (keys ledger)))

  (defun enforce-restriction (account:string restriction:bool)
    @doc "Enforce ACCOUNT restriction is RESTRICTION"
    (let ((r (is-restricted account)))
      (enforce (= r restriction)
               (if r "Account Restricted" "Account Unrestricted"))))


  (defun enforce-bridge (account:string bridge:bool)
    @doc "Enforce ACCOUNT bridge status is BRIDGE"
    (let ((b (is-bridge account)))
      (enforce (= b bridge)
               (if b "Bridge Account" "Regular Account"))))

  (defun revoke:string
      ( account:string
       revoke-account:string
       amount:decimal)

    @doc "Administrative revocation action"
    (with-capability (REVOKE account revoke-account amount)
      (emit-event (TRANSFER account revoke-account amount))
      (debit account amount)
      (credit-account revoke-account amount)))

  (defun mint-create:string (sender:string recipient:string recipient-guard:guard amount:decimal)
    @doc "Internal mint action when transfer-creating out of a bridge"
    (require-capability (MINT sender recipient amount))
    (credit recipient recipient-guard amount)
    (with-default-read mint-tracking 'minted { "total": 0.0 } { "total" := total}
                       (write mint-tracking 'minted  { "total": (+ total amount)}))
    "Mint succeeded")

  (defun mint:string (sender:string recipient:string amount:decimal)
    @doc "Internal mint action when transferring out of a bridge"
    (require-capability (MINT sender recipient amount))
    (credit-account recipient amount)
    (with-default-read mint-tracking 'minted { "total": 0.0 } { "total" := total}
                       (write mint-tracking 'minted  { "total": (+ total amount)}))
    "Mint succeeded")

  (defun burn:string (sender:string recipient:string amount:decimal)
    @doc "Internal burn action when transferring into a bridge"
    (require-capability (BURN sender recipient amount))
    (debit sender amount)
    (with-default-read mint-tracking 'burned { "total": 0.0 } { "total" := total}
                       (write mint-tracking 'burned  { "total": (+ total amount)}))
    "Burn succeeded")

  (defun get-total-minted:decimal ()
    @doc "Returns how many tokens have been minted total"
    (with-default-read mint-tracking 'minted { "total": 0.0 } { "total" := total}
                       total))

  (defun get-total-burned:decimal ()
    @doc "Returns how many tokens have been burned total"
    (with-default-read mint-tracking 'burned { "total": 0.0 } { "total" := total}
                       total))

  ;;
  ;; fungible-v2 functionality
  ;;

  (defconst MINIMUM_PRECISION 18)

  (defun precision:integer ()
    MINIMUM_PRECISION)

  (defun enforce-unit:bool (amount:decimal)
    (enforce-precision (precision) amount))

  (defun create-account:string
      ( account:string
       guard:guard)

    (enforce-valid-account account)
    (enforce-reserved account guard)
    (insert ledger account
            { "balance"    : 0.0
              , "guard"      : guard
              , "restricted" : false
              , "bridge"     : false}))


  (defun get-balance:decimal (account:string)
    (if (is-bridge account) FAKE_BRIDGE_BALANCE (at 'balance (read ledger account))))

  (defun details:object{fungible-v2.account-details}
    ( account:string)
    (with-read ledger account
      { "balance" := bal
        , "guard" := g
        , "bridge":= b}
      { "account" : account
        , "balance" : (if b FAKE_BRIDGE_BALANCE bal)
        , "guard": g}))

  (defun rotate:string (account:string new-guard:guard)
    (with-capability (ROTATE account)
      (update ledger account
              { "guard" : new-guard})))

  (defun transfer:string
      ( sender:string
       receiver:string
       amount:decimal)

    @model [(property (account-guard-enforced sender))]

    (let ((sender-bridge (is-bridge sender))
          (receiver-bridge (is-bridge receiver)))
      (enforce-valid-interparty-transfer sender sender-bridge receiver receiver-bridge (precision) amount))

    (if (is-bridge sender)
        (with-capability (MINT sender receiver amount) (mint sender receiver amount))
        (if (is-bridge receiver)
            (with-capability (BURN sender receiver amount) (burn sender receiver amount))
            (with-capability (TRANSFER sender receiver amount)
              (debit sender amount)
              (credit-account receiver amount)))))

  (defun transfer-create:string
      ( sender:string
       receiver:string
       receiver-guard:guard
       amount:decimal)

    @model [(property (account-guard-enforced sender))]

    (let ((sender-bridge (is-bridge sender))
          (receiver-bridge (is-bridge receiver)))
      (enforce-valid-interparty-transfer sender sender-bridge receiver receiver-bridge (precision) amount))

    (if (is-bridge sender)
        (with-capability (MINT sender receiver amount) (mint-create sender receiver receiver-guard amount))
        (if (is-bridge receiver)
            (with-capability (BURN sender receiver amount)
              (let ((recvguard (at 'guard (read ledger receiver)))) ;; this never fails because (is-bridge receiver) returned true
                (enforce (= recvguard receiver-guard) "account guards must match"))
              (burn sender receiver amount))
            (with-capability (TRANSFER sender receiver amount)
              (debit sender amount)
              (credit receiver receiver-guard amount)))))

  (defun debit:string (account:string amount:decimal)
    (require-capability (DEBIT account))
    (let ((bridge (is-bridge account)))
      (enforce (not bridge) "cannot debit bridge account"))
    (update ledger account
            { 'balance:
              (compute-debit
               (get-balance account) amount)}))

  (defun credit-account (account:string amount:decimal)
    (require-capability (CREDIT account))
    (credit account (at 'guard (read ledger account)) amount))

  (defun credit:string (account:string guard:guard amount:decimal)
    (require-capability (CREDIT account))
    (with-default-read ledger account
      { "balance" : -1.0, "guard" : guard, "restricted" : false, "bridge" : false}
      { "balance" := balance
        , "guard" := retg
        , "restricted" := restricted
        , "bridge" := bridge}

      (let ((is-new
             (if (= balance -1.0)
                 (enforce-reserved account guard)
                 false)))

        (enforce (= retg guard) "account guards must match")
        (enforce (not bridge) "cannot credit bridge account")
        (write ledger account
               { "balance" : (if is-new amount (+ balance amount))
                 , "guard"   : retg
                 , "restricted" : restricted
                 , "bridge" : bridge}))))

  (defschema xinfo source-chain:string)
  (defun xyield:object{xinfo} ()
         { 'source-chain: (current-chain-id)})

  (defpact transfer-crosschain:string
      ( sender:string
        receiver:string
        receiver-guard:guard
        target-chain:string
        amount:decimal)

    (step (with-capability (XCHAIN)
            (xchain-send sender receiver target-chain amount)))

    (step
     (resume { 'source-chain:= sc}
             (with-capability (XCHAIN)
               (xchain-receive sc receiver receiver-guard amount)))))

  (defun xchain-send:string
      ( sender:string
       receiver:string
       target-chain:string
       amount:decimal)
    (require-capability (XCHAIN))
    (with-capability (XCHAIN_DEBIT sender)
      (enforce-valid-xchain-transfer
       target-chain sender receiver (precision) amount)

      (if (not (is-bridge sender)) (debit sender amount) "No-op sending from bridge account")
      (emit-event (TRANSFER sender "" amount))
      (yield (xyield) target-chain))
    "Send successful")

  (defun xchain-receive:string
      ( source-chain:string
       receiver:string
       receiver-guard:guard
       amount:decimal)
    (require-capability (XCHAIN))
    (with-capability (CREDIT receiver)
      (emit-event (TRANSFER "" receiver amount))
      (if (not (is-bridge receiver)) (credit receiver receiver-guard amount) "No-op receiving to bridge account"))))


