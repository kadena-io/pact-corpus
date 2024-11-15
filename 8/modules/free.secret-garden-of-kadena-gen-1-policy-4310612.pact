(module secret-garden-of-kadena-gen-1-policy GOVERNANCE
  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

  (defcap GOVERNANCE ()
    (enforce-one "Any Guard passes" [(enforce-guard (keyset-ref-guard "free.isoko-admin"))
    (enforce-guard (keyset-ref-guard "free.isoko-admin"))])
  )

  (defun enforce-orchestrator:bool ()
     (enforce-guard (free.isoko-orchestrator.isoko-orchestrator-guard))
   )
  (defun enforce-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])
  (use kip.token-manifest)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Constants
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconst OG_MINT_START_TIME "2023-05-03T16:00:00Z")

  (defconst PRIVATE_WL_MINT_START_TIME "2023-05-05T16:00:00Z")

  (defconst PUBLIC_MINT_TIME "2023-06-23T16:00:00Z")

  (defconst OG_PHASE "og-phase")
  (defconst PRIVATE_WL_PHASE "private-wl-phase")
  (defconst PUBLIC_PHASE "public-phase")

  (defconst PRIVATE_WL_ROLE "private-wl-role")
  (defconst OG_ROLE "og-role")
  (defconst PUBLIC_ROLE "public-role")

  (defconst OG_PHASE_CAP 1500)
  (defconst PRIVATE_WL_PHASE_CAP 3000)

  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")
  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")
  (defconst CREATOR_ADDRESS "k:4375f43085a0f42b81cdd4b8f2357d8344998ba41855fb51ce18fef9af353008"
    @doc "effectively the admin address or account that created the collection. recieves partial mint royalties")
  (defconst ADMIN_ADDRESS "k:f157854c15e9bb8fb55aafdecc1ce27a3d60973cbe6870045f4415dc06be06f5"
    @doc "effectively the admin address that recvs mint payouts")
  (defconst PROVIDER_ADDRESS "k:3744b5b5252cf34412854ca03fa5506819db9fa03eca964874f1798ee4aa2d46"
    @doc "provider address which operates the mint")

  (defconst TOTAL-MINTED-KEY "total-minted"
    @doc "Total Minted key for integer-consts to track total minted")
  (defconst TOTAL-CREATED-KEY "total-created"
    @doc "Total Created key for integer-consts to track total created")
  (defconst KILL-SWITCH-KEY "KILL-SWITCH"
    @doc "Kill switch key for bool-consts used to disable mint")
  (defconst MINT-CAP-FOR-PRIVATE-WL-SALE 1500
    @doc "Max amount in Private WL Phase")
  (defconst MINT-CAP-FOR-PUBLIC-SALE 3000
    @doc "Max amount in Public Phase - Enforced through max supply, only here for Documentation")
  (defconst MAX-UNIQUE-SUPPLY 4100
    @doc "Max unique supply")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Capabilities
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defcap ACCOUNT-GUARD(account:string)
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard
            (at "guard" (coin.details account))
        )
  )
  (defcap MINT(token-id:string new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    (compose-capability (UPDATE-OWNER token-id new-owner))
    (compose-capability (INCREMENT-MINTED))
    (compose-capability (UPDATE-WL))
    (compose-capability (ACCOUNT-GUARD new-owner))
  )
  (defcap CREATE ()
    @doc "Private Capability to update WL amounts after mint"
    (compose-capability (INCREMENT-CREATED))
    true)
  (defcap UPDATE-OWNER (token-id:string new-owner:string)
    @doc "Private Capability to update owner of token"
    true)
  (defcap INCREMENT-MINTED ()
    @doc "Private Capability to increment amount minted"
    true)
  (defcap INCREMENT-CREATED ()
    @doc "Private Capability to increment amount created"
    true)
  (defcap UPDATE-WL ()
    @doc "Private Capability to update WL amounts after mint"
    true)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Schemas
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defschema attributes-schema
      @doc "Attributes certificate"
      trait_type:string
      value:string
  )

  (defschema tokens-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    royalty-rate:decimal
    owner:string
    token-id:string
    is-locked:bool
    lock-change-time:time
  )
  (defschema policy-schema
    @doc "minimal schema"
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    royalty-rate:decimal
  )

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (defschema account-wl-record-schema
    @doc "Schema to hold account whitelist information & Role"
    account:string
    minted-total:integer
    wl-granted:integer
    wl-remaining:integer
    role:string
  )

  (defschema bool-consts-schema
    @doc "schema for boolean constants such as KILL-SWITCH"
    val:bool
  )

  (defschema integer-consts-schema
    @doc "schema for integer constants such as TOTAL-MINTED-KEY"
    val:integer
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Tables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (deftable policies:{tokens-schema})
  (deftable quotes:{quote-schema})
  (deftable account-wl-records:{account-wl-record-schema})
  (deftable bool-consts:{bool-consts-schema})
  (deftable integer-consts:{integer-consts-schema})

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Init
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun init()
      (with-capability (GOVERNANCE)
        (insert integer-consts TOTAL-MINTED-KEY
          {'val: 0}
        )
        (insert integer-consts TOTAL-CREATED-KEY
          {'val: 0}
        )
        (insert bool-consts KILL-SWITCH-KEY
          {'val: true}
        )
      )
    )

  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Mint
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defun enforce-mint:bool
        ( token:object{token-info}
          account:string
          guard:guard
          amount:decimal
        )

    (with-capability (MINT (at 'id token) account)
      (enforce-orchestrator)
      (enforce-ledger)
      (increment-total-minted)
      (let*
          (
            (account-wl-info:object{account-wl-record-schema} (get-account-whitelist-info account))
            (role (at 'role account-wl-info))
            (current-phase (get-current-phase))
            (isOg (if (= OG_ROLE role) true false))
            (isWlPhase (if (= current-phase PRIVATE_WL_PHASE) true false))
            (shouldDeductWl (and (= false isOg) (= true isWlPhase)))
            (price (get-price-by-phase current-phase))
            (is-phase-live-for-user:bool (get-is-phase-live-for-user current-phase account-wl-info))
          )
           (enforce (= true is-phase-live-for-user) "Mint is not live right now, or is Paused.")
           (update-wl-info account account-wl-info role shouldDeductWl)
           (bind (get-policy token)
             { 'owner:=owner:string
             }
             (enforce (= (+ amount (at 'supply token)) 1.0) "Exceeds max supply")
             (update-owner (at 'id token) account)
           )
           (if
              (or (= account ADMIN_ADDRESS) (= account CREATOR_ADDRESS) )
              (enforce (= true true ) "no op")
              (if
                (> price 0.0)
                (pay-mint account price)
                (enforce-guard (get-user-guard account guard)))
           )
      )
    )
  )

  (defun pay-mint (account-minting:string price:decimal)
    (coin.transfer account-minting ADMIN_ADDRESS price)
  )

  (defun update-wl-info (account:string wl-info:object{account-wl-record-schema} role:string shouldDeductWl:bool)
    (require-capability (UPDATE-WL))
    (let*
        (
            (wl-remaining (at 'wl-remaining wl-info))
        )
        (write account-wl-records account
           { 'minted-total : (+ 1 (at 'minted-total wl-info))
            ,'role : (at 'role wl-info)
            ,'account : account
            ,'wl-granted : (at 'wl-granted wl-info)
            ,'wl-remaining : (if (= true shouldDeductWl) (- wl-remaining 1) wl-remaining)
          }
        )
    )
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-orchestrator)
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-orchestrator)
    (enforce-ledger)
    (with-capability (CREATE)
        (let* ( (spec:object{policy-schema} (read-msg TOKEN_SPEC))
                (fungible:module{fungible-v2} (at 'fungible spec))
                (creator:string (at 'creator spec))
                (creator-guard:guard (at 'creator-guard spec))
                (royalty-rate:decimal (at 'royalty-rate spec))
                (creator-details:object (fungible::details creator ))
                (created-total:integer (get-created-supply))
                (created-total-after (+ 1 created-total))
                )
          (increment-total-created)
          (enforce (=
            (at 'guard creator-details) creator-guard)
            "Creator guard does not match")
          (enforce (and
            (>= royalty-rate 0.0) (<= royalty-rate 1.0))
            "Invalid royalty rate")
          (enforce (<= created-total-after MAX-UNIQUE-SUPPLY) "Exceeded max unique collection supply")
          (insert policies (at 'id token)
            { 'fungible: fungible
            , 'creator: creator
            , 'creator-guard: creator-guard
            , 'owner: ""
            , 'token-id: (at 'id token)
            , 'is-locked:false
            , 'lock-change-time: (at "block-time" (chain-data))
            , 'royalty-rate: royalty-rate }))
        true
    )
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-orchestrator)
    (enforce-ledger)
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
                 (floor (* sale-price royalty-rate) (fungible::precision)))
            )
        (fungible::enforce-unit sale-price)
        (enforce (< 0.0 price) "Offer price must be positive")
        (enforce (=
          (at 'guard recipient-details) recipient-guard)
          "Recipient guard does not match")
        (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
        (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout creator spec)))
        false
      )
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-orchestrator)
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
            (fungible::transfer buyer recipient payout)))
            (update-owner (at 'id token) buyer)
            false
        ))
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
    (with-capability (UPDATE-OWNER (at 'id token) receiver)
        (enforce (= 1.0 amount) "Invalid amount for single supply NFT Collection")
        (update-owner (at 'id token) receiver)
    )
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (update-owner (at 'id token) receiver)
    (enforce false "Transfer prohibited")
  )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Utility Setters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun update-owner (token-id:string new-owner:string)
    @doc "Updates token with new owner"
    (require-capability (UPDATE-OWNER token-id new-owner))
      (update policies token-id
        {'owner: new-owner}
      )
  )

  (defun add-bool-consts:bool (key:string val:bool)
    @doc "Adds entry to boolean constants"
    (with-capability (GOVERNANCE)
      (insert bool-consts key
        {'val: val}
      )
    )
  )

  (defun add-integer-consts:bool (key:string val:bool)
    @doc "Adds entry to boolean constants"
    (with-capability (GOVERNANCE)
      (insert bool-consts key
        {'val: val}
      )
    )
  )

  (defun update-bool-consts:bool (key:string val:bool)
    @doc "Adds entry to boolean constants"
    (with-capability (GOVERNANCE)
      (update bool-consts key
        {'val: val}
      )
    )
  )
  (defun increment-total-minted:bool ()
    @doc "increments total minted amount"
    (require-capability (INCREMENT-MINTED))
      (update integer-consts TOTAL-MINTED-KEY {
        "val": (+ (get-integer-consts TOTAL-MINTED-KEY) 1)
      })
      true
  )

  (defun increment-total-created:bool ()
    @doc "increments total created amount"
    (require-capability (INCREMENT-CREATED))
      (update integer-consts TOTAL-CREATED-KEY {
        "val": (+ (get-integer-consts TOTAL-CREATED-KEY) 1)
      })
      true
  )
  (defun add-private-wl-bulk (wl-list:list)
    (map (add-private-wl-wrapper) wl-list)
  )
  (defun add-private-wl-wrapper (wl:object)
    (let*
        (
          (account:string (at 'account wl))
          (isOg:bool (at 'isOg wl))
          (additionalWls (at 'additionalWls wl))
        )
        (add-private-wl-or-role account isOg additionalWls)

    )
  )
  (defun add-private-wl-or-role (account:string isOg:bool additionalWls:integer)
    (with-capability (GOVERNANCE)
      (bind (get-account-whitelist-info account)
        {
          'wl-granted:=wl-granted,
          'wl-remaining:=wl-remaining,
          'minted-total:= minted-total,
          'role:= role,
          'account:= account
        }
        (write account-wl-records account {
          'minted-total: minted-total,
          'role: (if (= true isOg) OG_ROLE PUBLIC_ROLE),
          'account: account,
          'wl-granted: (+ wl-granted additionalWls),
          'wl-remaining: (+ wl-remaining additionalWls)
        })
        "new Free WL Spots added by Admin"
      )
    )
  )


  ;; TODO TEST
    (defun get-is-phase-live-for-user:bool (current-phase:string account-wl-info:object{account-wl-record-schema})
      (let* (
          (kill-switch:bool (get-kill))
          (role (at 'role account-wl-info))
        )
        (cond
          ((= kill-switch true) false)
          ((and (= role OG_ROLE) (or (= current-phase OG_PHASE) (= current-phase PRIVATE_WL_PHASE))) true)
          ((and (= true (check-has-wl account-wl-info)) (= current-phase PRIVATE_WL_PHASE)) true)
          ((= PUBLIC_PHASE current-phase) true)
          false)
      )
    )

    (defun deduct-wl-and-return-true:bool(account-wl-info:object{account-wl-record-schema})
      (require-capability (UPDATE-WL))
      (update account-wl-records (at 'account account-wl-info)
         {
           'wl-remaining: (- (at 'wl-remaining account-wl-info) 1)
         }
      )
      true
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Utility Getters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun check-has-wl:bool(account-wl-info:object{account-wl-record-schema})

    (if (> (at 'wl-remaining account-wl-info) 0)
      true
      false
    )
  )

  (defun get-account-whitelist-info:{account-wl-record-schema} (account:string)
    (with-default-read account-wl-records account
      {
        'wl-granted:0,
        'wl-remaining:0,
        'minted-total: 0,
        'role: PUBLIC_ROLE,
        'account: account
      }
      {
       'wl-granted:=wl-granted,
       'wl-remaining:=wl-remaining,
       'minted-total:= minted-total,
       'role:= role,
       'account:= account
      }
      {
        'wl-granted:wl-granted,
        'wl-remaining:wl-remaining,
        'minted-total: minted-total,
        'role: role,
        'account: account
      }
    )
  )

  (defun get-user-guard (account:string supplied-guard:guard)
    @doc "Get user guard from coin account on curr chain"
    (let*
      (
        (guard (at 'guard (coin.details account)))
        )
        ;;Validate that Guard passed matched set guard.
        (enforce (= guard supplied-guard) "Supplied Guard does not match")
        guard
      )
  )

  (defun get-price-by-role:decimal (role:string)
    @doc "Gets NFT price based on Role assigned. Checks for free mint to override role first."
        (let*
            ((isPublicOpen:bool (check-is-public-mint-live)))
            (cond
                ((= true isPublicOpen) 16.625)
                ((= role PRIVATE_WL_ROLE) 11.875)
                11.875)
        )
  )
  (defun get-price-by-phase:decimal(current-phase:string)
    (cond
        ((= OG_PHASE current-phase) 1.0)
        ((= PRIVATE_WL_PHASE current-phase) 2.0)
        ((= PUBLIC_PHASE current-phase) 3.0)
        10000000.0)
  )
  (defun get-nfts-by-owner(owner:string)
    @doc "All NFTs by owner"
      (select policies ["token-id"] (where 'owner (= owner)))
  )
  (defun get-bool-consts:bool (key:string)
    @doc "Get bool constants with false as default"
    (with-default-read bool-consts key
      { 'val : false }
      { 'val := s }
      s)
  )
  (defun get-integer-consts:bool (key:string)
    @doc "Get integer constants with -1 as default"
    (with-default-read integer-consts key
      { 'val : -1 }
      { 'val := s }
      s)
  )
  (defun get-kill:bool()
    @doc "Wrapper to get Kill switch used to disable mint. Default = true (true means STOP MINT). Can be deprecated."
    (let* (
        (kill:bool (get-kill-switch))
      )
      kill
    )
  )
  (defun get-kill-switch:bool ()
    @doc "Gets kill switch used to disable mint. Default = true (true means STOP MINT)"
    (with-default-read bool-consts KILL-SWITCH-KEY
      { 'val : true }
      { 'val := s }
      s)
  )

  (defun get-minted-supply:integer ()
    (at 'val (read integer-consts TOTAL-MINTED-KEY))
  )
  (defun get-created-supply:integer ()
    (at 'val (read integer-consts TOTAL-CREATED-KEY))
  )

  (defun get-current-phase()
    (let*
        (
            (isPublicStarted (check-is-mint-live-for-phase PUBLIC_MINT_TIME))
            (isPrivateStarted (check-is-mint-live-for-phase-and-limit PRIVATE_WL_MINT_START_TIME PRIVATE_WL_PHASE_CAP))
            (isOgStarted (check-is-mint-live-for-phase-and-limit OG_MINT_START_TIME OG_PHASE_CAP))
        )
        (cond
            ((= true isPublicStarted) PUBLIC_PHASE)
            ((= true isPrivateStarted) PRIVATE_WL_PHASE)
            ((= true isOgStarted) OG_PHASE)
            "Mint is not Live")
    )
  )

  (defun get-current-phase-visible-for-testing(private-wl-cap:integer og-cap:integer)
    (let*
        (
            (isPublicStarted (check-is-mint-live-for-phase PUBLIC_MINT_TIME))
            (isPrivateStarted (check-is-mint-live-for-phase-and-limit PRIVATE_WL_MINT_START_TIME private-wl-cap))
            (isOgStarted (check-is-mint-live-for-phase-and-limit OG_MINT_START_TIME og-cap))
        )
        (cond
            ((= true isPublicStarted) PUBLIC_PHASE)
            ((= true isPrivateStarted) PRIVATE_WL_PHASE)
            ((= true isOgStarted) OG_PHASE)
            "Mint is not Live")
    ))

  (defun is-public-mint-live:bool ()
    @doc "Enforces mint is live"
    (enforce-time PUBLIC_MINT_TIME)
    true
  )
  (defun enforce-time (start-time:string)
    (enforce (>= (at "block-time" (chain-data)) (time start-time)) "Mint is NOT Live yet for you.")
  )

  (defun check-is-public-mint-live:bool ()
    @doc "check is public mint is live without enforcing"
    (if (>= (at "block-time" (chain-data)) (time PUBLIC_MINT_TIME)) true false)
  )

  (defun check-is-mint-live-for-phase:bool (phase-time:string)
    @doc "check is public mint is live without enforcing"
    (if (>= (at "block-time" (chain-data)) (time phase-time)) true false)
  )

  (defun check-is-mint-live-for-phase-and-limit:bool (phase-time:string max-phase-cap:integer)
    @doc "check is public mint is live without enforcing"
    (let*
        (
            (minted-supply:integer (get-minted-supply))
            (supply-after-mint (+ 1 minted-supply))

        )
        (if (and (<= supply-after-mint max-phase-cap) (>= (at "block-time" (chain-data)) (time phase-time))) true false)
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Keyed Access
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun is-access-granted-by-any-ownership(account:string)
    (let*
        (
            (nfts:list (get-nfts-by-owner account))
            (nft-size (length nfts))
        )
        (enforce (> nft-size 0) "you do not own NFTs!")
        (validate-guard account)
    )
  )
  (defun allow-access-by-nft-ownership:bool (
    account:string
    nft-id:string)
      (let*
          (
              (user-coin-guard:guard (at 'guard (coin.details account)))
              (nfts:list (get-nfts-by-owner account))
              (has-nft:bool (contains {"token-id":nft-id} nfts))
          )
          (enforce-guard user-coin-guard)
          (enforce (= true has-nft) "NFT not owned by user, access not granted")
      )
    )
    (defun validate-guard:bool (account:string)
      (enforce-guard (at 'guard (coin.details account)))
    )
    (defun get-all-nfts ()
        (keys policies)
    )
    (defun get-nfts-details (id:string)
        (read policies id)
    )

    (defun get-nft-owners (id:string)
        (let*

         ((all-nfts:list (get-all-nfts))
          (all-objs (map (get-nfts-details) all-nfts))
         )
        ;;(map (at 'owner) all-objs)
        (map (at 'owner) all-objs)
        )
    )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Testing simulated windows : only test external methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defconst PUBLIC_MINT_TEST_TIME "2023-04-22T16:00:00Z")
  (defun get-price-by-role-test-only:decimal (role:string)
    @doc "Gets NFT price based on Role assigned. Checks for free mint to override role first."
        (let*
            ((isPublicOpen:bool (check-is-public-mint-live)))
            (cond
                ((= true isPublicOpen) 16.625)
                ((= role PRIVATE_WL_ROLE) 11.875)
                11.875)
        )

  )
  (defun check-is-public-mint-live-test-only:bool ()
    @doc "check is public mint is live without enforcing"
    (if (>= (at "block-time" (chain-data)) (time PUBLIC_MINT_TEST_TIME)) true false)
  )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Staking Related Methods
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun stake-nft (token-id:string)
    @doc "Locks token"
    (enforce-can-transact token-id)
    (let*
      (
        (owner (at 'owner (read policies token-id ["owner"])))
      )
      (with-capability (ACCOUNT-GUARD owner)
        (update policies token-id
          {'is-locked: true,
           'lock-change-time: (at "block-time" (chain-data))}
        )
      )
    )
  )

  (defun get-time-stake-status-change (token-id:string)
    @doc "gets time the stake status changed"
    (time-locked token-id)
  )
  (defun get-time-since-stake-status-change (token-id:string)
    @doc "gets time the stake status changed"
    (diff-time (at "block-time" (chain-data)) (time-locked token-id))
  )
  (defun enforce-can-transact (token-id:string)
    @doc "checks if an NFT can be sold/transfered/offered/bought based on if it is locked."
    (let*
        (
          (is-locked (is-locked token-id))
        )
        (enforce (= is-locked false) "nft is locked/staked and cannot be transacted (sold/transfered/offered/touched/looked at/smelt)")
    )
  )

  (defun is-locked(token-id:string)
    @doc "checks if an NFT is locked."
    (at 'is-locked (read policies token-id ["is-locked" "lock-change-time"]))
  )
  (defun time-locked(token-id:string)
    @doc "checks if an NFT is locked."
    (at 'lock-change-time (read policies token-id ["is-locked" "lock-change-time"]))
  )
)


