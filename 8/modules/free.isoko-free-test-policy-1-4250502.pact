(module isoko-free-test-policy-1 GOVERNANCE
  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.isoko-admin"))
  )

  (defun enforce-orchestrator:bool ()
     (enforce-guard (n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-orchestrator.isoko-orchestrator-guard))
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
  (defconst PRIVATE_WL_MINT_START_TIME "2023-04-20T04:20:00Z")
  (defconst PRIVATE_WL_MINT_END_TIME "2023-04-21T12:05:00Z")
  (defconst PUBLIC_MINT_TIME "2023-04-21T12:10:00Z")
  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")
  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")
  (defconst ADMIN_ADDRESS "k:45b39ca9e2b3cb852f474e2f7ed9681b634ac184357a2b9c63ac79e64c62372c"
    @doc "admin address which also recieves mint payouts")
  (defconst CREATOR_ADDRESS "k:f157854c15e9bb8fb55aafdecc1ce27a3d60973cbe6870045f4415dc06be06f5"
    @doc "effectively the admin address or account that created the collection. recieves partial mint royalties")
  (defconst PRIVATE_WL_ROLE "private-wl-role")
  (defconst PUBLIC_ROLE "public-role")
  (defconst TOTAL-MINTED-KEY "total-minted"
    @doc "Total Minted key for integer-consts to track total minted")
  (defconst TOTAL-CREATED-KEY "total-created"
    @doc "Total Created key for integer-consts to track total created")
  (defconst KILL-SWITCH-KEY "KILL-SWITCH"
    @doc "Kill switch key for bool-consts used to disable mint")
  (defconst MINT-CAP-FOR-PRIVATE-WL-SALE 1000
    @doc "Max amount in Private WL Phase")
  (defconst MINT-CAP-FOR-PUBLIC-SALE 2500
    @doc "Max amount in Public Phase - Enforced through max supply, only here for Documentation")
  (defconst MAX-UNIQUE-SUPPLY 2000
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
            (price:decimal (get-price-by-role role))
            (is-live-for-role:bool (get-is-mint-live role))
          )
        
           (update-wl-info account account-wl-info role)
           (enforce (= true is-live-for-role) "Mint is not live right now, or is Paused.")
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
    (coin.transfer account-minting CREATOR_ADDRESS price)
  )

  (defun update-wl-info (account:string wl-info:object{account-wl-record-schema} role:string)
    (require-capability (UPDATE-WL))
    (write account-wl-records account
       { 'minted-total : (+ 1 (at 'minted-total wl-info))
        ,'role : (at 'role wl-info)
        ,'account : account}
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
                )

          (increment-total-created)
          (enforce (=
            (at 'guard creator-details) creator-guard)
            "Creator guard does not match")
          (enforce (and
            (>= royalty-rate 0.0) (<= royalty-rate 1.0))
            "Invalid royalty rate")
          (enforce (<= created-total MAX-UNIQUE-SUPPLY) "Exceeded max unique collection supply")
          (insert policies (at 'id token)
            { 'fungible: fungible
            , 'creator: creator
            , 'creator-guard: creator-guard
            , 'owner: ""
            , 'token-id: (at 'id token)
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
    (update-owner (at 'id token) receiver)
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
          (isWhiteList:bool (at 'isWhiteList wl))
        )
        (add-private-wl-role account isWhiteList)

    )
  )
  (defun add-private-wl-role (account:string isWhiteList:bool)
    (with-capability (GOVERNANCE)
      (bind (get-account-whitelist-info account)
        {
          'minted-total:= minted-total,
          'role:= role,
          'account:= account
        }
        (write account-wl-records account {
          'minted-total: minted-total,
          'role: (if isWhiteList PRIVATE_WL_ROLE role),
          'account: account
        })
        "new Free WL Spots added by Admin"
      )
    )
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Utility Getters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun get-account-whitelist-info:{account-wl-record-schema} (account:string)
    (with-default-read account-wl-records account
      {
        'minted-total: 0,
        'role: PUBLIC_ROLE,
        'account: account
      }
      {
       'minted-total:= minted-total,
       'role:= role,
       'account:= account
      }
      {
       'minted-total: minted-total,
       'role: role,
       'account: account
      }
    )
  )

  (defun get-user-guard (account:string supplied-guard:guard)
    @doc "Get user guard from coin account on chain13"
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
    (cond
        ((= role PRIVATE_WL_ROLE) 0.01)
        0.01
    )
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
  (defun get-is-mint-live:bool (role:string)
    (let* (
        (kill-switch:bool (get-kill))
        (public-start (if (>= (at "block-time" (chain-data)) (time PUBLIC_MINT_TIME)) true false))
      )
      (cond
        ((= kill-switch true) false)
        ((= public-start true) true)
        ((= role PRIVATE_WL_ROLE) (is-private-wl-mint-live))
        false)

    )

  )

  (defun get-minted-supply:integer ()
    (at 'val (read integer-consts TOTAL-MINTED-KEY))
  )
  (defun get-created-supply:integer ()
    (at 'val (read integer-consts TOTAL-CREATED-KEY))
  )

  (defun is-private-wl-mint-live:bool ()
    @doc "Enforces mint is live"
    (let* 
        (
          (minted-supply:integer (get-minted-supply))
          (curr-time (at "block-time" (chain-data)))
        )  
        (enforce-time PRIVATE_WL_MINT_START_TIME)
        (enforce (<= curr-time (time PRIVATE_WL_MINT_END_TIME)) "Private WL has ended")
        (enforce (<= minted-supply MINT-CAP-FOR-PRIVATE-WL-SALE) "Private WL supply exceeded. Wait till public!")
        true
    )
    
  )
  (defun is-public-mint-live:bool ()
    @doc "Enforces mint is live"
    (enforce-time PUBLIC_MINT_TIME)
    true
  )
  (defun enforce-time (start-time:string)
    (enforce (>= (at "block-time" (chain-data)) (time start-time)) "Mint is NOT Live yet for you.")
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
)





