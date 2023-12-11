(module sgk-gen-0-policy GOVERNANCE
    @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."
  
    (defcap GOVERNANCE ()
      (enforce-one "Any Guard passes" [(enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
      (enforce-guard (at "guard" (coin.details "k:67a044c7585344504de4d3ae116b866e5929031113ee24f7d48fa4013dd67c4c")))
      (enforce-guard (at "guard" (coin.details "k:a3ecc7fc15052ea4ffecad3035bad35c8e3b20a70ddb5227e4c35d227e4c0d13")))])
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
    (defconst TOKEN_SPEC "token_spec"
      @doc "Payload field for token spec")
    (defconst QUOTE-MSG-KEY "quote"
      @doc "Payload field for quote spec")
    (defconst CREATOR_ADDRESS "k:67a044c7585344504de4d3ae116b866e5929031113ee24f7d48fa4013dd67c4c"
      @doc "effectively the admin address or account that created the collection. recieves partial mint royalties")
    (defconst ADMIN_ADDRESS "k:3744b5b5252cf34412854ca03fa5506819db9fa03eca964874f1798ee4aa2d46"
      @doc "effectively the admin address that recvs mint payouts")
    (defconst PROVIDER_ADDRESS "k:3744b5b5252cf34412854ca03fa5506819db9fa03eca964874f1798ee4aa2d46"
      @doc "provider address which operates the mint")
    (defconst AIRDROP_ADDRESS "k:dc935ba924a76d5d118b6000b9dcefee7a436dd7a45f744c1bb4d497d97281fc"
      @doc "provider address which operates the mint")
    (defconst TOTAL-MINTED-KEY "total-minted"
      @doc "Total Minted key for integer-consts to track total minted")
    (defconst TOTAL-CREATED-KEY "total-created"
      @doc "Total Created key for integer-consts to track total created")
    (defconst TOTAL-STAKED-KEY "total-staked"
      @doc "Total Staked key for integer-consts to track total staked")
    (defconst MAX-UNIQUE-SUPPLY 1050
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
      free-mints-granted:integer
      free-mints-remaining:integer
    )
  
    (defschema bool-consts-schema
      @doc "schema for boolean constants"
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
            (insert integer-consts TOTAL-STAKED-KEY
                {'val: 0}
            )
            (insert integer-consts TOTAL-MINTED-KEY
                {'val: 0}
            )
            (insert integer-consts TOTAL-CREATED-KEY
                {'val: 0}
            )
        )
      )
  
    (defun get-policy:object{tokens-schema} (token:object{token-info})
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
  
        (enforce false "mint ended")
        false
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
        (enforce false "mint ended")
        false
    )
  
    (defun enforce-offer:bool
      ( token:object{token-info}
        seller:string
        amount:decimal
        sale-id:string
      )
      @doc "Capture quote spec for SALE of TOKEN from message"
      (enforce-ledger)
        (enforce-can-transact (at 'id token))

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
          (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout CREATOR_ADDRESS spec)))
          true
        )
    )
  
    (defun enforce-buy:bool
      ( token:object{token-info}
        seller:string
        buyer:string
        buyer-guard:guard
        amount:decimal
        sale-id:string )
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
                  ;(royalty-payout:decimal (+ 0.0 0.0)) 
                   (royalty-payout:decimal
                      (floor (* sale-price royalty-rate) (fungible::precision)))
                   (payout:decimal (- sale-price royalty-payout)) )
              (if
                (> royalty-payout 0.0)
                (fungible::transfer buyer CREATOR_ADDRESS royalty-payout)
                "No royalty")
              (fungible::transfer buyer recipient payout)))
              (with-capability (UPDATE-OWNER (at 'id token) buyer)
                  (enforce (= 1.0 amount) "Invalid amount for single supply NFT Collection")
                  (update-owner (at 'id token) buyer)
              )
              true
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
          ;(enforce-can-transact (at 'id token))
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

    (defcap PRIVATE ()
        true
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
      true
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
  
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Utility Getters
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
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
  
  
    (defun get-nfts-by-owner(owner:string)
      @doc "All NFTs by owner"
        (select policies ["token-id","is-locked"] (where 'owner (= owner)))
    )

    (defun get-staked-nfts()
        @doc "Get all staked NFTs"
        (select policies ["token-id"] (where 'is-locked (= true)))
    )

    (defun get-bool-consts:bool (key:string)
      @doc "Get bool constants with false as default"
      (with-default-read bool-consts key
        { 'val : false }
        { 'val := s }
        s)
    )
    (defun get-integer-consts:integer (key:string)
      @doc "Get integer constants with -1 as default"
      (with-default-read integer-consts key
        { 'val : -1 }
        { 'val := s }
        s)
    )
  
    (defun get-minted-supply:integer ()
        (at 'val (read integer-consts TOTAL-MINTED-KEY))
    )
    (defun get-created-supply:integer ()
        (at 'val (read integer-consts TOTAL-CREATED-KEY))
    )
    (defun get-staked-supply:integer ()
        (at 'val (read integer-consts TOTAL-STAKED-KEY))
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
  
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Staking Related Methods
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun stake-nfts (tokens:[string])
        @doc "Locks multiple tokens"
        (with-capability (PRIVATE)
            (map (stake-nft) tokens)
            (format "Staked ids {}" [tokens])
        )
    )

    (defun stake-nft (token-id:string)
        @doc "Locks token"
        (enforce-can-transact token-id)
        (let*
            (
                (owner (at 'owner (read policies token-id ["owner"])))
            )
            (enforce-not-listed token-id owner)
            (with-capability (ACCOUNT-GUARD owner)
                (update policies token-id
                    {'is-locked: true,
                        'lock-change-time: (at "block-time" (chain-data))
                    }
                )
        
                (update integer-consts TOTAL-STAKED-KEY 
                    {"val": (+ (get-integer-consts TOTAL-STAKED-KEY) 1)}
                )
            )
        )
    )

    (defun enforce-not-listed (token-id:string owner:string)
        @doc "checks if an NFT is not currently listed via marm listing"
        (let*
            (
            (curr-balance (marmalade.ledger.get-balance token-id owner))
            )
            (enforce (= curr-balance 1.0) "NFT is already listed cannot be staked")
        )
    )
   
    (defun unlock-nfts (tokens:[string])
        @doc "unlocks multiple tokens"
        (with-capability (PRIVATE)
            (map (unlock-nft) tokens)
            (format "Unstaked ids {}" [tokens])
        )
    )

    (defun unlock-nft (token-id:string)
        @doc "unlocks token"
        (let*
        (
            (owner (at 'owner (read policies token-id ["owner"])))
            (is-locked (is-locked token-id))
        )
        (enforce (= true is-locked) "NFT is already unlocked")
        (with-capability (ACCOUNT-GUARD owner)
            (update policies token-id
            {'is-locked: false,
                'lock-change-time: (at "block-time" (chain-data))}
            )
            (update integer-consts TOTAL-STAKED-KEY {
            "val": (- (get-integer-consts TOTAL-STAKED-KEY) 1)
            })
        )
        )
    )

    (defun update-lock-time (token-id:string)
        (require-capability (n_7d47538766e6f80008f253dccd30451f4d483c38.gen-0-rewards-orchestrator.REWARD-USER))
        (let*
            (
              (is-locked (is-locked token-id)))
            (if (= true is-locked)
                (update policies token-id {'lock-change-time: (at "block-time" (chain-data))})
                "NFT not staked"
            )
        )
    )

    (defun update-lock-time-admin-test (token-id:string)
       (with-capability (GOVERNANCE)
        (let*
            (
                (is-locked (is-locked token-id))
            )
            (if (= true is-locked)
                (update policies token-id {'lock-change-time: (time "2023-10-22T18:00:00Z")})
                "NFT not staked"
            )
        ))
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
          (enforce (= is-locked false) "Cannot transfer/list NFT while it is staked")
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
  
  
