(module sgk-weapons-policy-9-1 GOVERNANCE 
    @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."
    
    (defcap GOVERNANCE ()
        (enforce-one "Any Guard passes" [(enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
        (enforce-guard (at "guard" (coin.details "k:67a044c7585344504de4d3ae116b866e5929031113ee24f7d48fa4013dd67c4c")))
        (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")))
        (enforce-guard (at "guard" (coin.details "k:a3ecc7fc15052ea4ffecad3035bad35c8e3b20a70ddb5227e4c35d227e4c0d13")))])
    )
    
    (defun enforce-orchestrator:bool ()
        (enforce-one "Any Guard passes" [
            (enforce-guard (n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-orchestrator.isoko-orchestrator-guard))
        ])
    )

    (defun enforce-ledger:bool ()
        (enforce-guard (marmalade.ledger.ledger-guard))
    )
    
    (implements n_f1c962776331c4773136dc1587a8355c9957eae1.queryable-collections-policy-v2)
    (implements kip.token-policy-v1)
    (use kip.token-policy-v1 [token-info])
    (use kip.token-manifest)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Constants
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defconst PUBLIC_MINT_TIME "2023-12-04T00:00:00Z")
    (defconst FREE_MINT_TIME "2023-12-25T18:15:00Z")
    (defconst PUBLIC_PHASE "PublicPhase")
    (defconst FREE_MINT_PHASE "FreeMintPhase")
    
    (defconst TOKEN_SPEC "token_spec"
        @doc "Payload field for token spec")
    (defconst QUOTE-MSG-KEY "quote"
        @doc "Payload field for quote spec")
    (defconst CREATOR_ADDRESS "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3"
        @doc "effectively the admin address or account that created the collection. recieves partial mint royalties")
    (defconst ADMIN_ADDRESS "k:67a044c7585344504de4d3ae116b866e5929031113ee24f7d48fa4013dd67c4c"
        @doc "effectively the admin address that recvs mint payouts")
    (defconst PROVIDER_ADDRESS "k:3744b5b5252cf34412854ca03fa5506819db9fa03eca964874f1798ee4aa2d46"
        @doc "provider address which operates the mint")
    
    (defconst AIRDROP_ADDRESS "k:dc935ba924a76d5d118b6000b9dcefee7a436dd7a45f744c1bb4d497d97281fc"
        @doc "provider address which operates the mint")
    ;;(defconst REWARDS_ADDRESS "k:80427d276c409892b1ff95582468ad0cab3b8aa033ef06a4a13210127c97d4c7"
      ;;  @doc "rewards address which operates staking rewards & orchestration")    
    (defconst REWARDS_ADDRESS "k:f157854c15e9bb8fb55aafdecc1ce27a3d60973cbe6870045f4415dc06be06f5"
        @doc "rewards address which operates staking rewards & orchestration")    

    
    (defconst TOTAL-PUBLIC_MINTED-KEY "total-public-minted"
        @doc "Total Public Minted key for integer-consts to track total minted")
    (defconst TOTAL-MINTED-KEY "total-minted"
        @doc "Total Minted key for integer-consts to track total minted")
    (defconst TOTAL-CREATED-KEY "total-created"
        @doc "Total Created key for integer-consts to track total created")
    (defconst TOTAL-STAKED-KEY "total-staked"
        @doc "Total Staked key for integer-consts to track total staked")
    (defconst KILL-SWITCH-KEY "KILL-SWITCH"
        @doc "Kill switch key for bool-consts used to disable mint")
   
    (defconst FREE-MINT-CAP 0
        @doc "Max unique supply") 
    (defconst MAX-UNIQUE-SUPPLY 10000
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
            ;(insert integer-consts TOTAL-STAKED-KEY
            ;{"val": 0}
            ;)
            ;(insert integer-consts TOTAL-MINTED-KEY
            ;{"val": 0}
            ;)
            ;(insert integer-consts TOTAL-CREATED-KEY
            ;{"val": 0}
            ;)
            ;(insert bool-consts KILL-SWITCH-KEY
            ;{"val": true}
            ;)
            (insert integer-consts TOTAL-PUBLIC_MINTED-KEY
            {"val": 0}
            )
        )
        )
    
    (defun get-policy:object{tokens-schema} (token:object{token-info})
        (read policies (at "id" token))
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
    (defun get-sender:string ()
        (at "sender" (chain-data))
    )

    (defun enforce-mint:bool
        ( token:object{token-info}
        account:string
        guard:guard
        amount:decimal
        )
        (with-capability (PRIVATE)
            (with-capability (MINT (at "id" token) account)
                (enforce-orchestrator)
                (enforce-ledger)
                (increment-total-minted)
                (let* 
                    (
                        (purpose (read-msg "purpose"))
                        (sender (get-sender))  
                        (account-info:object{account-wl-record-schema} (get-account-whitelist-info account))
        
                    )
                     (cond 
                        ( (= purpose "gen-0-rewards" ) (enforce-mint-gen-0-rewards token account guard 1.0 account-info))
                        ( (= purpose "bh-rewards" ) (enforce-mint-bh-rewards token account guard 1.0 account-info))
                         [(enforce-mint-internal-public token account guard 1.0 account-info)]
                    )
                )
            )
        )
    )
    
    (defun enforce-mint-bh-rewards:bool
      ( token:object{token-info}
        account:string
        guard:guard
        amount:decimal
        account-info:object{account-wl-record-schema})
        
            (require-capability (n_7d47538766e6f80008f253dccd30451f4d483c38.battle-heroes-rewards-orchestrator.REWARD-USER))
            (update-account-info-for-rewarded-nfts account account-info)
            (bind (get-policy token)
            { "owner":=owner:string
            }
            (enforce (= (+ amount (at "supply" token)) 1.0) "Exceeds max supply")
            (update-owner (at "id" token) account))
            true
    )

    (defun enforce-mint-gen-0-rewards:bool
      ( token:object{token-info}
        account:string
        guard:guard
        amount:decimal
        account-info:object{account-wl-record-schema})
            (require-capability (n_7d47538766e6f80008f253dccd30451f4d483c38.gen-0-rewards-orchestrator.REWARD-USER))
            (update-account-info-for-rewarded-nfts account account-info)
            (bind (get-policy token)
            { "owner":=owner:string
            }
            (enforce (= (+ amount (at "supply" token)) 1.0) "Exceeds max supply")
            (update-owner (at "id" token) account))
            true
    )
    
    
    (defun enforce-mint-internal-public 
      ( token:object{token-info}
        account:string
        guard:guard
        amount:decimal
        account-info:object{account-wl-record-schema})
            (require-capability (MINT (at "id" token) account))
            (require-capability (PRIVATE))
            (let*
                (
                (nft-id (at "id" token))
                (split-id (at 1 (free.util-strings.split ":" nft-id)))
                (split-id-number (str-to-int split-id))

                ;;(current-phase (get-current-phase))
                (current-phase (if (= account REWARDS_ADDRESS) PUBLIC_PHASE (get-current-phase)))
                (isPublicPhase:bool (if (= current-phase PUBLIC_PHASE) true false))
                (can-mint:bool (= true isPublicPhase))
                (price:decimal (get-price-by-phase current-phase))
                )
                (enforce (> split-id-number 9900) "ID not slated for public mint")
                (enforce (= true can-mint) "Mint is not live right now, or is Paused.")
    
                (update-account-info account account-info false)
                (bind (get-policy token)
                { "owner":=owner:string
                }
                (enforce (= (+ amount (at "supply" token)) 1.0) "Exceeds max supply")
                (update-owner (at "id" token) account)
                )
                (increment-total-public-minted)
                (if
                    (or (= account ADMIN_ADDRESS) (= account PROVIDER_ADDRESS) )
                    (enforce (= true true ) "no op")
                    (if
                    (> price 0.0)
                    (pay-mint account price)
                    (enforce-guard (get-user-guard account guard)))
                )
            )
    )

    (defun enforce-mint-g0-rewards-v2
        ( token:object{token-info}
          account:string
          guard:guard
          amount:decimal
          account-info:object{account-wl-record-schema})
                (require-capability (n_7d47538766e6f80008f253dccd30451f4d483c38.gen-0-rewards-orchestrator.REWARD-USER))
                (require-capability (MINT (at "id" token) account))
                (require-capability (PRIVATE))
                (let*
                    (
                        (nft-id (at "id" token))
                        (split-id (at 1 (free.util-strings.split ":" nft-id)))
                        (split-id-number (str-to-int split-id))
                    )
                    (enforce (< split-id-number 9651) "ID not slated for staking")
                    (update-account-info-for-rewarded-nfts account account-info)
                    (bind (get-policy token)
                        { "owner":=owner:string}
                        (enforce (= (+ amount (at "supply" token)) 1.0) "Exceeds max supply")
                        (update-owner (at "id" token) account)
                    )
                    true
        )
    )

    (defun enforce-mint-bh-rewards-v2
        ( token:object{token-info}
          account:string
          guard:guard
          amount:decimal
          account-info:object{account-wl-record-schema})
                (require-capability (n_7d47538766e6f80008f253dccd30451f4d483c38.battle-heroes-rewards-orchestrator.REWARD-USER))
                (require-capability (MINT (at "id" token) account))
                (require-capability (PRIVATE))
                (let*
                    (
                        (nft-id (at "id" token))
                        (split-id (at 1 (free.util-strings.split ":" nft-id)))
                        (split-id-number (str-to-int split-id))
                    )
                    (enforce (< split-id-number 9651) "ID not slated for staking")
                    (update-account-info-for-rewarded-nfts account account-info)
                    (bind (get-policy token)
                        { "owner":=owner:string}
                        (enforce (= (+ amount (at "supply" token)) 1.0) "Exceeds max supply")
                        (update-owner (at "id" token) account)
                    )
                    true
        )
    )


    (defun pay-mint (account-minting:string price:decimal)
        (coin.transfer account-minting ADMIN_ADDRESS price)
        true
    )
    
    (defun update-account-info (account:string account-info:object{account-wl-record-schema} shouldDeductFreeMints:bool)
        (require-capability (UPDATE-WL))
        (let*
            (
                (free-mints-remaining (at "free-mints-remaining" account-info))
                (free-mints-granted (at "free-mints-granted" account-info))
    
            )
            (write account-wl-records account
                { "minted-total" : (+ 1 (at "minted-total" account-info))
                ,"account" : account
                ,"free-mints-granted" : free-mints-granted
                ,"free-mints-remaining" : (if (= true shouldDeductFreeMints) (- free-mints-remaining 1) free-mints-remaining)
            }
            )
        )
    )

    (defun update-account-info-for-rewarded-nfts (account:string account-info:object{account-wl-record-schema})
        (require-capability (UPDATE-WL))
        (let*
            (
                (free-mints-remaining (at "free-mints-remaining" account-info))
                (free-mints-granted (at "free-mints-granted" account-info))
    
            )
            (write account-wl-records account
                { "minted-total" : (+ 1 (at "minted-total" account-info))
                ,"account" : account
                ,"free-mints-granted" : (+ free-mints-granted 1)
                ,"free-mints-remaining" : free-mints-remaining
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
                    (fungible:module{fungible-v2} (at "fungible" spec))
                    (creator:string (at "creator" spec))
                    (creator-guard:guard (at "creator-guard" spec))
                    (royalty-rate:decimal (at "royalty-rate" spec))
                    (creator-details:object (fungible::details creator ))
                    (created-total:integer (get-created-supply))
                    (created-total-after (+ 1 created-total))
                    )
            (increment-total-created)
            (enforce (=
                (at "guard" creator-details) creator-guard)
                "Creator guard does not match")
            (enforce (and
                (>= royalty-rate 0.0) (<= royalty-rate 1.0))
                "Invalid royalty rate")
            (enforce (<= created-total-after MAX-UNIQUE-SUPPLY) "Exceeded max unique collection supply")
            (insert policies (at "id" token)
                { "fungible": fungible
                , "creator": creator
                , "creator-guard": creator-guard
                , "owner": ""
                , "token-id": (at "id" token)
                , "is-locked":false
                , "lock-change-time": (at "block-time" (chain-data))
                , "royalty-rate": royalty-rate }))
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
        (enforce-ledger)
    
        (enforce-sale-pact sale-id)
        (bind (get-policy token)
        { "fungible" := fungible:module{fungible-v2}
            ,"royalty-rate":= royalty-rate:decimal
            ,"creator":= creator:string
        }
        (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
                (price:decimal (at "price" spec))
                (recipient:string (at "recipient" spec))
                (recipient-guard:guard (at "recipient-guard" spec))
                (recipient-details:object (fungible::details recipient))
                (sale-price:decimal (* amount price))
                (royalty-payout:decimal
                    (floor (* sale-price royalty-rate) (fungible::precision)))
                )
            (fungible::enforce-unit sale-price)
            (enforce (< 0.0 price) "Offer price must be positive")
            (enforce (=
            (at "guard" recipient-details) recipient-guard)
            "Recipient guard does not match")
            (insert quotes sale-id { "id": (at "id" token), "spec": spec })
            (emit-event (QUOTE sale-id (at "id" token) amount price sale-price royalty-payout creator spec)))
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
        { "fungible" := fungible:module{fungible-v2}
        , "creator":= creator:string
        , "royalty-rate":= royalty-rate:decimal
        }
        (with-read quotes sale-id { "id":= qtoken, "spec":= spec:object{quote-spec} }
            (enforce (= qtoken (at "id" token)) "incorrect sale token")
            (bind spec
            { "price" := price:decimal
            , "recipient" := recipient:string
            }
            (let* ((sale-price:decimal (* amount price))
                    ;(royalty-payout:decimal (+ 0.0 0.0)) 
                    (royalty-payout:decimal
                      (floor (* sale-price royalty-rate) (fungible::precision)))
                    (payout:decimal (- sale-price royalty-payout)) )
                (if
                (> royalty-payout 0.0)
                (fungible::transfer buyer ADMIN_ADDRESS royalty-payout)
                "No royalty")
                (fungible::transfer buyer recipient payout)))
                (with-capability (UPDATE-OWNER (at "id" token) buyer)
                    (enforce (= 1.0 amount) "Invalid amount for single supply NFT Collection")
                    (update-owner (at "id" token) buyer)
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
        (with-capability (UPDATE-OWNER (at "id" token) receiver)
            (enforce (= 1.0 amount) "Invalid amount for single supply NFT Collection")
            (update-owner (at "id" token) receiver)
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
        (update-owner (at "id" token) receiver)
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
            {"owner": new-owner}
        )
        true
    )

    (defun back-fill-owner (token-id:string new-owner:string)
        @doc "Updates token with new owner"
        (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")))
        (update policies token-id
            {"owner": new-owner}
        )
        true
    )
    
    (defun add-bool-consts:bool (key:string val:bool)
        @doc "Adds entry to boolean constants"
        (with-capability (GOVERNANCE)
        (insert bool-consts key
            {"val": val}
        )
        )
    )
    
    (defun add-integer-consts:bool (key:string val:bool)
        @doc "Adds entry to boolean constants"
        (with-capability (GOVERNANCE)
        (insert bool-consts key
            {"val": val}
        )
        )
    )
    
    (defun update-bool-consts:bool (key:string val:bool)
        @doc "Adds entry to boolean constants"
        (with-capability (GOVERNANCE)
        (update bool-consts key
            {"val": val}
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
    (defun increment-total-public-minted:bool ()
        @doc "increments total minted amount"
        
        (require-capability (INCREMENT-MINTED))
        (update integer-consts TOTAL-PUBLIC_MINTED-KEY {
            "val": (+ (get-integer-consts TOTAL-PUBLIC_MINTED-KEY) 1)
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
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; WL Bulk Set
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (defun add-private-wl-bulk (wl-list:list)
        (map (add-private-wl-wrapper) wl-list)
    )
    (defun add-private-wl-wrapper (wl:object)
        (let*
            (
            (account:string (at "account" wl))
            (additionalFreeMints (at "additionalFreeMints" wl))
            )
            (add-private-wl-or-role account additionalFreeMints)
    
        )
    )
    (defun add-private-wl-or-role (account:string additionalFreeMints:integer)
        (with-capability (GOVERNANCE)
        (bind (get-account-whitelist-info account)
            {
            "free-mints-granted":=free-mints-granted,
            "free-mints-remaining":=free-mints-remaining,
            "minted-total":= minted-total,
            "account":= account
            }
            (write account-wl-records account {
            "minted-total": minted-total,
            "account": account,
            "free-mints-granted": (+ free-mints-granted additionalFreeMints),
            "free-mints-remaining": (+ free-mints-remaining additionalFreeMints)
            })
            "new Free WL Spots added by Admin"
        )
        )
    )
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; WL Bulk Set : Keeps same Role
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun bulk-add-private-wl-only (wl-list:list)
        (map (bulk-add-private-wl-only-wrapper) wl-list)
    )
    (defun bulk-add-private-wl-only-wrapper (wl:object)
        (let*
            (
            (account:string (at "account" wl))
            (additionalFreeMints (at "additionalFreeMints" wl))
            )
            (add-private-wl-only account additionalFreeMints)
        )
    )
    (defun add-private-wl-only (account:string additionalFreeMints:integer)
        (with-capability (GOVERNANCE)
        (bind (get-account-whitelist-info account)
            {
            "free-mints-granted":=free-mints-granted,
            "free-mints-remaining":=free-mints-remaining,
            "minted-total":= minted-total,
            "account":= account
            }
            (write account-wl-records account {
            "minted-total": minted-total,
            "account": account,
            "free-mints-granted": (+ free-mints-granted additionalFreeMints),
            "free-mints-remaining": (+ free-mints-remaining additionalFreeMints)
            })
            "new WL Spots added by Admin"
        )
        )
    )
    
    
    
    (defun bulk-add-wl-records (wl-list:list)
        (map (add-account-records) wl-list)
    )
    
    (defun add-account-records (wl)
        (with-capability (GOVERNANCE)
            (write account-wl-records (at "account" wl) {
            "minted-total": (at "minted-total" wl) ,
            "role": (at "role" wl),
            "account": (at "account" wl),
            "free-mints-granted": (at "free-mints-granted" wl),
            "free-mints-remaining": (at "free-mints-remaining" wl)
            })
            "wl++"
        )
    )
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Utility Getters
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun get-account-whitelist-info:object{account-wl-record-schema} (account:string)
        (with-default-read account-wl-records account
        {
            "account": account,
            "free-mints-granted":0,
            "free-mints-remaining":0,
            "minted-total": 0
        }
        {
            "account":= account,
            "free-mints-granted":=free-mints-granted,
            "free-mints-remaining":=free-mints-remaining,
            "minted-total":= minted-total
    
        }
        {
            "free-mints-granted":free-mints-granted,
            "free-mints-remaining":free-mints-remaining,
            "minted-total": minted-total,
            "account": account
        }
        )
    )
    
    (defun get-user-guard (account:string supplied-guard:guard)
        @doc "Get user guard from coin account on curr chain"
        (let*
        (
            (guard (at "guard" (coin.details account)))
            )
            ;;Validate that Guard passed matched set guard.
            (enforce (= guard supplied-guard) "Supplied Guard does not match")
            guard
        )
    )
    
    (defun get-price-by-phase:decimal(current-phase:string)
        (cond
            ((= PUBLIC_PHASE current-phase) 2.91)
            10000000.0)
    )
    
    (defun get-nfts-by-owner:list(account:string)
        @doc "All NFTs by owner"
        (select policies ["token-id","is-locked"] (where "owner" (= account)))
    )

    (defun get-bool-consts:bool (key:string)
        @doc "Get bool constants with false as default"
        (with-default-read bool-consts key
        { "val" : false }
        { "val" := s }
        s)
    )
    (defun get-integer-consts:integer (key:string)
        @doc "Get integer constants with -1 as default"
        (with-default-read integer-consts key
        { "val" : -1 }
        { "val" := s }
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
        { "val" : true }
        { "val" := s }
        s)
    )
    
    (defun get-minted-supply:integer ()
        (at "val" (read integer-consts TOTAL-MINTED-KEY))
    )

    (defun get-public-minted-supply:integer ()
        (at "val" (read integer-consts TOTAL-PUBLIC_MINTED-KEY))
    )

    (defun get-staked-supply:integer ()
        (at "val" (read integer-consts TOTAL-STAKED-KEY))
    )

    (defun get-created-supply:integer ()
        (at "val" (read integer-consts TOTAL-CREATED-KEY))
    )
    
    (defun get-current-phase:string()
        @doc "This method checks which minting phase is open based on time windows and minted supply"
        (let*
            (
                (isPublicStarted (check-is-mint-live-for-phase PUBLIC_MINT_TIME))
                (post-increment-mint-supply:integer (get-public-minted-supply))
            )
            (cond
                ( (and (= true isPublicStarted) (<= post-increment-mint-supply 100)) PUBLIC_PHASE)
                "Mint is not Live for you now!")
        )
    )
    
    (defun get-all-wl-keys ()
            (keys account-wl-records)
        )
        (defun get-wl-details (id:string)
            (read account-wl-records id)
        )
    
    
        (defun get-all-wl-details ()
            (let*
            ((all-wl-keys:list (get-all-wl-keys))
            (all-objs (map (get-wl-details) all-wl-keys))
            )
            all-objs
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
    
            )
            (if (and (<= minted-supply max-phase-cap) (>= (at "block-time" (chain-data)) (time phase-time))) true false)
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
                (user-coin-guard:guard (at "guard" (coin.details account)))
                (nfts:list (get-nfts-by-owner account))
                (has-nft:bool (contains {"token-id":nft-id} nfts))
            )
            (enforce-guard user-coin-guard)
            (enforce (= true has-nft) "NFT not owned by user, access not granted")
        )
    )
    (defun validate-guard:bool (account:string)
        (enforce-guard (at "guard" (coin.details account)))
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
        ;;(map (at "owner") all-objs)
        (map (at "owner") all-objs)
        )
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
        (require-capability (PRIVATE))
        (enforce-can-transact token-id)
        (let*
            (
                (owner (at "owner" (read policies token-id ["owner"])))
            )
            (with-capability (ACCOUNT-GUARD owner)
                (update policies token-id
                    {"is-locked": true,
                        "lock-change-time": (at "block-time" (chain-data))
                    }
                )
        
                (update integer-consts TOTAL-STAKED-KEY 
                    {"val": (+ (get-integer-consts TOTAL-STAKED-KEY) 1)}
                )
            )
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
            (owner (at "owner" (read policies token-id ["owner"])))
            (is-locked (is-locked token-id))
        )
        (enforce (= true is-locked) "NFT is already unlocked")
        (with-capability (ACCOUNT-GUARD owner)
            (update policies token-id
            {"is-locked": false,
                "lock-change-time": (at "block-time" (chain-data))}
            )
            (update integer-consts TOTAL-STAKED-KEY {
            "val": (- (get-integer-consts TOTAL-STAKED-KEY) 1)
            })
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
        (at "is-locked" (read policies token-id ["is-locked" "lock-change-time"]))
    )
    (defun time-locked(token-id:string)
        @doc "checks if an NFT is locked."
        (at "lock-change-time" (read policies token-id ["is-locked" "lock-change-time"]))
    )
) 
