(module kadena-mining-club GOVERNANCE
  @doc "Kadena Mining Club mint contract."
    (use coin)
    (use lago.kwUSDC) ;deprecated
; ============================================
; ==               CONSTANTS                ==
; ============================================
    (defconst TEST "test")
    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ACCOUNTS_CREATED_COUNT "accounts-created-count")
    (defconst FOUNDERS_CREATED_COUNT "founders-count")
    (defconst MINERS_CREATED_COUNT "miners-count")
    (defconst MINERS_MINTED_COUNT "miners-minted-count") ;the count for front end interface, phase 2
    (defconst CURRENT_MINER_ID_COUNT "current-miner-id-count")
    (defconst WHITELIST_MINTED_COUNT "whitelist-minted-count")
    (defconst MINER_URIS_CREATED_COUNT "miner-uris-count")
    (defconst GAS_PER_NFT_MINTED "gas")

    (defconst FOUNDERS_PRICE_KEY "founders-price-key")
    (defconst PRICE_KEY "price-key")
    (defconst WHITELIST_PRICE_KEY "whitelist-price-key")
    (defconst USDC_PRICE_KEY "usdc-price-key")
    
    (defconst MINERS_URI_KEY "miners-uri-key")
    (defconst MINT_CHAIN_ID "mint-chain-id")
    (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
    
    (defconst MINT_STATUS "mint-status")
    (defconst MINT_PAUSED "mint-paused")
    (defconst MINT_STARTED "mint-started")
    
    (defconst FOUNDERS_MAX_SUPPLY 420 "The maximum supply of the founders pass NFTs")
    (defconst MAX_SUPPLY 10000 "The max supply of generation 0 miners")
    (defconst WHITELIST_MAX_MINT 1500 "The maximum number of mints that can be done at the whitelist price")
    
    (defconst MINT_WALLET "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad") 
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst CREATOR_FUND "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst DISCORD_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")
    (defconst FOUNDERS_FUND "k:0ab2f447374b4968abd6e689b9fb00e7e82ffd99bb0543084e9eeaba10651f92")
    (defconst AIRDROP_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")

    (defconst MINIMUM_PRECISION 0 
      "Specifies the minimum denomination for token transactions.")
    (defconst uACCOUNT_ID_CHARSET CHARSET_LATIN1
      "Allowed character set for Account IDs.")
    (defconst uACCOUNT_ID_MIN_LENGTH 3
      "Minimum character length for account IDs.")
    (defconst uACCOUNT_ID_MAX_LENGTH 256
      "Maximum character length for account IDs.")

; ============================================
; ==             CAPABILITIES               ==
; ============================================

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap AIRDROP_ACCOUNT (account:string)
        @doc "Only allows the AIRDROP wallet to call these functions"
        (enforce (= account AIRDROP_ADDRESS) "Airdrop account only")
        (compose-capability (ACCOUNT_GUARD AIRDROP_ADDRESS))
    )

    (defcap DISCORD(account:string)
        @doc "Only allows the discord wallet to send information"
        (enforce (= account DISCORD_ADDRESS) "only the administator discord wallet can call this function")
        (compose-capability (ACCOUNT_GUARD DISCORD_ADDRESS))
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )

    (defcap FOUNDERS_OWNER (account:string id:string)
        @doc "Enforces that an account owns the particular miner ID"
        (let
            (
                (nft-owner (at "owner-address" (read fledger id ["owner-address"])))
            )
            (enforce (= nft-owner account) "Account is not owner of the NFT")
                (compose-capability (ACCOUNT_GUARD account))
        )
    )

    (defcap MINER_OWNER (account:string id:string)
        @doc "Enforces that an account owns the particular miner ID"
        (let
            (
                (nft-owner (at "owner-address" (read mledger id ["owner-address"])))
            )
            (enforce (= nft-owner account) "Account is not owner of the NFT")
                (compose-capability (ACCOUNT_GUARD account))
        )
    )

    (defcap MINT_FOUNDER (id:string account:string)
        @doc "Emitted event when an NFT is purchased"
        @event true
    )
    
    (defcap LIST_FOUNDER (nft-id:string owner:string price:decimal for-sale:bool)
        @doc " Emitted event when a Founders pass is sold "
        @event true
    )

    (defcap BUY_FOUNDER (nft-id:string buyer:string seller:string price:decimal)
      @doc " Emitted event when a Founders pass is purchased "
      @event true
    )

    (defcap TRANSFER_FOUNDER (id:string sender:string receiver:string amount:decimal)
        @doc "Allows transferring of NFTs between two accounts"
        @event true
    )

    (defcap MINT_MINER (id:string account:string)
        @doc "Emitted event when a Miner NFT is purchased"
        @event true
    )
    
    (defcap LIST_MINER (nft-id:string owner:string price:decimal for-sale:bool)
        @doc " Emitted event when a Miner is sold "
        @event true
    )

    (defcap BUY_MINER (nft-id:string buyer:string seller:string price:decimal)
      @doc " Emitted event when a Miner is purchased "
      @event true
    )

    (defcap TRANSFER_MINER (id:string sender:string receiver:string amount:decimal)
        @doc "Allows transferring of NFTs between two accounts"
        @event true
    )

    (defcap TURN_OFF_MINER (id:string)
        @doc "Emitted event when a Miner NFT is turned off"
        @event true
    )

    (defcap TURN_ON_MINER (id:string)
        @doc "Emitted event when a Miner NFT is turned on"
        @event true
    )

    (defcap AIRDROP (account:string)
        @doc "Emitted event when an airdrop to an account occurs"
        @event true
    )

    (defcap PRIVATE ()
        true
    )

; ============================================
; ==            SCHEMA AND TABLES           ==
; ============================================

    (defschema wl-schema
        @doc "Basic schema used for WL members, keys are account ids"
        role:string
    )

    (defschema airdrop-schema
        airdrop:list
    )

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )

    (defschema price-schema
        @doc "Stores the price of each type of NFT or upgrade"
        price:decimal
    )

    (defschema values-schema
        @doc "Schema used for storing basic values"
        value:string
    )

    (defschema mint-schema
        status:string
    )

    (defschema uri-schema
        @doc "A schema to store all URIs before mint"
        uri:string 
    )

    (defschema wl-tracker-schema
        @doc "stores how many whitelist mints one account has remaining"
        wl-mints-remaining:integer
    )
      
    ;every entry is one NFT, to be stored on the ledger fledger for Founders Ledger
    (defschema fentry
        @doc "stores information of the Founders NFT collection"
        founders-nft-id:string
        owner-address:string
        uri:string
        airdrops-remaining:integer
        free-mints-remaining:decimal
        market-price:decimal
        unique-id:string
        for-sale:bool
    )

    ; every entry is one NFT, to be stored on the ledger "mledger"
    (defschema entry
        ;add market conditions
        @doc "Stores information of the Miners NFT collection"
        nft-id:string
        generation:integer
        owner-address:string
        uri:string
        hashrate:decimal ;hashrate is only updated once per payment cycle
        tied-asic:list
        mint-date:time
        special-attributes:string
        staked:bool 
        staked-unstaked:list
        staked-time:time
        for-sale:bool
        market-price:decimal
        updated-at:time
    )

    (deftable wl-remaining-per-account:{wl-tracker-schema})
    (deftable miner-uri-table:{uri-schema})
    (deftable mledger:{entry}) ;mledger stands for Miners Ledger. Contains info for all 10,000 Miners
    (deftable wl:{wl-schema})
    (deftable fledger:{fentry})
    (deftable counts-table:{counts-schema})
    (deftable price-table:{price-schema})
    (deftable values-table:{values-schema})
    (deftable mint-status:{mint-schema})
    (deftable airdrop-table:{airdrop-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        ; (insert values-table GAS_PER_NFT_MINTED {"value": "300" })
        ; (insert counts-table MINERS_CREATED_COUNT {"count": 1})
        ; (insert counts-table MINER_URIS_CREATED_COUNT {"count": 1})
        ; (insert counts-table MINERS_MINTED_COUNT {"count": 1501}) ;starting at 1501 for aug 13th mint
        ; (insert counts-table WHITELIST_MINTED_COUNT {"count": 1})
        ; (insert price-table WHITELIST_PRICE_KEY {"price": 0.1})
        ; (insert price-table PRICE_KEY {"price": 0.3})
        (insert price-table TOTAL_VOLUME_KEY {"price": 130600.5864})
        ; (insert counts-table MINER_URIS_CREATED_COUNT {"count": 1})
        ; (insert counts-table ACCOUNTS_CREATED_COUNT {"count": 0})
        ; (insert price-table FOUNDERS_PRICE_KEY {"price": 0.2})
        ; (insert counts-table FOUNDERS_CREATED_COUNT {"count": 0})
        ; (insert price-table TOTAL_VOLUME_KEY {"price": 0.0})
        ; (insert values-table MINT_CHAIN_ID {"value": "2"})
        ; (insert mint-status MINT_STATUS {"status": MINT_STARTED})
    )

; ============================================
; ==              MINT FUNCTIONS            ==
; ============================================
    ; (key) inputs are any of the following
        ; "accounts-created-count"
        ; "miners-count"
        ; "miner-uris"
        ; "founders-count"
        ; "whitelist-minted-count"
    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counts-table key ['count]))
    )
        
    (defun set-price(key:string value:decimal)
        @doc "Sets the price for a key to store in the price-table"
        (with-capability (ADMIN)
            (update price-table key 
                {"price": value} 
            )
        )
    )

    (defun set-count(key:string value:integer)
        @doc "Sets the count for a key to store in the counts-table"
        (with-capability (ADMIN)
            (update counts-table key 
                {"count": value} 
            )
        )
    )
    (defun mint-miner-multiple (account:string number-to-mint:integer usdc:bool)
        @doc "Allows any valid account to mint Miner NFTs"
        (enforce (<= (time "2022-08-27T17:00:00Z") (at "block-time" (chain-data))) "Minting will be enabled on August 27 17:00 UTC")
        ; (enforce (<= (time "2022-08-27T19:00:00Z") (at "block-time" (chain-data))) "public Minting will be enabled on August 27 17:00 UTC")
        (let ((mint-chain-id (get-value MINT_CHAIN_ID)))
            (enforce (= (curr-chain-id) mint-chain-id) "Can only mint on chain 2"))
        (enforce (> number-to-mint 0) "You must mint at least 1 NFT")
        (validate-account-id account)
        (enforce-coin-account-exists account)
        (with-default-read counts-table CURRENT_MINER_ID_COUNT ;starts at 01
            { 'count: 0.0 }
            { 'count := current-count }
            (enforce (>= 10000 (+ current-count number-to-mint)) (format "Max supply exceeded, current supply is {}" [current-count]))
        )

        (with-capability (ACCOUNT_GUARD account)
            (with-default-read price-table PRICE_KEY
                { 'price: 0.0 }
                { 'price := price_1 }
                (if (= true usdc) (lago.kwUSDC.transfer account MINT_WALLET (* 300.0 number-to-mint)) (coin.transfer account MINT_WALLET (* number-to-mint price_1)) )
            )
            (with-capability (PRIVATE)
                (map 
                    (mint-miner)
                    (make-list number-to-mint account)
                )
            )
        )
        (format "{} NFTs minted for address {}" [number-to-mint, account])
    )

    (defun mint-miner (account:string)
        @doc "Moves a miner NFT from the zero address to a new owner"
        (require-capability (PRIVATE))
        (require-capability (ACCOUNT_GUARD account))
        (let 
            ( 
              (miner-id (int-to-str 10 (get-count CURRENT_MINER_ID_COUNT)))
              (new-uri (get-initialized-miner-uri (int-to-str 10 (get-count CURRENT_MINER_ID_COUNT))))
            )
            (update mledger miner-id
            { "owner-address" : account
            , "mint-date": (at "block-time" (chain-data))
            , "uri": new-uri
            , "staked-time": (at "block-time" (chain-data))
            , "updated-at": (at "block-time" (chain-data)) })
            (increase-count CURRENT_MINER_ID_COUNT)
            (emit-event (MINT_MINER miner-id account))
        )
    )

    (defun admin-mint-miner-multiple (accounts:list)
        @doc "mints multiple miners to one address, only callable by ADMIN"
        (with-capability (ACCOUNT_GUARD MINT_WALLET)
            (with-capability (ADMIN)
                (with-capability (PRIVATE)
                    (map 
                        (admin-mint-miner)
                        accounts
                    )
                )
            )
        )
        (format "NFTs minted for addresses {}" [accounts])
    )

    (defun admin-mint-miner (account:string)
        @doc "mints a miner for a second address, only callable by ADMIN"
        (require-capability (PRIVATE))
        (with-capability (ADMIN)
            (let 
                ( 
                  (miner-id (int-to-str 10 (get-count CURRENT_MINER_ID_COUNT)))
                  (new-uri (get-initialized-miner-uri (int-to-str 10 (get-count CURRENT_MINER_ID_COUNT))))
                )
                (update mledger miner-id
                    { "owner-address" : account 
                    , "mint-date": (at "block-time" (chain-data)) 
                    , "uri": new-uri
                    , "staked-time": (at "block-time" (chain-data))
                    , "updated-at": (at "block-time" (chain-data)) })
                (increase-count CURRENT_MINER_ID_COUNT)
                (emit-event (MINT_MINER miner-id account))
            )
        )
    )

    (defun remove-free-mint-founder-multiple (id:list)
        (with-capability (ADMIN)
            (with-capability (PRIVATE)
                (map 
                    (remove-free-mint-founder)
                    id
                )
            )
        )
    )

    (defun remove-free-mint-founder (id:string)
        (with-capability (ADMIN)
            (update fledger id
                { "free-mints-remaining" : 0.0 })
        )
    )

    (defun admin-swap-id-mint-wallet (ending-account:string nft-id:string )
        (with-capability (ADMIN)
            (update mledger nft-id
                { "owner-address" : ending-account })
        )
    )

; ============================================
; ==       State-modifying functions        ==
; ============================================

    (defun airdrop ( calling-account:string account:string nft-ids:list item:list payout:decimal)
        @doc "update the airdrop table, and update staked-time for all staked NFTs owned by one account"
        (with-capability (AIRDROP_ACCOUNT calling-account)
            (coin.transfer calling-account account payout)
            (with-capability (PRIVATE)
                (map
                    (update-staked-time)
                    nft-ids
                )
                (update-airdrop-table account item)
            )
        )
    )

    (defun airdrop-admin ( calling-account:string account:string nft-ids:list item:list)
        @doc "update the airdrop table, and update staked-time for all staked NFTs owned by one account"
        (with-capability (ADMIN)
            (with-capability (PRIVATE)
                (map
                    (update-staked-time)
                    nft-ids
                )
            (update-airdrop-table account item)
            )
        )
    )

    (defun airdrop-transfer (calling-account:string account:string payout:decimal )
        (coin.transfer calling-account account payout)
    )

    (defun update-staked-time (nft-id:string)
        @doc "update the time an NFT was staked after an airdrop or claim occurs"
        (require-capability (PRIVATE))
        (update mledger nft-id
            { "staked-time": (at "block-time" (chain-data))  })
    )

    (defun update-airdrop-table (account:string item:list)
        (require-capability (PRIVATE))
        (let 
            ( 
              (exists (try false (let ((ok true)) (with-read airdrop-table account {'airdrop := airdrop-temp1}"") ok)))
            )
            (if (= exists true) 
                (update-airdrop account item)
                (add-airdrop account item)
            )
            (format "airdropped {} " [item])
        )
    )

    (defun add-airdrop (account:string item:list)
        (insert airdrop-table account
          { "airdrop": item}
        )
    )

    (defun update-airdrop (account:string item:list)
        (with-read airdrop-table account
            {'airdrop := airdrop-temp }
            (update airdrop-table account
                {"airdrop": (+ airdrop-temp item) }
            )
        )
    )

    (defun turn-off-miner-multiple1 (nft-ids:list account:string guard:guard)
        (with-capability (PRIVATE)
            (map 
                (turn-off-miner)
                nft-ids
            )
        )
        (let ((num-miners (length nft-ids)))
            (format "Successfully turned off {} miners" [num-miners])
        )
    )

    (defun turn-off-miner (nft-id:string)
        (require-capability (PRIVATE))
        (with-capability (MINER_OWNER (get-owner-mledger nft-id) nft-id)
            (update mledger nft-id
                {"staked": false }))
        (emit-event (TURN_OFF_MINER nft-id))
    )

    (defun turn-on-miner-multiple1 (nft-ids:list account:string guard:guard)
        (let ((exist (coin-account-exists account)))
            (if (= exist true) "" (coin.create-account account guard))
        )
        (with-capability (PRIVATE)
            (map 
                (turn-on-miner1)
                nft-ids
            )
        )
        (let ((num-miners (length nft-ids)))
            (format "Successfully turned on {} miners" [num-miners])
        )
    )

    (defun turn-on-miner1 (nft-id:string)
        (require-capability (PRIVATE))

        (with-read mledger nft-id
          { 'for-sale := for-sale }
          (enforce (= for-sale false) "Your NFT is listed on the marketplace, please de-list before turning on your miner")
        )
        (with-capability (MINER_OWNER (get-owner-mledger nft-id) nft-id)
            (update mledger nft-id
                {"staked": true
                ,"staked-time": (at "block-time" (chain-data))}))
        (emit-event (TURN_ON_MINER nft-id))
    )

    (defun increase-count (key:string)
        ;increase the count of a key in a table by 1
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )

    (defun set-staked-time-multiple (nft-ids:list tiempo:string)
        (with-capability (PRIVATE)
        (with-capability (ADMIN)
            (map 
                (set-staked-time tiempo)
                nft-ids
            )
        ))
    )

    (defun set-staked-time (tiempo:string nft-id:string)
        @doc "initializes the staked-time variable in mledger for one NFT"
        (with-capability (ADMIN)
          (update mledger nft-id {"staked-time": (time tiempo)})
        )
    )


    (defun set-value(key:string value:string)
        @doc "Sets the value for a key to store in a table"
        (with-capability (ADMIN)
            (update values-table key 
                {"value": value} 
            )
        )
    )

    (defun set-wl-manual(key:string value:integer)
        (with-capability (ADMIN)
            (update wl-remaining-per-account key 
                {"wl-mints-remaining": value} 
            )
        )
    )

    (defun add-to-wl-for-role (wl-mints-remaining:integer accounts:list calling-account:string)
        @doc "Adds wl users with a role"
        (enforce
            (contains wl-mints-remaining [1, 2, 3, 5, 10])
              "Must specify a valid role for adding WL members"
        )
        (with-capability (DISCORD calling-account)
            (map (add-to-wl wl-mints-remaining calling-account) accounts)
        )
    )
    
    (defun add-to-wl (wl-mints-remaining:integer calling-account:string account:string )
        @doc "Adds a user to a wl"
        (require-capability (DISCORD calling-account))
        (let 
            ( 
              (role-exists (try false (let ((ok true)) (get-wl-mints-remaining account) ok)))
            )
            (if (= role-exists true) (update wl-remaining-per-account account {"wl-mints-remaining": wl-mints-remaining}) (insert wl-remaining-per-account account {"wl-mints-remaining": wl-mints-remaining}))
        )
    ) 

     (defun manual-add-to-wl-for-role (wl-mints-remaining:integer accounts:list)
        @doc "Adds wl users with a role"
        (enforce
            (contains wl-mints-remaining [1, 2, 3, 5, 10])
              "Must specify a valid role for adding WL members"
        )
        (with-capability (ADMIN)
            (map (manual-add-to-wl wl-mints-remaining) accounts)
        )
    )   
    
    (defun manual-add-to-wl (wl-mints-remaining:integer account:string)
        @doc "Allows Admin to manually add a person to whitelist"
        (with-capability (ADMIN)
            (let 
                ( 
                  (role-exists (try false (let ((ok true)) (get-wl-mints-remaining account) ok)))
                )
                (if (= role-exists true) (update wl-remaining-per-account account {"wl-mints-remaining": wl-mints-remaining}) (insert wl-remaining-per-account account {"wl-mints-remaining": wl-mints-remaining}))
            )
        )
    )

    (defun set-miner-uri-hidden-table-multiple (new-uris:list)
        (with-capability (ADMIN)
            (with-capability (PRIVATE)
                (map 
                    (set-miner-uri-hidden-table)
                    new-uris
                )
            )
        )
    )

    (defun set-miner-uri-hidden-table (new-uri:string)
        (require-capability (PRIVATE))
        (require-capability (ADMIN))
        (write miner-uri-table (int-to-str 10 (get-count MINER_URIS_CREATED_COUNT))
            {"uri": new-uri} 
        )
        (if (> (get-count CURRENT_MINER_ID_COUNT) (get-count MINER_URIS_CREATED_COUNT)) (update mledger (int-to-str 10 (get-count MINER_URIS_CREATED_COUNT)) {"uri": new-uri}) "")
        (increase-count MINER_URIS_CREATED_COUNT)
        
    )
    
; ============================================
; ==     NON STATE-MODIFYING FUNCTIONS      ==
; ============================================

    (defun get-staked-nfts-for-account (account:string)
        @doc "Returns the NFTs that are staked and owned by one account"
        (select mledger ['nft-id]
            (and? (where 'owner-address (= account))
              (where 'staked (= true))))
    )

    (defun get-percent-time-staked-for-nft (nft-id:string airdrop-time:string earliest-staked-time:string)
        @doc "Returns the percent of time an NFT has been staked compared to the earliest possible and next airdrop time"
        (with-read mledger nft-id
          { 'staked-time := staked-time }
            (let 
                (
                  (time-staked (diff-time (time airdrop-time) staked-time))
                  (maximum-time-staked (diff-time (time airdrop-time) (time earliest-staked-time)))
                )
                (/ time-staked maximum-time-staked)
            )
        )
    )

    (defun all-ids ()
        @doc "Returns all the ids"
        (keys mledger) 
    )

    ; (nft-id) inputs are    0<=nft-id<=FOUNDERS_MAX_MINT 
    (defun get-founders-details:string (nft-id:string)
        @doc "Returns the details of a Founder's Pass"
        {
          "uri" : (at "uri" (read fledger nft-id ['uri] ))
        , "founders-nft-id" : (at "founders-nft-id" (read fledger nft-id ['founders-nft-id] ))
        , "owner-address:string" : (at "owner-address" (read fledger nft-id ['owner-address] ))
        , "market-price" : (at "market-price" (read fledger nft-id ['market-price] ))
        , "airdrops-remaining" : (at "airdrops-remaining" (read fledger nft-id ['airdrops-remaining] ))
        , "free-mints-remaining" : (at "free-mints-remaining" (read fledger nft-id ['free-mints-remaining] ))
        , "for-sale" : (at "for-sale" (read fledger nft-id ['for-sale] ))
        }
    )
    
    (defun get-founders-uri:string (nft-id:string)
        (at "uri" (read fledger nft-id ['uri] ))
    )
    
    (defun generate-random-number (nft-id:string)
        (require-capability (PRIVATE))
        (let 
            (
              (x (str-to-int 16 (format-time "%H%M%S%v" (at "block-time" (chain-data)))))
              (y (str-to-int 64 (at 'prev-block-hash (chain-data))))
            )
            (hash (+ (str-to-int 16 nft-id) (+ x y)))
        )
    )
    
    (defun get-unique-founders-id (nft-id:string account:string) 
        @doc "Only callable by the owner of an NFT"
        (with-capability (ACCOUNT_GUARD account) 
            (at "unique-id" (read fledger nft-id ['unique-id] ))
        )
    )

    (defun get-founder-for-sale-true-false:bool (nft-id:string)
        @doc "returns the status of an NFT for sale, true or false."
        (at "for-sale" (read fledger nft-id ['for-sale] ))
    )

    (defun get-miner-for-sale-true-false:bool (nft-id:string)
        @doc "returns the status of an NFT for sale, true or false."
        (at "for-sale" (read mledger nft-id ['for-sale] ))
    )

    (defun get-miner-details:string (nft-id:string)
        @doc "Returns the details of a miner NFT"
        {
          "nft-id" : (at "nft-id" (read mledger nft-id ['nft-id] ))
        , "generation" : (at "generation" (read mledger nft-id ['generation] ))
        , "owner-address" : (at "owner-address" (read mledger nft-id ['owner-address] ))
        , "uri" : (at "uri" (read mledger nft-id ['uri] ))
        , "hashrate" : (at "hashrate" (read mledger nft-id ['hashrate] ))
        , "tied-asic" : (at "tied-asic" (read mledger nft-id ['tied-asic] ))
        , "mint-date" : (at "mint-date" (read mledger nft-id ['mint-date] ))
        , "special-attributes" : (at "special-attributes" (read mledger nft-id ['special-attributes] ))
        , "staked" : (at "staked" (read mledger nft-id ['staked] ))
        , "staked-time" : (at "staked-time" (read mledger nft-id ['staked-time] ))
        , "staked-unstaked" : (at "staked-unstaked" (read mledger nft-id ['staked-unstaked] ))
        , "for-sale" : (at "for-sale" (read mledger nft-id ['for-sale] ))
        , "market-price" : (at "market-price" (read mledger nft-id ['market-price] ))
        }
    )

    (defun get-miner-details-object (nft-id:string)
        @doc "Returns all fields of a miner NFT"
        (read mledger nft-id)
    )

    (defun get-airdrop-details-object (account:string)
        @doc "Returns all fields of a miner NFT"
        (read airdrop-table account)
    )

    ; (nft-id) inputs are    0<=nft-id<MAX_MINT 
    (defun get-owner-mledger (nft-id:string) 
        @doc "Returns the owner of a particular miner in the Miner Ledger"
        (at "owner-address" (read mledger nft-id ['owner-address]))
    ) 

    ; (account) inputs are any valid k:address"
    (defun get-user-miners (account:string)
        @doc "Returns all miners owned by one address"
        (select mledger ["nft-id"] (where "owner-address" (= account)))
    ) 

    (defun ids-owned-by (account:string)
        @doc "Returns all miners owned by one address"
        (select mledger ["nft-id"] (where "owner-address" (= account)))
    ) 

    ; (account) inputs are any valid k:address"
    (defun get-user-miners-object (account:string)
        @doc "Returns all miners owned by one address"
        (select mledger (where "owner-address" (= account)))
    ) 

    (defun get-initialized-miner-uri (id:string)
        (require-capability (PRIVATE))
        (at "uri" (read miner-uri-table id ['uri]))
    ) 
    
    ; (price-key) inputs are any of the following
        ; "price-key"
        ; "whitelist-price-key"
        ; "founders-price-key" 
        ; "total-volume-count-key"
    (defun get-price (price-key:string) 
        @doc "Gets the price for a key"
        (at "price" (read price-table price-key ["price"]))
    )

    (defun get-value (key:string)
        @doc "Gets value for a key"
        (at "value" (read values-table key ['value]))
    )
    
    (defun get-wl-members ()
        @doc "Returns all addresses currently on the miner whitelist table"
        (keys wl)
    )

    (defun get-all-wl-roles ()
        (select wl ["role"] (where "role" (!= "KMCisthebest")))
    )

    (defun get-wl-role (account:string)
        @doc "Returns the highest whitelist role for a specific account"
        (at "role" (read wl account ['role]))
    )

    (defun get-wl-mints-remaining (account:string)
        @doc "Returns the number of whitelist mints remaining"
        (at "wl-mints-remaining" (read wl-remaining-per-account account ['wl-mints-remaining]))
    )

    ; (key) inputs are the same as get-count
    (defun id-for-next-key (key:string)
        @doc "returns the next id for a given key"
        (int-to-str 10 (get-count key))
    )

    (defun get-all-founders-owners ()
        (select fledger ["owner-address"] (where "owner-address" (!= "KMCisthebest")))
    )

    (defun get-all-miner-owners ()
        (select mledger ["owner-address"] (where "owner-address" (!= "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")))
    )

    (defun get-user-founders (account:string)
        @doc "Returns all Founder's Passes owned by one address"
        (select fledger ["founders-nft-id"] (where "owner-address" (= account)))
    )

    (defun get-user-founders-object (account:string)
        @doc "Returns all Founder's Passes owned by one address"
        (select fledger (where "owner-address" (= account)))
    )

    ; (account) inputs are any valid k:address"
    (defun get-num-founders-per-address (account:string)
        @doc "gets the number of founders that one k:address has minted"
        (let 
            (
                (owned-count (length (select fledger ["owner-address"] (where "owner-address" (= account)))))
            )
            owned-count
        )
    )
    
    (defun enforce-max-founders-mint (account:string number-to-mint:integer)
        (if (> (+ (get-num-founders-per-address account) number-to-mint) 2)
            (enforce (> 0 1) "You may only mint a total of two Founder's Passes") "")
    )

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )


; ============================================
; ==           COIN ACCOUNT CHECKS          ==
; ============================================

    (defun enforce-coin-account-exists (account:string)
        (let ((exist (coin-account-exists account)))
            (enforce exist "Account does not exist in coin contract, please send 0.001 KDA to the address on chain 2 and try again."))
    )

    (defun coin-account-exists:bool (account:string)
        (try false
            (let ((ok true))
                (coin.details account)
                ok))
    )

    (defun coin-account-guard (account:string)
        @doc "enforces coin account guard"
        (at "guard" (coin.details account))
    )

    (defun get-coin-guard (account)
       (format "{}" [(at "guard" (coin.details account))])
    )

    (defun key (id:string account:string)
        @doc "returns id/account data structure"
        (format "{}:{}" [id account])
    )   

    ;Enforces rules for account IDs
    (defun validate-account-id ( accountId:string )
        @doc " Enforce that an account ID meets charset and length requirements. "
        (enforce
            (is-charset uACCOUNT_ID_CHARSET accountId)
            (format
            "Account ID does not conform to the required charset: {}"
            [accountId]))
        (let ((accountLength (length accountId)))
            (enforce
                (>= accountLength uACCOUNT_ID_MIN_LENGTH)
                (format
                    "Account ID does not conform to the min length requirement: {}"
                    [accountId]))
            (enforce
                (<= accountLength uACCOUNT_ID_MAX_LENGTH)
                (format
                    "Account ID does not conform to the max length requirement: {}"
                    [accountId])))
    )

    ;Enforces valid amounts of token
    (defun enforce-valid-amount
        ( precision:integer
          amount:decimal)
        @doc " Enforces positive amounts "
        (enforce (> amount 0.0) "Positive non-zero amounts only.")
        (enforce-precision precision amount)
    )

    ;Enforces token precision of decimal placement
    (defun enforce-precision
        ( precision:integer
          amount:decimal)
        @doc " Enforces whole numbers "
        (enforce
            (= (floor amount precision) amount)
            "Whole NFTs only.")
    )

    (defun pause-mint()
        (with-capability (ADMIN)
            (update mint-status MINT_STATUS { 
                'status: MINT_PAUSED  
            })
        )
    )

    (defun resume-mint()
        (with-capability (ADMIN)
            (update mint-status MINT_STATUS { 
                'status: MINT_STARTED  
            })
        )
    )

    ; for-sale can be true for listing a NFT, false for removing a pass from the marketplace
    ; price is not relevant when de-listing an NFT
    (defun list-miner-on-market (nft-id:string owner:string price:decimal for-sale:bool)
        @doc "Make a new listing or update a listing for a Miner on the marketplace"
        (with-read mledger nft-id
          { 'nft-id := ledger-id 
          , 'owner-address := ledger-owner
          , 'staked := staked_status}
          (enforce (= staked_status false) "Your NFT is currently powered ON. Please power it off on the farm dashboard and try again")
          (with-capability (MINER_OWNER ledger-owner ledger-id)
            (enforce (= owner ledger-owner) "Account Owners dont match")
            (enforce (> price 1.0)  "Price must be greater than 1.0" )
            (update mledger nft-id 
                {
                    "for-sale": for-sale,
                    "market-price": price,
                    "updated-at": (at "block-time" (chain-data))
                }
            )
            (emit-event (LIST_MINER nft-id ledger-owner price for-sale))
            (if (= for-sale true) (format "Miner with id {} is listed on the market for {} KDA" [nft-id price]) (format "Miner with id {} has been taken off the market" [nft-id]))
          )
        )
    )
    
    (defun edit-miner-market-price (account:string nft-id:string new-price:decimal)
        @doc "allows the owner of an NFT to edit the price of an NFT if it is already for sale"
        (with-capability (MINER_OWNER account nft-id) 
            (if (get-miner-for-sale-true-false nft-id) (update mledger nft-id { "market-price": new-price } ) (format "NFT is not for sale, please list it on the marketplace before trying to edit the price"))
        )
    )
    
    (defun buy-miner-off-market
        ( nft-id:string
          buyer:string
          price:decimal )
        @doc " Buy a Miner from the marketplace "
        (with-read mledger nft-id
            { 'nft-id := ledger-id 
            , 'owner-address := current-owner
            , 'for-sale := ledger-for-sale
            , 'market-price := ledger-price 
            }
            (enforce (= ledger-for-sale true)  "You can only purchase an NFT that is for sale." )
            (enforce (= price ledger-price) "The price you chose does not match the price of the NFT you're trying to buy.")
            (enforce (!= buyer current-owner) "You cannot buy your own NFT.")
            (with-capability (ACCOUNT_GUARD buyer) 
                (coin.transfer buyer current-owner (round (* 0.95 ledger-price) 2))
                (coin.transfer buyer CREATOR_FUND (round (* 0.05 ledger-price) 2))
                (with-capability (PRIVATE)
                    (set-volume (+ (get-price TOTAL_VOLUME_KEY) ledger-price))
                )
                (update mledger nft-id 
                    {
                        "owner-address": buyer,
                        "for-sale": false
                    } 
                )
                (emit-event (BUY_MINER nft-id buyer current-owner price))
                (format "Purchased a Miner with ID {} for {} KDA " [nft-id price])
            )
        )
    )

; ============================================
; ==         FMarketplace Functions         ==
; ============================================

    (defun transfer-founder:string (id:string sender:string receiver:string amount:decimal)
        @doc "Transfer an NFT to another valid k:address"
        (enforce-coin-account-exists receiver)
        (enforce (= 1.0 amount) "Only 1 Pass can be transferred at a time")
        (with-read fledger id
            { 'founders-nft-id := ledger-id 
            , 'owner-address := current-owner
            , 'for-sale := ledger-for-sale
            }
            (enforce (= ledger-for-sale false)  "Your NFT is currently for sale, please de-list and try to transfer again" )
            (enforce (!= sender receiver) "You can't send an NFT to yourself")
            (enforce (= current-owner sender) "You are not the owner of this NFT")
            (with-capability (FOUNDERS_OWNER sender ledger-id)
                (update fledger id 
                    {"owner-address": receiver
                    ,"unique-id"    : (with-capability (PRIVATE) (generate-random-number ledger-id)) })
            )
        )
        (emit-event (TRANSFER_FOUNDER id sender receiver amount))
    )

    ; for-sale can be true for listing a NFT, false for removing a pass from the marketplace
    ; price is not relevant when de-listing an NFT
    (defun list-founder-pass-on-market(nft-id:string owner:string price:decimal for-sale:bool)
        @doc "Make a new listing or update a listing for a founders pass on the marketplace"
        (with-read fledger nft-id
          { 'founders-nft-id := ledger-id, 
          'owner-address := ledger-owner }
          (with-capability (FOUNDERS_OWNER ledger-owner ledger-id)
            (enforce (= owner ledger-owner) "Account Owners dont match")
            (enforce (> price 0.0)  "Price must be greater than 0.0" )
            (update fledger nft-id 
                {
                    "for-sale": for-sale,
                    "market-price": price
                }
            )
            (emit-event (LIST_FOUNDER nft-id ledger-owner price for-sale))
            (if (= for-sale true) (format "Founder's Pass with id {} is listed on the market for {} KDA" [nft-id price]) (format "Founder's Pass with id {} has been taken off the market" [nft-id]))
          )
        )
    )
    
    (defun edit-founders-market-price (account:string nft-id:string new-price:decimal)
        @doc "allows the owner of an NFT to edit the price of an NFT if it is already for sale"
        (with-capability (FOUNDERS_OWNER account nft-id) 
            (if (get-founder-for-sale-true-false nft-id) (update fledger nft-id { "market-price": new-price } ) (format "NFT is not for sale, please list it on the marketplace before trying to edit the price"))
        )
    )

    (defun buy-founders-pass-off-market
        ( nft-id:string
          buyer:string
          price:decimal )
        @doc " Buy a Founder's Pass from the marketplace "
        (with-read fledger nft-id
            { 'founders-nft-id := ledger-id 
            , 'owner-address := current-owner
            , 'for-sale := ledger-for-sale
            , 'market-price := ledger-price 
            }
            (enforce (= ledger-for-sale true)  "You can only purchase an NFT that is for sale." )
            (enforce (= price ledger-price) "The price you chose does not match the price of the NFT you're trying to buy.")
            (enforce (!= buyer current-owner) "You cannot buy your own NFT.")
            (with-capability (ACCOUNT_GUARD buyer) 
                (coin.transfer buyer current-owner (round (* 0.95 ledger-price) 2))
                (coin.transfer buyer FOUNDERS_FUND (round (* 0.05 ledger-price) 2))
                (with-capability (PRIVATE)
                    (set-volume (+ (get-price TOTAL_VOLUME_KEY) ledger-price))
                )
                (update fledger nft-id 
                    {
                        "owner-address": buyer,
                        "for-sale": false
                    } 
                )
                (emit-event (BUY_FOUNDER nft-id buyer current-owner price))
                (format "Purchased a Founder's Pass with ID {} for {} KDA " [nft-id price])
            )
        )
    )

    (defun set-volume(value:decimal)
        @doc "Sets the price for a key to store in the price-table"
        (require-capability (PRIVATE))
        (update price-table TOTAL_VOLUME_KEY 
            {"price": value} 
        )
        
    )

    (defun get-all-founders-for-sale (index-start:integer index-end:integer)
        @doc "Gets all Founder's Passes for sale, indexes are inclusive"
        (take (- (- index-end (- index-start 1))) (take index-end (sort (select fledger ["founders-nft-id", "for-sale", "market-price", "free-mints-remaining", "airdrops-remaining"] (where "for-sale" (= true))))))

    )

; ============================================
; ==     Miner Marketplace Functions        ==
; ============================================

    (defun transfer-miner-multiple (ids:list sender:string receiver:string)
        @doc "transfer multiple miner NFTs to another valid k:address"
        (with-capability (PRIVATE)
            (map 
                (transfer-miner1 sender receiver 1.0)
                ids
            )
        )
        (format "NFTs with id(s) {} transferred to address {}" [ids, receiver])
    )

    (defun transfer-miner1:string (sender:string receiver:string amount:decimal id:string)
        @doc "Transfer a Miner NFT to another valid k:address, reordered for mapping"
        (enforce-coin-account-exists receiver)
        (enforce (= 1.0 amount) "Only 1 NFT can be transferred at a time")
        (with-read mledger id
            { 'nft-id := ledger-id 
            , 'owner-address := current-owner
            , 'for-sale := ledger-for-sale
            , 'staked := staked_status
            }
            (enforce (= ledger-for-sale false)  "Your NFT is currently for sale, please de-list and try to transfer again" )
            (enforce (= staked_status false) "Your NFT is currently powered ON. Please power it off on the farm dashboard and try again")
            (enforce (!= sender receiver) "You can't send an NFT to yourself")
            (enforce (= current-owner sender) "You are not the owner of this NFT")
            (with-capability (MINER_OWNER sender ledger-id)
                (update mledger id 
                    {"owner-address": receiver })
            )
        )
        (emit-event (TRANSFER_MINER id sender receiver amount))
    )

    (defun transfer-miner:string (id:string sender:string receiver:string amount:decimal)
        @doc "Transfer a Miner NFT to another valid k:address"
        (enforce-coin-account-exists receiver)
        (enforce (= 1.0 amount) "Only 1 NFT can be transferred at a time")
        (with-read mledger id
            { 'nft-id := ledger-id 
            , 'owner-address := current-owner
            , 'for-sale := ledger-for-sale
            , 'staked := staked_status
            }
            (enforce (= ledger-for-sale false)  "Your NFT is currently for sale, please de-list and try to transfer again" )
            (enforce (= staked_status false) "Your NFT is currently powered ON. Please power it off on the farm dashboard and try again")
            (enforce (!= sender receiver) "You can't send an NFT to yourself")
            (enforce (= current-owner sender) "You are not the owner of this NFT")
            (with-capability (MINER_OWNER sender ledger-id)
                (update mledger id 
                    {"owner-address": receiver })
            )
        )
        (emit-event (TRANSFER_MINER id sender receiver amount))
    )
    
    ; for-sale can be true for listing a NFT, false for removing a pass from the marketplace
    ; price is not relevant when de-listing an NFT
    (defun put-id-for-sale (nft-id:string price:decimal)
        @doc "Make a new listing or update a listing for a Miner on the marketplace"
        (with-read mledger nft-id
          { 'nft-id := ledger-id 
          , 'owner-address := ledger-owner
          , 'staked := staked_status}
          (enforce (= staked_status false) "Your NFT is currently powered ON. Please power it off on the farm dashboard and try again")
          (with-capability (MINER_OWNER ledger-owner ledger-id)
            (enforce (> price 1.0)  "Price must be greater than 1.0" )
            (update mledger nft-id 
                {
                    "for-sale": true,
                    "market-price": price,
                    "updated-at": (at "block-time" (chain-data))
                }
            )
            (emit-event (LIST_MINER nft-id ledger-owner price true))
            (format "Miner with id {} is listed on the market for {} KDA" [nft-id price])
          )
        )
    )

    ; for-sale can be true for listing a NFT, false for removing a pass from the marketplace
    ; price is not relevant when de-listing an NFT
    (defun remove-id-from-sale (nft-id:string)
        @doc "Removes a miner NFT from the marketplace"
        (with-read mledger nft-id
          { 'nft-id := ledger-id 
          , 'owner-address := ledger-owner}
          (with-capability (MINER_OWNER ledger-owner ledger-id)
            (update mledger nft-id 
                {
                    "for-sale": false,
                    "market-price": -1.0,
                    "updated-at": (at "block-time" (chain-data))
                }
            )
            (emit-event (LIST_MINER nft-id ledger-owner -1.0 false))
            (format "Miner with id {} has been taken off the market" [nft-id])
          )
        )
    )
    
    (defun buy-id-on-sale ( nft-id:string buyer:string )
        @doc " Buy a Miner from the marketplace "
        (with-read mledger nft-id
            { 'nft-id := ledger-id 
            , 'owner-address := current-owner
            , 'for-sale := ledger-for-sale
            , 'market-price := ledger-price 
            }
            (enforce (= ledger-for-sale true)  "You can only purchase an NFT that is for sale." )
            (enforce (!= buyer current-owner) "You cannot buy your own NFT.")
            (with-capability (ACCOUNT_GUARD buyer) 
                (coin.transfer buyer current-owner (round (* 0.95 ledger-price) 2))
                (coin.transfer buyer CREATOR_FUND (round (* 0.05 ledger-price) 2))
                (with-capability (PRIVATE)
                    (set-volume (+ (get-price TOTAL_VOLUME_KEY) ledger-price))
                )
                (update mledger nft-id 
                    {
                        "owner-address": buyer,
                        "for-sale": false,
                        "updated-at": (at "block-time" (chain-data))
                    } 
                )
                (emit-event (BUY_MINER nft-id buyer current-owner ledger-price))
                (format "Purchased a Miner with ID {} for {} KDA " [nft-id ledger-price])
            )
        )
    )

    (defun get-all-miners-for-sale (index-start:integer index-end:integer)
        @doc "Gets all Miner NFTs for sale, indexes are inclusive"
        (take (- (- index-end (- index-start 1))) (take index-end (sort (select mledger ["nft-id", "generation", "for-sale", "market-price", "uri"] (where "for-sale" (= true))))))
    )

    (defun get-nft-fields-for-ids (fields:list ids:list) 
        @doc "Return fields for a list of ids"
        (map 
            (get-nft-fields-for-id fields)
            ids
        )
    )

    (defun get-nft-fields-for-id (fields:list id:string )
        @doc "Return the fields for a given id"
        (+ {"nft-id": id} (read mledger id fields))
    )

    (defun get-all-on-sale ()
        @doc "Returns all items on sale"
        (let (
          (items (select mledger ["nft-id", "market-price"] (where "for-sale" (= true)))))
              (map (format-item) items)
        )
    )

    (defun format-item (item:object)
        @doc "Returns formatted item"
        {
            "id": (at "nft-id" item),
            "price": (at "market-price" item)
        }
    )

    (defun create-nft-to-mint-multiple (amount:integer account:string)
        @doc "Allows the admin to create NFTs before mint, in order to reduce gas costs"
        (with-capability (PRIVATE)
            (map 
                (create-nft-to-mint)
                (make-list amount account)
            )
        )
        (format "{} NFTs created at address {}" [amount, account])
    )

    (defun create-nft-to-mint (account:string)
        @doc "callable only by create-nft-to-mint-multiple"
        (require-capability (PRIVATE))
        (insert mledger (int-to-str 10 (get-count MINERS_CREATED_COUNT))
            { "nft-id": (int-to-str 10 (get-count MINERS_CREATED_COUNT))
            , "generation": 1
            , "owner-address": "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad" ;the zero account
            , "uri": "https://gateway.pinata.cloud/ipfs/QmWGQ3324Tt9QGQfYjmjhZnwJ3GVeFDehCGaxqXscE57iv"
            , "hashrate": 640.0
            , "tied-asic": []
            , "mint-date": (at "block-time" (chain-data))
            , "special-attributes": "none"
            , "staked": false
            , "staked-unstaked": []
            , "for-sale": false 
            , "market-price": 0.0
            , "staked-time": (at "block-time" (chain-data))
            , "updated-at": (at "block-time" (chain-data)) })
        (increase-count MINERS_CREATED_COUNT)
    )
)
; (create-table airdrop-table)
; (create-table wl-remaining-per-account)
; (create-table miner-uri-table)
; (create-table mledger)
; (create-table wl)
; (create-table fledger)
; (create-table counts-table)
; (create-table price-table)
; (create-table values-table)
; (create-table mint-status)
; (initialize)

