(module kadena-mining-club GOVERNANCE
  @doc "Kadena Mining Club mint contract."
    (use coin)

; ============================================
; ==               CONSTANTS                ==
; ============================================

    (defconst ADMIN_KEYSET (read-keyset 'kmc-admin))
    (defconst ACCOUNTS_CREATED_COUNT "accounts-created-count")
    (defconst FOUNDERS_CREATED_COUNT "founders-count")

    (defconst WL_MINERLIST_ROLE "wl-minerlist-role") ; lets user mint early 
    (defconst WL_KD2_ROLE "wl-kd2-role") ; lets user mint 1 NFT at discounted price
    (defconst WL_KD5_ROLE "wl-kd5-role") ; lets user mint 2 NFTs at discounted price
    (defconst WL_KD6_ROLE "wl-kd6-role") ; lets user mint 3 NFTs at discounted price
    (defconst WL_KD7_ROLE "wl-kd7-role") ; lets user mint 5 NFTs at discounted price
    (defconst WL_KD8_ROLE "wl-kd8-role") ; lets user mint 10 NFT at discounted price
    
    (defconst FOUNDERS_PRICE_KEY "founders-price-key")

    (defconst MINT_CHAIN_ID "mint-chain-id")
    (defconst TOTAL_VOLUME_KEY "total-volume-key")

    (defconst MINT_STATUS "mint-status")
    (defconst MINT_PAUSED "mint-paused")
    (defconst MINT_STARTED "mint-started")

    (defconst FOUNDERS_MAX_SUPPLY 420 "The maximum supply of the founders pass NFTs")
    (defconst MINT_WALLET "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad") 
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst DISCORD_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")
    (defconst FOUNDERS_FUND "k:0ab2f447374b4968abd6e689b9fb00e7e82ffd99bb0543084e9eeaba10651f92")

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

    (defschema market-history-schema 
        @doc "Stores market history information"
        time-sold-at:string
        price:decimal
        nft-id:string
        request-key:string
        seller:string
        buyer:string
    )

    (defschema user-account-schema
        @doc "Stores user account information"
        account-address:string
        id:string
        free-mint-count:integer
        guard:guard
        whitelist-mints-completed:integer
        profile-picture-url:string
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

    (deftable wl:{wl-schema})
    (deftable fledger:{fentry})
    (deftable counts-table:{counts-schema})
    (deftable price-table:{price-schema})
    (deftable values-table:{values-schema})
    (deftable user-accounts-table:{user-account-schema})
    (deftable mint-status:{mint-schema})
    (deftable market-history:{ market-history-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        (insert counts-table ACCOUNTS_CREATED_COUNT {"count": 0})
        (insert price-table FOUNDERS_PRICE_KEY {"price": 0.2})
        (insert counts-table FOUNDERS_CREATED_COUNT {"count": 0})
        (insert price-table TOTAL_VOLUME_KEY {"price": 0.0})
        (insert values-table MINT_CHAIN_ID {"value": "2"})
        (insert mint-status MINT_STATUS {"status": MINT_STARTED})
    )

; ============================================
; ==              MINT FUNCTIONS            ==
; ============================================

    ; 
    (defun buy-founders-pass ( account:string number-to-mint:integer)
        @doc "Mint a founders pass"
        (enforce (<= (time "2022-07-30T17:00:00Z") (at "block-time" (chain-data))) "Minting will be enabled on July 30 17:00 UTC")
        (with-read mint-status MINT_STATUS {
            'status:= status
            }
            (enforce (= status MINT_STARTED) "MINT is paused or completed, cannot mint now")
        )
        (let ((mint-chain-id (get-value MINT_CHAIN_ID)))
            (enforce (= (curr-chain-id) mint-chain-id) "Can only mint on specific chain")
        )
        (with-capability (ACCOUNT_GUARD account)
            (if (account-exists account) "account exists, continuing with transaction"  (with-capability (PRIVATE) (create-account account)))
            (enforce-max-founders-mint account number-to-mint)
            (enforce (> number-to-mint 0) "You must mint at least 1 NFT")
            (validate-account-id account)
            (enforce-coin-account-exists account)
            (with-default-read counts-table FOUNDERS_CREATED_COUNT
                { 'count: 0.0 }
                { 'count := current-count }
                (enforce (>= (- FOUNDERS_MAX_SUPPLY 22) (+ current-count number-to-mint )) (format "Max supply reached, current supply is {}" [current-count]))
            )
            (with-default-read price-table FOUNDERS_PRICE_KEY
                { 'price: 0.0 }
                { 'price := price_1 }
                (coin.transfer account MINT_WALLET (* number-to-mint price_1))
            )
            (with-capability (PRIVATE) (mint-founders-pass account (id-for-next-key FOUNDERS_CREATED_COUNT)))   
            (if (= number-to-mint 2) (with-capability (PRIVATE) (mint-founders-pass account (id-for-next-key FOUNDERS_CREATED_COUNT))) "")
            (format "{} Founder(s) successfully purchased." [number-to-mint])
        )
    )

    (defun admin-mint (account:string number-to-mint:integer)
        @doc "Allows the admin to distribute the 22 free mints to giveaway winners"
        (with-capability (ADMIN)    
            (validate-account-id account)
            (enforce-coin-account-exists account)
            (with-default-read counts-table FOUNDERS_CREATED_COUNT
                { 'count: 0.0 }
                { 'count := current-count }
                (enforce (>= FOUNDERS_MAX_SUPPLY (+ current-count number-to-mint)) (format "Max supply reached, current supply is {}" [current-count]))
            )
            (with-capability (PRIVATE) (mint-founders-pass account (int-to-str 10 (get-count FOUNDERS_CREATED_COUNT))))
            (if (= number-to-mint 2) (with-capability (PRIVATE) (mint-founders-pass account (id-for-next-key FOUNDERS_CREATED_COUNT))) "")
            (format "{} Founder(s) successfully minted." [number-to-mint])
        )
    )

    (defun mint-founders-pass (account:string id:string)
        @doc "Write all founders pass information to a table"
        (require-capability (PRIVATE))
        (insert fledger id
            { "founders-nft-id"      : id
            , "owner-address"        : account
            , "airdrops-remaining"   : 1
            , "free-mints-remaining" : 0.5
            , "market-price"         : 0.0
            , "for-sale"             : false
            , "unique-id"            : (generate-random-number id)
            , "uri"                  : "https://gateway.pinata.cloud/ipfs/QmNgbMbkNFCAgWDu2LtiY94uHA1ec6WPbgmtYjvVpYJCma/Founders%20Pass.mp4" })
        (with-capability (PRIVATE) (increase-count FOUNDERS_CREATED_COUNT))
        (emit-event (MINT_FOUNDER id account))
    )

; ============================================
; ==         Marketplace Functions          ==
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
        (with-capability (ACCOUNT_GUARD account) 
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
                (if (account-exists buyer) "" (with-capability (PRIVATE) (create-account buyer)))
                (with-capability (PRIVATE)
                    (set-volume (+ (get-price TOTAL_VOLUME_KEY) ledger-price))
                )
                (update fledger nft-id 
                    {
                        "owner-address": buyer,
                        "for-sale": false,
                        "unique-id" : (with-capability (PRIVATE) (generate-random-number nft-id))
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
        (take (- (- index-end (- index-start 1))) (take index-end (sort (select fledger ["founders-nft-id"] (where "for-sale" (= true))))))

    )

; ============================================
; ==       State-modifying functions        ==
; ============================================

    (defun create-account (account:string)
    ;ensure that only one account can be created per address
        @doc "Creates an account"
        (require-capability (PRIVATE))
        (enforce-coin-account-exists account)
        (with-capability (ACCOUNT_GUARD account)
            (let ((id (id-for-next-key ACCOUNTS_CREATED_COUNT)))
                (insert user-accounts-table account
                    { "free-mint-count" : 0
                    , "guard"   : (coin-account-guard account)
                    , "id"      : id 
                    , "account-address" : account
                    , "whitelist-mints-completed": 0
                    , "profile-picture-url" : "https://imgur.com/CCCbcjz"
                    }
                )
                (with-capability (PRIVATE) (increase-count ACCOUNTS_CREATED_COUNT))
            )
        )
    )
    
    (defun get-profile-picture-url (account:string) 
        (at "profile-picture-url" (read user-accounts-table account ['profile-picture-url] ))
    )

    (defun increase-count (key:string)
        ;increase the count of a key in a table by 1
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )

    (defun set-value(key:string value:string)
        @doc "Sets the value for a key to store in a table"
        (with-capability (ADMIN)
            (update values-table key 
                {"value": value} 
            )
        )
    )

    (defun set-price(key:string value:decimal)
        @doc "Sets the price for a key to store in the price-table"
        (with-capability (ADMIN)
            (update price-table key 
                {"price": value} 
            )
        )
    )

    (defun add-to-wl-for-role (role:string accounts:list calling-account:string)
        @doc "Adds wl users with a role"
        (enforce
            (contains role [WL_MINERLIST_ROLE, WL_KD2_ROLE, WL_KD5_ROLE, WL_KD6_ROLE, WL_KD7_ROLE, WL_KD8_ROLE])
              "Must specify a valid role for adding WL members"
        )
        (with-capability (DISCORD calling-account)
            (map (add-to-wl role calling-account) accounts)
        )
    )
    
    (defun add-to-wl (role:string calling-account:string account:string )
        @doc "Adds a user to a wl"
        (require-capability (DISCORD calling-account))
        (if (contains account (get-wl-members)) (update wl account {"role":role}) (insert wl account {"role": role}))
    ) 

; ============================================
; ==     NON STATE-MODIFYING FUNCTIONS      ==
; ============================================

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

    ; (defun admin-get-founder-unique-id-pairs ()
    ;     @doc "returns the Founders unique ID paired with the account that owns the pass"
    ;     (with-capability (ADMIN)
    ;         (select fledger ["unique-id"] (where "unique-id" (!= "KMCisthebest")))
    ;     )
    ; )
    
    ; (price-key) inputs are any of the following
        ; "price-key"
        ; "whitelist-price-key"
        ; "founders-price-key" 
        ; "total-volume-key"
    (defun get-price (price-key:string) 
        @doc "Gets the price for a key"
        (at "price" (read price-table price-key ["price"]))
    )

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

    (defun get-value (key:string)
        @doc "Gets value for a key"
        (at "value" (read values-table key ['value]))
    )
    
    (defun get-wl-members ()
        @doc "Returns all addresses currently on the miner whitelist table"
        (keys wl)
    )

    (defun get-wl-role (account:string)
        @doc "Returns the highest whitelist role for a specific account"
        (at "role" (read wl account ['role]))
    )

    ; (key) inputs are the same as get-count
    (defun id-for-next-key (key:string)
        @doc "returns the next id for a given key"
        (int-to-str 10 (get-count key))
    )

    (defun get-all-founders-owners ()
        (select fledger ["owner-address"] (where "owner-address" (!= "KMCisthebest")))
    )

    (defun get-user-founders (account:string)
        @doc "Returns all Founder's Passes owned by one address"
        (select fledger ["founders-nft-id"] (where "owner-address" (= account)))
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

    (defun account-exists:bool (account:string)
        @doc "Enforces only one account per address"
        (let 
            (
                (num-accounts (length (select user-accounts-table ["account-address"] (where "account-address" (= account)))))
            )
            (if (= num-accounts 0) false true)
        )
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
            (enforce exist "Account does not exist in coin contract"))
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
)

; (create-table market-history)
; (create-table wl)
; (create-table fledger)
; (create-table counts-table)
; (create-table price-table)
; (create-table values-table)
; (create-table user-accounts-table)
; (create-table mint-status)
; (initialize)

