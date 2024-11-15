(module nineties-bulls GOV
    "Nineties Bulls NFTs"
    (use free.arkade-orchestrator [mint-token])
    (use n_4e470a97222514a8662dd1219000a0431451b0ee.ledger [transfer sale get-token-info])
    (use n_4e470a97222514a8662dd1219000a0431451b0ee.policy-auction-sale [place-bid])

    (defconst COLLECTION_ID "c_Arkade 90's Bulls_izEBv6viJhCRtM439l26ILzBrHGKFV9k3dFyus7IZ6Q")  
    (defconst NFTS_TO_MINT_COUNT 1990.0)  
    (defconst WL_MINT_PRICE 9.0)
    (defconst PUBLIC_MINT_PRICE 18.0)
    (defconst WL_MINT_START_TIME "2023-01-01T19:00:00Z")
    (defconst PUBLIC_MINT_START_TIME "2023-01-02T19:00:00Z")
    (defconst MINT_END_TIME "2023-01-07T19:00:00Z")
    (defconst TRADING_START_TIME "2023-01-07T19:00:00Z")
    (defconst MAX_WL_MINT 1990)
    (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
    (defconst TOTAL_REWARDS_CLAIMED_KEY "total-rewards-claimed-count-key")
    (defconst NFTS_MINTED_COUNT_KEY "nfts-minted-count-key")
    (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
    (defconst CURR_WL_ROLE_KEY "curr-wl-role-key")
    (defconst WL_PREMIUM_ROLE "whitelist")
    (defconst URI_KEY "uri-key")
    (defconst PRICE_KEY "price-key")
    (defconst OFFERS_COUNT_KEY "offers-count-key")
    (defconst OFFERS_BANK:string "nineties-bulls-offers-bank")
    (defconst STAKING_BANK:string "nineties-bulls-staking-bank")
    (defconst ADMIN_KEYSET "free.arkade-admin")
    (defconst ADMIN_ADDRESS "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec")
    (defconst CREATOR_ADDRESS "k:3abb60b658d2754bf14c6c15a8178a590efa36a1d420772174eb92d7a143b021")
    (defconst ROYALTY_FEE 0.1)
    (defconst FUNDED_STAKING_BANK_BALANCE 5000000.00)
    (defconst IMAGES_URI "https://arkade-prod.s3.us-east-1.amazonaws.com/arkade-90s-bulls")

    (defun nineties-bulls-guard:guard ()
        @doc "collection module guard for policy to be able to access collection information."
        (create-module-guard "nineties-bulls-guard")
    )
  
    (defcap GOV () 
        (enforce-guard (keyset-ref-guard ADMIN_KEYSET))
    ) 

    (defcap PRIVATE () 
        @doc "Can only be called from a private context"
        true
    ) 

    (defcap ACCOUNT_GUARD(account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard   
            (at "guard" (coin.details account))
        )
    )

    (defcap OWNER (id:string)
        @doc "Enforces that an account owns an NFT"
        (let 
            (
                (nft-owner (at "account" (get-token-info id)))
            )
            (compose-capability (ACCOUNT_GUARD nft-owner))
        )
    )

    (defcap ADMIN() 
        @doc "Only allows admin to call these"
        (enforce-keyset  ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap STAKE_NFT (id:string owner:string)
        @event true
    )

    (defcap UNSTAKE_NFT (id:string owner:string)
        @event true
    )
  
     (defcap REWARD_FROM_STAKE (account:string amount:decimal)
        @event true
    )
  
    (defun initialize ()
        @doc "Initializes the contract the first time its loaded"
        (insert counts NFTS_MINTED_COUNT_KEY {"count": 0.0})
        (insert counts TOTAL_VOLUME_KEY {"count": 0.0})
        (insert values MINT_CHAIN_ID_KEY {"value": (at "chain-id" (chain-data))})
        (insert price PRICE_KEY {"price": WL_MINT_PRICE})
        (insert values CURR_WL_ROLE_KEY {"value": "whitelist"})
        (insert counts OFFERS_COUNT_KEY {"count": 0.0})
        (insert values URI_KEY {"value": "https://arkade-prod.s3.us-east-1.amazonaws.com/arkade-90s-bulls/"})
        (coin.create-account OFFERS_BANK (create-BANK-guard))
    )
  
    ;;;;; SCHEMAS AND TABLES ;;;;;
    (defschema staked-schema
        nft-id:string
        account:string
        timestamp:time
        staked:bool
    )

    (defschema rewards-schema
        account:string
        reward:decimal)
  
    (defschema counts-schema
        @doc "Basic schema used for counting things"
        count:decimal
    )

    (defschema values-schema
        @doc "Basic schema used for storing basic values"
        value:string
    )

    (defschema wl-schema
        @doc "Basic schema used for WL members, keys are account ids"
        role:string
    )

    (defschema price-schema
        @doc "Prices schema"
        price:decimal
    )

    (deftable staked-table:{staked-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})
    (deftable wl:{wl-schema})
    (deftable price:{price-schema})
    (deftable rewards:{rewards-schema})

    ;;;;;; MINT FUNCTIONS ;;;;;;
    
    (defun mint-nfts-bulk (owner:string amount:integer)
        @doc "Mints NFTs bulk"
        (enforce-mint-wl-role owner)
        (enforce-mint-live)
        (coin.transfer owner CREATOR_ADDRESS (* 0.90 (* (get-mint-price) amount)))
        (coin.transfer owner ADMIN_ADDRESS (* 0.10 (* (get-mint-price) amount)))
        (with-capability (ACCOUNT_GUARD owner)
        (with-capability (PRIVATE)
            (map (mint-nft owner amount) (make-list amount 1))
        ))
    )

    (defun mint-nft (owner:string amount:integer)
        @doc "Mints an NFT"
        (require-capability (PRIVATE))
        (require-capability (ACCOUNT_GUARD owner))
        (mint-token COLLECTION_ID owner (at "guard" (coin.details owner))) 
        (with-capability (PRIVATE)
            (increase-count TOTAL_VOLUME_KEY (* (get-mint-price) amount))
            (increase-count NFTS_MINTED_COUNT_KEY 1.0)
        )   
    )

    (defun mint-nfts-free (owner:string amount:integer)
        @doc "Mints nfts as admin for free"
        (with-capability (ADMIN)
            (with-capability (ACCOUNT_GUARD owner)
            (with-capability (PRIVATE)
                (map (mint-nft owner) (make-list amount 1))
            ))
        )
    )

    (defun add-to-wl-bulk (role:string accounts:list)
        @doc "Adds wl users with a role"
        (with-capability (ADMIN)
            (map (add-to-wl role) accounts)
        )
    )

    (defun add-to-wl (role:string account:string)
        @doc "Adds a user to a wl"
        (require-capability (ADMIN))
        (insert wl account {"role": role})
    )

    (defun update-user-wl-role (role:string account:string)
        @doc "Updates a user's wl role"
        (with-capability (ADMIN)
            (update wl account {"role": role})
        )
    )

    (defun set-price(price-value:decimal)
        @doc "Set the price"
        (with-capability (ADMIN)
            (update price PRICE_KEY {"price": price-value})
        )
    )
    
    (defun get-latest-nft-to-mint-data ()
        (let 
            (
                (minted-count (get-count NFTS_MINTED_COUNT_KEY))
                (created-to-mint-count NFTS_TO_MINT_COUNT)
            )
            (enforce (< 0.0 created-to-mint-count) "No NFTs are available for mint.")
            (enforce 
                (< minted-count created-to-mint-count)
                "All NFTs put up for mint have already been minted."
            )
            (let 
                ((id (int-to-str 10 (+ (floor minted-count) 1))))
                id
            )
        )
    )
  
    ;;;;;; MARKETPLACE FUNCTIONS ;;;;;;

    (defun transfer-token:string (receiver:string id:string)
        @doc "Transfer an NFT to an account."
        (let 
            (
                (sender (at "account" (get-token-info id)))
            )
            (enforce-not-staked id)
            (enforce-account-exists receiver)
            (with-capability (OWNER id)
                (transfer id sender receiver (at "guard" (coin.details receiver)) 1.0)
        ))
    )
  
    (defun put-id-for-sale(id:string price:decimal)
        @doc "Puts an NFT up for sale"
        (with-capability (OWNER id)
            (enforce-not-staked id)
            (enforce (> price 0.0) "Price must be positive")
            (sale id (at "account" (get-token-info id)) price (read-msg 'tout))
        )
    )
  
    (defun buy-id-on-sale (sale-id:string buyer:string)
        @doc "Buys an NFT that was put up for sale"
        (with-capability (ACCOUNT_GUARD buyer)
        (with-capability (PRIVATE)
            (increase-count TOTAL_VOLUME_KEY price)
        ))
    )

    (defun make-offer (sale-id:string buyer:string amount:decimal)
        @doc "Make an offer for an NFT"
        (with-capability (ACCOUNT_GUARD buyer)
        (with-capability (PRIVATE)
            (let 
                (
                    (new-offer-id (int-to-str 10 (floor (get-count OFFERS_COUNT_KEY))))
                )
                (enforce (> amount 0.0) "Amount must be greater then zero")
                (increase-count OFFERS_COUNT_KEY 1.0)
                (place-bid sale-id buyer (at "guard" (coin.details buyer)) amount)
            )
        ))
    )

    (defun get-market-fee-from-price (price:decimal)
        @doc "Market fee cost for ID sold at a given price"
        (* price ROYALTY_FEE)
    )

    (defun get-to-seller-amount-from-price (price:decimal)
        @doc "Amount that goes to a seller when NFT sold at a given price"
        (* price (- 1 ROYALTY_FEE))
    )

    ;;;;; STAKE FUCNTIONS ;;;;;

    (defun stake-bulk (items:list)
        (map (stake-item) items)
    )

    (defun stake-item (item:object)
        (let 
            (
                (nft-id (at "id" item))
                (owner (at "owner" item))
            )
            (stake nft-id owner)
        )
    )

    (defun stake (nft-id:string owner:string)
        (with-capability (OWNER nft-id)
            (enforce-not-staked nft-id)
            (write staked-table nft-id
                {"nft-id": nft-id,
                "account": owner,
                "timestamp": (at "block-time" (chain-data)),
                "staked": true}
            )
            (emit-event (STAKE_NFT nft-id owner))
        )
        ; )
    )

    (defun claim-unstake-bulk (items:list)
        (map
            (claim-unstake-item)
            items
        )
    )

    (defun claim-unstake-item (item:object)
        (let (
                (nft-id (at "id" item))
                (owner (at "owner" item))
            )
            (unstake nft-id owner)
        )
    )

    (defun unstake (nft-id:string owner:string)
        (let ((data (get-nft-staked nft-id)))
            (enforce (= (at "staked" data) true) "NFT already unstaked")
            (with-capability (OWNER nft-id)
                (with-read staked-table nft-id
                    {"timestamp":= stakedTime,
                    "account":= owner
                    "nft-id":=nft-id
                    "staked":=staked}
                    (update staked-table nft-id
                        {"staked": false}
                    )
                    (with-capability (PRIVATE)
                        (reward-from-stake owner stakedTime)
                    )
                )
                (emit-event (UNSTAKE_NFT nft-id owner))
            )
        )
    )

    (defun claim-without-unstake-bulk (items:list)
            (map
                (claim-without-unstake-item)
                items
            )
    )

    (defun claim-without-unstake-item (item:object)
        (let (
                (nft-id (at "id" item))
                (owner (at "owner" item))
            )
            (claim-without-unstake nft-id owner)
        )
    )

    (defun claim-without-unstake (nft-id:string owner:string)
        (with-capability (OWNER nft-id)
            (with-read staked-table nft-id
                {"timestamp":= stakedTime,
                "account":= owner
                "nft-id":=nft-id
                "staked":=staked}
                (with-capability (PRIVATE)
                    (reward-from-stake owner stakedTime)
                )
                (update staked-table nft-id
                    {"timestamp": (at "block-time" (chain-data))}
                )
            )
        )
    )

    (defun reward-from-stake (account:string stakedTime:time)
        (require-capability (PRIVATE))

          (let
              (
                (reward (calculate-reward stakedTime))
            )
            (arkade.token.transfer-create STAKING_BANK account (at 'guard (coin.details account)) reward)

            (emit-event (REWARD_FROM_STAKE account reward))
            (increase-count TOTAL_REWARDS_CLAIMED_KEY reward)
          )
        
    )

    (defun calculate-reward (stakedTime:time)
        (ceiling (* (/ (diff-time (at "block-time" (chain-data)) stakedTime) 86400) 25) 2)
    )

    (defun ids-staked-by (owner:string)
        @doc "All IDs staked by an owner"
        (select staked-table ["nft-id", "account", "timestamp", "staked"] (where "account" (= owner)))
    )

    (defun get-nft-staked (nft-id:string)
        (read staked-table nft-id)
    )

    (defun get-total-staked ()
        (length (keys staked-table))
    )

    (defun get-owner-unclaimed-amount (owner:string)
          (let ((items (ids-staked-by owner)))
            (let ((rewards
                   (map (lambda (item)
                          (if (= (at "staked" item) true)
                              (let ((days (/ (diff-time (at "block-time" (chain-data)) (at "timestamp" item)) 86400)))
                                (* 25 days))
                              0))
                        items)))
          (fold (lambda (reward sum)
                  (+ reward sum))
                0
                rewards)))
    )

    (defun get-items-unclaimed-amount (items:list)
            (let ((rewards
                   (map (lambda (item)
                          (if (= (at "staked" item) true)
                              (let ((days (/ (diff-time (at "block-time" (chain-data)) (time (at "timestamp" item))) 86400)))
                                (* 25 days))
                              0))
                        items)))
          (fold (lambda (reward sum)
                  (+ reward sum))
                0
                rewards))
    )
  
    ;;;;; ENFORCEMENTS ;;;;;

    (defun enforce-mint-wl-role (account:string)
        @doc "Enforce the account has a role that allows them to mint NFTs"
        (if (< (at "block-time" (chain-data)) (time PUBLIC_MINT_START_TIME))
            (let 
                (
                    (curr_wl_role (get-value CURR_WL_ROLE_KEY))
                    (user_wl_role (get-wl-role account))
                )
                (if 
                    (= curr_wl_role WL_PREMIUM_ROLE)
                    (enforce (= user_wl_role WL_PREMIUM_ROLE) "Only whitelist members allowed")
                    true
                )
            )    
            true
        )
    )

    (defun enforce-account-exists (account:string)
        @doc "Enforces that an account exists in the coin table"
        (let ((coin-account (at "account" (coin.details account))))
            (enforce (= coin-account account) "account was not found")
        )
    )

    (defun enforce-mint-live ()
        @doc "Enforces mint is live"
        (enforce (>= (at "block-time" (chain-data)) (time WL_MINT_START_TIME)) "Mint is not live.")
        (enforce (<= (at "block-time" (chain-data)) (time MINT_END_TIME)) "Mint has ended.")
    )

    (defun enforce-marketplace-live ()
        @doc "Enforces mint has ended and marketplace is live"
        (enforce (>= (at "block-time" (chain-data)) (time TRADING_START_TIME)) "Trading is not live.")
    )

    (defun enforce-not-staked (id:string)
        @doc "Enforces the NFT for the ID is not staked"
        (let 
        (
            (staked-data (get-nft-staked id))
        )
        (enforce (= (at "staked" staked-data) false) "NFT is staked")
        )
    )
      
  
    ;;;;;; HELPER FUNCTIONS ;;;;;;;;;

    (defun create-BANK-guard ()
        (create-user-guard (require-PRIVATE))
    )

    (defun require-PRIVATE ()
        (require-capability (PRIVATE))
    )
      
    (defun increase-count(key:string amount:decimal)
        @doc "Increases count of a key in a table by amount"
        (require-capability (PRIVATE))
        (update counts key 
            {"count": (+ amount (get-count key))} 
        )
    )

    (defun set-value(key:string value:string)
        @doc "Sets the value for a key to store in a table"
        (with-capability (ADMIN)
            (update values key 
                {"value": value} 
            )
        )
    )

    (defun get-count (key:string)
        @doc "Gets count for key"
        (at "count" (read counts key ['count]))
    )

    (defun get-value (key:string)
        @doc "Gets value for a key"
        (at "value" (read values key ['value]))
    )      

    (defun get-mint-price()
        (if (< (at "block-time" (chain-data)) (time PUBLIC_MINT_START_TIME)) WL_MINT_PRICE PUBLIC_MINT_PRICE)
    )

    (defun get-wl-role (account:string)
        @doc "Gets current wl role for the user"
        (try
            ""
            (at "role" (read wl account ["role"]))
        )
    )

    (defun get-image-uri (id:string)
        @doc
        "Give URI for ID. If not supported, return \"\" (empty string)."
        (+ (get-value URI_KEY) id)
    )

    (defun bulk-unstake-and-track-rewards (items:list)
      @doc "Unstakes all staked NFTs and tracks rewards earned till the moment of unstaking."
      (with-capability (ADMIN)
          (map (lambda (item)
                (let* ((nft-id (at "nft-id" item))
                        (owner (at "account" item))
                )
                  (unstake-admin nft-id owner)))
              items))
    )

    (defun unstake-admin (nft-id:string owner:string)
        (let ((data (get-nft-staked nft-id)))
            (with-capability (ADMIN)
                (with-read staked-table nft-id
                    {"timestamp":= stakedTime,
                    "account":= owner
                    "nft-id":=nft-id
                    "staked":=staked}
                    (update staked-table nft-id
                        {"staked": false}
                    )
                    (with-capability (PRIVATE)
                        (reward-from-stake-new owner stakedTime)
                    )
                )
            )
        )
    )

    (defun reward-from-stake-new (account:string stakedTime:time)
      (require-capability (PRIVATE))
      (let ((reward (calculate-reward stakedTime)))
        (with-default-read rewards account { "reward": 0.0, "account": account } { "reward":= existing-reward, "account":= account }
          (write rewards account {"reward": (+ existing-reward reward), "account": account})
          (format "New reward balance for {} is {} plus {}" [account existing-reward reward])
        )
      )
    )

    (defun withdraw-rewards (account:string)
      @doc "Allows users to manually withdraw their staking rewards."
      (with-capability (ACCOUNT_GUARD account)
        (with-read rewards account
          {"reward":= reward-balance, "account":= account}
          (with-capability (PRIVATE) (arkade.token.transfer-create STAKING_BANK account (at 'guard (coin.details account)) reward-balance))
          (write rewards account {"reward": 0.0, "account": account})))
    )

    (defun get-rewards-by-owner (account:string)
      @doc "Gets rewards by owner"
      (select rewards ["account", "reward"]
        (where "account" (= account)))
    )
)
