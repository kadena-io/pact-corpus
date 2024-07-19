(module kor-blocks-policy GOVERNANCE

  @doc "Collection token policy."

 


  (implements kip.token-policy-v1)

  (use kip.token-policy-v1 [token-info])
  (use marmalade.ledger)
  (use coin)
  (use free.kor-oracle [get-price-for-price-tracker])






;;------------------------------------------------------
;;Constants


  (defconst SALE_CLOSED       "sale closed")
  (defconst SALE_OPEN         "sale open")
  (defconst SALE_REVEALED     "sale revealed")
  (defconst TIER_TYPE_PUBLIC  "PUBLIC")
  (defconst KOR_BANK  "k:a6f4b28bcdbdbf6d557dc2d3a0c1d80893e864d7ae38b5f4f007d7197ca4500a") 
  (defconst DEV_BANK  "k:b2e645cc9f86f332f3d2d345825b339e83a0d38e9ec20096ad14099e24789c53")
  (defconst OPERATOR_ACC  "k:96a46fe541b6e1370f7a4b8a20599002f87250bd7940725cf1804843a1d8bdd8")




;;------------------------------------------------------
;;Schemas and tables

  (defschema collection
    @doc
    "Stores all relevant collection info. Token list and collection hash will be updated after randomization of tokens"
    
    id:string
    collection-size:integer
    collection-hash:string
    kor-account:string
    kor-guard:guard
    dev-account:string
    dev-guard:guard
    dev-fee:decimal
    tokens:[string]
    sold-tokens:[string]
    unsold-tokens:[string]
    index:integer
    tiers:[object:{tier}]
    fungible:module{fungible-v2}
    operator-account:string
    operator-guard:guard
    shift-index:integer
    sale-status:string)



  (defschema tier
    @doc  
    "Stores the start time, end time, tier type (WL, PUBLIC), tier-id,collection-id, cost for this tier to mint, and the limit for each minter."
    
    collection-id:string
    tier-id:string
    tier-type:string
    start-time:time
    end-time:time
    cost:decimal
    limit:integer)



  (defschema token
    @doc 
    "Stores the data for a minted token. The key is the token-id."

    token-id:string
    collection-id:string
    account:string
    supply:decimal
    precision:integer
    manifest:object{kip.token-manifest.manifest})




  (defschema reserve-info
    @doc
    "Stores the information for each account that reserves one or more NFTs in the colletion. The key is the account-collection-id "
    
    collection-id:string
    indexes:[integer]
    token-id-list:[string]
    account:string
    guard:guard
    minted:integer)



  (defschema whitelist
    @doc "Stores whitelist info for all tiers. Key is account-tier-id-collection-id"
    
    collection-id:string
    tier-id:string
    account:string
    reserved:integer)

  (defschema t-data
    @doc "Structure that needs to be enforced to mint a marmalade token"

    precision:integer
    scheme:string
    data:string
    datum:object{datum}
    policy:module{kip.token-policy-v1}
    )

  (defschema datum
    @doc "Stores all the info of the NFT"

    attributes:[object{attributes}]
    collection:string
    rank:object{rank}
    description:string
    image:string
    name:string
    properties:object{properties})


  (defschema attributes
    @doc "Specifies the rarity and value of a given trait"

    rarity:decimal
    trait_type:string
    value:string)

  (defschema properties
    @doc "Specifies the creators and lists the relevant files for the NFT"

    creators:[object{creators}]
    files:[object{files}])

  (defschema creators
    @doc "Royalties should be sent to this address"

    address:string
    blockchain:string)

  (defschema files
    @doc "Additional information on the media files of the NFT"

    type:string
    uri:string)

  (defschema rank
    @doc "Information on the ranking of the NFT (poistion and score)"

    position:integer
    score:decimal)

  (deftable collections:{collection})
  (deftable reserve-info-table:{reserve-info})
  (deftable tokens:{token})
  (deftable whitelist-table:{whitelist})


  ;;------------------------------------------------------------------------------
  ;;Capabilities
  ;; -----------------------------------------------------------------------------

  (defcap GOVERNANCE ()
    @doc "Governance capability"

    (enforce-keyset  "free.kor-admin" ))


  (defcap INTERNAL ()
    @doc "Capability to ensure Interal usage of functions"

    true)

  (defcap END_SALE ()
    @doc "Capability to ensure only autohorized people/functions can call the end of sale"

    true)

  (defcap OPERATOR (collection-id:string)
    @doc "Capability to enforce the function is being called by the operator"

    (compose-capability (END_SALE))
    (with-read collections collection-id 
      {
      'operator-guard:= operator-guard:guard
      }
      (enforce-keyset operator-guard)))



  (defcap ACCT_GUARD (account)
    @doc "Capability to enforce the function is being called by the account owner"

    (enforce-keyset (at 'guard (coin.details account))))



  (defcap INIT_COLLECTION:bool (collection-id:string collection-size:integer fungible:module{fungible-v2} price:decimal operator:string)
    @doc  "Capability that emits an event when a collection is initialized"

    @event
    (fungible::details operator))



  (defcap RESERVE_SALE:bool (collection-id:string account:string index:integer)
    @doc  "Capability that emits an event when a sale reserve is made"

    @event
    true)

  (defcap SHIFT:bool (collection-id:string shift-index:integer index:integer)
    @doc  "Capability that emits an event when the sale is over and the indexes are shifted"

    @event
    true)

  (defcap REVEAL_TOKENS:bool (collection-id:string tokens:[string])
    @doc  "Capability that emits an event when the collection tokens-hashes are registered onchain"

    @event
    true)

  (defcap MINT_REQUEST:bool (collection-id:string account:string guard:guard indexes:[integer])
    @doc  "Capability that emits an event when a user requests to mint his tokens. Operator must call create-multiple-tokens function"

    @event
    true)

  (defcap RESERVED:bool (collection-id:string account:string token-id:string )
    @doc  "Capability that enforces the token-id to be reserved for the account"
    (with-read reserve-info-table (format "{}-{}" [account collection-id]) 
      {"token-id-list":= token-ids}
      (enforce (contains token-id token-ids) "Mismatching whitelist index" )))

  (defcap CREATE_TOKEN (mint-guard:guard account:string)
    @doc "Capability that enforces a valid account and guard to create the token"

      (enforce (validate-principal mint-guard account) "Not a valid account")
      (enforce-keyset mint-guard))

      (defcap AUTO_SALE (collection-id:string account:string token-ids:[string])
      
      @event
      true)
 


  
;; -----------------------------------------------------------------------------
;; PRE-SALE Functions
;; -----------------------------------------------------------------------------

  (defun init-collection:bool
    ( collection-id:string
      collection-size:integer
      collection-hash:string
      dev-fee:decimal
      tiers:[object:{tier}]
      fungible:module{fungible-v2})
      @doc "Function that initializes a collection. The key is collection-id."
      
      (with-capability (GOVERNANCE)
        (insert collections collection-id {
        "id": collection-id
        ,"collection-size": collection-size
        ,"collection-hash": collection-hash
        ,"kor-account": KOR_BANK
        ,"kor-guard": (at "guard" (coin.details KOR_BANK))
        ,"dev-account": DEV_BANK
        ,"dev-guard": (at "guard" (coin.details DEV_BANK))
        ,"dev-fee": dev-fee
        ,"tokens": []
        ,"sold-tokens": []
        ,"unsold-tokens": []
        ,"index": 1
        ,"tiers": tiers
        ,"operator-account": OPERATOR_ACC
        ,"operator-guard": (at "guard" (coin.details OPERATOR_ACC))
        ,"fungible": fungible
        ,"shift-index": 0
        ,"sale-status": SALE_OPEN
      }))
      true)



  (defun add-whitelist-tier:bool
    (collection-id:string accounts:[string] tier-id:string)
    @doc "Function to add multiple whitelists accounts to a specific tier. Needs to be called by OPERATOR"

    (with-capability (OPERATOR collection-id)
      (with-capability (INTERNAL)
        (map (add-whitelist collection-id tier-id)accounts))
    true))


  (defun add-whitelist
    ( collection-id:string
      tier-id:string
      account:string)
      @doc "Creates 1 whitelist entry with tier and account to whitelist-table. Only callable internally"

      (require-capability (INTERNAL))

      (write whitelist-table (format "{}-{}-{}" [account tier-id collection-id])
      {
        "collection-id":collection-id
        ,"tier-id": tier-id
        ,"account": account
        ,"reserved": 0
      }))

;; -----------------------------------------------------------------------------      
;; SALE Functions
;; -----------------------------------------------------------------------------




  (defun reserve-sale:bool (collection-id:string account:string amount:integer)
      @doc "User-called function to reserve an NFT from the collection. Checks if the sale is open, and if it hasnt sold out yet. Also checks for WL account if not in Public sale. Calls reserve-internal and may call end-sale, so needs both caps"

      (enforce (is-principal account) "Invalid account name") 
      (enforce (> amount 0) "Amount must be greater than 0")
      (with-capability (END_SALE)
        (with-capability (INTERNAL)
          (with-read collections collection-id {
            "collection-size":= collection-size
            ,"kor-account":=creator
            ,"dev-account":=dev
            ,"dev-fee":=dev-fee
            ,"fungible":=fungible
            ,"index":= current-index
            ,"sale-status":= sale-status
            ,"tiers":=tiers
            }
            (enforce (<= (+ (- current-index 1) amount) collection-size) "Can't reserve more than collection size")
            (enforce (= sale-status SALE_OPEN) "Sale is not open")
            (bind (get-current-tier tiers )
            { "cost":= cost
            , "tier-type":= tier-type
            , "tier-id":= tier-id
            , "limit":= mint-limit
            , "cost":=price
            }
            (let 
              (
                (reserved (get-whitelist-reserved collection-id tier-id account))
              )

              ;; If the tier is public, anyone can mint
              ;; If the mint count is -1, the account is not whitelisted
              (enforce (or (= tier-type TIER_TYPE_PUBLIC) (!= reserved -1)) "Account is not whitelisted")
              (enforce (or (= mint-limit -1) (<= (+ amount reserved) mint-limit) )(format "Can't reserve more than {}" [mint-limit])))
            
            (coin.transfer account creator (* amount (floor (* (get-kda-price price) (- 1.0 dev-fee))4)))
            (coin.transfer account dev (* amount (floor(* (get-kda-price price) dev-fee)4)))
            
            ;when minting on any WL tier, register the amount reserved to check if limit is hit on next reserve request 
            (if (!= tier-type TIER_TYPE_PUBLIC)
              (with-read whitelist-table (format "{}-{}-{}" [account tier-id collection-id])
                {
                  "reserved":= reserved-before
                }
                (update whitelist-table (format "{}-{}-{}" [account tier-id collection-id])
                  {
                    "reserved":(+ reserved-before amount)
                  }))
              true)
            (if 
              (= tier-id "Auto Reveal")
              (reserve-auto-sale collection-id account (at "guard" (coin.details account)) amount )
              (reserve-internal collection-id account (at "guard" (coin.details account)) amount )))))))




  (defun reserve-admin:string (collection-id:string account:string amount:integer)
    @doc  "This function will be used to reserve the free mints and will be called to reserve from ETH as well. Only OPERATOR can cal, has Internal cap to call reserve-internal"

      (with-capability (OPERATOR collection-id)
        (with-capability (INTERNAL)
          (let*
            (
              (collection-data (read collections collection-id))
              (tier (get-current-tier (at "tiers" collection-data)))
              (tier-id (at "tier-id" tier))
              (guard (at "guard" (coin.details account)))
            )
            (if 
                (= tier-id "Auto Reveal")
                (reserve-auto-sale collection-id account guard amount )
                (reserve-internal collection-id account guard amount ))))))


  (defun reserve-internal:bool
    (collection-id:string account:string guard:guard amount:integer)
    @doc "Function that can only be called internally. Updates the required tables for the reservation and emits an event. May need to call end-sale, so has END_SALE cap."

    (require-capability (INTERNAL))
    (with-capability (END_SALE)
      (with-read collections collection-id
        {
          "index":=current-index
          ,"collection-size":=collection-size
          ,"sold-tokens":=sold-tokens
          ,"unsold-tokens":= unsold-tokens
        }
          (let 
            (
              (indexes-to-add (enumerate current-index (+ (- current-index 1) amount)))
            )
            (update-indexes collection-id account indexes-to-add amount))
          (update collections collection-id {"index": ( + current-index amount)})
        
          ;;Buyers know their index from the emitted event. Index is needed in mint.
          (emit-event (RESERVE_SALE collection-id account current-index))
        
          ;; If indexes are full, then choose a shift index
          (if 
            (= (+ collection-size 1)  (+ current-index amount))
            (end-sale collection-id)  
            true))))

  (defun reserve-auto-sale
    (collection-id:string account:string guard:guard amount:integer)
    (require-capability (INTERNAL))
    (with-read collections collection-id
      {
        "index":=current-index
        ,"collection-size":=collection-size
        ,"sold-tokens":=sold-tokens
        ,"unsold-tokens":= unsold-tokens
      }
      (enforce (= (+ (length sold-tokens) (length unsold-tokens)) collection-size) "lengths don't match")
      (let*
        (
          (unsold-length (length unsold-tokens))
          (shift-index (random unsold-length))
          (shifted-token-ids (+ (drop (mod shift-index unsold-length) unsold-tokens) (take (mod shift-index unsold-length) unsold-tokens)))
          (tokens-to-add (take amount shifted-token-ids))
          (updated-unsold-tokens (drop amount shifted-token-ids))
          (updated-sold-tokens (+ sold-tokens tokens-to-add))
        )
        (update collections collection-id 
          {
            "index": ( + current-index amount)
            ,"sold-tokens": updated-sold-tokens
            ,"unsold-tokens": updated-unsold-tokens
          })
        (update-token-ids collection-id account tokens-to-add )
        (emit-event (AUTO_SALE collection-id account tokens-to-add))
        )))
  

  (defun end-sale-admin 
    (collection-id:string)
    @doc "Function to end-sale by OPERATOR. To be called at a specific date if collection does not sell out."

    (with-capability (OPERATOR collection-id)
      (end-sale collection-id)))




  (defun end-sale 
    (collection-id:string)
    @doc "Function that changes the sale-status to closed. It also creates a shift-index that will randomize the token-id list after reveal and emit the SHIFT event"

    (require-capability (END_SALE))
    (with-capability (INTERNAL)
      (with-read collections collection-id
        {
          "sale-status":=status
          ,"collection-size":=collection-size
          ,"index":=index
        }
        (enforce (!= status SALE_CLOSED) "Can't close a closed sale" )
        (update-shift-index collection-id collection-size index)
        (update collections collection-id {"sale-status":SALE_CLOSED}))))



;; -----------------------------------------------------------------------------
;; POST-SALE Functions
;; -----------------------------------------------------------------------------



  (defun reveal-tokens:[string] 
    (collection-id:string token-ids:[string])
    @doc "Function to be called after receiving the SHIFT event message. It will hash the token list and confirm it matches the collection-hash inputted at collection creation"

    (with-capability (OPERATOR collection-id)
      (with-read collections collection-id 
        {
          "collection-size":= collection-size
          ,"collection-hash":=collection-hash
          ,"shift-index" := shift-index
          ,"sale-status" := sale-status
          ,"index" := index
        }
        (enforce (= collection-hash (hash token-ids)) "Token manifests don't match")
        (enforce (= sale-status SALE_CLOSED) "Sale is still running")
        (let*
            (
              (shifted-token-ids (+ (drop (mod shift-index collection-size) token-ids) (take (mod shift-index collection-size) token-ids)))
              
            )
            (enforce (= (length token-ids) collection-size) "Token list is invalid")
            
            ;The new list, shifted-token-index, will be stored and hashed. The indexes will point to the new token-id, shifted by shift-index
            ;The new list is sent in the REVEAL_TOKENS event
            (update collections collection-id 
              {
                "tokens": shifted-token-ids
                ,"sold-tokens": (take (- index 1) shifted-token-ids)
                ,"unsold-tokens": (drop (- index 1) shifted-token-ids)
                ,"sale-status": SALE_REVEALED
              })
            (map (update-account-token-ids collection-id) (filter (where "tokens" (!= [])) (map-all-accounts-tokens collection-id ) ))
            (emit-event (REVEAL_TOKENS collection-id shifted-token-ids))))))

 
  (defun update-account-token-ids
    ( collection-id:string account-tokens:object)
    (require-capability (OPERATOR collection-id))
    (update reserve-info-table (format "{}-{}" [(at "account" account-tokens) collection-id])
      {
        "token-id-list": (at "tokens" account-tokens)
      }))

  ;  (defun mint-user-tokens 
  ;    ( account:string collection-id:string)
  ;    @doc "Function called by the user. It will lookup the user indexes and attribute the respective token-ids. Has CREATE_TOKEN capability to ensure the account owner is the same as when reserve was made"

  ;    (with-capability (ACCT_GUARD account)
  ;      (with-read collections collection-id 
  ;        {
  ;          "sale-status":=sale-status
  ;          ,"sold-tokens":=sold-tokens
  ;        }
  ;        (enforce (= sale-status SALE_REVEALED) "Can't Mint while sale is ongoing")
  ;        (with-read reserve-info-table (format "{}-{}" [account collection-id])
  ;        {
  ;          "guard":=mint-guard
  ;        }
  ;        (with-capability (CREATE_TOKEN mint-guard account)
  ;      (let*
  ;        (
  ;          (indexes (get-account-reserved-indexes account collection-id) )
  ;          (token-ids (map(get-token-id sold-tokens) indexes))
  ;        )
  ;        (enforce (!= indexes []) "No tokens to mint")

  ;        ;The MINT_REQUEST event is emited with the necessary info for the create and mint functions to be called by the OPERATOR
  ;        (emit-event (MINT_REQUEST collection-id account (at "guard" (coin.details account)) indexes ))
  ;        (with-read reserve-info-table (format "{}-{}" [account collection-id])
  ;          {
  ;            "indexes":= minted-indexes
  ;          }

  ;          (update reserve-info-table (format "{}-{}" [account collection-id]) 
  ;            {
  ;              "indexes":[]
  ;              ,"token-id-list":token-ids
  ;            }))))))))




  (defun create-multiple-tokens 
    (collection-id:string account:string t-data:[object{t-data}])
    @doc "Function to create multiple tokens from the same account. Limited to 100 at a time, due to gas. Needs to be called by OPERATOR"

    (with-capability (OPERATOR collection-id)  
      (map (create-marmalade-token collection-id account) t-data)))


  (defun create-marmalade-token:string 
    (collection-id:string account:string t-data:object{t-data})
    @doc "Requires OPERATOR cap. Creates the token on marmalade using the supplied data"

    (require-capability (OPERATOR collection-id))
    (bind t-data
      { 
        "precision":= precision
      , "scheme":= scheme
      , "data":= data
      , "datum":= datum
      , "policy":= policy
      }
      (let*
        (
          (uri (kip.token-manifest.uri scheme data))
          (datum-complete (kip.token-manifest.create-datum uri datum))
          (manifest (kip.token-manifest.create-manifest uri [datum-complete]))
          (token-hash (concat ["t:" (at "hash" manifest)]))
          (token-id (at "name" datum))
          (token {"id":token-id,"supply":0.0,"precision":precision,"manifest":manifest})
        )
        
        ;The RESERVED capability ensures that the token is part of the user's reserved list
        (with-capability (RESERVED collection-id account token-hash)
          (marmalade.ledger.create-token 
            token-hash
            precision
            manifest
            policy
          )
          (insert tokens token-hash
            { 
            "token-id" : token-hash
            ,"collection-id" : collection-id
            ,"account":account
            ,"supply": 0.0
            ,"precision":precision
            ,"manifest": manifest
            }) 
          (install-capability (marmalade.ledger.MINT token-hash account 1.0))
          (marmalade.ledger.mint
            token-hash
            account
            (at "guard" (coin.details account))
            1.0
          )
          (update tokens token-hash
            {"supply": 1.0})
            token-hash))))



;; -----------------------------------------------------------------------------
;;ENFORCE FUNCTIONS
;; -----------------------------------------------------------------------------

  (defun enforce-init:bool 
    (token:object{token-info})
    @doc "Function to be called from marmalade.ledger."
    
    (enforce-ledger)

    (let* 
      (
        (precision:integer (at 'precision token))
      )     
          
      ;enforce one-off
      (enforce (= precision 0) "Invalid precision")))




  (defun enforce-mint:bool 
    (token:object{token-info} account:string guard:guard amount:decimal)
    @doc "Function to be called from marmalade.ledger."

    (enforce-ledger)

    ;;enforce one-off
    (enforce (= amount 1.0) "Invalid mint amount"))



  (defun enforce-burn:bool
      ( token:object{token-info} account:string amount:decimal)

      (enforce-ledger)
      (enforce false "BURN prohibited"))



  (defun enforce-offer:bool 
    (token:object{token-info} seller:string amount:decimal sale-id:string)

    (enforce-ledger)
    true
    ;(enforce false "SALE prohibited")
  )

  (defun enforce-buy:bool 
    (token:object{token-info} seller:string buyer:string buyer-guard:guard amount:decimal sale-id:string)
    
    (enforce-ledger)
    (let
      (
        (token-id (at 'id token))
      )
      (with-read tokens token-id {"account":=owner}
        (enforce (= seller owner))))
        ;(enforce false "SALE prohibited")
  )


  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
     (enforce-ledger)
      (let (
            (token-id (at 'id token))
            )
           (update tokens token-id 
            {
              "account":receiver
            })))

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
     (enforce-ledger)

      (let (
        (token-id (at 'id token))
        )
      (with-read tokens token-id {"account":=owner}
        (enforce (= sender owner)))
       (update tokens token-id 
        {
          "account":receiver
        })))
    ;(enforce false "Transfer prohibited")
  


;; -----------------------------------------------------------------------------
;;UPDATE FUNCTIONS
;; -----------------------------------------------------------------------------


  (defun update-shift-index (collection-id:string collection-size:integer index:integer)
    @doc "Function to be called internally when sale ends. Emits SHIFT event"

    (require-capability (INTERNAL))
    (update collections collection-id {"shift-index": (random collection-size)})
    (emit-event (SHIFT collection-id (random collection-size) index)))


  (defun update-sale-status 
    (collection-id:string sale-status:string)
    @doc "Function to be called by operator when the time limit for the sale is reached"

    (with-capability (OPERATOR collection-id)
      (update collections collection-id{"sale-status": sale-status})))



  (defun update-tiers (collection-id:string tiers:[object:{tier}])
    @doc "Function to be called by operator to set any WL tiers"

    (with-capability (OPERATOR collection-id)
      (update collections collection-id{"tiers": tiers})))


  (defun update-indexes (collection-id:string  account:string indexes-to-add:[integer] amount:integer)
    @doc "Function that is called when a reserve is made to update the account indexes. If the account is not WListed or has not bought yet, it creates a new entry"

    (require-capability (INTERNAL))
    (with-default-read reserve-info-table (format "{}-{}" [account collection-id])
      {
        "collection-id": collection-id
      ,"indexes" : []
      ,"token-id-list": []
      ,"account": account
      ,"guard": (at "guard" (coin.details account))
      ,"minted": 0
      }
      {
        "collection-id" := collection-id
        ,"indexes" := indexes-owned
        ,"token-id-list" := token-id-list
        ,"guard":= guard
        ,"minted":= minted
      }
      (write reserve-info-table (format "{}-{}" [account collection-id])
        {
          "collection-id": collection-id
          ,"indexes" : (+ indexes-owned indexes-to-add)
          ,"token-id-list" : token-id-list
          ,"account": account
          ,"guard": guard
          ,"minted": minted
        })))

  (defun update-token-ids
    (collection-id:string  account:string token-ids-to-add:[string])
    (with-default-read reserve-info-table (format "{}-{}" [account collection-id])
      {
        "collection-id": collection-id
      ,"indexes" : []
      ,"token-id-list": []
      ,"account": account
      ,"guard": (at "guard" (coin.details account))
      ,"minted": 0
      }
      {
        "collection-id" := collection-id
        ,"indexes" := indexes
        ,"token-id-list" := token-id-list
        ,"guard":= guard
        ,"minted":= minted
      }
      (write reserve-info-table (format "{}-{}" [account collection-id])
        {
          "collection-id": collection-id
          ,"indexes" : indexes
          ,"token-id-list" : (+ token-id-list token-ids-to-add)
          ,"account": account
          ,"guard": guard
          ,"minted": minted
        })))



      
;; -----------------------------------------------------------------------------
;;GET FUNCTIONS
;; -----------------------------------------------------------------------------

  (defun get-operator-account:string 
    (collection-id:string)
    @doc "Returns the operator account"

    (with-read collections collection-id {"operator-account":= operator} operator))



  (defun get-current-tier-for-collection:object{tier} (collection-id:string)
    @doc "Gets the current tier for the collection"

    (with-read collections collection-id {"tiers":= tiers} (get-current-tier tiers)))


  (defun get-current-tier-id-for-collection:object{tier} (collection-id:string)
    @doc "Gets the current tier for the collection"
    (with-read collections collection-id {"tiers":= tiers}
      (at "tier-id"(get-current-tier tiers))))

  (defun get-left-on-sale-for-collection:object{tier} (collection-id:string)
    @doc "Gets the current tier for the collection"
    (with-read collections collection-id {"index":= index, "collection-size":=collection-size}
      (- collection-size (- index 1))))



  (defun get-current-tier:object{tier} (tiers:[object:{tier}])
    @doc "Gets the current tier from the list based on block time"
      (let*
        (
          (filter-tier
            (lambda (tier:object{tier})
              (if 
                (= (at "start-time" tier) (at "end-time" tier)) 
                (>= (curr-time) (at "start-time" tier))  
                (and? 
                  (<= (at "start-time" tier))
                  (> (at "end-time" tier))
                  (curr-time)))))
          (filtered-tiers (filter (filter-tier) tiers)))
        (enforce (> (length filtered-tiers) 0) (format "No tier found"))
        (at 0 filtered-tiers)))



      
  (defun get-whitelist-reserved:integer  
    (collection-id:string current-tier-id:string account:string)
    @doc "Get the amount of reserved NFTs for an account in the current tier"

    (with-default-read whitelist-table (format "{}-{}-{}" [account current-tier-id collection-id])
      { "reserved": -1, "tier-id":TIER_TYPE_PUBLIC }
      { "reserved":= reserved, "tier-id":=account-tier-id }
      (if (= account-tier-id current-tier-id) reserved -1)))




  (defun get-collection-status:string 
    (collection-id:string)
    (at "sale-status" (read collections collection-id)))


  (defun get-collection-detail:string 
    (collection-id:string detail:string)
    (at detail (read collections collection-id)))

  (defun get-kda-price 
    (price:decimal)
    (/ price (free.kor-oracle.get-price-for-price-tracker "kda" "usd")))


  (defun get-policy:object{token} 
    (token:object{token-info})
    (read tokens (at 'id token)))


  (defun get-collection:object{collection} 
    (collection-id:string )
    (read collections collection-id))


  (defun get-token:object{token} 
    (token-id:string)
    (read tokens token-id))


  (defun get-token-from-index:string 
    (collection-id:string index:integer)
    (if (= index 0)
      ""
      (at (- index 1) (get-collection-detail collection-id "sold-tokens"))))
    

  (defun get-token-id 
    (sold-tokens:[string] index:integer)
    (at (- index 1) sold-tokens))


  (defun get-token-info 
    (token-id:string)
    (read tokens token-id))


  (defun get-current-tier-details:object 
    (collection-id:string)
    (let* (
      (tier (get-current-tier-for-collection collection-id))
      (tier-id (at "tier-id" tier))
      (tier-end-time (at "end-time" tier))
      (tier-price-usd (at "cost" tier))
      (tier-price-kda (floor (get-kda-price tier-price-usd) 4)) 
      (tier-details {"tier-id":tier-id, "tier-end-time":tier-end-time, "tier-price-usd":tier-price-usd, "tier-price-kda":tier-price-kda})
        )
      tier-details))

  (defun get-reserved-for-account 
    (collection-id:string account:string)
    (with-default-read reserve-info-table (format "{}-{}" [account collection-id])
      {
        "indexes":[]
      }
      {
        "indexes":=indexes
      }
      (let 
        (
          (reserved (length indexes))
        )
      reserved)))

;; -----------------------------------------------------------------------------
;;UTILITY FUNCTIONS
;; -----------------------------------------------------------------------------


  (defun curr-time:time 
    ()
  (at "block-time" (chain-data)))


  (defun verify-account-for-current-tier
    (collection-id:string account:string)
    @doc "This function verifies if account is able to reserve in the current tier"

    (let 
        (
          (current-tier-id (get-current-tier-id-for-collection collection-id))
        )
        (if (= current-tier-id "Public Sale")
          true
          (with-default-read whitelist-table (format "{}-{}-{}" [account current-tier-id collection-id])
            {
              "tier-id":""
            }
            {
            "tier-id":=tier-id
            }
            (if (!= tier-id "") true false)))))


  (defun get-account-reserved-indexes:[string] 
    (account:string collection-id:string)
    (at "indexes" (read reserve-info-table (format "{}-{}" [account collection-id]) ["indexes"])))
                    

  (defun get-account-reserved-tokens:object
    (collection-id:string account:string)
    {
      "account":account
      ,"tokens":(map (get-token-from-index collection-id) (get-account-reserved-indexes account collection-id))
    })

  (defun map-all-accounts-tokens:object
    (collection-id:string)
    (map (get-account-reserved-tokens collection-id)(map (at "account")(select reserve-info-table ["account"] (where "collection-id" (= collection-id))))))


  (defun enforce-ledger:bool 
    ()
    (enforce-keyset (marmalade.ledger.ledger-guard)))


  (defun random:integer 
    (collection-size:integer)
    (mod (at 'block-height (chain-data)) collection-size))


  (defun create-offchain-multiple 
    (array:[object])
    (map (offchain-token-id-and-manifest) array))

  (defun offchain-token-id-and-manifest:object 
    (t-data:object{t-data})
    @doc "Function to be used locally (offchain). Returns the manidest and token-id for a given token"

    (let
      (
        (precision (at "precision" t-data))
        (scheme (at "scheme" t-data))
        (data (at "data" t-data))
        (datum (at "datum" t-data))
        (policy (at "policy" t-data))
      )
      (let*
        (
          (uri (kip.token-manifest.uri scheme data))
          (datum-complete (kip.token-manifest.create-datum uri datum))
          (manifest (kip.token-manifest.create-manifest uri [datum-complete]))
          (token-id (format "t:{}" [(at 'hash manifest)]))
        )
        {
          "manifest": manifest
        , "token-id": token-id
      })))

      

  (defun get-token-id-offchain 
    (t-data:object{t-data})
    @doc "Function to be used locally (offchain). Returns the token-id for a given token"
    (let
      (
        (precision (at "precision" t-data))
        (scheme (at "scheme" t-data))
        (data (at "data" t-data))
        (datum (at "datum" t-data))
        (policy (at "policy" t-data))
      )
      (let*
        (
          (uri (kip.token-manifest.uri scheme data))
          (datum-complete (kip.token-manifest.create-datum uri datum))
          (manifest (kip.token-manifest.create-manifest uri [datum-complete]))
          (token-id (format "t:{}" [(at 'hash manifest)]))
        )
      token-id)))



  (defun map-all-token-ids 
    (array:[object])
    @doc "Functio that gives an array of token-ids"

    (map (get-token-id-offchain) array))

  (defun get-account-owned
    (collection-id:string account:string)
    (select tokens ["token-id", "collection-id"] (where "account" (= account))))

  (defun admin-correct-account-index
    (collection-id:string account:string  new-indexes:[integer])
    (with-capability (OPERATOR collection-id)
      (update reserve-info-table (format "{}-{}" [account collection-id])
        {
          "indexes":new-indexes
        })))

  (defun admin-correct-collection-index
    (collection-id:string new-index:integer)
    (with-capability (OPERATOR collection-id)
      (update collections collection-id
        {
          "index":new-index
        })))

        
)


