(module kadenai-create-v2 GOVERNANCE

    (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.admin"))
    )
  
    (defcap OPS()
    (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.create-ops"))
    (compose-capability (OPS_INTERNAL))
    (compose-capability (WLMOD))
    )

  (defcap CREATECOL()
  true
  )

  (defcap OPS_INTERNAL ()
  (compose-capability (MINT))
  )

  (defcap WHITELIST_UPDATE ()
  true
)

(defcap MINT ()
(compose-capability (WHITELIST_UPDATE))
(compose-capability (SPLITTER))
)

(defcap MINT_EVENT
 (
  collection:string
  tierId:string
  account:string
  amount:integer
 )
 @event true
)

(defcap RESERVE_MINT 
  (collection:string 
   account:string
   amount:integer
   creator:string
   creator-amount:decimal 
   bankAc:string 
   bank-amount:decimal
  )
  @event true
 )

(use marmalade-v2.ledger)
(use marmalade-v2.util-v1)


(defcap WLCREATOR:bool (collection:string)
(with-read collections collection {'creatorGuard:=creatorGuard}
(util.guards.enforce-or creatorGuard (keyset-ref-guard "free.ku-ops")))     
    "Must be the collection creator or have OPS capability"
  (compose-capability (WLMOD))
)

(defcap MINTPROCESS:bool (collection:string)
(enforce-guard (at 'creatorGuard (read collections collection ['creatorGuard])))
"Must be the collection creator guard"
)

(defcap WLMOD ()
	true
)

; #################################################################
; #                      Schema Details                           #
; #################################################################

  ; NFT collections are stored in the nft-collections table.
  ; An NFT collection is uniquely identified by its name.
  ; The creator is the k:account of the original creator.
  ; totalSupply is the total number of NFTs in this collection.
  ; of the entire hash of tokens
  ; tiers hold mint prices, qty allowed to mint, and start/end times.
  (defschema collection
    @doc "Stores the name of the collection, the tiers, \
    \ the total supply of the collection. \
    \ The id is the name of the collection."
    name:string
    id:string
    totalSupply:integer
    creator:string
    creatorGuard:guard
    description:string
    currentIndex:integer
    fungible:module{fungible-v2}
    tiers:[object:{tier}]
  )

      (defschema minted-token
        @doc "Stores the data for a minted token. \
        \ The id is the collection, tierId, account, and token-id."
        collection:string
        account:string
        guard:guard
        token-id:integer
        marmToken:string
        revealed:bool
      )

      (defschema tier
        @doc "Stores the start time, end time, tier type (WL, PUBLIC), \
        \ tierId, cost for this tier to mint, \
        \ and the limit for each minter."
        tierId:string
        tierType:string
        startTime:time
        endTime:time
        cost:decimal
        limit:integer
        currencyType:string ; currency type ('KDA' or 'USD')
      )

      (defschema fungible-account
        @doc "account and guard information of a fungible"
        account:string
        guard:guard
      )

  (defschema tier-whitelist-data
  @doc "A data structure for the whitelist data for a tier"
  tierId:string
  accounts:[string]
  )

      (defschema whitelisted
        @doc "Stores the account of the whitelisted user, the tierId, \
        \ and amount they have minted. The id is 'collection:tierId:account'."
        account:string
        tierId:string
        mint-count:integer
      )

      (defschema nft
        id:string
        owner:string
      )
      
      (deftable nft-table:{nft})

  ; ============================================
  ; ==                 Tables                 ==
  ; ============================================


  (deftable collections:{collection})
  (deftable minted-tokens:{minted-token})
  (deftable whitelist-table:{whitelisted})
  (deftable tiers:{tier})
  (deftable tdata:{token-data})


  ; ============================================
  ; ==         Initialize Collection          ==
  ; ============================================

  (defun create-collection:string
    (
      collection-data:object
      collectionSize:integer
      fungible:module{fungible-v2}
    )
    @doc "Creates a collection with the provided data."
    (with-capability (CREATECOL)
      ; Validate the collection tiers
      (validate-tiers (at "tiers" collection-data))
  
      (let*
        (
          (collection-name:string (at "name" collection-data))
          (operator-account:string (at "creator" collection-data))
          (operator-guard:guard (at "creatorGuard" collection-data))
          (col-owner:string (create-principal (at "creatorGuard" collection-data)))
          (collection-id:string (marmalade-v2.collection-policy-v1.create-collection-id collection-name operator-guard))
        )
          (insert collections collection-name
          (+
              { "fungible": fungible
              , "currentIndex": 1
              , "totalSupply": collectionSize
              , "id": collection-id
              , "creator": operator-account
              , "creatorGuard": operator-guard
              }
              collection-data
            )
          )
     
        ; Call init-collection in the collection-policy-v1 contract with the required fields
        (marmalade-v2.collection-policy-v1.create-collection
          collection-name
          collectionSize
          operator-guard
          col-owner
        )
        "Collection successfully created" 
      )

     )
  )
  
  (defun update-collection-tiers:bool
    (
      collection:string
      tiers:[object:{tier}]
    )
    @doc "Updates the tiers of the given collection"
    (with-capability (WLCREATOR collection)
      (validate-tiers tiers)
      (update collections collection
        { "tiers": tiers }
      )
    )
    true
  )


  (defun validate-tiers:bool (tiers:[object:{tier}])
  @doc "Validates the tier start and end time, ensuring they don't overlap \
  \ and that start is before end for each."
  (let*
    (
      (no-overlap
        (lambda (tier:object{tier} other:object{tier})
          ;; If the other is the same as the tier, don't check it
          (if (!= (at "tierId" tier) (at "tierId" other))
            (enforce
              (or
                ;; Start and end of other is before start of tier
                (and?
                  (<= (at "startTime" other))
                  (<= (at "endTime" other))
                  (at "startTime" tier)
                )
                ;; Start and end of other is after end of tier
                (and?
                  (>= (at "endTime" other))
                  (>= (at "startTime" other))
                  (at "endTime" tier)
                )
              )
              "Tiers overlap"
            )
            []
          )
        )
      )
      (validate-tier
        (lambda (tier:object{tier})
          ;; Enforce start time is before end time,
          ;; and that the tier type is valid
          (enforce
            (<= (at "startTime" tier) (at "endTime" tier))
            "Start must be before end"
          )
          (enforce (!= (at "startTime" tier) (at "endTime" tier ))
           "Start and end date must not be the same"
          )
          (enforce
            (or
              (= (at "tierType" tier) TIER_TYPE_WL)
              (= (at "tierType" tier) TIER_TYPE_PUBLIC)
            )
            "Invalid tier type"
          )
          (enforce
            (>= (at "cost" tier) 0.0)
            "Cost must be greater than 0"
          )
          ; KDA = default, if USD then use Oracle to calculate real time price
          (enforce
            (contains (at "currencyType" tier) ["KDA" "USD"])
            "Invalid currency type"
          )  
          ;; Loop through all the tiers and ensure they don't overlap
          (map (no-overlap tier) tiers)

        )
      )
    )
    (map (validate-tier) tiers) 
  )
  true
)

; #######################################
;              Bank Info
; #######################################

  (defschema bank-info
    @doc "Stores string values"
    value:string
  )
  (deftable bankInfo:{bank-info})

  (defun update-bank (bankId:string value:string)
    @doc "Updates the account for the bank"

    (with-capability (OPS)
      (write bankInfo bankId
        { "value": value }
      )
    )
  )

  (defun get-bank-value:string (bankId:string)
    @doc "Gets the value with the provided id"
    (at "value" (read bankInfo bankId ["value"]))
  )

  (defconst BANK_ACCOUNT:string "BANK")
  (defconst PERCENT_TO_CREATOR:decimal 0.98)
  
  (defun get-bank:string ()
    (get-bank-value BANK_ACCOUNT)
  )

  ; ============================================
  ; ==               Constants                ==
  ; ============================================

    (defconst TIER_TYPE_WL:string "WL")
    (defconst TIER_TYPE_PUBLIC:string "PUBLIC")
    (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")
    (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")
  
  ; ============================================
  ; ==           Mint Functionality           ==
  ; ============================================

  (defun admin-mint:string
    (
      collection:string
      account:string
      guard:guard
      amount:integer
    )
    @doc "Requires OPS. Mints the given amount of tokens \
    \ for the account for free."
    (with-capability (OPS)
      (let*
        (
          (collection-data (read collections collection))
          (currentIndex (at "currentIndex" collection-data))
          (tier (get-current-tier (at "tiers" collection-data)))
          (tierId (at "tierId" tier))
        )

        (mint-internal
          collection
          account
          guard
          amount
          tierId
          currentIndex
        )
      )
    )
  )

  (defun reserve-mint:bool
    (
      collection:string
      account:string
      amount:integer
   )
    @doc "Mints the given amount of tokens for the account. \
    \ Gets the current tier and tries to mint from it. \
    \ If the tier is a whitelist, checks that the account is whitelisted \
    \ and that the mint count is within the limit. \
    \ If the tier is public, it allows anyone to mint."
    (enforce (> amount 0) "Amount must be greater than 0")

    (with-capability (MINT)
      (with-read collections collection
        { "currentIndex":= currentIndex
        , "totalSupply":= totalSupply
        , "fungible":= fungible:module{fungible-v2}
        , "creator":= creator
        , "creatorGuard":= creatorGuard
        , "tiers":= tiers
        }
        (if (> totalSupply 0)
        (enforce
          (<= (+ (- currentIndex 1) amount) totalSupply)
          "Can't mint more than total supply"
        )
        true
        )

        (bind (get-current-tier tiers)
          { "cost":= cost
          , "tierType":= tierType
          , "tierId":= tierId
          , "limit":= mint-limit
          , "currencyType":= currencyType ; KDA or USD
          }

          (let*
            (
              (actual-cost:decimal (if (= currencyType "USD") 
                                       (let ((kdausdprice:decimal (at "kda-usd-price" (n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.kai-oracle.get-kda-usd-price)))) 
                                        (floor (/ cost kdausdprice)8))
                                       cost))
              (mint-count:integer (get-whitelist-mint-count collection tierId account))
              (bankAc:string (get-bank))
              (total-cost:decimal  (* (dec amount) actual-cost))
              (creator-amount:decimal (* total-cost PERCENT_TO_CREATOR))
              (bank-amount:decimal (- total-cost creator-amount))
              (acguard:guard (at "guard" (fungible::details account)))
            )
            ;; If the tier is public, anyone can mint
            ;; If the mint count is -1, the account is not whitelisted
            (enforce
              (or
                (= tierType TIER_TYPE_PUBLIC)
                (!= mint-count -1)
              )
              "Account is not whitelisted"
            )
            ;; If the mint limit is -1, there is no limit
            ;; If the mint count is less than the limit, the account can mint
            (enforce
              (or
                (= mint-limit -1)
                (<= (+ mint-count amount) mint-limit)
              )
              "Mint limit reached"
            )
            (if (> cost 0.0 )
              
            ;; Transfer funds
            (let
              (
                (splitter-account:string (get-SPLITTER-account))
              )  
              ; Transfer funds to the splitter account
              (fungible::transfer account splitter-account total-cost)

              ; Install capabilities for the transfers from the splitter account to creator and bank accounts
              (install-capability (fungible::TRANSFER splitter-account creator creator-amount))
              (install-capability (fungible::TRANSFER splitter-account bankAc bank-amount))

              ; Transfer funds from the splitter account to creator and bank accounts
              (fungible::transfer splitter-account creator creator-amount)
              (fungible::transfer splitter-account bankAc bank-amount)
            )
            []
            )
            
            (emit-event (RESERVE_MINT collection account amount creator creator-amount bankAc bank-amount))

            ;; Handle the mint
            (if (= tierType TIER_TYPE_WL)
              (update-whitelist-mint-count collection tierId account (+ mint-count amount))
              []
            )
            (mint-internal
              collection
              account
              acguard
              amount
              tierId
              currentIndex
            )
          )
        )
      )
    )
  )

  (defun mint-internal:bool
    (
      collection:string
      account:string
      guard:guard
      amount:integer
      tierId:string
      currentIndex:integer
    )
    (require-capability (MINT))

    (update collections collection
      { "currentIndex": (+ currentIndex amount) }
    )
    (map
      (mint-token collection account guard)
      (map (+ currentIndex) (enumerate 0 (- amount 1)))
    )
    (emit-event (MINT_EVENT collection tierId account amount))
  )

  (defun mint-token:string
    (
      collection:string
      account:string
      guard:guard
      token-id:integer
    )
    @doc "Mints a single token for the account."
    (require-capability (MINT))
    (insert minted-tokens (get-mint-token-id collection token-id)
      { "collection": collection
      , "account": account
      , "guard": guard
      , "token-id": token-id
      , "marmToken": ""
      , "revealed": false
      }
    )
  )

(defschema token-data
  @doc "The information necessary to mint the token on marmalade"
  precision:integer
  uri:string
  policy:module{kip.token-policy-v2}
)

(defun create-marmalade-token:string
  (
    uri:string
    precision:integer 
    collection:string
    marmToken:integer
    policies:[module{kip.token-policy-v2}]
  )
  @doc "Requires Private OPS. Creates the token on marmalade using the supplied data"
(with-capability (MINTPROCESS collection)
  (with-read minted-tokens (get-mint-token-id collection marmToken)
  {
    "account":= account
    , "revealed":= revealed
  }
  (enforce (= revealed false)
  "Can't mint this token more than once"
  )
      (let*
      (
        (guard:guard (at 'creatorGuard (read collections collection ['creatorGuard ])))
        (mintto:guard (at "guard" (coin.details account)))
        (token-id:string (create-token-id {'precision:precision, 'policies: policies, 'uri:uri} guard))        
      )
      ; This is required to create an actual NFT Token
      (create-token
        token-id
        precision
        uri
        policies
        guard
      )
      ; This is where the actual NFT is minted on the ledger.
      (marmalade-v2.ledger.mint
        token-id
        account
        mintto
        1.0
      )

      (update minted-tokens (get-mint-token-id collection marmToken)
        { "revealed": true
        , "marmToken": token-id
        }
      )

      ; Add NFT to the NFT table 
      (insert nft-table token-id
        {
          "id": token-id,
          "owner": account
        }
      )
      token-id
    )
  )
 )
)

; Used to return the concatenated collection ID string value 
(defun get-mint-token-id:string
  (
    collection:string
    token-id:integer
  )
  (concat [collection "|" (int-to-str 10 token-id)])
)

  ; ============================================
  ; ==             Whitelisting               ==
  ; ============================================

  (defun add-whitelist-to-collection
    (
      collection:string
      tier-data:[object{tier-whitelist-data}]
    )
    @doc "Requires creator guard. Adds the accounts to the whitelist for the given tier."
    (with-capability (WLCREATOR collection)
      (let
        (
          (handle-tier-data
            (lambda (tier-data:object{tier-whitelist-data})
              (let
                (
                  (tierId (at "tierId" tier-data))
                  (whitelist (at "accounts" tier-data))
                )
                (map (add-to-whitelist collection tierId) whitelist)
              )
            )
          )
        )
        (map (handle-tier-data) tier-data)
      )
    )
  )

(defun add-whitelist-to-tier:[string]
(
  collection:string
  tier-data:object{tier-whitelist-data}
)
@doc "Requires creator guard. Adds the accounts to the whitelist for the given tier."
(with-capability (WLCREATOR collection)
  (let
    (
      (tierId (at "tierId" tier-data))
      (whitelist (at "whitelist" tier-data))
    )
    (map (add-to-whitelist collection tierId) whitelist)
  )
)
)


(defun add-to-whitelist:string
(
  collection:string
  tierId:string
  account:string
)
@doc "Requires creator guard. Adds the account to the whitelist for the given tier."
(require-capability (WLMOD))

(insert whitelist-table (concat [collection ":" tierId ":" account])
  {
    "tierId": tierId
  , "account": account
  ,  "mint-count": 0
  }
)
)

(defun is-whitelisted:bool
(
  collection:string
  tierId:string
  account:string
)
@doc "Returns true if the account is whitelisted for the given tier."
(let
  (
    (whitelist-id (get-whitelist-id collection tierId account))
  )
  (with-default-read whitelist-table whitelist-id
    { "mint-count": -1 }
    { "mint-count":= mint-count }
    (!= mint-count -1)
  )
)
)

(defun get-whitelist-mint-count:integer
(
  collection:string
  tierId:string
  account:string
)
(let
  (
    (whitelist-id (get-whitelist-id collection tierId account))
  )
  (with-default-read whitelist-table whitelist-id
    { "mint-count": -1 }
    { "mint-count":= mint-count }
    mint-count
  )
)
)

(defun get-whitelist-id:string
(
  collection:string
  tierId:string
  account:string
)
(concat [collection ":" tierId ":" account])
)

(defun update-whitelist-mint-count
(
  collection:string
  tierId:string
  account:string
  count:integer
)
@doc "Requires Whitelist Update. Updates the mint count for the given account in the whitelist."
(require-capability (WHITELIST_UPDATE))

(let
  (
    (whitelist-id (get-whitelist-id collection tierId account))
  )
  (update whitelist-table whitelist-id
    { "mint-count": count }
  )
)
)

  ; ============================================
  ; ==         Get Detail Functions           ==
  ; ============================================


  (defun get-current-tier-for-collection:object{tier} (collection:string)
    @doc "Gets the current tier for the collection"
    (with-read collections collection
      { "tiers":= tiers}
      (get-current-tier tiers)
    )
  )

  (defun curr-time:time ()
  @doc "Returns current chain's block-time"
  (at 'block-time (chain-data))
  )

  (defun get-current-tier:object{tier} (tiers:[object:{tier}])
    @doc "Gets the current tier from the list based on block time"
    (let*
      (
        (now:time (at "block-time" (chain-data)))
        (filter-tier
          (lambda (tier:object{tier})
            (if (= (at "startTime" tier) (at "endTime" tier))
              (>= now (at "startTime" tier))
              (and?
                (<= (at "startTime" tier))
                (> (at "endTime" tier))
                now
              )
            )
          )
        )
        (filtered-tiers (filter (filter-tier) tiers))
      )
      (enforce (> (length filtered-tiers) 0) (format "No tier found: {}" [now]))
      (at 0 filtered-tiers)
    )
  )

  (defun get-unrevealed-tokens-for-collection:[object:{minted-token}]
    (
      collection:string
    )
    @doc "Returns a list of unrevealed tokens."
    (select minted-tokens
      (and?
        (where "revealed" (= false))
        (where "collection" (= collection))
      )
    )
  )

  (defun get-owned:[object:{minted-token}]
    (
      account:string
    )
    @doc "Returns a list of tokens owned by the account."
    (select minted-tokens (where "account" (= account)))
  )

  (defun get-owned-for-collection:[object:{minted-token}]
    (
      account:string
      collection:string
    )
    @doc "Returns a list of tokens owned by the account."
    (select minted-tokens
      (and?
        (where "account" (= account))
        (where "collection" (= collection))
      )
    )
  )

  (defun get-wl-collection ([object:{whitelisted}])
    @doc "pull list of whitelist ID's for all collections."
        (keys whitelist-table)
  )

(defun get-collection-data:object{collection} (collection:string)
  (read collections collection)
)

     (defun get-all-collections ()
       @doc "Returns a list of all collections."
     (select collections (constantly true))
   )

; #############################################
;                 Splitter Account
; #############################################


  (defcap SPLITTER ()
    @doc "Checks to make sure the guard for the given account name is satisfied"
   true
  )

  (defun require-SPLITTER ()
    @doc "The function used when building the user guard for managed accounts"
    (require-capability (SPLITTER))
  )

  (defun create-SPLITTER-guard ()
    @doc "Creates the user guard"
    (create-user-guard (require-SPLITTER))
  )

(defun get-col-account ()
 (create-principal (read-keyset 'creatorGuard))
)

  (defun get-SPLITTER-account ()
    (create-principal (create-SPLITTER-guard))
  )

  (defun init ()
    (with-capability (GOVERNANCE)
      (coin.create-account (get-SPLITTER-account) (create-SPLITTER-guard))
    )
  )
  )


