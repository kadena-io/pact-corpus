(module kingsmint GOVERNANCE
  ;;make sure to use time utils and ng
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MODULE IMPORTS
  ;;-------------------------------------------------------
  ;; Refer to any modules required in module body
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use free.util-time)
  (use free.util-lists)
  (use n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.kai-oracle)
  ;; Load marmalade-ng
  (use n_4e470a97222514a8662dd1219000a0431451b0ee.ledger)
  (use n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CAPABILITIES
  ;;-------------------------------------------------------
  ;; Capabilities are permissions granted to keysets to 
  ;; access functions of the smart contract module
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;Governance: Global tier access
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_c61c4d579fdb5c1bf99cf05342fe48ea4b850c82.kdabet_admin"))
    ;;Tacking on operations to the governance cap
    (compose-capability (OPS_INTERNAL))
  )
  
  ;;Operations: Managerment type access 
  (defcap OPS()
    (enforce-guard (keyset-ref-guard "n_c61c4d579fdb5c1bf99cf05342fe48ea4b850c82.kdabet_operations"))
    ;;Tacking on operations 
    (compose-capability (OPS_INTERNAL))
    ;;Allows for Whitelist modification
    (compose-capability (WLMOD))
  )

  ;;Allows for the creation of a collection
  (defcap CREATECOL()
    (enforce-guard (keyset-ref-guard "n_c61c4d579fdb5c1bf99cf05342fe48ea4b850c82.kdabet_operations"))
  )

  ;;Allows for the operation of internal smart contract functions
  (defcap OPS_INTERNAL ()
    ;;Tacks on the ability to mint an ng token
    (compose-capability (MINT))
  )

  ;;Allows for updating of a whitelist entry
  (defcap WHITELIST_UPDATE ()
    true
  )

  ;;Allows for the capacity to mint an NG token
  (defcap MINT ()
    ;;Tacks on the ability to update a row in the whitelist
    (compose-capability (WHITELIST_UPDATE))
  )

  ;;Allows for the modifcation of the whitelist table
  (defcap WLMOD ()
	  true
  )

  (defcap WLCREATOR:bool (collection:string)
    @doc "Used to manage whitelists and tier updates"    
    (with-read collections collection {'creatorGuard:=creatorGuard}
    (util.guards.enforce-or creatorGuard  (keyset-ref-guard "n_c61c4d579fdb5c1bf99cf05342fe48ea4b850c82.kdabet_operations") ))
       
      "Must be the collection creator or have OPS capability"
    
    (compose-capability (WLMOD))
  )

  (defcap MINTPROCESS:bool (collection:string)
    @doc "Used to manage who can mint an NG NFT after paying"    
    (enforce-guard (at 'creatorGuard (read collections collection ['creatorGuard ])))
    "Must be the collection creator guard"
  )

  ;;Events are emitted upon a MINT operation
  (defcap MINT_EVENT
    ;;The data we send to the event
    (
      collection:string
      id:string
      account:string
      amount:integer
    )
    @event true
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Schemas                           
  ;;-------------------------------------------------
  ;; Please refer to medium for explinations of the 
  ;; schemas purpose.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;The main collection schema
  (defschema collection
    creator:string
    creatorGuard:guard
    currentIndex:integer
    description:string
    fungible:module{fungible-v2}
    id:string
    name:string
    totalSupply:integer
    types:[object:{type}]
    collectionId:string
  )

  ;;Describes the type of nft in a collection
  (defschema type
    id:string
    startTime:time
    endTime:time
    cost:decimal
    limit:integer
    currencyType:string ;('KDA' or 'USD')
  )

  ;;Describes the policies for each nft type
  (defschema policy-data
    type:string
    policy_stack:string  
  )

  ;;Describes the actual nft token data
  (defschema token-data
    precision:integer
    uri:string
    policy:[module{n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1}]
  )

  ;;Describes a minted token
  (defschema minted-token
    collection:string
    account:string
    guard:guard
    token-id:integer
    marmToken:string
    revealed:bool
  )

  ;;Descibes a whitelist entry
  (defschema whitelisted
    account:string
    id:string
    mint-count:integer
  )

  ;;Describes an entire NFT type whitelist
  (defschema type-whitelist-data
    id:string
    accounts:[string]
  )

  ;;Describes a fungible account that pays for the nft
  (defschema fungible-account
    account:string
    guard:guard
  )

  ;;Describes the current owner of the NFT
  (defschema nft
    id:string
    owner:string
  )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Tables                          
  ;;-------------------------------------------------
  ;; Defines the table names where we store data.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (deftable collections:{collection})
  (deftable minted-tokens:{minted-token})
  (deftable whitelist-table:{whitelisted})
  (deftable types:{type})
  (deftable policies:{policy-data})
  (deftable tdata:{token-data})
  (deftable type-data:{type-whitelist-data})
  (deftable nft-table:{nft})

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Create Collection                          
  ;;-------------------------------------------------
  ;; The definition of the collection and its nft types
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun create-collection:string 
    (
      collectionSize:integer
      collection-data:object
      fungible:module{fungible-v2}
    )
    @doc "Creates a collection with the provided data."
    (with-capability (CREATECOL)
      
      ; Validate the collection tiers
      (enforce (= collectionSize 500) "Total supply must be 500")

      ;; Validate the types are correctly set.
      (validate-types (at "types" collection-data))
  
      (let*
        (
          (name:string (at "name" collection-data))
          (operator-account:string (at "creator" collection-data))
          (operator-guard:guard (at "creatorGuard" collection-data))
          (col-owner:string (create-principal (at "creatorGuard" collection-data)))
          (collection-id:string (n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection.create-collection-id name operator-guard))
        )

        ;;enforce that the operator account is a keyset
        (enforce 
          (and (= "k:" (take 2 operator-account)) (= 66 (length operator-account))) 
          "The creator must be a k: followed by a 64 bit key"
        )

        ;;Insert into the collections table 
        (insert collections name
          (+
            { "fungible": fungible
            , "currentIndex": 0
            , "totalSupply": collectionSize
            , "id": collection-id
            , "creator": operator-account
            , "creatorGuard": operator-guard
            , "collectionId": collection-id
            }
            collection-data
          )
        )
        ;; Call init-collection in the NG policy-collection contract with the required fields
        (n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection.create-collection
          collection-id
          name
          collectionSize
          col-owner
          operator-guard
        )
      "Collection successfully created" 
      ) 
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Updates the NFT types in case of neede change
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun update-collection-types
    ( 
      collection:string
      types:[object:{type}]
    )
    @doc "Updates the types of the given collection"
    (with-capability (CREATECOL)
      (validate-types types)
      (let
        (
          (kingobj:object{type} {"cost": 300.0,"currencyType": "USD","endTime": (time "2024-02-26T00:00:00Z"),"id": "king","limit": 1,"startTime": (time "2024-01-26T08:00:00Z")})
        )

        ;;Cannot change the king immutable type
        (enforce
          (contains kingobj types)
          "Any change to types must contain the immutable king object"
        )

        (update collections collection
          { "types": types }
        )
      )
    )
    true
  )


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Adds policy stack string to the DB for a type
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun add-policies (collection:string policies: [object{policy-data}])
    @doc "records nft type policies in the db"
    (with-capability (CREATECOL)
      (map (add-policy collection) policies)
    )
  )

  (defun add-policy:string (collection:string policy:object{policy-data})
    @doc "Add a policy stack to the policies table"
     (require-capability (CREATECOL))
     (let 
        (
          (type:string (at "type" policy))
          (policy_stack:string (at "policy_stack" policy))
        )
        
        ;;insert policies into db of type collection-type
        (insert policies (concat [collection "-" type])
          { 
            "type": type, 
            "policy_stack": policy_stack
          }
        )
        "Added Policy Stacks"
      )
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Collection Validation                           
  ;;-------------------------------------------------
  ;; A series of logical validators enduring sound
  ;; environmental data is sent in.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defun validate-types:bool (types:[object:{type}])
    @doc "Validates the type start and end time, ensuring they don't overlap \
    \ and that start is before end for each."
    (let* 
      (
        ;;NO-OVERLAP WITH KINGS: a binding that ensures no time overlap
        (no-overlap (lambda (type:object{type} other:object{type})
          ;; If the other is the same as the type, don't check it
          (if (!= (at "id" type) (at "id" other))
            (enforce
              (or
                ;; Start and end of other is before start of type
                (and?
                  (<= (at "startTime" other))
                  (<= (at "endTime" other))
                  (at "startTime" type)
                )
                ;; Start and end of other is after end of type
                (and?
                  (>= (at "endTime" other))
                  (>= (at "startTime" other))
                  (at "endTime" type)
                )
              )
              ;;If the types overlap
              "Types overlap"
              )
              ;;If the types do not overlap
              []
            )
          )
        )
        ;;VALIDATE-TYPE: a binding that ensures what the type.
        (validate-type
          (lambda (type:object{type})
            ;; Enforce start time is before end time,
            ;; and that the tier type is valid
            (enforce
              (< (at "startTime" type) (at "endTime" type))
              "Starttime must be before endtime"
            )
            (enforce
              (or (= (at "id" type) "lord") (> (at "cost" type) 0.0))
              "Either be a lord or the cost must be greater than 0"
            )
            (enforce
              (contains (at "currencyType" type) ["KDA" "USD"])
              "Invalid currency type"
            )          
            ;; Loop through all the tiers and ensure they don't overlap
            (map (no-overlap type) types)
          )
        )
      )
      (map validate-type types) 
    )
    ;; types have been validated
    true
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mint Functions                           
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Convert Overdue Kings                           
  ;;-------------------------------------------------
  ;; If a king has not minted be a specific time we 
  ;; fire this function once to convert the overdue
  ;; kings into lords
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  (defun convert-overdue-kings:string 
    (
      collection:string
      id:string
      account:string
    ) 
    @doc "Converts overdue kings to lords and updates whitelst"
    (with-capability (OPS)
      (with-read collections collection 
        {
          "types":= types
        }
        (let* 
          (
            (endtime (at "endTime" (first (filter (compose (at "id") (= "king")) types))))
            (dead-accounts:[object{whitelisted}] 
              (select whitelist-table 
                (and? (where "mint-count" (= 0)) (where "id" (= "king")))
              )
            )
            (numlords (length dead-accounts)) 
          )
          ;;make sure the end time for kings mint is greater than current time
          (enforce (>= (curr-time) endtime) "King whitelisters are still valid")
          ;;map through the dead accounts and set counts to -1 (invalid)
          (map (invalidate-king collection) dead-accounts)
          ;;mint the invalid kings as lords
          (map (mint-lord collection account) (enumerate 1 numlords))   
          "Conversion can take place"
        )
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mint-all-lords                           
  ;;-------------------------------------------------
  ;; Mints the initial 50 lords and hard caps that 
  ;; initial mint to exactly 50.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun mint-all-lords:string 
    (
      collection:string
      account:string
      amount:integer   
    )
    @doc "Requires OPS. Enumerates over a list to mass mint lords."
    ;; Only available to ops
    (with-capability (OPS) 
      ;;The amount of lords are exactly capped 
      (enforce (= amount 47) "Must mint an exact number of lords")
      (map (mint-lord collection account) (enumerate 1 amount))
    )
    "Lords have been minted"
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mint-lords                           
  ;;-------------------------------------------------
  ;; mints an individual lord.  Requires the mint
  ;; capability and a whitelisted wallet to exist
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  (defun mint-lord:bool
    (
      collection:string
      account:string
      count:integer
    )
    @doc "Requires OPS. Mints a lord into a house account for free."
    ;; Only available to ops
    (require-capability (MINT)) 
    (with-read collections collection
      { 
        "currentIndex":= currentIndex, 
        "totalSupply":= totalSupply, 
        "fungible":= fungible:module{fungible-v2}, 
        "types":= types
      }

      ;;Enforces that the total supply is not eclipsed
      (if (>= totalSupply 0) 
        (enforce
          (<= currentIndex totalSupply)
          "Can't mint more than total supply"
        )
        true
      )
 
      ;;enumerate over list
      (let*
        (
          (mint-count (get-whitelist-mint-count collection "lord" account))
          (acguard:guard (at "guard" (fungible::details account)))
        )

        ;;update the whitelist
        (update-whitelist-mint-count collection "lord" account (+ mint-count 1))
    
        ;;mints tokens
        (mint-internal
          collection
          account
          acguard
          1
          "lord"
          currentIndex
        )
      ) 
    )
  )
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Reserve-mint                           
  ;;-------------------------------------------------
  ;; General minting function for a Kadena king or 
  ;; Monarch class of nft.  Enforces a whitelist and 
  ;; a max supply. Validates timelineness of mint.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  (defun reserve-mint:bool
    (
      collection:string
      account:string
    )
    
    (with-capability (MINT)
      (with-read collections collection
        { 
          "currentIndex":= currentIndex, 
          "totalSupply":= totalSupply, 
          "fungible":= fungible:module{fungible-v2}, 
          "creator":= creator:string, 
          "creatorGuard":= creatorGuard, 
          "types":= types
        }

        ;;Enforces that the total supply is not eclipsed
        (if (>= totalSupply 0) 
          (enforce
            (<= currentIndex totalSupply)
            "Can't mint more than total supply"
          )
          true
        )
      
        (bind (get-current-type types)
          { "cost":= cost
          , "id":= id
          , "limit":= mint-limit
          , "currencyType":= currencyType ; KDA or USD
          }
          ;;Make sure that we aren't minting free nfts
          (enforce 
            (and (or (= id "king") (= id "monarch")) (> cost 0.0)) 
            "Is a king/monarch and amount should be greater than 0.0"
          )

          (let*
            (
              (actual-cost:decimal 
                (if 
                  (= currencyType "USD") 
                  (let 
                    (
                      (kdausdprice:decimal (at "kda-usd-price" (n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.kai-oracle.get-kda-usd-price)))
                    ) 
                    (floor (/ cost kdausdprice) 2)
                  ) 
                  cost
                )
              )
              (mint-count (get-whitelist-mint-count collection id account))
              (total-cost:decimal actual-cost)
              (acguard:guard (at "guard" (fungible::details account)))
              (current-type:string (get-whitelist-type collection id account))
            )

            ;;The whitelister must be within time bounds
            (enforce 
              (= id current-type)
              (format 
                "The whitelister: {} cannot mint in invalid time range. \n Current type is {}" 
                [account id current-type]
              )
            )
 
            ;;The account must be whitelisted
            (enforce (= mint-count 0) (format "The account {} has already minted a king" [account]))
            
            ;;A whitelister cannot mint more than 1.
            (enforce
              (or
                (= mint-limit 0)
                (<= (+ mint-count 1) mint-limit)
              )
              "Mint limit reached"
            )

            ;; Perform transaction
            (fungible::transfer account creator total-cost)

            ;; Update the whiteliset
            (update-whitelist-mint-count collection id account (+ mint-count 1))
           
            ;; Records the mint as complete in the internal table 
            (mint-internal
              collection
              account
              acguard
              1
              id
              currentIndex
            )
          true
          )
        )
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mint-internal                           
  ;;-------------------------------------------------
  ;; Handles the token mint.  Sets up table for writing
  ;; to NG.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  (defun mint-internal:bool
    (
      collection:string
      account:string
      guard:guard
      amount:integer
      id:string
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

    (emit-event (MINT_EVENT collection id account amount))
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; get-mint-token-id                           
  ;;-------------------------------------------------
  ;; Creates a unique token id
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  (defun get-mint-token-id:string
    (
      collection:string
      token-id:integer
    )
    (concat [collection "|" (int-to-str 10 token-id)])
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Create-marmalade-token                           
  ;;-------------------------------------------------
  ;; Writes the NG token to the ledger.  Attaches policies
  ;; and generates the token itself.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun create-marmalade-token:string
    (
      uri:string
      precision:integer 
      collection:string
      marmToken:integer
    )
    @doc "Requires Creator Guard. Creates the token on marmalade using the supplied data"

    ;;Check the paid tokens table and gather the account and status fields
    (with-capability (MINTPROCESS collection)
      (with-read minted-tokens (get-mint-token-id collection marmToken)
        {"account":= account
        ,"revealed":= revealed
        }
        ;;Enforcing singular mints
        (enforce (= revealed false)
          "Can't mint this token more than once"
        )
        ;;Bindpair the input data
        (let*
          (
            (guard:guard (at 'creatorGuard (read collections collection ['creatorGuard ])))
            (mintto:guard (at "guard" (coin.details account)))
            (token-id:string (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.create-token-id guard uri))
            (accountType:string (check-whitelist-for-type account))
          )
          ;Check the whitelist and gather the nft type
          (with-read policies (concat [collection "-" accountType])
            {
              "policy_stack":= policy_stack
            }
            ;; Bindpair the policies based on type
            (let
              (
                (policy:[module{n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1}] (n_4e470a97222514a8662dd1219000a0431451b0ee.std-policies.to-policies policy_stack))
              )
              ;; Create the token (NG)
              (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.create-token token-id precision uri policy guard)
              ;; Write to NG ledger
              (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.mint token-id account mintto 1.0)
              ;; Update minted tokens ledger
              (update minted-tokens (get-mint-token-id collection marmToken) {"revealed": true, "marmToken": token-id})
              ;; Add NFT to the NFT table 
              (insert nft-table token-id {"id": token-id, "owner": account})
            )
          )
          token-id
        )
      )
    )
  )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Functions for fetching current NFT mint type.                           
  ;;-------------------------------------------------
  ;; Based on the time of the mint, fetches the current
  ;; type that can be minted based on time
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  (defun get-current-type-for-collection:object{type} (collection:string)
    @doc "Gets the current type for the collection"    
    (with-read collections collection
      { "types":= types}
      (get-current-type types)
    )
  )

  (defun get-current-type:object{type} (types:[object:{type}])
    @doc "Gets the current type from the list based on block time"
    (let* 
      (
        ;;BIND filtered type list to filter-typeNG. Returns boolean  
        (filter-type
          (lambda (type:object{type})
            (bind type {'startTime:=start, 'endTime:=end}
              (or 
                (and? (= end) (is-past) start)
                (time-between start end (now))
              )
            )
          )
        )
   
        ;;filters the type
        (filtered-types (filter (filter-type) types))
     )
     ;;makes sure the type is not empty!
     (enforce (is-not-empty filtered-types) (format "No type found: {} {}" [(now) types]))
     ;;fetch the first element in types list
     (first filtered-types))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Add-whitelist-by-type                           
  ;;-------------------------------------------------
  ;; For a set of type objects, fetch the whitelist 
  ;; for each and add them to the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  (defun add-whitelist-by-type:string
    (
      collection:string
      type-datum: [object{type-whitelist-data}]
    )
    @doc "Requires ops capability. Adds the accounts to the whitelist for the given tier."
    (with-capability (OPS)
        (map
          (process-whitelist collection)
          type-datum
        )
    )
    "Added Whitelists Successfully"
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Process-whitelist                           
  ;;-------------------------------------------------
  ;; Doubly subscripted map which handles a singular
  ;; whitelist type.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  (defun process-whitelist (collection:string type-data:object{type-whitelist-data})
    (require-capability (OPS))
    (let
      (
        (id (at "id" type-data))
        (whitelist (at "accounts" type-data))
      )

      (map (add-to-whitelist collection id) whitelist)
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Get-whitelist-mint-count                           
  ;;-------------------------------------------------
  ;; fetches the whitelisters total minted NFT count
  ;; returns a default of -1 if the minter does not
  ;; exist  {-1:DNE, 0:exists but not minter, 1:minted}
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (defun get-whitelist-mint-count:integer (collection:string id:string account:string)
    (require-capability (MINT))
    (let
      (
        (whitelist-id (get-whitelist-id collection id account))
      )
      (with-default-read whitelist-table whitelist-id { "mint-count": 0 } { "mint-count":= mint-count }
        mint-count
      )
    )
  )
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Get-whitelist-type                      
  ;;-------------------------------------------------
  ;; Validates that whitelist id exists and furthermore
  ;; has a type the same as the id suggests
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (defun get-whitelist-type:string (collection:string id:string account:string)
    (let
      (
        (whitelist-id (get-whitelist-id collection id account))
      )
      (with-default-read whitelist-table whitelist-id 
        { "id": "Does not exist" } 
        { "id":= id }
        id
      )
    )
  )
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check-Account-for-type     
  ;;-------------------------------------------------
  ;; Used to ensure that a account has a single WL 
  ;; element and returns the type.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
  (defun check-whitelist-for-type:string (account:string)
    @doc "Selects a token by account."
    (let 
      (
        (rowset:[object{whitelisted}] (select whitelist-table (where "account" (= account))))
      )
      ;;the row should be exactly 1 element long!
      (enforce (= (length rowset) 1) "Must be a Monarch or a King")
      ;;select the type
      (at "id" (first rowset))
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Get-whitelist-id
  ;;-------------------------------------------------
  ;; A function that gets us out of writing a concat 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  (defun get-whitelist-id:string (collection:string id:string account:string)
    (concat [collection ":" id ":" account])
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Invalidate-king
  ;;-------------------------------------------------
  ;; Given a collection and a whitelister, flip the 
  ;; whitelisters status to Does Not Exist. Requires
  ;; Update capacity.  Should only be fireable if time
  ;; is up  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  (defun invalidate-king:string
    (
      collection:string
      whitelister:object(whitelisted)
    )
    (require-capability (WHITELIST_UPDATE))
    (let*
      (
        (id:string (at "id" whitelister))
        (account:string (at "account" whitelister))
        (whitelist-id:string (get-whitelist-id collection id account))
      )
      (update whitelist-table whitelist-id
        {"mint-count": -1}
      )   
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Update-Whitelist-mint-count
  ;;-------------------------------------------------
  ;; A simple function for accruing the whitelisters
  ;; Mint count by 1. Requires update capability.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  (defun update-whitelist-mint-count
    (
      collection:string
      id:string
      account:string
      count:integer
    )
    (require-capability (WHITELIST_UPDATE))

    (let
      (
        (whitelist-id (get-whitelist-id collection id account))
      )
      ;;cannot mint more than 1 king or monarch
      (enforce (or (= id "lord") (= count 1)) "Must be a lord or cannot mint more than 1 king/monarch")
      (update whitelist-table whitelist-id
        { "mint-count": count }
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Add-to-whitelist
  ;;-------------------------------------------------
  ;; Adds a whitelister to the whitelist table
  ;; Requires modification capability.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  (defun add-to-whitelist:string
    (
      collection:string
      id:string
      account:string
    )
    @doc "Requires creator guard. Adds the account to the whitelist for the given tier."
    ;;NOTE:  You cannot run this outside of a granted CAP.
    (require-capability (WLMOD))
    (with-read collections collection 
      {
        "types":= types
      }
      (let* 
        (
          (endtime (at "endTime" (first (filter (compose (at "id") (= id)) types))))
          (starttime (at "startTime" (first (filter (compose (at "id") (= id)) types))))
          (mintstatus (is-whitelisted collection id account))
          (hasminted (has-minted collection id account))
        )
        
        ;;Make sure the time is not elapsed for the type or is a lord
        (enforce 
          (or 
            (= id "lord")
            (and
              (>= (curr-time) starttime) 
              (<= (curr-time) endtime) 
            )
          )
          (format
            "{} type cannot be minted in illegal time start:{} end:{} current:{}" 
            [id starttime endtime (curr-time)]
          )
        )

        ;;Make sure the kings does not double list.

        ;;Make sure the kings does not double list.
        (enforce (not (or mintstatus hasminted)) (format "The wallet {} has already whitelisted or minted" [account]))

        ;;passed validation,  can now whitelist
        (insert whitelist-table (concat [collection ":" id ":" account])
          {
            "id": id, 
            "account": account,
            "mint-count": 0
          } 
        )
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Is-whitelisted
  ;;-------------------------------------------------
  ;; Checks if an account is whitelisted for a certain 
  ;; NFT type in the kadena-kings collection.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  (defun is-whitelisted:bool
    (
      collection:string
      id:string
      account:string
    )
    @doc "Returns true if the account is whitelisted for the given tier."
    (let
      (
        (whitelist-id (get-whitelist-id collection id account))
      )
      (with-default-read whitelist-table whitelist-id
        { "mint-count": -1 }
        { "mint-count":= mint-count }
        (= mint-count 0)
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Has-minted
  ;;-------------------------------------------------
  ;; Checks if an account has minted a certain NFT type in 
  ;; the kadena-kings collection.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  (defun has-minted:bool
    (
      collection:string
      id:string
      account:string
    )
    @doc "Returns true if the account has minted for the given tier."
    (let
      (
        (whitelist-id (get-whitelist-id collection id account))
      )
      (with-default-read whitelist-table whitelist-id
        { "mint-count": -1 }
        { "mint-count":= mint-count }
        (>= mint-count 1)
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Is-invalidated
  ;;-------------------------------------------------
  ;; Checks if a whitelister account is invalidated
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  (defun is-invalidated:bool
    (
      collection:string
      id:string
      account:string
    )
    @doc "Returns true if the account is in invlaid minter"
    (let
      (
        (whitelist-id (get-whitelist-id collection id account))
      )
      (with-default-read whitelist-table whitelist-id
        { "mint-count": 0 }
        { "mint-count":= mint-count }
        (= mint-count -1)
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; IO/General functions         
  ;;-------------------------------------------------
  ;; General Helper functions for unit testing
  ;; check-for-named-collection: checks for a certain collection of nft
  ;; > get-owned: retrieves all owned NFTs by a single address
  ;; > curr-time: retrieves the current block-time
  ;; > get-all-nft: gets all nfts in a list
  ;; > get-all-revealed-nft: gets all nfts on the NG ledger and are revealed
  ;; > get-wl-collection: gets all whitelisted people
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun check-for-named-collection:string (idx:string) 
    @doc "Checks the collections object for a named collection"
    (with-read collections idx {"name":= name}
      (format "The table contains the collection: {}" [name])
    )     
  )

  (defun check-collection-data:string (idx:string) 
    @doc "Checks the collections object for a named collection"
    (format "{}" [(read collections idx)])   
  )
 
  (defun get-owned:[object:{minted-token}]
    (
      account:string
    )
    @doc "Returns a list of tokens owned by the account."
    (select minted-tokens (where "account" (= account)))
  )

  (defun get-all-revealed:[object:{minted-token}]()
    @doc "Returns a list of all revealed tokens."
    (select minted-tokens (where "revealed" (= true)))
  )
 
  (defun get-all-unrevealed:[object:{minted-token}]()
    @doc "Returns a list of all unrevealed tokens." 
    (with-capability (CREATECOL)
      (select minted-tokens (where "revealed" (= false)))
    )
  )
  
  (defun fetch-policies:string (type:string)
    @doc "Returns a policy list based on type"
    (at "policy_stack" (first (select policies (where "type" (= type)))))
  )
 
  (defun get-all-nft ([object:{minted-token}])
    @doc "Returns a list of all tokens."
    (keys minted-tokens)
  )
   
  (defun get-wl-collection ([object:{whitelisted}])
    @doc "pull list of whitelist ID's for all collections."
    (keys whitelist-table)
  )

  (defun curr-time:time ()
    @doc "Returns current chain's block-time"
    (at 'block-time (chain-data))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create the Critical Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


