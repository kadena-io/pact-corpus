(module isoko-orchestrator GOVERNANCE

  @doc "Collection token policy."


  (use kip.token-policy-v1 [token-info])
  (use marmalade.ledger)
  (use kip.token-manifest)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Constants
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defconst COLLECTION_SUPPLY "collection-unique-supply"
    @doc "Max Unique Tokens in a collection")


  (defconst SGK_WEAPONS_LEVEL_ONES_DEVO ["test-sgk-weapons-4-1" "test-sgk-weapons-5-1" "test-sgk-weapons-6-1" "test-sgk-weapons-7-1" "test-sgk-weapons-8-1" "test-sgk-weapons-9-1"
    "test-sgk-weapons-10-1" "test-sgk-weapons-11-1" "test-sgk-weapons-12-1" "test-sgk-weapons-13-1" "test-sgk-weapons-14-1" "test-sgk-weapons-15-1" "test-sgk-weapons-16-1"
    "test-sgk-weapons-17-1" "test-sgk-weapons-18-1" "test-sgk-weapons-19-1" "test-sgk-weapons-20-1" "test-sgk-weapons-21-1" "test-sgk-weapons-22-1" "test-sgk-weapons-23-1"
    "test-sgk-weapons-24-1" "test-sgk-weapons-25-1" "test-sgk-weapons-26-1" "test-sgk-weapons-27-1" "test-sgk-weapons-28-1" "test-sgk-weapons-29-1" "test-sgk-weapons-30-1"
    "test-sgk-weapons-31-1" "test-sgk-weapons-32-1"]
    @doc "Max Unique Tokens in a collection")

(defconst SGK_WEAPONS_LEVEL_ONES_PROD [
  "sgk-weapons-1-1" "sgk-weapons-2-1" "sgk-weapons-3-1" "sgk-weapons-4-1" "sgk-weapons-5-1" "sgk-weapons-6-1" "sgk-weapons-7-1" "sgk-weapons-8-1" "sgk-weapons-9-1" 
  "sgk-weapons-10-1" "sgk-weapons-11-1" "sgk-weapons-12-1" "sgk-weapons-13-1" "sgk-weapons-14-1" "sgk-weapons-15-1" "sgk-weapons-16-1" "sgk-weapons-17-1" "sgk-weapons-18-1" 
  "sgk-weapons-19-1" "sgk-weapons-20-1" "sgk-weapons-21-1" "sgk-weapons-22-1" "sgk-weapons-23-1" "sgk-weapons-24-1" "sgk-weapons-25-1" "sgk-weapons-26-1" "sgk-weapons-27-1"
  "sgk-weapons-28-1" "sgk-weapons-29-1" "sgk-weapons-30-1" "sgk-weapons-31-1" "sgk-weapons-32-1"])


(defconst SGK_WEAPONS_LEVEL_ONES_PROD_MINT [
    "sgk-weapons-1-1" "sgk-weapons-2-1" "sgk-weapons-3-1" "sgk-weapons-4-1" "sgk-weapons-5-1" "sgk-weapons-6-1" "sgk-weapons-7-1" "sgk-weapons-8-1" "sgk-weapons-9-1"
    "sgk-weapons-10-1" "sgk-weapons-11-1" "sgk-weapons-12-1" "sgk-weapons-13-1" "sgk-weapons-14-1" "sgk-weapons-15-1" "sgk-weapons-16-1"
    "sgk-weapons-17-1" "sgk-weapons-18-1" "sgk-weapons-19-1" "sgk-weapons-20-1" "sgk-weapons-21-1" "sgk-weapons-22-1" "sgk-weapons-23-1"
    "sgk-weapons-24-1" "sgk-weapons-25-1" "sgk-weapons-26-1" "sgk-weapons-27-1" "sgk-weapons-28-1" "sgk-weapons-29-1" "sgk-weapons-30-1"
    "sgk-weapons-31-1" "sgk-weapons-32-1"])

(defconst SGK_WEAPONS_LEVEL_ONES_TEST_MINT ["test-sgk-weapons-4-1" "test-sgk-weapons-5-1" "test-sgk-weapons-6-1" "test-sgk-weapons-7-1" "test-sgk-weapons-8-1" "test-sgk-weapons-9-1"
    "test-sgk-weapons-10-1" "test-sgk-weapons-11-1" "test-sgk-weapons-12-1" "test-sgk-weapons-13-1" "test-sgk-weapons-14-1" "test-sgk-weapons-15-1" "test-sgk-weapons-16-1"
    "test-sgk-weapons-17-1" "test-sgk-weapons-18-1" "test-sgk-weapons-19-1" "test-sgk-weapons-20-1" "test-sgk-weapons-21-1" "test-sgk-weapons-22-1" "test-sgk-weapons-23-1"
    "test-sgk-weapons-24-1" "test-sgk-weapons-25-1" "test-sgk-weapons-26-1" "test-sgk-weapons-27-1" "test-sgk-weapons-28-1" "test-sgk-weapons-29-1" "test-sgk-weapons-30-1"
    "test-sgk-weapons-31-1" "test-sgk-weapons-32-1"])


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Capabilities
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defcap GOVERNANCE ()
        (enforce-one "Any Guard passes" [
            (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
            (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")))]
        )
  )

  (defcap RESERVE-NFTS (collection-id:string owner:string)
    (let*
      (
          (collection (get-collection collection-id))
          (collection-guard (at 'collection-guard collection))
      )
      (compose-capability (ADMIN-OR-COL-OWNER collection-id))
      (compose-capability (RANDOM owner))
      (compose-capability (GET-TOKENS))

    )
  )
  (defcap ACCOUNT_GUARD(account:string) ; Used for admin functions
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard   
            (at "guard" (coin.details account))
        )
  )

  (defcap REWARDS-ORIGIN ()
        ;;update to allow caller being rewards policy
        (enforce-one "Any Guard passes" [(enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")) (enforce-guard (keyset-ref-guard "free.isoko-generator"))])
        (compose-capability (REWARDS-PVT))
  )
  (defcap ADMIN-OR-DELEGATE (collection-id:string)
    
    (enforce-one "Any Guard passes" [
        (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")) 
        (enforce-guard (keyset-ref-guard "free.isoko-generator"))
        (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")))
    ])
    
  )
  (defcap ADMIN-OR-COL-OWNER (collection-id:string)
    (let*
      (
          (collection (get-collection collection-id))
          (collection-guard (at 'collection-guard collection))
      )
      (enforce-one "Any Guard passes" [(enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
            (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")))])
    )
  )
  (defcap MINT-RESERVED-NFTS (collection-id:string)
    (let*
      (
          (collection (get-collection collection-id))
          (collection-guard (at 'collection-guard collection))
      )
      (compose-capability (ADMIN-OR-COL-OWNER collection-id))
    )
  )
  (defcap MINT(token-id:string new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    true
  )
  (defcap MINT-INTERNAL()
    @doc "Capability to wrap all needed caps for minting"
    true
  )
  (defcap MINT-BULK(new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    (compose-capability (RANDOM new-owner))
    (compose-capability (GET-TOKENS))
    (compose-capability (MINT-INTERNAL))

  )

  (defcap MINT-BULK-REWARDS(new-owner:string amount:integer)
    @managed
    (compose-capability (RANDOM new-owner))
    (compose-capability (GET-TOKENS))
    (compose-capability (MINT-INTERNAL))
    (compose-capability (REWARDS-PVT))

  )
  (defcap CREATE-NFTS-BULK(collection-id:string)
    @doc "Capability to wrap all needed caps for minting"
    @event
    (compose-capability (ADMIN-OR-DELEGATE collection-id))
    (compose-capability (CREATE-NFT collection-id))
    (compose-capability (UPDATE-COLLECTION collection-id))

  )
  (defcap CREATE-NFT (collection-id:string)
    @doc "Private capability for creating an nft."
    true
  )
  (defcap UPDATE-COLLECTION (collection-id:string)
    @doc "Private capability for updating collection information."
    true
  )
  (defcap RANDOM (account:string)
    @doc "Private capability."
    true
  )
  (defcap GET-TOKENS ()
    @doc "Private capability."
    true
  )
  (defcap REWARDS-PVT ()
    @doc "Private capability."
    true
  )

  (defun isoko-orchestrator-guard:guard ()
    @doc "orchestrator module guard for policy to be able to validate access to collection information."
    (create-module-guard "isoko-orchestrator-guard")
  )
  (defschema collection
    id:string
    collection-guard:guard
    non-minted-tokens:[string]
    reserved-tokens:[string]
    minted-total:integer
    current-unique-supply:integer
    max-unique-supply:integer
    policy:module{kip.token-policy-v1}
  )

  (defschema token
    id:string
    collection-id:string
    supply:decimal
  )

  (defschema whitelist-info
    collection-id:string
    index:integer
    account:string
    guard:guard
  )

  (defschema sgk-reserve-tracking-schema
    collection-id:string
    staking-reserves:integer
    tourney-reserves:integer
    other-reserves:integer
  )

  (defschema sgk-reserve-tracking-cool-af-schema
    collection-id:string
    staking-reserves:integer
    tourney-reserves:integer
    other-reserves:integer
    hidden-reserve-list:[string]
  )


  (deftable collections:{collection})
  (deftable tokens:{token})
  (deftable sgk-reserve-tracking:{sgk-reserve-tracking-schema})

  (defcap INTERNAL () true)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Creating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun create-collection (
      policy:module{kip.token-policy-v1}
    )
    (with-capability (GOVERNANCE)
        
      (let* (
              (creator-guard:guard (read-msg "creator-guard"))
              (collection-identifier:string (read-msg 'collection-id))
              (collection-max-unique-supply:integer (read-integer COLLECTION_SUPPLY))
              )
              (insert collections collection-identifier
                  {'id:collection-identifier
                  ,'collection-guard:creator-guard
                  ,'non-minted-tokens:[]
                  ,'reserved-tokens:[]
                  ,'minted-total:0
                  ,'current-unique-supply:0
                  ,'max-unique-supply:collection-max-unique-supply
                  ,'policy:policy})
                  true
        )
    )
  )
  (defun create-nfts-bulk:list ()
    @doc "Bulk creates marmalade tokens for any collection. Formatted in specs : [{nft-data : {}, uri-data : "", uri-scheme: ""}]"
    (with-capability (GOVERNANCE)
      (let*
        (
          (collection-id (read-msg "collection-id"))
          (specs:[object] (read-msg "specs"))
          (collection (get-collection collection-id))
        )
        (with-capability (CREATE-NFTS-BULK collection-id)
          (let*
            (
              (token-ids (map (create-nft-token collection collection-id) specs))
              (amount (length token-ids))
            )
            (update-non-minted-collection token-ids collection-id collection)
            token-ids
          )

        )
       )
    )
  )
  (defun create-nfts-bulk-v2:list ()
    @doc "Bulk creates marmalade tokens for any collection (BB). Formatted in specs : [{nft-data : {}, uri-data : "", uri-scheme: ""}]"
      (let*
        (
          (collection-id (read-msg "collection-id"))
          (specs:[object] (read-msg "specs"))
          (collection (get-collection collection-id))
        )


        (with-capability (CREATE-NFTS-BULK collection-id)
          (let*
            (
              (token-ids (map (create-nft-token-v2 collection collection-id) specs))
              (amount (length token-ids))
            )
            (update-non-minted-collection token-ids collection-id collection)
            token-ids
          )
        )
      )
  )

  ;; DEPRECATED 
  (defun create-nft-token:string(collection:object{collection} collection-id:string obj:object)
    @doc "Creates a manifest then builds a marmalade token using supplied policy"
    (require-capability (CREATE-NFT collection-id))
    (let*
      (
        (policy (at 'policy collection))
        (datum-data (at 'nft-data obj))
        (uri-data:string (at 'uri-data obj))
        (uri-scheme:string (at 'uri-scheme obj))
        (nft-data (create-datum (uri "pact:schema" "nft-data") datum-data))
        (manifest:object{manifest} (create-manifest (uri uri-scheme uri-data) [nft-data]))
        (token-id (marmalade.ledger.create-token-id manifest))
      )
      (marmalade.ledger.create-token token-id 0 manifest policy)
      token-id
      )
  )
  (defun create-nft-token-v2:string(collection:object{collection} collection-id:string obj:object)
    @doc "Creates a manifest then builds a marmalade token using supplied policy"
    (require-capability (CREATE-NFT collection-id))
    (let*
      (
        (policy (at 'policy collection))
        (datum (at 'datum obj))
        (datum-data (at 'datum datum))
        (datum-uri (at 'uri datum))
        (datum-uri-data:string (at 'data datum-uri))
        (datum-uri-scheme:string (at 'scheme datum-uri))
        (top-uri (at 'uri obj))

        (nft-datum:object{mf-datum} (create-datum (uri datum-uri-scheme datum-uri-data) datum-data))
        (manifest:object{manifest} (create-manifest (uri (at 'scheme top-uri) (at 'data top-uri)) [nft-datum]))
        (token-id-suffix (at 'id (at 'datum datum)))
        (token-id (+ (+ collection-id ":") token-id-suffix))
      )
      (marmalade.ledger.create-token token-id 0 manifest policy)
      token-id
      )
  )
  (defun update-non-minted-collection (token-ids:list collection-id:string collection:object{collection} )
        (require-capability (UPDATE-COLLECTION collection-id))
        (bind collection
          { 'non-minted-tokens:= non-minted-tokens:list
            ,'current-unique-supply:= current-supply:integer
          }
          (update collections collection-id
            {'non-minted-tokens: (+ token-ids non-minted-tokens)
            ,'current-unique-supply: (+ (length token-ids) current-supply)}
          )
        )
  )

    (defun clean-non-minted-list:bool
        ( token-list:[string] collection-name:string)
        (with-capability (GOVERNANCE)
            
                (update collections collection-name
                    {'non-minted-tokens: token-list
                
                })
      )
    )

    (defun clean-reserves-and-non-minted-lists:bool
        ( reserves-list:[string] non-minted-list:[string] collection-id)
        (with-capability (GOVERNANCE)
            (update collections collection-id
                    {
                        'non-minted-tokens: non-minted-list,
                        'reserved-tokens:reserves-list
                
                })
      )
    )

(defun clean-reserves-lists:bool
        ( reserves-list:[string] collection-id)
        (with-capability (GOVERNANCE)
            (update collections collection-id
                    {
                        'reserved-tokens:reserves-list
                
                })
      )
    )

    (defun clean-non-minted-lists:bool
        ( non-minted-list:[string] collection-id)
        (with-capability (GOVERNANCE)
            (update collections collection-id
                    {
                        'non-minted-tokens: non-minted-list
                
                })
      )
    )

    (defun filter-non-minted-list:bool
        ( token-list:[string] )
        (with-capability (GOVERNANCE)
            (let* (
                    (non-minted-tokens (get-non-minted-tokens "sgk-gen-1"))
                )
                (filter (in-list token-list) non-minted-tokens)
            )
      )
    )

    (defun in-list (in-list:list element:string)
        (not (contains element in-list))
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Reserving and Minting from Reserve : For Projects to Guarantee NFTs for
  ; Promotional uses/Marketing. Reserve should called before mint start ideally.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun reserve-tokens (amount-to-reserve:integer owner:string collection-id:string)
    (with-capability (RESERVE-NFTS collection-id owner)
      (let*
          (
          (random (get-random owner))
          (token-ids-to-reserve:list (get-token-ids amount-to-reserve collection-id owner random false))
          (token-ids-to-reserve-length (length token-ids-to-reserve))
          (collection (get-collection collection-id))
          (reserved-tokens:list (at 'reserved-tokens collection))
          (created-tokenList (at 'non-minted-tokens collection))
          (created-tokenList-length (length created-tokenList))
          (new-created-token-list (subs-wrapper created-tokenList random amount-to-reserve))
          )
          (enforce (<= token-ids-to-reserve-length created-tokenList-length) "Not enough tokens to reserve!")
          (update collections collection-id
                {'non-minted-tokens: new-created-token-list,
                 'reserved-tokens: (+ reserved-tokens token-ids-to-reserve)})
      )
    )
  )
  (defun mint-bulk-reserved (
    amount:integer
    collection-id:string
    owner:string
    guard:guard)
    (with-capability (MINT-RESERVED-NFTS collection-id)
      (with-capability (MINT-BULK owner)
        (let*
            (
            (random (get-random owner))
            (token-ids-to-mint:list (get-token-ids amount collection-id owner random true))
            (collection (get-collection collection-id))
            (reserved-tokenList (at 'reserved-tokens collection))
            (reserved-tokenList-length (length reserved-tokenList))
            (total-to-mint (length token-ids-to-mint))
            (minted-total (at 'minted-total collection))
            (new-created-token-list (subs-wrapper reserved-tokenList random amount))
            )
            (enforce (<= total-to-mint reserved-tokenList-length) "Not enough tokens to mint!")
            (update collections collection-id
                  {'reserved-tokens: new-created-token-list,
                   'minted-total: (+ total-to-mint minted-total)})
            (map (mint owner guard) token-ids-to-mint)
        )
      )
    )
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Reserving and Minting for SGK rewards policies
  ; These are special purpose functions to work with reward-policies for SGK game ecosystem
  ; The SGK guard are enforced & any contract enabling reward reserves should reserve ahead of time by use
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (defun bulk-claim-weapons-staking:list (
    owner:string
    claim-amount:integer
    guard:guard)
        (with-capability (MINT-BULK-REWARDS owner claim-amount)
            (let* 
                (
                    (claim-list:list (make-list claim-amount 1))
                   ;; (all-weapon-lvl-1-collections:list (n_f1c962776331c4773136dc1587a8355c9957eae1.sgk-rewards-util.get-col-names))
                    (token-ids-to-mint:list (map (TEST_claim-weapon-staking owner guard SGK_WEAPONS_LEVEL_ONES_PROD) claim-list))
                )
                (map (mint owner guard) token-ids-to-mint)
            )
        )
   )
  
  (defun TEST_claim-weapon-staking:list (
    owner:string
    guard:guard
    all-weapon-lvl-1-collections
    nothing:integer)
        (require-capability (MINT-INTERNAL))
        (require-capability (REWARDS-PVT))

        (let*
            (   
                (rand (free.util-random.random-int))
                ;; find random weapon level 1 col to claim from based on available reserves of each
                (collection-id-to-reserve-from (TEST-get-and-decrement-random-staking-reserve-collection rand all-weapon-lvl-1-collections))
                
                (collection (get-collection collection-id-to-reserve-from))
                (reserved-tokenList (at 'reserved-tokens collection))

                ;;pick a random token to mint from reserves (bool = true) for that collection
                (token-ids-to-mint:list (take 1 reserved-tokenList))
                
                (collection (get-collection collection-id-to-reserve-from))
                (reserved-tokenList-length (length reserved-tokenList))
    
                (minted-total (at 'minted-total collection))
                ;;updated reserved-tokenList to not include token picked out for claim by splitting
                (new-reserved-token-list (drop 1 reserved-tokenList))
            )
            (enforce (<= 1 reserved-tokenList-length) "Not enough tokens to mint!")
            (update collections collection-id-to-reserve-from
                  {'reserved-tokens: new-reserved-token-list,
                   'minted-total: (+ 1 minted-total)})
            (at 0 token-ids-to-mint)
        )
  )

    (defun TEST-get-and-decrement-random-staking-reserve-collection:string (rand:integer all-weapon-lvl-1-collections:list)
        (require-capability (REWARDS-PVT))
        (let* 
            (
                (reserved-cols-list:list  (map (lambda (x) (get-reserve-token-amounts-staking-for-col x)) all-weapon-lvl-1-collections))
                (has-next-non-empty-reserve-cols-list:list (filter (compose (get-staking-reserve-amt) (< 0)) reserved-cols-list))
                (number-of-non-empty-cols (length has-next-non-empty-reserve-cols-list))
                (enforce (> 0 number-of-non-empty-cols) "No NFTs left for staking rewards. Contact Admin to release more weaponnnns!!")
                ;;TESTTODO Test mod logic
                (random (mod rand number-of-non-empty-cols))
                (picked-coll-id (at "collection-id" (at random has-next-non-empty-reserve-cols-list)))
                (picked-coll-reserves (at "staking-reserves" (at random has-next-non-empty-reserve-cols-list)))

            )
            ;;decrement reserve amounts for staking for that collection
            (decrement-staking-reserves picked-coll-id picked-coll-reserves)
            picked-coll-id
        )
    )

    (defun get-weapons-lvl-1-names:list()

        (let* 
            (
                (collection-suffix-list:list (enumerate 1 32)) 
                (collection-suffix-strings-list:list (map (lambda (x) (int-to-str 10 x)) collection-suffix-list))
                (collection-types-list:list (map (lambda (x) (+ "test-sgk-weapons-" x)) collection-suffix-strings-list))
                (collection-full-names-list:list (map (lambda (x) (+ x "-1" )) collection-types-list))
            )
            collection-full-names-list
        )
    )

    (defun decrement-staking-reserves(col-id:string curr-staking-reserves:integer)
         (require-capability (REWARDS-PVT))
            (update sgk-reserve-tracking col-id
                {'staking-reserves: (- curr-staking-reserves 1)})
    )


    (defun fill-staking-reserves(col-id:string staking-reserves:integer tourney-reserves:integer other-reserves:integer)
        (with-capability (GOVERNANCE)
            (write sgk-reserve-tracking col-id
                {
                    'collection-id: col-id,
                    'staking-reserves: staking-reserves,
                    'tourney-reserves:tourney-reserves,
                    'other-reserves:other-reserves
                })
        )
    )

    (defun update-staking-reserves:integer(col-id:string new-amt:integer)
        (with-default-read sgk-reserve-tracking col-id
        {
            'collection-id: col-id,
            'staking-reserves: 0,
            'tourney-reserves:0,
            'other-reserves:0
        }
        {
        'staking-reserves:=  staking-reserves
        }
        (update sgk-reserve-tracking col-id
                {
                    'staking-reserves: (+ staking-reserves new-amt)
                })
        (+ staking-reserves new-amt)
        )
 
    )

    (defun visible-for-test-get-reserved-tokens (col-id:string )
        (if (= col-id "battle-heroes-weapons-policy-level-1-col-6")
                {"reserved-tokens": ["1" "2"], "collection-id" : "battle-heroes-weapons-policy-level-1-col-6"}
                {"reserved-tokens": [], "collection-id" : (+ "battle-heroes-weapons-policy-level-1-col-" col-id)}
        )
    )
    ;;TESTTODO Test with multiple non empty 
    (defun TEST_get-reserve-token-amounts-staking-for-col (col-id:string )
        (if (= col-id "test-sgk-weapons-2-1")
                {"staking-reserves": 4, "collection-id" : "test-sgk-weapons-2-1"}
                {"staking-reserves": 0, "collection-id" : (+ (+ "test-sgk-weapons-" col-id) "-1")}
        )
    )

  (defun get-reserve-token-amounts-staking-for-col (col-id:string)
        (read sgk-reserve-tracking col-id ["staking-reserves" "collection-id"])
  )


  (defun get-non-minted-tokens-col (col-id:string)
        (read collections col-id ["non-minted-tokens" "collection-id"])
  )

  (defun get-reserved-tokens-col (col-id:string)
        (read collections col-id ["reserved-tokens" "collection-id"])
  )

    (defun get-staking-reserve-amt:integer (full-obj)
        (let*
            (
                (staking-reserves:integer (at 'staking-reserves full-obj))
            ) 
            staking-reserves
        )
    )

    ;;Deprecated
    (defun length-of-reserved:integer (full-obj)
        (let*
            (
                (reserved-list:list (at 'reserved-tokens full-obj))
                (length-of-reserved:integer (length reserved-list))
            ) 
            length-of-reserved
        )
    )

    ;;Deprecated
    (defun get-random-staking-reserve-collection (rand:integer)
        (let* 
            (
                (collection-suffix-list:list (enumerate 1 32)) 
                (collection-suffix-strings-list:list (map (lambda (x) (int-to-str 10 x)) collection-suffix-list))
                (collection-names-list:list (map (lambda (x) (+ "battle-heroes-weapons-policy-level-1-col-" x)) collection-suffix-strings-list))
                (reserved-cols-list:list  (map (lambda (x) (visible-for-test-get-reserved-tokens x)) collection-names-list))
                (has-next-non-empty-reserve-cols-list:list (filter (compose (length-of-reserved) (< 0)) reserved-cols-list))
                (random (mod rand (length has-next-non-empty-reserve-cols-list)))
            )
            (at "collection-id" (at random has-next-non-empty-reserve-cols-list))

        )
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Minting
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun mint-bulk (
    amount:integer
    collection-id:string
    owner:string
    guard:guard)

    (with-capability (MINT-BULK owner)
      (let*
          (
          (random (get-random owner))
          (token-ids-to-mint:list (get-token-ids amount collection-id owner random false))
          (collection (get-collection collection-id))
          (created-tokenList (at 'non-minted-tokens collection))
          (created-tokenList-length (length created-tokenList))
          (total-to-mint (length token-ids-to-mint))
          (minted-total (at 'minted-total collection))
          (new-created-token-list (subs-wrapper created-tokenList random amount))
          )
          (enforce (<= total-to-mint created-tokenList-length) "Not enough tokens to mint!")

          (update collections collection-id
                {'non-minted-tokens: new-created-token-list,
                 'minted-total: (+ total-to-mint minted-total)})
          (map (mint owner guard) token-ids-to-mint)
      )
    )
  )

  (defun subs-wrapper:list(l:list random:integer amount:integer)
      (let*
          (
              (n (length l))
          )
          (if (>= amount n) [] (subs l random amount n))
      )
  )

  (defun subs (l:list rand:integer amount:integer n:integer)
      ; prefix: the sublist before the removed elements
      ; postfix: the sublist after the removed elements
      ;; security through obscurity HAHAHA. Pls no heck
        (let*
          (
              (random (mod rand n))
              (y_m (+ random amount))
              (pre_s (if (>= y_m n) (mod y_m n) 0))
              (use_postfix (if (>= y_m n) false true))
              (pre_e random)
              (prefix (take (- pre_e pre_s) (drop pre_s l)))
              (postfix:list [])
              (postfix_final (if use_postfix (drop y_m l) postfix))
          )
          ( + prefix postfix_final)
        )
      )

  (defun get-token-ids (amount:integer collection-id:string owner:string random:integer using-reserved:bool)
    @doc "returns a list of randomly selected token ids matching amount"
    (require-capability (GET-TOKENS))
    (let*
      (
        (randoms (get-random owner))
      )
      (get-token-ids-from-index random amount collection-id using-reserved)
    )
  )
  (defun get-token-ids-from-index (start-index:integer amount:integer collection-id:string using-reserved:bool)
      ;; ending at start-index + amount
      ;; all indicies = enemurate start - end +1
      ;; t length = max - minted (can just be length non-minted)
      ;; m-indicies = indicies to mint : take each index and do mod.
      ;; f-index = function to get token from index
      ;; security through obscurity HAHAHA. Pls no heck
      (require-capability (GET-TOKENS))
      (let*
        (
          (collection (get-collection collection-id))
          (candidate-tokens (if using-reserved (at 'reserved-tokens collection) (at 'non-minted-tokens collection)))
          (end-index (+ start-index (- amount 1)))
          (indicies:list (get-indicies start-index end-index))
          (t-length (length candidate-tokens))
          (f-get (lambda (t-length x) (mod x t-length)))
          (m-indicies:list (map (f-get t-length) indicies))
          (f-index (lambda (x) (at x candidate-tokens)))
          (token-ids (map (f-index) m-indicies))
        )
        token-ids
      )
  )

  (defun get-indicies (start:integer end:integer)
    (enumerate start end)
  )

  (defun not-in-list (in-list:list element:string)
    (not (contains element in-list))
  )

  (defun mint(owner:string guard:guard token-id:string)
    (require-capability (MINT-INTERNAL))
    (install-capability (marmalade.ledger.MINT token-id owner 1.0))
    (marmalade.ledger.mint token-id owner guard 1.0)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Getters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun get-random:integer (account:string)
    (require-capability (RANDOM account))
    (let* (
      (prev-block-hash (at "prev-block-hash" (chain-data)))
      (block-time (at 'block-time (chain-data)))
      (block-hash (hash block-time))
      (total-hash (hash (+ block-hash prev-block-hash)))
      (random (str-to-int 64 (hash (+ total-hash (take 20 account)))))
    )
    random
    )
  )


  (defun get-collection (col-id:string)
    (read collections col-id)
  )
  (defun get-all-collections (col-id:string)
    (keys collections)
  )

  (defun get-non-minted-tokens (col-id:string)
    (at 'non-minted-tokens (read collections col-id))
  )

  (defun get-next-non-minted-token(col-id:string)
    (at 0 (at 'non-minted-tokens (read collections col-id)))
  )

(defun get-reserved-tokens (col-id:string)
    (at 'reserved-tokens (read collections col-id))
  )


   (defun bulk-mint-weapons:list (
    owner:string
    mint-amount:integer
    guard:guard)
        (with-capability (MINT-BULK-REWARDS owner mint-amount)
            (let* 
                (
                    (mint-list:list (make-list mint-amount 1))
                   ;; (all-weapon-lvl-1-collections:list (n_f1c962776331c4773136dc1587a8355c9957eae1.sgk-rewards-util.get-col-names))
                    (token-ids-to-mint:list (map (get-token-id-for-weapon-mint owner guard SGK_WEAPONS_LEVEL_ONES_PROD_MINT) mint-list))
                )
                (map (mint owner guard) token-ids-to-mint)
            )
        )
   )

    (defun bulk-mint-weapons-test:list (
    owner:string
    mint-amount:integer
    guard:guard)
        (with-capability (MINT-BULK-REWARDS owner mint-amount)
            (let* 
                (
                    (mint-list:list (make-list mint-amount 1))
                   ;; (all-weapon-lvl-1-collections:list (n_f1c962776331c4773136dc1587a8355c9957eae1.sgk-rewards-util.get-col-names))
                    (token-ids-to-mint:list (map (get-token-id-for-weapon-mint owner guard SGK_WEAPONS_LEVEL_ONES_TEST_MINT) mint-list))
                )
                (map (mint owner guard) token-ids-to-mint)
            )
        )
   )

    (defun get-token-id-for-weapon-mint:list (
        owner:string
        guard:guard
        all-weapon-lvl-1-collections
        nothing:integer)
        (require-capability (MINT-INTERNAL))
        (require-capability (REWARDS-PVT))

        (let*
            (   
                (random (free.util-random.random-int))
                ;; find random weapon level 1 col to claim from based on available reserves of each
                (collection-id-to-reserve-from (get-and-decrement-random-mint-collection random all-weapon-lvl-1-collections))
                (collection (get-collection collection-id-to-reserve-from))
                (reserves-left (at "other-reserves" (get-mint-reserves-amount-for-col collection-id-to-reserve-from)))

                (token-number-to-mint-p:integer (- 10001 reserves-left))
                (first-two-colls:bool (or (= collection-id-to-reserve-from "sgk-weapons-1-1") (= collection-id-to-reserve-from "sgk-weapons-2-1")))
                (should-override-token-id:bool (and (= first-two-colls true) (= token-number-to-mint-p 9951)))
                (token-number-to-mint (if (= should-override-token-id true) 9900 token-number-to-mint-p))

                (id-suffix (int-to-str 10 token-number-to-mint))
                (post-fix (+ collection-id-to-reserve-from ":"))
                (full-token-id (+ post-fix id-suffix))
                (minted-total (at 'minted-total collection))
                ;;updated reserved-tokenList to not include token picked out for claim by splitting
            )
            (decrement-other-reserves collection-id-to-reserve-from reserves-left)
            (update collections collection-id-to-reserve-from
                  {'minted-total: (+ 1 minted-total)})
            full-token-id
        )
    )

    (defun get-and-decrement-random-mint-collection:string (rand:integer all-weapon-lvl-1-collections:list)
        (require-capability (REWARDS-PVT))
        (let* 
            (
                (reserved-cols-list:list  (map (lambda (x) (get-mint-reserves-amount-for-col x)) all-weapon-lvl-1-collections))
                (has-next-non-empty-reserve-cols-list:list (filter (compose (get-other-reserve-amt) (< 0)) reserved-cols-list))
                (number-of-non-empty-cols (length has-next-non-empty-reserve-cols-list))
                (random (if (> number-of-non-empty-cols 0) (mod rand number-of-non-empty-cols) 0))
;                (random (mod rand number-of-non-empty-cols))
                (picked-coll-id (if (> number-of-non-empty-cols 0) (at "collection-id" (at random has-next-non-empty-reserve-cols-list)) ""))
            )
            (enforce (> number-of-non-empty-cols 0 ) "No NFTs left for minted rewards. Contact Admin to release more weaponnnns!!")
            ;;decrement reserve amounts for staking for that collection
            picked-coll-id
        )
    )

    (defun get-mint-reserves-amount-for-col (col-id:string)
        (read sgk-reserve-tracking col-id ["other-reserves" "collection-id"])
    )

    (defun decrement-other-reserves(col-id:string curr-other-reserves:integer)
         (require-capability (REWARDS-PVT))
            (update sgk-reserve-tracking col-id
                {'other-reserves: (- curr-other-reserves 1)})
    )
    
    (defun get-other-reserve-amt:integer (full-obj)
        (let*
            (
                (other-reserves:integer (at 'other-reserves full-obj))
            ) 
            other-reserves
        )
    )

    (defun update-other-reserves:integer(col-id:string new-amt:integer)
        (with-default-read sgk-reserve-tracking col-id
        {
            'collection-id: col-id,
            'staking-reserves: 0,
            'tourney-reserves:0,
            'other-reserves:0
        }
        {
        'other-reserves:=  other-reserves
        }
        (update sgk-reserve-tracking col-id
                {
                    'other-reserves: (+ other-reserves new-amt)
                })
        (+ other-reserves new-amt)
        )
 
    )

)

