(module KMPPV2 PASS

  (use n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1 [account-details sender-balance-change receiver-balance-change])
  (use free.util-strings [to-string starts-with])
  (use free.util-time [time-between now from-now])
  (use kip.token-manifest)
  (use free.util-fungible [enforce-precision enforce-reserved enforce-valid-account enforce-valid-transfer enforce-valid-amount])
  (use util.guards)
  (use n_4e470a97222514a8662dd1219000a0431451b0ee.ledger)
  (use n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection)

 ;-----------------------------------------------------------------------------
 ;Constants
 ;-----------------------------------------------------------------------------

  (defconst TOKENS "token-record")
  (defconst COLLECTION-NAME "priority_pass")
  (defconst PROPOSAL:string "proposal")
  (defconst TITLE:string "title")

  ;-----------------------------------------------------------------------------
  ; Capabilities
  ;-----------------------------------------------------------------------------

  (defcap PASS ()
    (enforce-guard (keyset-ref-guard "free.KMPPV2-ks-ks")))

  (defcap IS_ADMIN ()
    (compose-capability (PASS)))

  (defcap MINT_EVENT (account:string amount:integer )
    @event true )

  (defcap MINTPROCESS:bool ()
    @doc "Used to manage who can mint an NG NFT after paying"
    (compose-capability (PRIVATE))
    (enforce-guard (at 'guard (read collections COLLECTION-NAME ['guard ])))
    "Must be the collection creator guard")

  (defcap MINT-PASS (account:string)
    @managed ;; one-shot for a given amount
    (compose-capability (PRIVATE)))

  (defcap CREATE-COLLECTION ()
    @managed ;; one-shot for a given amount
     (compose-capability (PRIVATE)))

  (defcap PRIVATE () true )

  (defcap COLLECTION_CREATED ( name:string total-supply:integer guard:guard creator:string )
    @event true )

;-----------------------------------------------------------------------------
; Tables and Schemas
;-----------------------------------------------------------------------------

  (defschema passbalance-schema
    @doc "schema for holding the balance of a pass"
    balance:integer)

  (defschema collection-schema
    total-supply:integer
    collection-id:string
    collection-name:string
    creator:string
    guard:guard
    currentIndex:integer
    uri-list:[string]
    price:decimal
    policy:string)

  (defschema indexSchema
    index:integer)

  (defschema minted-token
    @doc "Stores the data for a minted token. \
    \ The id is the collection, tierId, account, and token-id."
    collection-name:string
    account:string
    guard:guard
    accountId:integer
    marmToken:string
    revealed:bool)

  (defschema account-schema
    account:string
    minted:integer)

  (defschema proposalIndex
    index:integer)

  (defschema proposal-schema
    start-time:time
    end-time:time
    title:string
    proposal-id:integer)

  (defschema proposalDetails
    title:string
    id:integer)

  (defschema token-record-schema
    uri-list:[string]
    current-length:integer)

  (defschema account-token-info
    account:string
    tokens:[string])

  (defschema nft-info
    owner:string
    tokenId:string
    collectionName:string)


  (defschema votingHistory
    account:string
    proposalId:integer)

  (defschema voteSchema
    yes:integer
    no:integer)

  (defschema voteCount
    proposalIds:[integer]
    account:string
    count:integer)
  (defschema burnDetialsSchema
    account:string
    guard:guard)

  (defschema priorityusers
    account:string)

  (deftable priority-users:{priorityusers})
  (deftable burnDetials:{burnDetialsSchema})
  (deftable collections:{collection-schema})
  (deftable account-details:{account-schema})
  (deftable passbalance_table:{passbalance-schema})
  (deftable indexTable:{indexSchema})
  (deftable minted-tokens:{minted-token})
  (deftable proposal:{proposal-schema})
  (deftable proposal-index:{proposalIndex})
  (deftable proposal-details:{proposalDetails})
  (deftable nfts-info-by-id:{nft-info})
  (deftable nfts-info-by-owner:{nft-info})
  (deftable accountsInfo:{account-token-info})
  (deftable token-record:{token-record-schema})
  (deftable voting-history:{votingHistory})
  (deftable votes:{voteSchema})
  (deftable votecount-table:{voteCount})

;-----------------------------------------------------------------------------
; Get functions
;-----------------------------------------------------------------------------

  (defun get-current-index()
    (at 'currentIndex (read collections COLLECTION-NAME)))

  (defun get-collection-creator()
    (at 'creator (read collections COLLECTION-NAME)))

  (defun get-total-supply()
    (at 'total-supply (read collections COLLECTION-NAME)))

  (defun get-collection-details ()
    (read collections COLLECTION-NAME))

  (defun get-collection-id ()
    (at 'collection-id (read collections COLLECTION-NAME)))

  (defun get-mint-price ()
    (at 'price (read collections COLLECTION-NAME)))

  (defun get-priority-users ()
    (keys priority-users))

  (defun get-tokenid-minted ()
    (keys nfts-info-by-id))

  (defun get-nft-info-by-token-id (token-id:string)
    (read nfts-info-by-id token-id))

  (defun get-propsal-voting-history (proposalId:string)
    (read votes proposalId))

  (defun get-proposal-details (proposalId:string)
    (read proposal proposalId))

  (defun get-all-proposal()
    (select proposal-details (where "id" (!= ""))))

  (defun get-pass-balance (account:string)
    (with-default-read passbalance_table account
        { 'balance: 0}
        { 'balance:= balance }
        balance
     )
   )

  (defun get-ids-owned-by (owner:string)
    @doc "All punks owned by someone"
      (select nfts-info-by-id ["tokenId"] (where "owner" (= owner)))
  )


  (defun get-all-tokens-by-account(account:string)
    (select accountsInfo
      (where "account" (= account))
    )
  )

  (defun get-mint-token-id:string
    ( collection:string
      accountId:integer )
    (concat [collection "|" (int-to-str 10 accountId)]))

  (defun get-policies-of-collection ()
    (with-read collections COLLECTION-NAME {
      "policy":=policy}
      policy))

  (defun get-account-minted:integer (account:string)
    (with-default-read account-details account
      {"minted": 0}
      {"minted":= minted}
      minted))

  (defun get-random:integer (account:string)
    (require-capability (PRIVATE))
    (let* ( (prev-block-hash (at "prev-block-hash" (chain-data)))
            (random (str-to-int 64 (hash (+ prev-block-hash (take 20 account)))))
          )
     random)
    )

  (defun get-current-length:integer ()
    (with-read token-record COLLECTION-NAME {
      'current-length:= current-length}
      current-length)
  )

  (defun get-current-proposal-index:integer ()
    (with-default-read proposal-index PROPOSAL
      {'index:0}
      {'index:= index}
      index
    )
  )

  (defun get-voting-count:integer (account:string)
    (with-default-read votecount-table account
      {'count:0}
      {'count:= count}
      count
    )
  )

  (defun get-proposal-ids:[integer] (account:string)
    (with-default-read votecount-table account
      {'proposalIds:[]}
      {'proposalIds:= ids}
      ids
      )
    )

  (defun get-voting-details-of-user(account:string)
    (select votecount-table
      (where "account" (=  account))
    )
  )

  ;-----------------------------------------------------------------------------
  ; Admin functions
  ;-----------------------------------------------------------------------------

  (defun get-unrevealed-tokens-for-collection:[object:{minted-token}] ()
    @doc "Returns a list of unrevealed tokens."
    (with-capability (IS_ADMIN)
      (select minted-tokens
        (and?
          (where "revealed" (= false))
          (where "collection-name" (= COLLECTION-NAME))
          )
       )
      )
  )

  (defun create-collection:string
    ( total-supply:integer
      guard:guard
      creator:string
      uri-list:[string]
      price:decimal
      policy:string )

      (with-capability (IS_ADMIN)
      (with-capability (CREATE-COLLECTION)
        (let ( (collection-id:string (n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection.create-collection-id COLLECTION-NAME guard  )) )

          (n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection.create-collection
            collection-id
            COLLECTION-NAME
            total-supply
            creator
            guard )

          (insert collections COLLECTION-NAME {
            "collection-id":collection-id,
            "collection-name":COLLECTION-NAME,
            "total-supply": total-supply,
            "guard": guard,
            "creator":creator,
            "currentIndex":1,
            "uri-list":uri-list,
            "price":price,
            "policy":policy})

          (add-tokens total-supply uri-list COLLECTION-NAME)
          (emit-event (COLLECTION_CREATED COLLECTION-NAME  total-supply  guard creator))
            collection-id
        )
      ))
    )

  (defun update-price:string (price:decimal)
    (enforce (< 0.0 price) "price is not a positive number")
    (with-capability (IS_ADMIN)
      (update collections COLLECTION-NAME {
        "price":price })
    )
  )

  (defun add-policies (policies:string)
    (with-capability (IS_ADMIN)
      (let ( (policy_stack:string (get-policies-of-collection)) )

        (update collections COLLECTION-NAME {
          "policy": (concat [policy_stack " " policies]) })
        )
    )
  )

  (defun replace-policies (policies:string)
    (with-capability (IS_ADMIN)
     (update collections COLLECTION-NAME {
       "policy": policies })
     )
  )

  (defun updatetokenlist:bool
    ( uri-list:[string]
      collection-name:string )

    (with-capability (IS_ADMIN)
      (let  ( (tkns:[integer](map (str-to-int 64) uri-list)) )

        (with-read token-record COLLECTION-NAME {
            "uri-list" := tkn-list }

        (let* ( (tkn:[integer](+  tkn-list tkns)) )

          (update token-record COLLECTION-NAME {
              "current-length": (+ (length uri-list) (get-current-length collection-name)),
              "uri-list": tkn } )

          (update collections collection-name {
                "total-supply": (length tkn) } )
           ))
        )
      )
  )

  ;-----------------------------------------------------------------------------
  ; Mint functions
  ;-----------------------------------------------------------------------------

  (defun reserve-token:bool
    ( account:string
      amount:integer )

    (with-capability (MINT-PASS account)
        (let (
                (currentIndex:integer (get-current-index))
                (total-supply:integer (get-total-supply ))
                (creator:string (get-collection-creator))
                (mint-price:decimal (get-mint-price))
                (acc-guard:guard (at "guard" (coin.details account)))
             )

        (enforce (<=  currentIndex total-supply) "We are unable to produce additional images as the current supply has been depleted.")
        (if (= account creator)
           ""
           (coin.transfer account creator (* mint-price amount)))

        (write accountsInfo account{
          "account":account,
          "tokens":[] })

        (write burnDetials account {
          'account:account,
          'guard:acc-guard
          })

         (mint-internal
           account
           acc-guard
           amount
          currentIndex
         )
        )
      )
    )

  (defun mint-internal:bool
    ( account:string
      guard:guard
      amount:integer
      currentIndex:integer)

      (require-capability (MINT-PASS account))

      (update collections COLLECTION-NAME {
         "currentIndex": (+ currentIndex amount) })

      (map
        (mint-token account guard amount)
        (map (+ currentIndex) (enumerate 0 (- amount 1)))
      )

      (emit-event (MINT_EVENT account amount))

  )

  (defun mint-token:string
    ( account:string
      guard:guard
      amount:integer
      accountId:integer )

      @doc "Mints a single token for the account."
      (require-capability (MINT-PASS account))
      (insert minted-tokens (get-mint-token-id COLLECTION-NAME accountId)
        { "collection-name": COLLECTION-NAME
        , "account": account
        , "guard": guard
        , "accountId": accountId
        , "marmToken": ""
        , "revealed": false
        }
      )
  )

  (defun bulk-sync-with-ng
    ( accountIds:[integer])
    (with-capability (MINTPROCESS)
      (map (sync-with-ng COLLECTION-NAME) accountIds)
    )
  )

  (defun sync-with-ng
    ( collection-name:string
      accountId:integer )

      (require-capability (MINTPROCESS))

      (with-read minted-tokens (get-mint-token-id COLLECTION-NAME accountId) {
         "account":= account
         ,"revealed":= revealed }

      (enforce (= revealed false) "Can't mint this token more than once")

      (let* (
              (collection-info:object{collection-schema} (get-collection-details))
              (creator:string (at 'creator collection-info))
              (collection-id:string (at 'collection-id collection-info))
              (guard:guard (at 'guard collection-info))
              (mintto:guard (at "guard" (coin.details account)))
            )

            (let*
              (
                (passHolders:[string] (get-priority-users))
                (account-minted:integer (get-account-minted account))
                (random:integer (get-random account))
                (current-length:integer (get-current-length))
                (index:integer (mod random current-length))
                (available-tokens:[string] (at 'uri-list (read token-record COLLECTION-NAME)))
                (minted-token:string (at index available-tokens))
                (token-id:string (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.create-token-id guard minted-token))
                (token-precision 0)
                (policy_stack:string (get-policies-of-collection ))
                (policy:[module{n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1}] (n_4e470a97222514a8662dd1219000a0431451b0ee.std-policies.to-policies policy_stack))

                )

                (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.create-token token-id token-precision minted-token policy guard)
                (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.mint token-id account mintto 1.0)
                (update minted-tokens (get-mint-token-id COLLECTION-NAME accountId) {"revealed": true, "marmToken": token-id})
                (update token-record COLLECTION-NAME {
                  'uri-list: (filter (!= minted-token) available-tokens),
                  'current-length: (- current-length 1)
                  })

                (mint-nft-info account token-id)
                (update-minted account account-minted)

                (with-read accountsInfo account{
                  "tokens":=tokens}

                  (update accountsInfo account{
                    "tokens":(+ tokens [token-id])})
                )
                (pass-details account passHolders)
              token-id
              )
      ))
  )

  ;-----------------------------------------------------------------------------
  ; Helper functions
  ;-----------------------------------------------------------------------------

  (defun pass-details
    ( account:string
      passHolders:[string] )

    (require-capability (PRIVATE))
      (cond
        ((contains account passHolders)
          [(let ( (bal:integer (get-pass-balance account)) )

              (update passbalance_table account {
                "balance": (+ bal 12) }))
           ]
        )

        [
          (insert priority-users account{"account":account})
          (write passbalance_table account { "balance": 12 })
        ]
    )
  )

  (defun update-minted:string
    ( account:string
      minted:integer )

      (require-capability (PRIVATE))
      (write account-details account {
        'account: account,
        'minted: (+ minted 1)})
  )

  (defun mint-nft-info:string
    ( account:string
      token-id:string )

      (require-capability(PRIVATE))

      (insert nfts-info-by-id token-id {
        "owner":account,
        "tokenId":token-id,
        "collectionName":COLLECTION-NAME })

      (write nfts-info-by-owner account {
        "owner":account,
        "tokenId":token-id,
        "collectionName":COLLECTION-NAME })

  )

  (defun add-tokens
    ( total-supply:integer
      uri-list:[string]
      collection-name:string )

      (require-capability (PRIVATE))
      (enforce (= (length uri-list) total-supply) "Total-supply and uri-list length does not match")

      (insert token-record COLLECTION-NAME {
        "current-length": (length uri-list),
        "uri-list": uri-list })
  )

  (defun enforce-owner:bool (account:string creatorguard:guard)
    (with-read burnDetials account
      { "guard":=guard }
      (enforce (= creatorguard guard) "You can't burn the pass balance")
    )
  )

  (defun burn:integer (account:string guard:guard)
   (enforce-owner account guard)
    (let ( (b:integer (get-pass-balance account)) )
      (update passbalance_table account {
        "balance": (- b 1 )})
    )
  )

  ;-----------------------------------------------------------------------------
  ; DAO functions
  ;-----------------------------------------------------------------------------

  (defun propsal-init()
    (with-capability (IS_ADMIN)
      (insert proposal-index PROPOSAL {
        "index":0 })
    )
  )

  (defun create-proposal
    ( start-time:time
      end-time:time
      title:string )

      (let* (
              (propsal-indx:integer (get-current-proposal-index))
              (currentproposal:integer (+ propsal-indx 1))
              (proposalId:string (int-to-str 16 currentproposal))
           )

        (insert proposal proposalId {
          "start-time":start-time,
          "end-time":end-time,
          "title":title,
          "proposal-id":currentproposal })

        (update proposal-index PROPOSAL {
          "index":currentproposal })

        (insert proposal-details proposalId {
          "title":title,
          "id":currentproposal })

        (insert votes proposalId {
          "yes":0,
          "no":0 })

        (format "Your Proposal ID is {}" [currentproposal])
      )
  )

  (defun update-proposal-time
    ( start-time:time
    end-time:time
    proposalId:string)

    (update proposal proposalId {
      "start-time":start-time,
      "end-time":end-time})
  )

  (defun check-propsal-deadline:bool (proposalId:string)
    (let (
          (start-time:time (at 'start-time (read proposal proposalId)))
          (end-time:time (at 'end-time (read proposal proposalId)))
          (chain-time (at 'block-time (chain-data)))
          )
        (and (<= chain-time end-time) (>= chain-time start-time))
    )
  )

  (defun cast-vote:string (account:string vote:bool proposalId:integer)
    (let* (
            (priorityUsers:[string] (get-priority-users))
            (contain:bool (contains account priorityUsers))
            (id:string (int-to-str 16 proposalId))
            (count:integer (get-voting-count account))
            (ids:[integer] (get-proposal-ids account))
            (deadline:bool (check-propsal-deadline id))
         )

         (enforce (= deadline true) "Check proposal deadlines!")
         (enforce (= contain true) "Sorry, your voting rights are on vacation!")

         (insert voting-history id {
           "account":account,
           "proposalId":proposalId })

        (write votecount-table account {
          "account":account,
          "proposalIds":(+ ids [proposalId]),
          "count":(+ count 1) })

         (if (= vote true)
              [
                (update votes id {
                  "yes":(+ (at 'yes (get-propsal-voting-history id)) 1) })
              ]

              [
                (update votes id {
                  "no":(+ (at 'no (get-propsal-voting-history id)) 1) })
              ]
         )
      )
    "Vote successfully submitted!"
  )

)


