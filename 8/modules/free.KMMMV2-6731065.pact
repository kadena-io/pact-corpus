(module KMMMV2 GOVERNANCE

  (use n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1 [account-details sender-balance-change receiver-balance-change])
  (use free.util-strings [to-string starts-with split])
  (use free.util-lists [first])
  (use free.util-time [time-between now from-now])
  (use kip.token-manifest)
  (use free.util-fungible [enforce-precision enforce-reserved enforce-valid-account enforce-valid-transfer enforce-valid-amount])
  (use util.guards)
  (use n_4e470a97222514a8662dd1219000a0431451b0ee.ledger)
  (use n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection)

  ; (use coin)

  ;-----------------------------------------------------------------------------
  ; Constants
  ;-----------------------------------------------------------------------------

  (defconst KRYPTOMERCH_BANK "Kryptomerch-Bank")
  ; (defconst POLICY:module{n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1} free.policy-lp)
  (defconst ADMIN:string "k:6d3408f25312d5342ea5513707cc18dfd7b2a2c30decd0250d6f6491bbbfcf32")
  (defconst FEE:string "fee")
  (defconst REQUEST:string "request")

  ;-----------------------------------------------------------------------------
  ; Capabilities
  ;-----------------------------------------------------------------------------

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.KMMMV2"))
    (compose-capability (PRIVATE))
  )

  (defcap PRIVATE () true)

  (defcap IS_ADMIN ()
       (compose-capability (GOVERNANCE)))

  (defcap OWNER (account:string)
    @doc "Verifies account meets format and belongs to caller"
     (enforce-guard (at "guard" (coin.details account)))
  )

  (defcap CREDIT-NFT (receiver:string) true)

  (defcap MINT-NFT (account:string)
     @managed ;; one-shot for a given amount
     (compose-capability (PRIVATE))
     (compose-capability (CREDIT-NFT account))
     (compose-capability (UPDATE_SUPPLY))
   )

  (defcap CREATE-COLLECTION (collection-name:string creator:string )
     @event true)

  (defcap UPDATE_SUPPLY ()
   "private cap for update-supply"
   true)


  (defun enforce-owner:bool (collection-name:string creatorguard:guard)
    (with-read collections collection-name
      { "creator-guard":=guard }
      (enforce (= creatorguard guard) "You are not the owner of the collection")
      )
  )

  (defcap MINT_EVENT ( collection-name:string account:string amount:integer )
    @event true )

  (defcap MINTPROCESS:bool (collection-name:string)
    @doc "Used to manage who can mint an NG NFT after paying"
    (compose-capability (PRIVATE))
    (enforce-guard (at 'creator-guard (read collections collection-name ['creator-guard ])))
    "Must be the collection creator guard"
  )

  (defcap AIRDROP:bool (collection-name:string)
    @doc "Used to manage who can airdrop an NG NFT"
    (compose-capability (PRIVATE))
    (enforce-guard (at 'creator-guard (read collections collection-name ['creator-guard ])))
    "Must be the collection creator guard"
  )

  (defcap REFUND () "Internal cap" true)
(defcap TRANSFER () "Internal cap" true)
(defcap INTERNAL_CLOSE_SALE () "Internal cap" true)

(defcap OPEN_SALE (token_id:string  sale_id:string sale_type:string owner:string buy_now_price:decimal sale_start_days:integer sale_end_days:integer ) @event true)
(defcap CLOSE_SALE (token_id:string id:string seller:string) @event true)
(defcap CANCEL_BID (token_id:string id:string account:string seller:string amount:decimal) @event true)
(defcap DECLINE_BID (token_id:string id:string account:string seller:string) @event true)
(defcap BID (token_id:string id:string buyer:string seller:string amount:decimal bid_expiration:time)  @event true)
(defcap OUTBID (token_id:string id:string account:string last_account:string amount:decimal) @event true)
(defcap BUY (token_id:string id:string buyer:string seller:string amount:decimal) @event true)
(defcap ACCEPT_BID (token_id:string id:string buyer:string seller:string amount:decimal) @event true)
(defcap SALE_EXTENDED (token_id:string id:string buyer:string seller:string) @event true)

  ;-----------------------------------------------------------------------------
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------

  (defschema collection-details
    collection-name:string
    symbol:string
    collection-id:string
    creator:string
    creator-guard:guard
    description:string
    category:string
    total-supply:integer
    urisIPFS:string
    mint-price:decimal
    royalty-rate:decimal
    royalty-account:string
    cover-image-url:string
    banner-image-url:string
    mint-start-date:string
    mint-start-time:time
    mint-end-date:string
    mint-end-time:time
    enable-airdrop:bool
    airdrop-supply:integer
    airdrop-addresses:[string]
    current-index:integer
    policy:string
  )

  (defschema id
    name:string
    collection-id:string)

  (defschema token-record-schema
    uri-list:[string]
    current-length:integer)

  (defschema nft-info
    owner:string
    token-id:string
    collection-name:string
    collection-id:string)

  (defschema account-schema
     account:string
     minted:integer)

  (defschema reservation
     account:string
     guard:guard
     amount-nft:integer)

  (defschema minted-token
    @doc "Stores the data for a minted token. \
    \ The id is the collection, tierId, account, and token-id."
    collection-name:string
    account:string
    guard:guard
    accountId:integer
    marmToken:string
    revealed:bool
  )

  (defschema token-ledger-schema
    current-length:integer)


    (defschema free-mint-schema
        total-supply:integer
        current-index:integer
    )

    (defschema free-mint-account-schema
        account:string
        collection-name:string
        claimed:bool
    )

  (defschema account-token-info
      account:string
      collection-name:string
      tokens:[string])

  (defschema counts-schema
    @doc "Keeps track of key counts."
    count:integer)

  (defschema airdrop-history-schema
      @doc "Stores airdrop history"
      collection-name:string
      account:string
      token-id:string
      airdrop-time:time)

  (defschema fee-schema
    marketplace-fee:decimal
    verification-fee:decimal
    mint-fee:decimal
  )


  (defschema royalty-schema
    collection-name:string
    royaltyRate:decimal
    royaltyAccount:string
  )

  (defschema sale-schema
    collection-name:string
    token_id:string
    sale_id:string
    royaltyRate:decimal
    royaltyAccount:string
    sale_type:string
    owner:string
    closed:bool
    can_buy_now:bool
    buy_now_price:decimal
    sale_start_date:time
    sale_end_date:time
    last_offer:decimal
    last_offer_account:string
    offer_end_date:time
)

(defschema verification-schema
  account:string
  approved:bool)


(defschema saleInfo
  token_id:string
  sale_type:string)

  (defschema request-schema
    request:bool
    approved:bool)

  (defschema request
      account:string
      request:bool
      approved:bool)

  (deftable fee-ledger:{fee-schema})
  (deftable collections:{collection-details})
  (deftable request-collection-ledger:{collection-details})
  (deftable nfts-info-by-id:{nft-info})
  (deftable nfts-info-by-owner:{nft-info})
  (deftable minted-tokens:{minted-token})
  (deftable token-record:{token-record-schema})
  (deftable id-details:{id})
  (deftable name-details:{id})
  (deftable reservations:{reservation})
  (deftable account-details:{account-schema})
  (deftable accountsInfo:{account-token-info})
  (deftable counts-table:{counts-schema})
  (deftable airdrop-history-table:{airdrop-history-schema})
  (deftable royalty:{royalty-schema})
  (deftable sale-ledger:{sale-schema})
  (deftable verification-ledger:{verification-schema})
  (deftable fixed-sale-ledger:{saleInfo})
  (deftable auction-sale-ledger:{saleInfo})
  (deftable verification-request:{request-schema})
  (deftable verification-request-for-all:{request})
  (deftable free-mint-ledger:{free-mint-schema})
  (deftable free-mint-account-ledger:{free-mint-account-schema})
  (deftable token-ledger:{token-ledger-schema})

  ;-----------------------------------------------------------------------------
  ; Get functions
  ;-----------------------------------------------------------------------------

  (defun get-launched-collection()
    (keys collections))

  (defun get-collection-creator-of-request (collection-name:string)
    (at 'creator (read request-collection-ledger collection-name)))

  (defun get-collection-id (collection-name:string)
    (at 'collection-id (read collections collection-name)))

  (defun get-collection-symbol (collection-name:string)
    (at 'symbol (read collections collection-name)))

  (defun get-collection-details ( collection-name:string )
    (read collections collection-name))

  (defun get-collection-creator (collection-name:string)
    (at 'creator (read collections collection-name)))

  (defun get-collection-creator-guard (collection-name:string)
    (at 'creator-guard (read collections collection-name)))

  (defun get-mint-start-time (collection-name:string)
    (at 'mint-start-time (read collections collection-name)))

  (defun get-mint-end-time (collection-name:string)
    (at 'mint-end-time (read collections collection-name)))

  (defun get-mint-price (collection-name:string)
      (at 'mint-price (read collections collection-name)))

  (defun get-airdrop-accounts ( collection-name:string )
    (at 'airdrop-addresses (read collections collection-name)))

  (defun get-category ( collection-name:string )
    (at 'category (read collections collection-name)))

  (defun get-description ( collection-name:string )
    (at 'description (read collections collection-name)))

  (defun get-cover-url ( collection-name:string )
    (at 'cover-image-url (read collections collection-name)))

  (defun get-policy-of-collection ( collection-name:string )
    (at 'policy (read collections collection-name)))

  (defun get-banner-url ( collection-name:string )
    (at 'banner-image-url (read collections collection-name)))

  (defun get-collection-request()
    (keys request-collection-ledger))

  (defun get-airdrop-enabled:bool (collection-name:string)
    (at 'enable-airdrop (read collections collection-name)))

  (defun get-free-mint-enabled:bool (collection-name:string)
    (at 'allow-free-mints (read collections collection-name)))

  (defun get-total-supply:integer (collection-name:string)
     (at 'total-supply (get-collection-details collection-name)))

  (defun get-current-index:integer (collection-name:string)
    (at 'current-index (read collections collection-name)))

    (defun get-current-index-free-mint:integer (collection-name:string)
      (at 'current-index (read free-mint-ledger collection-name)))


  (defun get-royalty-info (collection-name:string type:string)
    (cond
      ((= type "rate")
        (at 'royalty-rate (read collections collection-name))
      )
      ((= type "account")
        (at 'royalty-account (read collections collection-name))
      )
      ((= type "both")

        (format "Royalty Rate {} and Royalty Account {}" [(at 'royalty-rate (read collections collection-name)) (at 'royalty-account (read collections collection-name))])
        (at 'royalty-account (read collections collection-name))
      )
      ["Invalid Type"]
    )
  )

  (defun get-royalty-info-single-nft (nft-name:string type:string)
    (cond
      ((= type "rate")
        (at 'royalty-rate (read single-nft nft-name))
      )
      ((= type "account")
        (at 'royalty-account (read single-nft nft-name))
      )
      ((= type "both")

        (format "Royalty Rate {} and Royalty Account {}" [(at 'royalty-rate (read single-nft nft-name)) (at 'royalty-account (read single-nft nft-name))])
        (at 'royalty-account (read single-nft nft-name))
      )
      ["Invalid Type"]
    )
  )

  (defun get-free-mint-claim:bool (collection-name:string account:string )
    (with-default-read free-mint-account-ledger (concat [collection-name "|" account])
      { 'claimed: false }
      { 'claimed:= claimed }
      claimed
    )
 )

  (defun get-all-tokens-by-account(account:string)
    (select accountsInfo
        (where "account" (= account))
    )
  )

  (defun get-nft-reserved:integer (collection-name:string account:string )
      (with-default-read reservations (concat [collection-name "|" account])
        { 'amount-nft: 0 }
        { 'amount-nft:= amount }
        amount
      )
  )

  (defun get-minted:integer (collection-name:string)
    (with-read token-record collection-name
      { 'current-length:= current-length }
      (- (at 'total-supply (get-collection-details collection-name)) current-length)
    )
  )

  (defun get-random:integer (account:string)
    (require-capability (PRIVATE))
      (let* ( (prev-block-hash (at "prev-block-hash" (chain-data)))
              (random (str-to-int 64 (hash (+ prev-block-hash (take 20 account)))))
            )
        random
      )
  )

  (defun get-current-length:integer (collection-name:string)
    (with-read token-record collection-name
        { 'current-length:= current-length }
      current-length
    )
  )

  (defun get-account-minted:integer (account:string)
    (with-default-read account-details account
      {"minted": 0}
      {"minted":= minted}
      minted
    )
  )

  (defun get-collection-name (token_id:string)
    (let* (
            (strinList(split ":" token_id ))
            (colName (first strinList))
          )
          colName
    )
  )

  (defun get-urisIPFS:integer (collection-name:string)
      (at 'urisIPFS (read collections collection-name)))


;;-----------------------------------------------------------------------------
; Admin Functions
;;-----------------------------------------------------------------------------

  (defun get-fee (type:string)
    (cond
      ((= type "marketplace")
        (at 'marketplace-fee (read fee-ledger FEE))
      )
      ((= type "verification")
        (at 'verification-fee (read fee-ledger FEE))
      )
      ((= type "mint")
        (at 'mint-fee (read fee-ledger FEE))
      )
   ["Invalid type"]
   )
  )

  (defun update-fee (fee:decimal type:string)
    @doc   "Update mint price"
    (enforce (< 0.0 fee) "fee is not a positive number")
    (with-capability (IS_ADMIN)
    (with-read fee-ledger FEE
      {
        "marketplace-fee":=marketplace-fee,
        "verification-fee":=verification-fee,
        "mint-fee":=mint-fee
      }
      (cond
        ((= type "marketplace")
          [ (update fee-ledger FEE
             {"marketplace-fee":fee}
            )
            (format "Updated Marketplace Serive Fee from {} to {}" [marketplace-fee fee])]
        )
        ((= type "verification")
          [ (update fee-ledger FEE
             {"verification-fee":fee})
            (format "Updated Verification Fee from {} to {}" [verification-fee fee])]
         )

         ((= type "mint")
           [ (update fee-ledger FEE
              {"mint-fee":fee})
             (format "Updated mint-fee Fee from {} to {}" [mint-fee fee])]
          )
        ["Conditions not met"]
      )
    )
    )
  )

  ;;-----------------------------------------------------------------------------
  ; Collection Functions
  ;;-----------------------------------------------------------------------------

  (defun launch-collection:string
    ( collection-name:string
      symbol:string
      creator:string
      creator-guard:guard
      description:string
      category:string
      total-supply:integer
      urisIPFS:string
      mint-price:decimal
      royalty-rate:decimal
      royalty-account:string
      cover-image-url:string
      banner-image-url:string
      mint-start-date:string
      mint-start-time:time
      mint-end-date:string
      mint-end-time:time
      enable-airdrop:bool
      policies:string)
      (let* ( (g:guard (at 'guard (coin.details creator)))
              (collection-id:string (n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection.create-collection-id collection-name creator-guard))
           )
      (enforce-reserved creator g)
      (insert collections collection-name
        { 'collection-name: collection-name,
          'symbol:symbol,
          'collection-id:collection-id,
          'creator:creator,
          'creator-guard:creator-guard,
          'description: description,
          'category: category,
          'total-supply: total-supply,
          'urisIPFS:urisIPFS,
          'mint-price: mint-price,
          'royalty-rate: royalty-rate,
          'royalty-account:royalty-account,
          'cover-image-url: cover-image-url,
          'banner-image-url: banner-image-url,
          'mint-start-date:mint-start-date,
          'mint-start-time: mint-start-time,
          'mint-end-date:mint-end-date,
          'mint-end-time: mint-end-time,
          'enable-airdrop: enable-airdrop,
          'airdrop-supply: 0,
          'airdrop-addresses: [],
          'current-index:1,
          'policy:policies })
          (emit-event (CREATE-COLLECTION collection-name creator))
          (n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection.create-collection
            collection-id
            collection-name
            total-supply
            creator
            creator-guard )

          (insert counts-table collection-name { "count": 0 })

         )
      )

  (defun updateCollectionDetails
    ( creator:string
      creatorguard:guard
      collection-name:string
      description:string
      category:string
      cover-image-url:string
      banner-image-url:string
    )
    (with-read collections collection-name {
        "creator-guard":=guard
          }
          (enforce (= creatorguard guard) "You are not the owner of the collection")
          (update collections collection-name {
              "description":description,
              "category":category,
              "cover-image-url":cover-image-url,
              "banner-image-url":banner-image-url
            })
    )
  )

  ;-----------------------------------------------------------------------------
  ; Airdrop Functions
  ;-----------------------------------------------------------------------------

    (defun create-airdrop (collection-name:string  creatorguard:guard airdrop-addresses:[string])
      (with-capability (PRIVATE)
          (enforce-owner collection-name creatorguard)
          (let (
                (enabled:bool (get-airdrop-enabled collection-name))
                )

          (enforce (= enabled true) "Airdrop is disabled")
          (add-to-airdrop collection-name airdrop-addresses)
          (map (add-tokens-for-airdrop collection-name) airdrop-addresses)
          )
      )
    )

    (defun add-tokens-for-airdrop(collection-name:string account:string)
      (require-capability (PRIVATE))
      (write accountsInfo (concat [collection-name "|" account]){
        "account":account,
        "collection-name":collection-name,
        "tokens":[] })
    )

    (defun add-to-airdrop ( collection-name:string  addresses:[string] )
      (require-capability (PRIVATE))
      (update collections collection-name
        { "airdrop-addresses": (+ (get-airdrop-accounts collection-name) addresses) })
    )

    (defun remove-airdrop-addresses ( collection-name:string creatorguard:guard addresses:[string] )
      (with-capability (PRIVATE)
        (enforce-owner collection-name creatorguard)
        (map (remove-airdrop-address collection-name) addresses)
      )
    )

    (defun remove-airdrop-address ( collection-name:string  address:string )
      (require-capability (PRIVATE))
      (let* (
            (airdrop-addresses:[string] (get-airdrop-accounts collection-name))
            (contain:bool (contains address airdrop-addresses))
            )
            (if (= contain true)
              [(update collections collection-name
                { "airdrop-addresses": (filter (!= address) airdrop-addresses) })]
              "Account Doesnot Exist!"
            )
      )
    )

    (defun gift-nft (token_id:string owner:string account:string)
             ; (with-capability (TRANSFER)
                 (install-capability (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.TRANSFER token_id owner account 1.0))
                 (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.transfer-create token_id owner account (at 'guard (coin.details account)) 1.0)
    )

    (defun bulk-airdrop (collection-name:string data:list creator:string creator-guard:guard)
      (enforce-owner collection-name creator-guard)
      (with-capability (PRIVATE)
        ; (with-capability (AIRDROP collection-name)
          (map (airdrop-nft) data)
        ; )
      )
    )

    (defun airdrop-nft:string(data:object)
      (enforce (!= (at 'account data) "") "Account field can not be empty.")
      (require-capability (PRIVATE))
      (let* (
              (collection-name:string (at 'collection-name data))
              (airdrop-addresses:[string] (get-airdrop-accounts collection-name))
              (token_id:string (at 'token-id data))
              (creator:string (get-collection-creator collection-name))
              (account:string (at 'account data))
            )
            ; (format "Tokens" [token_id])
            (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.transfer-create token_id creator account (at 'guard (coin.details account)) 1.0)
            (increase-count collection-name)
            (insert airdrop-history-table (int-to-str 10 (+ 1  (get-count collection-name))) {
              'collection-name: collection-name
              ,'account: account
              ,'token-id: token_id
              ,'airdrop-time: (at "block-time" (chain-data))
              })


              (with-read accountsInfo (concat [collection-name "|" account]) { "tokens":=tokens }
                (let ((colToken:string (concat [collection-name "|" token_id])))

                      (update accountsInfo (concat [collection-name "|" account]) { "tokens":(+ tokens [colToken]) })
                )
              )
            ; account
      )
    )

    (defun increase-count (collection-name:string)
      @doc "Increase the count of a key in a table by 1"
      (require-capability (PRIVATE))
      (update counts-table collection-name {"count": (+ 1 (get-count collection-name))})
    )

    (defun get-count:integer (collection-name:string)
      @doc "Gets the count for a key"
      (at "count" (read counts-table collection-name ['count]))
    )

    (defun get-airdrop-history()
      @doc "Returns all the airdrop history."
      (select airdrop-history-table (where "account" (!= "")))
    )


  ;;-----------------------------------------------------------------------------
  ; Policies Functions
  ;;-----------------------------------------------------------------------------
  (defun get-policies-of-collection (collection-name:string)
    (with-read collections collection-name{
      "policy":=policy
      }
      policy
    )
  )

  (defun add-policies (collection-name:string creatorguard:guard policies:string)
    (enforce-owner collection-name creatorguard)
    (let (
          (policy_stack:string (get-policies-of-collection collection-name))
          )
        (update collections collection-name{
          "policy": (concat [policy_stack " " policies])
          })
    )
  )


  ;-----------------------------------------------------------------------------
  ; Mint functions
  ;-----------------------------------------------------------------------------

  (defun check-public:bool (collection-name:string)
    (let* (
          (mint-start-time:time (get-mint-start-time collection-name))
          (mint-end-time:time (get-mint-end-time collection-name))
          (total-supply:integer (get-total-supply collection-name))
          (chain-time (at 'block-time (chain-data)))
        )
      (and (<= chain-time mint-end-time) (>= chain-time mint-start-time))
    )
  )

  (defun bulk-sync-with-ng
    (collection-name:string
     accountIds:[integer]
     uris:[string])
    (enforce (= (length accountIds) (length uris))
      "Account IDs and URIs length mismatch")
    ;; Ensure the lists are not empty
    (enforce (> (length accountIds) 0) "Account IDs cannot be empty")
    (enforce (> (length uris) 0) "URIs cannot be empty")

    ;; Call a helper function that maps accountIds with URIs
    (with-capability (MINTPROCESS collection-name)
      (process-sync collection-name accountIds uris)
    )
  )

  (defun process-sync
    (collection-name:string
     accountIds:[integer]
     uris:[string])
     ;; Here, instead of zip, we manually process the lists
    (map
      (lambda (idx)
        (let ((accountId (at idx accountIds))
              (uri (at idx uris)))
          ;; Call sync logic here, passing the accountId and uri
          (sync-with-ng collection-name accountId uri)
        )
      )
      (enumerate 0 (- (length accountIds) 1)) ;; Fix: Correct index generation
    )
  )

  (defun sync-with-ng:string
    (
      collection-name:string
      accountId:integer
      uri:string
    )
      (require-capability (MINTPROCESS collection-name))
      (with-read minted-tokens (get-mint-token-id collection-name accountId)
        { "account":= account
          ,"revealed":= revealed
        }
        (enforce (= revealed false)
          "Can't mint this token more than once"
        )

      (let* (
             (collection-info:object{collection-details} (get-collection-details collection-name))
             (creator:string (at 'creator collection-info))
             (collection-id:string (at 'collection-id collection-info))
             (guard:guard (at 'creator-guard collection-info))
             (mintto:guard (at "guard" (coin.details account)))
           )
          (let*
                (
                  (random:integer (get-random account))
                  (account-minted:integer (get-account-minted account))
                  (token-id:string (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.create-token-id guard uri))
                  (token-precision 0)
                  (policy_stack:string (get-policy-of-collection collection-name))
                  (policy:[module{n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1}] (n_4e470a97222514a8662dd1219000a0431451b0ee.std-policies.to-policies policy_stack))
                )

                (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.create-token token-id token-precision uri policy guard)
                (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.mint token-id account mintto 1.0)

                (update minted-tokens (get-mint-token-id collection-name accountId) {"revealed": true, "marmToken": token-id})
                (update-minted account account-minted)

                (with-read accountsInfo (concat [collection-name "|" account]){
                  "tokens":=tokens}
                  (let (
                      (colToken:string (concat [collection-name "|" token-id]))
                    )
                  (update accountsInfo (concat [collection-name "|" account]){
                    "tokens":(+ tokens [colToken])}
                  )
                )
                )
                token-id

              )
            )
            )
  )

  (defun update-minted:string (account:string minted:integer)
    (require-capability (PRIVATE))
      (write account-details account {
      'account: account,
      'minted: (+ minted 1)})
  )

    (defun get-unrevealed-tokens-for-collection:[object:{minted-token}]
      (
        collection-name:string
        creator-guard:guard
      )
      @doc "Returns a list of unrevealed tokens."
      (enforce-owner collection-name creator-guard)
      (select minted-tokens
        (and?
          (where "revealed" (= false))
          (where "collection-name" (= collection-name))
        )
      )
    )

  (defun updateMintingDetails
    ( creator:string
      creatorguard:guard
      collection-name:string
      mint-start-date:string
      mint-start-time:time
      mint-end-date:string
      mint-end-time:time
      allow-free-mints:bool
    )
    (with-read collections collection-name {
      "creator-guard":=guard
    }
    (enforce (= creatorguard guard) "You are not the owner of the collection")
    (update collections collection-name {
      "mint-start-date":mint-start-date,
      "mint-start-time":mint-start-time,
      "mint-end-date":mint-end-date,
      "mint-end-time":mint-end-time,
      "allow-free-mint":allow-free-mints
      })
      )
    )

    (defun reserve-token:bool
      ( collection-name:string
        account:string
        amount:integer
      )
      (let (
              (indx:integer (get-current-index collection-name))
              (total-supply:integer (get-total-supply collection-name))
           )

           (enforce (<=  indx total-supply) "We are unable to produce additional images as the current supply has been depleted.")

        (with-capability (MINT-NFT account )
          (let* (
                  (current-indx:integer  (get-current-index collection-name))
                  (acc-guard:guard (at "guard" (coin.details account)))
                  (mintFee:decimal (get-fee "mint"))
                  (mint-price (get-mint-price collection-name))
              )

              (cond
                  (
                    (= account (get-collection-creator collection-name))

                    [
                      (coin.transfer account ADMIN (* mintFee (* mint-price amount)))
                      ; true
                    ]
                  )

                [(mint-for-price account collection-name amount)]
              )

          (write accountsInfo (concat [collection-name "|" account]){
            "account":account,
            "collection-name":collection-name,
            "tokens":[] })

          (mint-internal
            collection-name
            account
            acc-guard
            amount
            current-indx
          )
            )
          )

          )
        )

        (defun mint-internal:bool
          (
            collection-name:string
            account:string
            guard:guard
            amount:integer
            current-indx:integer
          )
          (require-capability (MINT-NFT account ))

          (update collections collection-name
            { "current-index": (+ current-indx amount) }
          )

          (map
            (mint-token collection-name account guard)
            (map (+ current-indx) (enumerate 0 (- amount 1)))
          )
          (emit-event (MINT_EVENT collection-name account amount))
        )

        (defun mint-token:string
          (
            collection-name:string
            account:string
            guard:guard
            accountId:integer
          )
          @doc "Mints a single token for the account."
          (require-capability (MINT-NFT account ))
          (insert minted-tokens (get-mint-token-id collection-name accountId)
            { "collection-name": collection-name
            , "account": account
            , "guard": guard
            , "accountId": accountId
            , "marmToken": ""
            , "revealed": false
            }
          )
        )

        ; Used to return the concatenated collection ID string value
        (defun get-mint-token-id:string
          (
            collection:string
            accountId:integer
          )
          (concat [collection "|" (int-to-str 10 accountId)])
        )

        (defun mint-for-price
          (
            account:string
            collection-name:string
            amount:integer
          )
                (let (
                    (creator:string (get-collection-creator collection-name))
                    (mintFee:decimal (get-fee "mint"))
                    (mint-price (get-mint-price collection-name))
                  )

                ; [(check-public collection-name)(enforce-public-mint collection-name) (coin.transfer account creator (at 'mint-price collection-info))]
                  (check-public collection-name)
                  (coin.transfer account ADMIN (* mintFee (* mint-price amount)))
                  (coin.transfer account creator (* amount mint-price))
                )
        )

        (defun update-public-price (collection-name:string creatorGuard:guard price:decimal)
          (enforce-owner collection-name creatorGuard)
          (update collections collection-name {
            "mint-price":price
            })
        )


        (defun update-mint-time (collection-name:string creator-guard:guard mint-start-time:time mint-end-time:time )
          (enforce-owner collection-name creator-guard)
          (update collections collection-name{
            "mint-start-time":mint-start-time,
            "mint-end-time":mint-end-time
            })
        )

  (defun nft-bank-guard () (create-module-guard "kmbank"))



;-----------------------------------------------------------------------------
; Marketplace Functions
;-----------------------------------------------------------------------------

(defun update-royalty-info (collection-name:string creatorGuard:guard name:string royaltyRate:decimal royaltyAccount:string)
  (enforce-owner collection-name creatorGuard)
    (update collections collection-name{
      "royalty-rate":royaltyRate,
      "royalty-account":royaltyAccount})
)


  (defun get-token-details (account:string)
    @doc "Get token details for an account"
    (let*
      ((balances (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.list-balances account))
      (token-ids (map (lambda (balance) (at 'id balance)) balances))
      (details (map (lambda (token-id)
      { "token-id": token-id
      , "uri": (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.get-uri token-id)
      })
      token-ids)))
    details))

  (defun init
    (
      marketplace-fee:decimal
      verification-fee:decimal
      mint-fee:decimal
    )
    ; (coin.create-account ADMIN (nft-bank-guard))
    "empty lock succeeded"
    (insert fee-ledger FEE {
        "marketplace-fee":marketplace-fee,
        "verification-fee":verification-fee,
        "mint-fee":mint-fee
      })
  )

  ;-----------------------------------------------------------------------------
  ; Single NFT
  ;-----------------------------------------------------------------------------
  (defschema single-nft-info
    creator:string
    creator-guard:guard
    nft-name:string
    uri:string
    policies:string
    mint-price:decimal
    minted:bool
    royalty-rate:decimal
    royalty-account:string
  )

  (defschema single-nft-schema
      minter:string
  )

  (deftable single-nft:{single-nft-info})
  (deftable single-nft-details:{single-nft-schema})

  (defun launch-single-nft:string
    ( creator:string
      creator-guard:guard
      nft-name:string
      uri:string
      policies:string
      mint-price:decimal
      royalty-rate:decimal
      royalty-account:string)

    (let* ( (g:guard (at 'guard (coin.details creator))) )

    (enforce-reserved creator g)
    (insert single-nft nft-name {
      'creator:creator,
      'creator-guard:creator-guard,
      'nft-name:nft-name,
      'uri:uri,
      'policies:policies,
      'mint-price:mint-price,
      'minted:false,
      'royalty-rate:royalty-rate,
      'royalty-account:royalty-account
      })

    (insert single-nft-details nft-name {
      'minter:""
      })
    )

  )

  (defun get-single-nft-uri (nft-name:string)
    (at 'uri (read single-nft nft-name)))

  (defun get-single-nft-policies (nft-name:string)
    (at 'policies (read single-nft nft-name)))

  (defun get-single-nft-mint-price (nft-name:string)
    (at 'mint-price (read single-nft nft-name)))

  (defun get-single-nft-creator (nft-name:string)
    (at 'creator (read single-nft nft-name)))

  (defun get-single-mint-status (nft-name:string)
      (at 'minted (read single-nft nft-name)))

  (defun get-single-nft-minter (nft-name:string)
      (at 'minter (read single-nft-details nft-name)))

  (defun reserve-single-nft
    (
      minter:string
      minter-guard:guard
      nft-name:string
    )
    (let (
            (mintFee:decimal (get-fee "mint"))
            (mint-price (get-single-nft-mint-price nft-name))
            (creator (get-single-nft-creator nft-name))
            (minted (get-single-mint-status nft-name))
         )

         (enforce (= minted false) "Nft Already Minted!")

         (if (= creator minter)

         (coin.transfer minter ADMIN (* mintFee mint-price))
         [
          (coin.transfer minter ADMIN (* mintFee mint-price))
          (coin.transfer minter creator mint-price)
         ]
        )
      (update single-nft nft-name{
        "minted":true
        })
      (update single-nft-details nft-name{
          'minter:minter
          })
    )
  )

  (defun mint-single-nft:string
    (
      minter:string
      minter-guard:guard
      nft-name:string
    )
    (let* (
            (pastMinter:string (get-single-nft-minter nft-name))
            (token-uri:string (get-single-nft-uri nft-name))
            (policies:string (get-single-nft-policies nft-name))
            (token-precision 0)
            (token-id:string (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.create-token-id minter-guard token-uri))
            (policy:[module{n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1}] (n_4e470a97222514a8662dd1219000a0431451b0ee.std-policies.to-policies policies))
         )

         (enforce (= minter pastMinter) "You didnt registerd the nft")

         ;; Create the token (NG)
         (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.create-token token-id token-precision token-uri policy minter-guard)
         ;; Update minted tokens ledger
         (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.mint token-id minter minter-guard 1.0)

    token-id
    )
  )



)

; (create-table request-collection-ledger)
; (create-table collections)
; (create-table id-details)
; (create-table name-details)
; (create-table token-record)
; (create-table token-ledger)
; (create-table reservations)
;
; (create-table nfts-info-by-id)
; (create-table nfts-info-by-owner)
; (create-table account-details)
; (create-table minted-tokens)
; (create-table accountsInfo)
; (create-table counts-table)
; (create-table airdrop-history-table)
; (create-table fee-ledger)
;
; (create-table sale-ledger)
; (create-table royalty)
; (create-table verification-ledger)
; (create-table verification-request-for-all)
; (create-table verification-request)
; (create-table auction-sale-ledger)
; (create-table fixed-sale-ledger)
;
; (create-table free-mint-ledger)
; (create-table free-mint-account-ledger)
;
; (create-table single-nft)
;
; (create-table single-nft-details)
;
;
;
; (free.KMMMV2.init 0.01 0.01 0.015)

