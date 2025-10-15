(module KMLPV2 GOVERNANCE

  (use n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1 [account-details sender-balance-change receiver-balance-change])
  (use free.util-strings [to-string starts-with])
  (use free.util-time [time-between now from-now])
  (use kip.token-manifest)
  (use free.util-fungible [enforce-precision enforce-reserved enforce-valid-account enforce-valid-transfer enforce-valid-amount])
  (use util.guards)
  (use n_4e470a97222514a8662dd1219000a0431451b0ee.ledger)
  (use n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection)
  (use free.KMPPV2 [get-priority-users burn get-pass-balance])

  ;-----------------------------------------------------------------------------
  ; Constants
  ;-----------------------------------------------------------------------------

  (defconst name:string "n_4e470a97222514a8662dd1219000a0431451b0ee")
  (defconst PRIORITY-CONST:string "PRIORITY")
  (defconst KRYPTOMERCH_BANK "Kryptomerch-Bank")
  (defconst LAUNCHPAD_ACC:string "k:6d3408f25312d5342ea5513707cc18dfd7b2a2c30decd0250d6f6491bbbfcf32")
  (defconst LAUNCH:string "launch")
  (defconst PRIME:string "Prime")
  (defconst DISCOUNT:string "Discount")
  (defconst FEE:string "fee")
  ;-----------------------------------------------------------------------------
  ; Capabilities
  ;-----------------------------------------------------------------------------

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.KMLPV2-KS"))
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

  (defcap RESERVE
     (collection-name:string)
     "Reserve event for NFT reservation"
     @event
     (enforce-guard (before-date (get-presale-end-time collection-name)))
     (enforce-guard (at-after-date (get-presale-start-time collection-name)))
     )

  (defcap CLAIM-RESERVED (account:string collection-name:string guard:guard)
    (let ( (accounts:[string] (get-presale-accounts collection-name)))
           (enforce (contains account accounts) "You did not reserve any tokens in the pre-sale")
         )
    (let ( (reserved-guard:guard (at 'guard (read-reservation account collection-name))))
           (enforce-guard reserved-guard)
         )
  )

  (defun enforce-owner:bool (collection-name:string creatorguard:guard)
    (with-read collections collection-name
      { "creator-guard":=guard }
      (enforce (= creatorguard guard) "You are not the owner of the collection")
      )
  )

  (defcap WHITELIST (collection-name:string creatorguard:guard)
    (compose-capability (PRIVATE))
    (enforce-owner collection-name creatorguard))

  (defcap PRIORITY ()
    (compose-capability (PRIVATE))
    (compose-capability (IS_ADMIN))
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
  )

  ;-----------------------------------------------------------------------------
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------

  (defschema collection-details
    collection-name:string
    symbol:string
    collection-id:string
    launched:bool
    creator:string
    creator-guard:guard
    description:string
    category:string
    total-supply:integer
    uri-list:[string]
    mint-price:decimal
    wl-price:decimal
    royalty-percentage:decimal
    royalty-address:string
    cover-image-url:string
    banner-image-url:string
    mint-start-date:string
    mint-start-time:time
    mint-end-date:string
    mint-end-time:time
    allow-free-mints:bool
    enable-whitelist:bool
    whitelist-addresses:[string]
    whitelist-start-time:time
    enable-presale:bool
    presale-addresses:[string]
    presale-start-date:string
    presale-start-time:time
    presale-end-date:string
    presale-end-time:time
    presale-mint-price:decimal
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

  (defschema token-ledger-schema
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

   (defschema sale
     status:string
     price:decimal)

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

  (defschema whitelist-schema
      account:string
      guard:guard
      claimed:integer)

  (defschema priority-schema
      account:string
      guard:guard
  )

  (defschema account-token-info
      account:string
      collection-name:string
      tokens:[string])

  (defschema pri-col
    collection-name:string
    claimed:bool)

  (defschema presale-schema
    collection-name:string
    account:string
    claimed:bool)

  (defschema free-mint-schema
      total-supply:integer
      current-index:integer
  )

  (defschema free-mint-account-schema
      account:string
      collection-name:string
      claimed:bool
  )

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
    fee:decimal
    discount:decimal)

  (defschema prime-schema
    accounts:string)

  (defschema discount-schema
    accounts:string)

  (deftable prime_role:{prime-schema})
  (deftable discount_role:{discount-schema})
  (deftable fee-ledger:{fee-schema})
  (deftable whitelists:{whitelist-schema})
  (deftable collections:{collection-details})
  (deftable request-collection-ledger:{collection-details})
  (deftable nfts-info-by-id:{nft-info})
  (deftable nfts-info-by-owner:{nft-info})
  (deftable minted-tokens:{minted-token})
  (deftable token-record:{token-record-schema})
  (deftable token-ledger:{token-ledger-schema})
  (deftable id-details:{id})
  (deftable name-details:{id})
  (deftable reservations:{reservation})
  (deftable sale-status:{sale})
  (deftable account-details:{account-schema})
  (deftable priority:{priority-schema})
  (deftable accountsInfo:{account-token-info})
  (deftable priority-collection:{pri-col})
  (deftable presale-ledger:{presale-schema})
  (deftable free-mint-ledger:{free-mint-schema})
  (deftable free-mint-account-ledger:{free-mint-account-schema})
  (deftable counts-table:{counts-schema})
  (deftable airdrop-history-table:{airdrop-history-schema})


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

    (defun get-wl-start-time (collection-name:string)
      (let (
            (getWlEnabled:bool (get-wl-enabled collection-name))
            )
          (if (= getWlEnabled true)
            (at 'whitelist-start-time (read collections collection-name))
            (time "9999-12-31T23:59:59Z")
          )
        )
      )

  (defun get-collection-launch-fee ()
    (at 'fee (read fee-ledger LAUNCH)))

  (defun get-collection-discount-fee ()
    (at 'discount (read fee-ledger LAUNCH)))

  (defun get-presale-start-time (collection-name:string)
    (at 'presale-start-time (read collections collection-name)))

  (defun get-presale-end-time (collection-name:string)
    (at 'presale-end-time (read collections collection-name)))

  (defun get-mint-price (collection-name:string)
      (at 'mint-price (read collections collection-name)))

  (defun get-wl-price (collection-name:string)
      (at 'wl-price (read collections collection-name)))

  (defun get-presale-price (collection-name:string)
      (at 'presale-mint-price (read collections collection-name)))

  (defun get-whitelist-accounts ( collection-name:string )
    (at 'whitelist-addresses (read collections collection-name)))

  (defun get-presale-accounts ( collection-name:string )
    (at 'presale-addresses (read collections collection-name)))

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

  (defun get-presale-enabled:bool (collection-name:string)
    (at 'enable-presale (read collections collection-name)))

  (defun get-wl-enabled:bool (collection-name:string)
    (at 'enable-whitelist (read collections collection-name)))

  (defun get-airdrop-enabled:bool (collection-name:string)
    (at 'enable-airdrop (read collections collection-name)))

  (defun get-free-mint-enabled:bool (collection-name:string)
    (at 'allow-free-mints (read collections collection-name)))

  (defun get-presale-mint-price:decimal(collection-name:string)
    (at 'presale-mint-price (read collections collection-name)))

  (defun get-presale-supply:integer(collection-name:string)
    (at 'max-nfts-for-presale (read collections collection-name)))

  (defun get-total-supply:integer (collection-name:string)
     (at 'total-supply (get-collection-details collection-name)))

  (defun get-current-index:integer (collection-name:string)
    (at 'current-index (read collections collection-name)))

    (defun get-current-index-free-mint:integer (collection-name:string)
      (at 'current-index (read free-mint-ledger collection-name)))

  (defun get-url-list:integer (collection-name:string)
    (at 'uri-list (read collections collection-name)))

  (defun get-total-nft-reserved:decimal (collection-name:string )
      (let (
            (presale-accounts:[string] (get-presale-accounts collection-name))
           )
        (fold (+) 0.0 (map (get-nft-reserved collection-name) presale-accounts))
     )
    )

  (defun get-whitelist-info:object{whitelist-schema} (account:string collection-name:string)
    (read whitelists (concat [collection-name "|" account])))

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

  (defun get-free-mint-claim:bool (collection-name:string account:string )
      (with-default-read free-mint-account-ledger (concat [collection-name "|" account])
        { 'claimed: false }
        { 'claimed:= claimed }
        claimed
      )
  )

  (defun is_col_launched:bool (collection-name:string)
    (with-read collections collection-name
      {
        'launched:=launched
      }
    (enforce (= launched true) "Not Launched Yet" )
    )
  )

  (defun get-minted:integer (collection-name:string)
    (with-read token-ledger collection-name
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

  (defun get-current-length:integer (collectionName:string)
    (with-read token-record collectionName
        { 'current-length:= current-length }
      current-length
    )
  )

  (defun get-length:integer (collectionName:string)
    (with-read token-ledger collectionName
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

  (defun get-royalty-info (collection-name:string type:string)
    (cond
      ((= type "rate")
        (at 'royalty-percentage (read collections collection-name))
    )
    ((= type "account")
      (at 'royalty-address (read collections collection-name))
    )
    ((= type "both")

      (format "Royalty Rate {} and Royalty Account {}" [(at 'royalty-percentage (read collections collection-name)) (at 'royalty-address (read collections collection-name))])
      (at 'royalty-address (read collections collection-name))
    )
    ["Invalid Type"]
  )
)

    (defun update-royalty-info (collection-name:string creator-guard:guard royalty-address:string royalty-percentage)
      (enforce-owner collection-name creator-guard)
      (update collections collection-name {
        'royalty-percentage: royalty-percentage,
        'royalty-address: royalty-address
        })
    )

    (defun get-prime-role ()
      (at 'accounts (read prime_role PRIME)))

    (defun get-discount-role ()
      (at 'accounts (read discount_role DISCOUNT)))


  ;;-----------------------------------------------------------------------------
  ; Admin Functions
  ;;-----------------------------------------------------------------------------

  (defun launch-collection:string (collection-name:string)
    (with-capability (IS_ADMIN)
      (with-read request-collection-ledger collection-name {
            "collection-name" := collection-name,
            'symbol:=symbol,
            'creator:=creator,
            'creator-guard:=guard,
            'description:= description,
            'category:= category,
            'total-supply:= total-supply,
            'uri-list:= uri-list,
            'mint-price:= mint-price,
            'wl-price:= wl-price,
            'royalty-percentage:= royalty-percentage,
            'royalty-address:= royalty-address,
            'cover-image-url:= cover-image-url,
            'banner-image-url:= banner-image-url,
            'mint-start-date:=mint-start-date,
            'mint-start-time:= mint-start-time,
            'mint-end-date:=mint-end-date,
            'mint-end-time:= mint-end-time,
            'allow-free-mints:= allow-free-mints,
            'enable-whitelist:= enable-whitelist,
            'whitelist-addresses:= wl-addresses,
            'whitelist-start-time:=wl-time,
            'enable-presale:= enable-presale,
            'presale-addresses:= presale-addresses,
            'presale-start-date:=presaleStartDate,
            'presale-start-time:=presaleStartTime ,
            'presale-end-date:=presaleEndDate,
            'presale-end-time:=presaleEndTime,
            'presale-mint-price:= preSaleMintPrice,
            'enable-airdrop:= enable-airdrop,
            'airdrop-supply:=airdropSupply,
            'airdrop-addresses := airdrop-addresses,
            'current-index := current-index,
            'policy:=policy
          }

        (insert collections collection-name {
            "collection-name" : collection-name,
            "symbol": symbol,
            "launched":true,
            "collection-id":"",
            'creator: creator,
            'creator-guard: guard,
            'description: description,
            'category: category,
            'total-supply: total-supply,
            'uri-list: uri-list,
            'mint-price: mint-price,
            'wl-price: wl-price,
            'royalty-percentage:  royalty-percentage,
            'royalty-address: royalty-address,
            'cover-image-url:  cover-image-url,
            'banner-image-url: banner-image-url,
            'mint-start-date: mint-start-date,
            'mint-start-time:  mint-start-time,
            'mint-end-date: mint-end-date,
            'mint-end-time:  mint-end-time,
            'allow-free-mints: allow-free-mints,
            'enable-whitelist: enable-whitelist,
            'whitelist-addresses: wl-addresses,
            'whitelist-start-time: wl-time,
            'enable-presale: enable-presale,
            'presale-addresses: presale-addresses,
            'presale-start-date: presaleStartDate,
            'presale-start-time: presaleStartTime ,
            'presale-end-date: presaleEndDate,
            'presale-end-time: presaleEndTime,
            'presale-mint-price: preSaleMintPrice,
            'enable-airdrop: enable-airdrop,
            'airdrop-supply: airdropSupply,
            'airdrop-addresses : airdrop-addresses,
            'current-index : 1,
            'policy:policy
          })

        (enforce (= (length uri-list) total-supply) "Total-supply and uri-list length does not match")

        (insert token-record collection-name {
          "current-length": (length uri-list),
          "uri-list": uri-list
          })

       (insert token-ledger collection-name {
            "current-length": (length uri-list)
            })
    )
    )
  )

  (defun add-roles (role:string accounts:[string])
    (with-capability (IS_ADMIN)
      (with-capability (PRIVATE)
        (map (add-role role) accounts)
      )
    )
  )

  (defun add-role (role:string account)
    ; (with-capability (IS_ADMIN)
      (require-capability (PRIVATE))
      (let* (
              (prime_accounts:string (get-prime-role))
              (discount_accounts:string (get-discount-role))
              (in_prime:bool (contains account prime_accounts))
              (in_discount:bool (contains account discount_accounts))
            )
        (cond
          ((= role "prime")
            [
              (enforce (!= in_discount true) "Remove from Discount Ledger")
              (update prime_role PRIME{
                "accounts":(+ (+ prime_accounts " ") account )
                })
            ]
          )

          ((= role "discount")
            [
              (enforce (!= in_prime true) "Remove from Prime Ledger")
              (update discount_role DISCOUNT{
                "accounts":(+ (+ discount_accounts " ") account)
                })
            ]
          )
          ["Invalid Role"]
          )
      )
    ; )
  )

  (defun deny-collection (collection-name:string)
    (with-capability (IS_ADMIN)
      (let (
            (fee:decimal (get-collection-launch-fee))
            (creator:string (get-collection-creator-of-request collection-name))
          )
          (coin.transfer LAUNCHPAD_ACC creator fee)
      )
    )
  )

  ;;-----------------------------------------------------------------------------
  ; Collection Functions
  ;;-----------------------------------------------------------------------------

  (defun create-ng-collection:string (collection-name:string creator:string creator-guard:guard)
    (enforce-owner collection-name creator-guard)
    (with-read collections collection-name
      { "launched":=launched,
        "creator-guard":=guard }
    (enforce (= launched true) "Wait for admin approval...")
    (let (
            (collection-id:string (n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection.create-collection-id collection-name guard))
            (collectionSize:integer (get-total-supply collection-name))
         )

    (n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection.create-collection
      collection-id
      collection-name
      collectionSize
      creator
      guard )

      (update collections collection-name {
        "collection-id":collection-id
        })
    )
    (insert counts-table collection-name { "count": 0 })
    )
  )

  (defun nft-collection-request
    ( collection-name:string
      symbol:string
      creator:string
      creator-guard:guard
      description:string
      category:string
      total-supply:integer
      uri-list:[string]
      mint-price:decimal
      royalty-percentage:decimal
      royalty-address:string
      cover-image-url:string
      banner-image-url:string
      mint-start-date:string
      mint-start-time:time
      mint-end-date:string
      mint-end-time:time
      allow-free-mints:bool
      enable-whitelist:bool
      enable-presale:bool
      enable-airdrop:bool
      policies:string)
      (enforce (and (>= royalty-percentage 0.0 ) (<= royalty-percentage 1.0 )) "Royalty Should be between 0.0 and 1.0")
      (let* ( (g:guard (at 'guard (coin.details creator)))
              (fee:decimal (get-collection-launch-fee))
              (discount:decimal (get-collection-discount-fee))
              (prime_accounts:string (get-prime-role))
              (discount_accounts:string (get-discount-role))
              (discounted-fee (* fee (- 1 discount)))
           )
      (enforce-reserved creator g)
      (cond
        ((contains creator prime_accounts)[])
        ((contains creator discount_accounts)
          [(coin.transfer creator LAUNCHPAD_ACC discounted-fee)])
        [(coin.transfer creator LAUNCHPAD_ACC fee)]
      )
      (insert request-collection-ledger collection-name
        { 'collection-name: collection-name,
          'symbol:symbol,
          'collection-id:"",
          'launched:false,
          'creator:creator,
          'creator-guard:creator-guard,
          'description: description,
          'category: category,
          'total-supply: total-supply,
          'uri-list:uri-list,
          'mint-price: mint-price,
          'wl-price: 0.0,
          'royalty-percentage: royalty-percentage,
          'royalty-address:royalty-address,
          'cover-image-url: cover-image-url,
          'banner-image-url: banner-image-url,
          'mint-start-date:mint-start-date,
          'mint-start-time: mint-start-time,
          'mint-end-date:mint-end-date,
          'mint-end-time: mint-end-time,
          'allow-free-mints: allow-free-mints,
          'enable-whitelist: enable-whitelist,
          'whitelist-addresses: [],
          'whitelist-start-time:(time "9999-12-31T23:59:59Z"),
          'enable-presale: enable-presale,
          'presale-addresses: [],
          'presale-start-date:"",
          'presale-start-time:(time "9999-12-31T23:59:59Z") ,
          'presale-end-date:"",
          'presale-end-time: (time "9999-12-31T23:59:59Z"),
          'presale-mint-price: 0.0,
          'enable-airdrop: enable-airdrop,
          'airdrop-supply: 0,
          'airdrop-addresses: [],
          'current-index:0,
          'policy:policies })

          (emit-event (CREATE-COLLECTION name creator))
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
  ; Whitelist Functions
  ;-----------------------------------------------------------------------------


  (defun create-whitelist:[string]
    ( collection-name:string
      creator:string
      creatorguard:guard
      wl-addresses:[string]
      wl-price:decimal
      wl-time:time )
    (with-capability (PRIVATE)
      (enforce-owner collection-name creatorguard)
      (let (
            (enabled:bool (get-wl-enabled collection-name))
            (launched:bool (is_col_launched collection-name))
            )
        (enforce (= launched true) "Collection Not Launched Yet!")
        (enforce (= enabled true) "Whitelist is disabled")
        (update collections collection-name
            { "whitelist-addresses": (+ (get-whitelist-accounts collection-name) wl-addresses) })
        (update collections collection-name
            { "whitelist-start-time": wl-time, "wl-price":wl-price })
        (add-whitelists collection-name wl-addresses (get-collection-creator-guard collection-name))
      )
    )
  )

  (defun add-wl-accounts ( collection-name:string wl-addresses:[string] creatorguard:guard)
    (enforce-owner collection-name creatorguard)
    (with-capability (PRIVATE)
    (update collections collection-name
        { "whitelist-addresses": (+ (get-whitelist-accounts collection-name) wl-addresses) })
      (add-whitelists collection-name wl-addresses creatorguard)
    )
  )

  (defun add-whitelists ( collection-name:string accounts:[string] creatorguard:guard)
    (require-capability (PRIVATE))
      (map (add-whitelist collection-name) accounts)
  )

  (defun add-whitelist (collection-name:string account:string)
    (require-capability (PRIVATE))
    (insert whitelists (concat [collection-name "|" account]){
      "account": account,
      "guard": (at 'guard (coin.details account)),
      "claimed": 0
    })
  )

  (defun remove-whitelist-addresses ( collection-name:string creatorguard:guard addresses:[string] )
    (with-capability (PRIVATE)
      (enforce-owner collection-name creatorguard)
      (map (remove-airdrop-address collection-name) addresses)
    )
  )

  (defun remove-whitelist-address ( collection-name:string  address:string )
    (require-capability (PRIVATE))
    (let* (
          (wl-addresses:[string] (get-whitelist-accounts collection-name))
          (contain:bool (contains address wl-addresses))
          )
          (if (= contain true)
            [(update collections collection-name
              { "whitelist-addresses": (filter (!= address) wl-addresses) })]
            "Account Doesnot Exist!"
          )
    )
  )


  (defun update-wl-price (collection-name:string creatorGuard:guard price:decimal)
    (enforce-owner collection-name creatorGuard)
    (update collections collection-name {
      "wl-price":price
      })
  )

  (defun enable-whitelist:string
    (creator:string
    creatorguard:guard
    collection-name:string)

    (enforce-owner collection-name creatorguard)
    (let (
          (enabled:bool (get-wl-enabled collection-name))
          (launched:bool (is_col_launched collection-name))
          (public-enabled:bool (check-public collection-name))
          )

    (enforce (= launched true) "Collection Not Launched Yet!")
    (enforce (!= public-enabled true) "Public Sale is Live!")
    (enforce (= enabled false) "Whitelist is already enabled!")
    (update collections collection-name {
      'enable-whitelist: true
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
                (launched:bool (is_col_launched collection-name))
                )

          (enforce (= launched true) "Collection Not Launched Yet!")
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

(defun gift-nft (tokenId:string creator:string account:string)
  (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.transfer-create tokenId creator account (at 'guard (coin.details account)) 1.0))

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
        )
      )


(defun increase-count (collection-name:string)
    @doc "Increase the count of a key in a table by 1"
    (require-capability (PRIVATE))
    (update counts-table collection-name {"count": (+ 1 (get-count collection-name))}))

(defun get-count:integer (collection-name:string)
    @doc "Gets the count for a key"
    (at "count" (read counts-table collection-name ['count])))

(defun get-airdrop-history()
    @doc "Returns all the airdrop history."
    (select airdrop-history-table (where "account" (!= ""))))
;-----------------------------------------------------------------------------
; Free-Mint Functions
;-----------------------------------------------------------------------------

(defun create-free-mint:string ( collection-name:string creator:string free-mint-supply:integer creatorguard:guard)
  (with-capability (PRIVATE)
    (enforce-owner collection-name creatorguard)
    (let (
          (enabled:bool (get-free-mint-enabled collection-name))
          (launched:bool (is_col_launched collection-name))
          (ttlSupply:integer (get-total-supply collection-name))
          )
      (enforce (= launched true) "Collection Not Launched Yet!")
      (enforce (= enabled true) "Free Mint is disabled")
      (if (!= ttlSupply free-mint-supply)
        (insert free-mint-ledger collection-name{
            'total-supply:  free-mint-supply,
            'current-index: 1
            })

        (update collections collection-name{
          'total-supply: (- ttlSupply free-mint-supply)
          })
      )
    )
  )
)

    (defun get-total-supply-free-mint:integer (collection-name:string)
      (with-default-read free-mint-ledger collection-name
        { 'total-supply: 0 }
        { 'total-supply:= total-supply }
      total-supply
     )
    )

    (defun before-reserving-token-free-mint:string (collection-name:string account:string)
      (insert free-mint-account-ledger (concat [collection-name "|" account]) {
        'account:account,
        'collection-name:collection-name,
        'claimed : false
      })
    )

    (defun reserve-token-free-mint:bool
      ( collection-name:string
        account:string
        amount:integer
      )
      (let (
             (current-indx:integer (get-current-index collection-name))
             (current-indx-free-mint:integer (get-current-index-free-mint collection-name))
             (total-supply:integer (get-total-supply collection-name))
             (claimed:bool (get-free-mint-claim collection-name account))
             (free-mint-enabled:bool (get-free-mint-enabled collection-name))
             (wl-enabled:bool (check-whitelist collection-name))
             (public-enabled:bool (check-public collection-name))
             (presale-enabled:bool (check-presale collection-name))
             (acc-guard:guard (at "guard" (coin.details account)))
             (current-length:integer (get-length collection-name))
             (presale-users:[string] (get-presale-accounts collection-name))
             (priority-sale-users:[string] (get-priority-users))
           )

           (enforce (= free-mint-enabled true) "Free Mint Not Enabled")
           (enforce (>= (- current-length amount) 0 )  "We are unable to produce additional images as the current supply has been depleted.")


           (with-capability (MINT-NFT account )
            (cond
              (
                (and (and (!= presale-enabled true ) (and  (!= public-enabled true) (!= wl-enabled true) )) (and (= claimed false) (<=  current-indx-free-mint (get-total-supply-free-mint collection-name))))
                [
                  (enforce (= amount 1) "One Free Mint allowed per user")
                  (update free-mint-account-ledger (concat [collection-name "|" account]) {
                    'claimed : true
                    })

                    (update free-mint-ledger collection-name
                      { "current-index": (+ current-indx-free-mint 1) }
                    )
                  true
                ]
              )
              (
                (and (check-presale collection-name)(contains account presale-users))
                [
                  (let (
                          (claimPresale:bool (get-presale-claim collection-name account))
                        )
                        (if (= claimPresale false)
                          (mint-presale-nft account collection-name amount)
                          (mint-for-price account collection-name amount)

                        )
                )]
              )

              [(mint-for-price account collection-name amount)]
            )

            (write accountsInfo (concat [collection-name "|" account]){
              "account":account,
              "collection-name":collection-name,
              "tokens":[] })
            (update token-ledger collection-name {
                'current-length: (- current-length amount)
                })


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

    (defun mint-internal-free-mint:bool
      (
        collection-name:string
        account:string
        guard:guard
        amount:integer
        current-indx:integer
        current-indx-free-mint:integer
      )
      (require-capability (MINT-NFT account ))

      (update free-mint-ledger collection-name
        { "current-index": (+ current-indx amount) }
      )

      (map
        (mint-token collection-name account guard)
        (map (+ current-indx) (enumerate 0 (- amount 1)))
      )
      (emit-event (MINT_EVENT collection-name account amount))
    )

  ;-----------------------------------------------------------------------------
  ; Priority Functions
  ;-----------------------------------------------------------------------------

  (defun add-priority-user (account:string)
    (with-capability (PRIVATE)
    (insert priority account{
      "account": account,
      "guard": (at 'guard (coin.details account))
    })
    )
  )


    (defun add-priority-users (accounts:[string] )
      ; (with-capability (PRIORITY)
        (map (add-priority-user) accounts)
      ; )
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

    (defun replace-policies (collection-name:string creatorguard:guard policies:string)
      (enforce-owner collection-name creatorguard)
      (update collections collection-name{
        "policy": policies
        })
    )

  ;;-----------------------------------------------------------------------------
  ; Presale Functions
  ;;-----------------------------------------------------------------------------

  (defun enforce-presale (account collection-name:string)
    (let ( (accounts:[string] (get-presale-accounts collection-name)))
           (enforce (contains account accounts) "You are not whitelisted")
           (enforce-guard (at-after-date (get-presale-start-time collection-name)))
           (enforce-guard (at-before-date (get-presale-end-time collection-name)))
    )
  )

  (defun update-presale-mint-price:decimal (creator-guard:guard collection-name:string price:decimal)
      (enforce-owner collection-name creator-guard)
      (update collections collection-name {
        "presale-mint-price":price })
  )

  (defun update-presale-mint-time:decimal (creator-guard:guard collection-name:string presale-start-time:time presale-end-time:time)
      (enforce-owner collection-name creator-guard)
      (update collections collection-name {
        "presale-start-time":presale-start-time,
        "presale-end-time": presale-end-time
      }
    )
  )

  (defun enable-presale:string
    (creator:string
    creatorguard:guard
    collection-name:string)

    (enforce-owner collection-name creatorguard)
    (let (
          (enabled:bool (get-presale-enabled collection-name))
          (wl-enabled:bool (check-whitelist collection-name))
          (launched:bool (is_col_launched collection-name))
          (public-enabled:bool (check-public collection-name))
          )

    (enforce (= launched true) "Collection Not Launched Yet!")
    (enforce (!=  wl-enabled true) "Whitelist Sale is Live!")
    (enforce (!= public-enabled true) "Public Sale is Live!")
    (enforce (= enabled false) "Presale is already enabled!")
    (update collections collection-name {
      'enable-presale: true
      })
    )
  )

  (defun create-presale
    ( creator:string
      creatorguard:guard
      collection-name:string
      presale-mint-price:decimal
      presale-start-date:string
      presale-start-time:time
      presale-end-date:string
      presale-end-time:time
      addresses:[string]
    )
    (with-capability (PRIVATE)
    (enforce-owner collection-name creatorguard)
    (let (
          (enabled:bool (get-presale-enabled collection-name))
          (launched:bool (is_col_launched collection-name))
          (wl-enabled:bool (check-whitelist collection-name))
          (public-enabled:bool (check-public collection-name))
          (mint-end-time:time (get-mint-end-time collection-name))
          (chain-time (at 'block-time (chain-data)))
          )
    (enforce (<= chain-time mint-end-time) "Mint not Live!")
    (enforce (= launched true) "Collection Not Launched Yet!")
    (enforce (!=  wl-enabled true) "Whitelist Sale is Live!")
    (enforce (!= public-enabled true) "Public Sale is Live!")
    (enforce (= enabled true) "Presale is disabled")
      (update collections collection-name {
          'presale-start-date:presale-start-date,
          'presale-start-time:presale-start-time,
          'presale-end-date:presale-end-date,
          'presale-end-time:presale-end-time,
          'presale-mint-price:presale-mint-price
          })
        (update collections collection-name
            { "presale-addresses": (+ (get-presale-accounts collection-name) addresses) })
        (add-presale-addresses collection-name addresses)
    )
    )
  )

  (defun add-presale-accounts ( collection-name:string presale-addresses:[string] creatorguard:guard)
    (enforce-owner collection-name creatorguard)
    (let (
          (enabled:bool (get-presale-enabled collection-name))
          (launched:bool (is_col_launched collection-name))
          (whitelistLive:bool (check-presale collection-name))
          )
    (with-capability (PRIVATE)
      (enforce (= launched true) "Collection Not Launched Yet!")
      (enforce (= enabled true) "Whitelist is disabled")
      (enforce (= whitelistLive false) "Presale is Live!")
      (update collections collection-name
          { "presale-addresses": (+ (get-presale-accounts collection-name) presale-addresses) })
      (add-presale-addresses collection-name presale-addresses)
    )
    )
  )

  (defun add-presale-addresses ( collection-name:string accounts:[string])
    (require-capability (PRIVATE))
    (map (add-presale-address collection-name) accounts)

  )

  (defun add-presale-address (collection-name:string account:string)
    (require-capability (PRIVATE))
    (insert presale-ledger (concat [collection-name "|" account]){
      "collection-name": collection-name,
      "account": account,
      "claimed": false
    })
  )

    (defun remove-presale-addresses ( collection-name:string creatorguard:guard addresses:[string] )
      (with-capability (PRIVATE)
        (enforce-owner collection-name creatorguard)
        (map (remove-presale-address collection-name) addresses)
      )
    )

    (defun remove-presale-address ( collection-name:string  address:string )
      (require-capability (PRIVATE))
      (let* (
            (presale-addresses:[string] (get-presale-accounts collection-name))
            (contain:bool (contains address presale-addresses))
            )
            (if (= contain true)
              [(update collections collection-name
                { "presale-addresses": (filter (!= address) presale-addresses) })]
              "Account Doesnot Exist!"
            )
      )
    )

  (defun read-reservation (account:string collection-name:string)
      (read reservations (concat [collection-name "|" account])))


  ;-----------------------------------------------------------------------------
  ; Mint functions
  ;-----------------------------------------------------------------------------

  (defun check-whitelist:bool (collection-name:string)
    (let* (
          (mint-start-time:time (get-mint-start-time collection-name))
          (whitelist-mint-time:time (get-wl-start-time collection-name))
          (chain-time (at 'block-time (chain-data)))
        )
        (and (<= whitelist-mint-time chain-time) (<= chain-time mint-start-time))
    )
  )


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

  (defun check-presale:bool (collection-name:string)
    (let* (
          (presale-start-time:time (get-presale-start-time collection-name))
          (presale-end-time:time (get-presale-end-time collection-name))
          (chain-time (at 'block-time (chain-data)))
        )
        (and (<= chain-time presale-end-time) (>= chain-time presale-start-time))
    )
  )

(defun bulk-sync-with-ng
  ( collection-name:string
    accountIds:[integer])
  (with-capability (MINTPROCESS collection-name)
    (map (sync-with-ng collection-name) accountIds)
  )
)

  (defun sync-with-ng:string
    (
      collection-name:string
      accountId:integer
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
                  (current-length:integer (get-current-length collection-name))
                  (index:integer (mod random current-length))
                  (available-tokens:[string] (at 'uri-list (read token-record collection-name)))
                  (minted-token:string (at index available-tokens))
                  (token-id:string (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.create-token-id guard minted-token))
                  (token-precision 0)
                  (policy_stack:string (get-policy-of-collection collection-name))
                  (policy:[module{n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1}] (n_4e470a97222514a8662dd1219000a0431451b0ee.std-policies.to-policies policy_stack))
                )
                (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.create-token token-id token-precision minted-token policy guard)
                (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.mint token-id account mintto 1.0)
                (update minted-tokens (get-mint-token-id collection-name accountId) {"revealed": true, "marmToken": token-id})

                (update token-record collection-name {
                  'uri-list: (filter (!= minted-token) available-tokens),
                  'current-length: (- current-length 1)
                  })

                (mint-nft-info collection-name account token-id)
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

  (defun available-tokens:[string] (collection-name:string creator-guard:guard)
    (enforce-owner collection-name creator-guard)
    (at 'uri-list (read token-record collection-name))
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

  (defun mint-nft-info:string (collection-name:string account:string token-id:string)
    (require-capability(PRIVATE))
    (insert nfts-info-by-id token-id  {
        "owner":account,
        "token-id":token-id,
        "collection-name":collection-name,
        "collection-id":(get-collection-id collection-name)})

    (write nfts-info-by-owner account {
          "owner":account,
          "token-id":token-id,
          "collection-name":collection-name,
          "collection-id":(get-collection-id collection-name)})
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
                  (priority-sale-users:[string] (get-priority-users))
                  (presale-users:[string] (get-presale-accounts collection-name))
                  (current-indx:integer  (get-current-index collection-name))
                  (acc-guard:guard (at "guard" (coin.details account)))
                  (passBalance:integer (get-pass-balance account))
              )
          (cond

              (
                (= account (get-collection-creator collection-name))
                [true]
              )
            (
              (and (check-presale collection-name)(contains account presale-users))
              [
                (let (
                        (claimPresale:bool (get-presale-claim collection-name account))
                      )
                      (if (= claimPresale false)
                        (mint-presale-nft account collection-name amount)
                        [
                          (and (> passBalance 0 )(contains account priority-sale-users))
                          [
                           (let((claimPass:bool (get-pass-claim collection-name account)))
                             (if (= claimPass false)
                               (mint-for-free  account collection-name amount)
                               (mint-for-price account collection-name amount)
                             )
                          )]
                        ]
                      )
              )]
            )
            (
              (and (> passBalance 0 )(contains account priority-sale-users))
              [
               (let((claim:bool (get-pass-claim collection-name account)))
                 (if (= claim false)
                   (mint-for-free account collection-name amount)
                   (mint-for-price account collection-name amount)
                 )
              )]
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
    ( collection-name:string
      account:string
      guard:guard
      amount:integer
      current-indx:integer
    )
    (require-capability (MINT-NFT account ))
    (update collections collection-name
      { "current-index": (+ current-indx amount) })

      (map
        (mint-token collection-name account guard)
          (map (+ current-indx) (enumerate 0 (- amount 1)))
          )
          (emit-event (MINT_EVENT collection-name account amount))
        )

  (defun mint-token:string
    ( collection-name:string
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

  (defun get-mint-token-id:string
    ( collection:string
      accountId:integer
    )
    (concat [collection "|" (int-to-str 10 accountId)])
  )

(defun mint-for-free:bool
  (
    account:string
    collection-name:string
    amount:integer
  )
  (let* (
        (acc-guard:guard (at "guard" (coin.details account)))
        (col-acc:[string] (keys priority-collection))
        (isAvailable:bool (contains (concat [collection-name "|" account]) col-acc))
      )

    (require-capability (MINT-NFT account))

    (if (= account LAUNCHPAD_ACC)
      (update priority-collection (concat [collection-name "|" account]){
          "claimed": false})
      [
        (enforce (= amount 1) "You can claim only One NFT per COLLECTION")

          (if (= isAvailable true)
            (update priority-collection (concat [collection-name "|" account]){
              "collection-name": collection-name,
              "claimed": true
              })
            (insert priority-collection (concat [collection-name "|" account]){
              "collection-name": collection-name,
              "claimed": true
              })
          )
        (free.KMPPV2.burn account acc-guard)
      ]
    )
    true
  )
)

  (defun mint-presale-nft (account:string collection-name:string amount:integer)
    (with-capability (RESERVE collection-name)
      (if (< (diff-time (at 'block-time (chain-data)) (get-presale-start-time collection-name)) 0.0) (enforce-presale account) "Pre-sale ended")
        (let (
                (creator:string (get-collection-creator collection-name))
                (g (at 'guard (coin.details account)))
                (nft-amount:integer (+ amount (get-nft-reserved collection-name account)))
                (price:decimal (* (get-presale-mint-price collection-name) amount))
              )
              (coin.transfer account creator price)
              (write reservations (concat [collection-name "|" account])
                { "account"    : account
                  , "amount-nft" : nft-amount
                  , "guard"      : g
                 })
                (format "{} reserved NFT with {} KDA" [account, price])
            )

            (update presale-ledger (concat [collection-name "|" account])
              {"claimed":true})
          )
        )

  (defun mint-for-price
    ( account:string
      collection-name:string
      amount:integer
     )
     (require-capability (MINT-NFT account ))
     (let* (
            (whitelist-enabled:bool (check-whitelist collection-name))
            (creator:string (get-collection-creator collection-name))
            (wl-accounts:[string] (get-whitelist-accounts collection-name))
            (contain:bool (contains account wl-accounts))
            )
            (cond
              (whitelist-enabled
                  [
                  (enforce (= contain true) "Account is not Whitelisted")
                  (enforce-whitelist account (at 'guard (coin.details account)) collection-name)
                  (let  ( (claimed:integer (at 'claimed (get-whitelist-info account collection-name)))
                          (price:decimal (get-wl-price collection-name))
                          )
                          (coin.transfer account creator (* amount price))
                          (update whitelists (concat [collection-name "|" account]){
                           "claimed": (+ claimed 1)
                            })
                    )]
                )

                [(check-public collection-name) (coin.transfer account creator (* amount (get-mint-price collection-name)))]
              )
          )
  )

  (defun mint-for-price-free-mint
    ( account:string
      collection-name:string
      amount:integer
      )

      (require-capability (MINT-NFT account ))

      (let* (
              (public-enabled:bool (check-public collection-name))
              (whitelist-enabled:bool (check-whitelist collection-name))
              (creator:string (get-collection-creator collection-name))
              (wl-accounts:[string] (get-whitelist-accounts collection-name))
              (contain:bool (contains account wl-accounts))
            )
            (cond
              (whitelist-enabled
                [
                (enforce (= contain true) "Account is not Whitelisted")
                (enforce-whitelist account (at 'guard (coin.details account)) collection-name)
                (let  ( (claimed:integer (at 'claimed (get-whitelist-info account collection-name)))
                        (price:decimal (get-wl-price collection-name))
                      )
                      (coin.transfer account creator (* amount price))
                      (update whitelists (concat [collection-name "|" account]){
                        "claimed": (+ claimed 1)
                        })
                  )]
                )
                [(check-public collection-name) (coin.transfer account creator 5.0)]
                )
              )
  )

  (defun update-public-price (collection-name:string creatorGuard:guard price:decimal)
    (enforce-owner collection-name creatorGuard)
    (update collections collection-name { "mint-price":price })
   )

  (defun enforce-whitelist:bool (account:string guard:guard collection-name:string)
    (with-read whitelists (concat [collection-name "|" account]){
      'guard:= g,
      'claimed:= claimed }
     (enforce (= g guard) "Guards doesn't match.")))

  (defun enforce-priority (account:string guard:guard)
    (with-read priority account{
      'guard:= g,
      'claimed:= claimed }
    (enforce (= g guard) "Guards doesn't match."))
    )

    (defun get-pass-claim:bool (collection-name:string account:string)
      (with-default-read priority-collection (concat [collection-name "|" account])
        { 'claimed : false}
        { 'claimed := claimed} ; Default value if the user isn't found
        claimed
        )
    )

  (defun get-presale-claim:bool (collection-name:string account:string)
   (with-read presale-ledger (concat [collection-name "|" account])
    { 'claimed:= claimed }
    claimed)
   )

  (defun claim-pass-nft (collection-name:string account:string creator-guard:guard)
    (enforce-owner collection-name creator-guard)
    (insert priority-collection (concat [collection-name "|" account]) {
      "collection-name":collection-name,
      "claimed":false }
    )
   )

  (defun update-mint-time (collection-name:string creator-guard:guard mint-start-time:time mint-end-time:time wl-start-time:time)
    (enforce-owner collection-name creator-guard)
      (update collections collection-name {
        "mint-start-time":mint-start-time,
        "mint-end-time":mint-end-time,
        "whitelist-start-time":wl-start-time })
  )

  (defun init (fee:decimal discount:decimal)
    (insert fee-ledger LAUNCH {
      "fee":fee,
      "discount":discount }
     )

     (insert prime_role PRIME{ "accounts":"" })

     (insert discount_role DISCOUNT{ "accounts":"" })
    )

    (defun update-launch-fee (fee:decimal type:string)
      (enforce (< 0.0 fee) "fee is not a positive number")
         (cond
           ((= type "launch")
            [ (update fee-ledger LAUNCH{
                'fee:fee})]
           )
           ((= type "discount")
            [ (update fee-ledger LAUNCH{
                'discount:fee})]
           )
           ["Invalid Type"]
         )
     )

     (defun get-token-details (account:string)
     @doc "Get token details for an account"
      (let*
        ((balances (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.list-balances account))
          (token-ids (map (lambda (balance) (at 'id balance)) balances))
          (details (map (lambda (token-id)
          { "token-id": token-id
          , "uri": (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.get-uri token-id)
          , "collection": (n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection.get-token-collection token-id)
          })
        token-ids)))
      details))
)
;

