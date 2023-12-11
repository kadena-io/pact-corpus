(module kmc-policy GOVERNANCE
    @doc "Kadena Mining Club Gen 1 Miner policy"
    
    (implements kip.token-policy-v1)
    (use kip.token-policy-v1 [token-info])
    (implements free.kmc-token-policy-v13)
    (use coin)

    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst MINT_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")
    (defconst KMC_ID "kmc-id") ;tracks the order of the nfts
    (defconst MINT_PAUSED:string "mint-paused")
    (defconst MINT_STARTED:string "mint-started")
    (defconst MINT_COMPLETED:string "mint-completed")
    (defconst NFT_COUNT:string "nft-count")
    (defconst MINT_COUNT:string "mint-count")

    (defcap GOVERNANCE ()
        (enforce-keyset ADMIN_KEYSET)
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defschema number-to-id-schema
        @doc "Stores the token id:number pairing"
        token-id:string
    )

    (defschema burned-vial-miner-schema
        @doc "Tracks if a miner had a vial burned simultaneously"
        burned-both:bool
    )

    (defschema royalty-details
        royalty-receiever:string
        royalty-rate:decimal
    )

    (defschema miner-status-schema
        @doc "Stores information of the Miners NFT collection"
        staked:bool 
        for-sale:bool
        token-id:string
    )

    (defschema store-guard
        g:guard
    )

    (defschema mint-schema
         tokens-list:[string]
         current-length:integer
         status:string
    )

    (defschema collection-schema
        id:string
        royalty-info:[object{royalty-details}] ;accounts which receive the royalty
        collection-size:integer ;total supply of tokens that will ever exist
        provenance-hash:string ;sha256 of combined string
        tokens:[string] ;list of sha256 of the images that will ever exist
        creator-account:string
        creator-guard:guard
        price:decimal
        fungible:module{fungible-v2}
    )

    (defschema token-schema
        id:string
        number:integer
        owner:string
        supply:decimal
    )

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )

    (deftable burned-both-table:{burned-vial-miner-schema})
    (deftable number-to-id-table:{number-to-id-schema})
    (deftable counts-table:{counts-schema})
    (deftable guard-storage-table:{store-guard})
    (deftable miner-status-table:{miner-status-schema})
    (deftable kmc-tokens:{token-schema})
    (deftable mint-status:{mint-schema})
    (deftable collection-info:{collection-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        (insert counts-table NFT_COUNT {"count": 10000})
        (insert counts-table MINT_COUNT {"count": 10000})
    )

    (defun get-tokens-owned:list (account:string)
        @doc "returns all tokens that an account owns"
        (select kmc-tokens ['id] (where "owner" (= account)))
    )

    (defun get-token:object (token-id:string)
        (read kmc-tokens token-id)
    )

    (defun get-token-nft-id (nft-id:string)
        @doc "returns the t:token of an nft-id `1`" 
        (select kmc-tokens ['id, 'number] (where "number" (= (str-to-int nft-id))))
    )

    (defun fix-multiple (start:integer end:integer)
        (map (fix-my-shit) (enumerate start end))
    )

    (defun fix-my-shit (number:integer)
        (let*
            (
                (token-id (get-token-nft-id2 (int-to-str 10 number)))  ; 10001 "t:v2YoCbHZuN6IUcBKphIehJejXVOgMJu2TLO-6AdWmVk"
                (original-number (at 'number (get-token token-id))) ;10000
                (fixed-number (+ original-number 1)) ;10001
            )
            (update kmc-tokens token-id
              { "number": fixed-number })
        )
    )

    (defun get-token-nft-id2 (nft-id:string)
        (at 'token-id (read number-to-id-table nft-id))
    )

    (defun set-token-nft-id (token-id:string count:string)
        (enforce-kmc)
        (write number-to-id-table count
            { "token-id" : token-id })
    )

    (defun update-burned (token-id:string)
        (enforce-kmc)
        (write burned-both-table token-id
            { "burned-both" : true })
    )

    (defun read-burned (token-id:string)
        (at 'burned-both (read burned-both-table token-id))
    )

    (defun get-all-for-sale:list ()
        (select miner-status-table ['token-id] (where 'for-sale (= true)))
    )

    (defun get-all-tokens ()
        (keys kmc-tokens)
    )

    (defun get-all-owners ()
        (select kmc-tokens ["owner"] (where "owner" (!= "null")))
    )

    (defun get-miner-status (token-id:string)
        (read miner-status-table token-id)
    )

    (defun get-status:object (token-id:string)
        @doc "Returns the for-sale and staked status of a t:token"
        (read miner-status-table token-id)
    )

    (defun get-owner:string (token-id:string)
        @doc "Returns the owner address of a t:token as a string"
        (at 'owner (read kmc-tokens token-id))
    )

    (defun get-collection:object{collection-schema} (collection-id:string)
        (read collection-info collection-id)
    )

    (defun get-details:object{collection-schema} (collection:string)
        @doc "Returns the details of the collection"
        (read collection-info collection)
    )

    (defun get-policy:object{token-schema} (token:object{token-info})
        (read kmc-tokens (at 'id token))
    )

    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counts-table key ['count]))
    )

    (defun enforce-offer:bool
        ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
        @doc "Capture quote spec for SALE of TOKEN from message"
        (enforce-ledger)
        (enforce-sale-pact sale-id)
        (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
                (fungible:module{fungible-v2} (at 'fungible spec) )
                (price:decimal (at 'price spec))
                (recipient:string (at 'recipient spec))
                (recipient-guard:guard (at 'recipient-guard spec))
                (recipient-details:object (fungible::details recipient))
                (sale-price:decimal (* amount price)) )
            (fungible::enforce-unit sale-price)
            (enforce (< 0.0 price) "Offer price must be positive")
            (enforce (=
              (at 'guard recipient-details) recipient-guard)
            "Recipient guard does not match")
            (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
            (emit-event (QUOTE sale-id (at 'id token) amount price sale-price spec)))
            (with-read miner-status-table (at 'id token)
                { 'staked := staked
                , 'for-sale := for-sale}
                    (update miner-status-table (at 'id token)
                        { "for-sale" : false })
                    (enforce (= staked false) "Please turn off your NFT via the Farm before trying to transfer")
                    (enforce (= for-sale false) "Please remove your NFT from the KMC or Hypercent marketplace before trying this action")
            )
          true
    )

    (defun enforce-buy:bool
        ( token:object{token-info}
          seller:string
          buyer:string
          buyer-guard:guard
          amount:decimal
          sale-id:string )
        (enforce-ledger)
        (enforce-sale-pact sale-id)
        (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
          (enforce (= qtoken (at 'id token)) "incorrect sale token")
          (bind spec
            { 'fungible := fungible:module{fungible-v2}
            , 'price := price:decimal
            , 'recipient := recipient:string
            }
            (fungible::transfer buyer recipient (* amount price))
          )
        )
        (update kmc-tokens (at 'id token)
            { "owner" : buyer }
        )
        true
    )
    
    (defun enforce-sale-pact:bool (sale:string)
        "Enforces that SALE is id for currently executing pact"
        (enforce (= sale (pact-id)) "Invalid pact/sale id")
    )

    (defun enforce-transfer:bool
        ( token:object{token-info}
          sender:string
          guard:guard
          receiver:string
          amount:decimal )
        (enforce-ledger)
        (with-read miner-status-table (at 'id token)
            { 'staked := staked }
                (update miner-status-table (at 'id token)
                    { "for-sale" : false })
                (enforce (= staked false) "Please turn off your NFT via the Farm before trying to transfer")
        )
        (update kmc-tokens (at 'id token)
            { "owner" : receiver }
        )
        true
    )

    (defun update-for-sale:bool (token-id:string sale:bool)
        (enforce-kmc)
        (update miner-status-table token-id
            { "for-sale": sale }
        )
    )

    (defun update-staked (token-id:string on-off:bool)
        (enforce-kmc)
        (update miner-status-table token-id
            { "staked": on-off }
        )
    )

    (defun enforce-kmc ()
        (with-read guard-storage-table "kmc"  {'g:=g}
            (enforce-guard g)
        )
    )

    (defun enforce-crosschain:bool
        ( token:object{token-info}
          sender:string
          guard:guard
          receiver:string
          target-chain:string
          amount:decimal )
        (enforce-ledger)
        (enforce false "Transfer across chains prohibited")
    )

    (defun initialize-collection (
        new-collection:object
        tokens-list:list
        fungible:module{fungible-v2}
        )
        (with-capability (GOVERNANCE)
        (write collection-info (at 'id new-collection) {
          "id": (at 'id new-collection),
          "royalty-info": (at 'royalty-info new-collection),
          "collection-size": (at 'collection-size new-collection),
          "provenance-hash": (at 'provenance-hash new-collection),
          "tokens": tokens-list,
          "creator-account": (at 'creator-account new-collection),
          "creator-guard": (at 'creator-guard new-collection),
          "price": (at 'price new-collection),
          "fungible": fungible
        }))
    )

    (defcap QUOTE:bool
        ( sale-id:string
          token-id:string
          amount:decimal
          price:decimal
          sale-price:decimal
          spec:object{quote-spec}
        )
        @doc "For event emission purposes"
        @event
        true
    )

    (defconst QUOTE-MSG-KEY "quote"
        @doc "Payload field for quote spec")

    (defschema quote-spec
        @doc "Quote data to include in payload"
        fungible:module{fungible-v2}
        price:decimal
        recipient:string
        recipient-guard:guard
    )

    (defschema quote-schema
        id:string
        spec:object{quote-spec}
    )

    (deftable quotes:{quote-schema})

    (defun enforce-ledger:bool ()
        (enforce-guard (marmalade.ledger.ledger-guard))
    )

    (defun enforce-mint:bool
        ( token:object{token-info}
          account:string
          guard:guard
          amount:decimal
        )
        (enforce-ledger)
        (enforce (= 1.0 amount) "Invalid mint amount")
        (with-read kmc-tokens (at 'id token) 
            { 'supply := supply
            , 'number := token-number }
            (if (<= 20000 token-number) ;TODO, somehow enforce that the legendary miner has been 
                (enforce (read-burned (get-token-nft-id2 (int-to-str 10 (- token-number 10000)))) 
                    (format "Miner {} was not immersed in fluid and is not eligible for upgrade" [(- token-number 10000)]))
                (enforce (read-burned (get-token-nft-id2 (int-to-str 10 token-number))) 
                    (format "Miner {} was not immersed in fluid and is not eligible for upgrade" [(- token-number 10000)])))
            (enforce (= supply 0.0) "Token has been minted")
            (let 
                (
                    (number (get-count MINT_COUNT))
                )
                (update counts-table MINT_COUNT {"count": (+ 1 number)})
                (update kmc-tokens (at 'id token) ;TODO update the number to account for legendary
                    { "supply": 1.0
                    , "number": (- 10000 token-number) } ; if a legendary vial was used, this number should be different
                )
            )
        )
        false
    )

    (defun enforce-burn:bool
        ( token:object{token-info}
          account:string
          amount:decimal
        )
        (enforce-ledger)
        (enforce (= 1.0 amount) "Invalid burn amount")
        (with-read kmc-tokens (at 'id token) 
            { 'supply:= supply }
            (enforce (= supply 1.0) "Token has been burned already")
        )
        (update kmc-tokens (at 'id token) 
            {"supply": 0.0
            ,"owner":"null"}
        )
        true
    )

    (defun register-guard (g)
        (write guard-storage-table "kmc" {'g:g})
    )

    (defun emit-buy:bool (nft-id:string buyer:string seller:string price:decimal policy:string)
        @doc "emits the buy event from another contract"
        (enforce-kmc)
        (emit-event (BUY_KMC nft-id buyer seller price policy))
    )

    (defcap BUY_KMC:bool (nft-id:string buyer:string seller:string price:decimal policy:string)
        @doc "Emitted when an NFT is sold"
        @event true
    )

    (defun enforce-init:bool
        ( token:object{token-info}
        )
        (enforce-ledger)
        (with-capability (ACCOUNT_GUARD MINT_ADDRESS)
            (let 
                (
                    (number (get-count NFT_COUNT))
                )
                (update counts-table NFT_COUNT {"count": (+ 1 number)})
                (insert kmc-tokens (at 'id token)
                  { "id": (at 'id token)
                  , "number": (+ 1 number)
                  , "owner": "null"
                  , "supply": 0.0
                  })
                (insert miner-status-table (at 'id token)
                    { "staked": false
                    , "for-sale": false
                    , "token-id": (at 'id token) })
                (insert number-to-id-table (int-to-str 10 (+ 1 number) )
                    { "token-id" : (at 'id token) })
            )
            true
        )
    )
)


