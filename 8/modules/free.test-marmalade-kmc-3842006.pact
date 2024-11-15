(module test-marmalade-kmc GOVERNANCE
  @doc "marmalade test contract for kmc"
    (use kip.token-policy-v1 [token-info])
    (implements kip.token-policy-v1)

    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst QUOTE-MSG-KEY "quote")

    (defschema nft-manifest-datum
        name:string
        description:string
        creator:string
        collection-name:string
        content-uri:object{kip.token-manifest.mf-uri}
        edition:integer
        mint-time:time
        mint-index:time
    )

    (defschema nft
        name:string
        description:string
        creator:string
        collection-name:string
        content-uri:object{kip.token-manifest.mf-uri} 
        marmalade-token-id:string
        edition:integer
        mint-index:integer
        mint-time:time
        current-owner:string
        current-owner-guard:guard
        revealed:bool
        minted:bool
    )

    (deftable nft-table:{nft})

    (defschema token-data
        @doc "The information necessary to mint the token on marmalade"
        precision:integer
        scheme:string 
        data:string
        datum:object 
        policy:module{kip.token-policy-v1}
    )

    (defun create-marmalade-token:string 
          (t-data:object{token-data} 
            current-owner:string 
            current-owner-guard:guard
          )
        @doc "Creates the token on marmalade using the supplied data"
        ; (with-capability (ADMIN)
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
            )
            (marmalade.ledger.create-token 
              (concat ["t:" (at "hash" manifest)]) 
              precision 
              manifest
              policy
            )
            ; (marmalade.ledger.create-account (concat ["t:" (at "hash" manifest)]) current-owner current-owner-guard)
            ; (marmalade.ledger.mint (concat ["t:" (at "hash" manifest)])  current-owner current-owner-guard 1.0)
            (format "id is {}" [(concat ["t:" (at "hash" manifest)]) ])
          )
        );)
    )

    (defun mint ( token-id:string current-owner:string current-owner-guard:guard amount:decimal)
        (let 
            ( 
              (exists (try false (let ((ok true)) (marmalade.ledger.get-balance token-id current-owner) ok)))
            )
            ; (if (= exists false) 
            ;     (marmalade.ledger.create-account token-id current-owner current-owner-guard)
            ; )
            (format " {} " [exists])
        )
        (marmalade.ledger.mint token-id current-owner current-owner-guard amount)
    )

    (defun add-to-marmalade:bool ( nft:object{nft} )
        @doc "Takes an nft object and adds it to the marmalade ledger"
        (with-capability (ADMIN)
        (bind nft
            { 'name := name
            , 'description := description
            , 'creator := creator 
            , 'collection-name := collection-name 
            , 'content-uri := content-uri 
            , 'marmalade-token-id := marmalade-token-id 
            , 'edition := edition
            , 'mint-index := mint-index 
            , 'mint-time := mint-time 
            , 'current-owner := current-owner 
            , 'current-owner-guard := current-owner-guard
            }
        (let*
            (
                (datum-object:object{nft-manifest-datum}
                { 'name: name 
                , 'description: description 
                , 'creator: creator
                , 'collection-name: collection-name 
                , 'content-uri: content-uri
                , 'edition: edition
                , 'mint-index: mint-index 
                , 'mint-time: mint-time
                })
              (datum-uri (kip.token-manifest.uri "test-uri:kmc" "marmalade.fixed-quote-policy"))
              (manifest-datum (kip.token-manifest.create-datum datum-uri datum-object)) 
              (manifest-uri content-uri) 
              (nft-manifest (kip.token-manifest.create-manifest manifest-uri [manifest-datum]))
              (token-id marmalade-token-id)
              (token-precision 0)
            )
            (marmalade.ledger.create-token token-id token-precision nft-manifest mintit-policy-v1)
            (marmalade.ledger.create-account token-id current-owner current-owner-guard)
            (marmalade.ledger.mint token-id current-owner current-owner-guard 1.0)
        )))
    )

; ============================================
; ==        MARMALADE REQUIREMENTS          ==
; ============================================

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
          true
      )

    (defun enforce-ledger:bool ()
        (enforce-guard (marmalade.ledger.ledger-guard))
    )

    (defun enforce-sale-pact:bool (sale:string)
        @doc "Enforces that SALE is id for currently executing pact"
        (enforce (= sale (pact-id)) "Invalid pact/sale id")
    )

    (defun enforce-crosschain:bool
        ( token:object{token-info}
          sender:string
          guard:guard
          receiver:string
          target-chain:string
          amount:decimal )
        (enforce-ledger)
        (enforce false "Transfer prohibited")
    )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
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
    true
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let* ( (mint-guard:guard (read-keyset 'mint-guard ))
            (max-supply:decimal (read-decimal 'max-supply ))
            (min-amount:decimal (read-decimal 'min-amount ))
            )
    (enforce (>= min-amount 0.0) "Invalid min-amount")
    (enforce (>= max-supply 0.0) "Invalid max-supply")
    (insert policies (at 'id token)
      { 'mint-guard: mint-guard
      , 'max-supply: max-supply
      , 'min-amount: min-amount })
    true)
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (bind (get-policy token)
      { 'mint-guard:=mint-guard:guard
      , 'min-amount:=min-amount:decimal
      , 'max-supply:=max-supply:decimal
      }
      (enforce-guard mint-guard)
      (enforce (>= amount min-amount) "mint amount < min-amount")
      (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
  ))

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )

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

  (defschema policy-schema
    mint-guard:guard
    max-supply:decimal
    min-amount:decimal
  )

  (deftable policies:{policy-schema})

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
; ============================================
; ==             CAPABILITIES               ==
; ============================================

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defcap PRIVATE ()
        true
    )
) 

; (create-table nft-table)
; (create-table quotes)
; (create-table policies)



