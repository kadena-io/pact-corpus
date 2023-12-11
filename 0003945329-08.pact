(module test-marmalade-kmc GOVERNANCE
  @doc "marmalade test contract for kmc"
    (use kip.token-policy-v1 [token-info])
    (implements kip.token-policy-v1)
    (use modify-fixed-quote-policy)

    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst QUOTE-MSG-KEY "quote")

    (defschema token-data
        @doc "The information necessary to mint the token on marmalade"
        precision:integer
        scheme:string 
        data:string
        datum:object 
        policy:module{kip.token-policy-v1}
    )

    (defun create-multiple-marmalade-tokens (t-data:[object])
        @doc "Creates multiple tokens from Objects"
        (map (create-marmalade-token) t-data)
    )

    (defun create-marmalade-token:string 
          (manifest:object token-id:string
          )
        @doc "Creates the token on marmalade using the supplied data"
        ; (with-capability (ADMIN)
        ; (let
        ;   (
            ;   (token-id (at "token-id" t-data))
            ;   (manifest (at "manifest" t-data))
        ;     (precision (at "precision" t-data))
        ;     (scheme (at "scheme" t-data))
        ;     (data (at "data" t-data))
        ;     (datum (at "datum" t-data))
        ;     (policy (at "policy" t-data))
        ;   )
        ;   (let*
        ;     (
        ;       (uri (kip.token-manifest.uri scheme data))
        ;       (datum-complete (kip.token-manifest.create-datum uri datum))
        ;       (manifest (kip.token-manifest.create-manifest uri [datum-complete]))
        ;       (token-id (format "t:{}" [(at 'hash manifest)]))
        ;     )
            (modify-fixed-quote-policy.initialize-tokens-list token-id "KMC-test")
            (marmalade.ledger.create-token 
              token-id
              0 
              manifest
              free.modify-fixed-quote-policy
            )
            (mint token-id ADMIN_ADDRESS (read-keyset 'mint-guard) 1.0)
            (format "Minted NFT id {}" [token-id])
        ;   )
        ; );)
    )
    
    (defun create-offchain-multiple (array:[object])
        (map (offchain-token-id-and-manifest) array)    
    )

    (defun offchain-token-id-and-manifest:object (t-data:object)
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
            {"manifest": manifest, "token-id": token-id}
            ; (format "{\"manifest\": {},\"token-id\": \"{}\"}" [manifest token-id])
          )
        )
    )
    
    (defun create-token-id-from-manifest (ids:[string]) 
        (let*
            ( 
                (get-manifest (lambda (muppet:string) (kip.token-manifest.create-manifest (kip.token-manifest.uri "text" muppet) [])))
                (manifests:list (map get-manifest ids))
                (tokens:list (map (marmalade.ledger.create-token-id) manifests))
            )
           tokens
        )
    )

    (defun hash-test (formatted-ids:[string])
        (hash formatted-ids)
    )

    (defun mint ( token-id:string current-owner:string current-owner-guard:guard amount:decimal)
        (let 
            ( 
              (exists (try false (let ((ok true)) (marmalade.ledger.get-balance token-id current-owner) ok)))
            )
            (if (= exists false) 
                (marmalade.ledger.create-account token-id current-owner current-owner-guard) "")
            (format "exists = {} " [exists])
        )
        (install-capability (marmalade.ledger.MINT token-id current-owner 1.0))
        (marmalade.ledger.mint token-id current-owner current-owner-guard amount)
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

; (create-table collections)
; (create-table quotes)
; (create-table policies)



