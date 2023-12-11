(module kapepe-nft-test GOVERNANCE
  
  (implements kip.token-policy-v1) 

  (use coin)
  (use marmalade.ledger)
  (use kip.token-policy-v1 [token-info])
  (use kip.token-manifest)

  ; Constants

  (defconst COLLECTION_SUPPLY 1000)
  (defconst ITEM_PRICE 10.0)
  (defconst ADMIN_ADDRESS "k:1604706e7486b5abc1c5391a2f83c3d636a4668eb08572a0777286323a2e4c48")
  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  ; Capabilities

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_5a7ccd559b245b7dcbd5259e1ee43d04fbf93eab.kapepe-admin-keyset")))

  (defun kapepe-orchestrator-guard:guard ()
    @doc "orchestrator module guard for policy to be able to validate access to collection information."
    (create-module-guard "kapepe-orchestrator-guard")
  )

  (defun enforce-orchestrator:bool ()
     (enforce-guard (kapepe-orchestrator-guard))
   )

    (defun enforce-ledger:bool ()
        (enforce-guard (marmalade.ledger.ledger-guard))
    )

  (defcap ACCOUNT-GUARD(account:string)
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard
            (at "guard" (coin.details account))
        )
  )

  (defcap CREATE-NFT ()
    @doc "Private capability for creating an nft."
    true
  )

  (defcap UPDATE-COLLECTION ()
    @doc "Private capability for updating collection information."
    true
  )

  (defcap CREATE-NFTS-BULK()
    @doc "Capability to wrap all needed caps for minting"
    @event
    (compose-capability (CREATE-NFT))
    (compose-capability (UPDATE-COLLECTION))

  )

  (defcap MINT-BULK(new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    (compose-capability (RANDOM new-owner))
    (compose-capability (GET-TOKENS))
    (compose-capability (MINT-INTERNAL))
  )

  (defcap MINT(token-id:string new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    (compose-capability (UPDATE-OWNER token-id new-owner))
    (compose-capability (ACCOUNT-GUARD new-owner))
  )

  (defcap MINT-INTERNAL()
    @doc "Capability to wrap all needed caps for minting"
    true
  )
 
  (defcap UPDATE-OWNER (token-id:string new-owner:string)
    @doc "Private Capability to update owner of token"
    true)

  (defcap RANDOM (account:string)
    @doc "Private capability."
    true
  )

  (defcap GET-TOKENS ()
    @doc "Private capability."
    true
  )

  (defschema counter-schema
    @doc "schema for integer constants such as TOTAL-MINTED-KEY"
    count:integer
  )

  (deftable counter:{counter-schema})

  (defschema nfts-schema
    owner:string
    token-id:string
    minted:bool
  )

  (deftable nfts:{nfts-schema})

    (defschema quote-spec
        @doc "Quote data to include in payload"
        fungible:module{fungible-v2}
        price:decimal
        recipient:string
        recipient-guard:guard
    )

    (defschema quote-schema
        id:string
        spec:object{quote-spec})

    (deftable quotes:{quote-schema})

  ; Creating

    (defun create-nfts-bulk (items:list policy:module{kip.token-policy-v1})
      @doc "Bulk creates marmalade tokens for any collection."
        (with-capability (GOVERNANCE)
            (with-capability (CREATE-NFTS-BULK)
                (map (lambda (item) (create-nft-token item policy)) items)
            )
        )
    )
    
    (defun create-nft-token (datum:object policy:module{kip.token-policy-v1})
      @doc "Creates a manifest then builds a marmalade token using supplied policy"
      (require-capability (GOVERNANCE))
      (let*
        (
          (datum-uri (at 'uri datum))
          (datum-uri-data:string (at 'data datum-uri))
          (datum-uri-scheme:string (at 'scheme datum-uri))
          (nft-datum:object{mf-datum} (create-datum (uri datum-uri-scheme datum-uri-data) datum))
          (manifest:object{manifest} (create-manifest (uri (at 'scheme datum-uri) (at 'data datum-uri)) [nft-datum]))
          (token-id-suffix (at 'edition datum))
          (token-id (+ "kapepe-nft-gen-1-test" token-id-suffix))
        )
        (marmalade.ledger.create-token token-id 0 manifest policy)
        token-id
        )
    )

  ; Minting
  
    (defun mint-bulk (
        amount:integer
        owner:string
        guard:guard)
    (with-capability (GOVERNANCE)
        (with-capability (MINT-BULK owner)
          (let*
              (
              (random (get-random owner))
              (created-tokenList (select nfts ["token-id"] (where 'minted (= false))))
              (token-ids-to-mint:list (get-token-ids amount owner random created-tokenList))
              (created-tokenList-length (length created-tokenList))
              )
              (map (mint owner guard created-tokenList) token-ids-to-mint)
          )
        ))
      )

  (defun mint(owner:string guard:guard created-tokenList:list token-id:string)
    (require-capability (MINT-INTERNAL))
    (install-capability (marmalade.ledger.MINT token-id owner 1.0))
    (marmalade.ledger.mint token-id owner guard 1.0)
    (update nfts token-id { "minted": true })
  )

  ;; @Author : Amr Al Jundi
  ;; This was a genius gas optimization by Amr to go from o(n) best
  ;; to o(m*n) worse case
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

  (defun get-token-ids (amount:integer owner:string random:integer created-tokenList:list)
    @doc "returns a list of randomly selected token ids matching amount"
    ; (require-capability (GET-TOKENS))
    (let*
      (
        (randoms (get-random owner))
      )
      (get-token-ids-from-index random amount created-tokenList)
    )
  )

  (defun get-token-ids-from-index (start-index:integer amount:integer candidate-tokens:list)
      ;; ending at start-index + amount
      ;; all indicies = enemurate start - end +1
      ;; t length = max - minted (can just be length non-minted)
      ;; m-indicies = indicies to mint : take each index and do mod.
      ;; f-index = function to get token from index
    ;   (require-capability (GET-TOKENS))
      (let*
        (
          (end-index (+ start-index (- amount 1)))
          (indicies:list (get-indicies start-index end-index))
          (t-length (length candidate-tokens))
          (f-get (lambda (t-length x) (mod x t-length)))
          (m-indicies:list (map (f-get t-length) indicies))
          (f-index (lambda (x) (at x candidate-tokens)))
          (tokens (map f-index m-indicies))
        )
        (map (lambda (token) (at "token-id" token)) tokens)
      )
  )

  (defun get-indicies (start:integer end:integer)
    (enumerate start end)
  )

  ; Enforcements
  
    (defun enforce-mint:bool
        ( token:object{token-info}
          account:string
          guard:guard
          amount:decimal
        )

        (with-capability (MINT-INTERNAL)
          (enforce-orchestrator)
          (enforce-ledger)
        ;   (enforce (= false (at "minted" (read nfts (at 'id token))) "NFT minted already"))
          (write counter "nfts-minted" {"count": (+ 1 (get-count "nfts-minted"))})
          (bind (get-token token)
             { 'owner:=owner:string
             }
             (enforce (= (+ amount (at 'supply token)) 1.0) "Exceeds max supply")
             (update-owner (at 'id token) account)
          )
          (if
              (= account ADMIN_ADDRESS)
              true
              (coin.transfer account ADMIN_ADDRESS ITEM_PRICE)
          ))
    )

    (defun enforce-burn:bool
        ( token:object{token-info}
          account:string
          amount:decimal )
        (enforce-orchestrator)
        (enforce-ledger)
        (enforce false "Burn prohibited")
    )

    (defun enforce-init:bool
        ( token:object{token-info}
        )
        (enforce-orchestrator)
        (enforce-ledger)
        (write counter "nfts-created" {"count": (+ 1 (get-count "nfts-created"))})
        (insert nfts (at 'id token)
          { "token-id": (at 'id token)
          , "owner": ""
          , "minted": false
        })
    )

    (defun enforce-offer:bool
        ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
        @doc "Capture quote spec for SALE of TOKEN from message"
        (enforce-orchestrator)
        (enforce-ledger)
        (enforce-minted)
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
          (insert quotes sale-id { 'id: (at 'id token), 'spec: spec }))
          true
    )

    (defun enforce-buy:bool
        ( token:object{token-info}
          seller:string
          buyer:string
          buyer-guard:guard
          amount:decimal
          sale-id:string )
        (enforce-orchestrator)
        (enforce-ledger)
        (enforce-minted)
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

    (defun enforce-transfer:bool
        ( token:object{token-info}
          sender:string
          guard:guard
          receiver:string
          amount:decimal )
        (enforce-orchestrator)
        (enforce-ledger)
        (enforce-minted)
        (update tokens (at 'id token)
            { "owner" : receiver }
        )
        true
    )


    (defun enforce-crosschain:bool
        ( token:object{token-info}
          sender:string
          guard:guard
          receiver:string
          target-chain:string
          amount:decimal )
        (enforce-orchestrator)
        (enforce-ledger)
        (enforce false "Transfer across chains prohibited")
    )

   (defun enforce-sale-pact:bool (sale:string)
        "Enforces that SALE is id for currently executing pact"
        (enforce (= sale (pact-id)) "Invalid pact/sale id")
    )

   (defun enforce-minted:bool (sale:string)
        "Enforces that SALE is id for currently executing pact"
        (enforce (= true (at "minted" (read nfts)) "NFT not minted"))
    )

  ; Setters
  
  (defun update-owner (token-id:string new-owner:string)
    @doc "Updates token with new owner"
    (require-capability (UPDATE-OWNER token-id new-owner))
      (update tokens token-id
        {'owner: new-owner}
      )
  )

  ; Getters
  
    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counter key ['count]))
    )
  
  (defun get-token:object{token-schema} (token:object{token-info})
    (read tokens (at 'id token))
  )

  (defun get-nft-details (token-id:string)
    (marmalade.ledger.get-manifest token-id)
  )

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

)

; (mint-bulk 1
;   "k:b8b74b220522884b7b02481a823034a2dc03b170f8931f4e7b90940472188ad2"
;   (read-keyset "kapepe-test-keyset"))



