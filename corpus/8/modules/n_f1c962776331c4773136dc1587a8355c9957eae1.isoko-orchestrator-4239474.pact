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


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Capabilities
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")))
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
  (defcap ADMIN-OR-DELEGATE (collection-id:string)
    
    (enforce-one "Any Guard passes" [(enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")) (enforce-guard (keyset-ref-guard "free.isoko-generator"))])
  )
  (defcap ADMIN-OR-COL-OWNER (collection-id:string)
    (let*
      (
          (collection (get-collection collection-id))
          (collection-guard (at 'collection-guard collection))
      )
      (enforce-one "Any Guard passes" [(enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")) (enforce-guard collection-guard)])
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
  (defcap MINT-BULK(new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    (compose-capability (RANDOM new-owner))
    (compose-capability (GET-TOKENS))
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

  (deftable collections:{collection})
  (deftable tokens:{token})

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
        (token-id (+ (+ collection-id " #") token-id-suffix))
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

  (defun backfill (non-minted-tokens:list collection-id:string)
    (with-capability (GOVERNANCE)
        (update collections collection-id
            {'non-minted-tokens: non-minted-tokens}
        )
    )  
  )
  (defun get-collection (col-id:string)
    (read collections col-id)
  )

  (defun get-non-minted-tokens (col-id:string)
    (at 'non-minted-tokens (read collections col-id))
  )

(defun get-reserved-tokens (col-id:string)
    (at 'reserved-tokens (read collections col-id))
  )

)


