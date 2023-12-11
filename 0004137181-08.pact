(module lp-orchestrator GOVERNANCE

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
    (enforce-guard (keyset-ref-guard "free.lp-admin")))
  (defcap MINT(token-id:string new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    true
  )
  (defcap MINT-BULK(new-owner:string)
    @doc "Capability to wrap all needed caps for minting"
    (compose-capability (RANDOM new-owner))
    (compose-capability (GET-TOKENS))

  )
  (defcap RANDOM (account:string)
    @doc "Private capability."
    true
  )
  (defcap GET-TOKENS ()
    @doc "Private capability."
    true
  )

  (defun atrium-orchestrator-guard:guard ()
    @doc "orchestrator module guard for policy to be able to validate access to collection information."
    (create-module-guard "atrium-orchestrator-guard")
  )
  (defschema collection
    id:string
    collection-guard:guard
    non-minted-tokens:[string]
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
            (enforce-guard creator-guard)
            ;;TODO : Move collection-id to pull from make of manifest
            (insert collections collection-identifier
                {'id:collection-identifier
                ,'collection-guard:creator-guard
                ,'non-minted-tokens:[]
                ,'minted-total:0
                ,'current-unique-supply:0
                ,'max-unique-supply:collection-max-unique-supply
                ,'policy:policy})
            )
            true
    )
  )

  (defun create-nfts-bulk:list ()
    @doc "Bulk creates marmalade tokens for any collection. Formatted in specs : [{nft-data : {}, uri-data : "", uri-scheme: ""}]"
    (let*
      (
        (collection-id (read-msg "collection-id"))
        (specs:[object] (read-msg "specs"))
        (collection (get-collection collection-id))
        (token-ids (map (create-nft-token collection collection-id) specs))
      )
      (update-non-minted-collection token-ids collection-id collection)
      token-ids
    )
  )
  (defun create-nft-token:string(collection:object{collection} collection-id:string obj:object)
    @doc "Creates a manifest then builds a marmalade token using supplied policy"
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
  (defun update-non-minted-collection (token-ids:list collection-id:string collection:object{collection} )
      ;;  (require-capability (UPDATE-COLLECTION))
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
          (token-ids-to-mint:list (get-token-ids amount collection-id owner random))
          (collection (get-collection collection-id))
          (created-tokenList (at 'non-minted-tokens collection))
          (created-tokenList-length (length created-tokenList))
          (total-to-mint (length token-ids-to-mint))
          (minted-total (at 'minted-total collection))
          (new-created-token-list (subs-wrapper created-tokenList random amount))
          )
          ;;TODO: Can update filter to just take and drop around index start and end if more gas effecient
          (enforce (<= (+ total-to-mint minted-total) created-tokenList-length) "Not enough tokens to mint!")
          (update collections collection-id
                {'non-minted-tokens: new-created-token-list,
                 'minted-total: (+ total-to-mint minted-total)})
          (map (mint owner guard) token-ids-to-mint)
      )
    )
  )

  (defun mint-bulk-gas-filter-test (
    amount:integer
    collection-id:string
    owner:string
    guard:guard)

    (with-capability (MINT-BULK owner)
      (let*
          (
          (random (get-random owner))
          (token-ids-to-mint:list (get-token-ids amount collection-id owner random))
          (collection (get-collection collection-id))
          (created-tokenList (at 'non-minted-tokens collection))
          (created-tokenList-length (length created-tokenList))
          (total-to-mint (length token-ids-to-mint))
          (minted-total (at 'minted-total collection))
          )
          ;;TODO: Can update filter to just take and drop around index start and end if more gas effecient
          (enforce (<= (+ total-to-mint minted-total) created-tokenList-length) "Not enough tokens to mint!")
          (update collections collection-id
                {'non-minted-tokens: (filter (not-in-list token-ids-to-mint) created-tokenList),
                 'minted-total: (+ total-to-mint minted-total)})
          (map (mint owner guard) token-ids-to-mint)
      )
    )
  )

  ;; @Author : Amr Al Jundi
  ;; This was a genius gas optimization by Amr to go from o(n) best
  ;; to o(n) worse case
  (defun subs-wrapper:list(l:list random:integer amount:integer)
      (let*
          (
              (n (length l))
          )
          (if (>= amount n) [] (subs l random amount n))
      )
  )
  ;; @Author : Amr Al Jundi
  ;; This was a genius gas optimization by Amr to go from o(n) best
  ;; to o(n) worse case
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


  (defun get-token-ids (amount:integer collection-id:string owner:string random:integer)
    @doc "returns a list of randomly selected token ids matching amount"
    (require-capability (GET-TOKENS))
    (let*
      (
        (randoms (get-random owner))
      )
      (get-token-ids-from-index random amount collection-id)
    )
  )

  (defun get-token-ids-from-index (start-index:integer amount:integer collection-id:string)
      ;; ending at start-index + amount
      ;; all indicies = enemurate start - end +1
      ;; t length = max - minted (can just be length non-minted)
      ;; m-indicies = indicies to mint : take each index and do mod.
      ;; f-index = function to get token from index
      (require-capability (GET-TOKENS))
      (let*
        (
          (collection (get-collection collection-id))
          (non-minted-tokens (at 'non-minted-tokens collection))
          (end-index (+ start-index (- amount 1)))
          (indicies:list (get-indicies start-index end-index))
          (t-length (length non-minted-tokens))
          (f-get (lambda (t-length x) (mod x t-length)))
          (m-indicies:list (map (f-get t-length) indicies))
          (f-index (lambda (x) (at x non-minted-tokens)))
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

  (defun get-collection (col-id:string)
    (read collections col-id)
  )

  (defun get-non-minted-tokens (col-id:string)
    (at 'non-minted-tokens (read collections col-id))
  )
  (defun get-minted-total-for-collection (col-id:string)
    (at 'minted-total (read collections col-id))
  )

  (defun test-get()
    (format "got test" [])
  )

)

