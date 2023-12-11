(module kapepe-nft-gen-0 GOVERNANCE
  
  (use coin)
  (use marmalade.ledger)
  (use kapepe-nft-gen-0-policy)
  (use kip.token-manifest)

  (bless "SbcVktEs9Maf4y5eUwGhQ8vrjIseaP3YtLxZ-dCn-Lc")

  ; Capabilities

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_5a7ccd559b245b7dcbd5259e1ee43d04fbf93eab.kapepe-admin-keyset")))


  (defcap ACCOUNT_GUARD(account:string) ; Used for admin functions
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard   
            (at "guard" (coin.details account))
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

  (defcap CREATE-NFTS-BULK()
    @doc "Capability to wrap all needed caps for minting"
    @event
    (compose-capability (CREATE-NFT))
  )

  (defcap CREATE-NFT ()
    @doc "Private capability for creating an nft."
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
          (token-id (+ "kapepe-nft-gen-0-" token-id-suffix))
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
        (with-capability (MINT-BULK owner)
          (let*
              (
              (random (get-random owner))
              (non-minted-tokens (n_5a7ccd559b245b7dcbd5259e1ee43d04fbf93eab.kapepe-test-policy-1.get-non-minted))
              (token-ids-to-mint:list (get-token-ids amount owner random non-minted-tokens))
              )
              (map (mint owner guard) token-ids-to-mint)
          )
        )
      )

  (defun mint(owner:string guard:guard token-id:string)
    (require-capability (MINT-INTERNAL))
    (install-capability (marmalade.ledger.MINT token-id owner 1.0))
    (marmalade.ledger.mint token-id owner guard 1.0)
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
    (require-capability (GET-TOKENS))
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
      (require-capability (GET-TOKENS))
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
