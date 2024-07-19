(module doc-nft-mint GOVERNANCE

  @doc "Module for minting the Docushield/Mintit tokens"

  (defcap GOVERNANCE ()
    (enforce-guard "free.doc-nft-mint-admin"))

  ; Constants

  (defconst DOC_ACCOUNT "k:4159aa0d1f1e6c119c532d9286746274c3cc46dadd50ffc486a38de502ad6855")

  (defconst TOKEN_POLICY mintit-presale-policy)

  (defschema nft 
    nft-name:string
    nft-manifest:object
    token-supply:decimal
    token-id:string
    escrow-account:string
    mint-price:decimal
    mint-price-wl:decimal
    limit:integer
    guard:guard
    wl:list
    wl-limit:integer
    public-mint-at:time
  )

  (deftable nfts:{nft})

  ; Create and mint token on marmalade
  (defun init-nft:bool (
    nft-name:string
    nft-manifest:object
    guard:guard
    token-supply:decimal
    mint-price:decimal
    mint-price-wl:decimal
    wl:list
    wl-limit:integer
    limit:integer
    public-mint-at:time
  )
    (enforce (>= (length nft-name) 8) "NFT name should be >= 8 characters long")
    (enforce (>= mint-price 0.0) "Mint price should not be negative")
    (enforce (>= mint-price 0.0) "WL Mint price should not be negative")
    (enforce (>= mint-price mint-price-wl) "Whitelist mint price should not be greater than regular")
    (enforce (>= token-supply 1.0) "Token supply should be >= 1")
    (enforce (<= token-supply 10000.0) "Token supply should be <= 10000")
    (enforce (<= (length wl) 1000) "Whitelist should be less than 1000 addresses")
    (enforce (>= wl-limit 0) "Whitelist limit should not be negative")
    (enforce (>= limit 0) "Regular limit should not be negative")
    (enforce-guard guard)

    (let
      (
        (nft-guard (nft-guard nft-name))
        (account-name (nft-account-name nft-name))
        (token-id (get-token-id nft-name))
      )

      (marmalade.ledger.create-token token-id 0 nft-manifest TOKEN_POLICY)
      (marmalade.ledger.create-account token-id account-name nft-guard)
      (marmalade.ledger.mint token-id account-name nft-guard token-supply)

      ; Create the stakable nft recorddef
      (insert nfts nft-name
        {
            "nft-name": nft-name
          , "nft-manifest": nft-manifest
          , "token-supply": token-supply
          , "token-id": token-id
          , "escrow-account": account-name
          , "mint-price": mint-price
          , "mint-price-wl": mint-price-wl
          , "wl": wl
          , "limit": limit
          , "wl-limit": wl-limit
          , "public-mint-at": public-mint-at
          , "guard": guard
        }
      )
    )
  )

  (defun get-nft:object (nft-name:string)
    (read nfts nft-name)
  )

  (defun is-wl:bool (nft-name:string account:string)
    (with-read nfts nft-name {"wl" := wl}
      (contains account wl)
    )
  )

  (defun is-wl-active:bool (nft-name:string account:string)
    (bind (chain-data)
      { "block-time" := curr-time }
      (and 
        (is-wl nft-name account)
        
        (with-read nfts nft-name {
          "token-id" := token-id,
          "wl-limit" := wl-limit,
          "public-mint-at" := public-mint-at}
          (and 
            (< (at 'balance (marmalade.ledger.details token-id account)) wl-limit)
            (< curr-time public-mint-at)
          )
        )
      )
    )
  )

  ; User mint token
  (defun mint:bool (
      nft-name:string
      account:string
      guard:guard
    )

    ; Create marmalade account
    (with-read nfts nft-name {
      "token-id" := token-id,
      "escrow-account" := escrow-account
    }
      (let 
        ((mint-price (get-mint-price nft-name account)))
      
        (marmalade.ledger.create-account token-id account guard)

        ; Transfer payment in KDA
        (coin.transfer account DOC_ACCOUNT mint-price)

        ; Transfer token on marmalade
        (install-capability (marmalade.ledger.TRANSFER token-id escrow-account account 1.0))
        (marmalade.ledger.transfer token-id escrow-account account 1.0)
      )
    )
  )

  (defun nft-guard:guard (nft-name:string)
    @doc "Creates a guard that is used for the escrow accounts for the nft"
    (create-module-guard nft-name)
  )

  (defun nft-account-name:string (nft-name:string)
    (create-principal (nft-guard nft-name))
  )

  (defun get-token-id:string (nft-name:string)
    (concat ["t:" nft-name])
  )

  (defun get-mint-price:decimal (nft-name:string account:string)
    (with-read nfts nft-name {
      "mint-price" := mint-price,
      "mint-price-wl" := mint-price-wl
    }
      (if (is-wl-active nft-name account)
        mint-price-wl
        mint-price
      )
    )
  )
)


