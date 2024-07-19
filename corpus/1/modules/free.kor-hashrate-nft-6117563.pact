(module kor-hashrate-nft GOV

  ;; -------------------------------
  ;; Governance and Permissions

  (defconst GOV_GUARD:string "gov")
  (defconst OPS_GUARD:string "ops")

  (defcap GOV ()
    (enforce-guard (at "guard" (read m-guards GOV_GUARD ["guard"])))
  )

  (defcap OPS ()
    (enforce-guard (at "guard" (read m-guards OPS_GUARD ["guard"])))
    (compose-capability (MINT))
  )

  (defschema m-guard ;; ID is a const: OPS_GUARD, GOV_GUARD etc.
    @doc "Stores guards for the module"
    guard:guard  
  )
  (deftable m-guards:{m-guard})

  (defun rotate-ops:string (guard:guard)
    @doc "Requires GOV. Changes the ops guard to the provided one."

    (with-capability (OPS)
      (update m-guards OPS_GUARD
        { "guard": guard }  
      )

      "Rotated OPS to a new guard"
    )
  )

  (defun rotate-gov:string (guard:guard)
    @doc "Requires GOV. Changes the gov guard to the provided one."

    (with-capability (GOV)
      (update m-guards GOV_GUARD
        { "guard": guard }  
      )

      "Rotated GOV to a new guard"
    )
  )

  (defun init-perms:string (gov:guard ops:guard)
    @doc "Initializes the guards and creates the tables for the module"

    ;; This is only vulnerable if GOV_GUARD doesn't exist
    ;; Which means it's only vulnerable if you don't call 
    ;; init when you deploy the contract.
    ;; So let us be sure that init is called. =)
    (insert m-guards GOV_GUARD
      { "guard": gov }  
    )
    (insert m-guards OPS_GUARD
      { "guard": ops }  
    )
  )

  ;; -------------------------------
  ;; String Values

  (defschema value
    @doc "Stores string values"
    value:string
  )
  (deftable values:{value})

  (defun update-string-value (val-id:string account:string)
    @doc "Updates the account for the bank"

    (with-capability (OPS)
      (write values val-id
        { "value": account }
      )
    )
  )

  (defun get-string-value:string (val-id:string)
    @doc "Gets the value with the provided id"

    (at "value" (read values val-id ["value"]))
  )

  ;; -------------------------------
  ;; Counter

  (defcap INCREMENT ()
    @doc "Private capability for incrementing the counter"
    true
  )

  (defschema counter
    @doc "Stores counters"
    counter:integer
  )
  (deftable counters:{counter})

  (defun increment-counter:integer (counter-id:string)
    @doc "Increments the given counter and returns the new count"

    (require-capability (INCREMENT))

    (with-read counters counter-id
      { "counter" := count }
      (let
        (
          (increment (+ count 1))
        )
        (update counters counter-id
          { "counter": increment }  
        )

        increment
      )
    )
  )

  (defun get-counter:integer (counter-id:string)
    (at "counter" (read counters counter-id ["counter"]))
  )

  (defun init-counter:string (counter-id:string)
    @doc "Initializes the guards and creates the tables for the module"

    (insert counters counter-id
      { "counter": 0 }  
    )
  )

  ;; -------------------------------
  ;; Constants

  (defconst BANK_ACCOUNT:string "BANK_ACCOUNT")
  (defconst HASHRATE_NFT_COUNTER:string "HASHRATE_NFT_COUNTER")
  (defconst OG_BADGE_NFT_COUNTER:string "OG_BADGE_NFT_COUNTER")

  ;; -------------------------------
  ;; Hashrate NFT Data

  (defconst TIER_DATA_KEY:string "TIER_DATA")

  (defschema tier 
    @doc "The info for a Tier"
    rarity:string 
    min-hash-rate:decimal
    cost-per-th-usd:decimal
  )
  (defschema tier-data
    tiers:[object{tier}]
  )
  (deftable tiers:{tier-data})

  (defun write-tiers:string (tier-l:[object{tier}])
    @doc "Writes or overwrites the current tier list"
    (with-capability (OPS)
      (write tiers TIER_DATA_KEY
        { "tiers": tier-l }
      )

      (format "Tiers updated to: {}" [tier-l])
    )
  )

  (defun get-tiers:[object{tier}] ()
    (at "tiers" (read tiers TIER_DATA_KEY ["tiers"]))
  )

  (defun get-tier:object{tier} (hash-rate:decimal)
    @doc "Get's the tier based on the provided hash rate"

    (enforce (> hash-rate 0.0) "Hash rate must be greater than 0")
    
    (let* 
      (
        (in-bounds ;; Lambda function to determine if hash rate could in in tier
          (lambda (t) 
            (<= (at "min-hash-rate" t) hash-rate)
          )
        )
        (li ;; Sort and filter based on in bounds
          (sort ["min-hash-rate"] (filter (in-bounds) (get-tiers))))
      )

      ;; Return the last element in the list, based on sort
      (at 
        (- (length li) 1)
        li
      )
    )
  )

  (defun get-price-per-th-usd:decimal (hash-rate:decimal)
    (round (at "cost-per-th-usd" (get-tier hash-rate)) (coin.precision))
  )

  (defun get-price-per-th-kda:decimal (hash-rate:decimal)
    (let
      (
        (price-usd (get-price-per-th-usd hash-rate))
        (kda-usd (free.kor-oracle.get-price-for-price-tracker "kda" "usd"))
      )
      (round (/ price-usd kda-usd) (coin.precision))
    )
  )

  (defun get-total-price-usd:decimal (hash-rate:decimal)
    (round (* hash-rate (at "cost-per-th-usd" (get-tier hash-rate))) (coin.precision))
  )

  (defun get-total-price-kda:decimal (hash-rate:decimal)
    (let
      (
        (price-usd (get-total-price-usd hash-rate))
        (kda-usd (free.kor-oracle.get-price-for-price-tracker "kda" "usd"))
      )
      (round (/ price-usd kda-usd) (coin.precision))
    )
  )

  ;; -------------------------------
  ;; Hashrate NFTs

  (defcap MINT ()
    (compose-capability (INCREMENT))
  )

  (defschema hashrate-nft
    @doc "Stores the owner information for each nft, nft-id is the key"
    nft-id:integer
    nft-uuid:string
    account:string
    hash-rate:decimal
    rarity:string
    cost-kda:decimal
    created-date:time
  )
  (deftable hashrate-nfts:{hashrate-nft})

  (defun mint-hashrate-nft:string
    (
      account:string 
      hash-rate:decimal
    )
    @doc "Mints an NFT with the given hash rate.\
    \ Automatically calculates the cost of an NFT in KDA and charges that much \
    \ from the account. Account is owner of created NFT."
    
    (with-capability (MINT)
      (let
        (
          (price-kda:decimal (get-total-price-kda hash-rate))
        )

        ;; Transfer KDA
        (coin.transfer account (get-string-value BANK_ACCOUNT) price-kda)

        ;; Create NFT
        (create-hashrate-nft account hash-rate price-kda "")
      )
    )
  )

  (defun admin-mint-hashrate-nft:string
    (
      account:string 
      hash-rate:decimal
      cost-kda:decimal
      nft-uuid:string
    )
    @doc "Admin function to create a hashrate NFT owned by the given account"
    (enforce (> hash-rate 0.0) "Hash rate must be greater than 0")
    (enforce (> cost-kda 0.0) "KDA cost must be greater than 0")
    
    (with-capability (OPS)
      (create-hashrate-nft account hash-rate cost-kda nft-uuid)
    )
  )

  (defun create-hashrate-nft
    (
      account:string 
      hash-rate:decimal
      cost-kda:decimal
      nft-uuid:string
    )
    @doc "Private function to create an NFT."

    (require-capability (MINT))

    (let
      (
        (nft-id (increment-counter HASHRATE_NFT_COUNTER)) 
        (rarity (at "rarity" (get-tier hash-rate)))
      )
      (insert hashrate-nfts (int-to-str 10 nft-id)
        { "nft-id": nft-id
        , "nft-uuid": nft-uuid
        , "account": account
        , "hash-rate": hash-rate
        , "rarity": rarity
        , "cost-kda": cost-kda
        , "created-date": (curr-time)
        }
      )
    )
  )

  (defun get-all-nfts:[object{hashrate-nft}] ()
    (select hashrate-nfts (constantly true))
  )

  (defun get-owned-nfts:[object{hashrate-nft}] (account:string)
    (select hashrate-nfts (where "account" (= account)))
  )

  (defun get-nft-with-id:object{hashrate-nft} (nft-id:string)
    (read hashrate-nfts nft-id)
  )

  (defun get-bank:string ()
    (get-string-value BANK_ACCOUNT)
  )

  ;; -------------------------------
  ;; OG Badge

  (defschema og-badge
    @doc "The table for og badges, ID is the owner, only one per account"
    account:string 
    nft-id:integer
    og-badge:string
    phone-encrypted:string
  )
  (deftable og-badges:{og-badge})

  (defun admin-mint-og-badge:string
    (
      account:string 
      og-badge:string
      phone-encrypted:string
    )
    @doc "Admin function to create an OG Badge NFT owned by the given account"
    (enforce (!= account "") "Account cannot be blank")
    
    (with-capability (OPS)
      (let
        (
          (nft-id:integer (increment-counter OG_BADGE_NFT_COUNTER)) 
        )
        (insert og-badges (int-to-str 10 nft-id)
          { "nft-id": nft-id
          , "account": account
          , "og-badge": og-badge
          , "phone-encrypted": phone-encrypted
          }
        )
      )
    )
  )

  (defun get-all-og-badges:[object{og-badge}] ()
    (select og-badges (constantly true))
  )

  (defun get-owned-og-badges:[object{og-badge}] (account:string)
    (select og-badges (where "account" (= account)))
  )

  (defun update-kyc (nft-id:string phone-encrypted:string)
    (with-capability (OPS)
      (update og-badges nft-id 
        { "phone-encrypted": phone-encrypted
        }
      )
    )
  )

  (defun update-og-badge (nft-id:string og-badge:string)
    (with-capability (OPS)
      (update og-badges nft-id 
        { "og-badge": og-badge
        }
      )
    )
  )

  ;; -------------------------------
  ;; Utils

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"

    (at 'block-time (chain-data))
  )
)


