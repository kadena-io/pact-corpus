(module kor-og-badge GOV

  ;; -------------------------------
  ;; Governance and Permissions

  (defconst GOV_GUARD:string "gov")
  (defconst OPS_GUARD:string "ops")

  (defcap GOV ()
    (enforce-guard (at "guard" (read m-guards GOV_GUARD ["guard"])))
  )

  (defcap OPS ()
    (enforce-guard (at "guard" (read m-guards OPS_GUARD ["guard"])))
    (compose-capability (INCREMENT))
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

  (defconst OG_BADGE_NFT_COUNTER:string "OG_BADGE_NFT_COUNTER")

  ;; -------------------------------
  ;; OG Badge

  (defschema og-badge
    @doc "The table for og badges, ID is the owner, only one per account"
    account:string 
    nft-id:integer
    og-badge:string
    phone-encrypted:string
    created-date:time
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
          , "created-date": (curr-time)
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


