(module kor-og-badge GOV

  ;; -------------------------------
  ;; Governance and Permissions

  (defconst GOV_GUARD:string "gov")
  (defconst OPS_GUARD:string "ops")

  (defcap GOV ()
    (enforce-guard (at "guard" (read m-guards GOV_GUARD ["guard"])))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap OPS ()
    (enforce-guard (at "guard" (read m-guards OPS_GUARD ["guard"])))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap OPS_INTERNAL ()
    (compose-capability (INCREMENT))
  )

  (defschema m-guard ;; ID is a const: OPS_GUARD, GOV_GUARD etc.
    @doc "Stores guards for the module"
    guard:guard  
  )
  (deftable m-guards:{m-guard})

  (defun rotate-gov:string (guard:guard)
    @doc "Requires GOV. Changes the gov guard to the provided one."

    (with-capability (GOV)
      (update m-guards GOV_GUARD
        { "guard": guard }  
      )

      "Rotated GOV to a new guard"
    )
  )

  (defun rotate-ops-from-gov (guard:guard)
    @doc "Requires GOV. Changes the ops guard to the provided one."

    (with-capability (GOV)
      (rotate-ops-internal guard)
    )
  )

  (defun rotate-ops:string (guard:guard)
    @doc "Requires OPS. Changes the ops guard to the provided one."

    (with-capability (OPS)
      (rotate-ops-internal guard)
    )
  )

  (defun rotate-ops-internal:string (guard:guard)
    @doc "Requires GOV. Changes the ops guard to the provided one."
    (require-capability (OPS_INTERNAL))

    (update m-guards OPS_GUARD
      { "guard": guard }  
    )

    "Rotated OPS to a new guard"
  )

  (defun get-gov-guard:guard ()
    @doc "Gets the current gov guard and returns it"
    (at "guard" (read m-guards GOV_GUARD))
  )

  (defun get-ops-guard:guard ()
    @doc "Gets the current ops guard and returns it"
    (at "guard" (read m-guards OPS_GUARD))
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
  ;; Decimal Values

  (defschema decimal-value
    @doc "Stores decimal values"
    value:decimal
  )
  (deftable decimal-values:{decimal-value})

  (defun update-decimal-value (val-id:string value:decimal)
    @doc "Updates the account for the bank"

    (with-capability (OPS)
      (write decimal-values val-id
        { "value": value }
      )
    )
  )

  (defun get-decimal-value:decimal (val-id:string)
    @doc "Gets the value with the provided id"

    (at "value" (read decimal-values val-id ["value"]))
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
  (defconst OG_BADGE_PERCENT_PAYOUT:string "OG_BADGE_PERCENT_PAYOUT")

  ;; -------------------------------
  ;; Kor Percent Payout Source

  (implements kor-percent-payout-source-v1)
  (use kor-percent-payout-source-v1 [percent-payout-info])

  (defun get-percent-payout:decimal ()
    (get-decimal-value OG_BADGE_PERCENT_PAYOUT)
  )

  (defun get-all-percent-payout-infos:[object{percent-payout-info}] ()
    (let 
      (
        (transform 
          (lambda (percent:decimal nft:object{og-badge})
            (bind nft 
              { "account":= account
              , "nft-id":= nft-id
              , "last-payout":= last-payout
              }
              { "account": account
              , "nft-id": (int-to-str 10 nft-id)
              , "last-payout": last-payout
              , "guard": (at "guard" (coin.details account))
              }
            )
          )
        )
      )
      (map (transform (get-percent-payout)) (get-all-og-badges))
    )
  )

  (defun get-percent-payout-info:object{percent-payout-info} (id:string)
    @doc "Gets a single hashrate nft's info."

    (bind (get-og-badge-with-id id) 
      { "account":= account
      , "hash-rate":= hashrate 
      , "last-payout":= last-payout
      }
      { "account": account
      , "nft-id": (int-to-str 10 id)
      , "percent": (get-percent-payout)
      , "last-payout": last-payout
      , "guard": (at "guard" (coin.details account))
      }
    )
  )

  (defun update-percent-payout-time:string
    (
      id:string
      t:time
    )

    (with-capability (OPS)
      (update og-badges id
        { "last-payout": t }
      )
    )
  )

  ;; -------------------------------
  ;; OG Badge

  (defcap CLAIM_OG_BADGE (account:string)
    (enforce-guard (at "guard" (coin.details account)))
    (compose-capability (OPS))
  )

  (defschema unclaimed-badge
    @doc "The table for unclaimed og badges, ID is the encrypted phone"
    account:string
    phone-encrypted:string 
    claimed:bool
  )
  (deftable unclaimed-badges:{unclaimed-badge})
  
  (defun add-encrypted-phone (account:string encrypted:string)
    (with-capability (OPS)
      (insert unclaimed-badges encrypted
        { "account": account
        , "phone-encrypted": encrypted
        , "claimed": false
        }
      )
    )  
  )

  (defun reset-encrypted-phone (encrypted:string)
    (with-capability (OPS)
      (update unclaimed-badges encrypted
        { "claimed": false
        }
      )
    )  
  )

  (defun get-all-unclaimed-badges:[object{unclaimed-badge}] ()
    (select unclaimed-badges (constantly true))
  )

  (defun get-unclaimed-badges-for-account:[object{unclaimed-badge}] (account:string)
    (filter 
      (where "claimed" (= false))
      (select unclaimed-badges
        (where "account" (= account)) 
      )
    )
  )

  (defun get-unclaimed-badges:[object{unclaimed-badge}] ()
    (select unclaimed-badges (where "claimed" (= false)))
  )

  (defschema og-badge
    @doc "The table for og badges, ID is the counter"
    account:string 
    nft-id:integer
    og-badge:string
    phone-encrypted:string
    created-date:time
    last-payout:time
  )
  (deftable og-badges:{og-badge})

  (defun claim-og-badge:string
    (
      account:string 
      og-badge:string
      phone-encrypted:string
    )
    @doc "Function to create an OG Badge NFT owned by the given account"
    (enforce (!= account "") "Account cannot be blank")

    (with-read unclaimed-badges phone-encrypted
      { "account":= unclaimed-account
      , "claimed":= claimed 
      }
      (enforce (= account unclaimed-account) "Accounts don't match")
      (enforce (not claimed) "Phone number already claimed")
    )
    
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
          , "last-payout": (curr-time)
          }
        )
        (update unclaimed-badges phone-encrypted
          { "claimed": true
          }
        )
      )
    )
  )

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
          , "last-payout": (curr-time)
          }
        )
        (write unclaimed-badges phone-encrypted
          { "account": account
          , "phone-encrypted": phone-encrypted
          , "claimed": true
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

  (defun get-og-badge-with-id:object{og-badge} (nft-id:string)
    (read og-badges nft-id)
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


