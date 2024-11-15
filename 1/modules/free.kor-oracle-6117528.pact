(module kor-oracle GOV
  @doc "This contract is a simple oracle for KOR that keeps track of \
  \ token prices."

  (defconst GOV_GUARD:string "gov")
  (defconst OPS_GUARD:string "ops")

  (defcap GOV ()
    (enforce-guard (at "guard" (read m-guards GOV_GUARD ["guard"])))
  )

  (defcap OPS ()
    (enforce-guard (at "guard" (read m-guards OPS_GUARD ["guard"])))
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
  ;; Oracle Functions

  (defschema price-tracker ;; ID is token-currency: kda-usd, flux-usd, etc.
    @doc "Stores each price tracker"
    token:string
    price:decimal 
    currency:string
    last-update:time
  )
  (deftable price-trackers:{price-tracker})

  (defun create-price-tracker:string (token:string currency:string price:decimal)
    (with-capability (OPS)
      (insert price-trackers (get-price-tracker-id token currency)
        { "token": token
        , "currency": currency
        , "price": price
        , "last-update": (curr-time)
        }
      )
    )
  )

  (defun update-price-tracker:string (token:string currency:string price:decimal)
    (with-capability (OPS)
      (update price-trackers (get-price-tracker-id token currency)
        { "price": price
        , "last-update": (curr-time)
        }
      )
    )
  )

  ;; -------------------------------
  ;; Getters

  (defun get-all-price-trackers:[{price-tracker}] ()
    (select price-trackers (constantly true))
  )

  (defun get-price-tracker:{price-tracker} (token:string currency:string)
    (read price-trackers (get-price-tracker-id token currency))
  )

  (defun get-price-for-price-tracker:decimal (token:string currency:string)
    (at "price" (read price-trackers (get-price-tracker-id token currency) ["price"]))
  )

  (defun get-last-update-for-price-tracker:time (token:string currency:string)
    (at "last-update" (read price-trackers (get-price-tracker-id token currency) ["last-update"]))
  )

  ;; -------------------------------
  ;; Utils

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"

    (at 'block-time (chain-data))
  )

  (defun get-price-tracker-id:string (token:string currency:string)
    (concat [token "-" currency])
  )

)



