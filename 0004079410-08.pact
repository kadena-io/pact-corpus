(module kor-oracle GOVERNANCE
  @doc "This contract is a simple oracle for KOR that keeps track of \
  \ token prices."

(defcap GOVERNANCE ()
(enforce-keyset  "free.kor-admin" ))
  


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
    (with-capability (GOVERNANCE)
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
    (with-capability (GOVERNANCE)
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



