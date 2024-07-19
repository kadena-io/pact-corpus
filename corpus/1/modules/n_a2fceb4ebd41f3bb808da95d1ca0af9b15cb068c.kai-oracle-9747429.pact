(module kai-oracle GOV

    (defcap GOV ()
        (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.admin")))
    
    (defcap OPS ()
        (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.oracle-ops"))
    )

    (defschema pricing-schema
        kda-usd-price:decimal
        updated:time
    )

    (deftable pricing-table:{pricing-schema})
    
    ;; Price functions

    (defun init:bool ()
        @doc "Initializes the price table with default price"
        (with-capability (GOV)
        (insert pricing-table "kda-usd-price" 
        {"kda-usd-price": 1.0
        , "updated": (curr-time)
    }))true
    )

    (defun update-kda-usd-price:bool (new-price:decimal)
        @doc "Updates the KDA to USD price"
        (with-capability (OPS)
        (update pricing-table "kda-usd-price" { "kda-usd-price" : new-price, "updated": (curr-time)})
    )true)
  
    (defun get-kda-usd-price:object ()
        @doc "Returns the current KDA to USD price"
        (read pricing-table "kda-usd-price")
    )

    (defun curr-time:time ()
    @doc "Returns current chain's block-time"
    (at 'block-time (chain-data))
  )

)


