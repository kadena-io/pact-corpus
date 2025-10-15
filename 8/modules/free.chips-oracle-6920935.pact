(module chips-oracle GOVERNANCE
  @doc "Chips oracle contract."
  (use coin)
; ============================================
; ==               CONSTANTS                ==
; ============================================
    (defconst ADMIN_KEYSET "n_e98a056e3e14203e6ec18fada427334b21b667d8.chips-admin" )
    (defconst KDA_PRICE_KEY "kda-price-key") ; kda price in USD
    (defconst kWATT_PRICE_KEY "kWatt-price-key")
    (defconst cKDA_PRICE_KEY "cKDA-price-key")
    (defconst cBTC_PRICE_KEY "cBTC-price-key")
    (defconst cLTC_PRICE_KEY "cLTC-price-key")
    (defconst cKAS_PRICE_KEY "cKAS-price-key")

    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst DISCORD_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")

; ============================================
; ==                  STUFF                 ==
; ============================================

    (defschema price-schema
        @doc "Stores the price of each type of NFT or upgrade"
        price:decimal
    )

    (defschema counts-schema
        count:integer
    )

    (defschema price-history-schema
        @doc "key is the coin and the count"
        coin:string
        price:decimal
        date:time
    )

    (deftable price-table:{price-schema})
    (deftable price-history-table:{price-history-schema})
    (deftable counts-table:{counts-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed"
        (insert counts-table "KDA" {"count": 1})
        ; (insert counts-table "kWATT" {"count": 1})
        (insert counts-table "cKDA" {"count": 1})
        (insert counts-table "cBTC" {"count": 1})
        (insert counts-table "cLTC" {"count": 1})
        (insert counts-table "cKAS" {"count": 1})
        (set-new-price "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad" 1.623 "KDA")
        (set-new-price "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad" 25.0 "cKDA")
        (set-new-price "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad" 119.0 "cLTC")
        (set-new-price "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad" 23.0 "cKAS")
        (set-new-price "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad" 21.9 "cBTC")
    )

    (defun get-price-history (coin:string)
        @doc "Returns the entire price history of a coin"
        (select price-history-table (where "coin" (= coin)))
    )

    (defun get-current-price (coin:string)
        (let* (
                (count (get-count coin)) 
                (key (format "{}-{}" [coin (- count 1)]))
            )
            (at 'price (read price-history-table key))
        )
    )

    (defun set-new-price (account:string price:decimal coin:string)
        (with-capability (ADMIN_OR_DISCORD account)
        (let* (
                (count (get-count coin)) 
                (key (format "{}-{}" [coin count]))
            )
            (insert price-history-table key
              { "coin" : coin
              , "price" : price
              , "date" : (at 'block-time (chain-data)) }
            )
            (increase-count coin)
        ))
    )


  (defun increase-count (key:string)
    ;increase the count of a key in a table by 1
    (require-capability (PRIVATE))
    (update counts-table key {"count": (+ 1 (get-count key))})
  )

  (defun get-count (key:string)
    @doc "Gets the count for a key"
    (at "count" (read counts-table key ['count]))
  )

  (defun set-count (key:string amount:integer)
    (update counts-table key
      { "count" : amount })
  )
; ============================================
; ==             CAPABILITIES               ==
; ============================================

    (defcap ADMIN_OR_DISCORD (account:string)
        (compose-capability (ACCOUNT_GUARD account))
        (compose-capability (PRIVATE))
        (enforce-one "admin or discord" [(enforce (= account ADMIN_ADDRESS) "") (enforce (= account DISCORD_ADDRESS)"")])
    )

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap ACCOUNT_GUARD (account:string)
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

 (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_e98a056e3e14203e6ec18fada427334b21b667d8.chips-admin")))
    
    (defcap PRIVATE ()
        true
    )

)


