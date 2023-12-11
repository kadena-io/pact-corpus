(module bardPlayground GOVERNANCE
  @doc "Mainnet playground for test data"
    (use coin)
; ============================================
; ==               CONSTANTS                ==
; ============================================
    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst DISCORD_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")
    (defconst CHIPS_AVAILABLE "chips-available")
; ============================================
; ==            SCHEMA AND TABLES           ==
; ============================================

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )

    (defschema price-schema
        @doc "Stores the price of each type of NFT or upgrade"
        price:decimal
    )

    (defschema rent-schema
        @doc "stores information about currently rented items"
        ;key is the k:address and a counter in format xyz:1
        lock-end-date:time
        items-locked:integer
        watts-deposited:decimal
        key:string
    )

    (defschema earnings-schema
        @doc "Shows earnings over time for a given rental" 
        ; same key as rent-table for a given rental
        claimable:decimal
        predicted-apy:decimal
        total-value-locked:decimal
    )

    (deftable counts-table:{counts-schema})
    (deftable price-table:{price-schema})
    (deftable rent-table:{rent-schema})
    (deftable earnings-table:{earnings-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        (insert counts-table CHIPS_AVAILABLE {"count": 100})
    )

; ============================================
; ==              MINT FUNCTIONS            ==
; ============================================

    (defun start-mining 
      ( account:string
        num-chips:integer
        lock-duration-in-days:integer
        cost:decimal
      )
        ;buying watts simulation
        (with-default-read counts-table account
          { "count": 0 }
          { "count" := count} 
        (let*
            (
                (watts-required (* (* num-chips lock-duration-in-days) 0.005)) ;$0.005 per watt
                (lock-count (+ count 1))
                (key (format "{}:{}" [account lock-count]))
                (chips-available (get-count CHIPS_AVAILABLE))
            )
            (enforce (= cost watts-required) "The amount you're being charged does not match with the cost of your selected chips")
            (coin.transfer account ADMIN_ADDRESS watts-required)
            (update counts-table CHIPS_AVAILABLE
               { "count" : (- chips-available num-chips) })
            (with-capability (ACCOUNT_GUARD account)
                (insert rent-table key
                    { "lock-end-date" : (add-time (at "block-time" (chain-data)) (* 86400 lock-duration-in-days))
                    , "items-locked" : num-chips
                    , "watts-deposited" : watts-required
                    , "key" : key }
                )
            )
        ))
    )

    (defun get-all-active-locks-for-account (account:string)
        (select rent-table (where "lock-end-date" (> (at "block-time" (chain-data)))))
    )
    
    (defun get-earnings-for-all-user-locks (account:string)
        (with-default-read counts-table account
          { "count": 0 }
          { "count" := count}
          (map (check-earnings-for-lock account) (enumerate 1 count))
        )
    )
    
    (defun check-earnings-for-lock (account:string key:string)
        (read earnings-table (format "{}:{}" [account key]))
    )

    (defun admin-insert-earnings (account-key:string earned:decimal apy:decimal tvl:decimal)
        (with-capability (GOVERNANCE)
            (write earnings-table account-key
                { "claimable" : earned
                , "predicted-apy" : apy
                , "total-value-locked" : tvl } )
        )
    )

    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counts-table key ['count]))
    )

; ============================================
; ==             CAPABILITIES               ==
; ============================================

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap DISCORD(account:string)
        @doc "Only allows the discord wallet to send information"
        (enforce (= account DISCORD_ADDRESS) "only the administrator discord wallet can call this function")
        (compose-capability (ACCOUNT_GUARD DISCORD_ADDRESS))
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )

    (defcap PRIVATE ()
        true
    )

)


; (create-table counts-table)
; (create-table price-table)
; (create-table rent-table)
; (create-table earnings-table)

; (initialize)


