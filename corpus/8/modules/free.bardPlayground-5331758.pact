(module bardPlayground GOVERNANCE
  @doc "Mainnet playground for test data"
    (use coin)
    (use marmalade-v2.ledger)
; ============================================
; ==               CONSTANTS                ==
; ============================================
    (defconst BANK_KDA_ACCT "test-bank1")
    (defconst CLAIM_COUNT "claim-count")
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

    (defschema claim-schema
        claim-count:integer
        account:string
        claim:object
    )

    (defschema available-chips-schema
        available:bool
        id:string
    )

    (deftable counts-table:{counts-schema})
    (deftable price-table:{price-schema})
    (deftable rent-table:{rent-schema})
    (deftable earnings-table:{earnings-schema})
    (deftable chips-table:{available-chips-schema})
    (deftable claim-table:{claim-schema})
    
    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        ; (insert counts-table CHIPS_AVAILABLE {"count": 100})
        (insert counts-table CLAIM_COUNT {"count": 0})
        ; (insert chips-table "1" {"available": true, "id": "1"})
        ; (insert chips-table "2" {"available": true, "id": "2"})
        ; (insert chips-table "3" {"available": true, "id": "3"})
        ; (insert chips-table "4" {"available": true, "id": "4"})
        ; (insert chips-table "5" {"available": true, "id": "5"})
        ; (insert chips-table "6" {"available": true, "id": "6"})
        ; (insert chips-table "7" {"available": true, "id": "7"})
        ; (insert chips-table "8" {"available": true, "id": "8"})
        ; (insert chips-table "9" {"available": true, "id": "9"})
        ; (insert chips-table "10" {"available": true, "id": "10"})
        ; (insert chips-table "11" {"available": true, "id": "11"})
        ; (insert chips-table "12" {"available": true, "id": "12"})
        ; (insert chips-table "13" {"available": true, "id": "13"})
        ; (insert chips-table "14" {"available": true, "id": "14"})
        ; (insert chips-table "15" {"available": true, "id": "15"})
        ; (insert chips-table "16" {"available": true, "id": "16"})
        ; (insert chips-table "17" {"available": true, "id": "17"})
        ; (insert chips-table "18" {"available": true, "id": "18"})
        ; (insert chips-table "19" {"available": true, "id": "19"})
        ; (insert chips-table "20" {"available": true, "id": "20"})
    )

; ============================================
; ==              MINT FUNCTIONS            ==
; ============================================

    (defun transfer-marmalade-nft (id:string sender:string receiver:string)
        (marmalade-v2.ledger.transfer-create id sender receiver (at 'guard (coin.details receiver)) 1.0)
    )

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
            (insert earnings-table key
                { "claimable" : 0.0
                , "predicted-apy" : 20.58
                , "total-value-locked" : (+ (* 100 num-chips) watts-required) })
            (update counts-table CHIPS_AVAILABLE {"count": (- chips-available num-chips)})
        ))
    )

    (defun claim (account:string key:string)
        @doc "Allows a user to claim rewards from a single rental contract"
        (with-capability (ACCOUNT_GUARD account)
        (with-capability (BANK_DEBIT)
            (with-read earnings-table key
                { 'claimable := earned }
                (enforce (> earned 0.0) "You have no kda claimable at this time")
                (coin.transfer BANK_KDA_ACCT account earned)
                (update earnings-table key
                    { "claimable" : 0.0 })
                (add-claim account earned)
            )
        ))
        
    )

    (defun add-claim (account:string amount:string)
        @doc "Adds a claim to the claim table"
        (insert claim-table (int-to-str 10 (get-count CLAIM_COUNT))
            { "account": account
            , "claim": {"amount": amount, "tiempo": (at "block-time" (chain-data))}
            , "claim-count": (get-count CLAIM_COUNT) }
        )
        (with-capability (PRIVATE)
            (increase-count CLAIM_COUNT))
    )

    (defun admin-get-all-active-locks ()
        (select rent-table (where "lock-end-date" (< (at "block-time" (chain-data)))))
    )

    (defun get-all-active-locks-for-account (account:string)
        @doc "name is deceptive. This returns all of an accounts locks, regardless of if they're active"
        (select rent-table (where "account" (= account)))
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

    (defun insert-all-earnings (account-keys:list earned:decimal)
        (map (insert-earnings earned) account-keys)
    )

    (defun insert-earnings (earned:decimal account-key:string)
        (with-read earnings-table account-key
            { "claimable":= claimable }
            (update earnings-table account-key
                { "claimable" : (+ earned claimable) })
        )
    )

    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counts-table key ['count]))
    )

    (defun increase-count (key:string)
        ;increase the count of a key in a table by 1
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )

    (defun create-simple-user-guard (funder:string amount:decimal account:string)
        (coin.transfer-create funder account 
          (create-BANK_DEBIT-guard) amount)
    )

    ;; Capability user guard: capability predicate function
    (defun require-BANK_DEBIT () 
        (require-capability (BANK_DEBIT))
    )
    
    ;; Capability user guard: guard constructor
    (defun create-BANK_DEBIT-guard ()
        (create-user-guard (require-BANK_DEBIT))
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

    (defcap BANK_DEBIT () true)

)


