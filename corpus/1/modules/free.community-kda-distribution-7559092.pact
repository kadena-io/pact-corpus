(module community-kda-distribution GOVERNANCE
  @doc "Community-kda-distribution contract."
    (use coin)

    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")

    (defschema project-schema
        project-name:string
        project-admin-address:string
        project-account:string
    )



    (deftable project-table:{project-schema})

    (defun distribute-to-many-addresses (project-name:string receivers:list amounts:list )
        (enforce (= (length receivers) (length amounts)) "length of each list must be the same")
        (map (distribute-to-address project-name) (zip (lambda (x y) { 'receiver: x, 'amount: y }) ["k:1" "k:2" "k:3" "k:4"] [4 5 6 7]))
    )
    
    (defun distribute-to-address (project-name:string obj:object )
        (let 
            (
                (receiver (at 'receiver obj) )
                (amount (at 'amount obj))
            )
            (with-read project-table project-name
                { 'project-admin-address:= project-admin-address
                , 'project-name:= project-name
                , 'project-account:= project-account}
                (with-capability (ACCOUNT_GUARD project-admin-address)
                (with-capability (BANK_DEBIT)
                    (install-capability (coin.TRANSFER project-account receiver amount))
                    (coin.transfer-create project-account receiver (at 'guard (coin.details receiver)) amount)
                ))
            )
        )
    )

    (defun check-balance-of-project-account (project-name)
        (with-read project-table project-name
            { 'project-account:= project-account }
            (coin.get-balance project-account)
        )
    )

    (defun fund-project-account (funder:string amount:decimal project-name:string)
        (with-read project-table project-name
            { 'project-account:= project-account }
            (coin.transfer funder project-account amount)
        )
    )

    (defun create-project-account (funder:string amount:decimal project-account:string project-name:string)
        (coin.transfer-create funder project-account 
          (create-BANK_DEBIT-guard) amount)
        (insert project-table project-name
            { "project-name": project-name
            , "project-admin-address": funder }
        )
    )

    ;; Capability user guard: capability predicate function
    (defun require-BANK_DEBIT () 
        (require-capability (BANK_DEBIT))
    )

    ;; I want the user to be able to fund the account and be the only one who can 
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

    (defcap BANK_DEBIT (funder:string)
        
        true)
)


