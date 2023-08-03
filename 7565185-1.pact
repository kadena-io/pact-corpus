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

    ;; example input:
    ;; project-name: "kmc"
    ;; receivers: ["k:1234" "k:3456" "k:5678"]
    ;; amounts: [1.0 1.5 20.4321]
    ;; this will send 1.0 kda to k:1234, it will send 1.5 kda to k:3456, and it will send 20.4321 kda to "k:5678"
    (defun distribute-to-many-addresses (project-name:string receivers:list amounts:list )
        @doc "Distributes to many addresses. Inputs are two lists. Index 1 for each list pairs together"
        (enforce (= (length receivers) (length amounts)) "length of each list must be the same")
        (map (distribute-to-address project-name) (zip (lambda (x y) { 'receiver: x, 'amount: y }) receivers amounts))
        (format "Distributed {} kda to {}" [amounts receivers])
    )
    
    (defun distribute-to-address (project-name:string obj:object )
        @doc "Distributes to a single address. Probably a waste of time to use this alone"
        (let* 
            (
                (receiver (at 'receiver obj) )
                (amount (at 'amount obj))
                (exists (try false (let ((ok true)) (at 'guard (coin.details receiver)) ok)))
                (guard1  (if (= true exists)
                            (at 'guard (coin.details receiver))
                            (format "KeySet {keys: [{}],pred: keys-all}"
                                [(drop 2 receiver)])
                        ))
            )
            (with-read project-table project-name
                { 'project-admin-address:= project-admin-address
                , 'project-name:= project-name
                , 'project-account:= project-account}
                (with-capability (ACCOUNT_GUARD project-admin-address)
                (with-capability (BANK_DEBIT)
                    (install-capability (coin.TRANSFER project-account receiver amount))
                    (coin.transfer-create project-account receiver guard1 amount)
                ))
            )
        )
    )

    (defun check-balance-of-project-account (project-name:string)
        @doc "Checks the balance of a project account"
        (with-read project-table project-name
            { 'project-account:= project-account }
            (coin.get-balance project-account)
        )
    )

    (defun fund-project-account (funder:string amount:decimal project-name:string)
        @doc "Add additional funds to an already existing project account"
        (with-read project-table project-name
            { 'project-account:= project-account }
            (coin.transfer funder project-account amount)
        )
    )

    (defun create-project-account (funder:string amount:decimal project-account:string project-name:string)
        @doc "Initializes a project in the database"
        (coin.transfer-create funder project-account 
          (create-BANK_DEBIT-guard) amount)
        (insert project-table project-name
            { "project-name": project-name
            , "project-admin-address": funder
            , "project-account": project-account }
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

    (defcap BANK_DEBIT ()
        
        true)
)


