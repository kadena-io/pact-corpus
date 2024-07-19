(module airdrop GOVERNANCE
    @doc "Arkade's airdrop contract for distributing ARKD tokens."
    (use arkade.token [ transfer get-balance transfer-create ])

    (defconst AIRDROP_BANK "arkd-airdrop-bank")
    (defconst AIRDROP_ADMIN "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec")
    (defconst AIRDROP_COUNT "airdrop-count")

    ; --------------------------------------------------------------------------
    ; Utilities
    ; --------------------------------------------------------------------------
    (defcap PRIVATE ()
        true
    )

    (defcap GOVERNANCE ()
        (enforce-one "Any Guard passes" [
            (enforce-guard (at "guard" (coin.details "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec")))
        ])
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard 
            (at "guard" (coin.details account))
        )
    )

    ; --------------------------------------------------------------------------
    ; Schema
    ; --------------------------------------------------------------------------
    (defschema counts-schema
        @doc "Keeps track of key counts."
        count:integer
    )

    (defschema airdrop-history-schema
        @doc "Stores airdrop history"
        account:string
        amount:decimal
        airdrop-time:time
    )

    ; --------------------------------------------------------------------------
    ; Tables
    ; --------------------------------------------------------------------------
    (deftable airdrop-history-table:{airdrop-history-schema})
    (deftable counts-table:{counts-schema})

    ; --------------------------------------------------------------------------
    ; Init
    ; --------------------------------------------------------------------------
    (defun init()
      (with-capability (GOVERNANCE)
        (insert counts-table AIRDROP_COUNT { "count": 0 })
      )
    )

    ; --------------------------------------------------------------------------
    ; Functions 
    ; --------------------------------------------------------------------------
    (defun get-count:integer (key:string)
        @doc "Gets the count for a key" 
        (at "count" (read counts-table key ['count]))
    )

    (defun get-airdrop-history()
        @doc "Returns all the airdrop history."
        (select airdrop-history-table (where "account" (!= "")))
    )
    
    (defun increase-count (key:string)
        @doc "Increase the count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )

    (defun airdrop-accounts(accounts:list)
        (with-capability (ACCOUNT_GUARD AIRDROP_ADMIN)
        (with-capability (PRIVATE)
            (map (airdrop-user-account) accounts)
        ))
    )

    (defun airdrop-user-account(item:object)
        (enforce (!= (at 'account item) "") "Account field can not be empty.")
        (enforce (> (at 'amount item) 0.0) "Amount must be greater than 0.0")
        (require-capability (PRIVATE))
        (with-capability (BANK_DEBIT)
            (let*
                (
                    (account (at 'account item))
                    (amount (at 'amount item))
                )
                (install-capability (arkade.token.TRANSFER AIRDROP_BANK account amount))
                (transfer AIRDROP_BANK account amount)
                (increase-count AIRDROP_COUNT)
                (insert airdrop-history-table (int-to-str 10 (+ 1  (get-count AIRDROP_COUNT))){
                    'account: account
                    ,'amount: amount
                    ,'airdrop-time: (at "block-time" (chain-data))
                })
            )
        )
    )

    ; --------------------------------------------------------------------------
    ; Utility 
    ; --------------------------------------------------------------------------
    (defun create-airdrop-user-guard (funder:string amount:decimal account:string)
        (with-capability (GOVERNANCE)
            (transfer-create funder account 
                (create-BANK_DEBIT-guard) amount)
        )
    )
    
    (defun require-BANK_DEBIT () 
        (require-capability (BANK_DEBIT))
    )
    
    (defun create-BANK_DEBIT-guard ()
        (create-user-guard (require-BANK_DEBIT))
    )

    (defcap BANK_DEBIT ()
        true
    )
)

; (if (read-msg "upgrade")
;   ["upgrade"]
;   [
;     (create-table airdrop-history-table)
;     (create-table counts-table)
;     (init)
;   ]
; )

; [{"account": "k:089b297cd59bc847ea09bd039dea7652d90901a59d7a61923bef3cf0c3b334ec", "amount": 1.0}]


