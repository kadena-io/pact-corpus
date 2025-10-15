(module claim-heron GOV 

(defcap GOV ()
      @doc " Governance "
      (enforce-keyset "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heron-token-gov")
)

(defcap AMOUNT-CLAIM (account:string)
    (let ((hb-balance:decimal (n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heronbond.get-balance account)))
            (enforce-guard (at 'guard (coin.details account))))
)

(defcap CLAIM-CAP (account:string)
    (compose-capability (AMOUNT-CLAIM account))
    (compose-capability (CLAIM)))

(defschema account-schema
    account:string
    bond:decimal
    paid:bool
)

(deftable account-table:{account-schema})

;; Const and claim caps
(defconst BANK:string "k:dabfcd1efdac0f24942619a11c14561eeeda066650044afe91e23fc81bc2df1a")

(defcap CLAIM () true)
(defconst CLAIM-GUARD (create-capability-guard (CLAIM)))
(defconst CLAIM-ACCOUNT (create-principal CLAIM-GUARD))


(defun h-bal ()
(heron.get-balance CLAIM-ACCOUNT)
)

(defun c-bal ()
(coin.get-balance CLAIM-ACCOUNT)
)

(defun claim (account:string)
    @doc "Claim by swapping your bond for a mix of KDA/Heron"

    (let* (
        (hb:decimal (n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heronbond.get-balance account))
        (coin-amount:decimal (* 0.8 hb))
        (heron-amount:decimal (round (/ hb 0.0013 )12))
        (g (at 'guard (coin.details account)))
        (hbal (h-bal))
        (cbal (c-bal))
    )

    (enforce (and (> hb 0.0) (<= hb 10000.0)) "hb must be greater than 0 or less than 5000")
    (enforce (> coin-amount 0.0) "coin-amount must be greater than 0")
    (enforce (> heron-amount 0.0) "heron-amount must be greater than 0")
    (enforce (<= coin-amount cbal) "coin-amount must be less than or equal to the balance")
    (enforce (<= heron-amount hbal) "heron-amount must be less than or equal to the balance")

    ; Transfer the bond to the bank
    (n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heronbond.transfer account BANK hb)

    (with-default-read account-table account
        { 'account : "", 'bond : "", 'paid : false }
        { 'account := new-account, 'bond := new-hb, 'paid := paid }
        (enforce (not paid) "already paid"))
        
        (insert account-table account 
        {
        'account: account, 
        'bond: hb,
        'paid: true
        })
    
    (with-capability (CLAIM-CAP account)
    (install-capability (coin.TRANSFER CLAIM-ACCOUNT account coin-amount))
    (coin.transfer-create CLAIM-ACCOUNT account g coin-amount)

    (install-capability (heron.TRANSFER CLAIM-ACCOUNT account heron-amount))
    (heron.transfer-create CLAIM-ACCOUNT account g heron-amount)
    )
    (format "Sent {} Heron Bond, Claimed {} Heron, and {} KDA" [hb heron-amount coin-amount])
    )
)

(defun get-claim-info (account:string)
    @doc "Get the claim status of an account"
    (read account-table account))

(defun init ()
    (coin.create-account CLAIM-ACCOUNT CLAIM-GUARD)
    (heron.create-account CLAIM-ACCOUNT CLAIM-GUARD)
    (heronbond.create-account CLAIM-ACCOUNT CLAIM-GUARD)
)

)
      
      
