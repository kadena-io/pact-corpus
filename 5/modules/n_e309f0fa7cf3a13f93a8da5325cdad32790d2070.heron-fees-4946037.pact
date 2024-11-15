(module heron-fees GOV
    
  (defcap GOV ()
    (enforce-guard (keyset-ref-guard "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.admin"))
    (compose-capability (WALLET))
  )

  (defcap PAY_EVENT
        (
          id:string
          to:string
          amount:decimal
          fungible:module{fungible-v2}
        )
        @event true
  )

  (defschema payment-schema
    payee:string
    amount:decimal
  )

  (deftable payments:{payment-schema})


(defun get-all ()
(select payments (constantly true))
)

  (defun get-payment-status (id:string)
    (with-read payments id { "paid":= paid }
      paid))

  (defun get-payment-details (id:string)
    (read payments id))

    (defconst PAYER1:string "payer1")
    (defconst PAYER2:string "payer2")

(defun transfer-funds (fungible:module{fungible-v2} to:string amount:decimal)
  (let* (
    (wallet-account:string (get-WALLET-account))
    (id:string (hash { 'to:to, 'time:time, 'amount:amount}))
    (time:time (curr-time))
  )
  (enforce (> amount 0.0 ) "balance too low")
 (with-capability (GOV)
   ; Install capabilities for the transfers from the wallet account to creator and bank accounts
   (install-capability (fungible::TRANSFER wallet-account to amount))
  
   ; Transfer funds from the wallet account to creator and bank accounts
   (fungible::transfer wallet-account to amount) 
   (insert payments id
    {
      'payee: to
      ,'amount: amount
    } )
   (emit-event (PAY_EVENT id to amount fungible )))
  )
)

 (defun curr-time:time ()
  @doc "Returns current chain's block-time"
  (at 'block-time (chain-data))
  )

; #############################################
;                 Wallet Account
; #############################################


(defcap WALLET ()
@doc "Checks to make sure the guard for the given account name is satisfied"
true
)

(defun require-WALLET ()
@doc "The function used when building the user guard for managed accounts"
(require-capability (WALLET))
)

(defun create-WALLET-guard ()
@doc "Creates the user guard"
(create-user-guard (require-WALLET))
)

(defun get-WALLET-account ()
(create-principal (create-WALLET-guard))
)

    (defun init ()
    (with-capability (GOV)
      (coin.create-account (get-WALLET-account) (create-WALLET-guard))
      (n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heron.create-account (get-WALLET-account) (create-WALLET-guard))
    )
  )

)  


