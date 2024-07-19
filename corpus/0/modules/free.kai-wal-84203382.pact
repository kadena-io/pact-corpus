(module kai-wal GOV
    
  (defcap GOV ()
    (enforce-guard (keyset-ref-guard "free.pay-oracle-admin")))

  (defcap OPS ()
    (enforce-guard (keyset-ref-guard "free.pay-oracle-ops")))

  (defcap MFA ()
  (enforce-guard (keyset-ref-guard "free.pay-oracle-mfa")))

   
    (defcap PAY_EVENT
        (
          id:string
          payee:string
          amount:decimal
        )
        @event true
        )

        (defcap SENDY ()
        (compose-capability (WALLET))
        )

  (defschema payment-schema
    id:string
    payee:string
    amount:decimal
    mfa:bool
    paid:bool
    fungible:module{fungible-v2}
    )

  (deftable payments:{payment-schema})

  (defconst fungible "fungible")

  (defun get-payment-status (id:string)
    (with-read payments id { "paid":= paid }
      paid))

  (defun get-payment-details (id:string)
    (read payments id))

  (defun mfa-payment (id:string  payee:string amount:decimal fungible:module{fungible-v2})
  (with-capability (OPS)      
  (insert payments id
        { "id": id
        , "payee": payee
        , "amount": amount
        , "mfa": false
        , "paid": false
        , "fungible": fungible
        }) )
      ) 

      (defun set-mfa
      (id:string
        satisfied:bool)
      @doc "If MFA = Satisfied, then allow payment"
      (with-capability (MFA)
      (enforce (= satisfied true) "MFA Validation Failed")
        (update payments id { "mfa": true})
        ))

  (defun process-payment:object{payment-schema}
    (id:string
        fungible:module{fungible-v2}
        )
   (with-capability (SENDY)
    (with-read payments id
            { 
             "payee":= payee
            , "amount":= amount
            , "paid":= paid
            } 
            (enforce (= paid true) "Payment already processed")
            (let
                (
                  (wallet-account (get-WALLET-account))
                )  
  
                ; Install capabilities for the transfers from the wallet account to creator and bank accounts
                (install-capability (fungible::TRANSFER wallet-account payee amount))
  
                ; Transfer funds from the wallet account to creator and bank accounts
                (fungible::transfer wallet-account payee amount)    
            )
    (update payments id { "paid": true })
    (emit-event (PAY_EVENT id payee amount ))))
  )
  

  (defun get-unprocessed-transactions:[object:{payment-schema}]
    (
      id:string
    )
    @doc "Returns a list of unpaid transactions."
    (select payments id
      (and?
        (where "paid" (= false))
        (where "mfa" (= true))
        (where "id" (= id))
      )
    )
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
      ;  (coin.create-account KDA_BANK_ACCOUNT (kda-bank-guard))
      (coin.create-account (get-WALLET-account) (create-WALLET-guard))
    )
  )

)  


