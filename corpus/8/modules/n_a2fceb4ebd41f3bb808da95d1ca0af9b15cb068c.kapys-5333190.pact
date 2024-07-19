(module kapys GOV
    
  (defcap GOV ()
    (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.admin")))

  (defcap OPS ()
    (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.kapys-ops"))
    (compose-capability (WALLET))
    )

  
  (defcap PAY_EVENT
        (
         id:string
          payer1:string
          payer2:string
          amount:decimal
        )
        @event true
        )

(defschema wallet-schema
  account:string
  account-guard:guard
  paid:decimal
 )

(deftable wallets:{wallet-schema})


(defun get-all-wallets ()
(select wallets (constantly true))
)

  (defschema payment-schema
    payee:[string]
    amount:decimal
  )

  (deftable payments:{payment-schema})


(defun get-all ()
(select payments (constantly true))
)

(defun add-wallet (payer:string account:string account-guard:guard)
    (with-capability (OPS)
    (insert wallets payer
      {
        'account:account
       ,'account-guard:account-guard
       ,'paid: 0.0
      }
    )
    )
)

  (defun get-payment-status (id:string)
    (with-read payments id { "paid":= paid }
      paid))

  (defun get-payment-details (id:string)
    (read payments id))

    (defconst PAYER1:string "payer1")
    (defconst PAYER2:string "payer2")


(defun payout ()
 @doc "Pays out royalty distributions"
 (let* (
  (payer1:string (at 'account (read wallets PAYER1 ['account ])))
  (payer2:string (at 'account (read wallets PAYER2 ['account ])))
  (wallet:string (get-WALLET-account))
  (amount:decimal (at "balance" (coin.details wallet)))
  (first-amount:decimal (* amount 0.50))
  (second-amount:decimal (- amount first-amount))
  (time:time (curr-time))
 )
 (enforce (> amount 0.0 ) "balance too low")
  ; Transfer funds to the splitter account
(with-capability (WALLET)
  ; Install capabilities for the transfers from the splitter account to creator and bank accounts
  (install-capability (coin.TRANSFER wallet payer1 first-amount))
  (install-capability (coin.TRANSFER wallet payer2 second-amount))

 ; Transfer funds from the splitter account to creator and bank accounts
 (coin.transfer wallet payer1 first-amount)
 (coin.transfer wallet payer2 second-amount)

 (let (
  (id:string (hash { 'payer1:payer1, 'payer2:payer2, 'time:time, 'amount:amount}))
 )
 (insert payments id
  {
    'payee: [payer1, payer2]
    ,'amount: amount
  } )
  (with-default-read wallets PAYER1
    {"account": "", "paid": 0.0}
    {"account":=act, "paid":=paid-qty}
    (let ((new-paid:decimal (+ first-amount paid-qty)))
    (update wallets PAYER1 
        {
            "paid": new-paid
        }
    )
)
)
(with-default-read wallets PAYER2 
  {"account": "", "paid": 0.0}
  {"account":=act, "paid":=paid-qty}
  (let ((new-paid:decimal (+ second-amount paid-qty)))
  (update wallets PAYER2 
      {
          "paid": new-paid
      }
  )
)
)
 
 (emit-event (PAY_EVENT id payer1 payer2 amount ))))
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
      ;  (coin.create-account KDA_BANK_ACCOUNT (kda-bank-guard))
      (coin.create-account (get-WALLET-account) (create-WALLET-guard))
    )
  )

)  


