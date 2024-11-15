(module kadenai-credits GOV

    (defcap GOV ()
    (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.admin" ))
    )

    (defcap ENFORCE-PAYER ()
        @doc "Scope the signature for validation"
        true
    )

    ; Emits credit payment information
    (defcap CREDIT-PAYMENT
        (
            id:string
            payer:string
            bank:string
            paid:decimal
            qty:integer
        )
        @event true
        )    

    (defschema payment-schema
        id:string
        payer:string
        payer-guard:guard
        payment-timestamp:time
        paid:decimal
        qty:integer
    )
    
    (deftable payments:{payment-schema})

    (defschema credit-schema
        account:string
        account-guard:guard
        credits:integer
    )

    (deftable credits:{credit-schema})

    (defun create-payment:bool (account:string account-guard:guard qty:integer)
        @doc "Create a new payment"
        (let* (
            (payment-time:time (curr-time))
            (id:string (create-tx-id account account-guard payment-time))
            (bank:string (get-bank))
            (actual-payment:decimal (get-price-quote qty))
            )
            (enforce (> qty 0) "Can't be 0")
            (enforce (> actual-payment 0.0)"Can't be 0")

           (enforce-tx-reserved id account account-guard payment-time)
            
           (coin.transfer account bank actual-payment)

            ; Register payment if successful
            (insert payments id
                {
                    "id": id,
                    "payer": account,
                    "payer-guard": account-guard,
                    "payment-timestamp": payment-time,
                    "paid": actual-payment,
                    "qty": qty
                }
            )
            ; Register credits after successful payment
            (with-default-read credits account 
                {"account": "", "credits": 0}
                {"account":=act, "credits":=credit-qty}
                (let ((new-credits:integer (+ qty credit-qty)))
                (write credits account 
                    {
                        "account": account,
                        "account-guard": account-guard,
                        "credits": new-credits
                    }
                )
            )
        )
            (emit-event (CREDIT-PAYMENT id account bank actual-payment qty))
))
        
    

(defun create-tx-id:string (account:string account-guard:guard reg:time)
        (let
            (   
                (id:string (hash {'account:account, 'account-guard:account-guard, 'payment-time:reg}))
            )
            id
            )
)

(defun enforce-tx-reserved:bool (id:string account:string account-guard:guard reg:time)
    @doc "Enforces you made this id"
    ; Enforce the creation guard
    (with-capability (ENFORCE-PAYER)
             (enforce-guard account-guard))
             
            (enforce (= id (create-tx-id account account-guard reg)) "You didn't make this ID")
            true
)

(defun get-price-quote:decimal (qty:integer)
    @doc "Returns the price quote for the given quantity with discounts for specific quantity ranges."
    (let* (
        (kdausdprice:decimal (at "kda-usd-price" (n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.kai-oracle.get-kda-usd-price)))
        (credit-cost:decimal (get-payment-amount))
        (actual-cost:decimal (floor (/ credit-cost kdausdprice) 2))
        (discount:decimal (get-discount qty))
        (discounted-cost:decimal (* actual-cost discount))
        (actual-payment:decimal (* (dec qty) discounted-cost))
    ) actual-payment)
)


(defun get-discount:decimal (qty:integer)
    @doc "Calculates discount based on the quantity."
    (cond
        ((>= qty 100) 0.85) ; 15% discount for qty >= 100
        ((>= qty 50) 0.90)  ; 10% discount for qty >= 50
        ((>= qty 20) 0.95)  ; 5% discount for qty >= 20
        (true 1.0)          ; No discount
        false
    )
)

(defun get-payment-info:object{payment-schema} (id:string)
    @doc "Retrieves payment information based on the payment ID."
    (read payments id)
)

(defconst CREDIT_VALUE:decimal 1.0)

(defun get-payment-amount:decimal ()
        @doc "Return the payment amount"
        CREDIT_VALUE
)

(defun curr-time:time ()
@doc "Returns current chain's block-time"
(at 'block-time (chain-data))
)

(defun get-payments:[object:{payment-schema}] ()
(select payments (constantly true))
)

(defun get-credits:[object:{credit-schema}] ()
(select credits (constantly true))
)

; #######################################
;              Bank Info
; #######################################

(defschema bank-info
    @doc "Stores string values"
    value:string
  )
  (deftable bankInfo:{bank-info})

  (defun update-bank (bankId:string value:string)
    @doc "Updates the account for the bank"

    (with-capability (GOV)
      (write bankInfo bankId
        { "value": value }
      )
    )
  )

  (defun get-bank-value:string (bankId:string)
    @doc "Gets the value with the provided id"
    (at "value" (read bankInfo bankId ["value"]))
  )

  (defconst BANK_ACCOUNT:string "BANK")
  
  (defun get-bank:string ()
    (get-bank-value BANK_ACCOUNT)
  )

)


