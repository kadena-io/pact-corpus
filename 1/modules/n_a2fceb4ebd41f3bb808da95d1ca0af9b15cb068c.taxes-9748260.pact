(module taxes GOV

   (defcap GOV ()
    (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.admin" ))
   )

   (defcap MOVEFUNDS ()
    (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.admin" ))
    (compose-capability (BANK))
   )

   (defcap PAYMENT (account:string year:string amount:decimal) @event true)

   (defcap USERPAY (account:string)
    (with-default-read taxes account {'guard: (create-null-guard) } {'guard:=guard}
      (if (!= guard (create-null-guard)) 
        (enforce-guard (at 'guard (coin.details account)))
        (enforce-guard guard)
      )
    )
   )

   (defschema tax-schema
     @doc "Schema for storing tax account data"
     account:string
     guard:guard
     payments:[object:{payment-schema}]
     secondary-payment:[object:{secondary-payment-schema}]
   )

   (deftable taxes:{tax-schema})

   (defschema payment-schema
     @doc "Schema for storing payment data"
     year:string
     payment:decimal
    )
   
   (defschema secondary-payment-schema
    @doc "Schema for storing secondary payment data"
     year:string
     secondary-account:string
     payment:decimal
    )

   (defschema cost-schema
     @doc "Stores cost values"
     year:string
     cost:decimal
   )

   (deftable costing:{cost-schema})

; #######################################
;             Main Functions
; #######################################

  (defun update-costs:string (year:string cost:decimal)
    @doc "Provides costs for the service"
        (with-capability (GOV)
           (write costing year
            { "year": year, "cost": cost}
           )
        )
  )

  (defun get-costs:[object:{cost-schema}] ()
    (select costing (constantly true))
  )

  (defun get-account:[object:{payment-schema}] (account:string)
   (at 'payments (read taxes account))
  )

  (defun get-secondary-account:[object:{payment-schema}] (account:string)
  (at 'secondary-payment (read taxes account))
  )

  (defun get-ac ()
  (select taxes (constantly true))
  )

  (defun process-payment:bool (year:string account:string guard:guard)
    @doc "Process a payment for generating tax information"
    (let* (
          (cost:decimal (get-payment-year-amount year))
          (bank:string (get-BANK-account))
          (val:bool (get-primary-year account year))
          )

      (enforce (= val false ) "You have already paid for this year")
      (enforce (validate-principal guard account) "Invalid Account Type")
 
      (with-capability (USERPAY account)
 
      (coin.transfer account bank cost)
 
      (with-default-read taxes account
        { 'account: account, 'guard: guard, 'payments: [], 'secondary-payment: []}
        { 'account:= acc, 'guard:= g, 'payments:=p, 'secondary-payment:=sp}

        (enforce (= g guard) "Guards need to match")
        (let* ((newp (+ p [{'year: year, 'payment: cost }]))
              )
              (write taxes account
              { "account": acc, "guard": g, "payments": newp, 'secondary-payment: []}
          )
        )  
      )
     )
     (emit-event (PAYMENT account year cost))    
    )
  )
  
  (defun process-secondary-payment:bool (year:string account:string guard:guard secondaryAccount:string)
  @doc "Process a secondary account payment for generating tax information"
  (let* (
        (bank:string (get-BANK-account))
        (secondaryCost:decimal (get-secondary-payment-amount year))
        (val:bool (get-secondary-year account year secondaryAccount))
        )

    (enforce (= val false ) "You have already paid for this year")
    (enforce (validate-principal guard account) "Invalid Account Type")
    (with-capability (USERPAY account)
    (with-default-read taxes account
      { 'account: account, 'guard: guard, 'secondary-payment: []}
      { 'account:= acc, 'guard:= g, 'secondary-payment:=sp}

      (enforce (= g guard) "Guards need to match")
      (let ((newsp (+ sp [{'year: year, 'secondary-account: secondaryAccount, 'payment: secondaryCost }]))
            
            )
            (coin.transfer account bank secondaryCost)  

            (update taxes account
            { 'secondary-payment: newsp}
        )
      )  
    )
   )
   (emit-event (PAYMENT account year secondaryCost))    
  )
)

 (defun get-payment-amount:decimal (year:string)
  (at 'cost (read costing year))
 )

 (defun get-payment-year-amount:decimal (year:string)
  (let* ( 
    (cost:decimal (at 'cost (read costing year)))
    (kdausdprice:decimal (at "kda-usd-price" (kai-oracle.get-kda-usd-price)))
    (p:decimal  (floor (/ cost kdausdprice) 8))
    )
    p
  )
 )

 (defconst DISCOUNT:decimal 0.25)

 (defun get-secondary-payment-amount:decimal (year:string)
  (let* ( 
    (cost:decimal (* DISCOUNT (at 'cost (read costing year))))
    (kdausdprice:decimal (at "kda-usd-price" (kai-oracle.get-kda-usd-price)))
    (p:decimal  (floor (/ cost kdausdprice) 8))
    )
    p
  )
)

(defun get-primary-year:bool (account:string year:string)
 (with-default-read taxes account
   {'payments: []}  
   {'payments:= sp} 
   (let ((year-entries
           (filter (lambda (entry) (= (at 'year entry) year)) sp)))
     (!= (length year-entries) 0))
 )
) 

(defun get-secondary-year:bool (account:string year:string secondaryAccount:string)
  (with-default-read taxes account
    {'secondary-payment: []}  
    {'secondary-payment:= sp} 
    (let ((matching-entries
            (filter (lambda (entry) 
                      (and (= (at 'year entry) year) 
                           (= (at 'secondary-account entry) secondaryAccount))) 
                    sp)))
      (!= (length matching-entries) 0)
    )
  )
) 


 (defun transfer-funds:string (account:string guard:guard amount:decimal)
   @doc "Transfer funds from the BANK account"
   (with-capability (MOVEFUNDS)
    (install-capability (coin.TRANSFER (get-BANK-account) account amount))
    (coin.transfer-create (get-BANK-account) account guard amount)
   )
 )

    
 (defun enforce-null:bool
  ()
  false
 )

 (defun create-null-guard:guard
  ()
  (create-user-guard (enforce-null))
 )

; #############################################
;                 Wallet Account
; #############################################


(defcap BANK ()
@doc "Checks to make sure the guard for the given account name is satisfied"
true
)

(defun require-BANK ()
@doc "The function used when building the user guard for managed accounts"
(require-capability (BANK))
)

(defun create-BANK-guard ()
@doc "Creates the user guard"
(create-user-guard (require-BANK))
)

(defun get-BANK-account ()
(create-principal (create-BANK-guard))
)

(defun initwallet ()
 (with-capability (GOV)
    (coin.create-account (get-BANK-account) (create-BANK-guard))
 )
)

)


