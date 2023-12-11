(module kadenai-donate GOVERNANCE
  
    (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.admin" ))
       )

    (defcap OPS()
    (enforce-guard (keyset-ref-guard "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.ops"))
       )

        (defcap DONATION_EVENT
        (
          id:string
          account:string
          amount:decimal
        )
        @event true
        )
  
     (defschema donate-table
      id:string
      account:string
      amount:decimal
      fungible:module{fungible-v2}
    )

    (deftable donate:{donate-table})


    ;; -------------------------------
    ;;            Donations
    ;; -------------------------------

    (defun create-donation (amount:decimal account:string fungible:module{fungible-v2}  )
    (let*
      (
        (time (get-time))
        (bank (get-bank))
        (amt:decimal (floor (* amount 1.0) 2))
        (id (hash {"account": account, "amount": amt, "time": time}))
      )
   
      (if (> amount 0.0)
      (fungible::transfer account bank amt)
      []
  )
   
      (insert donate id
       {
        "id": id,
         "account": account,
         "amount": amt,
         "fungible": fungible
       }
     )
     (emit-event (DONATION_EVENT id account amt))
     )
   )
   
    ;; -------------------------------
    ;;          String Values
    ;; -------------------------------
  
  
    (defschema value
      @doc "Stores string values"
      value:string
    )
    (deftable values:{value})
  
    (defun update-string-value (val-id:string value:string)
      @doc "Updates the account for the bank"
  
      (with-capability (OPS)
        (write values val-id
          { "value": value }
        )
      )
    )
  
    (defun get-string-value:string (val-id:string)
      @doc "Gets the value with the provided id"
  
      (at "value" (read values val-id ["value"]))
    )
  
    (defconst BANK_ACCOUNT:string "BANK")
  
    (defun get-bank:string ()
      (get-string-value BANK_ACCOUNT)
    )

    ; Getters

    (defun get-time ()
    (at "block-time" (chain-data)
    ))
   
    (defun get-donation (id:string)
    (read donate id)
  )
 
    ; Local Calls

     (defun getz ()
    (select donate (constantly true))
    )

)

  
