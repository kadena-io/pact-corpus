(module dadbod-policy GOV
  @doc "Policy for all dadbod NFTs. Transfer and sale are both allowed. \
  \ Mint and burn are OPS privileges."

  ;; -------------------------------
  ;; Governance and Permissions

  (defconst GOV_GUARD:string "gov")
  (defconst OPS_GUARD:string "ops")

  (defcap GOV ()
    (enforce-guard (at "guard" (read m-guards GOV_GUARD ["guard"])))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap OPS ()
    (enforce-guard (at "guard" (read m-guards OPS_GUARD ["guard"])))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap OPS_INTERNAL ()
    true
  )

  (defschema m-guard ;; ID is a const: OPS_GUARD, GOV_GUARD etc.
    @doc "Stores guards for the module"
    guard:guard  
  )
  (deftable m-guards:{m-guard})

  (defun rotate-gov:string (guard:guard)
    @doc "Requires GOV. Changes the gov guard to the provided one."

    (with-capability (GOV)
      (update m-guards GOV_GUARD
        { "guard": guard }  
      )

      "Rotated GOV to a new guard"
    )
  )

  (defun rotate-ops-from-gov (guard:guard)
    @doc "Requires GOV. Changes the ops guard to the provided one."

    (with-capability (GOV)
      (rotate-ops-internal guard)
    )
  )

  (defun rotate-ops:string (guard:guard)
    @doc "Requires OPS. Changes the ops guard to the provided one."

    (with-capability (OPS)
      (rotate-ops-internal guard)
    )
  )

  (defun rotate-ops-internal:string (guard:guard)
    @doc "Requires GOV. Changes the ops guard to the provided one."
    (require-capability (OPS_INTERNAL))

    (update m-guards OPS_GUARD
      { "guard": guard }  
    )

    "Rotated OPS to a new guard"
  )

  (defun get-gov-guard:guard ()
    @doc "Gets the current gov guard and returns it"
    (at "guard" (read m-guards GOV_GUARD))
  )

  (defun get-ops-guard:guard ()
    @doc "Gets the current ops guard and returns it"
    (at "guard" (read m-guards OPS_GUARD))
  )

  (defun init-perms:string (gov:guard ops:guard)
    @doc "Initializes the guards and creates the tables for the module"

    ;; This is only vulnerable if GOV_GUARD doesn't exist
    ;; Which means it's only vulnerable if you don't call 
    ;; init when you deploy the contract.
    ;; So let us be sure that init is called. =)
    (insert m-guards GOV_GUARD
      { "guard": gov }  
    )
    (insert m-guards OPS_GUARD
      { "guard": ops }  
    )
  )

  ;; -------------------------------
  ;; Bool Values

  (defconst CAN_OFFER:string "CAN_OFFER")
  (defconst CAN_BUY:string "CAN_BUY")
  (defconst CAN_TRANSFER:string "CAN_TRANSFER")
  (defconst CAN_XCHAIN:string "CAN_XCHAIN")

  (defschema bool-value
    @doc "Stores the boolean values for things like transfer/offer enforcing. \
    \ Enables turning it on and off at will."
    value:bool
  )
  (deftable bool-values:{bool-value})

  (defun update-bool-value (val-id:string value:bool)
    @doc "Updates the account for the bank"

    (with-capability (OPS)
      (write bool-values val-id
        { "value": value }
      )
    )
  )

  (defun get-bool-value:string (val-id:string)
    @doc "Gets the value with the provided id"

    (at "value" (read bool-values val-id ["value"]))
  )

  (defun get-can-offer:bool ()
    (get-bool-value CAN_OFFER)
  )

  (defun get-can-buy:bool ()
    (get-bool-value CAN_BUY)
  )

  (defun get-can-transfer:bool ()
    (get-bool-value CAN_TRANSFER)
  )

  (defun get-can-xchain:bool ()
    (get-bool-value CAN_XCHAIN)
  )

  ;; -------------------------------
  ;; Policy

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defun enforce-ledger:bool ()
    (enforce-guard (free.dadbod-ledger.ledger-guard))
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (enforce-guard (get-ops-guard))
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce-guard (get-ops-guard))
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (enforce (get-can-offer) "No offers")
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (enforce (get-can-buy) "No buys")
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce (get-can-transfer) "No transfers")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce (get-can-xchain) "No crosschain transfers")
  )
)



