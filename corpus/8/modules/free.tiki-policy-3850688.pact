(module tiki-policy GOV
  @doc "Policy for all tiki NFTs."

  ;; -------------------------------
  ;; Governance and Permissions

  (defcap GOV ()
    (enforce-guard (free.tiki-perms.get-gov-guard))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap OPS ()
    (enforce-guard (free.tiki-perms.get-ops-guard))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap OPS_INTERNAL ()
    true
  )

  ;; -------------------------------
  ;; Bool Values

  (defconst CAN_BURN:string "CAN_BURN")
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

  ;; -------------------------------
  ;; Policy

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defun enforce-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
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
    (enforce-guard (free.tiki-perms.get-ops-guard))
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce (get-bool-value CAN_BURN) "No burning")
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)

    (enforce (get-bool-value CAN_OFFER) "No offering")
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

    (enforce (get-bool-value CAN_BUY) "No buy")
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
    (enforce (get-bool-value CAN_TRANSFER) "No transfers")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce (get-bool-value CAN_XCHAIN) "No crosschain transfers")
  )
)



