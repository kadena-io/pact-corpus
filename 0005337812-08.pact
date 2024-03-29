(module policy-royalty GOVERNANCE
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use util-policies)
  (use free.util-fungible [enforce-valid-account])
  (use free.util-strings [to-string])

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defcap GOVERNANCE ()
    (governance.enforce-governance))

  ;-----------------------------------------------------------------------------
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------
  ; Store the royalty informations per token
  (defschema royalty-token-sch
    token-id:string
    creator-account:string
    creator-guard:guard
    rate:decimal
    currencies:[module{fungible-v2}]
  )

  (deftable royalty-tokens:{royalty-token-sch})

  ; Store the royalty informations per sale
  (defschema royalty-sale-sch
    currency:module{fungible-v2}
  )

  (deftable royalty-sales:{royalty-sale-sch})

  ;-----------------------------------------------------------------------------
  ; Capabilities and events
  ;-----------------------------------------------------------------------------
  (defcap ROYALTY-PAID (token-id:string creator-account:string amount:decimal)
    @doc "Event emitted when a royalty is paid to a creator"
    @event
    true)

  (defcap UPDATE-ROYALTY (token-id:string)
    @doc "Capability to modify the royalty"
    (with-read royalty-tokens token-id {'creator-guard:=current-guard}
      (enforce-guard current-guard))
  )

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defschema royalty-init-msg-sch
    creator_acct:string ; Creator account: recipient of the royalty
    creator_guard:guard ; Creator account: recipient of the royalty
    rate:decimal ; Royalty rate
    currencies:[module{fungible-v2}] ; List of currencies allowed for royalty payment
  )

  (defun read-royalty-init-msg:object{royalty-init-msg-sch} (token:object{token-info})
    (enforce-get-msg-data "royalty" token))

  ;-----------------------------------------------------------------------------
  ; Util functions
  ;-----------------------------------------------------------------------------
  (defun enforce-valid-fungibles:bool (currencies:[module{fungible-v2}])
    (enforce (and? (< 0) (>= 20) (length currencies)) "Incorrect currencies list"))

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-ROYALTY)

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-INIT token policy-royalty))
    (let ((royalty-init-msg (read-royalty-init-msg token))
          (token-id (at 'id token)))
      (bind royalty-init-msg {'creator_acct:=c-a, 'creator_guard:=c-g, 'rate:=rate, 'currencies:=cur}
        (enforce-valid-account c-a)
        (enforce-valid-rate rate)
        (enforce-valid-fungibles cur)
        (insert royalty-tokens token-id {'token-id:token-id,
                                         'creator-account:c-a,
                                         'creator-guard:c-g,
                                         'currencies:cur,
                                         'rate:rate})))
    true
   )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    true)

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    true)

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    true)

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (ledger.POLICY-ENFORCE-OFFER token (pact-id) policy-royalty))
    ; Read the fungible currency from the sale message
    (bind (enforce-read-sale-msg token) {'currency:=currency}
      ; Fetch from the database the allowed currencies for this token
      (with-read royalty-tokens (at 'id token) {'currencies:=allowed-currencies}
        ; And finally check that the requested currency is allowed
        ; Because of https://github.com/kadena-io/pact/issues/1307
        ;   until this one will be fixed, we have to compare by stringified versions
        (enforce (contains (to-string currency) (map (to-string) allowed-currencies))
                 "Currency is not allowed"))

      ;Store the currency for this sale-id => This will be needed during (enforce-settle)
      (insert royalty-sales (pact-id) {'currency: currency}))
    false ; We always return false because the royalty policy does not handle a sale
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    true)

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    true)

  (defun enforce-sale-settle:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-SETTLE token (pact-id) policy-royalty))
    (with-read royalty-sales (pact-id) {'currency:=currency:module{fungible-v2}}
      (with-read royalty-tokens (at 'id token) {'creator-account:=creator-a,
                                                'creator-guard:=creator-g,
                                                'rate:=rate}
        (let* ((escrow (ledger.escrow))
               (escrow-balance (currency::get-balance escrow))
               (royalty-amount (floor (* rate escrow-balance) (currency::precision)))
               (current-creator-g (try creator-g (at 'guard (currency::details creator-a)))))

          ; There are 3 possible cases:
          ;   - Creator account doesn't exist in the fungible contract
          ;       => transfer-create will create it
          ;   - Creator account already exists and the guard if his account is = to the registered guard
          ;        => transfer-create will transfer the funds safely
          ;   - Creator account already exists but the guard doesn't match with the registered guard
          ;        => To be sure, we don't charge the royalty
          (if (and (> royalty-amount 0.0) (= creator-g current-creator-g))
              (let ((_ 0))
                (install-capability (currency::TRANSFER escrow creator-a escrow-balance))
                (currency::transfer-create escrow creator-a creator-g royalty-amount)
                (emit-event (ROYALTY-PAID (at 'id token) creator-a royalty-amount)))
              false))))
    )

  ;-----------------------------------------------------------------------------
  ; External callable admin functions
  ;-----------------------------------------------------------------------------
  (defun rotate:string (token-id:string creator-account:string creator-guard:guard)
    @doc "Change/rotate the creator-account/creator-guard of the given tokenID"
    (enforce-valid-account creator-account)
    (with-capability (UPDATE-ROYALTY token-id)
      (update royalty-tokens token-id {'creator-account:creator-account,
                                       'creator-guard:creator-guard}))
  )

  (defun update-allowed-currencies:string (token-id:string currencies:[module{fungible-v2}])
    @doc "Change the list of allowed currencies"
    (enforce-valid-fungibles currencies)
    (with-capability (UPDATE-ROYALTY token-id)
      (update royalty-tokens token-id {'currencies:currencies}))
  )

  ;-----------------------------------------------------------------------------
  ; View functions
  ;-----------------------------------------------------------------------------
  (defun get-royalty-details:object{royalty-token-sch} (token-id:string)
    @doc "Return the details of the royalty spec for a token-id"
    (read royalty-tokens token-id))

  ;-----------------------------------------------------------------------------
  ; View functions (local only)
  ;-----------------------------------------------------------------------------
  (defun get-royalty-details-per-creator:[object{royalty-token-sch}] (creator:string)
    @doc "Return the details of the royalty specs of all tokens of a given creator"
    (select royalty-tokens (where 'creator-account (= creator))))
)

