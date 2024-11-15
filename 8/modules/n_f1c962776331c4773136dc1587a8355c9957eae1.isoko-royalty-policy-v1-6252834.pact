(module isoko-royalty-policy-v1 GOVERNANCE
  @doc "Concrete policy to support royalty payouts in a specified fungible during sale."
  (defconst ADMIN-KS:string "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")
  (defconst ROYALTY-POLICY:string (format "{}" [isoko-royalty-policy-v1]))
  (defcap GOVERNANCE ()
        (enforce-one "Any Guard passes" [
            (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
            (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")))]
        )
  )
  (use marmalade-v2.policy-manager)
  (use marmalade-v2.policy-manager [QUOTE-MSG-KEY quote-spec quote-schema])
  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (defschema royalty-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    royalty-rate:decimal
  )
  (defschema token-lock
    token-id:string
    locked:bool
  )
  (deftable royalties:{royalty-schema})
  (deftable token-locker:{token-lock})
  (defconst ROYALTY-SPEC-MSG-KEY "royalty_spec"
    @doc "Payload field for royalty spec")
  (defcap ROYALTY:bool (token-id:string royalty_spec:object{royalty-schema})
    @doc "Emits event with royalty information for discovery"
    @event
    true
  )
  (defcap ROYALTY-PAYOUT:bool
    ( sale-id:string
      token-id:string
      royalty-payout:decimal
      creator:string
    )
    @doc "Emits event with royalty payout information at sale's completion"
    @event
    true
  )
  (defun get-royalty:object{royalty-schema} (token:object{token-info})
    (read royalties (at 'id token))
  )
  (defun set-token-lock:object{token-lock} (token-id:string lock:bool)
    @doc "updates token lock value"
    ;   (require-capability (n_7d47538766e6f80008f253dccd30451f4d483c38.upgrade-handler.LOCKING))
      (enforce (= (at "locked" (read token-locker token-id)) false))
      (update token-locker token-id
        {
          "locked": lock
        }
      )
  )
  (defun get-token-lock:object{token-lock} (token-id:string)
      (read token-locker token-id)
  )
  (defun enforce-init:bool
    ( token:object{token-info}
    )
    @doc "Executed at `create-token` step of marmalade.ledger.      \
    \ Required msg-data keys:                                                  \
    \ * royalty_spec:object{royalty-schema} - registers royalty information of \
    \ the created token"
    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) ROYALTY-POLICY))
    (let* ( (spec:object{royalty-schema} (read-msg ROYALTY-SPEC-MSG-KEY))
            (fungible:module{fungible-v2} (at 'fungible spec))
            (creator:string (at 'creator spec))
            (creator-guard:guard (at 'creator-guard spec))
            (royalty-rate:decimal (at 'royalty-rate spec))
            (creator-details:object (fungible::details creator ))
            )
      (enforce (= (format "{}" [fungible]) (format "{}" [coin]))
        "Royalty support is restricted to coin")
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")
      (insert royalties (at 'id token)
        { 'fungible: fungible
        , 'creator: creator
        , 'creator-guard: creator-guard
        , 'royalty-rate: royalty-rate
        })
      (emit-event (ROYALTY (at 'id token) spec)))
  )
  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    true
  )
  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce false "Burn prohibited")
  )
  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce false "not enabled")
    (require-capability (OFFER-CALL (at "id" token) seller amount sale-id timeout ROYALTY-POLICY))
    (enforce (exists-msg-quote QUOTE-MSG-KEY) "Offer is restricted to quoted sale")
    (bind (get-royalty token)
      { 'fungible := fungible:module{fungible-v2} }
      (let* (
          (quote-spec:object{quote-spec} (read-msg QUOTE-MSG-KEY)) )
        (enforce (= fungible (at 'fungible quote-spec)) (format "Offer is restricted to sale using fungible: {}" [fungible]))
      )
    )
  )
  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce false "not enabled")
    (require-capability (BUY-CALL (at "id" token) seller buyer amount sale-id ROYALTY-POLICY))
    (enforce-sale-pact sale-id)
    (bind (get-royalty token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'royalty-rate:= royalty-rate:decimal
      }
      (let* ( (quote-spec:object{quote-schema} (get-quote-info sale-id))
              (sale-price:decimal (at 'sale-price quote-spec))
              (escrow-account:string (at 'account (get-escrow-account sale-id)))
              (royalty-payout:decimal
                 (floor (* sale-price royalty-rate) (fungible::precision))))
        (if
          (> royalty-payout 0.0)
          (let ((_ ""))
            (install-capability (fungible::TRANSFER escrow-account creator sale-price))
            (emit-event (ROYALTY-PAYOUT sale-id (at 'id token) royalty-payout creator))
            (fungible::transfer escrow-account creator royalty-payout))
          "No royalty"
          )))
        true)
  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
      true
  )
  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    true
  )
)
