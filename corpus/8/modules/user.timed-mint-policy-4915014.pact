(module timed-mint-policy GOVERNANCE

  @doc "Policy for timed-mint tokens with royalty and quoted sale in coin."

  (defcap GOVERNANCE ()
    (enforce-guard "user.timed-mint-policy-admin" ))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defschema token_spec
    creator-guard:guard
    max-supply:decimal
    mint-start-time:time
    mint-end-time:time
    royalty-rate:decimal
  )

  (defschema policy-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    max-supply:decimal
    mint-start-time:time
    mint-end-time:time
    royalty-rate:decimal
  )

  (deftable timed-tokens:{policy-schema})

  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (deftable quotes:{quote-schema})

  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read timed-tokens (at 'id token))
  )

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      royalty-payout:decimal
      creator:string
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
   )

  (defun update-mint-date (token-id:string mint-start-time:time mint-end-time:time)
    (with-capability (GOVERNANCE)
      (update timed-tokens token-id {
        "mint-start-time": mint-start-time
       ,"mint-end-time": mint-end-time
        })
    )
  )

 (defun enforce-init:bool
   ( token:object{token-info}
   )
   (enforce-ledger)
   (let* ( (spec:object{token_spec} (read-msg TOKEN_SPEC))
           (creator-guard:guard (at 'creator-guard spec))
           (creator:string (create-principal creator-guard))
           (max-supply:decimal (at 'max-supply spec))
           (mint-start-time:time (at 'mint-start-time spec))
           (mint-end-time:time (at 'mint-end-time spec))
           (royalty-rate:decimal (at 'royalty-rate spec))
           (creator-details:object (coin.details creator ))
           )
     (enforce (= (at 'precision token) 0) "Invalid precision")
     (enforce (>= max-supply 0.0) "Invalid max-supply")
     (enforce (=
       (at 'guard creator-details) creator-guard)
       "Creator guard does not match")
     (enforce (and
       (>= royalty-rate 0.0) (<= royalty-rate 1.0))
       "Invalid royalty rate")
     (insert timed-tokens (at 'id token)
       { 'fungible: coin
       , 'creator: creator
       , 'creator-guard: creator-guard
       , 'max-supply: max-supply
       , 'mint-start-time:mint-start-time
       , 'mint-end-time:mint-end-time
       , 'royalty-rate: royalty-rate }))
   true
 )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (let* ( (account-bal:decimal (try 0.0 (at 'balance (marmalade.ledger.details (at 'id token) account))))
            (policy-info:object{policy-schema} (get-policy token))
            (max-supply:decimal (at 'max-supply policy-info))
            (mint-start-time:time (at 'mint-start-time policy-info))
            (mint-end-time:time (at 'mint-end-time policy-info)) )
      (enforce (= account-bal 0.0) "Account has already minted")
      (enforce (= amount 1.0) "Mint amount can only be 1.0")
      (enforce (>= (at 'block-time (chain-data))  mint-start-time ) "Mint has not started yet")
      (enforce (<= (at 'block-time (chain-data))  mint-end-time ) "Mint has ended")
      (if (> 0.0 max-supply)
        (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
        true
      ))
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
       ,'royalty-rate:= royalty-rate:decimal
       ,'creator:= creator:string
      }
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price))
            (royalty-payout:decimal
               (floor (* sale-price royalty-rate) (fungible::precision))) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout creator spec)))
      true
    )
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
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'royalty-rate:= royalty-rate:decimal
      }
      (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          }
          (let* ((sale-price:decimal (* amount price))
                 (royalty-payout:decimal
                    (floor (* sale-price royalty-rate) (fungible::precision)))
                 (payout:decimal (- sale-price royalty-payout)) )
            (if
              (> royalty-payout 0.0)
              (fungible::transfer buyer creator royalty-payout)
              "No royalty")
            (fungible::transfer buyer recipient payout)))
            true
        ))
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
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )
)


