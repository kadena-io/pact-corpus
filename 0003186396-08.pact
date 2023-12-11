(module mintit-policy GOVERNANCE

  ; mintit-policy 

  @doc "Policy for NFT tokens with royalty table and quoted sale in Kadena"

  (defcap GOVERNANCE ()
    (enforce-guard (read-keyset 'admin-keyset))
  )

  ; All amount parameters in various functions must be equal to 1.0
  (defconst EXC_INVALID_TOKEN_AMOUNT "invalid-token-amount")

  ; Supply must always be 1.0
  (defconst EXC_CANNOT_MINT_TWICE "cannot-mint-twice")

  ; When creating an nft, the coin.creator guard must match the policy-schema creator guard 
  (defconst EXC_MISMATCH_CREATOR_GUARD "mismatched-creator-guard")

  ; 
  (defconst EXC_MISMATCHED_RECIPIENT_GUARD "mismatched-recipient-guard")

  ;
  (defconst EXC_MISMATCHED_SALE_TOKEN "mismatched-sale-token")

  
  (implements kip.token-policy-v1)
  
  (use kip.token-policy-v1 [token-info])

  (defconst QUOTE_SPEC_MSG_KEY "nft-quote"
    @doc "Payload field for quote-spec"
  )

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
  )

  (defschema quote-schema
    id:string
    spec:object{quote-spec}
  )

  (deftable quotes:{quote-schema})

  (defun get-nft-info:object{mintit-api.nft-info}
    ( token:object{token-info}
    )
    ; Note that the token is is the NFT content hash
    (mintit-api.get-nft-info (at 'id token))
  )

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      sale-price:decimal
      royalty-payouts:object{mintit-royalty.royalty-payouts}
      creator:string
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defun enforce-ledger:bool 
    ()
    (enforce-guard (marmalade.ledger.ledger-guard))
  )

  ; TODO should we add and enforce a mint-guard in the policy-shema?
  ; TODO ensure royalties add up to mint-price
  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string ; unused 
      guard:guard ; unused
      amount:decimal
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-ledger)
    (bind token 
      { 'supply := supply 
      }
    (bind (get-nft-info token)
      { 'owner := owner 
      , 'mint-price := mint-price 
      , 'mint-royalties := mint-royalties 
      }
      (enforce (= 0.0 supply) EXC_CANNOT_MINT_TWICE)
      (mintit-royalty.create-execute-payouts mint-royalties owner mint-price)
    ))
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-ledger)
    (enforce false "burn not implemented") 
  )

  ; NOTE sale and mint royalties are verified in mintit-api.create-nft-collection
  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    true
    ; Nothing to do here
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string ; unused 
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-nft-info token)
      { 'owner := owner
      , 'sale-royalties := sale-royalties
      }
    (bind 
      { 'token-id: (at 'id token)
      , 'quote-spec: (read-msg QUOTE_SPEC_MSG_KEY)
      }
      { 'token-id := token-id:string 
      , 'quote-spec := quote-spec:object{quote-spec}
      }
    (bind sale-royalties
      { 'fungible := fungible:module{fungible-v2}
      }
    (bind quote-spec
      { 'price := sale-price 
      , 'recipient := recipient 
      , 'recipient-guard := actual-recipient-guard 
      }
    (let  
      ( 
        (sale-payouts (mintit-royalty.create-payouts sale-royalties owner sale-price))
        (quote { 'id: token-id, 'spec: quote-spec })
      )
    (bind (fungible::details recipient)
      { 'guard := expected-recipient-guard ; TODO why do we need this?
      }
      (fungible::enforce-unit sale-price)
      (enforce (= actual-recipient-guard expected-recipient-guard) EXC_MISMATCHED_RECIPIENT_GUARD)
      (insert quotes sale-id quote)
      (emit-event (QUOTE sale-id token-id sale-price sale-payouts owner quote-spec)))
    )))))
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string ; unused 
      buyer:string
      buyer-guard:guard ; unused 
      amount:decimal
      sale-id:string 
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-nft-info token)
      { 'sale-royalties := sale-royalties
      }
    (bind sale-royalties
      { 'fungible := fungible:module{fungible-v2}
      }
    (with-read quotes sale-id 
      { 'id := qtoken
      , 'spec := spec
      }
    (bind spec
      { 'price := sale-price
      , 'recipient := recipient
      }
    (let*  
      ( 
        (token-id (at 'id token))
        (sale-payouts (mintit-royalty.create-payouts sale-royalties buyer sale-price))
        (recipient-payout (- sale-price (mintit-royalty.total-payout sale-payouts)))
      )
      (enforce (= qtoken token-id) EXC_MISMATCHED_SALE_TOKEN)
      (mintit-royalty.execute-payouts sale-payouts)
      ; If the recipient is also a royalty stackholder, the capability must 
      ; sum both due amounts!
      ; We can't specify two coin.TRANSFER capabilies with the same args exect 
      ; for "amount" 
      (fungible::transfer buyer recipient recipient-payout)
    )))))
    true
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
      amount:decimal 
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal 
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )
)



