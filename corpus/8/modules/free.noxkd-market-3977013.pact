(module noxkd-market GOVERNANCE
  

  @doc "noxkd marketplace"
  
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "noxkd-admin")))
  
  (use marmalade.ledger)
  (use kip.token-policy-v1 [token-info])
  
  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")
  
  (defconst noxkd-market (read-keyset "noxkd-market"))
  (defconst noxkd-operator (read-keyset "noxkd-operator"))

  (defcap OPERATOR ()
    (enforce-guard noxkd-operator ))

  (defschema quote-spec
    price:decimal
    recipient:string
    recipient-guard:guard
  )

  (defschema token-fees
    id:string
    fee:decimal )

  (defschema sale-info
    sale-id:string
    token-id:string
    collection-id:string
    amount:decimal
    price:decimal
    creator:string
    royalty-payout:decimal
    market-fee:decimal
    other-fees-bool:bool
    other-fees:[object{token-fees}]
    timeout:integer
    recipient:string
  ) 

  (defschema transaction-data
    "transaction details"
    sale-id:string
    token-id:string
    amount:decimal
    price:decimal
    timeout:integer
    seller:string
    recipient:string
  )

  (defschema collection
    "schema for marketplace collections"
    collection-id:string
    collection-size:integer
    collection-hash:string
    tokens:[string]
    creator:string
    royalty-payout:decimal
    market-fee:decimal
    other-fees-bool:bool
    other-fees:[object{token-fees}]
    fungible:module{fungible-v2}
    policy:module{kip.token-policy-v1}
  )
  
  (deftable transactions-data:{transaction-data})
  (deftable collections:{collection})

  (defcap TRANSACTION-FEE:bool (operator:string market-fee:decimal) 
    @event
    true )
    
  (defcap SALE-PAKT:bool
    (token-id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until TIMEOUT block height."
    @event
    (enforce (> amount 0.0) "Amount must be positive")
      (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
              (price (at 'price spec))
              (recipient (at 'recipient spec)) )     
         (insert transactions-data  sale-id {
           "sale-id": sale-id
          ,"token-id": token-id
          ,"amount": amount
          ,"price": price
          ,"timeout": timeout
          ,"seller": seller
          ,"recipient": recipient
         } ))
         true
  )

  (defun new-collection:bool
    (collection-id:string
     collection-size:integer
     collection-hash:string
     tokens:[string]
     creator:string
     royalty-payout:decimal
     market-fee:decimal
     other-fees-bool:bool
     other-fees:[object{token-fees}]
     fungible:module{fungible-v2}
     policy:module{kip.token-policy-v1}
     )
     (with-capability (OPERATOR)
       (insert collections collection-id {
         "collection-id": collection-id
        ,"collection-size": collection-size
        ,"collection-hash": collection-hash
        ,"tokens": tokens
        ,"creator":creator
        ,"royalty-payout":royalty-payout
        ,"market-fee":market-fee
        ,"other-fees-bool":other-fees-bool
        ,"other-fees":other-fees
        ,"fungible": fungible
        ,"policy": policy
       }
       )
     )
     true
  )

  (defun update-collection:bool
    (collection-id:string
     collection-size:integer
     collection-hash:string
     tokens:[string]
     creator:string
     royalty-payout:decimal
     market-fee:decimal
     other-fees-bool:bool
     other-fees:[object{token-fees}]
     fungible:module{fungible-v2}
     policy:module{kip.token-policy-v1}
     )
     (with-capability (GOVERNANCE)
       (update collections collection-id {
         "collection-id": collection-id
        ,"collection-size": collection-size
        ,"collection-hash": collection-hash
        ,"tokens": tokens
        ,"creator":creator
        ,"royalty-payout":royalty-payout
        ,"market-fee":market-fee
        ,"other-fees-bool":other-fees-bool
        ,"other-fees":other-fees
        ,"fungible": fungible
        ,"policy": policy
       }
       )
     )
     true
  )
            
  (defun market-fee:decimal ( token-id:string price:decimal )
    (let* ( (collection-info:object{collection} ( get-token-collection-info token-id ))
            (collection-id ( at 'collection-id collection-info))
            (enforce (!= "" collection-id) "your collection is not in our database yet")
            (market-fee:decimal ( at 'market-fee collection-info)))
      ( if (< 0.0 market-fee )  
          (- (floor (* price (+ 1.0 market-fee)) 4) price)
          0.0 )))

  (defun fee-payment:bool ( sale-id:string buyer:string)  
  (let* (( sale-info (get-sale-info sale-id)) 
        (market-fee (at 'market-fee sale-info))
        (collection-id (at 'collection-id sale-info)))
        (with-read collections collection-id
            { 'fungible := fungible:module{fungible-v2} }
        (if (< 0.0 market-fee)    
            (fungible::transfer buyer (create-principal noxkd-market ) market-fee ) 
            "no market charge") ))
      true )

  ;;GET FUNCTIONS

  (defun get-collections:[object{collection}] ()
    (select collections [ "collection-id" ] (where "collection-id" (!= "") )))

  (defun get-collection-info:object{collection} (collection-id:string )
    (read collections collection-id ))
  
  (defun get-collection-name:string (token-id:string)
    ( let* ( (collection (select collections [ "collection-id" ] (where "tokens" (contains token-id) )))
             (enforce (< 0 (length collection) ) "your collection is not in our database yet")
             (collection-name (at 'collection-id (at 0 collection)) ))
      collection-name ))

  (defun get-token-collection-info:object{collection} (token-id:string)
    (let ( (collection-name:string (get-collection-name token-id) ))
         (read collections collection-name)))
  
  (defun get-sale-info:object{sale-info} (sale-id)
    (with-read transactions-data sale-id {
      "sale-id":= sale-id:string
     ,"token-id":= token-id:string
     ,"amount":= amount:decimal
     ,"price":= price:decimal
     ,"timeout":=timeout:integer
     ,"recipient":= recipient:string
     }
    (let* ( (collection-info:object{collection} ( get-token-collection-info token-id ))  
            (collection-id (at 'collection-id collection-info))
            (creator (at 'creator collection-info))
            (royalty-payout (at 'royalty-payout collection-info))
            (other-fees-bool (at 'other-fees-bool collection-info))
            (other-fees (at 'other-fees collection-info))
            (market-fee (market-fee token-id (* price amount) )))
            {
              'sale-id: sale-id
              ,'token-id: token-id
              ,"collection-id": collection-id
              ,'amount: amount
              ,'price: price
              ,'creator: creator
              ,'royalty-payout: royalty-payout
              ,'market-fee: market-fee
              ,'other-fees-bool: other-fees-bool
              ,'other-fees: other-fees
              ,'timeout: timeout
              ,'recipient: recipient
            }
            ))
  )

  (defun get-transaction-info:object{transaction-data} (sale-id) 
    (read transactions-data sale-id))

  (defun get-transaction-info-all:object{transaction-data} () 
  (with-capability (OPERATOR)
  (select transactions-data (where "sale-id" (!= "") )))) 

  (defpact sale-token  
      ( token-id:string
        seller:string
        amount:decimal
        timeout:integer
      )
      (step-with-rollback
        (let* ((spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
              (price (at 'price spec))
              (sale-id (pact-id))
              ( token-sale 
        (with-capability (SALE-PAKT token-id seller amount timeout sale-id)
        (with-capability (TRANSACTION-FEE (create-principal noxkd-market) (market-fee token-id (* amount price)))
        (marmalade.ledger.sale  token-id seller amount timeout) )) ))
         (format "timed escrow: {}, id: {}" [token-sale sale-id]) )
        (let (( token-sale 
        (continue (marmalade.ledger.sale token-id seller amount timeout))))
        (format "withdrawal: {}, token-id {}" [token-sale token-id]) ))
      (step
        (let* ((fee (fee-payment (pact-id) (read-msg 'buyer )))
              ( token-sale
        (continue (marmalade.ledger.sale token-id seller amount timeout))))
           (format "transfer: {}, token-id {}" [token-sale token-id]) ))
  )
)


