(module simplesales18 GOVERNANCE

  (use fungible-util)
  (use util.guards)

  (defcap GOVERNANCE ()
  (with-read admin18 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
  ))

  (defconst ZTRUST_BANK:string 'ztrust-bank18)
  (defconst percFee:decimal 0.005)
  (defconst CF:string "CF")
  (defconst SL:string "SL")
  (defconst VP:string "VP")
  (defconst SF:string "SF")
  (defconst TO:string "TO")
  (defconst zeroAmount:decimal 0.0)

  (defun ztrust-bank-guard () (create-module-guard 'admin-keyset-peters))

  (defun init (aguard:guard)
    "Create Bank and set admin..."
    (coin.create-account ZTRUST_BANK (ztrust-bank-guard))
    (insert admin18 "admin" {
      "admin": (at "sender" (chain-data)),
      "aguard": aguard
      } )
  )

  (defschema admin
    admin:string
    aguard:guard
  )

  (deftable admin18:{admin})

  (defschema ledger
    subContractId:string
    entryType:string
    received:bool
    account:string
    amount:decimal
    eventAt:time
  )

  (deftable ledger18:{ledger})

  (defschema subContract
    subContractId:string
    subContractNumber:string
    seller:string
    sguard:guard
    buyer:string
    value:decimal
    timeout:integer
    funded:decimal
    shipper:string
    trackingNumber:string
    delivered:bool
    refunded:bool
    createdAt:time
    timesOut:time)

  (deftable subContracts18:{subContract})

  (defcap CREDIT (receiver:string) true)
  (defcap DEBIT (sender:string) true)

  (defun fund:string (subContractId:string amount:decimal)
    "Fund a subContract"
    (with-read subContracts18 subContractId
      { "value":= value,
        "funded":= funded,
        "timeout":= timeout,
        "timesOut":= timesOut,
        "createdAt":= createdAt,
        "buyer":= account }
      (enforce (= funded 0.0) "Must be unfunded")
      (enforce (= value amount) "Must fund with the exact value")
      (enforce (< (get-time) timesOut) "Not timed out yet")
      (coin.transfer account ZTRUST_BANK amount)
      (update subContracts18 subContractId
        { "funded": amount })
      (let
        ((tx-id (hash {"subContractId": subContractId, "entryType": CF, "account": account, "salt": (at "block-time" (chain-data))})))
        (insert ledger18 tx-id
            { "subContractId": subContractId,
              "entryType": CF,
              "received": false,
              "account": account,
              "amount": amount,
              "eventAt": (get-time)}
        )
      )
    )
  )

  (defun updateShipping:string (subContractId:string shipper:string trackingNumber:string)
    "Update shipping info"
    (with-read subContracts18 subContractId
      { "sguard" := sguard,
        "value":= value,
        "funded":= funded,
        "timeout":= timeout,
        "timesOut":= timesOut,
        "createdAt":= createdAt,
        "seller" := account }
      (enforce-guard sguard)
      (enforce (= value funded) "Must be fully funded")
      (enforce (< (get-time) timesOut) "Not timed out yet")
      (update subContracts18 subContractId
      { "shipper": shipper,
        "trackingNumber": trackingNumber}
      )
      (let
        ((tx-id (hash {"subContractId": subContractId, "entryType": SL, "account": account, "salt": (at "block-time" (chain-data))})))
        (insert ledger18 tx-id
            { "subContractId": subContractId,
              "entryType": SL,
              "received": false,
              "account": account,
              "amount": zeroAmount,
              "eventAt": (get-time)}
        )
      )
    )
  )

  (defun close-contract:string (sf:string subContract:object{subContract})
  (with-read admin18 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
      (bind subContract { 
        "subContractId" := subContractId,
        "buyer" := buyer,
        "value" := value,
        "funded" := funded,
        "delivered" := delivered,
        "refunded" := refunded }
        (if (= value funded) 
        (if (= false delivered)
        (if (= false refunded)
          (refund subContractId buyer value)
        "") "") "")
      )
    )
  )

  (defun refund:string (subContractId:string buyer:string value:decimal)
  (with-read admin18 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
      (install-capability (coin.TRANSFER ZTRUST_BANK buyer value))
      (coin.transfer ZTRUST_BANK buyer value)
      (update subContracts18 subContractId
        { "funded": 0.0,
          "refunded": true})
      (let
        ((tx-id (hash {"subContractId": subContractId, "entryType": TO, "account": buyer, "salt": (at "block-time" (chain-data))})))
        (insert ledger18 tx-id
            { "subContractId": subContractId,
              "entryType": TO,
              "received": false,
              "account": buyer,
              "amount": value,
              "eventAt": (get-time)}
        )
      )
    )
  )

  (defun checkTimeout ()
    "Check for timed out contracts"
  (with-read admin18 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
      (let ((cont (select subContracts18 
        (and?
        (where 'timesOut (> (get-time)))
        (where 'delivered (= false)) 
        ))))
        (fold (close-contract) "" cont) 
      )    
    )
  )

  (defun listTimedOut ()
    "List timed out contracts"
     (select subContracts18 (where 'timesOut (> (get-time))))
  )

  (defun setDelivered (subContractId)
    "Set delivered"
      (with-read admin18 "admin"
        { "aguard" := aguard,
          "admin" := admin }
        (enforce-guard aguard)
        (with-read subContracts18 subContractId
          { "seller" := seller,
            "value" := value,
            "funded" := funded,
            "shipper":= shipper,
            "delivered" := delivered,
            "trackingNumber":= trackingNumber }
          (enforce (= value funded) "Contract must be fully funded")
          (enforce (= false delivered) "Contract cannot yet be delivered")
          (enforce (!= "" shipper) "Shipping info cannot be empty")
          (enforce (!= "" trackingNumber) "Shipping info cannot be empty")
          (let
            ( (sellerGets:decimal (* value (- 1 percFee))))   
            (install-capability (coin.TRANSFER ZTRUST_BANK seller sellerGets))
            (coin.transfer ZTRUST_BANK seller sellerGets)
            (update subContracts18 subContractId
              { "delivered": true })
              (let
                ((tx-id (hash {"subContractId": subContractId, "entryType": VP, "account": seller, "salt": (get-time)})))
                (insert ledger18 tx-id
                    { "subContractId": subContractId,
                      "entryType": VP,
                      "received": true,
                      "account": seller,
                      "amount": sellerGets,
                      "eventAt": (at "block-time" (chain-data))}
                )
              )
            (install-capability (coin.TRANSFER ZTRUST_BANK admin (- value sellerGets))) 
            (coin.transfer ZTRUST_BANK admin (- value sellerGets))
              (let
                ((tx-id (hash {"subContractId": subContractId, "entryType": SF, "account": admin, "salt": (get-time)})))
                (insert ledger18 tx-id
                    { "subContractId": subContractId,
                      "entryType": SF,
                      "received": true,
                      "account": admin,
                      "amount": (- value sellerGets),
                      "eventAt": (get-time)}
                )
              )
          )
        )
    )
  )
 
  (defun get-time:time ()
    "Get block time"
    (at "block-time" (chain-data))
  )

  (defun create-subContract (subContractId subContractNumber buyer value timeout)
    "Create a subContract"
    (enforce (>= value 0.0) "Value must be >= 0.")
    (enforce (> timeout 0) "Timeout must be > 0")
    (enforce (!= "" subContractId) "subContractId cannot be empty")
    (enforce (!= "" subContractNumber) "subContractNumber cannot be empty")
    (enforce (!= "" buyer) "buyer cannot be empty")
    (insert subContracts18 subContractId
            { "subContractId": subContractId,
              "subContractNumber": subContractNumber,
              "seller": (at "sender" (chain-data)),
              "sguard": (read-keyset "keyset"),
              "buyer": buyer,
              "value": value,
              "timeout": timeout,
              "funded": 0.0,
              "shipper": "",
              "trackingNumber": "",
              "delivered": false,
              "refunded": false,
              "createdAt": (get-time),
              "timesOut": (add-time (get-time) (days timeout))}
    )
  )

  (defun get-ledgerForAccount (account:string)
    "Get ledger entries for an account"
    (select ledger18 (where 'account (= account)))
  )

  (defun get-contractsForAccount (account:string)
    "Get subContracts for an account"
    (select subContracts18 (or? (where 'seller (= account)) (where 'buyer (= account))))
  )

  (defun get-allContracts ()
    "Get all subContracts"
    (select subContracts18 (constantly true))
  )

  (defun read-admin ()
    "Read admin guard"
    (with-read admin18 "admin"
      { "admin":= admin,
      "aguard":= aguard }
    (format "Admin guard is {}, admin {}" [aguard admin]))
  )

  (defun test-admin ()
    "Test admin guard"
  (with-read admin18 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
      (format "Admin guard success! {}" [aguard] )
    )
  )

  (defun read-subContract (subContractId)
    "Read a subContract"
    (read subContracts18 subContractId) 
  )
)
; create-table

