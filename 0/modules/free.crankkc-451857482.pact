(module crankkc GOVERN

  (use fungible-util)
  (use util.guards)

  (defconst ROW_KEY_SEPARATOR:string "/")
  (defconst RADIO01_SUBSCR_BANK:string 'radio01-subscr01-bank)
  (defconst RADIO01_SOL_BANK:string 'radio01-sol01-bank)

  (defcap GOVERN ()
  (with-read cAdmin1 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
  ))

  (defcap INTERNAL ()
    "mark some functions as internal only...."
    true
  )

  (defun init (aguard:guard)
    "Set admin...."
    (insert cAdmin1 "admin" {
      "admin": (at "sender" (chain-data)),
      "aguard": aguard
      } )
  )

  (defschema admin
    admin:string
    aguard:guard
  )

  (deftable cAdmin1:{admin})

  (defschema nodeExtra
     address: string
     oracle: bool
  )    

  (deftable nodeExtras9:{nodeExtra})

  (defschema links
     linked: string
     base: string
     active: bool
     max: decimal
     min: decimal
     chain: string
     lastTopUpAt: time
  )    

  (deftable links4:{links})

  (defschema history
     base: string
     linked: string
     amount: decimal
     chain: string
     at: time
  )    

  (deftable history2:{history})

  (defun create-card-account ()
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (k-account)
    (coin.create-account (get-base) (create-CAR_DEBIT-guard))
    (get-base)
  )    

  (defun create-card-account-for-token (token:module{fungible-v2})
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (k-account)
    (token::create-account (get-base) (create-CAR_DEBIT-guard))
    (get-base)
  )    

  (defun create-card-for-address (address)
      (with-read cAdmin1 "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (coin.create-account (+ "CCAR-" (hash address)) (create-CAR_DEBIT-guard))
        (+ "CCAR-" (hash address))
      )
  )    

  (defun create-card-for-address-for-token (address token:module{fungible-v2})
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (token::create-account (+ "CCAR-" (hash address)) (create-CAR_DEBIT-guard))
        (+ "CCAR-" (hash address))
      )
  )    

  (defun get-my-card ()
    (try false (coin.details (get-base)))
  )    

  (defun is-card-owned ()
    (= (format "{}" [(at 'guard (coin.details (get-base)))]) 
       "UserGuard {fun: free.crankkc.require-CAR_DEBIT,args: []}")
  )    

  (defun is-card-owned-for-token (token:module{fungible-v2})
    (= (format "{}" [(at 'guard (token::details (get-base)))]) 
       "UserGuard {fun: free.crankkc.require-CAR_DEBIT,args: []}")
  )    

  (defun fund (base linked)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (with-read links4 (compound-key3 base linked "0")
          {"base":= baseDb,
           "active":= active,
           "max":= max,
           "min":= min,
           "chain":= chainDb,
           "lastTopUpAt":= lastTopUpAt}
           (let* (
             (baseBalance (coin.get-balance base))
             (linkedBalance (coin.get-balance linked))
             (topUp (- max linkedBalance))
             )
             (enforce (= chainDb "0") "Chain mismatch")
             (enforce (<= linkedBalance min) "No topup needed")
             (enforce (= active true) "Link inactive")
             (enforce (> baseBalance topUp) "Insufficient balance on base")
             (enforce (> (get-time) (add-time lastTopUpAt (days 1))) "Too soon")
              (with-capability (CAR_DEBIT) 
                  (install-capability (coin.TRANSFER base linked topUp))
                  (coin.transfer base linked topUp)
              )
              (update links4 (compound-key3 baseDb linked chainDb) {"lastTopUpAt": (get-time)})
              (insert history2 (hash (+ (+ baseDb (+ linked chainDb)) (get-formatted-time)))
                {"base": baseDb,
                 "linked": linked,
                 "amount": topUp,
                 "chain": chainDb,
                 "at": (get-time)}
              )
           )
        ) 
    )
  )

  (defun fund-crosschain (base:string linked:string chain:string chainBalance:decimal)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (with-read links4 (compound-key3 base linked chain)
          {"base":= baseDb,
           "active":= active,
           "max":= max,
           "min":= min,
           "chain":= chainDb,
           "lastTopUpAt":= lastTopUpAt}
           (let* (
             (baseBalance (coin.get-balance base))
            ;  (linkedBalance (coin.get-balance linked))
             (topUp (- max chainBalance))
             )
             (enforce (= chain "19") "Only other chain for now")
             (enforce (= chainDb chain) "Chain mismatch")
             (enforce (>= chainBalance 0.0) "Balance can't be negative")
             (enforce (<= chainBalance min) "No topup needed")
             (enforce (= active true) "Link inactive")
             (enforce (> baseBalance topUp) "Insufficient balance on base")
             (enforce (> (get-time) (add-time lastTopUpAt (days 1))) "Too soon")
              (with-capability (CAR_DEBIT) 
                  (install-capability (coin.TRANSFER_XCHAIN baseDb linked topUp chain))
                  (coin.transfer-crosschain baseDb linked (at 'guard (coin.details linked)) chain topUp)
              )
              (update links4 (compound-key3 baseDb linked chain) {"lastTopUpAt": (get-time)})
              (insert history2 (hash (+ (+ baseDb linked) (get-formatted-time)))
                {"base": baseDb,
                 "linked": linked,
                 "amount": topUp,
                 "chain": chain,
                 "at": (get-time)}
              )
           )
        )
    )
  )

;   (defun copy-link (account chain)
;     (with-read links4 (compound-key account chain)
;       {"linked":= accountDb,
;       "base":= baseDb,
;       "active":= activeDb,
;       "max":= maxDb,
;       "min":= minDb,
;       "chain":= chainDb,
;       "lastTopUpAt":= lastTopUpAtDb}
;         (insert links4 (compound-key3 baseDb accountDb chainDb)
;           {"linked": accountDb,
;           "base": baseDb,
;           "active": activeDb,
;           "max": maxDb,
;           "min": minDb,
;           "chain": chainDb,
;           "lastTopUpAt": lastTopUpAtDb}
;           )
;     )
;   )    

  (defun create-link (account chain min:decimal max:decimal)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (k-account)
    (let ((owned (is-card-owned))) 
    (enforce (= owned true) "Card not owned"))
    (enforce (>= min 0.0) "Min can't be negative")
    (enforce (> max min) "Max must be greater than min")
    (insert links4 (compound-key3 (get-base) account chain)
      {"linked": account,
       "base": (get-base),
       "active": true,
       "max": max,
       "min": min,
       "chain": chain,
       "lastTopUpAt": (get-epoch)}
      )
  )    

  (defun activate-link (account chain min:decimal max:decimal)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (k-account)
    (let ((owned (is-card-owned))) 
    (enforce (= owned true) "Card not owned"))
    (enforce (>= min 0.0) "Min can't be negative")
    (enforce (> max min) "Max must be greater than min")
    (with-default-read links4 (compound-key3 (get-base) account chain)
        {"linked": "",
         "base": (get-base)}
        {"linked":= accountDb,
         "base":= baseDb}
        (enforce (= baseDb (get-base)) "Base mismatch")
        (if (= accountDb "")
            (insert links4 (compound-key3 (get-base) account chain)
              {"linked": account,
               "base": (get-base),
               "active": true,
               "max": max,
               "min": min,
               "chain": chain,
               "lastTopUpAt": (get-epoch)}
            )
            (update links4 (compound-key3 (get-base) account chain)
              {"active": true,
               "max": max,
               "min": min}
            )
        )
    )
  )    

  (defun set-min-max (account chain min:decimal max:decimal)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (k-account)
    (enforce (>= min 0.0) "Min can't be negative")
    (enforce (> max min) "Max must be greater than min")
    (with-read links4 (compound-key3 (get-base) account chain)
        {"linked":= accountDb,
         "base":= baseDb}
        (enforce (= baseDb (get-base)) "Base mismatch")
        (update links4 (compound-key3 (get-base) account chain)
          {"max": max,
           "min": min}
        )
    )
  )    

  (defun deactivate-link (account chain)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (k-account)
    (with-read links4 (compound-key3 (get-base) account chain)
        {"linked":= accountDb,
         "base":= baseDb}
        (enforce (= baseDb (get-base)) "Base mismatch")
        (update links4 (compound-key3 (get-base) account chain)
          {"active": false}
        )
    )
  )    

  (defun withdraw-from-card (account amount)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (k-account)
    (with-capability (CAR_DEBIT) 
      (install-capability (coin.TRANSFER (get-base) account amount))
      (coin.transfer (get-base) account amount)
    )
  )    

  (defun withdraw-from-card-for-token (account amount token:module{fungible-v2})
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (k-account)
    (with-capability (CAR_DEBIT) 
      (install-capability (token::TRANSFER (get-base) account amount))
      (token::transfer (get-base) account amount)
    )
  )    

  (defun get-base ()
    (+ "CCAR-" (hash (at "sender" (chain-data))))
  )

  (defun get-base-for-address (address:string )
    (+ "CCAR-" (hash address))
  )

  (defun k-account ()
    (enforce (= (take 2 (at "sender" (chain-data))) "k:") "Has to be a k: address")
    (enforce (= (length (at "sender" (chain-data))) 66) "Has to be a k: address")
  )

  (defun get-my-linked ()
    (select links4 (where 'base (= (get-base))))
  )

  (defun get-links ()
    (select links4 (constantly true))
  )

  (defun get-links-keys ()
    (keys links4)
  )

  (defun account-exists (address token:module{fungible-v2})
    (try -1.0 (token::get-balance address))
  )

  (defun compound-key2:string
    ( part1:string
      part2:string )
    (format "{}{}{}" [part1 ROW_KEY_SEPARATOR part2])
  )

  (defun compound-key3:string
    ( part1:string
      part2:string
      part3:string )
    (format "{}{}{}{}{}" [part1 ROW_KEY_SEPARATOR part2 ROW_KEY_SEPARATOR part3])
  )

  (defun update-node-extra (address oracle)
      (with-read cAdmin1 "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-default-read nodeExtras9 address 
          {"address": "", "oracle": false}
          {"address":= addressDb, "oracle":= oracleDb}
          (if (= addressDb "")
          (insert nodeExtras9 address {
              "address": address,
              "oracle": oracle
              }
          )
          (update nodeExtras9 address {
              "oracle": oracle
              }
          )
          )    
        )
      )
  )

  (defschema exchangeRate
     curr1: string 
     curr2: string
     rate: decimal
  )    

  (deftable exchangeRates1:{exchangeRate})

  (defun update-USDtoCRKK (USDtoCRKK:decimal)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (with-default-read exchangeRates1 "USD/CRKK"
            {"rate": 0.0}
            {"rate":= rate}
            (if (= rate 0.0)
                (insert exchangeRates1 "USD/CRKK"
                    {"curr1": "USD",
                    "curr2": "CRKK",
                    "rate": USDtoCRKK}
                )
                (update exchangeRates1 "USD/CRKK"
                    {"rate": USDtoCRKK}
                )
            )
        )
    )
  )

  (defun update-USDtoKDA (USDtoKDA:decimal)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (with-default-read exchangeRates1 "USD/KDA"
            {"rate": 0.0}
            {"rate":= rate}
            (if (= rate 0.0)
                (insert exchangeRates1 "USD/KDA"
                    {"curr1": "USD",
                    "curr2": "KDA",
                    "rate": USDtoKDA}
                )
                (update exchangeRates1 "USD/KDA"
                    {"rate": USDtoKDA}
                )
            )
        )
    )
  )

  (defschema subscriptionMenu
    subscriptionId:string
    periodDays:integer
    value:decimal
    valueUnit:string
    paidIn:string
    feeWallet:string
  )    
  (deftable subscriptionMenu5:{subscriptionMenu})

  (defun modify-subscription-menu (   
    subscriptionId:string
    periodDays:integer
    value:decimal
    valueUnit:string
    paidIn:string
    feeWallet:string)
    (with-read cAdmin1 "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-default-read subscriptionMenu5 subscriptionId
          {"subscriptionId": ""}
          {"subscriptionId":= subscriptionIdDb}
            (if (= subscriptionIdDb "")  
                (insert subscriptionMenu5 subscriptionId
                {
                "subscriptionId": subscriptionId,
                "periodDays": periodDays,
                "value": value,
                "valueUnit":valueUnit,
                "paidIn": paidIn,
                "feeWallet": feeWallet
                })
                (update subscriptionMenu5 subscriptionId
                {
                "subscriptionId": subscriptionId,
                "periodDays": periodDays,
                "value": value,
                "valueUnit":valueUnit,
                "paidIn": paidIn,
                "feeWallet": feeWallet
                })
            )
        )
    )
  )    

  (defschema parameters
    name:string
    value:string
  )    

  (defschema subscription
    address:string
    subscriptionId:string
    purchaseDate:time
    validUntil:time
    active:bool
    params:[object{parameters}]
  )    
  (deftable subscriptions7:{subscription})

  (defschema subscriptionExtra
    purchasePrice:decimal
    termsViolated:bool
  )    
  (deftable subscriptionExtras7:{subscriptionExtra})

 (defun buy-subscription (subscriptionId:string params:[object{parameters}])
    ; (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (with-read subscriptionMenu5 subscriptionId
        {
        "periodDays":= periodDays,
        "value":= value,
        "valueUnit":= valueUnit,
        "paidIn":= paidIn
        }
        (insert subscriptions7 (compound-key3 (at "sender" (chain-data)) subscriptionId (at 'value (at 0 params)))
            {
            "address": (at "sender" (chain-data)),
            "subscriptionId": subscriptionId,
            "purchaseDate": (get-time),
            "validUntil": (add-time (get-time) (days periodDays)),
            "active":true,
            "params": params
            }
        )
        (insert subscriptionExtras7 (compound-key3 (at "sender" (chain-data)) subscriptionId (at 'value (at 0 params)))
            {
            "purchasePrice": (get-subscription-price subscriptionId),
            "termsViolated": false
            }
        )
        (if (= paidIn "KDA")
            (coin.transfer (at "sender" (chain-data)) RADIO01_SUBSCR_BANK
             (get-subscription-price subscriptionId))
            (if (= paidIn "CRKK")
                (free.crankk01.transfer (at "sender" (chain-data)) RADIO01_SUBSCR_BANK
                (get-subscription-price subscriptionId))
                (enforce false "Not supported payment token")
            )
        )
    )
  )    

  (defschema requestToSol
    address:string
    key:string
    solAddress:string
    amount:decimal
    timestamp:time
    hash:string
    mintedOnSol:bool
    mintDetails:string
  )    
  (deftable requestToSol3:{requestToSol})

 (defun request-to-sol (amount:decimal solAddress:string)
        (insert requestToSol3 (compound-key2 (at "sender" (chain-data)) (get-formatted-time))
            {
            "address": (at "sender" (chain-data)),
            "key": (compound-key2 (at "sender" (chain-data)) (get-formatted-time)),
            "solAddress": solAddress,
            "amount": amount,
            "timestamp": (get-time),
            "hash": (tx-hash),
            "mintedOnSol": false,
            "mintDetails": ""
            }
        )
        (free.crankk01.transfer (at "sender" (chain-data)) RADIO01_SOL_BANK amount)
  )    

 (defun minted-on-sol (key:string mintDetails:string)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (update requestToSol3 (compound-key2 (at "sender" (chain-data)) (get-formatted-time))
            {
            "mintedOnSol": true,
            "mintDetails": mintDetails
            }
        )
    )
  )    

 (defun buy-subscription-w-card (subscriptionId:string params:[object{parameters}])
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (k-account)
    (with-read subscriptionMenu5 subscriptionId
        {
        "periodDays":= periodDays,
        "value":= value,
        "valueUnit":= valueUnit,
        "paidIn":= paidIn
        }
        (insert subscriptions7 (compound-key3 (at "sender" (chain-data)) subscriptionId (at 'value (at 0 params)))
            {
            "address": (at "sender" (chain-data)),
            "subscriptionId": subscriptionId,
            "purchaseDate": (get-time),
            "validUntil": (add-time (get-time) (days periodDays)),
            "active":true,
            "params": params
            }
        )
        (insert subscriptionExtras7 (compound-key3 (at "sender" (chain-data)) subscriptionId (at 'value (at 0 params)))
            {
            "purchasePrice": (get-subscription-price subscriptionId),
            "termsViolated": false
            }
        )
        (if (= paidIn "KDA")
            (with-capability (CAR_DEBIT) 
              (install-capability (coin.TRANSFER (get-base) RADIO01_SUBSCR_BANK (get-subscription-price subscriptionId)))
              (coin.transfer (get-base) RADIO01_SUBSCR_BANK (get-subscription-price subscriptionId))
            )
            (if (= paidIn "CRKK")
                (with-capability (CAR_DEBIT) 
                  (install-capability (free.crankk01.TRANSFER (get-base) RADIO01_SUBSCR_BANK (get-subscription-price subscriptionId)))
                  (free.crankk01.transfer (get-base) RADIO01_SUBSCR_BANK (get-subscription-price subscriptionId))
                )
                (enforce false "Not supported payment token")
            )
        )
    )
  )    

 (defun insert-subscription (primAddress:string pDate:string subscriptionId:string active:bool params:[object{parameters}])
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (with-read subscriptionMenu5 subscriptionId
            {
            "periodDays":= periodDays,
            "value":= value,
            "valueUnit":= valueUnit,
            "paidIn":= paidIn
            }
            (insert subscriptions7 (compound-key3 primAddress subscriptionId (at 'value (at 0 params)))
                {
                "address": primAddress,
                "subscriptionId": subscriptionId,
                "purchaseDate": (time pDate),
                "validUntil": (add-time (time pDate) (days periodDays)),
                "active": active,
                "params": params
                }
            )
        )
    )
  )    
  (defschema subsHistory
    card:string
    subscriptionId:string
    params:[object{parameters}]
    paidIn:string
    amount:decimal
    ts:time
  )

  (deftable subsHistory1:{subsHistory})


 (defun validity-test (primAddress:string subscriptionId:string params:[object{parameters}])
    (with-read cAdmin1 "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (update subscriptions7 (compound-key3 primAddress subscriptionId (at 'value (at 0 params)))
            {
            "validUntil": (get-time)
            }
        )
    )
 )

 (defun renew-subscription (primAddress:string subscriptionId:string params:[object{parameters}])
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
    (with-read subscriptionMenu5 subscriptionId
        {
        "periodDays":= periodDays,
        "value":= value,
        "valueUnit":= valueUnit,
        "paidIn":= paidIn
        }
        (with-read subscriptions7 (compound-key3 primAddress subscriptionId (at 'value (at 0 params)))
            {
            "validUntil":= validUntil,
            "active":= active
            }
            (enforce active "Subscription must be active")
            (enforce (< (diff-time validUntil (get-time)) 3600.0) "Not yet due for renewal")
            (update subscriptions7 (compound-key3 primAddress subscriptionId (at 'value (at 0 params)))
                {
                "validUntil": (add-time (get-time) (days periodDays))
                }
            )
            (if (= paidIn "KDA")
                (with-capability (CAR_DEBIT) 
                  (install-capability (coin.TRANSFER (get-base-for-address primAddress) RADIO01_SUBSCR_BANK (get-renewal-price subscriptionId)))
                  (coin.transfer (get-base-for-address primAddress) RADIO01_SUBSCR_BANK (get-renewal-price subscriptionId))
                )
                (if (= paidIn "CRKK")
                    (with-capability (CAR_DEBIT) 
                      (install-capability (free.crankk01.TRANSFER (get-base-for-address primAddress) RADIO01_SUBSCR_BANK (get-renewal-price subscriptionId)))
                      (free.crankk01.transfer (get-base-for-address primAddress) RADIO01_SUBSCR_BANK (get-renewal-price subscriptionId))
                    )
                    (enforce false "Not supported payment token")
                )
            )
            (insert subsHistory1 (compound-key3 (get-base-for-address primAddress) subscriptionId (get-formatted-time))
                {
                "card": (get-base-for-address primAddress),
                "subscriptionId": subscriptionId,
                "params":params,
                "paidIn": paidIn,
                "amount": (get-renewal-price subscriptionId),
                "ts": (get-time)
                }
            )
        )
    )
    )
  )    

 (defun subscription-on-off (subscriptionId:string params:[object{parameters}] switch:bool)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (k-account)
    (with-read subscriptionMenu5 subscriptionId
        {
        "periodDays":= periodDays,
        "value":= value,
        "valueUnit":= valueUnit,
        "paidIn":= paidIn
        }
        (update subscriptions7 (compound-key3 (at "sender" (chain-data)) subscriptionId (at 'value (at 0 params)))
            {
            "active":switch
            }
        )
    )
  )    

  (defun get-subscription-price (subscriptionId)
    "Get subscription price"
    (with-read subscriptionMenu5 subscriptionId
        {
        "value":= value,
        "valueUnit":= valueUnit,
        "paidIn":= paidIn
        }
        (with-read exchangeRates1 (compound-key2 valueUnit paidIn)
            {"rate":= rate}
            ; (round (* value rate) 2)
            (round (* (get-sale-value subscriptionId value) rate) 2)
        )
    )    
  )

  (defun get-renewal-price (subscriptionId)
    "Get subscription price"
    (with-read subscriptionMenu5 subscriptionId
        {
        "value":= value,
        "valueUnit":= valueUnit,
        "paidIn":= paidIn
        }
        (with-read exchangeRates1 (compound-key2 valueUnit paidIn)
            {"rate":= rate}
            ; (round (* value rate) 2)
            (round (* (get-renew-value subscriptionId value) rate) 2)
        )
    )    
  )

  (defun get-exchange-rates ()
    "Get exchange-rates"
    (select exchangeRates1 (constantly true))
  )

  (defun get-subscription-menu ()
    "Get subscription-menu"
    (select subscriptionMenu5 (constantly true))
  )

  (defun get-subscription-menu-new ()
    "Get subscription-menu new"
    ; (let ((respArray [])
    (fold (calculate-sale) [] (keys subscriptionMenu5))
    ;   respArray
    ; )
  )

   (defun calculate-sale (respArray key)
    (with-read subscriptionMenu5 key
        { "subscriptionId":= subscriptionId, 
          "periodDays":= periodDays,
          "value":= value,
          "valueUnit":= valueUnit,
          "paidIn":= paidIn,
          "feeWallet":= feeWallet }
        (+ respArray 
          [{"subscriptionId": subscriptionId, 
          "periodDays": periodDays,
          "value": value,
          "valueUnit": valueUnit,
          "paidIn": paidIn,
          "feeWallet": feeWallet,
          "saleValue": (get-sale-value subscriptionId value),
          "saleDesc": (get-sale-desc subscriptionId) }])
    )
  )

  (defun get-sale-value (subscriptionId value)
    (with-default-read subsDiscounts2 subscriptionId
        { "endDate": (get-time), 
          "saleValue": value }
        { "endDate":= endDate, 
          "saleValue":= saleValue }
        (if (<= endDate (get-time))
           value
           saleValue
        )  
    )
  )

  (defun get-renew-value (subscriptionId value)
    (with-default-read subsDiscounts2 subscriptionId
        { "termEndDate": (get-time), 
          "saleValue": value }
        { "termEndDate":= termEndDate, 
          "saleValue":= saleValue }
        (if (<= termEndDate (get-time))
           value
           saleValue
        )  
    )
  )

  (defun get-sale-desc (subscriptionId)
    (with-default-read subsDiscounts2 subscriptionId
        { "endDate": (get-time), 
          "description": ""}
        { "endDate":= endDate, 
          "description":= description}
        (if (<= endDate (get-time))
           ""
           description
        )  
    )
  )

  (defschema subsDiscounts
      subscriptionId:string
      beginDate:time
      endDate:time
      saleValue:decimal
      saleDisc:decimal
      termEndDate:time
      interruptionAllowed:bool
      description:string
  )    
  (deftable subsDiscounts2:{subsDiscounts})

  (defun get-subsDiscounts ()
    (select subsDiscounts2 (constantly true))
  )

  (defun add-subsDiscount (
    subscriptionId:string
    beginDate:string
    endDate:string
    saleValue:decimal
    saleDisc:decimal
    termEndDate:string
    interruptionAllowed:bool
    description:string)
    "Add subscription discount"
    (with-read cAdmin1 "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
    
        (insert subsDiscounts2 subscriptionId
          {"subscriptionId": subscriptionId,
           "beginDate": (time beginDate),
           "endDate":(time endDate),
           "saleValue":saleValue,
           "saleDisc":saleDisc,
           "termEndDate":(time termEndDate),
           "interruptionAllowed":interruptionAllowed,
           "description":description
          }
        )
    )
  )

  (defun get-subscriptions ()
    "Get subscriptions"
    (select subscriptions7 (constantly true))
  )


  (defun get-subscriptions-keys ()
    "Get subscriptions keys"
    (keys subscriptions7)
  )

  (defun get-subscriptionMenu-keys ()
    "Get subscriptions keys"
    (keys subscriptionMenu5)
  )


  (defun get-subscriptions-renew-history ()
    "Get subscriptions renew history"
    (select subsHistory1 (constantly true))
  )


  (defun get-time ()
    (at "block-time" (chain-data))
  )

  (defun get-formatted-time:string ()
    "Get formatted time"
    (format-time "%Y-%m-%dT%H:%M:%S" (get-time))
  )

  (defun get-epoch:time ()
    "Get epoch"
    (time "1970-01-01T00:00:00Z")
  )

  (defun get-token-key
    ( token:module{fungible-v2}
    )
    "Token key"
    (format "{}" [token])
  )

  (defun read-admin ()
    "Read admin guard"
    (with-read cAdmin1 "admin"
      { "admin":= admin,
      "aguard":= aguard }
    (format "Admin guard is {}, admin {}" [aguard admin]))
  )

  (defun get-nodeExtras () 
    (select nodeExtras9 (constantly true)) 
  )

  (defun get-sol-requests () 
    (select requestToSol3 (constantly true)) 
  )

  (defun get-history () 
    (select history2 (constantly true)) 
  )

  (defun get-my-history () 
    (select history2 (where 'base (= (get-base)))) 
  )

;; Invoking capability definition
(defcap WITHDRAW (recipient:string amount:decimal)
  (compose-capability (CAR_DEBIT))
)
;; Capability user guard: capability definition
(defcap CAR_DEBIT () true)
;; Capability user guard: capability predicate function
(defun require-CAR_DEBIT () 
  (require-capability (CAR_DEBIT)))

;; Capability user guard: guard constructor
(defun create-CAR_DEBIT-guard ()
  (create-user-guard (require-CAR_DEBIT)))

(defun init-subscr-bank ()
      (coin.transfer-create (at "sender" (chain-data)) RADIO01_SUBSCR_BANK 
        (create-CAR_DEBIT-guard) 0.01)
)

(defun init-sol-bank ()
      (free.crankk01.transfer-create (at "sender" (chain-data)) RADIO01_SOL_BANK 
        (create-CAR_DEBIT-guard) 0.01)
)
)
; create-table
; (create-table cAdmin1)
; (create-table nodeExtras9)
; (create-table links4)
; (create-table subscriptions7)
; (create-table subscriptionMenu5)
; (create-table subsHistory1)
; (create-table subsDiscounts2)
; (create-table subscriptionExtras7)

