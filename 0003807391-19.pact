(module crankkAffs GOVERNANCE

;   (use fungible-util)
  (use util.guards)

  (defconst ROW_KEY_SEPARATOR:string "/")

  (defcap GOVERNANCE ()
  (with-read radmin "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
  ))

  (defcap INTERNAL ()
    "mark some functions as internal only...."
    true
  )

  (defun init (aguard:guard)
    "Set admin...."
    (insert radmin "admin" {
      "admin": (at "sender" (chain-data)),
      "aguard": aguard
      } )
  )

  (defschema admin
    admin:string
    aguard:guard
  )

  (deftable radmin:{admin})


  (defun get-time:time ()
    "Get block time"
    (at "block-time" (chain-data))
  )

  (defun get-epoch:time ()
    "Get epoch"
    (time "1970-01-01T00:00:00Z")
  )

  (defun get-formattedTime:string (t:time)
    "Get formatted time"
    (format-time "%Y-%m-%dT%H:%M:%S" t)
  )

  (defun read-admin ()
    "Read admin guard"
    (with-read radmin "admin"
      { "admin":= admin,
      "aguard":= aguard }
    (format "Admin guard is {}, admin {}" [aguard admin]))
  )

  (defschema affiliate
     category: string ;User U, Influencer I, Company C, Company Agent A
     address: string
     percentage: decimal
     approved: bool
     denied: bool
     company: string ;Only for company and agents
     appliedAt: time
     decidedAt: time
     updatedAt: time
     applicationFee: decimal
  )    
  (deftable affiliates4:{affiliate})

  (defschema code
     category: string ;User U, Influencer I, Company C, Company Agent A
     address: string
     affCode: string
  )    
  (deftable codes1:{code})

  (defun is-oracle ()
    (let ((oracle (at 'oracle (read wallets1 (at "sender" (chain-data)) ['oracle]))))
      (enforce oracle "Must be an oracle")
    )
  )

  (defun add-affCode-ora (address affCode)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (is-oracle)
    (with-read affiliates4 (compound-key2 address (take 1 affCode))
        {"category":= category,
         "approved":= approved}
        (enforce (= (take 2 affCode) (+ category "_")) (+ "Must start with " (+ category "_")))
        (enforce approved "Must be approved first")
        (insert codes1 affCode
            {"category": category,
             "address": address,
             "affCode": affCode
            }
        )
    )
  )

  (defun apply-affiliate-user-ora (address)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (is-oracle)
    (insert affiliates4 (compound-key2 address "U")
        {"category": "U",
         "address": address,
         "percentage": 3.333,
         "approved": true, ;Auto approve user applications
         "denied": false,
         "company": "",
         "appliedAt": (get-time),
         "decidedAt": (get-time),
         "updatedAt": (get-epoch),
         "applicationFee": 0.0
        }
    )
  )

  (defun apply-affiliate-infl-ora (address)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (is-oracle)
    (insert affiliates4 (compound-key2 address "I")
        {"category": "I",
         "address": address,
         "percentage": 6.666,
         "approved": false,
         "denied": false,
         "company": "",
         "appliedAt": (get-time),
         "decidedAt": (get-epoch),
         "updatedAt": (get-epoch),
         "applicationFee": 0.0
        }
    )
  )

  (defun apply-affiliate-comp-ora (address company)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (is-oracle)
    (enforce (!= company "") "Company name cannot be blank")
    (insert affiliates4 (compound-key2 address "C")
        {"category": "C",
         "address": address,
         "percentage": 13.333,
         "approved": false,
         "denied": false,
         "company": company,
         "appliedAt": (get-time),
         "decidedAt": (get-epoch),
         "updatedAt": (get-epoch),
         "applicationFee": 0.0
        }
    )
  )

  (defun apply-affiliate-agnt-ora (address company)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (is-oracle)
    (insert affiliates4 (compound-key2 address "A")
        {"category": "A",
         "address": address,
         "percentage": 0.0,
         "approved": false,
         "denied": false,
         "company": company,
         "appliedAt": (get-time),
         "decidedAt": (get-epoch),
         "updatedAt": (get-epoch),
         "applicationFee": 0.0
        }
    )
  )

  (defun add-affCode (affCode)
    (with-read affiliates4 (compound-key2 (at "sender" (chain-data)) (take 1 affCode))
        {"category":= category,
         "approved":= approved}
        (enforce (= (take 2 affCode) (+ category "_")) (+ "Must start with " (+ category "_")))
        (enforce approved "Must be approved first")
        (insert codes1 affCode
            {"category": category,
             "address": (at "sender" (chain-data)),
             "affCode": affCode
            }
        )
    )
  )

  (defun apply-affiliate-user ()
    ; (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (insert affiliates4 (compound-key2 (at "sender" (chain-data)) "U")
        {"category": "U",
         "address": (at "sender" (chain-data)),
         "percentage": 3.333,
         "approved": true, ;Auto approve user applications
         "denied": false,
         "company": "",
         "appliedAt": (get-time),
         "decidedAt": (get-time),
         "updatedAt": (get-epoch),
         "applicationFee": 0.0
        }
    )
  )

  (defun apply-affiliate-infl ()
    ; (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (insert affiliates4 (compound-key2 (at "sender" (chain-data)) "I")
        {"category": "I",
         "address": (at "sender" (chain-data)),
         "percentage": 6.666,
         "approved": false,
         "denied": false,
         "company": "",
         "appliedAt": (get-time),
         "decidedAt": (get-epoch),
         "updatedAt": (get-epoch),
         "applicationFee": 0.0
        }
    )
  )

  (defun apply-affiliate-comp (company)
    ; (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (enforce (!= company "") "Company name cannot be blank")
    (insert affiliates4 (compound-key2 (at "sender" (chain-data)) "C")
        {"category": "C",
         "address": (at "sender" (chain-data)),
         "percentage": 13.333,
         "approved": false,
         "denied": false,
         "company": company,
         "appliedAt": (get-time),
         "decidedAt": (get-epoch),
         "updatedAt": (get-epoch),
         "applicationFee": 0.0
        }
    )
  )

  (defun apply-affiliate-agnt (company)
    ; (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (insert affiliates4 (compound-key2 (at "sender" (chain-data)) "A")
        {"category": "A",
         "address": (at "sender" (chain-data)),
         "percentage": 0.0,
         "approved": false,
         "denied": false,
         "company": company,
         "appliedAt": (get-time),
         "decidedAt": (get-epoch),
         "updatedAt": (get-epoch),
         "applicationFee": 0.0
        }
    )
  )

  (defun acc-exists ()
    (with-default-read affiliates4 (at "sender" (chain-data))
        {"address":""}
        {"address":= address}
        (!= address "")
    )
  )

  (defun approve-affiliate (address category decision:bool)
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (update affiliates4 (compound-key2 address category)
            {
            "approved": decision,
            "denied": (not decision),
            "decidedAt": (get-time)}
        )
      )
  )

  (defschema payouts
     address: string
     affCode: string
     paidAt: time
     amount: decimal
  )    
  (deftable payouts1:{payouts})

  (defun insert-payout (address affCode amount)
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-read affiliates4 affCode
            {
            "address":= addressDb 
            }
            (enforce (= address addressDb) "Address must match to registered")
            (insert payouts1 (compound-key2 affCode (get-formattedTime (get-time)))
                {
                 "address": address,
                 "affCode": affCode,
                 "amount": amount,
                 "paidAt": (get-time)
                }
            )
        )
    )
  )

  (defun compound-key2:string
    ( part1:string
      part2:string )
    (format "{}{}{}" [part1 ROW_KEY_SEPARATOR part2])
  )

  (defun get-affiliate (affCode)
    (with-read codes1 affCode
      {"address":= address}
      (read affiliates4 (compound-key2 address (take 1 affCode)))
    )
  )

  (defun acc-not-reg-other (category)
    (with-default-read affiliates4 (at "sender" (chain-data))
      {"category": category}      
      {"category":= categoryDb}
      (= category categoryDb)
    )
  )

  (defschema wallet
     address: string
     oracle: bool
     type: string
     updatedAt: time
  )    

  (deftable wallets1:{wallet})

  (defun update-oracle-wallet (address oracle)
  (with-read radmin "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
    (with-default-read wallets1 address 
      {"address": "", "oracle": false}
      {"address":= addressDb, "oracle":= oracleDb}
      (if (= addressDb "")
      (insert wallets1 address {
          "address": address,
          "oracle": oracle,
          "type": "normal",
          "updatedAt": (get-time)
          }
      )
      (update wallets1 address {
          "oracle": oracle
          }
      )
      )    
    )
  )
  )

  (defun insert-primary-wallet (address)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (is-oracle)
      (insert wallets1 address {
          "address": address,
          "oracle": false,
          "type": "primary",
          "updatedAt": (get-time)
          }
      )
  )    

  (defun remove-primary-wallet (address)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (is-oracle)
      (update wallets1 address {
          "type": "normal",
          "updatedAt": (get-time)
          }
      )
  )    

  (defun get-wallets () 
    (select wallets1 (constantly true)) 
  )

  (defun get-affiliates () 
    (select affiliates4 (constantly true)) 
  )

  (defun get-affiliates-for-account (address)
      (select affiliates4 (where 'address (= address))) 
  )

  (defun get-affCodes () 
    (select codes1 (constantly true)) 
  )

  (defun get-affCodes-for-account (address) 
    (select codes1 (where 'address (= address))) 
  )

  (defun get-payouts () 
    (select payouts1 (constantly true)) 
  )

  (defun get-payouts-for-account (address) 
    (select payouts1       
      (where 'address (= address))) 
  )

)
; create-table

