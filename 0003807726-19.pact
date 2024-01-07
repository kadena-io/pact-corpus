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
    "mark some functions as internal only..."
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

  (defun add-affiliate-ora (aff:object{affiliate})
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (is-oracle)
    (bind aff {
      "category":= category,
      "address":= address,
      "percentage":= percentage,
      "approved":= approved,
      "denied":= denied,
      "company":= company,
      "appliedAt":= appliedAt,
      "decidedAt":= decidedAt,
      "updatedAt":= updatedAt,
      "applicatonFee":= applicationFee
      }
    (insert affiliates4 (compound-key2 address category)
        {"category": category,
         "address": address,
         "percentage": percentage,
         "approved": approved,
         "denied": denied,
         "company": company,
         "appliedAt": appliedAt,
         "decidedAt": decidedAt,
         "updatedAt": updatedAt,
         "applicationFee": 0.0
        }
    )
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


  (defun acc-exists ()
    (with-default-read affiliates4 (at "sender" (chain-data))
        {"address":""}
        {"address":= address}
        (!= address "")
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

  (defun update-oracle-wallet (address:string oracle:bool)
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


)
; create-table
; (create-table radmin)
; (create-table affiliates4)
; (create-table codes1)
; (create-table wallets1)

