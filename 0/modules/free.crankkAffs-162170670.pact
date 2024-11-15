(module crankkAffs GOVERNANCE

  (use fungible-util)
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


  (defun apply-affiliate-user (affCode)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (enforce (= (take 2 affCode) "U_") "Must start with U_")
    
    (insert codes1 affCode
        {"category": "U",
         "address": (at "sender" (chain-data)),
         "affCode": affCode
        }
    )
    (if (acc-exists)
        ""
        (insert affiliates4 (at "sender" (chain-data))
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
  )

  (defun apply-affiliate-infl (affCode)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (enforce (= (take 2 affCode) "I_") "Must start with I_")
    (insert codes1 affCode
        {"category": "I",
         "address": (at "sender" (chain-data)),
         "affCode": affCode
        }
    )
    (if (acc-exists)
        ""
        (insert affiliates4 (at "sender" (chain-data))
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
  )

  (defun apply-affiliate-comp (company)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (enforce (!= company "") "Company name cannot be blank")

    (insert codes1 (+ "C_" company)
        {"category": "C",
         "address": (at "sender" (chain-data)),
         "affCode": (+ "C_" company)
        }
    )
    (if (acc-exists)
        ""
        (insert affiliates4 (at "sender" (chain-data))
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
  )

  (defun apply-affiliate-agnt (affCode company)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (enforce (= (take 2 affCode) "A_") "Must start with A_")
    (read affiliates4 (+ "C_" company))

    (insert codes1 affCode
        {"category": "A",
         "address": (at "sender" (chain-data)),
         "affCode": affCode
        }
    )
    (if (acc-exists)
        ""
        (insert affiliates4 (at "sender" (chain-data))
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
  )

  (defun acc-exists ()
    (with-default-read affiliates4 (at "sender" (chain-data))
        {"address":""}
        {"address":= address}
        (!= address "")
    )
  )

  (defun approve-affiliate (address decision:bool percentage)
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (update affiliates4 address
            {"percentage": percentage,
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
      (read affiliates4 address)
    )
  )

  (defun get-affiliates () 
    (select affiliates4 (constantly true)) 
  )

  (defun get-affiliates-for-account (address) 
    (select affiliates4       
      (where 'address (= address))) 
  )

  (defun get-affCodes () 
    (select codes1 (constantly true)) 
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
; (create-table radmin)
; (create-table affiliates4)
; (create-table codes1)
; (create-table payouts1)

