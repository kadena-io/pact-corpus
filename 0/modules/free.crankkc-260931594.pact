(module crankkc GOVERN

  (use fungible-util)
  (use util.guards)

  (defconst ROW_KEY_SEPARATOR:string "/")

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

  (deftable links3:{links})

  (defschema history
     base: string
     linked: string
     amount: decimal
     chain: string
     at: time
  )    

  (deftable history2:{history})

  (defun create-card-account ()
    (coin.create-account (+ "CCAR-" (hash (at "sender" (chain-data)))) (create-CAR_DEBIT-guard))
    (+ "CCAR-" (hash (at "sender" (chain-data))))
  )    

  (defun get-my-card ()
    (try false (coin.details (+ "CCAR-" (hash (at "sender" (chain-data))))))
  )    

  (defun is-card-owned ()
    (= (format "{}" [(at 'guard (coin.details (+ "CCAR-" (hash (at "sender" (chain-data))))))]) 
       "UserGuard {fun: free.crankkc.require-CAR_DEBIT,args: []}")
  )    

  (defun fund (linked) 
    (with-read links3 (compound-key linked "0")
      {"base":= base,
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
          (update links3 (compound-key linked chainDb) {"lastTopUpAt": (get-time)})
          (insert history2 (hash (+ (+ base (+ linked chainDb)) (get-formatted-time)))
            {"base": base,
             "linked": linked,
             "amount": topUp,
             "chain": chainDb,
             "at": (get-time)}
          )
       )
    )  
  )

  (defun fund-crosschain (linked:string chain:string chainBalance:decimal) 
    (with-read links3 (compound-key linked chain)
      {"base":= base,
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
              (install-capability (coin.TRANSFER_XCHAIN base linked max chain))
              (coin.transfer-crosschain base linked (at 'guard (coin.details linked)) chain max)
          )
          (update links3 (compound-key linked chain) {"lastTopUpAt": (get-time)})
          (insert history2 (hash (+ (+ base linked) (get-formatted-time)))
            {"base": base,
             "linked": linked,
             "amount": topUp,
             "chain": chain,
             "at": (get-time)}
          )
       )
    )  
  )

  (defun create-link (account chain)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (insert links3 (compound-key account chain)
      {"linked": account,
       "base": (+ "CCAR-" (hash (at "sender" (chain-data)))),
       "active": true,
       "max": 1.0,
       "min": 0.15,
       "chain": chain,
       "lastTopUpAt": (get-epoch)}
      )
  )    

  (defun deactivate-link (account chain)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (update links3 (compound-key account chain)
      {"active": false}
    )
  )    

  (defun withdraw-from-card (account amount)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (with-capability (CAR_DEBIT) 
      (install-capability (coin.TRANSFER (+ "CCAR-" (hash (at "sender" (chain-data)))) account amount))
      (coin.transfer (+ "CCAR-" (hash (at "sender" (chain-data)))) account amount)
    )
  )    

  (defun get-my-linked ()
    (select links3 (where 'base (= (+ "CCAR-" (hash (at "sender" (chain-data)))))))
  )

  (defun get-links ()
    (select links3 (constantly true))
  )

  (defun account-exists (address token:module{fungible-v2})
    (try -1.0 (token::get-balance address))
  )

  (defun compound-key:string
    ( part1:string
      part2:string )
    (format "{}{}{}" [part1 ROW_KEY_SEPARATOR part2])
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

  (defun get-history () 
    (select history2 (constantly true)) 
  )

  (defun get-my-history () 
    (select history2 (where 'base (= (+ "CCAR-" (hash (at "sender" (chain-data))))))) 
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
)
; create-table
; (create-table cAdmin1)

