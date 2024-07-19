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
     lastTopUpAt: time
  )    

  (deftable links2:{links})

  (defun create-card-account ()
    (coin.create-account (+ "CCAR-" (hash (at "sender" (chain-data)))) (create-CAR_DEBIT-guard))
  )    

  (defun fund (linked) 
    (with-read links2 linked
      {"base":= base,
       "active":= actve,
       "max":= max,
       "min":= min}
       (let* (
         (baseBalance (coin.get-balance base))
         (linkedBalance (coin.get-balance linked))
         (topUp (- max linkedBalance))
         )
         (enforce (<= linkedBalance min) "No topup needed")
         (enforce (> baseBalance topUp) "Insufficient balance on base")
          (with-capability (CAR_DEBIT) 
              (install-capability (coin.TRANSFER base linked topUp))
              (coin.transfer base linked topUp)
          )
          (update links2 linked {"lastTopUpAt": (get-time)})
       )
    )  
  )

  (defun create-link (account)
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (insert links2 account
      {"linked": account,
       "base": (+ "CCAR-" (hash (at "sender" (chain-data)))),
       "active": true,
       "max": 1.0,
       "min": 0.15}
      )
  )    

  (defun get-my-linked ()
    (select links2 (where 'base (= (+ "CCAR-" (hash (at "sender" (chain-data)))))))
  )

  (defun account-exists (address token:module{fungible-v2})
    (try -1.0 (token::get-balance address))
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
