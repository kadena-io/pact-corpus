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

  (defun create-card-for-address (address)
      (with-read cAdmin1 "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (coin.create-account (+ "CCAR-" (hash address)) (create-CAR_DEBIT-guard))
        (+ "CCAR-" (hash (at "sender" (chain-data))))
      )
  )    

  (defun get-my-card ()
    (try false (coin.details (get-base)))
  )    

  (defun is-card-owned ()
    (= (format "{}" [(at 'guard (coin.details (get-base)))]) 
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

  (defun get-base ()
    (+ "CCAR-" (hash (at "sender" (chain-data))))
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
)
; create-table
; (create-table cAdmin1)
; (create-table nodeExtras9)
; (create-table links4)



