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

  (defun create-card-account ()
    (coin.create-account (+ "CCAR-" (hash (at "sender" (chain-data)))) (create-CAR_DEBIT-guard))
    (+ "CCAR-" (hash (at "sender" (chain-data))))
  )    

  (defun create-card-for-address (address)
    (coin.create-account (+ "CCAR-" (hash address)) (create-CAR_DEBIT-guard))
    (+ "CCAR-" (hash (at "sender" (chain-data))))
  )    

  (defun get-my-card ()
    (+ "CCAR-" (hash (at "sender" (chain-data))))
  )    

  (defun is-card-owned ()
    (= (format "{}" [(at 'guard (coin.details (+ "CCAR-" (hash (at "sender" (chain-data))))))]) 
       "UserGuard {fun: free.crankkc.require-CAR_DEBIT,args: []}")
  )    

  (defun read-admin ()
    "Read admin guard"
    (with-read cAdmin1 "admin"
      { "admin":= admin,
      "aguard":= aguard }
    (format "Admin guard is {}, admin {}" [aguard admin]))
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

