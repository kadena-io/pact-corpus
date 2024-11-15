(module crankks GOVERN

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

  (defschema tokens
    token:module{fungible-v2}
    symbol:string
    toUSDRate:decimal
  )

  (deftable tokens1:{tokens})

  (defun add-token (token:module{fungible-v2} symbol:string toUSDRate:decimal)
      (with-read tokens1 "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (insert tokens1 token {
            "token": token,
            "symbol": symbol,
            "toUSDRate": toUSDRate
            }
        )
      )
  )

  (defun k-account ()
    (enforce (= (take 2 (at "sender" (chain-data))) "k:") "Has to be a k: address")
    (enforce (= (length (at "sender" (chain-data))) 66) "Has to be a k: address")
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

;; Capability user guard: capability definition
(defcap SAL_DEBIT () true)
;; Capability user guard: capability predicate function
(defun require-SAL_DEBIT () 
  (require-capability (SAL_DEBIT)))

;; Capability user guard: guard constructor
(defun create-SAL_DEBIT-guard ()
  (create-user-guard (require-SAL_DEBIT)))
)
; create-table
; (create-table cAdmin1)
; (create-table nodeExtras9)

