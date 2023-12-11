(module kor-blocks-payout GOVERNANCE

  @doc "Collection token policy."

(use coin)
 
;;------------------------------------------------------
;;Schemas and tables
;;------------------------------------------------------


  (defschema payout-info 
      token-id:string
      account:string
      hashrate:decimal
      last-payout:time)


  (defschema account-info
    account:string
    tokens:[string]
    amount:decimal)

  (defschema operator
    account:string
    operator-guard:guard)

  (defschema og-badges
    og-accounts:[string]
    amount:decimal)


  (deftable payout-info-table:{payout-info})
  (deftable operator-table:{operator})
  (deftable og-badges-table:{og-badges})


;;------------------------------------------------------------------------------
;;Capabilities
;; -----------------------------------------------------------------------------

  (defcap GOVERNANCE ()
    @doc "Governance capability"

    (enforce-keyset  "free.kor-admin" ))



  (defcap INTERNAL ()
    @doc "Capability to ensure Interal usage of functions"

    true)


  (defcap UPDATE ()
    @doc "Capability to ensure Interal usage of functions"

    true)


  (defcap OPERATOR 
    ()
  @doc "Capability to enforce the function is being called by the operator"
  (with-read operator-table "1" 
    {
    'operator-guard:= operator-guard:guard
    }
    (enforce-keyset operator-guard)))


;;------------------------------------------------------------------------------
;; admin
;; -----------------------------------------------------------------------------


  (defun set-operator
    (account:string operator-guard:guard)
    (with-capability (GOVERNANCE)
    (insert operator-table "1"
      {
        "account":account
        ,"operator-guard":operator-guard
      })))


  (defun create-multiple-payout-info
    (payout-info-list:[object{payout-info}])
    (with-capability (GOVERNANCE)
      (with-capability (INTERNAL)
        (map (create-payout-info) payout-info-list)
        )))

  (defun create-payout-info
    (payout-info:object{payout-info})

    (with-capability (GOVERNANCE)
    (insert payout-info-table (at "token-id" payout-info)
    {
      "token-id":(at "token-id" payout-info)
      ,"account":(at "account" payout-info)
      ,"hashrate":(at "hashrate" payout-info)
      ,"last-payout":(at "last-payout" payout-info)
    })
    true))



  (defun update-owner
    (token-id:string account:string)
    (with-capability (GOVERNANCE)
    (update payout-info-table token-id 
      {
        "account":account
      })))


  (defun update-last-payout
    (token-id:string)
    (require-capability (UPDATE))
    (update payout-info-table token-id 
      {
        "last-payout":(curr-time)
      }))


  ;  (defun update-og-badges-info
  ;    (og-accounts:[string])
  ;    (with-capability (GOVERNANCE)
  ;    (write og-badges-table "1"
  ;      {
  ;        "og-accounts":og-accounts
  ;        ,"amount":(length og-accounts)
  ;      })))



;;------------------------------------------------------------------------------
;; blocks-payout
;; -----------------------------------------------------------------------------



  (defun payout-blocks
    (account-info-list:[object{account-info}] og-list:object{og-badges})

      (with-capability (INTERNAL)
      (map (pay-account) account-info-list)
      (map (payout-og-account (at "amount" og-list)) (at "og-accounts" og-list))
      
      ))



  (defun pay-account
    (account-info:object{account-info})
    (require-capability (INTERNAL))
    (with-capability (UPDATE)
    (with-read operator-table "1" 
      {
        "account":=account-kor
      }
      (coin.transfer-create account-kor (at "account" account-info) (read-msg (concat ["ks-blocks-" (at "account" account-info)])) (floor (at "amount" account-info) 8) )
      (map (update-last-payout) (at "tokens" account-info))
      )))



;;------------------------------------------------------------------------------
;; og-payout
;; -----------------------------------------------------------------------------


(defun payout-og-account
  (amount:decimal account:string)
  (require-capability (INTERNAL)) 
  (with-read operator-table "1" 
    {
      "account":=account-kor
    }
  (coin.transfer-create account-kor account (read-msg  (concat ["ks-og-" account])) (floor amount 8))))





;;------------------------------------------------------------------------------
;; getters
;; -----------------------------------------------------------------------------

  (defun get-amount-to-pay-account
    (kda-per-th-per-day:decimal account-info:object{account-info})
    (let* 
      (
        (account (at "account" account-info))
        (tokens (at "tokens" account-info))
        (amount-to-pay-list (map (get-amount-to-pay-token kda-per-th-per-day) tokens))
      )
      (fold (+) 0 amount-to-pay-list)))

  (defun get-amount-to-pay-token:decimal
    (kda-per-th-per-day:decimal token-id:string)
    (with-read payout-info token-id 
      {
        "hashrate" := hashrate
        ,"last-payout" := last-payout
      }
      (let* 
        (
          (days-to-pay (floor ( / (diff-time (curr-time) last-payout) 86400)0))
          (amount-to-pay (* days-to-pay (* hashrate (* kda-per-th-per-day 0.75))))
        )
        amount-to-pay
        )))

  (defun get-total-amount-to-pay-ogs
    (kda-per-th-per-day:decimal account-info-list:[object{account-info}])
    (map (get-amount-to-pay-account kda-per-th-per-day) account-info-list)
    )


  


        (defun curr-time:time 
          ()
        (at "block-time" (chain-data)))

  (defun get-token-info:object{payout-info}
    (token-id:string)
    (read payout-info-table token-id))

)


