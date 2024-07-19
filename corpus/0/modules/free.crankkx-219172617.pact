(module crankkx GOVERN

  (use fungible-util)
  (use util.guards)

  (defconst CRANKKX_BANK:string 'crankkx-bank)
  (defconst CRANKKX_BANK_NEW:string 'crankkx-bank-new)
  (defconst ROW_KEY_SEPARATOR:string "/")

  (defcap GOVERN ()
  (with-read cxAdmin1 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
  ))

  (defcap INTERNAL ()
    "mark some functions as internal only...."
    true
  )

  (defun init (aguard:guard)
    "Create Bank and set admin...."
    ; (insert cxAdmin1 "admin" {
    ;   "admin": (at "sender" (chain-data)),
    ;   "aguard": aguard
    ;   } )
    ; (free.crankk01.create-account CRANKKX_BANK (create-module-guard "CRANKKX"))
    ; (coin.create-account CRANKKX_BANK (create-module-guard "CRANKKX"))
    (cETH.create-account CRANKKX_BANK (create-module-guard "CRANKKX"))
  )

  (defun init-new (amount:decimal)
    (coin.transfer-create (at "sender" (chain-data)) CRANKKX_BANK_NEW (create-DEX_DEBIT-guard) amount)
    (free.crankk01.transfer-create (at "sender" (chain-data)) CRANKKX_BANK_NEW (create-DEX_DEBIT-guard) amount)
  )    

  (defschema admin
    admin:string
    aguard:guard
  )

  (deftable cxAdmin1:{admin})

  (defschema pair
    token0:module{fungible-v2}
    token1:module{fungible-v2}
  )

  (deftable pairs1:{pair})

  (defun create-pair (token0:module{fungible-v2} token1:module{fungible-v2})
      (with-read cxAdmin1 "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
    
        (insert pairs1 (get-pair-key token0 token1)
            {"token0": token0,
             "token1": token1}
        )
      )    
  )

  (defschema minimum
    token:module{fungible-v2}
    minAmount: decimal
  )

  (deftable minimum1:{minimum})

  (defun update-minimum (token:module{fungible-v2} minAmount:decimal)
      (with-read cxAdmin1 "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-default-read minimum1 (get-token-key token)
          {"token": ""}
          {"token":= tokenDb}
            (if (= tokenDb "")  
                (insert minimum1 (get-token-key token)
                    {"token": token,
                     "minAmount": minAmount}
                )
                (update minimum1 (get-token-key token)
                    {
                     "minAmount": minAmount}
                )
            )
        )
      )
  )

  (defun get-minimum (token:module{fungible-v2})
        (with-default-read minimum1 (get-token-key token)
          {"token": "",
           "minAmount": 0.0}
          {"token":= tokenDb,
           "minAmount":= minAmountDb}
           minAmountDb
        )
  )

   (defschema ledger                     
      offerorA:string
      tokenA:module{fungible-v2}
      offerorB:string
      tokenB:module{fungible-v2}
      effrate:decimal
      effamntA:decimal
      effamntB:decimal
      eventAt:time
   )

  (deftable ledger3:{ledger})

  (defschema offer
    offeror:string
    oguard:guard
    token0:module{fungible-v2} ; selling this token
    token1:module{fungible-v2} ; buying this token
    amount:decimal
    ramount:decimal
    rate:decimal
    createdAt:time
    validityMinutes:integer
    complete:bool
  )

  (deftable offers3:{offer})

  (defschema nodeExtra
     address: string
     oracle: bool
  )    

  (deftable nodeExtras9:{nodeExtra})

  (defun create-offer 
    (token0:module{fungible-v2} 
     token1:module{fungible-v2} 
     amount:decimal 
     rate:decimal 
     validityMinutes:integer)

    ; (enforce (!= (at "sender" (chain-data)) "k:7a5ed216b0e736fb145c422a355fe18e0ff85e664e1b067d023c87c4947c9d82") "Blocked")

    (with-read pairs1 (get-pair-key token0 token1) ; this just makes sure pair exists
        {"token0":= rtoken0,
         "token1":= rtoken1}
        (let*
          ((offeror (at "sender" (chain-data)))
           (oguard (read-keyset "keyset")))
    
          (enforce (> amount 0.0) "Amount must be positive")     
          (enforce (> rate 0.0) "Rate must be positive")
          (get-minimum token0)
          (insert offers3 (compound-key4 offeror token0 token1 (get-formatted-time))
             {"offeror": offeror,
              "oguard": oguard,
              "token0": token0,
              "token1": token1,
              "amount": amount,
              "ramount": amount,
              "rate": rate,
              "createdAt": (get-time),
              "validityMinutes": validityMinutes,
              "complete": false
              }
          )
    
          (token0::transfer offeror CRANKKX_BANK_NEW amount)
        )
    )
  )

  (defun move-bank (token0:module{fungible-v2} amount:decimal)
      (with-read cxAdmin1 "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
    
        (with-capability (DEX_DEBIT)     
          (install-capability (token0::TRANSFER CRANKKX_BANK CRANKKX_BANK_NEW amount))
          (token0::transfer CRANKKX_BANK CRANKKX_BANK_NEW amount)
        )
      )    
  )

  (defun cancel-offer (key:string)
    (with-read offers3 key
      {"offeror":= offeror,
       "oguard":= oguard,
       "ramount":= ramount,
       "complete":= complete,
       "token0":= token0:module{fungible-v2}}
      (enforce (= offeror (at "sender" (chain-data))) "Sender must match") 
      (enforce (= false complete) "Must still be open") 
      (enforce-guard oguard) 
      (update offers3 key
        {"complete": true})
      (with-capability (DEX_DEBIT) 
          (install-capability (token0::TRANSFER CRANKKX_BANK_NEW offeror ramount))
          (token0::transfer CRANKKX_BANK_NEW offeror ramount)
      )
    )   
  )

  (defun close-expired-offer (key:string)
    (with-read offers3 key
      {"offeror":= offeror,
       "oguard":= oguard,
       "ramount":= ramount,
       "complete":= complete,
       "createdAt":= createdAt,
       "validityMinutes":= validityMinutes,
       "token0":= token0:module{fungible-v2}}
      (enforce (= false complete) "Must still be open") 
      (enforce (> (get-time) (add-time createdAt (* validityMinutes 60))) "Must be expired")
      (update offers3 key
        {"complete": true})
      (with-capability (DEX_DEBIT) 
          (install-capability (token0::TRANSFER CRANKKX_BANK_NEW offeror ramount))
          (if (> ramount 0.0)
              (token0::transfer CRANKKX_BANK_NEW offeror ramount)
              "Nothing to return"
          )
      )      
    )   
  )

  (defun min (a:decimal b:decimal)
    (if (<= a b)
        a
        b
    )    
  )

  (defun match-offer (keyA:string keyB:string)
    (with-read offers3 keyA
      {"offeror":= offerorA,
       "oguard":= oguardA,
       "ramount":= ramountA,
       "rate":= rateA,
       "complete":= completeA,
       "createdAt":= createdAtA, 
       "validityMinutes":= validityMinutesA,
       "token0":= token0A:module{fungible-v2},
       "token1":= token1A:module{fungible-v2}}
        (with-read offers3 keyB
          {"offeror":= offerorB,
           "oguard":= oguardB,
           "ramount":= ramountB,
           "rate":= rateB,
           "complete":= completeB,
           "createdAt":= createdAtB,
           "validityMinutes":= validityMinutesB,
           "token0":= token0B:module{fungible-v2},
           "token1":= token1B:module{fungible-v2}}
           (enforce (!= offerorA offerorB) "No exchange between same account")               
           (enforce (= token0A token1B) "Same pair exchange")               
           (enforce (= token0B token1A) "Same pair exchange")
           (enforce (>= ramountA 0.0) "Remaining amount must be positive")
           (enforce (>= ramountB 0.0) "Remaining amount must be positive")
           (enforce (= completeA false) "Cannot be complete")
           (enforce (= completeB false) "Cannot be complete")
           (enforce (< (get-time) (add-time createdAtA (* validityMinutesA 60))) "Cannot be expired")
           (enforce (< (get-time) (add-time createdAtB (* validityMinutesB 60))) "Cannot be expired")
           (let*
             ((revrate (round (/ 1 rateB) 6))
              (effrate (/ (+ revrate rateA) 2)) ; middle rate
              (effamnt1A (* ramountA effrate)) 
              (effamntBp:decimal (min effamnt1A ramountB)) ; B's amount going to A in token 1A (receive)
              (effamntAp:decimal (/ effamntBp effrate)) ; A's amount going to B in token 1B (receive)
              (effamntB (round effamntBp 6))
              (effamntA (round effamntAp 6))
              (newramountA (- ramountA effamntA))  
              (newramountB (- ramountB effamntB)))  

              (enforce (>= revrate (round rateA 6)) "Rate must be favorable")
              (update offers3 keyA
                 {"ramount": newramountA, "complete": (< newramountA 0.0001)}
              )         
              (update offers3 keyB
                 {"ramount": newramountB, "complete": (< newramountB 0.0001)}
              )   
              (with-capability (DEX_DEBIT)     
                  (install-capability (token1A::TRANSFER CRANKKX_BANK_NEW offerorA effamntB))
                  (if (< (account-exists offerorA token1A) 0.0)
                      (token1A::transfer-create CRANKKX_BANK_NEW offerorA oguardA effamntB)
                      (token1A::transfer CRANKKX_BANK_NEW offerorA effamntB)
                  )        
                  (install-capability (token1B::TRANSFER CRANKKX_BANK_NEW offerorB effamntA))
                  (if (< (account-exists offerorB token1B) 0.0)
                      (token1B::transfer-create CRANKKX_BANK_NEW offerorB oguardB effamntA)
                      (token1B::transfer CRANKKX_BANK_NEW offerorB effamntA)
                  )
              )
              (let
                ((tx-id (hash {"offerorA": offerorA, "offerorB": offerorB, "effamntA": effamntA, "effamntB": effamntB, "salt": (get-time)})))
                (insert ledger3 tx-id
                    { "offerorA": offerorA,
                      "tokenA": token0A,
                      "offerorB": offerorB,
                      "tokenB": token1A,
                      "effrate": effrate,
                      "effamntA": effamntA,
                      "effamntB": effamntB,
                      "eventAt": (get-time)}
                )
              )

              (format "revrate:{} effrate:{} effamntB:{} efamntA:{}" 
                [revrate, effrate, effamntB, effamntA])    
           )
        )
    )
  )

  (defun account-exists (address token:module{fungible-v2})
    (try -1.0 (token::get-balance address))
  )

  (defun update-node-extra (address oracle)
      (with-read cxAdmin1 "admin"
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

  (defun get-ledger ()
    (select ledger3 (constantly true))
  )

  (defun get-pairs ()
    (select pairs1 (constantly true))
  )

  (defun get-minimums ()
    (select minimum1 (constantly true))
  )

  (defun get-offers ()
     (select offers3 (constantly true))
  )

  (defun get-closed-offers ()
     (select offers3 (where 'complete (= true)))
  )

  (defun get-open-offers ()
     (select offers3 (where 'complete (= false)))
  )

  (defun get-my-open-offers ()
     (select offers3 (and?
       (where 'offeror (= (at "sender" (chain-data))))
       (where 'complete (= false)))
     )
  )

  (defun get-other-open-offers ()
     (select offers3 (and?
       (where 'offeror (!= (at "sender" (chain-data))))
       (where 'complete (= false)))
     )
  )

  (defun get-offer-keys ()
     (keys offers3)
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

  (defun get-pair-key
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    "Create canonical key for pair."
    (format "{}:{}" (canonicalize tokenA tokenB))
  )

  (defun canonicalize:[module{fungible-v2}]
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (if (is-canonical tokenA tokenB) [tokenA tokenB] [tokenB tokenA])
  )

  (defun is-canonical
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (< (format "{}" [tokenA]) (format "{}" [tokenB]))
  )


  (defun compound-key4:string
    (part1 part2 part3 part4)
    (format "{}{}{}{}{}{}{}" [part1 ROW_KEY_SEPARATOR part2 ROW_KEY_SEPARATOR part3 ROW_KEY_SEPARATOR part4])
  )

  (defun read-admin ()
    "Read admin guard"
    (with-read cxAdmin1 "admin"
      { "admin":= admin,
      "aguard":= aguard }
    (format "Admin guard is {}, admin {}" [aguard admin]))
  )

  (defun get-bank-name:string ()
    CRANKKX_BANK_NEW
  )    

;; Invoking capability definition
(defcap WITHDRAW (recipient:string amount:decimal)
  (compose-capability (DEX_DEBIT))
)
;; Capability user guard: capability definition
(defcap DEX_DEBIT () true)
;; Capability user guard: capability predicate function
(defun require-DEX_DEBIT () 
  (require-capability (DEX_DEBIT)))

;; Capability user guard: guard constructor
(defun create-DEX_DEBIT-guard ()
  (create-user-guard (require-DEX_DEBIT)))


)
; create-table
;(create-table minimum1)
