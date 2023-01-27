(module crankkx GOVERN

  (use fungible-util)
  (use util.guards)

  (defconst CRANKKX_BANK:string 'crankkx-bank)
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
    (insert cxAdmin1 "admin" {
      "admin": (at "sender" (chain-data)),
      "aguard": aguard
      } )
    (free.crankk01.create-account CRANKKX_BANK (create-module-guard "CRANKKX"))
    (coin.create-account CRANKKX_BANK (create-module-guard "CRANKKX"))
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

  (deftable ledger2:{ledger})

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

  (defun create-offer 
    (token0:module{fungible-v2} 
     token1:module{fungible-v2} 
     amount:decimal 
     rate:decimal 
     validityMinutes:integer)

    (with-read pairs1 (get-pair-key token0 token1) ; this just makes sure pair exists
        {"token0":= rtoken0,
         "token1":= rtoken1}
        (let*
          ((offeror (at "sender" (chain-data)))
           (oguard (read-keyset "keyset")))
    
          (enforce (> amount 0.0) "Amount must be positive")     
          (enforce (> rate 0.0) "Rate must be positive")     
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
    
          (token0::transfer offeror CRANKKX_BANK amount)
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
;      (enforce-guard oguard) ; This needs to be resolved 
      (update offers3 key
        {"complete": true}) 
      (install-capability (token0::TRANSFER CRANKKX_BANK offeror ramount))
      (token0::transfer CRANKKX_BANK offeror ramount)
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
       "ramount":= ramountA,
       "rate":= rateA,
       "complete":= completeA,
       "createdAt":= createdAtA, 
       "validityMinutes":= validityMinutesA,
       "token0":= token0A:module{fungible-v2},
       "token1":= token1A:module{fungible-v2}}
        (with-read offers3 keyB
          {"offeror":= offerorB,
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
           (enforce (> ramountA 0.0) "Remaining amount must be positive")
           (enforce (> ramountB 0.0) "Remaining amount must be positive")
           (enforce (= completeA false) "Cannot be complete")
           (enforce (= completeB false) "Cannot be complete")
           (enforce (< (get-time) (add-time createdAtA (* validityMinutesA 60))) "Cannot be expired")
           (enforce (< (get-time) (add-time createdAtB (* validityMinutesB 60))) "Cannot be expired")
           (let*
             ((revrate (/ 1 rateB))
              (effrate (/ (+ revrate rateA) 2)) ; middle rate
              (effamnt1A (* ramountA effrate)) 
              (effamntB (round (min effamnt1A ramountB)) 5) ; B's amount going to A in token 1A (receive)
              (effamntA (round (/ effamntB effrate)) 5) ; A's amount going to B in token 1B (receive)
              (newramountA (- ramountA effamntA))  
              (newramountB (- ramountB effamntB)))  

              (enforce (>= revrate rateA) "Rate must be favorable")
              (update offers3 keyA
                 {"ramount": newramountA, "complete": (= newramountA 0.0)}
              )         
              (update offers3 keyB
                 {"ramount": newramountB, "complete": (= newramountB 0.0)}
              )         
              (install-capability (token1A::TRANSFER CRANKKX_BANK offerorA effamntB))
              (token1A::transfer CRANKKX_BANK offerorA effamntB)
        
              (install-capability (token1B::TRANSFER CRANKKX_BANK offerorB effamntA))
              (token1B::transfer CRANKKX_BANK offerorB effamntA)

              (let
                ((tx-id (hash {"offerorA": offerorA, "offerorB": offerorB, "effamntA": effamntA, "effamntB": effamntB, "salt": (get-time)})))
                (insert ledger2 tx-id
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

  (defun get-ledger ()
    (select ledger2 (constantly true))
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

)
; create-table
;(create-table ledger2)
