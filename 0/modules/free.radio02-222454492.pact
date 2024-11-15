(module radio02 GOVERNANCE

  (use fungible-util)
  (use util.guards)

  (defconst ROW_KEY_SEPARATOR:string "/")
  (defconst RADIO01_BANK:string 'radio01-bank)
  (defconst RADIO01_STAKE_BANK:string 'radio01-stake01-bank)
  (defconst RADIO01_LICENSE_BANK:string 'radio01-license01-bank)
  (defconst NEW_LIC_BANK:string 'radio01-license02-bank)
  (defconst CRANKK_SELFAFF_BANK:string 'crankk-selfaff-bank)
  (defconst RADIO01_AWARD_BANK:string 'radio01-award-bank) 
  (defconst CRANKK_FOUND_BANK:string 'crankk-found-bank) 

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
    "Create Bank and set admin...." 
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

  (defschema receive
     address: string
     gatewayId: string ;of the sender
     mic: string
  )    

  (defschema receiveGw
     address: string
     gatewayId: string ;of the receiver
     mic: string
     chain: string
  )    

  (defschema gateway
     address: string
     receives:[object:{receiveGw}]
  )    

  (deftable gateways7:{gateway})

  (defschema gatewayExtra
     gatewayId: string
     online: bool
     hosted: bool
     factor: decimal
  )    

  (deftable gatewayExtras7:{gatewayExtra})

  (defschema gatewayGPS
     gatewayId: string
     lastDetected: time
  )    

  (deftable gatewayGPSs1:{gatewayGPS})

  (defschema gatewayDir
     gatewayId: string
     firstTime: time
     chain: integer
     online: bool
     hosted: bool
     factor: decimal
  )    

  (deftable gatewayDirectory:{gatewayDir})

  (defschema gwTransfer
     gatewayId: string
     transferable: bool
     hashedPw: string
  )    

  (deftable gwTransfers9:{gwTransfer})

  (defschema witness
     id: string
     distance: decimal
  )    

  (defschema node
     address: string
     guard: guard
     gatewayId: string
     pubkey: string
     send: bool
     director: string
     pubkeyd: string
     net: time
     sent: string
     consMember: bool
     validReceives:[object:{witness}]
     lastAction: time
  )    

  (deftable nodes7:{node})

  (defschema nodeExtra
     address: string
     oracle: bool
  )    

  (deftable nodeExtras9:{nodeExtra})

  (defschema rewardBase
     base: decimal
  )    

  (deftable rewardBase1:{rewardBase})

  (defun insert-base ()
    (with-read radmin "admin"
      {"aguard" := aguard }
      (enforce-guard aguard)
      (insert rewardBase1 "base" {"base": 0.9132})
    )
  )

  (defun decrement-base ()
    (with-read radmin "admin"
      {"aguard" := aguard }
      (enforce-guard aguard)
      (with-read rewardBase1 "base"
        {"base":= baseDb}
        (update rewardBase1 "base" {"base": (- baseDb 0.0004)})
      )
    )
  )

  (defun get-base ()
    (round (at 'base (read rewardBase1 "base" ['base])) 5)
  )

  (defschema txn-detail
     txn-type: string
     sender: string
     message: string
     award: decimal
     witnesses:[object:{witness}]
     timestamp: time
  )    

  (defschema node-stat
     address: string
     txn-details:[object:{txn-detail}]
     lastAction: time
  )    

  (deftable node-stats7:{node-stat})

  (defschema stake-menu
     code: string ;same as key
     name: string
     description: string
     valueInCRKK: decimal
     valueDefault: decimal 
     tokenDefault: string ; it's priced in this token
     validFrom: time ; for initiating staking 
     validTo: time ; for initiating staking
     stakeWait: integer ; in seconds, from staking to active
     destakeWait: integer ; in seconds, from destaking to inactive
     term: integer ; in seconds, term without penalty
     lockup: integer ; in seconds, while no destake
     earlyPenalty: integer ; percentage right after lockup release, linearly goes to zero
     lastAction: time
  )    

  (deftable stakeMenu2:{stake-menu})

  (defschema stake-extra
     code: string ;same as key
     count: integer
     maxCount: integer
  )    

  (deftable stakeExtra:{stake-extra})

  (defun insert-steak-menu (code name description valueInCRKK valueDefault tokenDefault validFrom validTo stakeWait destakeWait term lockup earlyPenalty)
      (with-read radmin "admin"
        { "aguard" := aguard }
          (enforce-guard aguard)
          (insert stakeMenu2 code {
              "code": code,
              "name": name,
              "description": description,
              "valueInCRKK": valueInCRKK,
              "valueDefault": valueDefault,
              "tokenDefault": tokenDefault,
              "validFrom": (time validFrom),
              "validTo": (time validTo),
              "stakeWait": stakeWait,
              "destakeWait": destakeWait,
              "term": term,
              "lockup": lockup,
              "earlyPenalty": earlyPenalty,
              "lastAction": (get-time)
              }
          )
       )
  )

  (defun upgate-steak-menu (code name description)
      (with-read radmin "admin"
        { "aguard" := aguard }
          (enforce-guard aguard)
          (update stakeMenu2 code {
              "code": code,
              "name": name,
              "description": description
              }
          )
       )
  )

  (defun update-steak-menu-date (code  validTo)
      (with-read radmin "admin"
        { "aguard" := aguard }
          (enforce-guard aguard)
          (update stakeMenu2 code {
              "validTo": (time validTo)
              }
          )
       )
  )

  (defun insert-steak-extra (code count maxCount)
      (with-read radmin "admin"
        { "aguard" := aguard }
          (enforce-guard aguard)
          (insert stakeExtra code {
              "code": code,
              "count": count,
              "maxCount": maxCount
              }
          )
       )
  )

  (defun update-steak-extra (code count maxCount)
      (with-read radmin "admin"
        { "aguard" := aguard }
          (enforce-guard aguard)
          (update stakeExtra code {
              "code": code,
              "count": count,
              "maxCount": maxCount
              }
          )
       )
  )

  (defun update-USDtoCRKK (code:string USDtoCRKK)
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (with-read stakeMenu2 code
            {"valueDefault":= valueDefault}
            (update stakeMenu2 code
                {"valueInCRKK": (round (* valueDefault USDtoCRKK) 2)}
            )
        )
    )
  )

  (defun is-staked:bool (address code)
    (with-default-read staking3 (compound-key2 address code) 
      {"code": "123",
       "closedAt": (get-epoch)}
      {"code":= code,
       "closedAt":= closedAt}
      (and (!= code "123") (= closedAt (get-epoch)))
    )
  )

  (defun get-stake-menu ()
    "Get all stakeMenu"
    (select stakeMenu2 (constantly true))
  )

  (defschema staking
     addrCode: string ;same as key (address+code)
     address: string
     code: string
     guard: guard
     gatewayId: string
     value: decimal ; in CRKK
     stakedAt: time
     destakedAt: time
     closedAt: time ; destake wait expired, funds returned
     validFrom: time ; staked at plus stake wait
     validTo: time ; destaked at plus destake wait
     term: integer
     lockedUntil: time ; until available for destake
     penalty: integer ; percentage penalty when destaked
     lastAction: time
  )    

  (deftable staking3:{staking})

  (defun stake (code gatewayId)
    "Stake"
    ; (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (if (= (take 4 code) "CONS")
        (with-default-read stakeExtra code
            {"maxCount": "9999999",
             "count": 0}
            {"count":= count,
             "maxCount":= maxCount}
            (enforce (< count maxCount) "Maximum slots reached")
            (if (!= maxCount 9999999)
                (update stakeExtra code
                   {"count": (+ count 1)}
                )
                ""
            )
        )
        ""
    )
    (if (= (take 4 code) "CONS")
        (with-default-read nodes7 (at "sender" (chain-data))
            ;This ensures address owns a Gateweay
            {"gatewayId": "123"}
            {"gatewayId":= gatewayId}
            (with-default-read gateways7 gatewayId
              {"address": "123"}
              {"address":= address}
              (enforce (= address (at "sender" (chain-data))) "Address does not own a gateway")
            )
        )
        ""
    )    
    (with-read stakeMenu2 code 
      {"valueInCRKK":= value,
       "stakeWait":= stakeWait,
       "term":= term,
       "lockup":= lockup}
        (insert staking3 (compound-key2 (at "sender" (chain-data)) code)
            {"addrCode": (compound-key2 (at "sender" (chain-data)) code),
             "address": (at "sender" (chain-data)),
             "code": code,
             "guard": (read-keyset "keyset"),
             "gatewayId": gatewayId,
             "value": value,
             "stakedAt": (get-time),
             "destakedAt": (get-epoch),
             "closedAt": (get-epoch),
             "validFrom": (add-time (get-time) stakeWait),
             "validTo": (get-epoch),
             "term": term,
             "lockedUntil": (add-time (get-time) lockup),
             "penalty": 0,
             "lastAction": (get-time)}
        )
        (free.crankk01.transfer-create (at "sender" (chain-data)) RADIO01_STAKE_BANK 
            (create-STAKE_DEBIT-guard) value)
        ; (free.crankk01.transfer (at "sender" (chain-data)) RADIO01_STAKE_BANK value)
    )
  )

  (defun unstake-request (code gatewayId)
    "Un-stake"
    (enforce-guard (at 'guard (read staking3 (compound-key2 (at "sender" (chain-data)) code) ['guard])))
    (with-read staking3 (compound-key2 (at "sender" (chain-data)) code)
      {"lockedUntil":= lockedUntil,
       "destakedAt":= destakedAt
      }
      (enforce (= destakedAt (get-epoch)) "Already destaked")
      (enforce (< lockedUntil (get-time)) "Still locked")
        (update staking3 (compound-key2 (at "sender" (chain-data)) code)
            {"destakedAt": (get-time),
             "lastAction": (get-time)}
        )
    )
  )

  (defun unstake-request-cancel (address code)
    "Un-stake request cancel"
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-read staking3 (compound-key2 address code)
          {"lockedUntil":= lockedUntil,
           "destakedAt":= destakedAt,
           "closedAt":= closedAt
          }
          (enforce (!= destakedAt (get-epoch)) "Not destaked")
          (enforce (= closedAt (get-epoch)) "Already closed")
          (update staking3 (compound-key2 address code)
                {"destakedAt": (get-epoch)}
          )
        )
    )
  )

  (defun unstake-cancel (address code)
    "Un-stake cancel"
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-read staking3 (compound-key2 address code)
          {"lockedUntil":= lockedUntil,
           "destakedAt":= destakedAt,
           "closedAt":= closedAt
          }
          (enforce (!= destakedAt (get-epoch)) "Not destaked")
          (enforce (!= closedAt (get-epoch)) "Not closed, use request cancel")
          (update staking3 (compound-key2 address code)
                {"destakedAt": (get-epoch),
                 "closedAt": (get-epoch)}
          )
        )
    )
  )

  (defun unstake (address code)
    "Un-stake"
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-read stakeMenu2 code 
          {"valueInCRKK":= value,
           "destakeWait":= destakeWait,
           "term":= term,
           "earlyPenalty":= earlyPenalty,
           "lockup":= lockup}
    
            (with-read staking3 (compound-key2 address code)
              {
               "stakedAt":= stakedAt,
               "destakedAt":= destakedAt,
               "closedAt":= closedAt,
               "value":= value
              }
              (enforce (!= destakedAt (get-epoch)) "Not yet requested")
              (enforce (= closedAt (get-epoch)) "Already closed")
              (enforce (< (add-time destakedAt destakeWait) (get-time)) "Destake wait not passed")
              (let* 
                ((remTerm (diff-time (add-time stakedAt term) (get-time)))
                 (penTerm (- term (+ lockup destakeWait)))
                 (retPerc (round (+ earlyPenalty (* (- 100 earlyPenalty) (/ (- penTerm remTerm) penTerm))) 2))
                 (retAmnt (round (* value (/ retPerc 100)) 2))
                 (penalty (round (- value retAmnt) 2)))

                 (with-capability (STAKE_DEBIT) 
                     (install-capability (free.crankk01.TRANSFER RADIO01_STAKE_BANK address retAmnt))
                     (free.crankk01.transfer RADIO01_STAKE_BANK address retAmnt)
                     (install-capability (free.crankk01.TRANSFER RADIO01_STAKE_BANK CRANKK_FOUND_BANK penalty))
                     (free.crankk01.transfer-create RADIO01_STAKE_BANK CRANKK_FOUND_BANK 
                     (create-FOUND_DEBIT-guard) penalty)
                 )
                 (update staking3 (compound-key2 address code)
                  {"closedAt": (get-time)}
                 ) 
                 (format "{}, {}, {}, {}" [value retPerc retAmnt penalty])    
              )
            )
        )
    )
  )

  (defun unstake-sold (address code)
    "Un-stake when sold"
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-read stakeMenu2 code 
          {"valueInCRKK":= value,
           "destakeWait":= destakeWait,
           "term":= term,
           "earlyPenalty":= earlyPenalty,
           "lockup":= lockup}
    
            (with-read staking3 (compound-key2 address code)
              {
               "stakedAt":= stakedAt,
               "destakedAt":= destakedAt,
               "closedAt":= closedAt,
               "value":= value
              }
              (enforce (= closedAt (get-epoch)) "Already closed")
                 (with-capability (STAKE_DEBIT) 
                     (install-capability (free.crankk01.TRANSFER RADIO01_STAKE_BANK address value))
                     (free.crankk01.transfer RADIO01_STAKE_BANK address value)
                 )
                 (update staking3 (compound-key2 address code)
                  {"closedAt": (get-time)}
                 )
                 (format "{}, {}" [address value])   
              )
        )
    )
  )

  (defun get-stakings ()
    "Get all stakings"
    (select staking3 (constantly true))
  )

  (defun get-stake-menu-extras ()
    "Get staking extras"
    (select stakeExtra (constantly true))
  )

  (defun get-stakings-for-code (code)
    "Get all stakings for a code"
    (select staking3 (where 'code (= code)))
  )

  (defun get-my-pubkey ()
       (with-default-read nodes7 (at "sender" (chain-data)) 
           {"pubkey": ""} 
           {"pubkey":= pubkey}
           (format "{}" [pubkey])
       )   
  )

  (defun set-my-pubkey (pubkey)
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
       (update nodes7 (at "sender" (chain-data)) 
           {"pubkey": pubkey} 
       )   
  )

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

  (defun get-nodes ()
    "Get all nodes7"
    (select nodes7 (constantly true))
  )

  (defun get-my-node ()
    "Get my node"
    (read nodes7 (at "sender" (chain-data)))
  )

  (defun get-my-gatewayExtras ()
    "Get my node"
    (with-read nodes7 (at "sender" (chain-data))
        {"gatewayId":= gatewayId}
        (read gatewayExtras7 gatewayId)
    )
  )

  (defun get-nodeExtras ()
    "Get all nodeExtras"
    (select nodeExtras9 (constantly true))
  )

  (defun get-gatewayExtras ()
    "Get all gatewayExtras"
    (select gatewayExtras7 (constantly true))
  )

  (defun get-gatewayGPSs ()
    "Get all gatewayGPSs"
    (select gatewayGPSs1 (constantly true))
  )

  (defun get-currentGPSs ()
    "Get current gatewayGPSs"
    (select gatewayGPSs1 (where 'lastDetected (< (add-time (get-time) (days -1)))))
  )

  (defun update-node-extra (address oracle)
  (with-read radmin "admin"
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

  (defun update-gateway-extra (gatewayId online hosted factor)
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"address":= sender, "oracle":= oracle}
    (enforce (= oracle true) "Must be an oracle node")
    (with-default-read gatewayExtras7 gatewayId 
      {"gatewayId": "", "online": false, "hosted": false, "factor": 0.1}
      {"gatewayId":= gatewayIdDb, "online":= onlineDb, "hosted":= hostedDb, "factor":= factorDb}
      (if (= gatewayIdDb "")
      (insert gatewayExtras7 gatewayId 
          {"gatewayId": gatewayId, "online": online, "hosted": hosted, "factor": factor}
      )
      (update gatewayExtras7 gatewayId
          {"online": online, "hosted": hosted, "factor": factor}
      )
      )    
    )
  )
  )

  (defun update-gateway-gps (gatewayId)
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"address":= sender, "oracle":= oracle}
    (enforce (= oracle true) "Must be an oracle node")
    (with-default-read gatewayGPSs1 gatewayId 
      {"gatewayId": ""}
      {"gatewayId":= gatewayIdDb}
      (if (= gatewayIdDb "")
      (insert gatewayGPSs1 gatewayId 
          {"gatewayId": gatewayId, "lastDetected": (get-time)}
      )
      (update gatewayGPSs1 gatewayId
          {"gatewayId": gatewayId, "lastDetected": (get-time)}
      )
      )    
    )
  )
  )

  (defun update-gateway-directory (gatewayId firstTime chain online hosted factor)
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"address":= sender, "oracle":= oracle}
    (enforce (= oracle true) "Must be an oracle node")
    (with-default-read gatewayDirectory gatewayId 
      {"gatewayId": "", "firstTime":firstTime, "chain": 0, "online": false, "hosted": false, "factor": 0.1}
      {"gatewayId":= gatewayIdDb, "firstTime":= firstTimeDb, "chain":= chainDb, "online":= onlineDb, "hosted":= hostedDb, "factor":= factorDb}
      (if (= gatewayIdDb "")
          (insert gatewayDirectory gatewayId 
              {"gatewayId": gatewayId, "firstTime": firstTime, "chain": chain, "online": online, "hosted": hosted, "factor": factorDb}
          )
          ""    
    ;   (update gatewayDirectory gatewayId
    ;       {"gatewayId": gatewayId, "firstTime": firstTime, "chain": chain, "online": online, "hosted": hosted, "factor": factorDb}
    ;   )
      )    
    )
  )
  )

  (defun insert-my-node (gatewayId: string password:string)
    "Insert my node"
        (with-default-read gwTransfers9 gatewayId
        {"transferable": false, "hashedPw": ""}
        {"transferable":= transferable, "hashedPw":= hashedPw}
        (enforce (= transferable true) "Must be transferable to get new owner")
        (enforce (= (hash password) hashedPw) "Takeover password must match")
            (insert nodes7 (at "sender" (chain-data))
                {"address": (at "sender" (chain-data)),
                 "guard": (read-keyset "keyset"),
                 "gatewayId": gatewayId,
                 "pubkey": "",
                 "send": false,
                 "director": "",
                 "pubkeyd": "",
                 "net": (get-time),
                 "sent": "",
                 "consMember": false,
                 "validReceives": [],
                 "lastAction": (get-time)}
            )
            (insert gateways7 gatewayId
                {"address": (at "sender" (chain-data)),
                 "receives": []}
            )
            (update gwTransfers9 gatewayId
                {"hashedPw": "",
                 "transferable": false}
            )
        )
  )

  (defun insert-my-node-with-transfer (gatewayId:string password:string)
    "Insert my node with gateway transfer"
    (with-default-read gwTransfers9 gatewayId
        {"transferable": false, "hashedPw": ""}
        {"transferable":= transferable, "hashedPw":= hashedPw}
        (enforce (= transferable true) "Must be transferable to get new owner")
        (enforce (= (hash password) hashedPw) "Takeover password must match")
        (insert nodes7 (at "sender" (chain-data))
            {"address": (at "sender" (chain-data)),
             "guard": (read-keyset "keyset"),
             "gatewayId": gatewayId,
             "pubkey": "",
             "send": false,
             "director": "",
             "pubkeyd": "",
             "net": (get-time),
             "sent": "",
             "consMember": false,
             "validReceives": [],
             "lastAction": (get-time)}
        )
        (update gateways7 gatewayId
            {"address": (at "sender" (chain-data)),
             "receives": []}
        )
        (update gwTransfers9 gatewayId
            {"hashedPw": "",
             "transferable": false}
        )
    )
  )

  (defun move-stake (address:string code:string)
    "Move stake to new account"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard]))) 
    (with-read staking3 (compound-key2 (at "sender" (chain-data)) code)
        {
         "gatewayId":= gatewayId,
         "value":= value,
         "stakedAt":= stakedAt,
         "destakedAt":= destakedAt,
         "closedAt":= closedAt,
         "validFrom":= validFrom,
         "validTo":= validTo,
         "term":= term,
         "lockedUntil":= lockedUntil,
         "penalty":= penalty
         }
        (update staking3 (compound-key2 (at "sender" (chain-data)) code)
            {
             "closedAt": (get-time),
             "lastAction": (get-time)}
        )
        (insert staking3 (compound-key2 address code)
            {"addrCode": (compound-key2 address code),
             "address": address,
             "code": code,
             "guard": (read-keyset "keyset"),
             "gatewayId": gatewayId,
             "value": value,
             "stakedAt": stakedAt,
             "destakedAt": destakedAt,
             "closedAt": closedAt,
             "validFrom": validFrom,
             "validTo": validTo,
             "term": term,
             "lockedUntil": lockedUntil,
             "penalty": penalty,
             "lastAction": (get-time)}
        )
    )
  )

  (defun transfer-gateway-to-account (address:string gatewayId:string password:string)
    "Insert my node with gateway transfer - by the current wallet"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard]))) 
    (with-read nodes7 (at "sender" (chain-data))
        {"gatewayId":= gatewayIdDb,
         "consMember":= consMember}
        (enforce (= gatewayId gatewayIdDb) "Provided gateway must match")
        (with-default-read gwTransfers9 gatewayId
            {"transferable": false, "hashedPw": ""}
            {"transferable":= transferable, "hashedPw":= hashedPw}
            (enforce (= transferable true) "Must be transferable to get new owner")
            (enforce (= (hash password) hashedPw) "Takeover password must match")
            (update nodes7 (at "sender" (chain-data))
                {
                 "gatewayId": (+ "_" gatewayId),
                 "consMember": false
                }
            )
            (insert nodes7 address
                {"address": address,
                 "guard": (read-keyset "keyset"),
                 "gatewayId": gatewayId,
                 "pubkey": "",
                 "send": false,
                 "director": "",
                 "pubkeyd": "",
                 "net": (get-time),
                 "sent": "",
                 "consMember": consMember,
                 "validReceives": [],
                 "lastAction": (get-time)}
            )
            (update gateways7 gatewayId
                {"address": address,
                 "receives": []}
            )
            (update gwTransfers9 gatewayId
                {"hashedPw": "",
                 "transferable": false}
            )
            (if (is-staked (at "sender" (chain-data)) "PONP")
                (move-stake address "PONP")
                ""
            )
        )
    )
  )

  (defun update-node-with-gateway (address:string gatewayId:string)
    "Update node with gateway"
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-read nodes7 address
            {"gatewayId":= gatewayIdDb}
            (update nodes7 address
                {"gatewayId": gatewayId}
            )
            (insert gateways7 gatewayId
                {"address": address,
                 "receives": []}
            )
        )
    )
  )

  (defun update-node-with-gateway-if-exists (address:string gatewayId:string)
    "Update node with gateway"
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-read nodes7 address
            {"gatewayId":= gatewayIdDb}
            (update nodes7 address
                {"gatewayId": gatewayId}
            )
            (update gateways7 gatewayId
                {"address": address,
                 "receives": []}
            )
        )
    )
  )

  (defun add-received (gatewayId:string mic:string)
    "Add received"
    (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
    (let ((received [{"address": (at "sender" (chain-data)), "mic": mic}]))
        (with-read gateways7 gatewayId
          {"receives":= receives}
          (if (< (length receives) 5)
            (update gateways7 gatewayId
                {"receives": (append receives received)}
            )
            "Maximum witnesses reached"
          )     
        )
    )
  )

;   (defun add-received-with-chain (gatewayId:string mic:string)
;     "Add received"
;     (enforce-guard (at 'guard (coin.details (at "sender" (chain-data)))))
;     (let ((received [{"address": (at "sender" (chain-data)), "mic": mic}]))
;         (with-read gateways7 gatewayId
;           {"receives":= receives}
;           (if (< (length receives) 5)
;             (update gateways7 gatewayId
;                 {"receives": (append receives received)}
;             )
;             "Maximum witnesses reached"
;           )     
;         )
;     )
;   )

  (defun add-received-with-chain (gatewayId:string mic:string chain:string)
    "Add received"
    ; (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (let ((received [{"address": (at "sender" (chain-data)), "mic": mic, "chain": chain}]))
        (with-read gateways7 gatewayId
          {"receives":= receives}
          (if (< (length receives) 5)
            (update gateways7 gatewayId
                {"receives": (append receives received)}
            )
            "Maximum witnesses reached"
          )     
        )
    )
  )

  (defun clear-received (gatewayId:string)
    "Clear received"
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (with-read gateways7 gatewayId
          {"receives":= receives}
            (update gateways7 gatewayId
                {"receives": []}
            )
        )
    )
  )

  (defun add-received-with-check (address:string gatewayId:string mic:string)
    "Add received"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (let ((received [{"address": address, "mic": mic}]))
        (with-read nodes7 address
            ;This ensures that receiver owns a Gateweay
            {"gatewayId":= receiverGw}
            (with-read gateways7 gatewayId
              {"receives":= receives}
              (if (< (length receives) 5)
                (update gateways7 gatewayId
                    {"receives": (append receives received)}
                )
                "Maximum witnesses reached"
              )     
            )
        )
        )
    )
  )

  (defun append (array1:[object:{receive}] array2:[object:{receive}]) 
    (+ array1 array2)
  )

  (defun close-send-receive (sender receivers:[object:{receive}] gateways:[object:{witness}])
    "Award send / receive"
        (with-read nodes7 sender
            {"gatewayId":= gatewayId,
             "send":= send,
             "sent":= sent,
             "net":= net}
            (if (and (= send false) (!= sent ""))
                (close-send-receive-internal sender receivers gateways)
                "Already closed....."
            )
        )
  )

  (defun get-rand:integer ()
    (str-to-int (take 3 (format-time "%v" (get-time))))
  )

  (defun close-send-receive-internal (sender receivers:[object:{receive}] gateways:[object:{witness}])
    "Award send / receive"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodes7 (at "sender" (chain-data))
        {"consMember":= consMember}
        (enforce (= consMember true) "Must be consensus member")
        (with-capability (INTERNAL)
        (with-read nodes7 sender
            {"gatewayId":= gatewayId,
             "send":= send,
             "sent":= sent,
             "net":= net}
            (enforce (= send false) "Send status must be false by now")
            (enforce (!= sent "") "Node must send first")
            (enforce (< net (get-time)) "Send time must be in the past")
            (update nodes7 sender
                {"sent": "",
                 "net": (add-time (get-time) (+ 14000 (get-rand))),
                 "validReceives": gateways}
            )
            (with-default-read gateways7 gatewayId
              {"address": ""}
              {"address":= addressDb}
              (if (!= addressDb "")
                (update gateways7 gatewayId
                    {"receives": []}
                )
                ""
              )
            )
            ;Award sender if received by at least one
            ;Award correct receivers
            (if (> (length receivers) 0)
                (let*
                  ((base (get-base))
                   (amount (round (/ base (length receivers)) 5)))
                   (if (has-gps sender)    
                       (award sender (round (* base 1.01) 4))
                       (award sender base)
                   )    
                   (fold (award-receivers) amount receivers)
                   ;Award consensus member 
                   (award (at "sender" (chain-data)) (round (/ base 10) 5))
                )
                (let
                  ((base (get-base)))
                   (with-default-read gatewayExtras7 gatewayId 
                      {"gatewayId": "", "online": false, "hosted": false, "factor": 0.1}
                      {"gatewayId":= gatewayIdDb, "online":= onlineDb, "hosted":= hostedDb, "factor":= factorDb}
                       (if (= onlineDb true)
                           (if (has-gps sender)    
                               (award sender (round (* base 1.01) 4))
                               (award sender base)
                           )    
                           (award sender 0.00001)
                       )
                   )
                   ;Award consensus member 
                   (award (at "sender" (chain-data)) (round (/ base 10) 5))
                )
            )
        )
        )
    )
  )

  (defun award-receivers (amount:decimal receiver:object{receive})
    "Award correct receivers"
    (require-capability (INTERNAL))
    (bind receiver {"address":= address, "gatewayId":= gatewayId}
       (if (has-gps address)    
           (award address (round (* amount 1.01) 4))
           (award address amount)
       )    
    )
    amount
  )
  
  (defun account-exists-crkk (address)
      (!= (format "{}" [(at 'guard (free.crankk01.details-new address))]) "")
  )
  
  (defun award (address:string amount:decimal)
    "Award"
      (require-capability (INTERNAL))
      (if (account-exists-crkk address)
          (with-capability (AWARD_DEBIT) 
              (install-capability (free.crankk01.TRANSFER RADIO01_AWARD_BANK address (final-amount address amount)))
              (free.crankk01.transfer RADIO01_AWARD_BANK address (final-amount address amount))
          ) 
          ""        
      )
  )

  (defun final-amount (address amount) 
      (if (is-staked address "PONP")
         amount
         0.00002
      )
  )

  (defun has-gps (address) 
    (with-default-read nodes7 address
        {"gatewayId": ""}
        {"gatewayId":= gatewayId}
        (with-default-read gatewayGPSs1 gatewayId
            {"lastDetected": (get-epoch)}
            {"lastDetected":= lastDetected}
            (> (add-time lastDetected (days 1)) (get-time))
        )
    )    
  )

  (defun update-sent (mic:string)
    "Update sent"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodes7 (at "sender" (chain-data))
        {"send":= send,
         "sent":= sent}
        (if (and (= send true) (= sent ""))
            (update nodes7 (at "sender" (chain-data))
                {"sent": mic,
                 "send": false,
                 "lastAction": (get-time)}
            )
            "Has not been directed to send or already sent...."
        )    
    )
  )

  (defun update-sent-with-check (address:string mic:string)
    "Update sent"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (with-read nodes7 address
            {"send":= send,
             "sent":= sent}
            (update nodes7 address
                {"sent": mic,
                 "send": false,
                 "lastAction": (get-time)}
            )
        )
    )
  )

  (defun set-net (address)
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (update nodes7 address
            {"net": (get-time)}
        )
      )
  )

  (defun direct-to-send (address)
    "Direct to send"
    (with-read nodes7 address
        {"send":= send,
         "sent":= sent,
         "net":= net}
        (if (and (and (= send false) (= sent "")) (< net (get-time)))
          (direct-to-send-internal address)
          "Already directed...."
        )
    )
  )
  
  (defun direct-to-send-internal (address)
    "Direct to send"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodes7 (at "sender" (chain-data))
        {"consMember":= consMember,
         "pubkey":= pubkey}
        (enforce (= consMember true) "Must be consensus member")
        (enforce (!= pubkey "") "Pubkey must be set for director")
        (with-read nodes7 address
            {"send":= send,
             "sent":= sent,
             "net":= net}
            (enforce (= send false) "Already directed to send")
            (enforce (= sent "") "Sent and not processed yet")
            (enforce (< net (get-time)) "NET violated")
            (with-capability (INTERNAL)
                (save-to-mod)
            )
            (if (< (get-modSum) 35)
                (update nodes7 address
                    {"send": true,
                     "pubkeyd": pubkey,
                     "director": (at "sender" (chain-data)),
                     "validReceives": []}
                )
                (update nodes7 address
                    {"net": (add-time (get-time) (+ 3000 (get-rand)))}
                )
            )
        )  
    )
  )
  
  (defun read-admin ()
    "Read admin guard"
    (with-read radmin "admin"
      { "admin":= admin,
      "aguard":= aguard }
    (format "Admin guard is {}, admin {}" [aguard admin]))
  )

  (defun set-cons-member (address)
      "Set cons member"
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
          (update nodes7 address 
            {"consMember": true}
          )
      )
  )

  (defun set-cons-member-oracle (address)
      "Set cons member - Oracle"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
          (update nodes7 address 
            {"consMember": true}
          )
    )
  )

  (defun set-cons-member-off (address)
      "Set cons member"
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
          (update nodes7 address 
            {"consMember": false}
          )
      )
  )

  (defun set-cons-member-off-oracle (address)
      "Set cons member off - Oracle"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
          (update nodes7 address 
            {"consMember": false}
          )
    )
  )

  (defschema bank
     address: string
  )    

  (deftable banks:{bank})

  (defun add-bank (address)
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (insert banks "licBank"
            {"address": address}
        )
      )
  )

  (defschema affiliate
     address: string
     percentage: decimal
  )    

  (deftable affiliates1:{affiliate})

  (defun add-affiliate (affCode address percentage)
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (insert affiliates1 affCode
            {"address": address,
             "percentage": percentage}
        )
      )
  )

  (defun update-affiliate (affCode address percentage)
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (update affiliates1 affCode
            {"address": address,
             "percentage": percentage}
        )
      )
  )

  (defschema licenseType
     priceUSD: decimal
     priceKDA: decimal
     USDtoKDA: decimal
  )    

  (deftable licenseTypes1:{licenseType})

  (defun add-licenseType (code:string priceUSD priceKDA USDtoKDA)
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (insert licenseTypes1 code
            {"priceUSD": priceUSD,
             "priceKDA": priceKDA,
             "USDtoKDA": USDtoKDA}
        )
      )
  )

  (defun update-priceUSD (code:string priceUSD)
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (update licenseTypes1 code
            {"priceUSD": priceUSD}
        )
      )
  )

  (defun update-USDtoKDA (code:string USDtoKDA)
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodeExtras9 (at "sender" (chain-data))
        {"oracle":= oracle}
        (enforce (= oracle true) "Must be an oracle node")
        (with-read licenseTypes1 code
            {"priceUSD":= priceUSD}
            (update licenseTypes1 code
                {"priceKDA": (round (* priceUSD USDtoKDA) 2), 
                 "USDtoKDA": USDtoKDA}
            )
        )
    )
  )

  (defschema license
     address: string
     guard: guard
;     hashedPw: string
     usedForGw: string
     boughtAt: time
     refundedAt: time
     usedAt: time
     used: bool
     refunded: bool
     affCode: string
     kdaPaid: decimal
     affAmount: decimal
  )    

  (deftable licenses4:{license})

  (defschema license6
     address: string
     guard: guard
;     hashedPw: string
     usedForGw: string
     boughtAt: time
     refundedAt: time
     usedAt: time
     affPaidAt: time
     used: bool
     refunded: bool
     affPaid: bool
     affCode: string
     kdaPaid: decimal
     affAmount: decimal
  )    
  (deftable licenses6:{license6})

  (defun buy-license (affCode)
      "Buy license"
      (with-read licenseTypes1 "license150"
        {"priceKDA" := priceKDA}
    ;   (with-read affiliates1 affCode
    ;     {"address" := affAddress,
    ;      "percentage":= affPerc}
        (bind (free.crankkAffs.get-affiliate affCode) 
          {"address":= affAddress, "percentage":= affPerc, "approved":= approved}
          (enforce approved "Affiliate not approved")
          (insert licenses6 (at "sender" (chain-data)) 
            {"address": (at "sender" (chain-data)),
            "guard": (read-keyset "keyset"),
            "usedForGw": "",
            "used": false,
            "refunded": false,
            "affPaid": false,
            "usedAt": (get-epoch),
            "refundedAt": (get-epoch),
            "affPaidAt": (get-epoch),
            "boughtAt": (get-time),
            "affCode": affCode,
            "kdaPaid": priceKDA,
            "affAmount": (round (/ (* priceKDA affPerc) 100) 2)}
          )
          (coin.transfer (at "sender" (chain-data)) NEW_LIC_BANK priceKDA)
      )
      )    
  )

  (defun create-license (address)
      "Create license"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))   
    (with-read nodeExtras9 (at "sender" (chain-data))
      {"oracle":= oracle}
        (enforce (= oracle true) "Needs to be an oracle node")
          (insert licenses4 address 
            {"address": address,
            "guard": (read-keyset "keyset"), ;keyset needs to match the address
            "usedForGw": "",
            "used": false,
            "refunded": false,
            "usedAt": (get-epoch),
            "refundedAt": (get-epoch),
            "boughtAt": (get-time),
            "affCode": "CRKK",
            "kdaPaid": 0.0,
            "affAmount": 0.0}
          )
    )  
  )

  (defun move-license (oldAddress newAddress)
      "Move license to another wallet"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))   
    (with-read nodeExtras9 (at "sender" (chain-data))
      {"oracle":= oracle}
        (enforce (= oracle true) "Needs to be an oracle node")
      (with-read licenses4 oldAddress
        {"used":= used}
      (enforce (= used false) "Needs to unused")
      (insert licenses4 newAddress 
        {"address": newAddress,
        "guard": (read-keyset "keyset"),
        "usedForGw": "",
        "used": false,
        "refunded": false,
        "usedAt": (get-epoch),
        "refundedAt": (get-epoch),
        "boughtAt": (get-time),
        "affCode": "transferred",
        "kdaPaid": 0.0,
        "affAmount": 0.0}
      )
      (update licenses4 oldAddress 
        {"used": true,
        "usedAt": (get-time)}
      )
      )    
  ))

  (defun mitigate-license (amount:decimal)
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
          (let* 
            ((admAddress (at 'admin (read radmin "admin" ['admin]))))
                (with-capability (LIC_DEBIT) 
                  (install-capability (coin.TRANSFER NEW_LIC_BANK admAddress amount))
                  (coin.transfer NEW_LIC_BANK admAddress amount)
                )
          )
    )
  )     

  (defun mitigate-selfaff-bank (amount:decimal)
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
          (let* 
            ((admAddress (at 'admin (read radmin "admin" ['admin]))))
                  (install-capability (coin.TRANSFER CRANKK_SELFAFF_BANK admAddress amount))
                  (coin.transfer CRANKK_SELFAFF_BANK admAddress amount)
          )
    )
  )     

  (defun mitigate-award-bank (amount:decimal)
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
          (let* 
            ((admAddress (at 'admin (read radmin "admin" ['admin]))))
            (install-capability (free.crankk01.TRANSFER RADIO01_BANK admAddress amount))
            (free.crankk01.transfer RADIO01_BANK admAddress amount)
          )
    )
  )     

  (defun license-is-new ()    
    (with-default-read licenses6 (at "sender" (chain-data))
        {"address": ""}
        {"address":= address}
        (if (!= address "") true false)
    )
  )

  (defun activate-license (gatewayId:string)
    (if (license-is-new)
        (activate-license-new gatewayId)
        (activate-license-old gatewayId)
    )
  )

  (defun activate-license-new (gatewayId:string)
      "Activate license"
      (enforce-guard (at 'guard (read licenses6 (at "sender" (chain-data)) ['guard])))
      (with-read licenses6 (at "sender" (chain-data))
        {"used":= used,
         "refunded":= refunded,
         "affCode":= affCode,
         "kdaPaid":= kdaPaid,
         "affPaid":= affPaid,
         "affAmount":= affAmount}
          (enforce (= used false) "Already used")
          (enforce (= refunded false) "Already refunded")
            (insert nodes7 (at "sender" (chain-data))
                {"address": (at "sender" (chain-data)),
                 "guard": (read-keyset "keyset"),
                 "gatewayId": gatewayId,
                 "pubkey": "",
                 "send": false,
                 "director": "",
                 "pubkeyd": "",
                 "net": (get-time),
                 "sent": "",
                 "consMember": false,
                 "validReceives": [],
                 "lastAction": (get-time)}
            )
            (with-default-read gateways7 gatewayId
              {"address": "123"}
              {"address":= addressDb}
              (if (= addressDb "123")
                (insert gateways7 gatewayId
                    {"address": (at "sender" (chain-data)),
                     "receives": []}
                )
                (update gateways7 gatewayId
                    {"address": (at "sender" (chain-data)),
                     "receives": []}
                )
              )
            )
            (update licenses6 (at "sender" (chain-data)) 
              {"used": true,
              "affPaidAt": (get-time),
              "affPaid": true,    
              "usedAt": (get-time)}
            )

            (bind (free.crankkAffs.get-affiliate affCode) 
                {"address":= affAddress, "percentage":= affPerc}
                (with-capability (LIC_DEBIT) 
                    (install-capability (coin.TRANSFER NEW_LIC_BANK affAddress affAmount))
                    (if (= affPaid false)
                        (coin.transfer NEW_LIC_BANK affAddress affAmount)
                        ""
                    )    
                    (install-capability (coin.TRANSFER NEW_LIC_BANK (at "sender" (chain-data)) (round (/ kdaPaid 30) 3)))
                    (if (!= affCode "C_CRANKK")
                        (coin.transfer NEW_LIC_BANK (at "sender" (chain-data)) (round (/ kdaPaid 30) 3))
                        ""
                    )
                )
            )
      )
  )

  (defun pay-aff-after-2weeks (address:string)
      "Pay affiliate 2 weeks after purchase if license still unactivated"
      (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))   
      (with-read nodeExtras9 (at "sender" (chain-data))
          {"oracle":= oracle}
          (enforce (= oracle true) "Must be an oracle node")

          (with-read licenses6 address
            {"used":= used,
             "refunded":= refunded,
             "affCode":= affCode,
             "kdaPaid":= kdaPaid,
             "affPaid":= affPaid,
             "boughtAt":= boughtAt,
             "affAmount":= affAmount}
              (enforce (= used false) "Already used")
              (enforce (= refunded false) "Already refunded")
              (enforce (= affPaid false) "Affiliate already paid")
              (enforce (< boughtAt (add-time (get-time) (days -14))) "2 weeks not passed")
              (update licenses6 address 
                  {
                  "affPaidAt": (get-time),
                  "affPaid": true 
                  }
              )
    
              (bind (free.crankkAffs.get-affiliate affCode) 
                  {"address":= affAddress, "percentage":= affPerc}
                  (with-capability (LIC_DEBIT) 
                    (install-capability (coin.TRANSFER NEW_LIC_BANK affAddress affAmount))
                    (coin.transfer NEW_LIC_BANK affAddress affAmount)
                  )
              )
          )
      )
  )


  (defun activate-license-old (gatewayId:string)
      "Activate license"
      (enforce-guard (at 'guard (read licenses4 (at "sender" (chain-data)) ['guard])))
      (with-read licenses4 (at "sender" (chain-data))
        {"used":= used,
         "refunded":= refunded,
         "affCode":= affCode,
         "kdaPaid":= kdaPaid}
          (enforce (= used false) "Already used")
          (enforce (= refunded false) "Already refunded")
            (insert nodes7 (at "sender" (chain-data))
                {"address": (at "sender" (chain-data)),
                 "guard": (read-keyset "keyset"),
                 "gatewayId": gatewayId,
                 "pubkey": "",
                 "send": false,
                 "director": "",
                 "pubkeyd": "",
                 "net": (get-time),
                 "sent": "",
                 "consMember": false,
                 "validReceives": [],
                 "lastAction": (get-time)}
            )
            (with-default-read gateways7 gatewayId
              {"address": "123"}
              {"address":= addressDb}
              (if (= addressDb "123")
                (insert gateways7 gatewayId
                    {"address": (at "sender" (chain-data)),
                     "receives": []}
                )
                (update gateways7 gatewayId
                    {"address": (at "sender" (chain-data)),
                     "receives": []}
                )
              )
            )
            (update licenses4 (at "sender" (chain-data)) 
              {"used": true,
              "usedAt": (get-time)}
            )
            ; (with-read affiliates1 affCode
            ;     {"address" := affAddress,
            ;      "percentage":= affPerc}
            ;   (install-capability (coin.TRANSFER RADIO01_LICENSE_BANK affAddress (round (* kdaPaid affPerc) 3)))
            ;   (coin.transfer RADIO01_LICENSE_BANK affAddress (round (* kdaPaid affPerc) 3))

            ; )

      )
  )

;   (defun reverse-license ()
;       "Reverse license"
;       (enforce-guard (at 'guard (read licenses4 (at "sender" (chain-data)) ['guard])))
;       (with-read licenses4 (at "sender" (chain-data))
;         {"kdaPaid":= kdaPaid,
;          "used":= used,
;          "refunded":= refunded}
;           (enforce (= used false) "Already used")
;           (enforce (= refunded false) "Already refunded")
;           (install-capability (coin.TRANSFER RADIO01_LICENSE_BANK (at "sender" (chain-data)) kdaPaid))
;           (coin.transfer RADIO01_LICENSE_BANK (at "sender" (chain-data)) kdaPaid)
;           (update licenses4 (at "sender" (chain-data)) 
;             {"refunded": true,
;             "refundedAt": (get-time)}
;           )
;       )
;   )

  (defun unreverse-license (address)
      "unReverse license - admin use"
    (with-read radmin "admin"
    { "aguard" := aguard }
      (enforce-guard aguard)
      (with-read licenses4 address
        {"kdaPaid":= kdaPaid,
         "used":= used,
         "refunded":= refunded}
          (enforce (= used false) "Already used")
          (enforce (= refunded true) "Refunded")
          (update licenses4 address 
            {"refunded": false,
            "refundedAt": (get-epoch)}
          )
      )
    )
  )

  (defun set-gateway-transferable (gatewayId hashedPw)
      "Set gateway transferable"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))   
    (with-read nodeExtras9 (at "sender" (chain-data))
      {"oracle":= oracle}
        (enforce (= oracle true) "Needs to be an oracle node")
          (with-default-read gwTransfers9 gatewayId
            {"gatewayId": "123"}
            {"gatewayId":= gatewayIdDb}
              (if (= gatewayIdDb "123")    
                  (insert gwTransfers9 gatewayId 
                    {"gatewayId": gatewayId,
                    "hashedPw": hashedPw,
                    "transferable": true}
                  )
                  (update gwTransfers9 gatewayId 
                    {"gatewayId": gatewayId,
                    "hashedPw": hashedPw,
                    "transferable": true}
                  )
              )
          )
      )
  )

  (defun set-gateway-transferable-admin (gatewayId hashedPw)
  "Set gateway transferable"
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
          (with-default-read gwTransfers9 gatewayId
            {"gatewayId": "123"}
            {"gatewayId":= gatewayIdDb}
              (if (= gatewayIdDb "123")    
                  (insert gwTransfers9 gatewayId 
                    {"gatewayId": gatewayId,
                    "hashedPw": hashedPw,
                    "transferable": true}
                  )
                  (update gwTransfers9 gatewayId 
                    {"gatewayId": gatewayId,
                    "hashedPw": hashedPw,
                    "transferable": true}
                  )
              )
          )
    )
  )

  (defschema mods
    mod0:integer
    mod1:integer
    mod2:integer
    mod3:integer
    mod4:integer
  )

  (deftable sendMods:{mods})

  (defun insert-sendMods ()
    (insert sendMods "sendrate"
        {"mod0": 0, "mod1": 0, "mod2": 0, "mod3": 0, "mod4": 0}
    )  
  )

  (defun get-modSum:integer ()
    (with-read sendMods "sendrate"
      {"mod0":= mod0, "mod1":= mod1, "mod2":= mod2, "mod3":= mod3, "mod4":= mod4}
          (+ mod0 (+ mod1 (+ mod2 (+ mod3 mod4))))
    )
  )

  (defun save-to-mod:integer ()
    (require-capability (INTERNAL))
    (let (
        (lo (mod (str-to-int (format-time "%M" (get-time))) 5))
        )
        (with-read sendMods "sendrate"
          {"mod0":= mod0, "mod1":= mod1, "mod2":= mod2, "mod3":= mod3, "mod4":= mod4}
            (if (= lo 0)
                (update sendMods "sendrate" 
                  {"mod0": (+ mod0 1), "mod1": 0, "mod2": 0, "mod3": 0, "mod4": 0}
                )
                ""
            )
            (if (= lo 1)
                (update sendMods "sendrate" 
                  {"mod0": 0, "mod1": (+ mod1 1), "mod2": 0, "mod3": 0, "mod4": 0}
                )
                ""
            )
            (if (= lo 2)
                (update sendMods "sendrate" 
                  {"mod0": 0, "mod1": 0, "mod2": (+ mod2 1), "mod3": 0, "mod4": 0}
                )
                ""
            )
            (if (= lo 3)
                (update sendMods "sendrate" 
                  {"mod0": 0, "mod1": 0, "mod2": 0, "mod3": (+ mod3 1), "mod4": 0}
                )
                ""
            )
            (if (= lo 4)
                (update sendMods "sendrate" 
                  {"mod0": 0, "mod1": 0, "mod2": 0, "mod3": 0, "mod4": (+ mod4 1)}
                )
                ""
            )
        )
    )
  )

  (defun get-sendMods ()
    "Get sendMods"
    (select sendMods (constantly true))
  )

  (defun compound-key2:string
    ( part1:string
      part2:string )
    (format "{}{}{}" [part1 ROW_KEY_SEPARATOR part2])
  )

  (defun get-sender-details (gatewayId) 
      (with-read gateways7 gatewayId
        {"address":= address}
        (read nodes7 address)     
      )     
  )

  (defun get-gateway (gatewayId) 
      (with-default-read gateways7 gatewayId
        {"receives": []}
        {"receives":= receives}
        (format "{}" [receives])
      )     
  )

  (defun get-gateway-details (gatewayId) 
      (read gateways7 gatewayId)     
  )

  (defun get-affiliate (affCode) 
      (read affiliates1 affCode)     
  )

  (defun get-gateways () 
    (select gateways7 (constantly true)) 
  )

  (defun get-licenses-old () 
    (select licenses4 (constantly true)) 
  )

  (defun get-licenses-new () 
    (select licenses6 (constantly true)) 
  )

  (defun get-licenses () 
    (+ (select licenses6 (constantly true)) (select licenses4 (constantly true)))
  )

  (defun get-license-old (address) 
    (read licenses4 address) 
  )

  (defun get-license (address) 
    (try (read licenses6 address) (read licenses4 address) )
  )

  (defun get-licenseTypes () 
    (select licenseTypes1 (constantly true)) 
  )

  (defun get-affiliates () 
    (select affiliates1 (constantly true)) 
  )

  (defun get-affcodes () 
    (keys affiliates1) 
  )

  (defun get-gateway-transfers () 
    (select gwTransfers9 (constantly true)) 
  )

  (defun get-stats () 
    (select node-stats7 (constantly true)) 
  )

  (defun get-gateways-keys () 
    (keys gateways7) 
  )

  (defun get-license-keys () 
    (keys licenses4) 
  )

  (defun init-bank ()
    (free.crankk01.create-account RADIO01_BANK (create-module-guard "RADIO01"))
  )

  (defun init-license-bank ()
    (coin.create-account RADIO01_LICENSE_BANK (create-module-guard "RADIO01"))
  )

  (defun init-self-aff-bank ()
    (coin.create-account CRANKK_SELFAFF_BANK (create-module-guard "RADIO01"))
  )

  (defun init-stake-bank ()
    (free.crankk01.create-account RADIO01_STAKE_BANK (create-module-guard "RADIO01"))
  )

;; Invoking capability definition
(defcap WITHDRAW (recipient:string amount:decimal)
  (compose-capability (STAKE_DEBIT))
)
;; Capability user guard: capability definition
(defcap STAKE_DEBIT () true)
;; Capability user guard: capability predicate function
(defun require-STAKE_DEBIT () 
  (require-capability (STAKE_DEBIT)))

;; Capability user guard: guard constructor
(defun create-STAKE_DEBIT-guard ()
  (create-user-guard (require-STAKE_DEBIT)))

;; Capability user guard: capability definition
(defcap AWARD_DEBIT () true)
;; Capability user guard: capability predicate function
(defun require-AWARD_DEBIT () 
  (require-capability (AWARD_DEBIT)))

;; Capability user guard: guard constructor
(defun create-AWARD_DEBIT-guard ()
  (create-user-guard (require-AWARD_DEBIT)))

;; Capability user guard: capability definition
(defcap FOUND_DEBIT () true)
;; Capability user guard: capability predicate function
(defun require-FOUND_DEBIT () 
  (require-capability (FOUND_DEBIT)))

;; Capability user guard: guard constructor
(defun create-FOUND_DEBIT-guard ()
  (create-user-guard (require-FOUND_DEBIT)))

;; Capability user guard: capability definition
(defcap LIC_DEBIT () true)
;; Capability user guard: capability predicate function
(defun require-LIC_DEBIT () 
  (require-capability (LIC_DEBIT)))

;; Capability user guard: guard constructor
(defun create-LIC_DEBIT-guard ()
  (create-user-guard (require-LIC_DEBIT)))


(defun init-award-bank ()
      (free.crankk01.transfer-create (at "sender" (chain-data)) RADIO01_AWARD_BANK 
        (create-AWARD_DEBIT-guard) 1.0)
)

(defun init-lic-bank ()
      (coin.transfer-create (at "sender" (chain-data)) NEW_LIC_BANK 
        (create-LIC_DEBIT-guard) 1.0)
)
)
; create-table
; (create-table rewardBase1)
; (create-table radmin)
; (create-table gwTransfers9)
