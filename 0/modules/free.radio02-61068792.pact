(module radio02 GOVERNANCE

  (use fungible-util)
  (use util.guards)

  (defconst ROW_KEY_SEPARATOR:string "/")
  (defconst RADIO01_BANK:string 'radio01-bank)
  (defconst RADIO01_STAKE_BANK:string 'radio01-stake01-bank)
  (defconst RADIO01_LICENSE_BANK:string 'radio01-license01-bank)
  (defconst CRANKK_SELFAFF_BANK:string 'crankk-selfaff-bank)
  (defconst RADIO01_AWARD_BANK:string 'radio01-award-bank) 

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

  (defschema gateway
     address: string
     receives:[object:{receive}]
  )    

  (deftable gateways7:{gateway})

  (defschema gatewayExtra
     gatewayId: string
     online: bool
     hosted: bool
     factor: decimal
  )    

  (deftable gatewayExtras7:{gatewayExtra})

  (defschema gwTransfer
     gatewayId: string
     transferable: bool
     hashedPw: string
  )    

  (deftable gwTransfers8:{gwTransfer})

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
        (with-default-read nodeExtras9 (at "sender" (chain-data))
            {"oracle": false}
            {"oracle":= oracle}
            (enforce (= oracle true) "Need to be an oracle node for now")
        )    
        ""
    )
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

  (defun insert-my-node (gatewayId: string password:string)
    "Insert my node"
        (with-default-read gwTransfers8 gatewayId
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
            (update gwTransfers8 gatewayId
                {"hashedPw": "",
                 "transferable": false}
            )
        )
  )

  (defun insert-my-node-with-transfer (gatewayId:string password:string)
    "Insert my node with gateway transfer"
    (with-default-read gwTransfers8 gatewayId
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
        (update gwTransfers8 gatewayId
            {"hashedPw": "",
             "transferable": false}
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

;   (defun update-node-with-gateway-ora (address:string gatewayId:string)
;     "Update node with gateway for oracle node"
;     (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
;     (with-read nodeExtras9 (at "sender" (chain-data))
;         {"oracle":= oracle}
;         (enforce (= oracle true) "Must be an oracle node")
;         (with-read nodes7 address
;             {"gatewayId":= gatewayIdDb}
;             (update nodes7 address
;                 {"gatewayId": gatewayId}
;             )
;             (insert gateways7 gatewayId
;                 {"address": address,
;                  "receives": []}
;             )
;         )
;     )
;   )

  (defun add-received (gatewayId:string mic:string)
    "Add received"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (let ((received [{"address": (at "sender" (chain-data)), "mic": mic}]))
        (with-read nodes7 (at "sender" (chain-data))
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
                 "net": (add-time (get-time) 14400),
                 "validReceives": gateways}
            )
            (update gateways7 gatewayId
                {"receives": []}
            )
            ;Award sender if received by at least one
            ;Award correct receivers
            (if (> (length receivers) 0)
                (let*
                  ((base (round (/ 91.32 100) 5))
                   (amount (round (/ base (length receivers)) 5)))
                   (award sender base)
                   (fold (award-receivers) amount receivers)
                   ;Award consensus member 
                   (award (at "sender" (chain-data)) (round (/ base 10) 5))
                )
                (let
                  ((base (round (/ 91.32 100) 5)))
                   (with-default-read gatewayExtras7 gatewayId 
                      {"gatewayId": "", "online": false, "hosted": false, "factor": 0.1}
                      {"gatewayId":= gatewayIdDb, "online":= onlineDb, "hosted":= hostedDb, "factor":= factorDb}
                       (if (= onlineDb true)
                           (award sender base)
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

  (defun close-send-receive-new (sender gateways:[object:{witness}])
    "Award send / receive"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read nodes7 (at "sender" (chain-data))
        {"consMember":= consMember}
        (enforce (= consMember true) "Must be consensus member")
        (with-capability (INTERNAL)
        (with-read nodes7 sender
            {"gatewayId":= gatewayId,
             "net":= net}
            (update nodes7 sender
                {"sent": "",
                 "net": (add-time (get-time) 14400),
                 "validReceives": gateways}
            )
            (update gateways7 gatewayId
                {"receives": []}
            )
            ;Award sender if received by at least one
            ;Award correct receivers
            (if (> (length gateways) 0)
                (let*
                  ((base (round (/ 91.32 100) 5))
                   (amount (round (/ base (length gateways)) 5)))
                   (award sender base)
                   (fold (award-receivers-new) amount gateways)
                   ;Award consensus member 
                   (award (at "sender" (chain-data)) (round (/ base 10) 5))
                )
                (let
                  ((base (round (/ 91.32 100) 5)))
                   (with-default-read gatewayExtras7 gatewayId 
                      {"gatewayId": "", "online": false, "hosted": false, "factor": 0.1}
                      {"gatewayId":= gatewayIdDb, "online":= onlineDb, "hosted":= hostedDb, "factor":= factorDb}
                       (if (= onlineDb true)
                           (award sender base)
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

  (defun stat (txn-type:string address:string sender:string receivers:[object:{receive}] award:decimal)
     (with-default-read node-stats7 address
        {"address": "", "txn-details": []}
        {"address":= addressOrig, "txn-details":= detailsOrig}
        (let* (
          (detail [{
            "sender": sender,
            "txn-type": txn-type,
            "message": "",
            "witnesses": receivers,
            "award": award,  
            "timestamp": (get-time)
          }])
          ;Max it at 2. Will work for less or more than 2    
          (detailsOrig (take (- 1) detailsOrig))     
        )
        (if (= addressOrig "")
        (insert node-stats7 address
          {
            "address": address,
            "txn-details": (+ detailsOrig detail),
            "lastAction": (get-time)
          }
        )
        (update node-stats7 address
          {
            "txn-details": (+ detailsOrig detail),
            "lastAction": (get-time)
          }
        )
        )
     ) 
    )
  )

  (defun award-receivers (amount:decimal receiver:object{receive})
    "Award correct receivers"
    (require-capability (INTERNAL))
    (bind receiver {"address":= address, "gatewayId":= gatewayId}
     (with-read nodes7 address
       {"address":= addressDb}
        (award address amount)
     )    
    )
    amount
  )
  
  (defun award-receivers-new (amount:decimal gateway:object{witness})
    "Award correct receivers"
    (require-capability (INTERNAL))
    (bind gateway {"id":= gatewayId}
      (with-read gateways7 gatewayId
        {"address":= address}
        (award address amount)
      )
    )
    amount
  )
  
  (defun award (address:string amount:decimal)
    "Award"
    (require-capability (INTERNAL))
    (with-default-read staking3 (compound-key2 address "PONP") 
      {"code": "123"}
      {"code":= code}
      (if (= code "123")
          (with-read nodes7 address
              {"guard":= nodeguard}
              (with-capability (AWARD_DEBIT) 
              (install-capability (free.crankk01.TRANSFER RADIO01_AWARD_BANK address 0.00002))
              (free.crankk01.transfer-create RADIO01_AWARD_BANK address nodeguard 0.00002)
              )    
          )
          (with-read nodes7 address
              {"guard":= nodeguard}
              (with-capability (AWARD_DEBIT) 
              (install-capability (free.crankk01.TRANSFER RADIO01_AWARD_BANK address amount))
              (free.crankk01.transfer-create RADIO01_AWARD_BANK address nodeguard amount)
              ) 
          )
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
        (if (and (= send false) (= sent ""))
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
            (update nodes7 address
                {"send": true,
                 "pubkeyd": pubkey,
                 "director": (at "sender" (chain-data)),
                 "validReceives": []}
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

;   (defun buy-license (affCode)
;       "Buy license"
;       (with-read licenseTypes1 "license150"
;         {"priceKDA" := priceKDA}
;       (with-read affiliates1 affCode
;         {"address" := affAddress,
;          "percentage":= affPerc}
;           (coin.transfer (at "sender" (chain-data)) RADIO01_LICENSE_BANK priceKDA)
;         ; Cannot pay the affiliate at this time since it's refundable
;         ;   (coin.transfer (at "sender" (chain-data)) affAddress 0.01)
;           (insert licenses4 (at "sender" (chain-data)) 
;             {"address": (at "sender" (chain-data)),
;             "guard": (read-keyset "keyset"),
;             "usedForGw": "",
;             "used": false,
;             "refunded": false,
;             "usedAt": (get-epoch),
;             "refundedAt": (get-epoch),
;             "boughtAt": (get-time),
;             "affCode": affCode,
;             "kdaPaid": priceKDA,
;             "affAmount": (round (/ (* priceKDA affPerc) 100) 2)}
;           )
;       )
;       )    
;   )

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
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
      (with-read licenses4 oldAddress
        {"guard":= guard}
      (insert licenses4 newAddress 
        {"address": newAddress,
        "guard": guard,
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
    ; (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))
    (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
          (let* 
            ((admAddress (at 'admin (read radmin "admin" ['admin]))))
                ; (with-read nodeExtras9 (at "sender" (chain-data))
                ;   {"oracle":= oracle}
                ;   (enforce (= oracle true) "Needs to be an oracle node")
                  (install-capability (coin.TRANSFER RADIO01_LICENSE_BANK admAddress amount))
                  (coin.transfer RADIO01_LICENSE_BANK admAddress amount)
                ; )
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

  (defun activate-license (gatewayId:string)
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

  (defun set-gateway-transferable (gatewayId hashedPw)
      "Set gateway transferable"
    (enforce-guard (at 'guard (read nodes7 (at "sender" (chain-data)) ['guard])))   
    (with-read nodeExtras9 (at "sender" (chain-data))
      {"oracle":= oracle}
        (enforce (= oracle true) "Needs to be an oracle node")
          (with-default-read gwTransfers8 gatewayId
            {"gatewayId": "123"}
            {"gatewayId":= gatewayIdDb}
              (if (= gatewayIdDb "123")    
                  (insert gwTransfers8 gatewayId 
                    {"gatewayId": gatewayId,
                    "hashedPw": hashedPw,
                    "transferable": true}
                  )
                  (update gwTransfers8 gatewayId 
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
          (with-default-read gwTransfers8 gatewayId
            {"gatewayId": "123"}
            {"gatewayId":= gatewayIdDb}
              (if (= gatewayIdDb "123")    
                  (insert gwTransfers8 gatewayId 
                    {"gatewayId": gatewayId,
                    "hashedPw": hashedPw,
                    "transferable": true}
                  )
                  (update gwTransfers8 gatewayId 
                    {"gatewayId": gatewayId,
                    "hashedPw": hashedPw,
                    "transferable": true}
                  )
              )
          )
    )
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
      (with-read gateways7 gatewayId
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

  (defun get-licenses () 
    (select licenses4 (constantly true)) 
  )

  (defun get-license (address) 
    (read licenses4 address) 
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
    (select gwTransfers8 (constantly true)) 
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

(defun init-award-bank ()
      (free.crankk01.transfer-create (at "sender" (chain-data)) RADIO01_AWARD_BANK 
        (create-AWARD_DEBIT-guard) 1.0)
)
)
; create-table
; (create-table radmin)
; (create-table nodeExtras9)
; (create-table gatewayExtras7)
; (create-table gateways7)
; (create-table licenses4)
; (create-table banks)
; (create-table affiliates1)
; (create-table licenseTypes1)
; (create-table stakeExtra)

 
