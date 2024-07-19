(module radio02 GOVERNANCE

  (use fungible-util)
  (use util.guards)

  (defconst ROW_KEY_SEPARATOR:string "/")
  (defconst RADIO01_BANK:string 'radio01-bank)
  (defconst RADIO01_STAKE_BANK:string 'radio01-stake-bank)
  (defconst RADIO01_LICENSE_BANK:string 'radio01-license-bank)

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
        (free.crankk01.transfer (at "sender" (chain-data)) RADIO01_STAKE_BANK value)
    )
  )

  (defun get-stakings ()
    "Get all stakings"
    (select staking3 (constantly true))
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

  (defun add-received (gatewayId:string mic:string)
    "Add received"
    (let ((received [{"address": (at "sender" (chain-data)), "mic": mic}]))
        (with-read nodes7 (at "sender" (chain-data))
            ;This ensures that receiver owns a Gateweay
            {"gatewayId":= receiverGw}
            (with-read gateways7 gatewayId
              {"receives":= receives}
                (update gateways7 gatewayId
                    {"receives": (append receives received)}
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
        (award address amount)
    )
    amount
  )
  
  (defun award (address:string amount:decimal)
    "Award"
    (require-capability (INTERNAL))
    (with-read nodes7 address
        {"guard":= nodeguard}
        (install-capability (free.crankk01.TRANSFER RADIO01_BANK address amount))
        (free.crankk01.transfer-create RADIO01_BANK address nodeguard amount)
    )
  )    

  (defun update-sent (mic:string)
    "Update sent"
    (update nodes7 (at "sender" (chain-data))
        {"sent": mic,
         "send": false,
         "lastAction": (get-time)}
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

  (deftable licenses2:{license})

  (defun buy-license (affCode)
      "Buy license"
    ;   (with-read banks "licBank"
    ;     {"address" := licBank}
      (with-read affiliates1 affCode
        {"address" := affAddress,
         "percentage":= affPerc}
          (coin.transfer (at "sender" (chain-data)) RADIO01_LICENSE_BANK 0.09)
        ; Cannot pay the affiliate at this time since it's refundable
        ;   (coin.transfer (at "sender" (chain-data)) affAddress 0.01)
          (insert licenses2 (at "sender" (chain-data)) 
            {"address": (at "sender" (chain-data)),
            "guard": (read-keyset "keyset"),
            "usedForGw": "",
            "used": false,
            "refunded": false,
            "usedAt": (get-epoch),
            "refundedAt": (get-epoch),
            "boughtAt": (get-time),
            "affCode": affCode,
            "kdaPaid": 0.09,
            "affAmount": (* 0.09 affPerc)}
          )
      )
    ;   )    
  )

  (defun set-gateway-transferable (gatewayId hashedPw)
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

  (defun get-gateways () 
    (select gateways7 (constantly true)) 
  )

  (defun get-licenses () 
    (select licenses2 (constantly true)) 
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

  (defun init-bank ()
    (free.crankk01.create-account RADIO01_BANK (create-module-guard "RADIO01"))
  )

  (defun init-stake-bank ()
    (free.crankk01.create-account RADIO01_STAKE_BANK (create-module-guard "RADIO01"))
  )

)
; create-table
; (create-table radmin)
; (create-table nodeExtras9)
; (create-table gatewayExtras7)
; (create-table gateways7)

