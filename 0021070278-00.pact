(module radio02 GOVERNANCE

  (use fungible-util)
  (use util.guards)

  (defconst ROW_KEY_SEPARATOR:string "/")
  (defconst RADIO01_BANK:string 'radio01-bank)
  (defconst RADIO01_STAKE_BANK:string 'radio01-stake-bank)

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
     stakeWait: integer ; in seconds
     destakeWait: integer ; in seconds
     lastAction: time
  )    

  (deftable stakeMenu1:{stake-menu})

  (defschema staking
     addrCode: string ;same as key (address+code)
     code: string
     value: decimal ; in CRKK
     stakedAt: time
     destakedAt: time
     validFrom: time ; staked at plus stake wait
     validTo: time ; destaked at plus destake wait
     lastAction: time
  )    

  (deftable staking1:{staking})

  (defun insert-steak-menu (code name description valueInCRKK valueDefault tokenDefault validFrom validTo stakeWait destakeWait)
      (with-read radmin "admin"
        { "aguard" := aguard }
          (enforce-guard aguard)
          (insert stakeMenu1 code {
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
              "lastAction": (get-time)
              }
          )
       )
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
                ;   (stat "S" sender gatewayId gateways base)
                   (fold (award-receivers) amount receivers)
                   ;Award consensus member 
                   (award (at "sender" (chain-data)) (round (/ base 10) 5))
                ;   (stat "C" (at "sender" (chain-data)) gatewayId [] (round (/ base 10) 5))
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
        ; (stat "R" address gatewayId [] amount)  
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
; (create-table stakeMenu1)
; (create-table staking1)

