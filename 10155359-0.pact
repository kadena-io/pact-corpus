(module radio02 GOVERNANCE

  (use fungible-util)
  (use util.guards)

  (defconst ROW_KEY_SEPARATOR:string "/")
  (defconst RADIO01_BANK:string 'radio01-bank)

  (defcap GOVERNANCE ()
  (with-read radmin "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
  ))

  (defcap INTERNAL ()
    "mark some functions as internal only..."
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
    ;  gatewayId: string
     mic: string
  )    

  (defschema gateway
     address: string
     receives:[object:{receive}]
  )    

  (deftable gateways7:{gateway})

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
     validReceives:[string]
     lastAction: time
  )    

  (deftable nodes7:{node})

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

  (defun insert-my-node (gatewayId: string)
    "Insert my node"
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
  )

  (defun add-received (gatewayId:string mic:string)
    "Add received"
    (let ((received 
      [{"address": (at "sender" (chain-data)),
      "mic": mic}]))
        (with-read gateways7 gatewayId
          {"receives":= receives}
            (update gateways7 gatewayId
                {"receives": (append receives received)}
            )
        )
    )
  )

  (defun append (array1:[object:{receive}] array2:[object:{receive}]) 
    (+ array1 array2)
  )

  (defun close-send-receive (sender receivers:[object:{receive}] gateways:[string])
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
                (let
                  ((amount (/ 1.0 (length receivers))))
                  (award sender 1.0)
                  (fold (award-receivers) amount receivers)
                ) ""
            )
            ;Award consensus member 
            ; (award (at "sender" (chain-data)) 1.0)
        )
        )
    )
  )
  
  (defun award-receivers (amount:decimal receiver:object{receive})
    "Award correct receivers"
    (require-capability (INTERNAL))
    (bind receiver {"address":= address}
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
         "send": false}
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

  (defun get-gateways () 
    (select gateways7 (constantly true)) 
  )

  (defun get-gateways-keys () 
    (keys gateways7) 
  )

  (defun init-bank ()
    (free.crankk01.create-account RADIO01_BANK (create-module-guard "RADIO01"))
  )

)
; create-table
; (create-table radmin)
; (create-table nodes7)
; (create-table gateways7)





