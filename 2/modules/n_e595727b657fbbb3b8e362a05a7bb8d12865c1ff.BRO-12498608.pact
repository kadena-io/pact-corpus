(module BRO GOVERNANCE

  (implements router-iface)

  ;; Imports
  (use hyperlane-message)

  (use token-message)

  (use router-iface)
  
  ;; Tables
  (deftable accounts:{fungible-v2.account-details})

  (deftable contract-state:{col-state})

  (deftable routers:{router-address})

  ;; Capabilities
  (defcap GOVERNANCE () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.upgrade-admin"))

  (defcap ONLY_ADMIN () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.bridge-admin"))

  (defcap INTERNAL () true)

  (defcap TRANSFER_REMOTE:bool 
    (
      destination:integer 
      sender:string
      recipient:string
      amount:decimal
    )
    (enforce (!= destination "0") "Invalid destination")
    (enforce (!= sender "") "Sender cannot be empty.")
    (enforce (!= recipient "") "Recipient cannot be empty.")
    (enforce-unit amount)
    (enforce (> amount 0.0) "Transfer must be positive.")
  )

  (defcap TRANSFER_TO:bool
    (
      target-chain:string 
    )
    (let 
      ((chain (str-to-int target-chain)))
      (enforce (and (<= chain 19) (>= chain 0)) "Invalid target chain ID")
    )
  )

  ;; Events
  (defcap SENT_TRANSFER_REMOTE
    (
      destination:integer
      recipient:string
      amount:decimal
    )
    @doc "Emitted on `transferRemote` when a transfer message is dispatched"
    @event true
  )

  (defcap RECEIVED_TRANSFER_REMOTE
    (
      origin:integer
      recipient:string
      amount:decimal
    )
    @doc "Emitted on `transferRemote` when a transfer message is dispatched"
    @event true
  )

  (defcap DESTINATION_GAS_SET
    (
      domain:integer
      gas:decimal
    )
    @doc "Emitted when a domain's destination gas is set."
    @event true
  )

  ;; Treasury 
  (defcap COLLATERAL () true)

  (defconst COLLATERAL_ACCOUNT (create-principal (create-treasury-guard)))

  (defun get-collateral-account ()
      COLLATERAL_ACCOUNT
  )
  
  (defun create-treasury-guard:guard ()
    (create-capability-guard (COLLATERAL))
  )

  (defun initialize (token:module{fungible-v2, fungible-xchain-v1})
    (with-capability (ONLY_ADMIN)
      (insert contract-state "default"
        {
          "token": token
        }
      )
      (token::create-account COLLATERAL_ACCOUNT (create-treasury-guard))
    )
  )

  (defun precision:integer () 12)

  (defun get-adjusted-amount:decimal (amount:decimal) 
    (* amount (dec (^ 10 (precision))))
  )

  (defun get-collateral-asset ()
    (with-read contract-state "default"
      {
        "token" := token:module{fungible-v2, fungible-xchain-v1}
      }
      token
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Router ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
  (defun enroll-remote-router:bool (domain:integer address:string)
    (with-capability (ONLY_ADMIN)
      (enforce (!= domain 0) "Domain cannot be zero")
      (insert routers (int-to-str 10 domain)
        {
          "remote-address": address
        }
      )
      true
    )
  )
  
  (defun has-remote-router:string (domain:integer)
    (with-default-read routers (int-to-str 10 domain)
      {
        "remote-address": "empty"
      }
      {
        "remote-address" := remote-address
      }
      (enforce (!= remote-address "empty") "Remote router is not available.")
      remote-address
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GasRouter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  (defun quote-gas-payment:decimal (domain:integer)
    (has-remote-router domain)
    (igp.quote-gas-payment domain)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TokenRouter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  (defun transfer-remote:string (destination:integer sender:string recipient-tm:string amount:decimal)
    (with-capability (TRANSFER_REMOTE destination sender recipient-tm amount)
      (let
        (
          (receiver-router:string (has-remote-router destination))
        )
        (transfer-from sender amount)
        receiver-router
      )
    ) 
  )
  
  (defun handle:bool 
    (
      origin:integer 
      sender:string 
      chainId:integer 
      reciever:string 
      receiver-guard:guard 
      amount:decimal
    )
    (require-capability (mailbox.ONLY_MAILBOX))
    (let
      (
        (router-address:string (has-remote-router origin))
      )
      (enforce (= sender router-address) "Sender is not router")
      (with-capability (INTERNAL)
        (if (= (int-to-str 10 chainId) (at "chain-id" (chain-data)))
          (transfer-create-to reciever receiver-guard amount)
          (transfer-create-to-crosschain reciever receiver-guard amount (int-to-str 10 chainId))
        )
        (emit-event (RECEIVED_TRANSFER_REMOTE origin reciever amount))
        true
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ERC20 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  (defun transfer-from (sender:string amount:decimal)
    (with-read contract-state "default"
      {
        "token" := token:module{fungible-v2, fungible-xchain-v1}
      }
      (token::transfer sender COLLATERAL_ACCOUNT amount)
    )
  )

  (defun transfer-create-to (receiver:string receiver-guard:guard amount:decimal)
    (require-capability (INTERNAL))
    (with-read contract-state "default"
      {
        "token" := token:module{fungible-v2, fungible-xchain-v1}
      }
      (with-capability (COLLATERAL)
        (install-capability (token::TRANSFER COLLATERAL_ACCOUNT receiver amount))
        (token::transfer-create COLLATERAL_ACCOUNT receiver receiver-guard amount)
      )
    )
  )

  (defun transfer-create-to-crosschain (receiver:string receiver-guard:guard amount:decimal target-chain:string)
    (require-capability (INTERNAL))
    (with-read contract-state "default"
      {
        "token" := token:module{fungible-v2, fungible-xchain-v1}
      }
      (with-capability (COLLATERAL)
        (install-capability (token::TRANSFER_XCHAIN COLLATERAL_ACCOUNT receiver amount target-chain))
        (token::transfer-crosschain COLLATERAL_ACCOUNT receiver receiver-guard target-chain amount)
      )
    )
  )

  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce (!= sender receiver) "same sender and receiver")
    (enforce-unit amount)
    (enforce (> amount 0.0) "Positive amount")
    (enforce-guard (at 'guard (read accounts sender)))
    (enforce (!= sender "") "valid sender")
    (enforce (!= receiver "") "valid receiver"))

  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0) (format "TRANSFER exceeded for balance {}" [managed]))
      newbal))
   
  (defcap TRANSFER_XCHAIN:bool (sender:string receiver:string amount:decimal target-chain:string)
      @managed amount TRANSFER_XCHAIN-mgr
      (enforce-unit amount)
      (enforce (> amount 0.0) "Cross-chain transfers require a positive amount")
      (enforce-guard (at 'guard (read accounts sender)))
      (enforce (!= sender "") "valid sender"))
    
  (defun TRANSFER_XCHAIN-mgr:decimal (managed:decimal requested:decimal)
      (enforce (>= managed requested)
        (format "TRANSFER_XCHAIN exceeded for balance {}" [managed]))
      0.0)

  (defun transfer:string (sender:string receiver:string amount:decimal)
    @model
      [ (property (= 0.0 (column-delta accounts "balance")))
        (property (> amount 0.0))
        (property (!= sender receiver))
      ]

    (with-capability (TRANSFER sender receiver amount)
      (with-read accounts sender { "balance" := sender-balance }
        (enforce (<= amount sender-balance) "Insufficient funds TRANSFER.")
        (update accounts sender { "balance": (- sender-balance amount) }))

      (with-read accounts receiver { "balance" := receiver-balance }
        (update accounts receiver { "balance": (+ receiver-balance amount) }))))

  (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    @model [ (property (= 0.0 (column-delta accounts "balance"))) ]

    (with-capability (TRANSFER sender receiver amount)
      (with-read accounts sender { "balance" := sender-balance }
        (enforce (<= amount sender-balance) "Insufficient funds TRANSFER_CREATE.")
        (update accounts sender { "balance": (- sender-balance amount) }))

      (with-default-read accounts receiver
        { "balance": 0.0, "guard": receiver-guard }
        { "balance" := receiver-balance, "guard" := existing-guard }
        (enforce (= receiver-guard existing-guard) "Supplied receiver guard must match existing guard.")
        (write accounts receiver
          { "balance": (+ receiver-balance amount)
          , "guard": receiver-guard
          , "account": receiver
          }))))

  (defun get-balance:decimal (account:string)
    (with-read contract-state "default"
      {
        "token" := token:module{fungible-v2, fungible-xchain-v1}
      }
      (token::get-balance account)
    )
  )

  (defun details:object{fungible-v2.account-details} (account:string)
    (enforce (!= account "") "Account name cannot be empty.")
    (read accounts account)
  )

  (defun enforce-unit:bool (amount:decimal)
    (enforce (>= amount 0.0) "Unit cannot be non-positive.")
    (enforce (= amount (floor amount (precision))) "Amounts cannot exceed precision.")
  )

  (defun create-account:string (account:string guard:guard)
    (with-read contract-state "default"
      {
        "token" := token:module{fungible-v2, fungible-xchain-v1}
      }
      (token::create-account account guard)
    )
  )

  (defun rotate:string (account:string new-guard:guard)
    (with-read contract-state "default"
      {
        "token" := token:module{fungible-v2, fungible-xchain-v1}
      }
      (token::rotate account new-guard)
    )
  )

  (defun transfer-crosschain:string (sender:string receiver:string receiver-guard:guard target-chain:string amount:decimal)
    (with-read contract-state "default"
      {
        "token" := token:module{fungible-v2, fungible-xchain-v1}
      }
      (token::transfer-crosschain sender receiver receiver-guard target-chain amount)
    )
  )


)


