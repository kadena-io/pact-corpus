(module kb-DAI GOVERNANCE
  (implements fungible-v2)

  (implements router-iface)

  ;; Imports
  (use hyperlane-message)

  (use token-message)

  (use router-iface)
  
  ;; Tables
  (deftable accounts:{fungible-v2.account-details})

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
  
  (defun precision:integer () 18)

  (defun get-adjusted-amount:decimal (amount:decimal) 
    (* amount (dec (^ 10 (precision))))
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
        (with-capability (INTERNAL)
          (transfer-from sender amount)
        )
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
      )
      (emit-event (RECEIVED_TRANSFER_REMOTE origin reciever amount))
      true
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ERC20 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  
  (defun transfer-from (sender:string amount:decimal)
    (require-capability (INTERNAL))
    (with-default-read accounts sender { "balance": 0.0 } { "balance" := balance }
        (enforce (<= amount balance) (format "Cannot burn more funds than the account has available: {}" [balance]))
        (update accounts sender { "balance": (- balance amount)})
    )
  )

  (defun transfer-create-to:string (receiver:string receiver-guard:guard amount:decimal)
    (require-capability (INTERNAL))
    (with-default-read accounts receiver
      { 
        "balance": 0.0, 
        "guard": receiver-guard 
      }
      { 
        "balance" := receiver-balance, 
        "guard" := existing-guard 
      }
      (enforce (= receiver-guard existing-guard) "Supplied receiver guard must match existing guard.")
      (write accounts receiver
        { 
          "balance": (+ receiver-balance amount),
          "guard": receiver-guard,
          "account": receiver
        }
      )
    )
  )

  (defpact transfer-create-to-crosschain:string (receiver:string receiver-guard:guard amount:decimal target-chain:string)
    (step
      (with-capability (TRANSFER_TO target-chain)
        (yield { "receiver": receiver, "receiver-guard": receiver-guard, "amount": amount } target-chain)
      )
    )

    (step
      (resume { "receiver" := receiver, "receiver-guard" := receiver-guard, "amount" := amount }
        (with-capability (INTERNAL)
          (transfer-create-to receiver receiver-guard amount)
        )
      )
    )
  )

  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce (!= sender receiver) "Sender cannot be the same as the receiver.")
    (enforce (!= sender "") "Sender cannot be empty.")
    (enforce (!= receiver "") "Receiver cannot be empty.")
    (enforce-unit amount)
    (enforce-guard (at 'guard (read accounts sender)))
    (enforce (> amount 0.0) "Transfer must be positive."))

  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    (let ((balance (- managed requested)))
      (enforce (>= balance 0.0) (format "TRANSFER exceeded for balance {}" [managed]))
      balance))

  (defun transfer:string (sender:string receiver:string amount:decimal)
    @model
      [ (property (= 0.0 (column-delta accounts "balance")))
        (property (> amount 0.0))
        (property (!= sender receiver))
      ]

    (with-capability (TRANSFER sender receiver amount)
      (with-read accounts sender { "balance" := sender-balance }
        (enforce (<= amount sender-balance) "Insufficient funds.")
        (update accounts sender { "balance": (- sender-balance amount) }))

      (with-read accounts receiver { "balance" := receiver-balance }
        (update accounts receiver { "balance": (+ receiver-balance amount) }))))

  (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    @model [ (property (= 0.0 (column-delta accounts "balance"))) ]

    (with-capability (TRANSFER sender receiver amount)
      (with-read accounts sender { "balance" := sender-balance }
        (enforce (<= amount sender-balance) "Insufficient funds.")
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
    (enforce (!= account "") "Account name cannot be empty.")
    (with-read accounts account { "balance" := balance }
      balance
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
    (enforce (!= account "") "Account name cannot be empty.")
    (enforce-guard guard)
    (insert accounts account { "account": account, "balance": 0.0, "guard": guard })
    "Account created!"
  )

  (defun rotate:string (account:string new-guard:guard)
    (enforce (!= account "") "Account name cannot be empty.")
    (with-read accounts account { "guard" := old-guard }
      (enforce-guard old-guard)
      (update accounts account { "guard": new-guard }))
  )

  (defcap TRANSFER_XCHAIN:bool
    ( sender:string
      receiver:string
      amount:decimal
      target-chain:string
    )

    @managed amount TRANSFER_XCHAIN-mgr
    (enforce-unit amount)
    (enforce (> amount 0.0) "Cross-chain transfers require a positive amount")
    (enforce (!= (at "chain-id" (chain-data)) target-chain) "Target chain cannot be current chain.")
    (enforce (!= "" target-chain) "Target chain cannot be empty.")
    (enforce-unit amount)
    (enforce (!= sender "") "Invalid sender")
    (enforce-guard (at 'guard (read accounts sender)))
  )

  (defun TRANSFER_XCHAIN-mgr:decimal
    ( managed:decimal
      requested:decimal
    )

    (enforce (>= managed requested)
      (format "TRANSFER_XCHAIN exceeded for balance {}" [managed]))
    0.0
  )

  (defschema transfer-crosschain-schema
    @doc "Schema for yielded (transfer-crosschain) arguments."
    receiver:string
    receiver-guard:guard
    amount:decimal
  )

  (defpact transfer-crosschain:string (sender:string receiver:string receiver-guard:guard target-chain:string amount:decimal)
    (step
      (with-capability (TRANSFER_XCHAIN sender receiver amount target-chain)
        (with-read accounts sender { "balance" := sender-balance }
          (enforce (<= amount sender-balance) "Insufficient funds.")
          (update accounts sender { "balance": (- sender-balance amount) }))

        (yield
          (let
            ((payload:object{transfer-crosschain-schema}
                { "receiver": receiver
                , "receiver-guard": receiver-guard
                , "amount": amount
                }))
            payload)
          target-chain)))

    (step
      (resume { "receiver" := receiver, "receiver-guard" := receiver-guard, "amount" := amount }
        (with-default-read accounts receiver
          { "balance": 0.0, "guard": receiver-guard }
          { "balance" := receiver-balance, "guard" := existing-guard }
          (enforce (= receiver-guard existing-guard) "Supplied receiver guard must match existing guard.")
          (write accounts receiver
            { "balance": (+ receiver-balance amount)
            , "guard": receiver-guard
            , "account": receiver
            })))))
)


