(module mailbox GOVERNANCE

   (implements mailbox-iface)

   ;; Imports
   (use hyperlane-message)

   (use mailbox-state-iface)

   ;; Tables
   (deftable contract-state:{mailbox-state})
   (deftable dependencies:{dependency})
   (deftable deliveries:{delivery})
   (deftable hashes:{router-hash})
   
   ;; Capabilities
   (defcap GOVERNANCE () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.upgrade-admin"))
   
   (defcap ONLY_ADMIN () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.bridge-admin"))

   (defcap ONLY_MAILBOX:bool () true)

   (defcap PROCESS-MLC (message-id:string message:object{hyperlane-message} signers:[string] threshold:integer)
      (enforce-verifier "hyperlane_v3_message")
      (enforce (= 3 (at "version" message)) "Invalid hyperlane version")
      (enforce (= message-id (hyperlane-message-id message)) "Invalid calculated messageId")
      (enforce (= LOCAL_DOMAIN (at "destinationDomain" message)) "Invalid destinationDomain")
   )

   ;; Constants
   (defconst LOCAL_DOMAIN 626)

   (defconst VERSION 3)

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

   (defcap DISPATCH
      (
         version:integer
         nonce:integer
         sender:string
         destination:integer
         recipient:string
         message-body:string
      )
      @doc "Emitted when a new message is dispatched via Hyperlane"
      @event true
   )

   (defcap DISPATCH-ID
      (
         message-id:string
      )
      @doc "Emitted when a new message is dispatched via Hyperlane"
      @event true
   )

   (defcap PROCESS
      (
      origin:integer
      sender:string
      recipient:string
      )
      @doc "Emitted when a Hyperlane message is delivered"
      @event true
   )

   (defcap PROCESS-ID
      (
         message-id:string
      )
      @doc "Emitted when a Hyperlane message is processed"
      @event true
   )

   (defun initialize ()
      (with-capability (ONLY_ADMIN)
         (insert contract-state "default"
            {
               "paused": false,
               "nonce": 0,
               "latest-dispatched-id": "0"
            }
         )
      )
   )

   (defun pause ()
      (with-capability (ONLY_ADMIN)
         (with-read contract-state "default"
            {"paused" := paused}
            (enforce (not paused) "Already paused")
         )
         (update contract-state "default"
               {"paused": true})
      )
   )

   (defun unpause ()
      (with-capability (ONLY_ADMIN)
         (with-read contract-state "default"
            {"paused" := paused}
            (enforce paused "Already unpaused")
         )
         (update contract-state "default"
               {"paused": false})
      )
   )

   (defun paused:bool ()
      (with-read contract-state "default"
         {
            "paused" := paused
         }
         paused
      )
   )

   (defun delivered:bool (id:string)
      (with-default-read deliveries id
         {
            "block-number": 0
         }
         {
            "block-number" := block-number
         }
         (> block-number 0)
      )
   )

   (defun nonce:integer ()
      (with-read contract-state "default"
         {
            "nonce" := nonce
         }
         nonce
      )
   )

   (defun recipient-ism ()
      domain-routing-ism
   )

   (defun define-hook (hook:module{hook-iface})
      (insert dependencies "default"
         {
            "hook": hook
         }
      )
   )

   (defun store-router (router:module{router-iface})
      (insert hashes (get-router-hash router) 
         {
            "router-ref": router
         }
      )
   )

   (defun get-router-hash (router:module{router-iface})
      (base64-encode (take 32 (hash router)))
   )

   (defun quote-dispatch:decimal (destination:integer)
      @doc "Computes payment for dispatching a message to the destination domain & recipient." ;; todo: extract docs to iface
      (igp.quote-gas-payment destination)
   )

   (defun dispatch:string (router:module{router-iface} destination:integer recipient-tm:string amount:decimal)
      @doc "Dispatches a message to the destination domain & recipient." ;; todo: extract docs to iface
      (let*
         (
            (sender:string  (get-router-hash router))
            (recipient:string (router::transfer-remote destination (at "sender" (chain-data)) recipient-tm amount))
            (remote-amount:decimal (router::get-adjusted-amount amount))
            (message-body:string (hyperlane-encode-token-message {"amount": remote-amount, "recipient": recipient-tm, "chainId": "0"}))
            (message:object{hyperlane-message} (prepare-dispatch-parameters sender destination recipient message-body))
            (id:string (hyperlane-message-id message))
         )
         (with-read contract-state "default"
            {
               "nonce" := old-nonce
            }
            (update contract-state "default"
               {
                  "latest-dispatched-id": id,
                  "nonce": (+ old-nonce 1)
               }
            )
            (igp.pay-for-gas id destination (quote-dispatch destination))
            (with-capability (ONLY_MAILBOX)
               (with-read dependencies "default"
                  {
                     "hook" := hook:module{hook-iface}
                  }
               (hook::post-dispatch id message)
               )
            )
            (emit-event (DISPATCH 3 old-nonce sender destination recipient message-body))
         )
         (emit-event (DISPATCH-ID id))
         id
      )
   )

   (defun prepare-dispatch-parameters (sender:string destination-domain:integer recipient:string message-body:string)
      (with-read contract-state "default"
         {
            "nonce" := nonce
         }
         {
            "version": VERSION,
            "nonce": nonce,
            "originDomain": LOCAL_DOMAIN,
            "sender": sender, 
            "destinationDomain": destination-domain,
            "recipient": recipient,
            "messageBody": message-body
         }
      )    
   )

   ;;TODO: extract 
   (defschema decoded-token-message
      recipient:keyset
      amount:decimal
      chainId:integer
   )

   (defun decode-token-message:object{decoded-token-message} (message:string)
      (bind (hyperlane-decode-token-message message)
         {
            "recipient" := recipient,
            "amount" := amount,
            "chainId" := chainId
         }
         {
            "recipient": recipient,
            "amount": (* amount 1.0),
            "chainId": chainId
         }
      )
   )


   (defun process (message-id:string message:object{hyperlane-message})
      @doc "Attempts to deliver HyperlaneMessage to its recipient."
      (with-read contract-state "default"
         {"paused" := paused}
         (enforce (not paused) "Bridge is paused.")   
      )
      (with-capability (PROCESS-MLC message-id message (domain-routing-ism.get-validators message) (domain-routing-ism.get-threshold message))
         (let
            (
               (origin:integer (at "originDomain" message))
               (sender:string (at "sender" message))
               (recipient-router:string (at "recipient" message)) 
               (id:string (hyperlane-message-id message))
            )
            (with-default-read deliveries id
               {
                  "block-number": 0
               }                  {
                  "block-number" := block-number
               }
               (enforce (= block-number 0) "Message has been submitted")   
            )
            (insert deliveries id
               {
                  "block-number": (at "block-height" (chain-data))
               }   
            )
            (bind (hyperlane-decode-token-message (at "messageBody" message))
               {
                  "chainId" := chainId,
                  "recipient" := recipient-guard,
                  "amount" := amount
               }
               (let
                  (
                     (chain:integer (str-to-int chainId))
                     (recipient:string (create-principal recipient-guard))
                  )
                  (enforce (and (<= chain 19) (>= chain 0)) "Invalid chain ID")
                  (with-read hashes recipient-router
                     {
                        "router-ref" := router:module{router-iface} 
                     }
                     (with-capability (ONLY_MAILBOX)
                        (router::handle origin sender (str-to-int chainId) recipient recipient-guard amount)
                     )
                  )
                  (emit-event (PROCESS origin sender recipient))
                  (emit-event (PROCESS-ID id)) 
               )
            )
         )
      )
   )
)


