(module blockchain-chat GOVERNANCE
  @doc " Send permanent messages to the blockchain for all to see. "
  
 (defcap GOVERNANCE ()
"Only admin can update this module"
(enforce-keyset 'bridge-tester))

(defschema chat-schema
  @doc " Chat table schema "
  @model [(invariant (!= message ""))]
  message:string
  bh:integer)

(deftable history:{chat-schema})

(defun chat (message:string) 
"Send your message to all Kadenians."
(enforce (!= message "") "Message cannot be empty")
(enforce (> (length message) 4) "Message is too short")
(enforce (< (length message) 2048) "Message is too long")
(write history message
  {"message": message, "bh": (at "block-height" (chain-data))}
)
(format "A fellow Kadenian said: {}" [message])
)
(defun message-history (key:string)
"Call this function to see all messages from prior Kadenians."
(map (read history) (keys history))
)
(defun recent-messages (key:string)
"Call this function to get all recent messages from prior Kadenians."
  (map (read history) (keys history))
  (select history ['message,'bh] (where 'bh (>= (- (at 'block-height (chain-data)) 5000))))
    )
)
;-------------------------------------------------------------------------------------------
; Create Table
;-------------------------------------------------------------------------------------------

