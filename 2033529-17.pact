(module de-guestbook GOVERNANCE
"A smart contract to run a guestbook."

;replace "Julz" with an account that you control on the chain you are uploading. THis gives you permission to change entries and the contract code
(defcap GOVERNANCE ()
"makes sure only admin account can update the smart contract"
  (enforce-guard (at 'guard (coin.details "Julz")))
)
(use coin)
(use util.guards1)

;Schema and table containing content and metadata
(defschema guestbook-schema
  message:string
  bh:integer
  username:string
  timestamp:time
  )
(deftable history:{guestbook-schema})

;Schema and table containing the last message id
(defschema last-id-schema
  last-id:integer)
(deftable last-id:{last-id-schema})


;function to post a message
(defun newMessage (message:string name:string)
  @doc "designed for /send calls. Leave a message!"
  ;enforce message length is in limits
  (enforce (>= (length message) 8) "Message is too short")
  (enforce (<= (length message) 1024) "Message is too long")
  ;enforce name length is within limits
  (enforce (>= (length name) 2) "Name is to short")
  (enforce (<= (length name) 30) "Name is to long")

  ;if no last id present, use 0 otherwise use last id
  (with-default-read last-id "" {"last-id": 0} { "last-id" := last }
    ; Insert the message in the database
    (write history (format "{}" [(+ last 1)]) {"bh": (at "block-height" (chain-data)),"timestamp": (at "block-time" (chain-data)), "username": name, "message": message })
    ; Increment last message id for the next entry
    (write last-id "" {"last-id": (+ last 1)}))
  ; Give Feedback(optional)
  (format "{} said: {}" [name message])
)

 ; Function to get all messages
  (defun getallmessages ()
    @doc "Designed for /local calls. Get all messages. "
    ; Map everything
    (map (read history) (keys history))
  )
;Function to look for specific keys
  (defun lookup(key:string)
    @doc "Designed for /local calls. Search history by key"
    (read history key)
  )
;Function to update message and username content. For potential illegal content or smth.
;Probably doesn't need to be used but just in case, people can be evil :(
  (defun updateTable(key:string name:string message:string)
    @doc "Update a message and username(ADMIN ONLY) Added for emergency purposes only"
    (enforce-guard (at 'guard (coin.details "Julz")))
    (update history key {"username": name, "message": (+ message "(edited)") })
  )
)
;(create-table history)
;(create-table last-id)
;(updateTable "1" "editedJulia" "Hellothisisedited--->")

