(module anon-chat GOVERNANCE
"A smart contract to meet the world."

(defcap GOVERNANCE ()
  (enforce-guard (keyset-ref-guard 'chat-keyset))
)

(defschema chat-schema
  @doc "Chat schema"
  @model [(invariant (!= message ""))]
  message:string
  bh:integer)

(deftable history:{chat-schema})

(defun chat (message:string)
  "Designed for /send calls. Chat with the world!"
  (enforce (!= message "") "Message cannot be empty")
  (enforce (> (length message) 8) "Message is too short")
  (write history message
    {"message": message, "bh": (at "block-height" (chain-data))}
  )
)

(defun lookup (key:string)
  "Designed for /local calls. Search the history!"
  (read history key)
)

(defun query-all ()
  "Designed for /local calls. Get complete history"
  (map (read history) (keys history))
)

)

