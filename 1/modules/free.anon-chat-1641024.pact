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

(defun chat (n:string)
  "Designed for /send calls. Chat with the world!"
  (enforce (!= n "") "Message cannot be empty")
  (enforce (> (length n) 8) "Message is too short")
  (write history n
    {"message": n, "bh": (at "block-height" (chain-data))}
  )
)

(defun lookup (key:string)
  "Designed for /lookup calls. Search the history!"
  (read history key)
)

(defun query-all ()
  "Designed for /query-all calls. Get complete history"
  (map (read history) (keys history))
)

)

