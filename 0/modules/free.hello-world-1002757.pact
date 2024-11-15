(module hello-world GOVERNANCE
"A smart contract to greet the world."

(defcap GOVERNANCE ()
  (enforce-guard (keyset-ref-guard 'hello-keyset))
)

(defschema name-schema
  @doc "Name schema"
  @model [(invariant (!= name ""))]
  name:string
  bh:integer)

(deftable memories:{name-schema})

(defun here (n:string)
  "Designed for /send calls. Leave your trace on Kadena mainnet!"
  (enforce (!= n "") "Name cannot be empty")
  (write memories n
    {"name": n, "bh": (at "block-height" (chain-data))}
  )
  (format "{} was here." [n])
)

(defun lookup (key:string)
  (read memories key)
)

(defun get-all ()
  (map (read memories) (keys memories))
)

)
