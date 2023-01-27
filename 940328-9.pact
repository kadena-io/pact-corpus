(module memory-wall GOVERNANCE
"A smart contract to greet the world."

(defcap GOVERNANCE ()
  (enforce-guard (keyset-ref-guard 'hello-keyset))
)

(defschema memory-schema
  @doc "Schema to store name and blockheight"
  @model [(invariant (!= name ""))]
  name:string
  block-height:integer)

(deftable memories:{memory-schema})

(defun here (name:string)
  "Designed for /send calls. Leave your trace on Kadena mainnet!"
  (enforce (!= name "") "Name cannot be empty")
  (write memories name
    { "name"         : name,
      "block-height" : (at "block-height" (chain-data)) }
  )
  (format "{} was here." [name])
)

(defun lookup (key:string)
  (read memories key)
)

(defun get-all ()
  (map (read memories) (keys memories))
)

)

