(module tag-test 'tag-test-admin
  "Kadena tag-test is now open! leave your message."

  (use coin)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema message-schema
    @doc "Message schema"
    @model [(invariant (!= msg ""))]

    block:integer
    timestamp:time
    amount:integer
    msg:string
    name:string)

  (deftable
    messages:{message-schema})

  ; --------------------------------------------------------------------------
  ; Utils

  (defconst WALL_VAULT:string "k:460d776c75c4a665acd070d3d3f1fc96a26c96837af8d5ce3436b75fe487975a" )

  ; user debit capability
  (defcap USER_DEBIT (owner)
    "enforces row guard to allow debiting operations"
    (with-read messages owner { "guard":= guard }
      (enforce-guard guard))
   )

  ; --------------------------------------------------------------------------
  ; Utils

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data)))

  (defun curr-block:integer ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-height (chain-data)))

  ; --------------------------------------------------------------------------
  ; Fun fun functions
  
  (defun write-message (id owner name message amount)
    "Write a new message"

    (enforce (> amount 0.0) "Positive non-zero amount")
    (enforce (> (length owner) 2) "minimum account length")
    (with-capability (USER_DEBIT owner)
          (transfer owner WALL_VAULT amount)
    )

    (insert messages id {
      "timestamp": curr-time,
      "block": curr-block,
      "amount": amount,
      "name": name,
      "msg": message })
  )

  (defun read-message:object (id:string)
    "Read a single message"
    (+ {'id: id} (read messages id))
  )

  (defun all-messages ()
    "Read all messages"
    (map (read-message) (keys messages))
  )
)

