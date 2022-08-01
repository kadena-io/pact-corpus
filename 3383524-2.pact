(module kadena-wall 'kadena-wall-admin
  "Kadena kadena-wall is now open! leave your message."

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
    messages-table:{message-schema})

  (defschema accounts
    balance:decimal
    guard:guard)

  (deftable accounts-table:{accounts})

  ; --------------------------------------------------------------------------
  ; Capability
  (defcap PAY (user-id)
    "enforces row guard to allow debiting operations"
    (with-read accounts-table user-id { "guard":= guard }
      (enforce-guard guard)))


  ; --------------------------------------------------------------------------
  ; Utils

  (defconst WALL_VAULT:string "k:460d776c75c4a665acd070d3d3f1fc96a26c96837af8d5ce3436b75fe487975a" )

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

  (defun create-account (id amount keyset)
    (enforce (>= amount 0.0) "Initial amount must be >= 0.")
    (insert accounts-table id
      { "balance": amount,
        "guard": keyset
      }
    )
  )

  (defun write-message (id owner name message amount)
    "Write a new message"

    (enforce (>= amount 0.0) "Initial amount must be >= 0.")
    (enforce (> (length owner) 2) "minimum account length")
    (create-account owner amount owner)

    (with-capability (PAY owner)
      (transfer owner WALL_VAULT amount)
      (insert messages-table id {
          "timestamp": curr-time,
          "block": curr-block,
          "amount": amount,
          "name": name,
          "msg": message
        }
      )
    )
  )

  (defun read-message:object (id:string)
    "Read a single message"
    (+ {'id: id} (read messages-table id))
  )

  (defun all-messages ()
    "Read all messages"
    (map (read-message) (keys messages-table))
  )
)


