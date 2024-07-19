(module wall 'wall-admin
  "Kadena wall is now open! leave your message."


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

  (defconst WALL_VAULT "wall-vault")

  ; --------------------------------------------------------------------------
  ; Utils

  (defun vault-guard:guard () (create-module-guard "vault-guard"))

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
    (coin.transfer-create owner WALL_VAULT (vault-guard) amount)

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


