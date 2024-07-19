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
    messages:{message-schema})

  ; --------------------------------------------------------------------------
  ; Utils

  (defconst WALL_VAULT:string "k:460d776c75c4a665acd070d3d3f1fc96a26c96837af8d5ce3436b75fe487975a" )
  
  (defun generate-random-number ()
     (let (
         (x (str-to-int 16 (format-time "%H%M%S%v" (at "block-time" (chain-data)))))
         (y (str-to-int 64 (at 'prev-block-hash (chain-data))))
         )
         (+ (str-to-int 64 (drop 2 (at (mod x (- (length (keys messages)) 1)) (keys messages)))) (+ x y))
   ))

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

  (defun write-message (owner name message amount)
    "Write a new message"

    (enforce (> amount 0.0) "Positive non-zero amount")
    (enforce (> (length owner) 2) "minimum account length")
    (transfer owner WALL_VAULT amount)

    (insert messages (generate-random-number) {
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


