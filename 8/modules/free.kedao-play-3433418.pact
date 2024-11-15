(module kedao-play GOVERNANCE
  @doc "module for series of interesting play in kedao.org"

  (use kedao)
  
  ; -------------------------------------------------------
  ; Schemas and Tables

  (defschema red-packet-schema
    @doc "Red packet module               \
          \ key: rp_id - id of red packet \
          \ payee: uid of payee (creator) "

    payer:string
    total_amount:decimal
    remain_amount:decimal
    created_time:string
  )
  (deftable red-packets:{red-packet-schema})

  (defschema red-packet-record-schema
    @doc "Record red packet delivery  \
          \ key: {rp_id}_{payee}      \
          \ rp_id: id of red packet   \
          \ payee: uid of payee "

    rp_id:string
    payee:string
    amount:decimal
    created_time:string
  )
  (deftable red-packet-records:{red-packet-record-schema})


  ; -------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    @doc " Only support upgrading by admin."
    (enforce-guard (at 'guard (coin.details kedao.KEDAO_ADMIN_ACCOUNT)))
  )

  (defcap IS_KEDAO_BOT ()
    @doc " Enforce requester is kedao bot"
    (enforce-guard (at 'guard (coin.details kedao.KEDAO_BOT_ACCOUNT)))
  )

  (defcap PACK_RED_PACKET (rp_id:string payer:string total_amount:decimal created_time:string)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )

  (defcap UNPACK_RED_PACKET (rp_id:string payee:string amount:decimal created_time:string) 
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap RED_PACKETS_POOL_AUTH ()
    (compose-capability (IS_KEDAO_BOT))
  )


  ; -------------------------------------------------------
  ; Constant



  ; -------------------------------------------------------
  ; Utilities

  (defun require-red-packets-pool-auth ()
    (require-capability (RED_PACKETS_POOL_AUTH))
  )

  (defun create-red-packet-account (uid:string account:string)
    (with-capability (IS_KEDAO_BOT)
      (coin.create-account account (create-user-guard (require-red-packets-pool-auth)))
      (kedao.update-user-account uid {"red_packet": account})
    )
  )

  (defun pack-red-packet:string
    (
      rp_id:string
      payer:string
      total_amount:decimal
      created_time:string
    )
    @doc "Pack KDA into red packet"

    (with-capability (PACK_RED_PACKET rp_id payer total_amount created_time)
      (with-default-read red-packets rp_id
        { "total_amount" : -1.0 }
        { "total_amount" := default_amount }
        (enforce (< default_amount 0.0) "Red packet already existed")
        (insert red-packets rp_id
          {
            "payer" : payer,
            "total_amount" : total_amount,
            "remain_amount" : total_amount,
            "created_time" : created_time
          }
        )
      )
    )
  )

  (defun validate-unpack-duplication:string (rp_id:string payee:string)
    @doc "Make sure everyone only unpack same red packet once"
    (let
      ((
        payee_unpack_record
        (filter 
          (lambda (obj) (= payee (at 'payee obj)) ) 
          (get-red-packet-records rp_id)
        )
      ))
      (enforce (= (length payee_unpack_record) 0) "User has already unpacked this red packet")
    )
  )

  (defun unpack-red-packet:string
    (
      rp_id:string
      payee:string
      amount:decimal
      created_time:string
    )
    @doc "Unpack certain red packet"

    ; validate
    (validate-unpack-duplication rp_id payee)

    (with-capability (RED_PACKETS_POOL_AUTH)
      (with-capability (UNPACK_RED_PACKET rp_id payee amount created_time)
        (with-default-read red-packets rp_id
          { "total_amount" : -1.0,
            "remain_amount" : -1.0
          }
          { "total_amount" := total_amount,
            "remain_amount" := remain_amount
          }
          (enforce (> total_amount 0.0) "Red packet doesn't exist")
          (enforce (> remain_amount 0.0) "Red packet is fully unpacked")
          (enforce (>= remain_amount amount) "Red packet balance is not enough")
          (let*
            (
              (red_packet (read red-packets rp_id))
              (payer_uid (at 'payer red_packet))
              (payer (kedao.get-user payer_uid))
              (payer_account (at 'red_packet (at 'accounts payer)))
              (payee_uid payee)
              (payee (kedao.get-user payee_uid))
              (payee_account (at 'main (at 'accounts payee)))
              (record 
                { "rp_id": rp_id,
                  "payee": payee_uid,
                  "amount": amount,
                  "created_time": created_time
                }
              )
            )
            (install-capability (coin.TRANSFER payer_account payee_account amount))
            (coin.transfer payer_account payee_account amount)
            (update red-packets rp_id
              { "remain_amount": (- remain_amount amount) }
            )
            (insert red-packet-records (hash record)
              record
            )
          )
        )
      )
    )
  )

  (defun get-red-packet:object (rp_id:string)
    @doc "Query info by certain red packet id"
    (read red-packets rp_id)
  )

  (defun get-red-packet-records:[object] (rp_id:string)
    @doc "Query records by certain red packet id"
    (let*
      (
        (filter-id (lambda (key obj) (= rp_id (at 'rp_id obj))) )
        (consumer (lambda (key obj) obj))
      )
      (fold-db red-packet-records (filter-id) (consumer))
    )
  )

)


