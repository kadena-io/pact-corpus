(module kedao-play GOVERNANCE
  @doc "module for series of interesting play in kedao.org"

  (use kedao)
  
  ; -------------------------------------------------------
  ; Schemas and Tables

  (defschema red-packet-schema
    @doc "Red packet module               \
          \ key: rp_id - id of red packet \
          \ payer: uid of payer (creator) \
          \ status: open / expired "

    payer:string
    total_amount:decimal
    remain_amount:decimal
    created_time:string
    refunded_time:string
    status:string
  )
  (deftable red-packets:{red-packet-schema})

  (defschema red-packet-record-schema
    @doc "Record red packet delivery  \
          \ key: {rp_id}_{payee}      \
          \ rp_id: id of red packet   \
          \ payee: uid of payee (receiver) "

    rp_id:string
    payee:string
    amount:decimal
    created_time:string
  )
  (deftable red-packet-records:{red-packet-record-schema})

  (defschema task-schema
    @doc "Task module             \
          \ key: task_id          \
          \ payer: uid of payer (creator) "

    payer:string
    title:string
    content:string
    total_amount:decimal
    total_places:decimal
    remain_places:decimal
    created_time:string
  )
  (deftable tasks:{task-schema})

  (defschema task-record-schema
    @doc "Record task finishing and rewarding \
          \ key: {task_id}_{payee}            \
          \ payee: uid of payee (receiver) "

    task_id:string
    payee:string
    amount:decimal
    created_time:string
  )
  (deftable task-records:{task-record-schema})


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
  (defcap UPDATE_RED_PACKET_AMOUNT (rp_id:string remain_amount:decimal)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap REFUND_RED_PACKET (rp_id:string amount:decimal refunded_time:string)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap RED_PACKETS_POOL_AUTH ()
    (compose-capability (IS_KEDAO_BOT))
  )

  (defcap CREATE_TASK (task_id:string payer:string title:string total_amount:decimal total_places:decimal created_time:string)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap FINISH_TASK (task_id:string payee:string created_time:string)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap UPDATE_TASK_PLACES (task_id:string remain_places:decimal) 
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap TASKS_POOL_AUTH ()
    (compose-capability (IS_KEDAO_BOT))
  )

  ; -------------------------------------------------------
  ; Constant

  (defconst RED_PACKET_OPEN_STATUS "open")
  (defconst RED_PACKET_EXPIRED_STATUS "expired")

  ; -------------------------------------------------------
  ; Utilities

  ; ----------- RED PACKET ------------

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
    @doc "Create red packet"

    ; validate amount
    (enforce (< 0.0 total_amount) "Amount must larger than 0")
    (coin.enforce-unit total_amount)

    (with-capability (PACK_RED_PACKET rp_id payer total_amount created_time)
      (insert red-packets rp_id {
        "payer" : payer,
        "total_amount" : total_amount,
        "remain_amount" : total_amount,
        "created_time" : created_time,
        "refunded_time" : "",
        "status": RED_PACKET_OPEN_STATUS
      })
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

    ; validate duplication
    (let
      ((record (try {"payee":""} (read red-packet-records (format "{}_{}" [rp_id payee])))))
      (enforce (= "" (at 'payee record)) "User has already unpacked this red packet")
    )

    (let*
      (
        (red_packet (read red-packets rp_id))
        (remain_amount (at 'remain_amount red_packet))
        (updated_remain_amount (- remain_amount amount))
        (payer_uid (at 'payer red_packet))
        (payer (kedao.get-user payer_uid))
        (payer_account (at 'red_packet (at 'accounts payer)))
        (payee_uid payee)
        (payee (kedao.get-user payee_uid))
        (payee_account (at 'main (at 'accounts payee)))
        (record_key (format "{}_{}" [rp_id, payee_uid]))
      )
      ; validate amount
      (enforce (< 0.0 remain_amount) "There are no amount left")
      (enforce (>= remain_amount amount) "Red packet balance is not enough")

      ; validate status
      (enforce (= RED_PACKET_OPEN_STATUS (at 'status red_packet)) "Red packet status is not open")

      (with-capability (RED_PACKETS_POOL_AUTH)
        (install-capability (coin.TRANSFER payer_account payee_account amount))
        (coin.transfer payer_account payee_account amount)
      )
      (with-capability (UNPACK_RED_PACKET rp_id payee_uid amount created_time)
        (insert red-packet-records record_key {
          "rp_id": rp_id,
          "payee": payee_uid,
          "amount": amount,
          "created_time": created_time
        })
      )
      (with-capability (UPDATE_RED_PACKET_AMOUNT rp_id updated_remain_amount)
        (update red-packets rp_id { "remain_amount": updated_remain_amount })
      )
    )
  )

  (defun refund-red-packet:string (rp_id:string refunded_time:string)
    @doc "Refund the remain amount after 24 hours"

    ; time validation
    (let*
      (
        (red_packet (read red-packets rp_id))
        (created_time (time (format "{}Z" [(at 'created_time red_packet)])))
        (expired_time (add-time created_time (days 1)))
        (current_time (time (format "{}Z" [refunded_time])))
      )
      (enforce (> current_time expired_time) "Cannot refund within 24 hours")
    )

    ; execute refund
    (let*
      (
        (red_packet (read red-packets rp_id))
        (payer_uid (at 'payer red_packet))
        (payer (kedao.get-user payer_uid))
        (payer_account (at 'red_packet (at 'accounts payer)))
        (refund_account (at 'main (at 'accounts payer)))
        (amount (at 'remain_amount red_packet))
      )
      ; validate status
      (enforce (!= RED_PACKET_EXPIRED_STATUS (at 'status red_packet)) "Red packet has been expired")

      (with-capability (RED_PACKETS_POOL_AUTH)
        (install-capability (coin.TRANSFER payer_account refund_account amount))
        (coin.transfer payer_account refund_account amount)
      )
      (with-capability (REFUND_RED_PACKET rp_id amount refunded_time)
        (update red-packets rp_id {
          "refunded_time": refunded_time,
          "status": RED_PACKET_EXPIRED_STATUS
        })
      )
    )
  )

  (defun get-red-packet:object (rp_id:string)
    @doc "Query info by certain red packet id"
    (read red-packets rp_id)
  )
  (defun get-all-red-packets:[object] ()
    @doc "Query info by certain red packet id"
    (let ((consumer (lambda (key obj) (+ { "key": key } obj))))
      (fold-db red-packets (constantly true) (consumer))
    )
  )
  (defun get-valid-red-packets:[object] ()
    @doc "Query red packets that are not expired and still have remain amount"
    (let 
      (
        (filter-valid (lambda (key obj) (and (= RED_PACKET_OPEN_STATUS (at 'status obj)) (< 0.0 (at 'remain_amount obj)))))
        (consumer (lambda (key obj) (+ { "key": key } obj)))
      )
      (fold-db red-packets (filter-valid) (consumer))
    )
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

  ; ----------- TASK ------------

  (defun require-tasks-pool-auth ()
    (require-capability (TASKS_POOL_AUTH))
  )

  (defun create-task-account (uid:string account:string)
    (with-capability (IS_KEDAO_BOT)
      (coin.create-account account (create-user-guard (require-tasks-pool-auth)))
      (kedao.update-user-account uid {"task": account})
    )
  )

  (defun create-task 
    ( task_id:string
      payer:string
      title:string
      content:string
      total_amount:decimal
      total_places:decimal
      created_time:string
    )
    @doc "Create task by payer "

    ; validate amount and places
    (enforce (< 0.0 total_amount) "Amount must larger than 0")
    (enforce (< 0.0 total_places) "Places must larger than 0")
    (coin.enforce-unit total_amount)
    (enforce (= total_places (* 1.0 (floor total_places))) "Places must be integer")
    (let
      ((avg_amount (/ total_amount total_places)))
      (enforce (= avg_amount (floor avg_amount 4)) "Avg must be round")
    )

    (with-capability (CREATE_TASK task_id payer title total_amount total_places created_time)
      (insert tasks task_id {
        "payer": payer,
        "title": title,
        "content": content,
        "total_amount": total_amount,
        "total_places": total_places,
        "remain_places": total_places,
        "created_time": created_time
      })
    )
  )

  (defun finish-task
    ( task_id:string
      payee:string
      created_time:string
    )
    @doc "Finish task by payee "

    ; validate duplication
    (let
      ((record (try {"payee":""} (read task-records (format "{}_{}" [task_id payee])))))
      (enforce (= "" (at 'payee record)) "User has already finished this task")
    )

    (let*
      (
        (task (read tasks task_id))
        (remain_places (at 'remain_places task))
        (updated_remain_places (- remain_places 1))
        (avg_amount (/ (at 'total_amount task) (at 'total_places task)))
        (amount avg_amount)
        (payer_uid (at 'payer task))
        (payer (kedao.get-user payer_uid))
        (payer_account (at 'task (at 'accounts payer)))
        (payee_uid payee)
        (payee (kedao.get-user payee_uid))
        (payee_account (at 'main (at 'accounts payee)))
        (record_key (format "{}_{}" [task_id payee_uid]))
      )
      ; validate places
      (enforce (< 0.0 remain_places) "There are no places left")
      
      (with-capability (TASKS_POOL_AUTH)
        (install-capability (coin.TRANSFER payer_account payee_account amount))
        (coin.transfer payer_account payee_account amount)
      )
      (with-capability (FINISH_TASK task_id payee_uid created_time)
        (insert task-records record_key {
          "task_id": task_id,
          "payee": payee_uid,
          "amount": amount,
          "created_time": created_time
        })
      )
      (with-capability (UPDATE_TASK_PLACES task_id updated_remain_places)
        (update tasks task_id { "remain_places": updated_remain_places })
      )
    )
  )

  (defun get-task:object (task_id:string)
    @doc "Query info by certain task id"
    (read tasks task_id)
  )

  (defun get-task-records:[object] (task_id:string)
    @doc "Query records by certain task id"
    (let*
      (
        (filter-id (lambda (key obj) (= task_id (at 'task_id obj))) )
        (consumer (lambda (key obj) obj))
      )
      (fold-db task-records (filter-id) (consumer))
    )
  )


  (defun update-all-red-packets:[string] ()
    (with-capability (GOVERNANCE)
      (map 
        (lambda (obj) 
          (if (= {} (take ['status ] obj))
            (update red-packets (at 'key obj) {"status":RED_PACKET_OPEN_STATUS, "refunded_time":""})
            "skip"
          )
        )
        (get-all-red-packets)
      )
    )
  )

)


