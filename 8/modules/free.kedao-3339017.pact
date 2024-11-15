(module kedao GOVERNANCE
  @doc "module for everything in kedao.org"

  ; -------------------------------------------------------
  ; Schemas and Tables

  (defschema user-schema 
    @doc "Store user info and score"

    telegram_user_id:string
    join_time:string
    score:decimal
  )

  (deftable users:{user-schema})

  (defschema score-record-schema
    @doc "Record score increasing \
          \ source: the platform user earned scores"

    account:string
    score_inc:decimal
    source:string
    comment:string
    created_time:string
  )

  (deftable score-records:{score-record-schema})


  ; -------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    @doc " Only support upgrading by admin."
    (enforce-guard (at 'guard (coin.details KEDAO_ADMIN_ACCOUNT)))
  )

  (defcap IS_KEDAO_BOT:bool ()
    @doc " Enforce requester is kedao bot"
    (enforce-guard (at 'guard (coin.details KEDAO_BOT_ACCOUNT)))
  )


  ; -------------------------------------------------------
  ; Constant

  (defconst KEDAO_ADMIN_ACCOUNT "k:eda0c886dccd1c036e3b1fa30ee3e1352e0c7af161b39773145d3bfbdace4239")
  (defconst KEDAO_BOT_ACCOUNT "k:eda09d458337883b5b1fa9c549ab9e81323549aad427215a938968f273d57de7")
  (defconst SCORE_INC_LIMIT 20.0)


  ; -------------------------------------------------------
  ; Utilities

  (defun create-user:string
    (
      account:string
      telegram_user_id:string
      join_time:string
    )
    @doc "Create user as DAO member, initial score is 0."

    (with-capability (IS_KEDAO_BOT)
      (with-default-read users account
        { "score" : -1.0 }
        { "score" := score }
        (enforce (< score 0.0) "User already existed")
        (insert users account
          {
            "telegram_user_id" : telegram_user_id,
            "join_time" : join_time,
            "score" : 0.0
          }
        )
      )
    )
  )

  (defun add-score:string 
    ( account:string 
      score_inc:decimal 
      source:string 
      comment:string 
      created_time:string
    )
    @doc "Add score for certain user"
    (with-capability (IS_KEDAO_BOT)
      (with-read users account { "score" := score }
        ; insert score record
        (let
          ((record 
            { 
              "account" : account,
              "score_inc" : score_inc,
              "source" : source,
              "comment" : comment,
              "created_time" : created_time
            }
          ))
          (insert score-records (hash record)
            record
          )
        )
        ; update user score
        (update users account
          { "score" : (+ score score_inc) }
        )
      )
    )
  )

  (defun add-score-with-object:string (record:object)
    @doc "Add score in object format"
    (add-score
      (at 'account record)
      (at 'score_inc record)
      (at 'source record)
      (at 'comment record)
      (at 'created_time record)
    )
  )
  (defun add-scores-in-bulk:[string] (records:[object])
    @doc "Add scores in batch"
    (map (add-score-with-object) records)
  )

  (defun get-user-with-tg-uid:object (telegram_user_id:string)
    @doc "Query info by certain user"
    (at 0 (select users (where 'telegram_user_id (= telegram_user_id))))
  )
  (defun get-user:object (account:string)
    @doc "Query info by certain user"
    (read users account)
  )

  (defun get-score:decimal (account:string)
    @doc "Query score by certain user"
    (with-read users account { "score" := score }
      score
    )
  )

  (defun get-total-score:decimal ()
    @doc "Query overall score"
    (fold (+) 0.0 (map (at "score") (select users (constantly true))))
  )

  (defun get-score-records:[object] (account:string)
    @doc "Query score records by certain user"
    (select score-records (where 'account (= account)))
  )


  ; -------------------------------------------------------
  ; Transaction Logs
  (defun users-txlog (tx-id:integer)
    (map (at 'key) (txlog users tx-id))
  )

)


