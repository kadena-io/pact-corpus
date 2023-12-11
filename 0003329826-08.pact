(module kedao GOVERNANCE
  @doc "module for everything in kedao.org"

  ; -------------------------------------------------------
  ; Schemas and Tables

  (defschema user-schema 
    @doc ""

    telegram_user_id:string
    join_date:string
    score:decimal
  )

  (deftable users:{user-schema})


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
      join_date:string
    )
    @doc "Create user as DAO member, initial score is 0."

    (with-capability (IS_KEDAO_BOT)
      (with-default-read users account
        { "score" : -1.0 }
        { "score" := score }
        (if
          (= score -1.0)
          (insert users account
            {
              "telegram_user_id" : telegram_user_id,
              "join_date" : join_date,
              "score" : 0.0
            }
          )
          "User already exists"
        )
      )
    )
  )

  (defun add-score:string (account:string score_inc:decimal comment:string)
    @doc "Add score for certain user"
    (with-capability (IS_KEDAO_BOT)
      (with-read users account { "score" := score }
        (update users account
          { "score" : (+ score score_inc) }
        )
      )
    )
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


  ; -------------------------------------------------------
  ; Transaction Logs
  (defun users-txlog (tx-id:integer)
    (map (at 'key) (txlog users tx-id))
  )

)


