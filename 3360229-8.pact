(module kedao GOVERNANCE
  @doc "module for basic things in kedao.org  \
        \ 1. user creation                    \
        \ 2. score calculation                \
        \ 3. reward payments "

  ; -------------------------------------------------------
  ; Schemas and Tables

  (defschema user-schema 
    @doc "Store user info and score \
          \ key: account in kadena network "

    telegram_user_id:string
    join_time:string
    score:decimal
  )
  (deftable users:{user-schema})

  (defschema score-record-schema
    @doc "Record score increasing \
          \ key: hash of record   \
          \ source: the platform user earned scores "

    account:string
    score_inc:decimal
    source:string
    comment:string
    created_time:string
  )
  (deftable score-records:{score-record-schema})

  (defschema reward-record-schema
    @doc "Record reward payments \
          \ key: hash of record "

    account:string
    amount:decimal
    begin_date:string
    end_date:string
  )
  (deftable reward-records:{reward-record-schema})


  ; -------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    @doc " Only support upgrading by admin."
    (enforce-guard (at 'guard (coin.details KEDAO_ADMIN_ACCOUNT)))
  )

  (defcap IS_KEDAO_BOT ()
    @doc " Enforce requester is kedao bot"
    (enforce-guard (at 'guard (coin.details KEDAO_BOT_ACCOUNT)))
  )

  (defcap CREATE_USER (account:string telegram_user_id:string join_time:string)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )

  (defcap REWARDED (account:string amount:decimal begin_date:string end_date:string) 
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap REWARDS_POOL_AUTH ()
    (compose-capability (REWARDED "" 0.0 "" ""))
  )

  (defcap ADD_SCORE (account:string score_inc:decimal source:string created_time:string) 
    @event
    (compose-capability (IS_KEDAO_BOT))
  )


  ; -------------------------------------------------------
  ; Constant

  (defconst KEDAO_ADMIN_ACCOUNT "k:eda0c886dccd1c036e3b1fa30ee3e1352e0c7af161b39773145d3bfbdace4239")
  (defconst KEDAO_BOT_ACCOUNT "k:eda09d458337883b5b1fa9c549ab9e81323549aad427215a938968f273d57de7")
  (defconst KEDAO_REWARDS_POOL "kedao_rewards_pool")

  (defconst TELEGRAM_DAILY_SCORE_LIMIT 20.0)
  (defconst WEEKLY_REWARDS 50.0)


  ; -------------------------------------------------------
  ; Utilities

  (defun require-rewarded ()
    (require-capability (REWARDS_POOL_AUTH))
  )

  (defun initialize:string ()
    @doc "Initialize module"
    (with-capability (IS_KEDAO_BOT)
      (coin.create-account KEDAO_REWARDS_POOL (create-user-guard (require-rewarded)))
    )
  )

  (defun create-user:string
    (
      account:string
      telegram_user_id:string
      join_time:string
    )
    @doc "Create user as DAO member, initial score is 0."

    (with-capability (CREATE_USER account telegram_user_id join_time)
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
    (with-capability (ADD_SCORE account score_inc source created_time)
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

  (defun pay-rewards:[string] (begin_date:string end_date:string)
    @doc "Calc and pay rewards every week"
    (enforce
      ( =
        (time (format "{}T00:00:00Z" [end_date]))
        (add-time (time (format "{}T00:00:00Z" [begin_date])) (days 6))
      )
      "begin_date and end_date did not match"
    )
    (enforce
      ( =
        (format-time "%u" (time (format "{}T00:00:00Z" [end_date])))
        "7"
      )
      "end_date must be sunday"
    )
    (let
      ((records_num (length (select reward-records (where 'end_date (= end_date))))
      ))
      (enforce
        (= 0 records_num)
        "rewards have been paid for last week"
      )
    )
    (let*
      (
        (consumer (lambda (key obj) (+ {"account": key} obj)))
        (all_users (fold-db users (constantly true) (consumer)))
        (total_score_sum (fold (+) 0.0 (map (at 'score ) all_users)))
        (weekly_score_records (get-score-records-by-date-range begin_date end_date))
        (weekly_score_sum (fold (+) 0.0 (map (at 'score_inc ) weekly_score_records)))
        (combine-user-score 
          ( lambda 
            (user) 
            (let* 
              (
                (has-same-account (lambda (obj1 obj2) (= (at 'account obj1) (at 'account obj2))))
              )
              (
                + 
                { "weekly_score_inc" : (fold (+) 0.0 (map (at 'score_inc ) (filter (has-same-account user) weekly_score_records))) } 
                user
              )
            )
          )
        )
        (weekly_users (map (combine-user-score) all_users))
        (rewards_for_total_part (* WEEKLY_REWARDS 0.382))
        (rewards_for_inc_part (* WEEKLY_REWARDS 0.618))
        (calc-rewards
          ( lambda
            (user)
            {
              "account" : (at 'account user),
              "amount" : (round (
                +
                (* (/ (at 'score user) total_score_sum) rewards_for_total_part)
                (* (/ (at 'weekly_score_inc user) weekly_score_sum) rewards_for_inc_part)
              ) 8)
            }
          )
        )
        (pay 
          ( lambda 
            (user) 
            (if
              (< 0.0 (at 'amount user))  ; amount must > 0
              (let
                ((record 
                  { 
                    "account" : (at 'account user), 
                    "amount" : (at 'amount user),
                    "begin_date" : begin_date,
                    "end_date" : end_date
                  }
                ))
                (with-capability (REWARDED (at 'account record) (at 'amount record) (at 'begin_date record) (at 'end_date record))
                  (install-capability (coin.TRANSFER KEDAO_REWARDS_POOL (at 'account user) (at 'amount user)))
                  (coin.transfer KEDAO_REWARDS_POOL (at 'account user) (at 'amount user))
                  (insert reward-records (hash record)
                    record
                  )
                )
              )
              "skip as amount <= 0"
            )
          )
        )
      )
      (with-capability (REWARDS_POOL_AUTH)
        (map (pay) (map (calc-rewards) weekly_users))
      )
    )
  )

  (defun get-user:object (account:string)
    @doc "Query info by certain user"
    (read users account)
  )
  (defun get-user-with-tg-uid:object (telegram_user_id:string)
    @doc "Query info by telegram uid"
    (let*
      (
        (filter-uid (lambda (key obj) (= (at 'telegram_user_id obj) telegram_user_id)))
        (consumer (lambda (key obj) (+ {"account": key} obj)))
        (matched (fold-db users (filter-uid) (consumer)))
      )
      (enforce (<= 1 (length matched)) "User doesn't exist")
      (at 0 matched)
    )
  )
  (defun get-users-with-tg-uids:[object] (uids:[string])
    @doc "Query user account list by telegram uid list"
    (let*
      (
        (filter-uid (lambda (key obj) (contains (at 'telegram_user_id obj) uids)))
        (consumer (lambda (key obj) (+ {"account": key} obj)))
      )
      (fold-db users (filter-uid) (consumer))
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

  (defun get-score-records:[object] (account:string)
    @doc "Query score records by certain user"
    (select score-records (where 'account (= account)))
  )
  (defun get-score-records-by-date:[object] (date:string)
    @doc "Query score records by certain date"
    (let*
      (
        (filter-date (lambda (key obj) (= date (take 10 (at 'created_time obj))) ) )
        (consumer (lambda (key obj) obj))
      )
      (fold-db score-records (filter-date) (consumer))
    )
  )
  (defun get-score-records-by-date-range:[object] (begin_date:string end_date:string)
    @doc "Query score records by certain date"
    (let*
      (
        (filter-date 
          ( 
            lambda (key obj) 
            ( and 
              (<= begin_date (take 10 (at 'created_time obj))) 
              (>= end_date (take 10 (at 'created_time obj))) 
            )
          )
        )
        (consumer (lambda (key obj) obj))
      )
      (fold-db score-records (filter-date) (consumer))
    )
  )

  (defun get-reward-records:[object] (account:string)
    @doc "Query reward records by certain user"
    (select reward-records (where 'account (= account)))
  )
  (defun get-reward-records-by-date-range:[object] (begin_date:string end_date:string)
    @doc "Query reward records by date range"
    (let*
      (
        (filter-date 
          ( 
            lambda (key obj) 
            ( and 
              (<= begin_date (at 'begin_date obj))
              (>= end_date (at 'end_date obj))
            )
          )
        )
        (consumer (lambda (key obj) obj))
      )
      (fold-db reward-records (filter-date) (consumer))
    )
  )

)


