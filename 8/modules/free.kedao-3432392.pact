(module kedao GOVERNANCE
  @doc "module for basic things in kedao.org  \
        \ 1. user creation                    \
        \ 2. score calculation                \
        \ 3. reward payments "

  ; -------------------------------------------------------
  ; Schemas and Tables

  (defschema user-schema 
    @doc "Store user info and score \
          \ key: uid, the member id in kedao, auto-increment, format: KD_001             \
          \ accounts: user's accounts in Kadena blockchain, common keys are ['main', 'red_packet', ...]  \
          \ social_ids: user's social media id info, common keys are ['telegram', 'twitter', ...]  \
          \ title: the title for active member "

    accounts:object
    social_ids:object
    join_time:string
    score:decimal
    level:decimal
    title:string
  )
  (deftable users:{user-schema})

  (defschema score-record-schema
    @doc "Record score increasing, once a day per user   \
          \ key: {uid}_{source}_{date}                         \
          \ source: the platform user earned scores "

    uid:string
    source:string
    date:string
    score_inc:decimal
    comment:string
  )
  (deftable score-records:{score-record-schema})

  (defschema reward-record-schema
    @doc "Record reward payments \
          \ key: {uid}_{begin_date}_{end_date} "

    uid:string
    begin_date:string
    end_date:string
    amount:decimal
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

  (defcap CREATE_USER (uid:string account:string social_ids:object join_time:string)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap UPDATE_USER_ACCOUNT (uid:string key:string value:string)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap UPDATE_USER_SOCIAL_ID (uid:string key:string value:string)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )

  (defcap REWARDED (account:string amount:decimal begin_date:string end_date:string) 
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap REWARDS_POOL_AUTH ()
    (compose-capability (IS_KEDAO_BOT))
  )

  (defcap ADD_SCORE (uid:string source:string date:string score_inc:decimal)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap UPDATE_SCORE (uid:string score:decimal)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )


  ; -------------------------------------------------------
  ; Constant

  (defconst KEDAO_ADMIN_ACCOUNT "k:eda0c886dccd1c036e3b1fa30ee3e1352e0c7af161b39773145d3bfbdace4239")
  (defconst KEDAO_BOT_ACCOUNT "k:eda09d458337883b5b1fa9c549ab9e81323549aad427215a938968f273d57de7")
  (defconst KEDAO_REWARDS_POOL "kedao_rewards_pool")

  (defconst CREATE_ACCOUNT_KDA 0.0001)

  (defconst WEEKLY_REWARDS 50.0)

  (defconst UID_PREFIX "KD")
  (defconst SCORE_SOURCE_TYPES ["telegram", "twitter"])
  (defconst SCORE_DAILY_LIMIT {
    "telegram": 20.0,
    "twitter": 10.0
  })


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
      guard:guard
      social_ids:object 
      join_time:string
    )
    @doc "Create user as DAO member, initial score is 0."

    ; social ids validation
    (enforce (contains "telegram" social_ids) "Telegram uid is needed")

    (let*
      (
        (all-users (get-all-users))
        (member_cnt (length all-users))
        (rank (+ 1 member_cnt))
        (rank_str
          (if (< rank 10) (format "00{}" [rank])
            (if (< rank 100)
              (format "0{}" [rank])
              (format "{}" [rank])
            )
          )
        )
        (uid (format "{}_{}" [UID_PREFIX rank_str]))
        (matched (filter (lambda (user) 
          (or 
            (= (at 'key user) uid)
            (or
              (= (at 'main (at 'accounts user)) account)
              (= (at 'telegram (at 'social_ids user)) (at 'telegram social_ids))
            )
          )
        ) all-users))
      )
      ; duplication validation
      (enforce (= 0 (length matched)) "User already existed, same uid/account/telegram_uid")

      (with-capability (CREATE_USER uid account social_ids join_time)
        (coin.enforce-reserved account guard)  ; Make sure k:account
        (coin.transfer-create KEDAO_BOT_ACCOUNT account guard CREATE_ACCOUNT_KDA)
        (insert users uid {
            "accounts" : {"main": account}, 
            "social_ids" : social_ids,
            "join_time" : join_time,
            "score" : 0.0,
            "level" : 0.0,
            "title" : ""
        })
      )
    )
  )

  (defun add-score:string 
    ( uid:string 
      source:string 
      date:string
      score_inc:decimal 
      comment:string 
    )
    @doc "Add score for certain user"
    ; validate source
    (enforce (contains source SCORE_SOURCE_TYPES) "source invalid")
    ; validate score
    (enforce (<= score_inc (at source SCORE_DAILY_LIMIT)) "score exceeded")

    (with-capability (ADD_SCORE uid source date score_inc)
      ; insert score record
      (let
        (
          (record 
            { 
              "uid" : uid, 
              "source" : source,
              "date" : date,
              "score_inc" : score_inc,
              "comment" : comment
            }
          )
          (key (format "{}_{}_{}" [uid, source, date]))
        )
        (with-default-read score-records key 
          { "uid" : ""  }
          { "uid" := default_uid }
          (enforce (= default_uid "") "record already existed")
          (insert score-records key
            record
          )
        )
        (with-read users uid { "score" := score }
          ; update user score
          (let ((new_score (+ score score_inc)))
            (with-capability (UPDATE_SCORE uid new_score)
              (update users uid
                { "score" : new_score }
              )
            )
          )
        )
      )
    )
  )

  (defun add-score-with-object:string (record:object)
    @doc "Add score in object format"
    (add-score
      (at 'uid record)
      (at 'source record)
      (at 'date record)
      (at 'score_inc record)
      (at 'comment record)
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
        (format-time "%u" (time (format "{}T00:00:00Z" [end_date])))
        "7"
      )
      "end_date must be sunday"
    )
    ;;;; As GasLimit is set to 150000, reduce multirow query
    ;(let
    ;  ((records_num (length (select reward-records (where 'end_date (= end_date))))
    ;  ))
    ;  (enforce
    ;    (= 0 records_num)
    ;    "rewards have been paid for last week"
    ;  )
    ;)
    (let*
      (
        (all_users (get-all-users))
        (total_score_sum (fold (+) 0.0 (map (at 'score ) all_users)))
        (weekly_score_records (query-score-records (map (at 'uid ) all_users) begin_date end_date 7))
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

  (defun get-user:object (uid:string)
    @doc "Query info by certain user"
    (read users uid)
  )
  (defun filter-uid:[object] (rows:[object])
    @doc "Filter rows for which key contains 'KD_' "
    (let*
      (
        (prefix (format "{}_" [UID_PREFIX]))
        (prefix_len (length prefix))
        (filter-row (lambda (obj) (contains prefix (take prefix_len (at 'key obj)))))
      )
      (filter (filter-row) rows)
    )
  )
  (defun get-all-users:[object] ()
    (filter-uid (get-all-users-without-filter))
  )
  (defun get-all-users-without-filter:[object] ()
    (let
      ( (consumer (lambda (key obj) (+ {"key": key} obj))) )
      (fold-db users (constantly true) (consumer))
    )
  )
  (defun get-users-with-tg-uids:[object] (tg_uids:[string])
    @doc "Query user account list by telegram uid list"
    (filter (lambda (obj) (contains (at 'telegram (at 'social_ids obj)) tg_uids)) (get-all-users))
  )
  (defun get-user-with-tg-uid:object (telegram_user_id:string)
    @doc "Query info by telegram uid"
    (let ((matched (get-users-with-tg-uids [telegram_user_id])))
      (enforce (<= 1 (length matched)) "User doesn't exist")
      (at 0 matched)
    )
  )

  (defun get-score:decimal (uid:string)
    @doc "Query score by certain user"
    (at 'score (read users uid ['score ]))
  )
  (defun get-total-score:decimal ()
    @doc "Query overall score"
    (fold (+) 0.0 (map (at "score") (get-all-users)))
  )

  (defun get-dates-between:[string] (begin_date:string end_date:string)
    @doc "Get date list from begin_date to end_date"
    (let*
      (
        (begin_time (time (format "{}T00:00:00Z" [begin_date])))
        (end_time (time (format "{}T00:00:00Z" [end_date])))
        (diff_secs (diff-time end_time begin_time))
        (diff_days (floor (/ diff_secs 86400)))
        (add (
          lambda 
          (day_cnt) 
          (format-time "%F" (add-time begin_time (days day_cnt)))
        ))
      )
      (map (add) (enumerate 0 diff_days))
    )
  )

  (defun get-all-score-records:[object] ()
    (filter-uid (get-all-score-records-without-filter))
  )
  (defun get-all-score-records-without-filter:[object] ()
    (let
      ((consumer (lambda (key obj) (+ {"key": key} obj))))
      (fold-db score-records (constantly true) (consumer))
    )
  )
  (defun get-score-records:[object] (uid:string)
    @doc "Query score records by certain user"
    (filter (lambda (x) (= (at 'uid x) uid)) (get-all-score-records))
  )
  (defun query-score-records:[object] (uids:[string] begin_date:string end_date:string days_cnt:integer)
    @doc "Query score records by certain user and certain dates"
    (let*
      (
        (dates (get-dates-between begin_date end_date))
        (add-source (lambda (date) (map (lambda (source) {"date": date, "source": source}) SCORE_SOURCE_TYPES)))
        (dates_and_sources (fold (+) [] (map (add-source) dates)))
        (get-record-with (lambda (uid obj)
          (try {"uid":""} (read score-records (format "{}_{}_{}" [uid, (at 'source obj), (at 'date obj)])))
        ))
        (get-records (lambda (uid)
          (filter (lambda (x) (!= "" (at 'uid x))) (map (get-record-with uid) dates_and_sources))
        ))
      )
      (enforce (= (- days_cnt 1) (length dates)) "days cnt doesn't match")
      (fold (+) [] (map (get-records) uids))
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

  ; -------------------------------------------------------
  ; Migrate

  (defun migrate-users:[string] (new_users:[object])
    ; modify user
    (with-capability (IS_KEDAO_BOT)
      (let
        ((update-user (
          lambda 
          (user)
          (insert users (at 'uid user) {
            "accounts": {"main": (at 'account user)},
            "social_ids": {"telegram": (at 'telegram_user_id user)},
            "join_time": (at 'join_time user),
            "score": (at 'score user),
            "level": 0.0,
            "title": ""
          })
        )))
        (map (update-user) new_users)
      )
    )
  )

  (defun migrate-score-records-step1:[string] (all_users:[object])
    (let*
      (
        (consumer (lambda (key obj) (+ {
          "key": key,
          "uid": (at 'uid (at 0 (filter (lambda (user) (= (at 'account user) (at 'account obj))) all_users)))
        } obj)))
        (update-uid (lambda (record)
          (update score-records (at 'key record) {
            "uid": (at 'uid record)
          })
        ))
      )
      (map (update-uid) (fold-db score-records (constantly true) (consumer)))
    )
  )

  (defun migrate-score-records-step2:[string] ()
    ; modify score-records
    (with-capability (IS_KEDAO_BOT)
      (let*
        (
          (create-new (
            lambda
            (old-record)
            ; insert new record
            (let*
              (
                (new-record {
                  "uid": (at 'uid old-record),
                  "source": (at 'source old-record),
                  "date": (take 10 (at 'created_time old-record)),
                  "score_inc": (at 'score_inc old-record),
                  "comment": (at 'comment old-record)
                })
                (key (
                  format 
                  "{}_{}_{}" 
                  [
                    (at 'uid new-record),
                    (at 'source new-record), 
                    (at 'date new-record)
                  ]
                ))
              )
              (insert score-records key
                new-record
              )
            )
            ; declare the status of old record
            (if
              (contains UID_PREFIX (at 'key old-record))
              "do nothing for new record"
              (update score-records (at 'key old-record)
                { "comment" : (format "(NOW THIS IS DEPRECATED!) PRE_SCORE_INC: {}" [(at 'score_inc old-record)]),
                  "score_inc": 0.0
                }
              )
            )
          ))
        )
        (map (create-new) (get-all-score-records-without-filter))
      )
    )
  )

)


