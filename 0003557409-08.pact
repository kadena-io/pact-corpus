(module kedao GOVERNANCE
  @doc "module for basic things in kedao.org      \
        \ 1. user creation and profile storage    \
        \ 2. elemental points calculation and updating   "

  ; -------------------------------------------------------
  ; Schemas and Tables

  (defschema user-schema 
    @doc "Store user wallet / profile and points \
          \ key @uid: the member id in kedao, auto-increment, format KD_001      \
          \ @wallet: user's wallet info in Kadena blockchain                     \
          \   - key @type: owned / managed / local                               \
          \   - key @account: the main account                                   \
          \ @profile: some of user's important info                              \
          \   - key @uid: the member id in kedao                                 \
          \   - key @join_time: the datetime user join at                        \
          \   - key @telegram_id: integer                                        \
          \ @points: user's all kinds of properties                              \
          \   - key @droplets: present one's contribution                        \
          \   - key @flames: for communticate governance, earned by contribution \
          \   - key @goldstone: for redeeming gifts                              \
          \   - key @leaves: present one's integrity                             \
          \ @additions: user's other information                                 \
          \   - key @auto_redeem: auto redeem goldstone (bool)                   "

    wallet:object
    profile:object
    points:object
    additions:object
  )
  (deftable users:{user-schema})

  (defschema point-snapshot-schema
    @doc "Snapshot user points, once a day per user       \
          \ @key: {uid}_{date}                            "
    points:object
  )
  (deftable point-snapshots:{point-snapshot-schema})

  (defschema point-record-schema
    @doc "Record point diff, once a day per user          \
          \ @key: {uid}_{date}                            \
          \ @records: {                                   \
          \ - @type: {                                    \
          \   - @diff: total diff amount for the type     \
          \   - @details: [                               \
          \       - @source: update source                \
          \       - @diff: partial diff                   \
          \       - @comment: (optional)                  \       
          \       - @created_time: (optional)             \
          \     ]                                         \
          \   }                                           \
          \ }                                             "

    records:object
  )
  (deftable point-records:{point-record-schema})

  ; -------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    @doc " Only support upgrading by admin."
    (enforce-guard (at 'guard (coin.details KEDAO_ADMIN_ACCOUNT)))
  )

  (defcap BOT_AUTH ()
    @doc " Enforce requesters have kedao bot's signature. "
    (validate-bot)
  )
  (defcap USER_AUTH (uid:string)
    (validate-user uid)
  )

  (defcap UPDATE_USER (key:string created_time:string update_contents:object)
    @event
    (compose-capability (BOT_AUTH))
  )
  (defcap UPDATE_POINT_RECORDS (key:string created_time:string update_contents:object)
    @event
    (compose-capability (BOT_AUTH))
  )
  (defcap UPDATE_POINTS ()
    (compose-capability (BOT_AUTH))
    (compose-capability (UPDATE_USER_POINTS))
  )
  (defcap UPDATE_USER_POINTS ()
    @doc " Only avaiable when update points "
    true
  )
  (defcap UPDATE_POINT_SNAPSHOTS (key:string created_time:string update_contents:object)
    @event
    (compose-capability (BOT_AUTH))
  )
  


  ; -------------------------------------------------------
  ; Constant

  (defconst KEDAO_ADMIN_ACCOUNT "k:eda0c886dccd1c036e3b1fa30ee3e1352e0c7af161b39773145d3bfbdace4239")
  (defconst KEDAO_BOT_ACCOUNT "k:eda09d458337883b5b1fa9c549ab9e81323549aad427215a938968f273d57de7")

  (defconst LEAVES_LIMIT 100.0)

  (defconst UID_PREFIX "KD")
  (defconst ELEMENT_PRECISIONS {
    "droplets": 0,
    "flames": 2,
    "goldstone": 2,
    "leaves": 0
  })


  ; -------------------------------------------------------
  ; Utilities

  (defun validate-bot:bool ()
    @doc "Validate the identity of kedao bot"
    (enforce-guard (at 'guard (coin.details KEDAO_BOT_ACCOUNT)))
  )
  (defun validate-user:bool (uid:string)
    @doc "Validate the identity of user, ignored if owned "
    (let
      ((wallet (at 'wallet (get-user uid))))
      ( if (= "owned" (at 'type wallet))
        true
        (enforce-guard (at 'guard (coin.details (at 'account wallet))))
      )
    )
  )

  (defun create-user:string
    (
      wallet:object
      profile:object
      guard:guard
    )
    @doc "Create user as DAO member."
    ; profile validation
    (enforce (contains 'telegram_id profile) "Telegram id is needed")

    (let*
      (
        (all-users (get-users))
        (member_cnt (length all-users))
        (rank (+ 1 member_cnt))
        (rank_str (if (< rank 10) (format "00{}" [rank])
          (if (< rank 100) (format "0{}" [rank]) (format "{}" [rank]))
        ))
        (uid (format "{}_{}" [UID_PREFIX rank_str]))
        (account (at 'account wallet))
        (matched (filter (lambda (user) 
          (or 
            (= (at 'uid (at 'profile user)) uid)
            (or
              (= (at 'account (at 'wallet user)) account)
              (= (at 'telegram_id (at 'profile user)) (at 'telegram_id profile))
            )
          )
        ) all-users))
        (balance (try -1.0 (coin.get-balance account)))
        (update_contents {  ; default user info
          "wallet" : wallet, 
          "profile" : (+ {"uid": uid} profile),
          "points": (get-default-points),
          "additions": {
            "auto_redeem": true
          }
        })
      )
      ; duplication validation
      (enforce (= 0 (length matched)) "User already existed, same uid/account/telegram_id")
      ; validate account and guard
      (coin.enforce-reserved account guard)  ; Make sure account and guard match
      (enforce (= (take 2 account) "k:") "Account must start with k:") ; Make sure k:account
      (if (>= balance 0.0) "" (coin.create-account account guard))  ; Create account for user, if account not existed
      ; execute creation
      (update-user uid (take 10 (at 'join_time profile)) (at 'join_time profile) update_contents)
    )
  )
  (defun get-default-points:object ()
    @doc "default elemental points settings"
    {
      "droplets": 0.0,
      "flames": 0.0,
      "goldstone": 0.0,
      "leaves": 0.0
    }
  )

  (defun update-user:string
    (
      uid:string
      date:string
      created_time:string
      update_contents:object
    )
    @doc "update_contents: {'wallet': {'xxx': }, 'profile': ... }  \
          \ can be specified at level 2 - partially update,  \
          \ for example {'wallet': {'only some part': 'xx'}} "
    (with-capability (UPDATE_USER uid created_time update_contents)
      (let*
        (
          (default_user {"wallet":{}, "profile": {}, "points": {}, "additions": {}})
          (user (try default_user (read users uid)))
          (after-update (lambda (key) (+ (if (contains key update_contents) (at key update_contents) {}) (at key user))))
          (new_user {
            "wallet": (after-update "wallet"),
            "profile": (after-update "profile"),
            "points": (after-update "points"),
            "additions": (after-update "additions")
          })
        )
        ( if 
          (= user default_user)
          [ ; if create, make sure points are zero
            (enforce (= (at 'points update_contents) (get-default-points)) "points error")
            (insert users uid new_user)
          ]
          ( if 
            (contains 'points update_contents)
            ; if points modified, certain guard is needed
            [
              (require-capability (UPDATE_USER_POINTS))
              (update users uid new_user)
            ]
            ; if points not modified, user's guard is needed
            (with-capability (USER_AUTH uid)
              (update users uid new_user)
            )
          )
        )
        "Write succeeded"
      )
    )
  )
  (defun update-point-records:object
    (
      key:string
      created_time:string
      update_contents:object
    )
    @doc "update_contents: {'dropltes': 'point-record', 'flames': ... }  \
          \ some point_type and some source can only exist one record    \
          \ return {'droplets': 'diff_changed', 'flames': ... }          "
    (with-capability (UPDATE_POINT_RECORDS key created_time update_contents)
      (let*
        (
          (default_records (get-default-records))
          (pre_records (+ (at 'records (try {"records": {}} (read point-records key))) default_records))
          (after-update 
            ( lambda 
              (key) 
              (let*
                (
                  (new_record (at key (+ update_contents default_records)))
                  (new_details (at 'details new_record))
                  (new_sources (map (at 'source ) new_details))
                  (pre_record (at key pre_records))
                  (pre_details (at 'details pre_record))
                  (excluded_details (filter (lambda (obj) (not (contains (at 'source obj) new_sources))) pre_details)) ; remove same source and add new ones
                  (updated_details (+ new_details excluded_details))
                  (updated_diff (fold (+) 0.0 (map (at 'diff ) updated_details))) ; recalculate total diff
                )
                (enforce-unit key (at 'diff new_record))
                ( if 
                  (contains key update_contents)
                  {
                    "diff": updated_diff,
                    "details": updated_details,
                    "diff_changed": (- updated_diff (at 'diff pre_record))
                  }
                  pre_record
                )
              )
            )
          )
          (updated_records {
            "droplets": (after-update "droplets"),
            "flames": (after-update "flames"),
            "goldstone": (after-update "goldstone"),
            "leaves": (after-update "leaves")
          })
          (cleared (lambda (obj) (remove 'diff_changed obj)))
          (updated_records_cleared {
            "droplets": (cleared (at 'droplets updated_records)),
            "flames": (cleared (at 'flames updated_records)),
            "goldstone": (cleared (at 'goldstone updated_records)),
            "leaves": (cleared (at 'leaves updated_records))
          })
          (updated_records_without_blank (fold 
            ( lambda 
              (obj point_type)
              ( if 
                (= (get-default-record) (at point_type updated_records))  ; if no record, remove
                (remove point_type obj)
                obj
              )
            )
            updated_records_cleared 
            ["droplets", "flames", "goldstone", "leaves"]
          ))
        )
        (write point-records key {
          "records": updated_records_without_blank
        })
        updated_records
      )
    )
  )
  (defun update-points:string
    (
      uid:string
      date:string
      created_time:string
      update_contents:object
    )
    @doc "update_contents: {'dropltes': {'details': 'xxx'}, 'flames': ... }   \
         \ only add inc = diff_2 - diff_1 if there're already point records from some source "
    (with-capability (UPDATE_POINTS)
      (let*
        (
          (record_key (record-key uid date))
          (changed_results (update-point-records record_key created_time update_contents))
          (user (read users uid))
          (pre_points (at 'points user))
          (add-diff 
            ( lambda (key) 
              ( if 
                (and (contains key update_contents) (!= 0.0 (at 'diff_changed (at key changed_results))))
                (+ (at 'diff_changed (at key changed_results)) (at key pre_points) )  ; if updated, add changed points to previous points 
                (at key pre_points)
              )
            )
          )
          (updated_points {
            "droplets": (add-diff "droplets"),
            "flames": (add-diff "flames"),
            "goldstone": (add-diff "goldstone"),
            "leaves": (keep-under-limit (add-diff "leaves"))
          })
          (new_points (fold 
            ( lambda 
              (obj point_type)
              ( if 
                (= (at point_type updated_points) (at point_type pre_points))  ; if points doesn't change, keep, otherwise remove
                (remove point_type obj)
                obj
              )
            )
            updated_points 
            ["droplets", "flames", "goldstone", "leaves"]
          ))
        )
        (if (!= {} new_points)
          (update-user uid date created_time {
            "points": new_points
          })
          "skip"
        )
      )
    )
  )
  (defun enforce-unit:bool (point_type:string amount:decimal)
    (enforce (= (floor amount (at point_type ELEMENT_PRECISIONS)) amount) "Precision error")
  )
  (defun keep-under-limit:decimal (leaves:decimal)
    (if (< LEAVES_LIMIT leaves) LEAVES_LIMIT leaves)
  )
  (defun get-default-record:object ()
    {"diff": 0.0, "details": []}
  )
  (defun get-default-records:object ()
    (let
      ((default_no_record (get-default-record)))
      {
        "droplets": default_no_record,
        "flames": default_no_record,
        "goldstone": default_no_record,
        "leaves": default_no_record
      }
    )
  )
  (defun validate-source:bool (uid:string date:string point_type:string source:string)
    (let*
      (
        (record_key (record-key uid date))
        (details (at 'details (at point_type (+ (try {} (read point-records record_key)) (get-default-records)))))
        (macthed (filter (compose (at 'source ) (= source)) details))
      )
      (enforce (= 0 (length macthed)) "source has already existed")
    )
  )

  (defun get-user:object (uid:string)
    @doc "Query info by certain user"
    (read users uid)
  )
  (defun get-users:[object] ()
    (let*
      (
        (prefix_len (length UID_PREFIX))
        (flt (lambda (key obj) (= UID_PREFIX (take prefix_len key))))
        (consumer (lambda (key obj) obj))
      )
      (fold-db users (flt) (consumer))
    )
  )
  (defun get-users-in-bulk:[object] (uids:[string])
    @doc "Query info by certain users"
    (let
      (
        (flt (lambda (key obj) (contains key uids)))
        (consumer (lambda (key obj) obj))
      )
      (fold-db users (flt) (consumer))
    )
  )
  (defun query-users-with-telegram-ids:[object] (profiles:[object])
    @doc "Query user account list by telegram id list, profiles: [{'telegram_id': xx}] "
    (filter (lambda (obj) (contains (at 'telegram_id (at 'profile obj)) (map (at 'telegram_id ) profiles))) (get-users))
  )
  (defun get-points-rank:[object] (point_types:[string])
    (let*
      (
        (all_users (get-users))
        (get-tops (lambda (point_type)
          (take -5 (sort ['point ] (map (lambda (user) (+ {"point": (at point_type (at 'points user))} user)) all_users)))
        )) 
      )
      (map (get-tops) point_types)
    )
  )

  (defun get-point-records:[object] ()
    (let
      ((consumer (lambda (key obj) (+ {"uid": (take 6 key), "date": (take -10 key)} (at 'records obj)))))
      (fold-db point-records (constantly true) (consumer))
    )
  )
  (defun get-point-records-with-key:[object] ()
    (let
      ((consumer (lambda (key obj) (+ {"uid": (take 6 key), "date": (take -10 key), "key": key} (at 'records obj)))))
      (fold-db point-records (constantly true) (consumer))
    )
  )
  (defun get-point-records-in-bulk:[object] (keys:[string])
    (let
      (
        (flt (lambda (key obj) (contains key keys)))
        (consumer (lambda (key obj) (+ {"uid": (take 6 key), "date": (take -10 key)} (at 'records obj))))
      )
      (fold-db point-records (flt) (consumer))
    )
  )
  (defun query-point-records-with-uid:[object] (uid:string)
    (let
      (
        (flt (lambda (key obj) (contains uid key)))
        (consumer (lambda (key obj) (+ {"date": (take -10 key)} (at 'records obj))))
      )
      (fold-db point-records (flt) (consumer))
    )
  )
  (defun query-point-records-with-uid-range:[object] (uid:string begin_date:string end_date:string)
    (let
      (
        (flt (lambda (key obj) 
          ( let ((date (take -10 key)))
            (and 
              (= uid (take 6 key)) 
              (and (<= begin_date date) (>= end_date date))
            )
          ))
        )
        (consumer (lambda (key obj) (+ {"date": (take -10 key)} (at 'records obj))))
      )
      (fold-db point-records (flt) (consumer))
    )
  )
  (defun query-point-records-with-date:[object] (date:string)
    (let
      (
        (flt (lambda (key obj) (= (take -10 key) date)))
        (consumer (lambda (key obj) (+ {"uid": (take 6 key)} (at 'records obj))))
      )
      (fold-db point-records (flt) (consumer))
    )
  )
  (defun query-point-records-with-range:[object] (begin_date:string end_date:string)
    (let
      (
        (flt (lambda (key obj) (and (<= begin_date (take -10 key)) (>= end_date (take -10 key)))))
        (consumer (lambda (key obj) (+ {"uid": (take 6 key), "date": (take -10 key)} (at 'records obj))))
      )
      (fold-db point-records (flt) (consumer))
    )
  )

  (defun get-point-snapshots:[object] ()
    (let
      ((consumer (lambda (key obj) (+ {"uid": (take 6 key), "date": (take -10 key)} (at 'points obj)))))
      (fold-db point-snapshots (constantly true) (consumer))
    )
  )
  (defun get-point-snapshots-in-bulk:[object] (keys:[string])
    (let
      (
        (flt (lambda (key obj) (contains key keys)))
        (consumer (lambda (key obj) (+ {"uid": (take 6 key), "date": (take -10 key)} (at 'points obj))))
      )
      (fold-db point-snapshots (flt) (consumer))
    )
  )
  (defun query-point-snapshots-with-uid:[object] (uid:string)
    (let
      (
        (flt (lambda (key obj) (contains uid key)))
        (consumer (lambda (key obj) (+ {"date": (take -10 key)} (at 'points obj))))
      )
      (fold-db point-snapshots (flt) (consumer))
    )
  )
  (defun query-point-snapshots-with-uid-range:[object] (uid:string begin_date:string end_date:string)
    (let
      (
        (flt (lambda (key obj) 
          ( let ((date (take -10 key)))
            (and 
              (= uid (take 6 key)) 
              (and (<= begin_date date) (>= end_date date))
            )
          ))
        )
        (consumer (lambda (key obj) (+ {"date": (take -10 key)} (at 'points obj))))
      )
      (fold-db point-snapshots (flt) (consumer))
    )
  )
  (defun query-point-snapshots-with-date:[object] (date:string)
    (let
      (
        (flt (lambda (key obj) (= (take -10 key) date)))
        (consumer (lambda (key obj) (+ {"uid": (take 6 key)} (at 'points obj))))
      )
      (fold-db point-snapshots (flt) (consumer))
    )
  )
  (defun query-point-snapshots-with-range:[object] (begin_date:string end_date:string)
    (let
      (
        (flt (lambda (key obj) (and (<= begin_date (take -10 key)) (>= end_date (take -10 key)))))
        (consumer (lambda (key obj) (+ {"uid": (take 6 key), "date": (take -10 key)} (at 'points obj))))
      )
      (fold-db point-snapshots (flt) (consumer))
    )
  )
  (defun get-yesterday-snapshot:object (uid:string date:string)
    (let*
      (
        (user (read users uid))
        (join_date (take 10 (at 'join_time (at 'profile user))))
      )
      ( if 
        (>= join_date date) 
        (get-default-points)  ; get default points if user join_time is later than today
        (at 'points (read point-snapshots (record-key uid (date-before date))))
      )
    )
  )

  (defun record-key:string (uid:string date:string)
    (format "{}_{}" [uid date])
  )

  (defun date-before:string (date:string)
    @doc "Get the date before input date  \
          \ input: '2022-01-02' output: '2022-01-01' "
    (format-time "%F" (add-time (parse-time "%F" date) (days -1)))
  )

  (defun update-all-point-snapshots:[string] (date:string created_time:string)
    (let*
      (
        (all_users (get-users))
        (add-snapshots (lambda (user)
          (let*
            (
              (yesterday (date-before date))
              (uid (at 'uid (at 'profile user)))
              (yesterday_key (record-key uid yesterday))
              (pre_snapshot (at 'points (try {"points": (get-default-points)} (read point-snapshots yesterday_key))))
              (today_key (record-key uid date))
              (records (+ (at 'records (try {"records": {}} (read point-records today_key))) (get-default-records)))
              (update_contents {
                "droplets": (+ (at 'droplets pre_snapshot) (at 'diff (at 'droplets records))),
                "flames": (+ (at 'flames pre_snapshot) (at 'diff (at 'flames records))),
                "goldstone": (+ (at 'goldstone pre_snapshot) (at 'diff (at 'goldstone records))),
                "leaves": (+ (at 'leaves pre_snapshot) (at 'diff (at 'leaves records)))
              })
            )
            (with-capability (UPDATE_POINT_SNAPSHOTS today_key created_time update_contents)
              (write point-snapshots today_key {
                "points": update_contents
              })
            )
          )
        ))
      )
      (map (add-snapshots) all_users)
    )
  )

)


