(module kedao GOVERNANCE
  @doc "module for basic things in kedao.org      \
        \ 1. user creation and profile storage    \
        \ 2. points calculation and updating      \
        \ 3. rewards redeeming   "

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
          \   - key @twitter_id / etc.                                           \
          \ @points: user's all kinds of properties                              \
          \   - key @droplets: present one's contribution                        \
          \   - key @flames: for communticate governance, earned by contribution \
          \   - key @goldstone: for redeeming gifts                              \
          \   - key @leaves: present one's integrity                             \
          \ @additions: user's other information                                 "

    wallet:object
    profile:object
    points:object
    additions:object
  )
  (deftable users:{user-schema})

  (defschema point-record-schema
    @doc "Record point diff, once a day per user        \
          \ @key: {uid}_{date}                          \
          \ @records: {type: {'diff', 'comments': [{'source', 'diff', 'comment'(opt), 'created_time'(opt)}]}}  "

    records:object
  )
  (deftable point-records:{point-record-schema})

  (defschema score-record-schema
    @doc "Record score increasing, once a day per user   \
          \ @key: {uid}_{source}_{date}                         \
          \ @source: the platform user earned scores "

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

  (defcap CREATE_USER (uid:string wallet:object profile:object)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap UPDATE_USER (uid:string update_contents:object)
    @event
    (compose-capability (IS_KEDAO_BOT))
  )
  (defcap UPDATE_POINTS (uid:string date:string type:string diff:decimal comments:[object])
    @event
    (compose-capability (IS_KEDAO_BOT))
  )

  (defcap GOLDSTONE_REDEEM (uid:string amount:decimal created_time:string)
    @event
    (enforce-guard (at 'guard (coin.details (at 'account (at 'wallet (get-user uid))))))
  )
  (defcap GOLDSTONE_REDEEM_AUTH ()
    (compose-capability (IS_KEDAO_BOT))
  )


  ; -------------------------------------------------------
  ; Constant

  (defconst KEDAO_ADMIN_ACCOUNT "k:eda0c886dccd1c036e3b1fa30ee3e1352e0c7af161b39773145d3bfbdace4239")
  (defconst KEDAO_BOT_ACCOUNT "k:eda09d458337883b5b1fa9c549ab9e81323549aad427215a938968f273d57de7")
  (defconst KEDAO_GOLDSTONE_MASTER "kedao_goldstone_master")

  (defconst DAILY_GOLDSTONE 75.0)
  (defconst GOLDSTONE_REDEEM_RATIO 0.1)                ; kda / goldstone redeem ratio
  (defconst GOLDSTONE_PRECISION 2)

  (defconst UID_PREFIX "KD")
  (defconst DROPLETS_DAILY_LIMIT {
    "telegram": 20.0,
    "twitter": 10.0
  })


  ; -------------------------------------------------------
  ; Utilities

  (defun require-goldstone-redeem ()
    (require-capability (GOLDSTONE_REDEEM_AUTH))
  )

  (defun initialize:string ()
    @doc "Initialize module"
    (with-capability (IS_KEDAO_BOT)
      (coin.create-account KEDAO_GOLDSTONE_MASTER (create-user-guard (require-goldstone-redeem)))
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
        (all-users (get-all-users))
        (member_cnt (length all-users))
        (rank (+ 1 member_cnt))
        (rank_str (if (< rank 10) (format "00{}" [rank])
          (if (< rank 100) (format "0{}" [rank]) (format "{}" [rank]))
        ))
        (uid (format "{}_{}" [UID_PREFIX rank_str]))
        (account (at 'account wallet))
        (matched (filter (lambda (user) 
          (or 
            (= (at 'key user) uid)
            (or
              (= (at 'account (at 'wallet user)) account)
              (= (at 'telegram_id (at 'profile user)) (at 'telegram_id profile))
            )
          )
        ) all-users))
        (balance (try -1.0 (coin.get-balance account)))
      )
      ; duplication validation
      (enforce (= 0 (length matched)) "User already existed, same uid/account/telegram_id")

      (with-capability (CREATE_USER uid wallet profile)
        (coin.enforce-reserved account guard)  ; Make sure account and guard match
        (enforce (= (take 2 account) "k:") "Account must start with k:") ; Make sure k:account
        (if (>= balance 0.0) ""
          (coin.create-account account guard)    ; Create account for user, if account not existed
        )
        (insert users uid {
          "wallet" : wallet, 
          "profile" : (+ {"uid": uid} profile),
          "points": {
            "droplets": 0.0,
            "flames": 0.0,
            "goldstone": 0.0,
            "leaves": 100.0
          },
          "additions": {}
        })
      )
    )
  )
  (defun update-user:string
    (
      uid:string
      wallet:object
      profile:object
      points:object
      additions:object
    )
    (with-capability (UPDATE_USER uid {
        "wallet": wallet,
        "profile": profile,
        "points": points,
        "additions": additions
      })
      (let ((user (read users uid)))
        (if (!= {} wallet)
          (update users uid {
            "wallet": (+ wallet (at 'wallet user))
          })
          "skip"
        )
        (if (!= {} profile)
          (update users uid {
            "profile": (+ profile (at 'profile user))
          })
          "skip"
        )
        (if (!= {} points)
          (update users uid {
            "points": (+ points (at 'points user))
          })
          "skip"
        )
        (if (!= {} additions)
          (update users uid {
            "additions": (+ additions (at 'additions user))
          })
          "skip"
        )
      )
    )
  )

  (defun increase-droplets:string 
    ( uid:string 
      date:string
      diff:decimal
      comments:[object]
    )
    @doc "Increase droplets for certain user \
          \ @comments: [{'source': 'xxx', 'diff': xxx.x, 'created_time': 'optional', 'comment': 'optional'}] "

    ; validate droplets diff
    (map (lambda 
        (comment)
        (enforce (contains (at 'source comment) DROPLETS_DAILY_LIMIT) (format "source {} from droplets is invalid" [(at 'source comment)]))
        (enforce (<= diff (at (at 'source comment) DROPLETS_DAILY_LIMIT)) (format "points for {} from droplets exceeded" [(at 'source comment)]))
        (enforce (> diff 0.0) "points diff cannot <= 0")
        )
      comments
    )
    (enforce (= diff (fold (+) 0.0 (map (at 'diff ) comments))) "Points diff can't match")

    (with-capability (UPDATE_POINTS uid date "droplets" diff comments)
      (let* 
        (
          (user (read users uid))
          (record_key (format "{}_{}" [uid date]))
          (pre_records (at 'records (try {"records": {}} (read point-records record_key))))
          (pre_droplets (at 'droplets (+ (take ['droplets ] pre_records) {"droplets": {"diff": 0.0, "comments": []}})))
          (pre_diff (at 'diff pre_droplets))
          (to_add (- diff pre_diff))
          (new_record {"diff": diff, "comments": comments})
          (new_records (+ {"droplets": new_record} pre_records))
        )
        (enforce (<= pre_diff diff) (format "New droplets diff must be larger than previous: got {}, {}" [diff pre_diff]))
        (if (< 0.0 to_add) (update-user uid {} {} { "droplets": (+ to_add (at 'droplets (at 'points user))) } {}) "skip")
        (write point-records record_key {"records": new_records})
      )
    )
  )
  (defun increase-droplets-in-bulk:[string] (records:[object])
    @doc "Add scores in batch"
    (map (lambda (record)
      ( increase-droplets
        (at 'uid record)
        (at 'date record)
        (at 'diff record)
        (at 'comments record)
      ))
      records
    )
  )

  (defun calc-goldstone-from-droplets:[string] (date:string)
    @doc "Calc goldstone every week"
    (let*
      (
        (all_users (get-all-users))
        (uids (map (at 'uid ) (map (at 'profile ) all_users)))
        (droplets_data (get-droplets-in-bulk uids date))
        (total_inc_droplets (fold (+) 0.0 (map (at 'diff ) droplets_data)))
        (calc-goldstone
          ( lambda
            (user_droplets)
            (let
              ((
                diff (round (* (/ (at 'diff user_droplets) total_inc_droplets) DAILY_GOLDSTONE) GOLDSTONE_PRECISION)
              ))
              {
                "uid" : (at 'uid user_droplets),
                "date": date,
                "diff" : diff,
                "comments": [{
                  "source": "droplets-rewards", 
                  "diff": diff,
                  "comment": (format "个人新增: {}, 社区新增: {}" [(at 'diff user_droplets) total_inc_droplets])
                }]
              }
            )
          )
        )
      )
      (filter (compose (at 'diff ) (< 0.0)) (map (calc-goldstone) droplets_data))
    )
  )
  (defun increase-goldstone:string (result:object date:string)
    @doc "Increase goldstone"
    (let*
      (
        (uid (at 'uid result))
        (user (read users uid))
        (diff (at 'diff result))
        (record_key (format "{}_{}" [uid date]))
        (pre_records (at 'records (try {"records": {}} (read point-records record_key))))
        (new_record {"diff": diff, "comments": (at 'comments result)})
        (new_records (+ {"goldstone": new_record} pre_records))
      )
      (enforce (not (contains 'goldstone pre_records)) (format "goldstone already increased for {}" [date]))
      (with-capability (UPDATE_POINTS uid date "goldstone" diff [])
        (update-user uid {} {} { "goldstone": (+ diff (at 'goldstone (at 'points user))) } {})
        (write point-records record_key {"records": new_records})
      )
    )
  )
  (defun increase-goldstone-in-bulk:[string] (results:[object] date:string)
    @doc "Increase goldstone"
    (map (lambda (result) (increase-goldstone result date)) results)
  )

  (defun get-point-records-overall:[object] ()
    (let
      ((consumer (lambda (key obj) (at 'records obj))))
      (fold-db point-records (constantly true) (consumer))
    )
  )
  (defun get-point-records:[object] (uid:string)
    (let*
      (
        (flt (lambda (key obj) (contains uid key)))
        (consumer (lambda (key obj) (at 'records obj)))
      )
      (fold-db point-records (flt) (consumer))
    )
  )
  (defun get-point-records-in-range:[object] (uid:string dates:[string])
    (filter (!= {}) (map (lambda (date) (at 'records (try {"records": {}} (read point-records (format "{}_{}" [uid date]))))) dates))
  )
  (defun get-droplets-in-bulk:[object] (uids:[string] date:string)
    (filter
      (!= {})
      ( map 
        (lambda (uid) 
          (let*
            (
              (records (at 'records (try {"records": {}} (read point-records (format "{}_{}" [uid date])))) )
              (droplets (at 'droplets (+ (take ['droplets ] records) {"droplets": {}})) )
            )
            (if (= droplets {}) {} (+ droplets {"uid": uid}))
          )
        )
        uids
      )
    )
  )

  (defun redeem-goldstone:string (uid:string amount:decimal created_time:string)
    (with-capability (GOLDSTONE_REDEEM uid amount created_time)
      (let*
        (
          (user (read users uid))
          (receiver (at 'account (at 'wallet user)))
          (date (take 10 created_time))
          (kda (* amount GOLDSTONE_REDEEM_RATIO))
          (record_key (format "{}_{}" [uid date]))
          (pre_goldstone (at 'goldstone (at 'points user)))
          (pre_records (at 'records (try {"records": {}} (read point-records record_key))))
          (pre_goldstone_records (at 'goldstone (+ (take ['goldstone ] pre_records) {"goldstone": {"diff":0.0,"comments":[]}})) )
          (new_record {
            "diff": (- (at 'diff pre_goldstone_records) amount), 
            "comments": (+ (at 'comments pre_goldstone_records) [{
              "source": "redeem-kda",
              "diff": (- amount),
              "created_time": created_time,
              "comment": (format "用{}金石兑换{}KDA" [amount kda])
            }])
          })
          (new_records (+ {"goldstone": new_record} pre_records))
        )
        (enforce (= (floor amount GOLDSTONE_PRECISION) amount) "Amount precision error")
        (enforce (< 0.0 amount) "Amount must be larger than 0")
        (enforce (<= amount pre_goldstone) "Goldstone balance is not sufficient")
        (update-user uid {} {} { "goldstone": (- pre_goldstone amount) } {})
        (write point-records record_key {"records": new_records})
        (with-capability (GOLDSTONE_REDEEM_AUTH)
          (install-capability (coin.TRANSFER KEDAO_GOLDSTONE_MASTER receiver kda))
          (coin.transfer KEDAO_GOLDSTONE_MASTER receiver kda)
        )
      )
    )
  )

  (defun get-user:object (uid:string)
    @doc "Query info by certain user"
    (read users uid)
  )
  (defun get-users:[object] (uids:[string])
    @doc "Query info by certain users"
    (let*
      (
        (flt (lambda (key obj) (contains key uids)))
        (consumer (lambda (key obj) (+ {"key": key} obj)))
      )
      (fold-db users (flt) (consumer))
    )
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
      ( (consumer (lambda (key obj) (+ {"key": key, "uid": key} obj))) )
      (fold-db users (constantly true) (consumer))
    )
  )
  (defun get-users-with-telegram-ids:[object] (telegram_ids:[string])
    @doc "Query user account list by telegram id list"
    (filter (lambda (obj) (contains (at 'telegram_id (at 'profile obj)) telegram_ids)) (get-all-users))
  )
  (defun get-user-with-telegram-id:object (telegram_id:string)
    @doc "Query info by telegram uid"
    (let ((matched (get-users-with-telegram-ids [telegram_id])))
      (enforce (<= 1 (length matched)) "User doesn't exist")
      (at 0 matched)
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
  (defun get-all-reward-records:[object] ()
    (filter-uid (get-all-reward-records-without-filter))
  )
  (defun get-all-reward-records-without-filter:[object] ()
    (let
      ((consumer (lambda (key obj) (+ {"key": key} obj))))
      (fold-db reward-records (constantly true) (consumer))
    )
  )

  ; -------------------------------------------------------
  ; Migrate

  (defun migrate-users:[string] (new_users:[object])
    ; modify user
    (with-capability (IS_KEDAO_BOT)
      (let
        ((upgrade-user (
          lambda 
          (new_user)
          (update users (at 'uid (at 'profile new_user)) {
            "wallet": (at 'wallet new_user),
            "profile": (at 'profile new_user),
            "points": {
              "droplets": 0.0,
              "flames": 0.0,
              "goldstone": 0.0,
              "leaves": 100.0
            },
            "additions": {}
          })
        )))
        (map (upgrade-user) new_users)
      )
    )
  )

  (defun migrate-droplets:[string] ()
    (let
      ((records (filter (compose (at 'score_inc ) (< 0.0)) (get-all-score-records))))
      (increase-droplets-in-bulk
        (map (lambda (record) {
          "uid": (take 6 (at 'key record)),
          "date": (take -10 (at 'key record)),
          "diff": (at 'score_inc record),
          "comments": [{
            "source": "telegram",
            "diff": (at 'score_inc record),
            "comment": (format "电报群内活跃发言{}{}次（内测期间收益翻倍）" [(floor (/ (at 'score_inc record) 2)), (if (<= 20.0 (at 'score_inc record)) "+" "")])
          }]
        })
        records
        )
      )
    )
  )

  (defun migrate-goldstone:[string] ()
    (let
      ((records (filter (compose (at 'amount ) (< 0.0)) (get-all-reward-records))))
      (map (lambda (record) 
          (let*
            (
              (pre_records (try {"records": {"droplets": {}} } (read point-records (at 'key record))))
              (goldstone (round (* 10.0 (at 'amount record)) GOLDSTONE_PRECISION))
              (kda (/ goldstone 10.0))
            )
            (write point-records (at 'key record) {
              "records": {
                "droplets": (at 'droplets (at 'records pre_records)),
                "goldstone": {
                  "diff": 0.0,
                  "comments": [{
                    "source": "droplets-rewards",
                    "diff": goldstone,
                    "comment": "周度活跃奖励"
                  },
                  {
                    "source": "redeem-kda",
                    "diff": (- goldstone),
                    "comment": (format "用{}金石兑换{}KDA" [goldstone kda])
                  }]
                }
              }
            })
          )
        )
        records
      )
    )
  )

)


