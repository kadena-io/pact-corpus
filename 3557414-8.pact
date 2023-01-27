(module kedao-rewards GOVERNANCE
  @doc "module for basic things in kedao.org      \
      \ 1. elemental points calculation and updating   \
      \ 2. goldstone redeeming   "

  ; -------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    @doc " Only support upgrading by admin."
    (enforce-guard (at 'guard (coin.details KEDAO_ADMIN_ACCOUNT)))
  )

  (defcap GOLDSTONE_REDEEM (uid:string amount:decimal created_time:string)
    @event
    (kedao.validate-user uid)
  )
  (defcap GOLDSTONE_REDEEM_AUTH ()
    (kedao.validate-bot)
  )

  ; -------------------------------------------------------
  ; Constant

  (defconst KEDAO_ADMIN_ACCOUNT "k:eda0c886dccd1c036e3b1fa30ee3e1352e0c7af161b39773145d3bfbdace4239")
  (defconst KEDAO_GOLDSTONE_MASTER "kedao-goldstone-master")

  (defconst DAILY_GOLDSTONE 75.0)
  (defconst GOLDSTONE_REDEEM_RATIO 0.1)                ; kda / goldstone redeem ratio
  (defconst GOLDSTONE_PRECISION 2)
  (defconst LEAVES_LIMIT 100.0)
  (defconst ADD_LEAVES_BY_DROPLETS_LIMIT 100.0)
  (defconst REDEEM_DROPLETS_REQUIRED 100.0)

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
    (kedao.validate-bot)
    (coin.create-account KEDAO_GOLDSTONE_MASTER (create-user-guard (require-goldstone-redeem)))
  )

  (defun increase-droplets:string 
    ( uid:string 
      date:string
      created_time:string
      details:[object]
    )
    @doc "Increase droplets for certain user \
          \ @details: [{'source': 'xxx', 'diff': xxx.x, 'created_time': 'optional', 'comment': 'optional'}] "

    ; validate droplets diff
    (map 
      (lambda 
        (detail)
        (enforce (contains (at 'source detail) DROPLETS_DAILY_LIMIT) (format "source {} from droplets is invalid" [(at 'source detail)]))
        (enforce (<= (at 'diff detail) (at (at 'source detail) DROPLETS_DAILY_LIMIT)) (format "points for {} from droplets exceeded" [(at 'source detail)]))
        (enforce (> (at 'diff detail) 0.0) "points diff cannot <= 0")
      )
      details
    )

    (let* 
      (
        (droplets_diff (fold (+) 0.0 (map (at 'diff ) details)))
        (user (kedao.get-user uid))
        (snapshot (kedao.get-yesterday-snapshot uid date))
        (pre_droplets_points (at 'droplets snapshot))
        (pre_leaves_points (at 'leaves snapshot))
        (new_leaves_points (kedao.keep-under-limit (+ pre_leaves_points droplets_diff)))
        (leaves_diff (- new_leaves_points pre_leaves_points))
        (record_key (kedao.record-key uid date))
        (new_droplets_record {"details": details})
        (new_leaves_record {"details": [{
          "source": "droplets-rewards",
          "diff": leaves_diff,
          "comment": "新人活跃奖励, 按新增水滴1:1提升树叶点数"
        }]})
        (update_contents
          ( if 
            (and (< pre_droplets_points ADD_LEAVES_BY_DROPLETS_LIMIT) (< 0.0 leaves_diff)) ; only add leaves when droplets < 100 and leaves_diff > 0
            {"droplets": new_droplets_record, "leaves": new_leaves_record} 
            {"droplets": new_droplets_record} 
          )
        )
      )
      (kedao.update-points uid date created_time update_contents)
    )
  )
  (defun increase-droplets-in-bulk:[string] (records:[object] created_time:string)
    @doc "Add droplets in batch"
    (map (lambda (record)
      ( increase-droplets
        (at 'uid record)
        (at 'date record)
        created_time
        (at 'details record)
      ))
      records
    )
  )

  (defun update-flames:string 
    ( uid:string 
      date:string
      created_time:string
      details:[object]
    )
    @doc "Update flames for certain user \
          \ @details: [{'source': 'xxx', 'diff': xxx.x, 'created_time': 'optional', 'comment': 'optional'}] "

    (let
      ((flames_diff (fold (+) 0.0 (map (at 'diff ) details))))
      (kedao.update-points uid date created_time {
        "flames": {
          "diff": flames_diff,
          "details": details
        }
      })
    )
  )
  (defun update-flames-in-bulk:[string] (records:[object] created_time:string)
    @doc "Update flames in batch"
    (map (lambda (record)
      ( update-flames
        (at 'uid record)
        (at 'date record)
        created_time
        (at 'details record)
      ))
      records
    )
  )
  (defun update-droplets-and-flames-in-bulk:[string] (droplets:[object] flames:[object] created_time:string)
    @doc "Update droplets and flames in batch"
    (increase-droplets-in-bulk droplets created_time)
    (update-flames-in-bulk flames created_time)
  )

  (defun calc-goldstone-from-droplets:[object] (date:string)
    @doc "Calc goldstone every week"
    (let*
      (
        (all_users (kedao.get-users))
        (uids (map (at 'uid ) (map (at 'profile ) all_users)))
        (point_records (kedao.get-point-records-in-bulk (map (lambda (uid) (kedao.record-key uid date)) uids)))
        (droplets_data (map
          ( lambda
            (records)
            (let*
              (
                (
                  result (take ['uid 'date 'diff 'leaves ] (+ {
                    "diff": (at 'diff (at 'droplets (+ records {"droplets": {"diff": 0.0}}))),
                    "leaves": (/ (at 'leaves (at 'points (kedao.get-user (at 'uid records)))) 100.0)
                  } records ))
                )
                (fixed_result (+ {"fixed_diff": (round (* (at 'diff result) (at 'leaves result)) 2)} result))
              )
              (if (< 0.0 (at 'fixed_diff fixed_result)) fixed_result {})  ; ignore zero records
            )
          )
          point_records
        ))
        (filtered_droplets_data (filter (!= {}) droplets_data))
        (total_inc_droplets (fold (+) 0.0 (map (at 'fixed_diff ) filtered_droplets_data)))
        (calc-goldstone
          ( lambda
            (user_droplets)
            (let
              ((
                goldstone (round (* (/ (at 'fixed_diff user_droplets) total_inc_droplets) DAILY_GOLDSTONE) GOLDSTONE_PRECISION)
              ))
              {
                "uid" : (at 'uid user_droplets),
                "date": date,
                "goldstone": {
                  "details": [{
                    "source": "droplets-rewards", 
                    "diff": goldstone,
                    "comment": (format "个人水滴系数: {} * {}, 社区新增: {}" [(at 'diff user_droplets) (at 'leaves user_droplets) total_inc_droplets])
                  }]
                }
              }
            )
          )
        )
      )
      (map (calc-goldstone) filtered_droplets_data)
    )
  )
  (defun increase-goldstone:string (result:object created_time:string)
    @doc "Increase goldstone"
    ( kedao.update-points 
      (at 'uid result)
      (at 'date result)
      created_time
      (take ['goldstone ] result)
    )
  )
  (defun increase-goldstone-in-bulk:[string] (results:[object] created_time:string)
    @doc "Increase goldstone"
    (map (lambda (result) (increase-goldstone result created_time)) results)
  )

  (defun redeem-goldstone:string (uid:string amount:decimal created_time:string)
    @doc "redeem goldstone"
    (with-capability (GOLDSTONE_REDEEM uid amount created_time)
      (let*
        (
          (nonce (take 6 (hash (chain-data))))
          (source_nonce (format "redeem-for-kda-{}" [nonce]))
          (date (take 10 created_time))
          (user (kedao.get-user uid))
          (receiver (at 'account (at 'wallet user)))
          (kda (* amount GOLDSTONE_REDEEM_RATIO))
          (update_contents {
            "goldstone": {
              "details": [{
                "source": source_nonce,
                "diff": (- amount),
                "created_time": created_time,
                "comment": (format "用{}金石兑换{}KDA" [amount kda])
              }]
            }
          })
        )
        (kedao.validate-source uid date "goldstone" source_nonce)
        (enforce (<= REDEEM_DROPLETS_REQUIRED (at 'droplets (at 'points user))) (format "Droplets must be larger than {}" [REDEEM_DROPLETS_REQUIRED]))
        (enforce (= (floor amount GOLDSTONE_PRECISION) amount) "Amount precision error")
        (enforce (< 0.0 amount) "Amount must be larger than 0")
        (enforce (<= amount (at 'goldstone (at 'points user))) "Goldstone balance is not sufficient")
        (kedao.update-points uid date created_time update_contents)
        (with-capability (GOLDSTONE_REDEEM_AUTH)
          (install-capability (coin.TRANSFER KEDAO_GOLDSTONE_MASTER receiver kda))
          (coin.transfer KEDAO_GOLDSTONE_MASTER receiver kda)
        )
      )
    )
  )

)


