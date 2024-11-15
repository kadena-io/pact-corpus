(module kedao-play GOVERNANCE
  @doc "module for series of interesting play in kedao.org"

  ; -------------------------------------------------------
  ; Schemas and Tables

  (defschema quiz-schema
    @doc "Quiz module               \
          \ key: quiz_id  \
          \ option_ids    \
          \ deadline      \
          \ correct_option_id "

    option_ids:[string]
    deadline:string
    correct_option_id:string
  )
  (deftable quizes:{quiz-schema})

  (defschema quiz-pool-schema
    @doc "Quiz pool module               \
          \ key: {quiz_id}_{uid}  \
          \ quiz_id    \
          \ uid        \
          \ option_id  \
          \ bet: goldstone amount  "

    quiz_id:string
    uid:string
    option_id:string
    bet:decimal
  )
  (deftable quiz-pools:{quiz-pool-schema})

  ; -------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    @doc " Only support upgrading by admin."
    (enforce-guard (at 'guard (coin.details KEDAO_ADMIN_ACCOUNT)))
  )

  ; -------------------------------------------------------
  ; Constant

  (defconst KEDAO_ADMIN_ACCOUNT "k:eda0c886dccd1c036e3b1fa30ee3e1352e0c7af161b39773145d3bfbdace4239")
  (defconst BET_MAX_AMOUNT 10.0)

  ; -------------------------------------------------------
  ; Utilities

  ; ----------- QUIZ ------------

  (defun create-quiz:string (quiz_id:string option_ids:[string] deadline:string)
    ;(kedao.validate-bot)
    (insert quizes quiz_id {
      "option_ids": option_ids,
      "deadline": deadline,
      "correct_option_id": ""
    })
  )

  (defun is-over-time:bool (deadline:string)
    ; use UTC-8 timezone
    (>= (format-time "%Y-%m-%dT%H:%M:%S" (add-time (at 'block-time (chain-data)) (hours 8))) deadline)
  )

  (defun update-quiz-result:string (quiz_id:string correct_option_id:string created_time:string)
    ;(kedao.validate-bot)
    (let 
      ((quiz (read quizes quiz_id)))
      (enforce (is-over-time (at 'deadline quiz)) "deadline error")
      (enforce (contains correct_option_id (at 'option_ids quiz)) "unexpected option_id")
      (update quizes quiz_id {
        "correct_option_id" : correct_option_id
      })
    )
    (close-quiz-pool quiz_id correct_option_id created_time)
  )

  (defun add-quiz-pool:string
    (
      quiz_id:string
      uid:string
      option_id:string
      bet:decimal
      created_time:string
    )
    ;(kedao.validate-bot)

    ; validate bet
    (enforce (< 0.0 bet) "Bet amount must larger than 0")
    (enforce (<= bet BET_MAX_AMOUNT) "unexpected bet amount")
    (kedao.enforce-unit "goldstone" bet)

    (let* 
      (
        (quiz (read quizes quiz_id))
        (quiz_pool (try {} (read quiz-pools (format "{}_{}" [quiz_id, uid]))))
        (user (kedao.get-user uid))
        (balance (at 'goldstone (at 'points user)))
        (nonce (take 6 (hash (chain-data))))
      )
      ;(kedao.validate-user uid)
      (enforce (>= balance bet) "Balance is not sufficient")
      (enforce (not (is-over-time (at 'deadline quiz))) "deadline error")
      (enforce (contains option_id (at 'option_ids quiz)) "unexpected option_id")
      (if (and (!= {} quiz_pool) (> (at 'bet quiz_pool) 0.0))
        ; if user have existed record and bet amount > 0, cancel first
        (cancel-quiz-pool quiz_id uid created_time)
        "no need for cancelling"
      )
      ( kedao.update-points 
        uid
        (take 10 created_time)
        created_time
        {
          "goldstone": {
            "details": [{
              "source": (format "quiz_{}_add_{}" [quiz_id, nonce]), 
              "diff": (- bet),
              "comment": (format "参与竞猜: {}, 金石数量: {}" [quiz_id, bet])
            }]
          }
        }
      )
      (write quiz-pools (format "{}_{}" [quiz_id, uid]) {
        "quiz_id" : quiz_id,
        "uid": uid,
        "option_id": option_id,
        "bet": bet
      })
    )
  )

  (defun cancel-quiz-pool:string
    (
      quiz_id:string
      uid:string
      created_time:string
    )
    ;(kedao.validate-bot)

    (let* 
      (
        (quiz (read quizes quiz_id))
        (quiz-pool (read quiz-pools (format "{}_{}" [quiz_id, uid])))
        (bet (at 'bet quiz-pool))
        (user (kedao.get-user uid))
        (balance (at 'goldstone (at 'points user)))
        (nonce (take 6 (hash (chain-data))))
      )
      ;(kedao.validate-user uid)
      (enforce (> bet 0.0) "Bet is already 0")
      (enforce (not (is-over-time (at 'deadline quiz))) "deadline error")
      ( kedao.update-points 
        uid
        (take 10 created_time)
        created_time
        {
          "goldstone": {
            "details": [{
              "source": (format "quiz_{}_cancel_{}" [quiz_id, nonce]), 
              "diff": bet,
              "comment": (format "取消竞猜: {}, 金石数量: {}" [quiz_id, bet])
            }]
          }
        }
      )
      (update quiz-pools (format "{}_{}" [quiz_id, uid]) {
        "option_id": "",
        "bet": 0.0
      })
    )
  )

  (defun close-quiz-pool:string (quiz_id:string correct_option_id:string created_time:string)
    ;(kedao.validate-bot)
    (let*
      (
        (flt (lambda (key obj) (= (at 'quiz_id obj) quiz_id)))
        (consumer (lambda (key obj) obj))
        (quiz_pool (fold-db quiz-pools (flt) (consumer)))
        (right_ones (filter (lambda (one_bet) (= correct_option_id (at 'option_id one_bet))) quiz_pool))
        (wrong_ones (filter (lambda (one_bet) (!= correct_option_id (at 'option_id one_bet))) quiz_pool))
        (right_bet (fold (+) 0.0 (map (at 'bet ) right_ones)))
        (total_bet (fold (+) 0.0 (map (at 'bet ) quiz_pool)))
        (refund 
          ( lambda 
            (one_bet) 
            ( kedao.update-points 
              (at 'uid one_bet)
              (take 10 created_time)
              created_time
              {
                "goldstone": {
                  "details": [{
                    "source": (format "quiz_{}_close" [quiz_id]), 
                    "diff": (at 'bet one_bet),
                    "comment": (format "竞猜结束: {}, 无人获胜，返还金石数量: {}" [quiz_id, (at 'bet one_bet)])
                  }]
                }
              }
            )
          )
        )
        (reward-winner
          ( lambda 
            (one_bet)
            (let*
              (
                (rewards_origin (* total_bet (/ (at 'bet one_bet) right_bet)))
                (rewards (floor rewards_origin (at 'goldstone kedao.ELEMENT_PRECISIONS)))
              )
              ( kedao.update-points 
                (at 'uid one_bet)
                (take 10 created_time)
                created_time
                {
                  "goldstone": {
                    "details": [{
                      "source": (format "quiz_{}_close" [quiz_id]), 
                      "diff": rewards,
                      "comment": (format "竞猜结束: {}, 赢得金石数量: {}" [quiz_id, rewards])
                    }]
                  }
                }
              )
            )
          )
        )
        (reward-loser
          ( lambda 
            (one_bet)
            (let*
              (
                (rewards 0.0)
              )
              ( kedao.update-points 
                (at 'uid one_bet)
                (take 10 created_time)
                created_time
                {
                  "goldstone": {
                    "details": [{
                      "source": (format "quiz_{}_close" [quiz_id]), 
                      "diff": rewards,
                      "comment": (format "竞猜结束: {}, 赢得金石数量: {}" [quiz_id, rewards])
                    }]
                  }
                }
              )
            )
          )
        )
      )
      (if (= 0 (length right_ones))
        ; if no one is right, then refund for everyone
        (map (refund) quiz_pool)
        ; otherwise, reward the winners
        [
          (map (reward-winner) right_ones)
          (map (reward-loser) wrong_ones)
        ]
      )
    )
  )

  (defun get-quiz:object (quiz_id:string)
    (+ {"quiz_id": quiz_id} (read quizes quiz_id))
  )
  (defun get-all-quizes:[object] ()
    (let ((consumer (lambda (key obj) (+ { "quiz_id": key } obj))))
      (fold-db quizes (constantly true) (consumer))
    )
  )
  (defun get-quiz-pool:object (quiz_id:string uid:string)
    (read quiz-pools (format "{}_{}" [quiz_id, uid]))
  )
  (defun get-all-quiz-pools:[object] ()
    (let ((consumer (lambda (key obj) obj)))
      (fold-db quiz-pools (constantly true) (consumer))
    )
  )
  (defun query-quiz-pools-with-quiz-id:[object] (quiz_id:string)
    (let
      (
        (flt (lambda (key obj) (= (at 'quiz_id obj) quiz_id)))
        (consumer (lambda (key obj) obj))
      )
      (fold-db quiz-pools (flt) (consumer))
    )
  )

)


