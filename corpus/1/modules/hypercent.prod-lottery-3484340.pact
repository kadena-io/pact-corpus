(module prod-lottery 'hyper-admin-multi-keyset-prod
  (defcap IS_ADMIN ()
    (enforce-keyset 'hyper-api-admin-prod))

  (defcap ADD_QUEUE() true)

  (defschema project
    token:string
    start_date:time
    winners:[string]
    round:integer
    pre_winners:[object:*]
    participants_processed:integer
  )
  (deftable projects-table:{project})

  (defschema guaranteed
    account:string
    project:string
    win:bool
  )
  (deftable guaranteed-table:{guaranteed})

  (defschema participant
    project:string
    account:string
    tickets:integer
    tickets-numbers:[string]
    win:bool
  )
  (deftable participants-table:{participant})

  (defschema queue
    position:integer
    round:integer
  )
  (deftable queue-table:{queue})


  (defconst QUEUE_TIME 5)

  ;time in minutes per participant
  (defconst QUEUE_TIME_PER_PARTICIPANT 10)

  (defconst DRAW_TICKET_LENGTH 5)
  (defconst HYPE_PROJECT_NAME "hype")
  (defconst HYPE_START_DATE (time (read-msg 'hype-start-date)))

  (defun init ()
    (with-capability (IS_ADMIN)
      (add-project HYPE_PROJECT_NAME HYPE_START_DATE)
    )
  )

  (defun add-project (token:string start_date:time)
    (with-capability (IS_ADMIN)
      (insert projects-table token
        {"token": token,
          "start_date": start_date,
          "winners": [],
          "pre_winners": [],
          "participants_processed": 0,
          "round": 0}
      )
    )
  )

  (defun get-project (project:string)
    (read projects-table project)
  )

  (defun get-project-round (token:string)
    (at 'round (read projects-table token))
  )

  (defun has-project-started(project:string)
    (> 0 (get-project project))
  )

  (defun has-guaranteed-allocation(project:string account:string)
      (hypercent.prod-community.has-ga account project)
  )

  (defun add-guaranteed-allocation (project:string account:string)
    false
  )

  (defun add-participant (account:string tickets:integer project:string)
    (enforce-guard (at 'guard (prod-hype-coin.details account)))
    (let* (
        (participant_key (get-participant-key account project))
        (is_winner (has-guaranteed-allocation project account))
      )
      (if (constantly is_winner)
        (let* (
            (tickets_list [])
          )
          (with-default-read participants-table participant_key
            {"tickets": 0,
              "account": "",
              "tickets-numbers": []}
            {"tickets" := existing_tickets,
              "account" := existing_participant,
              "tickets-numbers" := existing_tickets_numbers}
            (write participants-table participant_key
              {"project": project,
                "tickets": (+ existing_tickets tickets),
                "win": is_winner,
                "account": account,
                "tickets-numbers": (+ existing_tickets_numbers tickets_list) }
            )
            (with-read projects-table project {"winners":=winners}
              (update projects-table project {"winners": (+ [account] winners)})
              (with-capability (ADD_QUEUE)
                (insert-queue 1 false [0 participant_key])
              )
            )
          )
        )
        (let* (
            (has-tickets (enforce (> tickets 0) "tickets amount greater than 0"))
            (debit-tickets (prod-community.debit-tickets account tickets))
            (empty_tickets_arr (map (wrap-list) (enumerate 1 tickets)))
            (tickets_list (map (make-one-draw) (map (create-hash-from-salt) (map (+ [tickets account]) empty_tickets_arr))))
          )
          (with-default-read participants-table participant_key
            {"tickets": 0,
              "account": "",
              "tickets-numbers": []}
            {"tickets" := existing_tickets,
              "account" := existing_participant,
              "tickets-numbers" := existing_tickets_numbers}
            (write participants-table participant_key
              {"project": project,
              "tickets": (+ existing_tickets tickets),
              "win": is_winner,
              "account": account,
              "tickets-numbers": (+ existing_tickets_numbers tickets_list) }
            )
          )
        )
      )
    )
  )

  (defun wrap-list(item)
    [item]
  )

  (defun get-queue-entry (key:string)
    (read queue-table key)
  )

  (defun get-participant (account:string project:string)
    (read participants-table (get-participant-key account project))
  )


  (defun fix-guaranteed-allocation (account:string project:string)
   (with-capability (IS_ADMIN)
        (with-read projects-table project {"winners":=winners}
          (let ((participant_key (get-participant-key account project)))
              (prod-community.admin-claim-reward account (+ project "_GA"))
              (write participants-table participant_key
                {"project": project,
                  "tickets": 1,
                  "win":true,
                  "account": account,
                  "tickets-numbers": [] }
              )
              (with-capability (ADD_QUEUE)
                (insert-queue 1 false [0 participant_key])
              )
              (update projects-table project {"winners": (+ [account] winners)})
          )
        ) 
    )
  )

  ;each project participant has an unique key that identify him
  (defun get-participant-key (account: string project:string)
    (+ account project)
  )

  (defun get-participant-key-reversed (project:string account:string)
     (get-participant-key account project)
  )

  (defun get-participant-tickets (account:string project:string)
    (at "tickets" (get-participant account project))
  )

  (defun queue-details (account:string project:string)
      (let* (
        (project (get-project project))
        (queue (get-queue-entry (get-participant-key account project)))
        (position (at "position" queue))
        (now:time (at "block-time" (chain-data)))
        (redeem_start (at "start_date" project))
        (start:time (add-time redeem_start (minutes (* position QUEUE_TIME_PER_PARTICIPANT))))
        (end:time (add-time start (minutes QUEUE_TIME_PER_PARTICIPANT)))
        )
        [position start end (at "round" queue)]
      )
  )

  (defun has-won(account:string project:string)
    ;check if first round has ended
    ;(enforce (> (get-project-round project) 0))
    ;(enforce (> 1 0))
    (= (at "round" (get-queue-entry (get-participant-key account project))) (at "round" (get-project project)))
  )

  (defun is-next-in-queue(account:string project_key:string)
      (let* (
        (project (get-project project_key))
        (queue (get-queue-entry (get-participant-key account project_key)))
        (position (at "position" queue))
        (now:time (at "block-time" (chain-data)))
        (redeem_start (at "start_date" project))
        (start:time (add-time redeem_start (minutes (* position QUEUE_TIME_PER_PARTICIPANT))))
        (end:time (add-time start (minutes QUEUE_TIME_PER_PARTICIPANT)))
        )
          ;current round
          (enforce (= (at "round" project) (at "round" queue)) "Round mismatch")
          (if (= (at "round" project) 3)
            (enforce (and (>= now start) (< now end)) "Time slot mismatch")
            (enforce (>= now start) "Time slot mismatch")
          )
          true
        )
  )

  ;  (defun can-redeem(account:string project:string)
  ;    (and )
  ;  )

  (defun make-draw (salt: string)
    (let* (;(block-time-hash (hash (at "block-time" (chain-data))))
        (prev-block-hash (at "prev-block-hash" (chain-data)))
        (block-height-hash (format "{}" [(at "block-height"(chain-data))]))
        (master-hash-int (str-to-int 64 (hash (+ block-height-hash (+  prev-block-hash (create-hash-from-salt [salt (tx-hash)]))))))
        ;(master-hash-int (str-to-int 64 (hash prev-block-hash)))
        (master-hash-str (format "{}" [master-hash-int]))
        (pos-one (take (- DRAW_TICKET_LENGTH) master-hash-str))
        (master-hash-str (drop  (- DRAW_TICKET_LENGTH) master-hash-str))
        (pos-two (take  (- DRAW_TICKET_LENGTH) master-hash-str))
        (master-hash-str (drop  (- DRAW_TICKET_LENGTH) master-hash-str))
        (pos-three (take  (- DRAW_TICKET_LENGTH) master-hash-str))
        (master-hash-str (drop  (- DRAW_TICKET_LENGTH) master-hash-str))
        (pos-four (take  (- DRAW_TICKET_LENGTH) master-hash-str))
        (master-hash-str (drop  (- DRAW_TICKET_LENGTH) master-hash-str))
        (pos-five (take  (- DRAW_TICKET_LENGTH) master-hash-str))
      )

      ;(enforce false [pos-five pos-four pos-three pos-two pos-one])
      ;note that the draw ranking is in reverse order
      [pos-five pos-four pos-three pos-two pos-one]
    )
  )

  (defun draw-random-digit:string (salt: string max:integer)
    (let* ((block-time-hash (hash (at "block-time" (chain-data))))
        (prev-block-hash (at "prev-block-hash" (chain-data)))
        (block-height-hash (format "{}" [(at "block-height"(chain-data))]))
        (salt-hash (hash (+ prev-block-hash (hash (format "{}{}" [salt max])))))
        (master-hash-int (str-to-int 64 (hash (+ block-height-hash (+ prev-block-hash (+ block-time-hash salt-hash))))))
        ;(master-hash-int (str-to-int 64 (hash prev-block-hash )))
        ;(master-hash-int (str-to-int 64 (hash prev-block-hash)))
        (master-hash-str (format "{}" [master-hash-int]))
        (matches (filter (compose (str-to-int) (>= max)) (str-to-list master-hash-str)))
      )

      (str-to-int (at 0 matches))
    )
  )

  (defun in-array (arr:list el)
        (contains el arr)
  )

  (defun remove-duplicates (a:list b:list)
        (filter (compose (in-array b)  (not)) a)
  )

  
  (defun pre-process-participants (batch_length:integer project:string winners_per_round:integer)
      (with-capability (IS_ADMIN)
      (with-read projects-table project
        {"round" := round, "winners" := winners, "pre_winners" := pre_winners, "participants_processed" := participants_processed}
        (let* (
               (extracted_number (str-to-int (at 0 (make-draw (create-hash-from-salt [round project batch_length])))))
               (participant_keys (sort ( keys participants-table )))
               (all_players (remove-duplicates participant_keys (map (get-participant-key-reversed project) winners)))
               (batch_players (take batch_length (drop participants_processed all_players)))
               (processed (map (process_batched_player extracted_number) batch_players))
               (new_pre_winners (take winners_per_round (sort ["pos"] (+ pre_winners processed))))
          )
          (update projects-table project
            {
              "pre_winners": new_pre_winners,
              "participants_processed": (+ participants_processed batch_length)
            }
          )
          (length batch_players)
        )
      )
    )
  )

  (defun participant-key-to-account (project:string participant_key:string) 
      (drop (* -1 (length project)) participant_key)
  )

  (defun update-round-queue (project:string round:integer queued:bool round_start:integer start:integer limit:integer)
    (with-capability (IS_ADMIN)
        (with-read projects-table project
          {"winners" := winners}
          (let* (
              (round_winners (drop round_start winners))
              (ranked (map (rank-winners round_winners project)  (enumerate 0 (- (length round_winners) 1)) ))
              (batch (take limit (drop start ranked)))
            )
            (with-capability (ADD_QUEUE)
              (map (insert-queue round queued) batch)
            )
            batch
          )
        )
    )
  )

  (defun start_round (project:string round:integer buffer_hours:integer)
      (with-capability (IS_ADMIN)  
        (let* (
                    (now (at "block-time" (chain-data)))
                    (redeem_date (add-time now (hours buffer_hours)))
                  )
                  ;(map (insert-queue (+ round 1) queued) ranked)
                  (update projects-table project
                    {
                      "start_date": redeem_date,
                      "round": round
                    }
                  )
        )
      )
  )

  (defun run (project:string winners_per_round:integer)
    (with-capability (IS_ADMIN)
      (with-read projects-table project
        {"round" := round, "winners" := winners, "pre_winners":= pre_winners}
        (let* (
            (round_winners (map (participant-key-to-account project)  (map (at "key") (take winners_per_round pre_winners))))
          )
          (update projects-table project
            {
              "winners": (+ winners round_winners),
              "pre_winners": (drop winners_per_round pre_winners)
            }
          )
          round_winners
        )
      )
    )
  )

  (defun process_batched_ticket (extracted_number:integer ticket:integer)
    (let* (
        (draw (abs (- ticket extracted_number)))
      )
      draw
    )
  )

  (defun process_batched_player (extracted_number:integer participant_key:string)
    (require-capability (IS_ADMIN))
    (let* (
        (player  (read participants-table participant_key))
        ;user ticket lists might contain duplicate ticket entries
        (tickets_list (at "tickets-numbers" player))
        ;by comparing each user ticket with a fresh generate value we enforce fairness
        (tickets_position_list (map (process_batched_ticket extracted_number) (map (str-to-int) tickets_list)))
        (best_ticket (at 0 (sort tickets_position_list)))
      )
      { 
        "pos": best_ticket,
        "key": participant_key
      }
    )
  )

  (defun wrap-template-item-into-list(template-item item)
    [template-item item]
  )

  (defun get-winners (project: string)
    (at 'winners (read projects-table project))
  )

  (defun mark-participant-as-winner (key)
    (require-capability (IS_ADMIN))
    ;(write queue-table key {"round": round, "position": (if (= true queued) position 0 )})
    (update participants-table  key { "win": true })
  )

  (defun insert-queue(round:integer queued:bool entry)
      (require-capability (ADD_QUEUE))
      (insert queue-table (at 1 entry) {"round": round, "position": (if (= true queued) (at 0 entry) 0 )})

  )

  ;winners: [winner1, winner2, winner3] pos: 2
  ;[2, winner2]
  (defun rank-winners (winning:[string] project:string pos:integer)
            [pos (get-participant-key (at pos winning) project)]
  )
  

  ;this is probably way to much hashing
  (defun create-hash-from-salt (salt)
    (hash (format "{}" [salt]))
  )

  (defun make-one-draw (salt:string)
    (at 0 (make-draw salt))
  )
)


