(module lottery 'admin-multi-keyset
  (defcap IS_ADMIN ()
    (enforce-keyset 'admin-1))

  (defschema project
    token:string
    start_date:time
    winners:[string]
    round:integer
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

  ;time in hours for winners to do KYC
  (defconst ROUND_WARMUP_TIME 6)
  (defconst DRAW_TICKET_LENGTH 5)
  (defconst HYPE_PROJECT_NAME "epyh")
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
      (free.community.has-ga account project)
  )

  (defun add-guaranteed-allocation (project:string account:string)
    false
  )

  (defun add-participant (account:string tickets:integer project:string)
    (enforce-guard (at 'guard (hype-coin.details account)))
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
            (write queue-table participant_key {"position": 0, "round": 1})
          )
        )
        (let* (
            (has-tickets (enforce (> tickets 0) "tickets amount greater than 0"))
            (debit-tickets (community.debit-tickets account tickets))
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

  ;each project participant has an unique key that identify him
  (defun get-participant-key (account: string project:string)
    (+ account project)
  )

  (defun get-participant-tickets (account:string project:string)
    (at "tickets" (get-participant account project))
  )

  (defun get-round-participants (project: string)
    (select participants-table (and? (where "win" (= false)) (where "project" (= project))))
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
          (enforce (and (>= now start) (< now end)) "Time slot mismatch")
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

  (defun run (project:string winners_per_round:integer queued:bool)
    (with-capability (IS_ADMIN)
      (with-read projects-table project
        {"round" := round, "winners" := winners}
        (let* (;(random ( draw-random-number ))
            (now (at "block-time" (chain-data)))
            (redeem_date (add-time now (hours ROUND_WARMUP_TIME)))
            (drawn_hashes (make-draw (create-hash-from-salt [round time project])))
            (input (map (wrap-template-item-into-list {"winners_per_round": winners_per_round, "project": project, "queued": queued, "round": (+ round 1)}) drawn_hashes))
            (round_winners (fold (+) [] (map (get-winners-for-hash) input)))
            (ranked (map (rank-winners round_winners project)  (enumerate 0 (- (length round_winners) 1)) ))
          )
          (map (insert-queue (+ round 1) queued) ranked)
          (update projects-table project
            {"winners": (+ winners round_winners),
              "start_date": redeem_date,
              "round": (+ round 1)}
          )
          round_winners
        )
      )
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
      (write queue-table (at 1 entry) {"round": round, "position": (if (= true queued) (at 0 entry) 0 )})

  )


  ;winners: [winner1, winner2, winner3] pos: 2
  ;[2, winner2]
  (defun rank-winners (winning:[string] project:string pos:integer)
            [pos (get-participant-key (at pos winning) project)]
  )
  (defun get-winners-for-hash (project-spot)
    (require-capability (IS_ADMIN))
    (let* (
         (spot-hash:string (at 1 project-spot))
         (project:string (at "project" (at 0 project-spot)))
         (winners_per_round:integer (at "winners_per_round" (at 0 project-spot)))
         (round:integer (at "round" (at 0 project-spot)))
         (queued:bool (at "queued" (at 0 project-spot)))
         (players (get-round-participants project))
         (first_to_win (/ winners_per_round 5))
         (input (map (+ project-spot) (map (wrap-list) players)))
         ; pick the users who were closest to their generated ticket
         (outcome (sort ["pos"] (map (process_player) (map (+ [project spot-hash]) (map (wrap-list) players)))))
         (winning (take first_to_win outcome))
      )
      ;mark wining users in db
      (map (mark-participant-as-winner) (map (at "key") (map ( at "player" )  winning)))
      (map (at "account") (map (at "player") winning))
    )
  )

  ;this is probably way to much hashing
  (defun create-hash-from-salt (salt)
    (hash (format "{}" [salt]))
  )

  (defun make-one-draw (salt:string)
    ;use make draw to generate a random sequence of numbers
    ;split into 5 batches
    ;select a random batch as drawn number
    (let* ((possible-tickets (make-draw salt))
        ;this might be overkill
        (random-outcome (at 0 possible-tickets))
      )
      random-outcome
    )
  )

  (defun process-ticket-with-salt (ticket_hash)
    (let* (
        (extracted_number (at 0 ticket_hash))
        (ticket (at 1 ticket_hash))
        (draw (abs (- ticket (str-to-int (at 0 extracted_number)))))
      )
      draw
    )
  )

  (defun process_player (spot_player)
    (require-capability (IS_ADMIN))
    (let* (
        (project (at 0 spot_player))
        (player (at 2 spot_player))
        (spot-hash (at 1 spot_player))
        ;user ticket lists might contain duplicate ticket entries
        (tickets_list (at "tickets-numbers" player))
        ;by comparing each user ticket with a fresh generate value we enforce fairness
        (t3 (map ( compose (compose(str-to-int) (wrap-template-item-into-list [spot-hash (at "account" player)])) (process-ticket-with-salt)) tickets_list))
        ;  (t4 (map (str-to-int (at 0))   t3))
        (tickets_position_list t3)
        (best_ticket (at 0 (sort tickets_position_list)))
      )
      { "spot-hash":spot-hash,
        "pos":best_ticket,
        "player": {"key": (+ (at "account" player) project),
          "project": project,
          "account": (at "account" player),
          "tickets": (at "tickets" player)}
        }
    )
  )
)


