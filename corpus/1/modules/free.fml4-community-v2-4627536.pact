(module fml4-community-v2 'admin-multi-keyset
  (defcap IS_ADMIN ()
    (enforce-keyset 'admin-1))

  (defconst DRAW_TICKET_LENGTH 5)

  (defschema user
    account:string
    kyc_status:integer ;0 - none; 1 - in progress; 2 - it's done; 3 - has error
  )

  (deftable users-table:{user})

  (defschema project
    slug:string
    kyc_needed:bool

    round:integer
    whitelist_open:bool  
    round_winners:[string]  
    whitelist_close_date:time
    whitelist_close_block_height:string
    whitelist_close_previous_block_height:string
    whitelist_close_tx_hash:string
    fcfs:bool

  )
  (deftable projects-table:{project})

  (defschema users-in-projects
    account:string
    project:string
    tickets:integer
    ga:integer
    ga_wins:integer
  )

  (deftable users-in-projects-table:{users-in-projects})

  (defun add-project(slug:string kyc_needed:bool whitelist_open:bool)
    @doc "Add project to project table                               \
    \ param project:string - unique id of the project to add         \
    \ param slug:string - the slug of the project (should be unique) \
    \ param kyc_needed:bool - true if the project need kyc)          \
    \ return void                                                    \
    \ e.g: project('688787d8ff144c502c7f5cffaafe2cc588d86079f9de88304c26b0cb99ce91c6' 'my-project')"

    (with-capability (IS_ADMIN)
        (insert projects-table slug 
          { 
            "slug": slug, 
            "kyc_needed": kyc_needed, 
            "whitelist_open": whitelist_open,
            "whitelist_close_block_height": "",
            "whitelist_close_previous_block_height": "",
            "whitelist_close_date": (at "block-time" (chain-data)),
            "whitelist_close_tx_hash":"",
            "round_winners": [],
            "round": 1,
            "fcfs":false 
          }
          ; TODO: Emit event
          ; (emit-event (ADD_PROJECT project slug kyc_needed ))
        )
    )
  )

  (defun set-project-round (project:string round:integer)
    (with-capability (IS_ADMIN)
      (update projects-table project {"round": round})
    )
  )

  (defun get-project-round (project:string)
     (at "round" (read projects-table project))
  )

  (defun add-user-to-project(project:string account:string)
    @doc "Add a user to a projectsInTables table and also add the account in user table if not exist  \
    \ param project:string - the id of the project (uuid-v4)                                          \
    \ param account:string - the id of the user account                                               \
    \ param return void                                                                               \
    \ e:g add-user-to-project('78d7e113-1e54-42f0-91f2-e83fb38b89b0', 'K:asda123asdasd12')"
    (with-capability (ISSUE_EVENT "add-user-to-project" [project, account]) true)
    (with-capability (IS_ADMIN)
      ; find add create user if not exist
      (with-default-read users-table account { "kyc_status": -1 } { "kyc_status":=kyc_status}
        (if (= kyc_status -1)
          (write users-table account {
            "kyc_status": 0,
            "account": account
            })

          "User already exist"
        )
      )

      ; write the user in project table if no exist
      (with-default-read  users-in-projects-table (get-user-to-project-key project account)
        {"tickets": -1}
        {"tickets":=current_tickets_ammount}  
        (if (= current_tickets_ammount -1)
          (write users-in-projects-table (get-user-to-project-key project account) {
            "account": account,
            "project": project,
            "tickets":0,
            "ga":0,
            "ga_wins": 0
          })

      ;      ; TODO: Add event
          ;  (emit-event (ADD_PROJECT_ON_USER (get-user-to-project-key(project account))))
          "User is assigned already to this project"
        )
      )
    )
  )

  (defun get-user(account:string)
    (read users-table account)
  )

  (defun add-rewards(project:string account:string  tickets:integer ga:integer ga_wins:integer)
    (with-capability (ISSUE_EVENT "add-rewards" [project, account, tickets, ga, ga_wins]) true)
    (with-capability (IS_ADMIN)
      ; check paramenters
      (enforce (>= tickets 0) "can't add negative amount")
      (enforce (>= ga 0) "can't add negative ga")
      (enforce (>= ga_wins 0) "can't add negative ga_wins")

      (if (or (> tickets 0) (or (> ga 0) (> ga_wins 0)) ) 
        (with-read users-in-projects-table (get-user-to-project-key project account) {
          "ga" := current_ga_ammount,
          "ga_wins" := current_ga_wins_ammount,
          "tickets" := current_tickets_ammount }
          
          (update users-in-projects-table (get-user-to-project-key project account) {
            "ga": (+ ga current_ga_ammount),
            "ga_wins" : (+ ga_wins current_ga_wins_ammount),
            "tickets": (+ tickets current_tickets_ammount)
          })
          true
        )
        false
      )   
    )
  )
  

  (defun get-user-to-project-key:string (project:string account:string)
    @doc "Return the key (hash) of the project and userId                                       \
    \ param project:string - project slug                                                       \
    \ param account:string - user id                                                               \
    \ return hash:string - the hash of the input                                                \
    \ e.g get-user-to-project-key('78d7e113-1e54-42f0-91f2-e83fb38b89b0', 'K:asda123asdasd12')  \
    \ return Mk3PAn3UowqTLEQfNlol6GsXPe-kuOWJSCU0cbgbcs8'"

    (hash [project account] )
  )
  
  (defun user-details:object( account:string )
    @doc "Return user details of an account \
    \param account: string                  \
    \return object with user information    \
    \e.g: user-details"

    (with-default-read users-table account
      {"kyc_status": 0}
      { "kyc_status" := kyc_status }
      { "kyc_status": kyc_status }
    )
  )

  (defun get-kyc (account:string)
    @doc "Return kyc_status \
    \param account:string - user id \
    \return 0 - none; 1 - in progress; 2 - it's done; 3 - has error \
    \e.g:  (get-kyc 'K:asda123asdasd12') \
    \return: 0"
    (at "kyc_status" (user-details account))
  )
  
  (defun set-kyc(account:string kyc_status:integer)
    @doc "Return kyc_status \
    \param account:string - user id \
    \param key_status:integer -kyc-status 0,1,2 \ 
    \e.g:  (set-kyc 'K:asda123asdasd12' 2)"

    (with-capability (IS_ADMIN)
      (write users-table account {"kyc_status": kyc_status, "account": account})
    )
  )

  (defun has-kyc(project:string account:string)
    (with-read projects-table project {
      "kyc_needed":=current_kyc_needed}
      (if (= current_kyc_needed false)
        true 
        (= (get-kyc account) 2)
      )
    )
  )
  
  (defun enforce-has-kyc (project:string account:string)
    (enforce (= (get-kyc account) 2) "Has kyc")
  )

  ;lottery

  (defun wrap-list(item)
    [item]
  )

  (defun get-participants-keys (project:string)
    (map (get-user-to-project-key project) (map (at "account") (select users-in-projects-table ["account"] (where "project" (= project)))))
  )


  (defun set-round-winners (project:string winners:[string])
    (with-capability (IS_ADMIN)
      (update projects-table project {
          "round_winners": winners
      })
    )
  )

  (defun create-hash-from-salt (salt)
    (hash (format "{}" [salt]))
  )


  (defun get-participant-with-tickets (participant_key:string)
    (get-participant participant_key)
  )

  (defun get-participant (project: string account:string)
    (read users-in-projects-table (get-participant-key project account))
  )


  (defun get-project (project:string)
        (read projects-table project)
  )


  (defun is-round-winner (project:string account:string)
    (let* ((project_entry (get-project project)))
          (and (has-kyc project account)  (or (contains account (at "round_winners" project_entry)) (at "fcfs" project_entry)) )
    )
  )



  (defun close-whitelist (project:string)
    (with-capability (IS_ADMIN)
        (update projects-table project {
            "whitelist_open": false,
            "whitelist_close_block_height": (format "{}" [(at "block-height"(chain-data))]),
            "whitelist_close_previous_block_height": (at "prev-block-hash" (chain-data)),
            "whitelist_close_date": (at "block-time" (chain-data)),
            "whitelist_close_tx_hash": (tx-hash)
        }) 
    )
  )


  (defun set-project-fcfs (project:string fcfs:bool)
    (with-capability (IS_ADMIN)
      (update projects-table project {
          "fcfs": fcfs
      })
    )
  )

  (defun set-project-whitelist-status (project:string whitelist_open:bool)
  ;todo: is admin
  (with-capability (IS_ADMIN)
      (update projects-table project {
          "whitelist_open": whitelist_open
      })
    )
  )


  ;todo replace everywhere with get-user-to-project-key
  (defun get-participant-key (project:string account:string)
    (get-user-to-project-key project account)
  )

  ;return array of randomized tickets number
  ;ex [123,5363,67443]
  (defun get-participant-lottery-tickets (project account:string tickets:integer )
        (let* (
                (empty_tickets_arr (map (wrap-list) (enumerate 1 tickets)))
                (tickets_list (map (make-draw project) (map (create-hash-from-salt) (map (+ [tickets account project]) empty_tickets_arr)))))
                (map (lambda (ticket) {"account": account, "ticket":ticket}) tickets_list)
        )
  )

  (defun get-participants (project:string winners_amount:integer)
            (let*  (
                    (project_entry (get-project project))
                    (participants (map (get-participant-with-tickets) (get-participants-keys project)))
                    (participants_lottery_tickets_objects (map (lambda (participant)     (get-participant-lottery-tickets project_entry (at "account" participant) (at "tickets" participant)  )) participants))
                    (participants_lottery_tickets (fold (+) [] participants_lottery_tickets_objects))
                    (participants_lottery_tickets_sorted (take winners_amount (sort ["ticket"] participants_lottery_tickets)))
                    (winners_account_array (map (at "account") participants_lottery_tickets_sorted))
                    (participants_processed (map (lambda (participant) {
                        "account": (at "account" participant),
                        "project": (at "project" participant),
                        "tickets": (at "tickets" participant),
                        "wining_tickets": (map (at "ticket") (filter (where "account" (= (at "account" participant) )) participants_lottery_tickets_sorted)),
                        "ga": (at "ga" participant),
                        "ga_wins": (length (filter (= (at "account" participant)) winners_account_array))
                    }) participants))
                )
                participants_processed
            )
    )



  (defun make-draw (project salt: string)
    (let* (;(block-time-hash (hash (at "whitelist_close_date" project)))
        (prev-block-hash (at "whitelist_close_previous_block_height" project))
        (block-height-hash (at "whitelist_close_block_height" project))
        (master-hash-int (str-to-int 64 (hash (+ block-height-hash (+  prev-block-hash (create-hash-from-salt [salt (at "whitelist_close_tx_hash" project)]))))))
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
        pos-four
    )
  )

  (defconst TICKETS_PRICE [1.00,1.00,265.00,305.00,350.00,405.00,465.00])
  (defconst GA_PRICE [1.00,1.00,1490.00,1820.00,2220.00])
 
  ;todo: how to protect this with guards?
  (defun add-rewards-for-staking (project:string account:string amount:decimal tickets:integer ga:integer)
        ;  (with-capability (ISSUE_EVENT "add-rewards-for-staking" [project, account, amount, tickets, ga]) true)
        (let* (
              (locking-contract:module{callable} (fml-callable-contracts.get "fml-hype-locker"))
              (total_price (get-hype-amount-for-rewards tickets ga))
            )
            ;  (enforce-guard (locking-contract::call "GET_GUARD" []))
            ;  (enforce (= total_price amount) (format "Rewards not matching {} {}" [amount, total_price]))
            (with-default-read users-in-projects-table (get-user-to-project-key project account) 
              {
                "ga": 0,
                "tickets": 0,
                "ga_wins": 0,
                "account": account,
                "project": project
              }
              {
                "ga" := current_ga_ammount,
                "tickets" := current_tickets_ammount,
                "ga_wins" := ga_wins,
                "account" := initial_account,
                "project" := initial_project
              }
              (write users-in-projects-table (get-user-to-project-key project account) {
                "ga": (+ ga current_ga_ammount),
                "tickets": (+ tickets current_tickets_ammount),
                "account": initial_account,
                "project": initial_project,
                "ga_wins": 0
              })
              true
            )
        )
  )

  (defun get-hype-amount-for-rewards (tickets:integer ga:integer)
      (let* (
        (tickets_price (fold (+) 0.00 (take tickets TICKETS_PRICE)))
        (ga_price (fold (+) 0.00 (take ga GA_PRICE)))
        (total_price (+ tickets_price ga_price))
      )
      total_price
    )
  )

  (defcap ISSUE_EVENT(name:string paramenters)
        @event
        true
  )


  (defun get-participant-ga (project:string account:string)
      (with-default-read users-in-projects-table (get-participant-key project account) {"ga": 0, "ga_wins": 0} {"ga":=ga, "ga_wins":=ga_wins}
          (+ ga ga_wins)
      )
  )

)


