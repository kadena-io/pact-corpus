(module poller GOVERNANCE

  ;; Dependencies
  (use util.guards [at-before-date at-after-date chain-time])
  (use n_7d195c48a309af310bc16ced11d156aff8105579.bonder [add-new-poll read-bond read-lockup get-lockup-key can-account-bond add-user-interaction])
  ;; General contract capabilities
  (defcap GOVERNANCE ()
    (enforce-guard
     (keyset-ref-guard "n_7d195c48a309af310bc16ced11d156aff8105579.bonder-admin")))

  (defcap OPS ()
    (enforce-guard
     (keyset-ref-guard "n_7d195c48a309af310bc16ced11d156aff8105579.bonder-ops")))

  (defcap INTERNAL ()
    @doc "Manages internal methods of the contract"
    true)

  (defcap ACCOUNT_GUARD
      (account:string)
    @doc "Look up the guard for an account, required to interact with the contract."
    (enforce-guard (at 'guard (coin.details account))))

  ;; Role capabilities
  (defcap BONDER
      (account:string
       bond-id:string)
    @doc "Manages privileges for bonder accounts"
    (compose-capability (ACCOUNT_GUARD account))
    (let ((is-bonder:bool (not (can-account-bond account bond-id))))
      (enforce is-bonder "This account isn't a bonder")))

  ;; Schemas and tables

  (defschema poll-option
      option-index: integer
      option-name: string
      votes-polling-power: decimal)

  (defschema poll
      @doc "Tracks proposals created by CAB Core Members"
      @model [
              (invariant (> election-end creation-time))
              ]
      title:string
      description:string
      author:string
      poll-id:string ;; same as index, stored for convenience
      creation-time:time
      bond-id: string
      options:[object{poll-option}]
      election-start:time
      election-end:time
      number-votes:integer
      votes-quorum:integer
      quorum:decimal
      )

  (deftable polls:{poll})

  (defschema poll-vote
      @doc "Tracks votes in a poll from bonder accounts"
      @model [
              (invariant (> polling-power 0.0))
              (invariant (!= [] bond-ids))
              (invariant (or (= action VOTE_APPROVED)
                             (or (= action VOTE_REFUSED)
                                 (= action VOTE_ABSTAIN))))
              ]
      poll-id:string
      account:string
      polling-power:decimal
      bond-id:string
      action:string
      date:time
      )

  (deftable poll-votes:{poll-vote})

  (defschema polling-parameters
      @doc "Tracks duration of each election phase"
      @model [
              (invariant (> review-period 0))
              (invariant (>= quorum-percentage 0.0))
              (invariant (> election-period 0))
              (invariant (>= 1.0 quorum-percentage))
              (invariant (>= quorum-votes-percentage 0.0))
              (invariant (>= 1.0 quorum-votes-percentage))
              ]
      election-period: integer
      review-period: integer
      quorum-percentage:decimal
      quorum-votes-percentage:decimal
      )

  (deftable polling-parameters-table:{polling-parameters})

  (defschema contract-lock-status
      lock:bool)
  (deftable contract-lock:{contract-lock-status})

  (defconst CONTRACT_LOCK_KEY 'lock)

  (defun enforce-contract-unlocked ()
    "Asserts that the contract is not in a paused state."
    (with-read contract-lock CONTRACT_LOCK_KEY { 'lock := lock }
               (enforce (not lock) "Contract is paused")))

  (defun set-contract-lock
      ( lock:bool )
    "Toggles contract's lock status"
    (with-capability (OPS)
      (write contract-lock CONTRACT_LOCK_KEY {'lock: lock })))

  ;; Poll management capabilities
  (defcap PERIOD
      (start:time
       end:time)
    @doc "Enforces current time is within the start and end times"
    (enforce-guard (at-after-date start))
    (enforce-guard (at-before-date end))
    )

  (defcap CREATE_POLL
      (account:string
       title:string
       bond-id:string
       description:string)
    @doc "Ability to create polls in the CAB"
    @event

    (let (
          (bond-creator (at 'creator (read-bond bond-id)))
          )
      (enforce (<= (length description) 500) "Description can be a maximum of 500 characters long")
      (enforce (<= (length title) 150) "Title can be a maximum of 150 characters long")
      (enforce (>= (length description) 10) "Description can be a minimum of 10 characters long")
      (enforce (>= (length title) 5) "Title can be a minimum of 5 characters long")
      (enforce (is-charset CHARSET_ASCII title) "ASCII characters only.")
      (enforce (is-charset CHARSET_ASCII description) "ASCII characters only.")
      (compose-capability (ACCOUNT_GUARD bond-creator))
      )
    )

  (defcap VOTE
      (account:string
       poll-id:string
       action:string)
    @doc "Permission to vote in polls with bonded KDA via bonder contract"
    @event

    (with-read polls poll-id
      {
       'election-end := end-time,
       'election-start := start-time,
       'bond-id := bond-id}
      (compose-capability (PERIOD start-time end-time))

      (let ((already-voted:bool (account-already-voted account poll-id))
            (can-vote:bool (can-account-vote account poll-id))
            )
        (compose-capability (BONDER account bond-id))
        (enforce can-vote "You should have an active lockup at poll creation time to vote")
        (enforce (not already-voted)
                 "This account has already voted on this poll"))))

  ;; Methods to interact with bonder contract
  (defcap BONDER_NOTIFY ()
    @doc "Manages interaction with bonder contract"
    true)

  (defun enforce-bonder-update () (require-capability (BONDER_NOTIFY)))

  (defun bonder-guard:guard () (create-user-guard (enforce-bonder-update)))

  (defun register-new-poll
      (bond-id:string)
    "Internal function to manage incrementing total polls in a bond"
    (require-capability (BONDER_NOTIFY))
    (add-new-poll bond-id))

  (defun register-poll-interaction
      (account:string
       poll-id:string)
    @doc "Internal function to manage incrementing interactions from a bonder"
    (require-capability (BONDER_NOTIFY))
    (add-user-interaction (at 'bond-id (read-poll poll-id)) account)
    )

  (defun can-account-vote:bool
      (account:string
       poll-id:string)
    @doc "Returns true if an account can vote in a specified poll."
    (let*
        (
         (poll (read-poll poll-id))
         (poll-creation:time (at 'creation-time poll))
         (bond (at 'bond-id poll))
         (lockup (read-lockup (get-lockup-key bond account)))
         (lockup-start-time:time (at 'lockup-start-time lockup))
         (has-claimed-lockup:bool (at 'has-claimed lockup))
         )
      (and (not has-claimed-lockup ) (>= poll-creation lockup-start-time))
      ))

  (defun get-polling-power:decimal
      (account:string
       poll-id:string)
    @doc "Retrieves bonder's polling power from a specified lockup.\
    \ Only lockups that were active at the time of poll creation will count."
    @model [(property (> result 0.0))]
    (let* (
           (bond (at 'bond-id (read-poll poll-id)))
           (lockup (read-lockup (get-lockup-key bond account))))
      (at 'polling-power lockup)
      )
    )


  (defun validate-option-name (option-name:string)
    @doc "Enforces the properties required for an option name"
    (enforce (is-charset CHARSET_ASCII option-name) "ASCII characters only")
    (enforce (>= (length option-name) 2) "Option name too short")
    (enforce (<= (length option-name) 30) "Option name too long")
    )


  (defun compose-poll-option:object{poll-option} (option-name:string  index:integer)
         @doc "Composes a new poll option given an option name and an index"
         (require-capability (INTERNAL))
         (validate-option-name option-name)
         {
          'option-index: index,
          'option-name: option-name,
          'votes-polling-power: 0.0
          }
         )

  (defun compose-poll-options:[object{poll-option}] (option-names:[string])
         @doc "Composes poll options from a list of names"
         (enforce (>= (length option-names) 2) "A poll needs to have at least 2 options")
         (let ((indexes (enumerate 0 (length option-names))))
           (zip (lambda (x y) (compose-poll-option x y)) option-names indexes)
           )
         )

  ;; Main contract entry points
  (defun create-poll:string
      (account:string
       title:string
       bond-id:string
       description:string
       option-names:[string]
       )

    @doc "Create a poll (CAB Core agents only)"
    (enforce-contract-unlocked)
    (with-capability (CREATE_POLL account title bond-id description)

      (let* ((now:time (chain-time))
             (n-polls:integer (+ 1 (polls-counter)))
             (poll-id:string (format "CAB-{}" [n-polls]))
             (start-time:time (add-time now (review-period)))
             (end-time:time  (add-time start-time (election-period)))
             (quorum:decimal (compute-poll-quorum bond-id))
             (votes-quorum:integer (compute-poll-votes-quorum bond-id))
             (is-bond-active:bool (> (at 'active-bonders (read-bond bond-id)) 0))
             )
        (enforce (>= 10 (length option-names)) "A poll cannot have more than 10 options")
        (enforce is-bond-active "You can only create polls for active bonds")

        (with-capability (INTERNAL)
          (let ((new-poll:object{poll}
                  {
                   'title: title,
                   'description: description,
                   'author: account,
                   'creation-time: now,
                   'bond-id: bond-id,
                   'poll-id: poll-id,
                   'election-start: start-time,
                   'election-end: end-time,
                   'options: (compose-poll-options option-names),
                   'quorum: quorum,
                   'number-votes: 0,
                   'votes-quorum: votes-quorum
                   })
                )
            (insert polls poll-id new-poll)
            )

          (increment-polls-counter))

        ;; increment number of polls in active bonds
        (with-capability (BONDER_NOTIFY)
          (register-new-poll bond-id)
          )


        (format "Poll {} - until {} has been created" [poll-id (format-time "%c" end-time)]))))


  (defun read-poll-options:[object{poll-option}] (poll-id:string)
         @doc "Read all the poll options"
         (at 'options (read-poll poll-id)))

  (defun read-poll-option:object{poll-option} (poll-id:string index:integer)
         @doc "Read the poll option with the given index"
         (let
             (
              (options:[object{poll-option}] (filter (where 'option-index (= index)) (read-poll-options poll-id)))
              )
           (enforce (= 1 (length options)) "Option not found")
           (at 0 options)
           ))

  (defun update-poll-option:[object{poll-option}] (poll-id option-index vp)
         @doc "Update the option with the given index and add vp to the votes"
         (let*
             (
              (options (read-poll-options poll-id))
              (old-option (read-poll-option  poll-id option-index))
              (new-option {
                           'option-index: option-index
                           , 'option-name: (at 'option-name old-option)
                           , 'votes-polling-power: (+ (at 'votes-polling-power old-option) vp)
                           } )
              )
           (map (lambda (x) (if (= x old-option) new-option x)) options)
           )
         )

  (defun vote:string
      (account:string
       poll-id:string
       option-index:integer)

    @doc "Vote in a poll selecting the option via index. Only Bonders can perform this action."

    (enforce-contract-unlocked)
    (let* (
           (poll-option (read-poll-option poll-id option-index))
           (bond-id:string (at 'bond-id (read-poll poll-id)))
           (vp (get-polling-power account poll-id))
           )
      (with-capability (VOTE account poll-id (at 'option-name poll-option))
        (enforce (> vp 0.0)
                 "This account does not have polling power")
        (update polls poll-id
                {'options: (update-poll-option poll-id option-index vp)})

        (with-read polls poll-id {
                                  'number-votes := number-votes
                                  }
                   (update polls poll-id
                           {'number-votes: (+ number-votes 1)})
                   )

        (insert poll-votes (votes-table-key account poll-id)
                {
                 'poll-id: poll-id,
                 'account :account,
                 'polling-power: vp,
                 'bond-id: bond-id,
                 'action: (at 'option-name poll-option),
                 'date: (chain-time)
                 })
        )
      )

    (with-capability (BONDER_NOTIFY)
      (register-poll-interaction account poll-id))

    )


  (defun read-poll-results:string
      (poll-id:string)
    @doc "Returns the name of the winning option for a given poll. Returns \"Rejected by quorum\" if the quorum is not met."
    (with-read polls poll-id
      {
       'election-end := end-time,
       'options :=  poll-options,
       'quorum := quorum,
       'number-votes := votes,
       'votes-quorum := votes-quorum
       }
      (enforce-guard (at-after-date end-time))
      (let (
            (votes-quorum-passed (>= votes votes-quorum))
            (quorum-passed (>= (fold (+) 0.0 (map  (at 'votes-polling-power) poll-options)) quorum))
            (most-voted-option:object{poll-option} (at 0 (reverse (sort ['votes-polling-power] poll-options))))
            )
        (if (and quorum-passed votes-quorum-passed) (at 'option-name most-voted-option) "Rejected by quorum")
        )
      ))

  (defun read-account-created-polls:[object{poll}]
    (account:string)
    @doc "Retrieve all polls created by a specified account"
    (select polls (where 'author (= account)))
    )

  (defun read-active-polls:[object{poll}] ()
         @doc "Retrieve all active polls"
         (select polls (where 'election-end (< (chain-time)))))

  (defun read-all-polls:[object{poll}] ()
         @doc "Retrieve all active polls"
         (select polls (constantly true)))

  (defun account-already-voted:bool
      (account:string
       poll-id:string)
    @doc "Check if an account has already voted in a specified poll"

    (with-default-read poll-votes (votes-table-key account poll-id)
      {'account:""}
      {'account:=account}
      (not (= account ""))))

  (defun votes-table-key
      (account:string
       poll-id:string)
    @doc "Creates an id for inserting and updating poll-votes table"
    (format "{}-{}" [account poll-id]))

  (defun read-poll-votes:[object{poll-vote}]
    (poll-id:string)
    @doc "Retrieve all votes in a specified poll"
    (select poll-votes (where 'poll-id (= poll-id))))

  (defun read-all-account-votes:[object{poll-vote}]
    (account:string)
    @doc "Retrieve all poll votes for the specified account"
    (select poll-votes (where 'account (= account))))

  (defun read-poll-vote:object{poll-vote}
    (account:string
     poll-id:string)
    @doc "Retrieve a votes in a specified poll. Fails if account not voted in poll"
    (read poll-votes (votes-table-key account poll-id)))

  (defun read-poll:object{poll}
    (poll-id:string)
    @doc "Reads a poll from the polls table"
    (read polls poll-id))

  (defconst polling-params-key:string "cab-poll-parameters")

  (defun edit-polling-parameters
      (params:object{polling-parameters})
    @doc "Operator-only method to edit the durations of Voting periods."
    (enforce-contract-unlocked)
    (with-capability (OPS)
      (enforce (> (at 'election-period params) 0) "Election period must be positive")
      (enforce (> (at 'review-period params) 0) "Review period must be positive")
      (enforce (and (>= (at 'quorum-percentage params) 0.0) (>= 1.0 (at 'quorum-percentage params))) "Quorum percentage must be a number between 0.0 and 1.0")
      (enforce (and (>= (at 'quorum-votes-percentage params) 0.0) (>= 1.0 (at 'quorum-votes-percentage params))) "Quorum percentage must be a number between 0.0 and 1.0")
      (write polling-parameters-table polling-params-key params)))

  (defun election-period:integer ()
    @doc "Returns the duration (seconds) of the election period"
    (at 'election-period (read polling-parameters-table polling-params-key)))

  (defun quorum-percentage:decimal ()
    @doc "Returns the percentage of votes required to approve a proposal"
    (at 'quorum-percentage (read polling-parameters-table polling-params-key))
    )

  (defun quorum-votes-percentage:decimal ()
    @doc "Returns the number of votes required to approve a proposal"
    (at 'quorum-votes-percentage (read polling-parameters-table polling-params-key))
    )

  (defun review-period:integer ()
    @doc "Returns the duration (seconds) of the election period"
    (at 'review-period (read polling-parameters-table polling-params-key)))

  (defun compute-poll-quorum:decimal (bond-id:string)
    @doc "Returns the VP required to approve a proposal from a specified bond"
    (* (quorum-percentage) (at 'total-vp (read-bond bond-id)))
    )

  (defun compute-poll-votes-quorum:integer (bond-id:string)
    @doc "Returns the number of votes required to approve a proposal from a specified bond"
    (ceiling (* (quorum-votes-percentage) (dec (at 'active-bonders (read-bond bond-id))))
             ))


  (defschema poll-counter
      total-polls:integer )

  (deftable poll-counter-table:{poll-counter})

  (defconst POLL_COUNTER_KEY:string "poller-vars")

  (defun polls-counter:integer ()
    (with-read poll-counter-table  POLL_COUNTER_KEY {'total-polls:=t} t)
    )

  (defun increment-polls-counter ()
    (require-capability (INTERNAL))
    (update poll-counter-table POLL_COUNTER_KEY { 'total-polls: (+ 1 (polls-counter)) })
    )

  )


