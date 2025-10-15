(module poller GOVERNANCE

  ;; Dependencies
  (use util.guards [at-before-date at-after-date chain-time])
  (use n_7d195c48a309af310bc16ced11d156aff8105579.bonder [add-new-poll get-bond-lockups get-account-active-lockups get-account-lockups read-bond get-active-bond-ids is-account-bonded is-core-account is-bonder-account add-user-interaction])
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
      (account:string)
    @doc "Manages privileges for bonder accounts"
    (compose-capability (ACCOUNT_GUARD account))
    (let ((is-bonder:bool (is-bonder-account account)))
      (enforce is-bonder "This account isn't a bonder")))

  (defcap CORE_MEMBER
      (account:string)
    @doc "Manages privileges for core accounts"
    (compose-capability (ACCOUNT_GUARD account))
    (let ((is-core:bool (is-core-account account)))
      (enforce is-core "This account isn't a core member")))

  ;; Schemas and tables
  (defschema poll
      @doc "Tracks proposals created by CAB Core Members"
      @model [
              (invariant (>= votes-yes 0.0))
              (invariant (>= votes-no 0.0))
              (invariant (>= votes-abstentions 0.0))
              (invariant (> election-end creation-time))
              ]
      title:string
      description:string
      author:string
      poll-id:string ;; same as index, stored for convenience
      creation-time:time ;; also endorse-start
      bond-ids:[string]
      ;; consensus review attributes
      votes-yes:decimal
      votes-no:decimal
      votes-abstentions:decimal
      election-start:time
      election-end:time
      number-votes:integer
      votes-quorum:integer
      quorum:decimal
      )

  (deftable polls:{poll})

  (defschema poll-vote
      ;; invariants: poll-id must exist
      ;; account should be valid
      @doc "Tracks votes in a poll from bonder accounts"
      @model [
              (invariant (> polling-power 0.0))
              (invariant (!= [] bond-ids))
              (invariant (or (= action VOTE_APPROVED) (or (= action VOTE_REFUSED) (=
                                                                                   action VOTE_ABSTAIN))))
              ]
      poll-id:string
      account:string
      polling-power:decimal
      bond-ids:[string]
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
  (defun is-contract-locked ()
    "Asserts that the contract is not in a paused state."
    (with-read contract-lock CONTRACT_LOCK_KEY { 'lock := lock }
               lock))
  (defun set-contract-lock
      ( lock:bool )
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
       description:string)
    @doc "Ability to create polls in the CAB"
    @event

    (enforce (<= (length description) 500) "Description can be a maximum of 500 characters long")
    (enforce (<= (length title) 150) "Title can be a maximum of 150 characters long")
    (enforce (>= (length description) 10) "Description can be a minimum of 10 characters long")
    (enforce (>= (length title) 5) "Title can be a minimum of 5 characters long")
    (enforce (is-charset CHARSET_ASCII title) "ASCII characters only.")
    (enforce (is-charset CHARSET_ASCII description) "ASCII characters only.")
    (compose-capability (CORE_MEMBER account))
    )

  (defcap VOTE
      (account:string
       poll-id:string
       action:string)
    @doc "Permission to vote in polls with bonded KDA via bonder contract"
    @event

    (compose-capability (BONDER account))

    (with-read polls poll-id
      { 'election-end := end-time
                      , 'election-start := start-time
                      , 'bond-ids := bond-ids}
      (compose-capability (PERIOD start-time end-time))

      (let ((already-voted:bool (account-already-voted account poll-id))
            (can-vote:bool (can-account-vote account poll-id))
            )
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

  (defun increment-active-bonds
      (active-bonds: [string])
    @doc "Internal function to increment all active bonds when a poll is promoted to Final Election phase"
    (map (register-new-poll) active-bonds)
    )

  (defun register-poll-interaction
      (account:string
       poll-id:string)
    @doc "Internal function to manage incrementing interactions from a bonder"
    (require-capability (BONDER_NOTIFY))
    (let*
        (
         (poll (read-poll poll-id))
         (creation-time (at 'creation-time poll))
         (user-lockups (filter (lambda (x)
                                 (and (= (at 'status x) "locked")
                                      (and (>= creation-time (at 'lockup-start-time x))
                                           (>= (at 'lockup-end-time x) creation-time))))
                               (get-account-lockups account)))
         )
      (map (add-user-interaction) (map (lambda (x) (at 'lockup-id x)) user-lockups))
      )

    )

  (defun can-account-vote:bool
      (account:string
       poll-id:string)
    @doc "Returns true if an account can vote in a specified poll."
    (let* (
           (poll (read-poll poll-id))
           (bond-ids (at 'bond-ids poll))
           (creation-time (at 'creation-time poll))
           (already-voted (account-already-voted account poll-id))
           (user-lockups (filter (lambda (x)
                                   (and (>= creation-time (at 'lockup-start-time x))
                                        (>= (at 'lockup-end-time x) creation-time)))
                                 (get-account-lockups account)))
           (user-poll-lockups (filter (lambda (x) (contains (at 'bond-id x) bond-ids)) user-lockups))
           )
      (and (> (length user-poll-lockups) 0) (not already-voted))
      )
    )

  (defun get-polling-power:decimal
      (account:string
       poll-id:string)
    @doc "Retrieves bonder's polling power. The polling power is the \
  \ sum of all the polling powers coming from bonds associated with the poll. Only lockups \
  \ that were active at the time of poll creation will count."
    @model [(property (> result 0.0))]
    (let* (
           (poll (read-poll poll-id))
           (bond-ids (at 'bond-ids poll))
           (creation-time (at 'creation-time poll))
           (user-lockups (filter (lambda (x)
                                   (and (>= creation-time (at 'lockup-start-time x))
                                        (>= (at 'lockup-end-time x) creation-time)))
                                 (get-account-lockups account)))
           (user-poll-lockups (filter (lambda (x) (contains (at 'bond-id x) bond-ids)) user-lockups))
           )
      (enforce (!= [] user-poll-lockups) "Must have a lockup associated with the poll at creation time to vote")
      (fold (+) 0.0 (map (at 'polling-power) user-poll-lockups))
      ))

  (defun get-max-polling-power:decimal
      (account:string)
    @doc "Retrieves bonder's maximum polling power. It's what will be used on new polls."
    (let* (
           (user-lockups (get-account-active-lockups account))
           )
      (fold (+) 0.0 (map (at 'polling-power) user-lockups))
      ))

  ;; Main contract entry points
  (defun create-poll:string
      (account:string
       title:string
       description:string)
    @doc "Create a poll (CAB Core agents only)"
    (enforce-contract-unlocked)
    (with-capability (CREATE_POLL account title description)

      (let* ((now:time (chain-time))
             (n-polls:integer (+ 1 (length (read-all-polls))))
             (poll-id:string (format "CAB-{}" [n-polls]))
             (active-bonds:[string] (get-active-bond-ids))
             (start-time:time (add-time now (review-period)))
             (end-time:time  (add-time start-time (election-period)))
             (quorum:decimal (compute-poll-quorum active-bonds))
             (votes-quorum:integer (compute-poll-votes-quorum active-bonds))
             )

        (enforce (> (length active-bonds) 0) "No active bond sales found.")

        (let ((new-poll:object{poll}
                {'title: title
                         ,                 'description: description
                         ,                 'author: account
                         ,                 'creation-time: now
                         ,                 'bond-ids: active-bonds
                         ,                 'poll-id: poll-id
                         ,                 'election-start: start-time
                         ,                 'votes-yes: 0.0
                         ,                 'votes-no: 0.0
                         ,                 'votes-abstentions: 0.0
                         ,                 'election-end: end-time
                         ,                 'quorum: quorum
                         ,                 'number-votes: 0
                         ,                 'votes-quorum: votes-quorum
                         }))

          (insert polls poll-id new-poll)
          )

        ;; increment number of polls in active bonds
        (with-capability (BONDER_NOTIFY)
          (increment-active-bonds active-bonds)
          )

        (format "Poll {} - until {} has been created" [poll-id (format-time "%c" end-time)]))))

  (defun poll-vote-helper:string
      (account:string
       poll-id:string
       action:string
       polling-power:decimal)
    @doc "Helper function for inserting vote of an account on a poll"
    (require-capability (INTERNAL))

    (enforce (> polling-power 0.0) "You cannot vote")

    (let ((bond-ids:[string] (at 'bond-ids (read-poll poll-id))))

      (with-read polls poll-id {
                                'number-votes := number-votes
                                }
                 (update polls poll-id
                         {'number-votes: (+ number-votes 1)})
                 )

      (with-capability (BONDER_NOTIFY)
        (register-poll-interaction account poll-id))

      (insert poll-votes (votes-table-key account poll-id) {
                                                            'poll-id: poll-id
                                                            ,'account :account
                                                            ,'polling-power: polling-power
                                                            ,'bond-ids: bond-ids
                                                            ,'action: action
                                                            ,'date: (chain-time)
                                                            })


      ))

  (defconst VOTE_APPROVED "approved")
  (defconst VOTE_REFUSED "refused")
  (defconst VOTE_ABSTAIN "abstention")

  (defun vote-approved:string
      (account:string
       poll-id:string)
    @doc "Vote in a poll as approved. Only Bonders can perform this action."
    (enforce-contract-unlocked)
    (with-capability (VOTE account poll-id VOTE_APPROVED)
      (with-capability (INTERNAL)
        (let (
              (vp (get-polling-power account poll-id))
              )
          (enforce (> vp 0.0)
                   "This account does not have polling power")
          (with-read polls poll-id {
                                    'votes-yes := tot-approved
                                    }
                     (update polls poll-id
                             {'votes-yes: (+ tot-approved (floor vp 2))})
                     )
          (poll-vote-helper account poll-id VOTE_APPROVED vp)
          (format "Account {} APPROVED the '{}' poll" [account poll-id])
          )
        )
      )
    )


  (defun vote-refused:string
      (account:string
       poll-id:string)
    @doc "Vote in a poll as refused. Only Bonders can perform this action."
    (enforce-contract-unlocked)
    (with-capability (VOTE account poll-id VOTE_REFUSED)
      (with-capability (INTERNAL)
        (let (
              (vp (get-polling-power account poll-id))
              )
          (enforce (> vp 0.0)
                   "This account does not have polling power")
          (with-read polls poll-id {
                                    'votes-no := tot-refused
                                    }
                     (update polls poll-id
                             {'votes-no: (+ tot-refused (floor vp 2))})
                     )
          (poll-vote-helper account poll-id VOTE_REFUSED vp)
          (format "Account {} REFUSED the '{}' poll" [account poll-id])
          )
        )
      )
    )


  (defun vote-abstain:string
      (account:string
       poll-id:string)
    @doc "Abstain from voting in a poll. Only Bonders can perform this action."
    (enforce-contract-unlocked)
    (with-capability (VOTE account poll-id VOTE_ABSTAIN)
      (with-capability (INTERNAL)
        (let (
              (vp (get-polling-power account poll-id))
              )
          (enforce (> vp 0.0)
                   "This account does not have polling power")
          (with-read polls poll-id {
                                    'votes-abstentions := tot-abs
                                    }
                     (update polls poll-id
                             {'votes-abstentions: (+ tot-abs (floor vp 2))})
                     )
          (poll-vote-helper account poll-id VOTE_ABSTAIN vp)
          (format "Account {} ABSTAINED from '{}' poll" [account poll-id])
          )
        )
      )
    )


  (defun is-poll-approved:bool
      (poll-id:string)
    @doc "Returns true if a poll has been approved by election. Fails if poll is not finished"
    (with-read polls poll-id
      {'election-end := end-time
                     , 'votes-abstentions := abstentions
                     , 'votes-yes := yes
                     , 'votes-no := no
                     , 'quorum := quorum
                     , 'number-votes := votes
                     , 'votes-quorum := votes-quorum }
      (enforce-guard (at-after-date end-time))
      (and (>= votes votes-quorum)
           (and (> yes no) (>= (+ (+ yes no) abstentions) quorum))
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
    (at 'quorum-percentage (read polling-parameters-table polling-params-key))
    )

  (defun quorum-votes-percentage:decimal ()
    (at 'quorum-votes-percentage (read polling-parameters-table polling-params-key))
    )

  (defun review-period:integer ()
    @doc "Returns the duration (seconds) of the election period"
    (at 'review-period (read polling-parameters-table polling-params-key)))

  (defun compute-poll-quorum:decimal (active-bonds:[string])
    (let* (
           (bonds (map (read-bond) active-bonds))
           (vps (map (lambda (x) (at 'total-vp x)) bonds))
           )
      (* (quorum-percentage) (fold (+) 0.0 vps))
      )
    )

  ;; active-bonds -> all lockups -> accounts -> distinct
  (defun compute-poll-votes-quorum:integer (active-bonds:[string])

    (let* (
           (all-poll-lockups (fold (+) [] (map (get-bond-lockups) active-bonds)))
           (poll-accounts (distinct (map (take ["account"]) all-poll-lockups)))
           )
      (ceiling (* (quorum-votes-percentage) (dec (length poll-accounts))))
      )

    )


  )



