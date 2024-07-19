(module ecko-dao GOVERNANCE
  (implements gov-space-v1)
                                        ; ------------------ Tables ------------------------------------

  (deftable proposals-table:{gov-space-v1.proposal})
  (deftable votes-table:{gov-space-v1.vote})
  (deftable snapshot-table:{gov-space-v1.snapshot})
  (deftable current-snapshot-total-table:{gov-space-v1.current-snapshot-total})

                                        ; ------------------ Constants ---------------------------------

  ;; used for excluding too old snapshots
  (defconst MAX_SNAPSHOT_DURATION_PERIOD (days 10))
  ;; max proposal debate period
  (defconst MAX_PROPOSAL_DEBATE_PERIOD (days 14))
  ;; approved vote action constant
  (defconst VOTE_APPROVED "approved")
  ;; refused vote action constant
  (defconst VOTE_REFUSED "refused")
  ;; current-snapshot-total table key
  (defconst CURRENT-SNAPSHOT-TOTAL-KEY "current-snapshot-total")

                                        ; ------------------ Capabilities -----------------------------

  (defcap GOVERNANCE ()
    (enforce-guard
       (keyset-ref-guard "n_a8dcd1dcdb7454dc6f29828feff9cd86c6546d12.eckodao-gov")))

   (defcap OPS ()
     (enforce-guard
      (keyset-ref-guard "n_a8dcd1dcdb7454dc6f29828feff9cd86c6546d12.eckodao-ops")))

  (defcap INTERNAL ()
    "mark some functions as internal only"
    true)

  (defcap ACCOUNT_GUARD ( account:string )
    @doc " Look up the guard for an account"
    (enforce-guard (at 'guard (kaddex.kdx.details account))))

    (defcap CREATE_PROPOSAL
      (title:string 
       description:string 
       owner-account:string 
       start-date:time 
       end-date:time 
       minimum-quorum-percentage:decimal 
       majority-percentage:decimal 
       epoch-time:time
       )
    @doc "Ability to create proposal"
    @event
    (enforce (< (curr-time) start-date) "Start Date shouldn't be in the past")
    (enforce (< start-date end-date) "End Date should be later than start date")
    (enforce (< (diff-time end-date start-date) MAX_PROPOSAL_DEBATE_PERIOD)
               "proposal period is too long")
    (enforce (!= "" title) "Title should be declared")
    (enforce (!= "" description) "Description should be declared")
    (enforce (is-charset CHARSET_ASCII title) "ASCII characters only.")
    (enforce (is-charset CHARSET_ASCII description) "ASCII characters only.")
    (enforce (= "k:" (take 2 owner-account)) "only k: accounts allowed")
    (enforce (and (>= minimum-quorum-percentage 0.0) (<= minimum-quorum-percentage 1.0)) "Minimum quorum percentage must be between 0.0 and 1.0")
    (enforce (and (>= majority-percentage 0.0) (<= majority-percentage 1.0)) "Majority percentage must be between 0.0 and 1.0")
    (enforce (>= epoch-time end-date) "Epoch time must be gte end date")

    (compose-capability (OPS))
    )

    (defcap VOTE_PROPOSAL
      (account:string
      proposal-id:string)
      @doc "Ability to vote on a proposal"
      @event
      (compose-capability (ACCOUNT_GUARD account))
      (let (
        (proposal-exists (proposal-exists proposal-id))
        (account-already-voted (check-account-voted account proposal-id))
        (timestamp (curr-time))
        )
          (enforce proposal-exists "Proposal Id does not exists")
          (enforce (not account-already-voted)
                   "This account has already voted on this proposal")
          (with-read proposals-table proposal-id {
                    "start-date":=start-date,
                    "end-date":=end-date,
                    "account" := creator-account
                    }
            (enforce (>= timestamp start-date) "The proposal is not yet open")
            (enforce (<= timestamp end-date) "The proposal is closed")
            (enforce (!= creator-account account) "Cannot vote on your own proposal")
          )
      )
    )

                                        ; ------------------ Utility Functions -----------------------------

  ;; utility function for compute the votes table key
  (defun votes-table-key:string (account:string proposal-id:string)
    @doc "create id for insert and update vote-table"
    (format "{}-{}" [account proposal-id]))

  ;; utility function for retrieve the current time
  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data)))

  ;; utility function for check if an account voted on a proposal
  (defun check-account-voted:bool (account:string proposal-id:string)
    @doc "Checks if an account has already voted a specific proposal"
    (with-default-read votes-table (votes-table-key account proposal-id)
      {"account":""}
      {"account":=account}
      (if (= account "") false true)))

  ;; utility function for retrieve the creator account of a proposal
  (defun read-proposal-owner:string (proposal-id:string)
    @doc "Get the owner of a proposal"
    (at 'account (read-proposal proposal-id)))

  ;; utility function for check if a proposal exist
  (defun proposal-exists:bool (proposal-id:string)
    @doc "Returns whether a proposal exists."
    (with-default-read proposals-table proposal-id
      { 'id: "" }
      { 'id := id }
      (> (length id) 0)))

  ;; utility function for compute the minimum quorum value needed by a proposal
  ;; this function is used in the proposal submission.
  ;; Returns the percentage of the total-voting-power - proposal-owner-voting-power because the proposal creator can't vote
  (defun calculate-proposal-min-quorum:decimal (percentage:decimal proposal-owner:string)
    @doc "Returns the percentage of the total-voting-power - proposal-owner-voting-power"
    (floor (* percentage (- (get-total-snapshot-voting-power) (get-voting-power-snapshot proposal-owner))) 10))

                                        ; -------------- Snapshot functions ---------------------
  ;; Retrieve the list of all members participating on a space
  ;; It's used by the snapshot job on each cycle to execute the snapshot in chunks
  (defun get-dao-accounts:[string] ()
  (kaddex.aggregator.get-staking-ids))

  (defun update-snapshot-helper:string (account:string vp:decimal position:decimal snapshot-time:time)
   (require-capability (INTERNAL))
    (let (
      (vp-square-root (floor (sqrt vp) 5))
    )
      (write snapshot-table account {
        'voting-power: vp-square-root
        ,'position: position
        ,'last-updated: snapshot-time})

      (with-default-read current-snapshot-total-table CURRENT-SNAPSHOT-TOTAL-KEY {
        'voting-power:0.0
      }
      {
        'voting-power:=voting-power
      }
        (update current-snapshot-total-table CURRENT-SNAPSHOT-TOTAL-KEY {"voting-power":(+ voting-power vp-square-root), "last-updated":snapshot-time})
      )
      (format "Account {} snapshotted with {} vp and {} position" [account, vp-square-root, position])
    )
  )

  ;; Writes the snapshot of the users vp and the total-vp for the current cycle
  (defun snapshot-account:string (account:string snapshot-time:time)
  @doc "Writes a snapshot entry for the specified account"
  (with-capability (OPS)
   (with-capability (INTERNAL)
  (let* (
    (account-data (try {'vp:-1.0, 'multiplier:-1.0, 'staked-amount:-1.0} (kaddex.aggregator.get-account-data account)))
    (vp (at 'vp account-data))
    (position (at 'staked-amount account-data))
  )
    (if (= -1.0 vp) (format "Account {} skipped" [account]) (update-snapshot-helper account vp position snapshot-time))
  )
  
  )))

  ;; Function used by the job for set the voting power on each cycle. 
  ;; The job split the users list in chunks - the reset-tota-table fields is used to handle the reset only if it's the first chunk of the cycle
  (defun snapshot-account-batch:[string] (accounts:[string] snapshot-time:time reset-total-table:bool)
    @doc "Snapshot a list of accounts"
    (with-capability (OPS)
     (with-capability (INTERNAL)
      (if reset-total-table (write current-snapshot-total-table CURRENT-SNAPSHOT-TOTAL-KEY {'voting-power:0.0, 'last-updated:snapshot-time}) "")
      (map (lambda (x) (snapshot-account x snapshot-time)) accounts)
    )))

  ;; utility function to retrieve the latest voting power snapshot of an account
  (defun get-voting-power-snapshot:decimal (account:string)
    @doc "Returns the voting power for an account from last snapshot."
    (with-default-read snapshot-table account
      {'voting-power: 0.0
      ,'last-updated: (curr-time)}
      {'voting-power := voting-power
      ,'last-updated := last-updated}
      (if (< (curr-time) (add-time last-updated MAX_SNAPSHOT_DURATION_PERIOD)) voting-power 0.0)
    ))

  ;; utility function to retrieve the latest total voting power snapshot
  (defun get-total-snapshot-voting-power:decimal ()
    @doc "Retrieve the total voting power from snapshot table"
    (with-default-read current-snapshot-total-table CURRENT-SNAPSHOT-TOTAL-KEY {
      'voting-power:0.0
    }
    {
      'voting-power:=voting-power
    }
    voting-power)
  )

  ;; utility function to retrieve the list of all snapshotted accounts
  (defun get-snapshotted-accounts:[string] ()
    (keys snapshot-table))

                                        ;
                                        ;------------------ Functions ------------------------------------

  ;; create proposal function
  (defun create-proposal:object{gov-space-v1.created-proposal}
      (title:string description:string owner-account:string start-date:time end-date:time minimum-quorum-percentage:decimal majority-percentage:decimal epoch-time:time)
    @doc "Creates proposal with the parameters: `title` `description` `owner-account` `start-date` `end-date` and insert it into proposals-table"
    (with-capability (CREATE_PROPOSAL title description owner-account start-date end-date minimum-quorum-percentage majority-percentage epoch-time)
      (let
          (
           (id (hash [title description]))
           (min-quorum (calculate-proposal-min-quorum minimum-quorum-percentage owner-account))
           )
        (insert proposals-table id {
                                    "id":id,
                                    "title": title,
                                    "description":description,
                                    "account":owner-account,
                                    "start-date":start-date,
                                    "creation-date":(curr-time),
                                    "end-date":end-date,
                                    "total-approved": 0.0,
                                    "total-refused":0.0,
                                    "total-votes":0,
                                    "minimum-quorum":min-quorum,
                                    "minimum-quorum-percentage":minimum-quorum-percentage,
                                    "majority-percentage":majority-percentage,
                                    "epoch-time":epoch-time
                                    })
        {'id: id, 'minimum-quorum: min-quorum, 'start: start-date, 'end: end-date}
        )
      )
    )

  ;; Helper function for write the vote of an account on a proposal
  (defun vote-proposal-helper:string (proposal-id:string account:string action:string vp:decimal )
    @doc "Helper function for inserting vote of an account on a proposal"
    (require-capability (INTERNAL))
      (insert votes-table (votes-table-key account proposal-id) {
                                                                 "proposal-id":proposal-id
                                                                 ,"account":account
                                                                 ,"vp":vp
                                                                 ,"action":action
                                                                 ,"date": (curr-time)
                                                                 })
      
    )

  ;; Vote on a proposal as approved
  (defun approved-vote:string (proposal-id:string account:string)
    @doc "Vote for a proposal as approved"
    (with-capability (VOTE_PROPOSAL account proposal-id)
     (with-capability (INTERNAL)
        (let* (
               (vp (get-voting-power-snapshot account))
               )
          (enforce (> vp 0.0)
                   "This account does not have voting power")
          (with-read proposals-table proposal-id {
                                                  "total-approved":= tot-approved,
                                                  "total-votes":= tot-votes
                                                  }
                     (update proposals-table proposal-id
                             {"total-approved": (+ tot-approved (floor vp 2)), ;; Quadratic Voting
                              "total-votes":(+ tot-votes 1) })
                     )
          (vote-proposal-helper proposal-id account VOTE_APPROVED vp)
          (format "Account {} APPROVED the '{}' proposal" [account proposal-id])
          )
        )
      )
    )

  ;; Vote on a proposal as refused
  (defun refused-vote:string (proposal-id:string account:string)
    @doc "Vote for a proposal as refused"
    (with-capability (VOTE_PROPOSAL account proposal-id)
      (with-capability (INTERNAL)
        (let* (
               (vp (get-voting-power-snapshot account))
               )
          (enforce (> vp 0.0)
                   "This account does not have voting power")
          (with-read proposals-table proposal-id {
                                                  "total-refused":= tot-refused,
                                                  "total-votes":= tot-votes
                                                  }
                     (update proposals-table proposal-id
                             {"total-refused": (+ tot-refused (floor vp 2)), ;; Quadratic Voting
                              "total-votes":(+ tot-votes 1) })
                     )
          (vote-proposal-helper proposal-id account VOTE_REFUSED vp)
          (format "Account {} REFUSED the '{}' proposal" [account proposal-id])
          )
      )
    )
  )

  (defun get-account-data:object{gov-space-v1.snapshot} (account:string)
    @doc "Function that returns snapshotted data (voting-power, position and last update) about a specific account"
    (with-default-read snapshot-table account 
      {"voting-power":-1.0,
       "position":-1.0,
       "last-updated": (curr-time)}
      {"voting-power":=voting-power,
      "position":=position,
      "last-updated":=last-update
      }
      {'voting-power:voting-power, 'position:position, 'last-updated:last-update}
    )
  )

  (defun read-proposal:object{gov-space-v1.proposal} (proposal-id:string)
    @doc "Get proposal info giving a specific proposal-id"
    (read proposals-table proposal-id)
    )

  (defun read-all-proposals:[object{gov-space-v1.proposal}] ()
    @doc "Get all proposals info"
    (map (read proposals-table) (get-proposal-ids))
    )

  (defun get-proposal-ids:[string] ()
    @doc "read-all-proposals helper function"
    (keys proposals-table)
    )

  (defun read-proposal-votes:[object{gov-space-v1.vote}] (proposal-id:string)
    @doc "Get all votes for a proposal stored in votes-table"
    (select votes-table (where 'proposal-id (= proposal-id)) )
    )

  (defun read-account-votes:[object{gov-space-v1.vote}] (account:string)
    @doc "get vote info for each proposal that account has voted in the votes-table"
    (select votes-table (where 'account (= account)) )
    )

  (defun read-account-vote-proposal:object{gov-space-v1.vote} (account:string proposal:string)
    @doc "Get vote info of account on a specific proposal"
    (with-default-read votes-table (votes-table-key account proposal)
      {"proposal-id":"",
      "account":"",
      "vp":"",
      "action":"",
      "date":""
      }
      {"proposal-id":=proposal-id,
      "account":=acct,
      "vp":=vp,
      "action":=action,
      "date":=date
      }

      {"proposal-id":proposal-id, "account":acct, "vp":vp, "action":action, "date":date}
    )
  )

  (defun proposal-finished:bool (proposal-id:string)
    @doc "Returns true if a proposal is not open anymore"
    (let (
          (end (at 'end-date (read proposals-table proposal-id)))
          )
      (>= (curr-time) end)
      )
    )

  (defun proposal-approved:bool (proposal-id:string)
    @doc "Checks if a proposal has reached the minimum quorum and the majority needed on all votes"
    (with-read proposals-table proposal-id
      {"total-approved":= approved-vp
      ,"total-refused":= refused-vp
      ,"total-votes":= total-votes
      ,"minimum-quorum":= min-quorum
      ,"majority-percentage":= majority-percentage
      ,"end-date" := end
                       }
      (let ((is-finished (proposal-finished proposal-id)))
        (enforce is-finished
                 "Proposal still open"))
      ; Check conditions for approval
      (let ((total-voting-power (+ approved-vp refused-vp)))
        (and 
          (>= total-voting-power min-quorum) ; Check min-quorum is met
          (>= approved-vp (* total-voting-power majority-percentage)) ; Check approved-vp is >= majority-percentage of the total voting power used
        )
      )
    ))
  )


; ------------------ Defining tables ---------------------------------


