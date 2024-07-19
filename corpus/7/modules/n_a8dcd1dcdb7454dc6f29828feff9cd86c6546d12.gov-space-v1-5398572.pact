(interface gov-space-v1
  "Standard for DAO Space contracts aiming to use Ecko GOV portal functionality"
  ; ------------------ Schemas --------------------------------
  ;; Schema for store proposals details
  ;; key: hash([title,description]) 
  (defschema proposal
    @doc
    "Tracks the proposal created"
    @model [
            (invariant (>= total-approved 0.0))
            (invariant (>= total-refused 0.0))
            (invariant (>= total-votes 0))
            (invariant (> end-date start-date))
            (invariant (>= minimum-quorum 0.0))
            (invariant (>= minimum-quorum-percentage 0.0))
            (invariant (>= majority-percentage 0.0))
            ]
    id:string                                             ;; key for this row, used for convenience - hash([title,description])
    title:string                                          ;; proposal title
    description:string                                    ;; proposal description
    account:string                                        ;; proposal creator
    total-approved:decimal                                ;; total vp amount of approved votes
    total-refused:decimal                                 ;; total vp amount of refused votes
    total-votes:integer                                   ;; total number of votes
    start-date:time                                       ;; proposal start date
    end-date:time                                         ;; proposal end date
    creation-date:time                                    ;; proposal creation date
    minimum-quorum:decimal                                ;; minimum amount of total vp needed by a proposal to be accepted
    minimum-quorum-percentage:decimal                     ;; percentage amount of vp - it's used to compute the minimum-quorum
    majority-percentage:decimal                           ;; percentage of total-approved VP on total VP used on the proposal 
    epoch-time:time                                       ;; the epoch time needed in order to execute the proposal content after the election result
    )

  ;; Schema for store votes on proposals
  ;; key: {account}-{proposal-id}
  (defschema vote
    @model [
      (invariant (> vp 0.0))
      (invariant (or (= action VOTE_APPROVED) (= action VOTE_REFUSED)))
      ]                                         
    proposal-id:string                                    ;; proposal id which the vote refer on
    account:string                                        ;; voted by
    vp:decimal                                            ;; voting power used for the vote
    action:string                                         ;; action could be approve or reject - defined constants 
    date:time                                             ;; voted on
    )

    ;; Schema for store users voting power snapshot
  ;; key: account
  (defschema snapshot
    @model [
      (invariant (>= voting-power 0.0))
      (invariant (>= position 0.0))
      ]
    voting-power:decimal                                  ;; voting power of the user which will be used for the current cycle
    position:decimal
    last-updated:time                                     ;; updated on
    )

  ;; Schema for store the total amount of voting power in the current cycle
  ;; this table is useful for compute the quorum value in the proposal submission
  ;; key: constant value
  (defschema current-snapshot-total
    @model [
      (invariant (>= voting-power 0.0))
      ]
    voting-power:decimal                                  ;; total amount of voting power
    last-updated:time                                     ;; updated on
    )

  ;; Schema for create-proposal result
  (defschema created-proposal
    id:string
    minimum-quorum:decimal
    start:time
    end:time
    )

    ; ------------------ Functions --------------------------------
    (defun snapshot-account-batch:[string] (accounts:[string] snapshot-time:time reset-total-table:bool)
        @doc "Function used by the snapshot job. Writes the snapshot of the vp of each allowed users on every cycle."
     )

    (defun get-dao-accounts:[string] ()
     @doc "Retrieve the list of all members participating on a space. The snapshot job use this list to execute the snapshot of their voting power.")

    (defun get-snapshotted-accounts:[string] ()
      @doc "Retrieve the list of all snapshotted accounts.")

    (defun get-total-snapshot-voting-power:decimal ()
      @doc "Retrieve the total voting power. This is used to compute the quorum.")
    
    (defun get-account-data:object{snapshot} (account:string)
     @doc "Function that returns snapshotted information about a specific account.")
    
    (defun check-account-voted:bool (account:string proposal-id:string)
      @doc "Checks if an account has already voted a specific proposal")
    
    (defun read-account-votes:[object{vote}] (account:string)
      @doc "get vote info for each proposal that account has voted in the votes-table")
    
    (defun read-account-vote-proposal:object{vote} (account:string proposal:string)
      @doc "Get vote info of account on a specific proposal")
    
    (defun create-proposal:object{created-proposal} (title:string description:string owner-account:string start-date:time end-date:time minimum-quorum-percentage:decimal majority-percentage:decimal epoch-time:time)
      @doc "Creates proposal with the parameters: `title` `description` `owner-account` `start-date` `end-date` `minimum-quorum-percentage` `majority-percentage` `epoch-time` and insert it into proposals-table")
    
    (defun read-proposal:object{proposal} (proposal-id:string)
      @doc "Get proposal info giving a specific proposal-id")

    (defun get-proposal-ids:[string] ()
      @doc "read-all-proposals helper function")
    
    (defun read-all-proposals:[object{proposal}] ()
      @doc "Get all proposals info")
    
    (defun proposal-approved:bool (proposal-id:string)
      @doc "Checks if a proposal has a marjority of yes votes and the minimum quorum needed on all votes")
    
    (defun approved-vote:string (proposal-id:string account:string)
       @doc "Vote for a proposal as approved")
    
    (defun refused-vote:string (proposal-id:string account:string)
       @doc "Vote for a proposal as refused")
    
    )
