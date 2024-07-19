(module KAO GOVERNANCE
    "Kool Arkade Organization"
  
    (use coin [ details ])

    (defconst ADMIN_KEYSET "free.arkade-admin")

  
    ; ----------------------------------------------------------------------
    ; Schema
  
    (defschema candidates-schema
      "Candidates table schema"
      id:string
      name:string
      userId:string
      yesVotes:integer
      noVotes:integer
      )
  
    (defschema votes-schema
      "Votes table schema"
      candidateId:string
      type:string
    )
  
    ; ----------------------------------------------------------------------
    ; Tables
  
    (deftable votes:{votes-schema})
  
    (deftable candidates:{candidates-schema})
  
    ; ----------------------------------------------------------------------
    ; Capabilities
  
    (defcap GOVERNANCE ()
      "Only admin can update this module"
      (enforce-keyset ADMIN_KEYSET))
  
    (defcap ACCOUNT-OWNER (account:string)
      "Make sure the requester owns the KDA account"
      (enforce-guard (at 'guard (coin.details account)))
    )
  
    (defcap VOTED (candidateId:string type:string)
      @managed
      true)
  
    ; ----------------------------------------------------------------------
    ; Functionality
  
    (defun vote-yes (account:string candidateId:string numberOfVotes:integer)
      (require-capability (ACCOUNT-OWNER account))
  
      (with-read candidates candidateId { "yesVotes" := votesCount }
        (update candidates candidateId { "yesVotes": (+ votesCount numberOfVotes) })
        (insert votes account { "candidateId": candidateId })
        (emit-event (VOTED candidateId "yes"))
      )
    )

    (defun vote-no (account:string candidateId:string numberOfVotes:integer)
      (require-capability (ACCOUNT-OWNER account))
  
      (with-read candidates candidateId { "noVotes" := votesCount }
        (update candidates candidateId { "noVotes": (+ votesCount numberOfVotes) })
        (insert votes account { "candidateId": candidateId })
        (emit-event (VOTED candidateId "no"))
      )
    )
  
    (defun user-voted:bool (account:string candidateId:string)
      (with-default-read votes account
        { "candidateId": "" }
        { "candidateId":= votedCandidateId }
        (= votedCandidateId candidateId))
    )
  
    (defun candidate-exists:bool (candidateId:string)
      (with-default-read candidates candidateId
        { "name": "" }
        { "name" := name }
        (> (length name) 0))
    )
  
    ; (defun get-allowed-votes (account:string)
    ;   (let ((arkdBalance (arkade.token.get-balance account)))
    ;     (floor (/ arkdBalance 100000)))
    ;   )

(defun get-allowed-votes (account:string)
  (let ((locks (kdlaunch.kdswap-token-locker.get-account-locks account)))
    (let ((allowed-votes
           (fold (lambda (obj sum)
                   (if (> (at "interval" obj) 30)
                     (+ sum (at "lockAmount" obj))
                     sum))
                 locks
                 0)))
      (/ allowed-votes 100000))))
  
  
    (defun vote (account:string candidateId:string numberOfVotes:integer type:string)
      "Submit a new vote"
      
      (let ((allowedVotes (get-allowed-votes account)))
        (enforce (<= numberOfVotes allowedVotes) (format "You are only allowed {} votes" [allowedVotes])))
  
      (let ((alreadyVoted (user-voted account candidateId)))
        (enforce (= alreadyVoted false) "Multiple voting not allowed"))
  
      (let ((exists (candidate-exists candidateId)))
        (enforce (= exists true) "Candidate doesn't exists"))
  
      (with-capability (ACCOUNT-OWNER account)
        (if (= type "yes"
        (vote-yes account candidateId numberOfVotes)
        (vote-no account candidateId numberOfVotes)))
      )
  
      (format "Voted for candidate {}!" [candidateId])
    )
  
    (defun getYesVotes:integer (candidateId:string)
      "Get the votes count by candidateId"
      (at 'yesVotes (read candidates candidateId ['yesVotes]))
    )

    (defun getNoVotes:integer (candidateId:string)
      "Get the votes count by candidateId"
      (at 'noVotes (read candidates candidateId ['noVotes]))
    )
  
    (defun createCandidate (name:string userId:string)
      "Create candidate"
      (let ((id (int-to-str 10 (+ (get-count) 1))))
        (insert candidates id { "id": id, "name": name, "yesVotes": 0, "noVotes": 0, "userId": userId })  )
      
    )
  
    (defun getCandidate (id:string)
      "Get candidate by id"
      (read candidates id ['id 'name 'yesVotes 'noVotes 'userId])
    )

    (defun getCandidatesByUserId (userId:string)
      @doc "Get candidates by userId"
      (select candidates ["id", "name", "yesVotes", "noVotes", "userId"] (where "userId" (= userId)))
    )
  
    (defun getAllCandidates ()
      "Get all candidates"
        (map 
          (get-fields-for-id [])
          (keys candidates)
        )
    )

      (defun get-fields-for-ids (fields:list ids:list) 
          "Return fields for a list of IDs"
          (map 
              (get-fields-for-id fields)
              ids
          )
      )
  
      (defun get-fields-for-id (fields:list id:string )
          "Return the fields for a given ID"
          (+ {"id": id} (read candidates id fields))
      )
  
     (defun get-count ()
       @doc "Get candidates count"
       (length (keys candidates))
     )
  
  )

;   (create-table candidates)
;   (create-table votes)


