(module KAO GOVERNANCE
    "Kool Arkade Organization"
  
    (use coin [ details ])

    (defconst ADMIN_KEYSET "free.arkade-admin")

  
    ; ----------------------------------------------------------------------
    ; Schema
  
    (defschema candidates-schema
      "Candidates table schema"
      name:string
      userId:string
      votes:integer)
  
    (defschema votes-schema
      "Votes table schema"
      candidateId:string
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
  
    (defcap VOTED (candidateId:string)
      @managed
      true)
  
    ; ----------------------------------------------------------------------
    ; Functionality
  
    (defun vote-protected (account:string candidateId:string)
      (require-capability (ACCOUNT-OWNER account))
  
      (with-read candidates candidateId { "votes" := votesCount }
        (update candidates candidateId { "votes": (+ votesCount 1) })
        (insert votes account { "candidateId": candidateId })
        (emit-event (VOTED candidateId))
      )
    )
  
    (defun user-voted:bool (account:string)
      (with-default-read votes account
        { "candidateId": "" }
        { "candidateId":= candidateId }
        (> (length candidateId) 0))
    )
  
    (defun candidate-exists:bool (candidateId:string)
      (with-default-read candidates candidateId
        { "name": "" }
        { "name" := name }
        (> (length name) 0))
    )
  
    (defun get-allowed-votes (account:string)
      (let ((arkdBalance (arkade.token.get-balance account)))
        (if (< arkdBalance 10000) 1 2))
      )
  
  
    (defun vote (account:string candidateId:string numberOfVotes:integer)
      "Submit a new vote"
      
      (let ((allowedVotes (get-allowed-votes account)))
        (enforce (<= numberOfVotes allowedVotes) (format "You are only allowed {} votes" [allowedVotes])))
  
      (let ((doubleVote (user-voted account)))
        (enforce (= doubleVote false) "Multiple voting not allowed"))
  
      (let ((exists (candidate-exists candidateId)))
        (enforce (= exists true) "Candidate doesn't exists"))
  
      (with-capability (ACCOUNT-OWNER account)
        (vote-protected account candidateId))
  
      (format "Voted for candidate {}!" [candidateId])
    )
  
    (defun getVotes:integer (candidateId:string)
      "Get the votes count by candidateId"
      (at 'votes (read candidates candidateId ['votes]))
    )
  
    (defun createCandidate (name:string userId:string)
      "Create candidate"
      (insert candidates (int-to-str 10 (+ (get-count) 1)) { "name": name, "votes": 0, "userId": userId })
    )
  
    (defun getCandidate (id:string)
      "Get candidate by id"
      (read candidates id ['name 'votes 'userId])
    )

    (defun getCandidatesByUserId (userId:string)
      @doc "Get candidates by userId"
      (select candidates ["name", "votes", "userId"] (where "userId" (= userId)))
    )
  
    (defun getAllCandidates ()
      "Get all candidates"
      (select candidates ["name", "votes", "userId"] (where "votes" (>= 0)))
    )
  
     (defun get-count ()
       @doc "Get candidates count"
       (length (keys candidates))
     )
  
  )

;   (create-table candidates)
;   (create-table votes)



