(module isoko-collection-policy-v1 GOVERNANCE

  @doc "Collection token policy."

  (defconst ADMIN-KS:string "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)

  (use marmalade-v2.policy-manager)
  (use kip.token-policy-v2 [token-info])

  (defschema collection
    id:string
    name:string
    size:integer
    max-size:integer
    operator-guard:guard
    operator-account:string
  )

  (defschema token
    id:string
    collection-id:string
  )

  (deftable collections:{collection})
  (deftable tokens:{token})

  (defconst COLLECTION-ID-MSG-KEY:string "collection_id")

  (defcap COLLECTION:bool (collection-id:string collection-name:string collection-size:integer operator-guard:guard operator-account:string)
    @doc "Capability to grant creation of a collection and emit COLLECTION event for discovery"
    @event
    (enforce-guard operator-guard)
  )

  (defcap TOKEN-COLLECTION (collection-id:string token-id:string)
    @doc "Capability to grant creation of a collection's token and emit TOKEN-COLLECTION event for discovery"
    @event
    (with-read collections collection-id {
      'operator-guard:= operator-guard:guard
      }
      (enforce-one "Any Guard passes" [
            (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
            (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")))]
        )
    )
  )

  (defun create-collection:bool
    ( collection-name:string
      collection-size:integer
      operator-guard:guard
      operator-account:string
      )
      @doc "Executed directly on the policy, required to succeed before `create-token` \
      \ step for collection tokens and emits COLLECTION event for discovery"
      (enforce (>= collection-size 0) "Collection size must be positive")
      (enforce (validate-principal operator-guard operator-account) "Incorrect account guard, only principal accounts allowed")
      (let ((collection-id:string (create-collection-id collection-name operator-guard) ))
        (with-capability (COLLECTION collection-id collection-name collection-size operator-guard operator-account)
          (insert collections collection-id {
          "id": collection-id
          ,"name": collection-name
          ,"max-size": collection-size
          ,"size": 0
          ,"operator-guard": operator-guard
          ,"operator-account": operator-account
          })
          true
      ))
  )

  (defun enforce-init:bool (token:object{token-info})
    @doc "Executed at `create-token` step of marmalade.ledger.                 \
    \ Required msg-data keys:                                                  \
    \ * collection_id:string - registers the token to a collection and emits   \
    \ TOKEN-COLLECTION event for collection token discovery"
    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) isoko-collection-policy-v1))
    (let* ( (token-id:string  (at 'id token))
            (collection-id:string (read-msg COLLECTION-ID-MSG-KEY)) )
    ;;Enforce operator guard
    (with-capability (TOKEN-COLLECTION collection-id token-id)
      (with-read collections collection-id {
        "max-size":= max-size
       ,"size":= size
        }

      ; max-size=0 means unlimited collection
      (enforce (or? (= 0) (< size) max-size) "Exceeds collection size")

      (update collections collection-id {
        "size": (+ 1 size)
      }))
      (insert tokens token-id
        { "id" : token-id
         ,"collection-id" : collection-id
      })
      )
    )
    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    true
  )


  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string
    )
    true
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string
    )
    true
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string
    )
    true
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal
    )
    true
  )

  ;;UTILITY FUNCTIONS

  (defun create-collection-id:string (collection-name:string operator-guard:guard)
    (format "collection:{}" [(hash [collection-name operator-guard])])
  )

  (defun get-collection:object{collection} (collection-id:string )
    (read collections collection-id)
  )

  (defun get-token:object{token} (token-id:string)
    (read tokens token-id)
  )

)

