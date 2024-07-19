(module isoko-collection-policy-v1 GOVERNANCE

    @doc "Collection token policy."

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
        token-id:string
        collection-id:string
        owner: string
        mint-time: time
    )

    (defschema quote-spec
        @doc "Quote data to include in payload"
        price:decimal
        recipient:string
        recipient-guard:guard
    )

    (defschema quote-schema
        id:string
        spec:object{quote-spec})

    (defschema account-wl-record-schema
        @doc "Schema to hold account whitelist information & Role"
        account:string
        minted-total:integer
        free-mints-granted:integer
        free-mints-remaining:integer
    )

    (defschema bool-consts-schema
        @doc "schema for boolean constants"
        val:bool
    )

    (defschema integer-consts-schema
        @doc "schema for integer constants such as TOTAL-MINTED-KEY"
        val:integer
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Tables
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (deftable collection-table:{collection})
    (deftable token-table:{token})
    (deftable quotes:{quote-schema})
    (deftable account-wl-records:{account-wl-record-schema})
    (deftable bool-consts:{bool-consts-schema})
    (deftable integer-consts:{integer-consts-schema})

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Constants
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defconst ADMIN-KS:string "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin")
    (defconst COLLECTION-ID-MSG-KEY:string "collection_id")
    (defconst TOKEN_SPEC "token_spec"
      @doc "Payload field for token spec")
    (defconst QUOTE-MSG-KEY "quote"
      @doc "Payload field for quote spec")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Capabilities
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defcap GOVERNANCE () (enforce-guard ADMIN-KS))

    (defcap COLLECTION:bool (collection-id:string collection-name:string collection-size:integer operator-guard:guard operator-account:string)
        @doc "Capability to grant creation of a collection and emit COLLECTION event for discovery"
        @event
        (enforce-guard operator-guard)
    )
    (defcap CREATE ()
        @doc "Private Capability to update WL amounts after mint"
        (compose-capability (INCREMENT-CREATED))
        true)
    (defcap UPDATE-OWNER (token-id:string new-owner:string)
        @doc "Private Capability to update owner of token"
        true)
    (defcap INCREMENT-MINTED ()
        @doc "Private Capability to increment amount minted"
        true)
    (defcap INCREMENT-CREATED ()
        @doc "Private Capability to increment amount created"
        true)
    (defcap UPDATE-WL ()
        @doc "Private Capability to update WL amounts after mint"
        true)

    (defcap TOKEN-COLLECTION (collection-id:string token-id:string)
        @doc "Capability to grant creation of a collection's token and emit TOKEN-COLLECTION event for discovery"
        @event
        (with-read collection-table collection-id {
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
            (insert collection-table collection-id {
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
    (let* ( (token-id:string  (at 'id token)) (collection-id:string (read-msg COLLECTION-ID-MSG-KEY)) )
    ;;Enforce operator guard
    (with-capability (TOKEN-COLLECTION collection-id token-id)
      (with-read collection-table collection-id {
        "max-size":= max-size
       ,"size":= size
        }

      ; max-size=0 means unlimited collection
      (enforce (or? (= 0) (< size) max-size) "Exceeds collection size")

      (update collection-table collection-id {
        "size": (+ 1 size)
      }))
      (insert token-table token-id
        { 
          "token-id" : token-id,
          "collection-id" : collection-id,
          "owner": "",
          "mint-time": (at "block-time" (chain-data))
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
    (let*
        (
          (token-id (at "id" token))
          (token-details (get-token token-id))
          (collection-id (at "collection-id" token-details))
        )
        (enforce false token-id)
        (enforce (= amount 1.0) "Invalid amount")
        (enforce (= (at "supply" token) 0) "Token already owned")
        (update-owner token-id account)
        (update-mint-time token-id (at "block-time" (chain-data)))
        (increment-total-minted-for-collection collection-id)
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

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Utility Setters
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defun increment-total-minted-for-collection (collection-id:string collection:object{collection})
      (bind collection
        {
          "size":= size:integer
        }
        (update collection-table collection-id
          {
            "size": (+ size 1)
          }
        )
      )
    )

    (defun update-owner (token-id:string new-owner:string)
      @doc "Updates token with new owner"
      ;  (require-capability (UPDATE-OWNER token-id new-owner))
        (update token-table token-id
          {"owner": new-owner}
        )
      true
    )

    (defun update-mint-time (token-id:string mint-time:time)
      @doc "updates token mint time"
      (update token-table token-id
        {"mint-time": mint-time}
      )
    )

    (defun add-bool-consts:bool (key:string val:bool)
      @doc "Adds entry to boolean constants"
      (with-capability (GOVERNANCE)
        (insert bool-consts key
          {"val": val}
        )
      )
    )

    (defun add-integer-consts:bool (key:string val:bool)
      @doc "Adds entry to boolean constants"
      (with-capability (GOVERNANCE)
        (insert bool-consts key
          {'val: val}
        )
      )
    )

    (defun update-bool-consts:bool (key:string val:bool)
      @doc "Adds entry to boolean constants"
      (with-capability (GOVERNANCE)
        (update bool-consts key
          {'val: val}
        )
      )
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Utility Getters
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun get-nfts-by-owner(collection-id:string owner:string)
      @doc "All NFTs by owner"
        (let*
          (
            (collection (get-collection collection-id ))
          )
          (select collection ["token-id"] (where "owner" (= owner)))
        )
    )

    (defun get-bool-consts:bool (key:string)
      @doc "Get bool constants with false as default"
      (with-default-read bool-consts key
        { 'val : false }
        { 'val := s }
        s)
    )
    (defun get-integer-consts:integer (key:string)
      @doc "Get integer constants with -1 as default"
      (with-default-read integer-consts key
        { 'val : -1 }
        { 'val := s }
        s)
    )

    (defun get-collection:object{collection} (collection-id:string)
      (read collection-table collection-id)
    )
    (defun get-token:object{token} (token-id:string)
      (read token-table token-id)
    )
    (defun get-minted-supply:integer (collection-id:string)
        (at "size" (read collection-table collection-id))
    )
    (defun get-created-supply:integer (collection-id:string)
        (at "size" (read collection-table collection-id))
    )
)

