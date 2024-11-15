(module skellies-v1-ng  GOVERNANCE
    @doc "Manage Skellies v1.0 NG NFT's"

    (use n_4e470a97222514a8662dd1219000a0431451b0ee.ledger)
    (use n_4e470a97222514a8662dd1219000a0431451b0ee.policy-collection)
    (use n_4e470a97222514a8662dd1219000a0431451b0ee.std-policies)
	(use n_4e470a97222514a8662dd1219000a0431451b0ee.policy-royalty)
    (use free.util-lists [chain])

    ; --------------------------------------------------------------------------
    ; Constants
    ; --------------------------------------------------------------------------
    (defconst COLLECTION_COUNT "collection-count")
    (defconst TOKEN_COUNT "token-count")
    (defconst ADMIN_KEYSET "n_9a213df4df49c23a6c331b87ad0da14efd292f7b.skellies-admin-keys")

    ; --------------------------------------------------------------------------
    ; Schema
    ; --------------------------------------------------------------------------
    (defschema counts-schema
        @doc "Keeps track of key counts."
        count:integer
    )

    (defschema collection-schema
        @doc "Stores collection data"
        id:string
        collection-name:string
        collection-guard:guard
        minted-tokens:[string]
        non-minted-tokens:[string]
        reserved-tokens:[string]
        minted-total:integer
        supply:integer
    )
    
    (defschema token-schema
        @doc "Stores token data"
        id:string
        collection-id:string
        supply:decimal
        token-uri:string
        mint:bool
    )

    ; --------------------------------------------------------------------------
    ; Tables
    ; --------------------------------------------------------------------------
    (deftable count-table:{counts-schema})
    (deftable collection-table:{collection-schema})
    (deftable token-table:{token-schema})

    ; --------------------------------------------------------------------------
    ; Init
    ; --------------------------------------------------------------------------
    (defun init()
      (with-capability (GOVERNANCE)
        (insert count-table COLLECTION_COUNT { "count": 0 })
        (insert count-table TOKEN_COUNT { "count": 0 })
      )
    )

    ; --------------------------------------------------------------------------
    ; Functions
    ; --------------------------------------------------------------------------
    (defun create-skellies-collection (col-name:string)
        @doc "Allows the admin to create a collection"
        (with-capability (GOVERNANCE)
        (with-capability (PRIVATE)
            (let*
                (
                    (collection-guard (read-keyset 'ks))
                    (creator (read-string 'creator))
                    (old-col (try false (at 'collection-name (get-collection col-name))))
                    (collection-id (create-collection-id col-name collection-guard))
                )
                (enforce (= old-col false) "Collection already exists!")
                (create-collection collection-id col-name 1250 creator collection-guard)
                (insert collection-table collection-id {
                    'id: collection-id
                    ,'collection-name: col-name
                    ,'collection-guard: collection-guard
                    ,'minted-tokens:[""]
                    ,'non-minted-tokens:[""]
                    ,'reserved-tokens:[""]
                    ,'minted-total: 0
                    ,'supply: 1250
                })
                (increase-count COLLECTION_COUNT)
            )
        ))
    )

    ; Mint for a multiple receivers
    (defun create-mint-tokens (collection-id:string)
        @doc "Allows the admin to create tokens"
        (with-capability (GOVERNANCE)
        (with-capability (PRIVATE)
            (let* 
                (
                    (policies (list-to-policies (read-msg 'policies)))
                    (tmp-guard (read-keyset 'ks-tmp))
                    (receivers (read-msg 'receivers))
					(minters (read-msg 'minters))
                    (uris (read-msg 'uris))
                    (token-ids (map (create-token-id tmp-guard) uris))
                    (col-id (try false (at 'id (get-collection collection-id))))
                    (minted-total (+ (get-collection-total-minted collection-id) (length uris)))
                    (token-count (+ (get-count TOKEN_COUNT) (length uris)))
                    (old-minted-list (get-collection-minted-tokens col-id))
                    (new-minted-list (if (= (at 0 old-minted-list) "") token-ids (chain [old-minted-list token-ids])))
                )
                (enforce (!= col-id false) "Collection ID does not exist in Skellies collections!")
                (zip (lambda (id uri) (create-token id 0 uri policies tmp-guard)) token-ids uris)
                (zip (lambda (id receiver) (mint id (at 'receiver receiver) (at 'guard receiver) 1.0)) token-ids receivers)
				; Rotates the Royalty guard to the original Minter of the NFT
				(zip (lambda (id minter) (rotate id  (at 'minter minter) (at 'guard minter))) token-ids minters)
                (zip (lambda (id uri) (insert token-table id {
                    'id: id
                    ,'collection-id: collection-id
                    ,'supply: 1.0
                    ,'mint: true
                    ,'token-uri: uri})) token-ids uris)
                (update collection-table collection-id
                    { "minted-total": minted-total
                    , "minted-tokens": new-minted-list})
                (update-count TOKEN_COUNT token-count)
                ; Return token IDs
                token-ids
            )
        ))
    )

    (defun get-count:integer (key:string)
        @doc "Gets the count for a key" 
        (at "count" (read count-table key ['count]))
    )
    
    (defun increase-count (key:string)
        @doc "Increase the count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update count-table key {"count": (+ 1 (get-count key))})
    )

    (defun update-count (key:string count:integer)
        @doc "Increase the count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update count-table key {"count": count})
    )

    (defun get-collection(col-id:string)
        @doc "Returns collection information."
        (read collection-table col-id)
    )

    (defun get-collection-minted-tokens(col-id:string)
        @doc "Returns collection minted tokens."
        (at "minted-tokens" (read collection-table col-id ['minted-tokens]))
    )

    (defun get-collection-non-minted-tokens(col-id:string)
        @doc "Returns collection non minted tokens."
        (at "non-minted-tokens" (read collection-table col-id ['non-minted-tokens]))
    )

    (defun get-collection-total-minted(col-id:string)
        @doc "Returns collection total minted tokens."
        (at "minted-total" (read collection-table col-id ['minted-total]))
    )

    (defun get-collection-supply(col-id:string)
        @doc "Returns collection total supply."
        (at "supply" (read collection-table col-id ['supply]))
    )

    (defun get-all-collections()
        @doc "Returns all the collections."
        (select collection-table (where "id" (!= "")))
    )

    (defun get-token(token-id:string)
        @doc "Returns token information."
        (read token-table token-id)
    )

    (defun get-all-tokens()
        @doc "Returns all the tokens."
        (select collection-table (where "id" (!= "")))
    )

    ; --------------------------------------------------------------------------
    ; Utilities
    ; --------------------------------------------------------------------------
    (defcap PRIVATE ()
        true
    )

    (defcap GOVERNANCE ()
        (enforce-guard (keyset-ref-guard "n_9a213df4df49c23a6c331b87ad0da14efd292f7b.skellies-admin-keys"))
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard 
            (at "guard" (coin.details account))
        )
    )
)



