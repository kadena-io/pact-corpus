(module kitty-kads-gen-0-image-ledger 'kitty-kad
  "Kitty Kad Kitties NFTs game"

  (defconst ADMIN_KEYSET (read-keyset 'kitty-kad))

    (defcap ADMIN()
        @doc "Admin functionality capability"
        (enforce-keyset  ADMIN_KEYSET)
    )

    (defschema ledger-schema
        @doc "Gen 0s ledger"
        base-64:string
    )
    (deftable ledger-table:{ledger-schema})

    (defun write-base-64-for-ids (base-64s:list ids:list) 
        @doc "Writes base64 strings for a list of ids"
        (enforce ( = (length base-64s) (length ids) ) "fields and ids list must be equal length")
        (with-capability (ADMIN)
            (map 
                (write-base-64-for-index base-64s ids)
                (enumerate 0 (- (length ids) 1))
            )
        )
    )

    (defun write-base-64-for-index (base-64s:list ids:list index)
        @doc "Writes base64 value at the index for the id at the index"
        (require-capability (ADMIN))
        (write ledger-table (at index ids) {"base-64": (at index base-64s)})
    )

    (defun get-base-64-for-ids (ids:list)
        @doc "Returns the base 64 string for an id"
        (map (get-base-64-for-id) ids)
    )

    (defun get-base-64-for-id (id:string)
        @doc "Returns the base 64 string for an id"
        (at "base-64" (read ledger-table id ["base-64"]))
    )

    (defun all-ids ()
        @doc "All ids with b64 information"
        (keys ledger-table)
    )
)
;  (create-table ledger-table)

