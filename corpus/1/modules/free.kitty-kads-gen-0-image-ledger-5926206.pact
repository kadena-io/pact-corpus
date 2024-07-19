(module kitty-kads-gen-0-image-ledger GOVERNANCE
  "Kitty Kad Gen 0 image ledger"

    (defcap GOVERNANCE ()
        (enforce false "Enforce non-upgradeability"))

    (defschema ledger-schema
        @doc "Gen 0s ledger"
        base-64:string
    )
    (deftable ledger-table:{ledger-schema})

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

