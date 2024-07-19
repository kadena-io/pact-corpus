(module kadena-mining-club GOVERNANCE
  @doc "Kadena Mining Club placeholder contract."

    (defconst HELLO "hello")
    (defconst ADMIN_KEYSET (read-keyset 'kmc-admin))

    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )
)

