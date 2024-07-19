(module kadena-mining-club1 GOVERNANCE
  @doc "Kadena Mining Club placeholder contract."

    (defconst HELLO "hello")
    (defconst ADMIN_KEYSET "free.kmc-admin")

    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )
)

