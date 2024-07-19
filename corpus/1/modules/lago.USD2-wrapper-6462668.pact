(module USD2-wrapper MINTER-ADMIN


    (defcap MINTER-ADMIN () "Admin-only." (enforce-keyset 'lago-ns-user))
)
