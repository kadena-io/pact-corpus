(module bridge BRIDGE-ADMIN


    (defcap BRIDGE-ADMIN () "Admin-only." (enforce-keyset 'lago-ns-user))
)
