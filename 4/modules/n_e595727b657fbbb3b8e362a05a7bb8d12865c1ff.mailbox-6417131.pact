(module mailbox GOVERNANCE
    
    (implements mailbox-iface)

    (defcap GOVERNANCE () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.upgrade-admin"))

    (defcap ONLY_MAILBOX:bool () true)
)
