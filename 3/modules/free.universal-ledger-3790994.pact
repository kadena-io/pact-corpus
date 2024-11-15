(module universal-ledger GOVERNANCE
  (defcap GOVERNANCE () (enforce-guard (keyset-ref-guard "free.universal-ledger-admin")))
)
