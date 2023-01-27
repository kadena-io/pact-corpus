(module babena-ledger GOVERNANCE
  (defcap GOVERNANCE () (enforce-guard (keyset-ref-guard "babena-admin")))
)

