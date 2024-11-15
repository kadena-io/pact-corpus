(module universal-ledger GOVERNANCE
  ;;OMAR WAS HERE
  (defcap GOVERNANCE () (enforce-guard (keyset-ref-guard "free.universal-ledger-admin")))
)
