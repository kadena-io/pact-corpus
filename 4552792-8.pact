(module pay-to-and-mint GOVERNANCE
  (defcap GOVERNANCE ()
    "makes sure only admin account can update the smart contract"
    (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
  )
  
  
  (use coin)

    (defun pay-to-then-mint (cost:decimal acc1:string acc2:string amount:integer owner:string acc2-guard:guard)

        (coin.transfer-create acc1 acc2 acc2-guard cost)
        (n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-orchestrator.mint-bulk amount acc2 acc2-guard)

    )


)


