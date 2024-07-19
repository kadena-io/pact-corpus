(module marmalade-std-helper-v1 GOVERNANCE
  (implements n_4e470b4e150fafd1c50d1f016331142b55dd01db.marmalade-burn-helper-v1-beta1)

  (defcap GOVERNANCE ()
    (enforce false "Not upgradable"))

  (defun burn:bool (token:string account:string amount:decimal)
    (marmalade.ledger.burn token account amount))
)

