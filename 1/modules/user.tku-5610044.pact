(module tku CAP
  (implements lago.fungible-burn-mint)

  (defcap CAP()
    (enforce-keyset "user.ku-ks"))

  (defconst OK "OK")

  (defun mint:string (receiver:string amount:decimal)
    OK)

  (defun burn:string (burner:string amount:decimal)
    OK)

  (defun mint-create:string (receiver:string receiver-guard:guard amount:decimal)
    OK)
)

