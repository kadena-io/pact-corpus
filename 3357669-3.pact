(module test-calc1 GOVERNANCE
  
  (defconst M:integer 3)
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'calc-ks1)
  )

  (defun calc (a:decimal b:decimal)
    (+ a b))

  (defun mult (a:decimal b:decimal)
    (with-capability (GOVERNANCE)
        (* a b)
    )
  )

  (defun p-const ()
    M
  )

)
