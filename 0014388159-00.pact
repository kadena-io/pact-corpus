(module ABCD GOV  
  (defcap GOV() (enforce-keyset (keyset-ref-guard 'm-k)))
  (defcap TRANSFER2() true)
  (defcap TRANSFER() true)

  (defcap T () 
    (compose-capability (TRANSFER2))
  )

  (defun a () 
    (enforce-one "No capabilities, snifff" 
        [(require-capability (TRANSFER)) (require-capability (TRANSFER2))])
  )

  (defun b ()
    (with-capability (T) 
      (a)
    )
  )
)
