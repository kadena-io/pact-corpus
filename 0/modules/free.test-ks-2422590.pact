(module test-ks GOVERNANCE


  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-keyset)))

  (defcap OPS ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-ops)))

  (defun test-ops ()
    (with-capability (OPS)
      "hello"
    )
  )

)

