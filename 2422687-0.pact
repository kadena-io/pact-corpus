(module test-ks GOVERNANCE

  (use util.guards)


  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'kaddex-keyset)))

  (defcap OPS ()
    (enforce-guard (before-date END_TIME))
    (enforce-guard (at-after-date START_TIME))
    (enforce-guard
      (keyset-ref-guard 'kaddex-ops)))

(defconst START_TIME:time (time "2021-06-10T17:59:00Z"))

  (defconst END_TIME:time (time "2021-06-10T18:59:00Z"))

  (defun test-ops ()
    (with-capability (OPS)
      "hello"
    )
  )

)

