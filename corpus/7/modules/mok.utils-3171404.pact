(module utils GOVERNANCE
  (defcap GOVERNANCE ()
    @doc " Give the admin full access to call and upgrade the module. "
    (enforce-guard 'mok-admin)
  )

  (defun min (a b)
    (if (> a b) b a)
  )

  (defun max (a b)
    (if (> a b) a b)
  )
)


