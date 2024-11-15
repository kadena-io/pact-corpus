(module ammar-test 'admin-1
  (defun get-block-time (account:string amount:decimal)
      (at 'block-time (chain-data))
  )
)


