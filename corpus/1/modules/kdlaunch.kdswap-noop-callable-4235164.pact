(module kdswap-noop-callable GOVERNANCE
  "Noop implementation of swap-callable-v1"
  (implements kdswap-callable-v1)
  (defcap GOVERNANCE () (enforce-guard (enforce-keyset 'kdlaunch-admin)))
  (defun swap-call:bool
    ( token-in:module{fungible-v2}
      token-out:module{fungible-v2}
      amount-out:decimal
      sender:string
      recipient:string
      recipient-guard:guard
    )
    "Noop implementation"
    true
  )
)
