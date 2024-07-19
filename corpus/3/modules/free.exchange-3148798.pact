(module exchange GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'kadenaswap-keyset)))

)

