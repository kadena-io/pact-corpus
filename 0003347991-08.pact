(module prod-fungible-util GOVERNANCE

    (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard "hypercent.hyper-admin-1-prod")))
  
    (defconst COIN_CHARSET CHARSET_LATIN1
      "The default coin contract character set")
  
    (defconst MINIMUM_PRECISION 12
      "Minimum allowed precision for coin transactions")
  
    (defconst MINIMUM_ACCOUNT_LENGTH 3
      "Minimum account length admissible for coin accounts")
  
    (defconst MAXIMUM_ACCOUNT_LENGTH 256
      "Maximum account name length admissible for coin accounts")
  
    (defun enforce-valid-amount
      ( precision:integer
        amount:decimal
      )
      (enforce (> amount 0.0) "Positive non-zero amount")
      (enforce-precision precision amount)
    )
  
    (defun enforce-valid-account (account:string)
      @doc "Enforce that an account name conforms to the coin contract \
           \minimum and maximum length requirements, as well as the    \
           \latin-1 character set."
  
      (enforce
        (is-charset COIN_CHARSET account)
        (format
          "Account does not conform to the coin contract charset: {}"
          [account]))
  
      (let ((account-length (length account)))
  
        (enforce
          (>= account-length MINIMUM_ACCOUNT_LENGTH)
          (format
            "Account name does not conform to the min length requirement: {}"
            [account]))
  
        (enforce
          (<= account-length MAXIMUM_ACCOUNT_LENGTH)
          (format
            "Account name does not conform to the max length requirement: {}"
            [account]))
        )
    )
  
    (defun enforce-precision
      ( precision:integer
        amount:decimal
      )
      (enforce
        (= (floor amount precision) amount)
        "precision violation")
    )
  
    (defun enforce-valid-transfer
      ( sender:string
        receiver:string
        precision:integer
        amount:decimal)
      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")
      (enforce-valid-amount precision amount)
      (enforce-valid-account sender)
      (enforce-valid-account receiver)
    )
  )
  
