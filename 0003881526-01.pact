(module collect-gallinas-util GOVERNANCE

  (defcap GOVERNANCE ()
    @doc " Give the admin full access to call and upgrade the module. "
    (enforce-keyset 'admin-gallina)
  )

  (defconst MINIMUM_PRECISION 0
    " Specifies the minimum denomination for token transactions. ")

  (defconst uACCOUNT_ID_CHARSET CHARSET_LATIN1
    " Allowed character set for Account IDs. ")

  (defconst uACCOUNT_ID_MIN_LENGTH 3
    " Minimum character length for account IDs. ")

  (defconst uACCOUNT_ID_MAX_LENGTH 256
    " Maximum character length for account IDs. ")

  (defconst uGALLINA_NAME_MIN_LENGTH 3
    " Minimum character length for account IDs. ")

  (defconst uGALLINA_NAME_MAX_LENGTH 30
    " Maximum character length for account IDs. ")

  (defun coin-account-exists:bool (account:string)
    @doc "Returns true if account exists on coin contract"
	(try false
	     (let ((ok true))
		      (coin.details account)
			  ok)))

  (defun enforce-coin-account-exists (account:string)
    @doc "Enforces coin account existance"
     (let ((exist (coin-account-exists account)))
	      (enforce exist "Account does not exist in coin contract")))

  (defun coin-account-guard (account:string)
    @doc "Enforces coin account guard"
    (at "guard" (coin.details account)))

  (defun validate-account-id ( accountId:string )
    @doc " Enforce that an account ID meets charset and length requirements. "
    (enforce
      (is-charset uACCOUNT_ID_CHARSET accountId)
      (format
        "Account ID does not conform to the required charset: {}"
        [accountId]))
    (let ((accountLength (length accountId)))
      (enforce
        (>= accountLength uACCOUNT_ID_MIN_LENGTH)
        (format
          "Account ID does not conform to the min length requirement: {}"
          [accountId]))
      (enforce
        (<= accountLength uACCOUNT_ID_MAX_LENGTH)
        (format
          "Account ID does not conform to the max length requirement: {}"
          [accountId]))))

  (defun validate-gallina-name ( petName:string )
    @doc " Enforce that a Gallina Name meets charset and length requirements. "
    (enforce
      (is-charset uACCOUNT_ID_CHARSET petName)
      (format
        "Gallina Name does not conform to the required charset: {}"
        [petName]))
    (let ((nameLength (length petName)))
      (enforce
        (>= nameLength uGALLINA_NAME_MIN_LENGTH)
        (format
          "Gallina Name does not conform to the min length requirement: {}"
          [petName]))
      (enforce
        (<= nameLength uACCOUNT_ID_MAX_LENGTH)
        (format
          "Gallina Name does not conform to the max length requirement: {}"
          [petName]))))

  (defun enforce-valid-transfer
    ( sender:string
      receiver:string
      precision:integer
      amount:decimal)
      @doc " Enforces transfer rules "
    (enforce (!= sender receiver)
      "You must make a transfer to someone else besides your self.")
    (enforce-valid-amount precision amount)
    (enforce (= amount 1.0)
      "You may only transfer 1 Gallina or Egg at a time.")
    (validate-account-id sender)
    (validate-account-id receiver)
  )

  (defun enforce-valid-amount
    ( precision:integer
      amount:decimal
    )
    @doc " Enforces positive amounts "
    (enforce (> amount 0.0) "Positive non-zero amounts only.")
    (enforce-precision precision amount)
  )

  (defun enforce-precision
    ( precision:integer
      amount:decimal
    )
    @doc " Enforces whole numbers "
    (enforce
      (= (floor amount precision) amount)
      "Whole Eggs and Gallinas only.")
  )

)

