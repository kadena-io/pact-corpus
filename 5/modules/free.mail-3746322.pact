(module mail GOV
	(defcap GOV ()
		(enforce-keyset "free.mail-admin")
	)

	(defschema account-schema
		e2ee-messaging-account:string
	)

	(deftable account-table:{account-schema})

	(defun create-account (coin-account:string e2ee-messaging-account:string)
		(enforce-guard (at "guard" (coin.details)))
		(insert account-table coin-account {"e2ee-messaging-account":e2ee-messaging-account})
	)

	(defun change-account (coin-account:string e2ee-messaging-account:string)
		(enforce-guard (at "guard" (coin.details)))
		(update account-table coin-account {"e2ee-messaging-account":e2ee-messaging-account})
	)

	(defun get-account (coin-account:string)
		(with-read account-table coin-account {"e2ee-messaging-account":=e2ee-messaging-account}
			e2ee-messaging-account
		)
	)
)

;(create-table account-table)
