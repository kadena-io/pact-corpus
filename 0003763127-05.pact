(module e2ee-messaging GOV
	(defcap GOV ()
		(enforce-keyset "free.e2ee-messaging-admin")
	)

	(defschema account-schema-1
		username:string
		public-key:string
		private-key-hash:string
		xor-private-key-password-hash:string
		conversations:list
		block-new-conversations:bool
		guard:guard
	)

	(deftable account-table-1:{account-schema-1})

	(defschema conversation-schema-1
		accounts-canonical:list
		messages-count:integer
		active-flags:[bool]
	)

	(deftable conversation-table-1:{conversation-schema-1})

	(defschema message-schema-1
		direction-is-canonical:bool
		content:[object] ; list of 2 elements, first is the message encrypted with the first accounts key, and second is the message encrypted with the second accounts key.
		time:time
	)

	(deftable message-table-1:{message-schema-1})

	(defun create-account (username:string public-key:string private-key-hash:string xor-private-key-password-hash:string guard:guard)
		(insert account-table-1 (account-id username private-key-hash) {"username":username,"public-key":public-key,"private-key-hash":private-key-hash,"xor-private-key-password-hash":xor-private-key-password-hash,"conversations":[],"block-new-conversations":false,"guard":guard})
	)

	(defun create-conversation (your-account:string their-account:string)
		(enforce (not (= your-account their-account)) "Cannot create a conversation with yourself.")
		(let*
			(
				(their-account-data
					(with-read account-table-1 their-account {"conversations":=conversations,"block-new-conversations":=block-new-conversations}
						{"conversations":conversations,"block-new-conversations":block-new-conversations}
					)
				)
				(your-account-data
					(with-read account-table-1 your-account {"conversations":=conversations,"block-new-conversations":=block-new-conversations,"guard":=guard}
						{"conversations":conversations,"block-new-conversations":block-new-conversations,"guard":guard}
					)
				)
				(accounts-canonical-bool:bool
					(hash-canonical your-account their-account)
				)
				(accounts-canonical
					(if
						accounts-canonical-bool
						[your-account their-account]
						[their-account your-account]
					)
				)
				(active-flags
					(if
						accounts-canonical-bool
						[true (not (at "block-new-conversations" their-account-data))]
						[(not (at "block-new-conversations" their-account-data)) true]
					)
				)
				(conversation-id
					(hash accounts-canonical)
				)
			)
			(enforce-guard (at "guard" your-account-data))
			(insert conversation-table-1 conversation-id {"accounts-canonical":accounts-canonical,"messages-count":0,"active-flags":active-flags})
			(update account-table-1 your-account {"conversations":(+ (at "conversations" your-account-data) [conversation-id])})
			(update account-table-1 their-account {"conversations":(+ (at "conversations" their-account-data) [conversation-id])})
		)
	)

	(defun create-message (your-account:string their-account:string content:[object])
		(if
			(try false (read conversation-table-1 (conversation-id your-account their-account)))
			true
			(create-conversation your-account their-account)
		)
		(let*
			(
				(conversation-id:string
					(conversation-id your-account their-account)
				)
				(conversation-data:object
					(read conversation-table-1 conversation-id)
				)
				(is-first:bool
					(= your-account (at 0 (at "accounts-canonical" conversation-data)))
				)
			)
			(with-read account-table-1 your-account {"guard":=guard}
				(enforce-guard guard)
			)
			(enforce
				(=
					(length
						(filter
							(lambda
								(
									account
								)
								(= account your-account)
							)
							(at "accounts-canonical" conversation-data)
						)
					)
					1
				)
				"Must be in conversation to send message"
			)
			(insert message-table-1 (message-id conversation-id (at "messages-count" conversation-data)) {"direction-is-canonical":is-first,"content":content,"time":(at "block-time" (chain-data))})
			(update conversation-table-1 conversation-id {"messages-count":(+ 1 (at "messages-count" conversation-data))})
		)
	)

	(defun get-chronological-conversations:list (account:string)
		(let
			(
				(conversations
					(with-read account-table-1 account {"conversations":=conversations1}
						conversations1
					)
				)
			)
			(map
				(lambda
					(
						conversation-data
					)
					(at "conversation-id" conversation-data)
				)
				(reverse
					(sort
						["recent-message-time"]
						(filter
							(lambda
								(
									conversation-data
								)
								(fold
									(and)
									true
									(at "active-flags" conversation-data)
								)
							)
							(map
								(lambda
									(
										conversation
									)
									(let*
										(
											(conversation-data:object
												(+ (read conversation-table-1 conversation) {"conversation-id":conversation})
											)
											(recent-message-num:integer
												(- (at "messages-count" conversation-data) 1)
											)
											(recent-message-time
												(if
													(<
														-1
														recent-message-num
													)
													(with-read message-table-1 (message-id conversation recent-message-num) {"time":=time}
														time
													)
													(parse-time "%F" "2000-01-01")
												)
											)
										)
										(+ conversation-data {"recent-message-time":recent-message-time})
									)
								)
								conversations
							)
						)
					)
				)
			)
		)
	)

	(defun get-messages (message-ids:list)
		(map
			(lambda
				(
					message-id
				)
				(read message-table-1 message-id)
			)
			message-ids
		)
	)

	(defun get-conversations (conversation-ids:list)
		(map
			(lambda
				(
					conversation-id
				)
				(read conversation-table-1 conversation-id)
			)
			conversation-ids
		)
	)

	(defun get-accounts (account-ids:list)
		(map
			(lambda
				(
					account-id
				)
				(read account-table-1 account-id)
			)
			account-ids
		)
	)

	(defun change-password (your-account:string new-xor-private-key-password-hash:string)
		(with-read account-table-1 your-account {"guard":=guard}
			(enforce-guard guard)
		)
		(update account-table-1 your-account {"xor-private-key-password-hash":new-xor-private-key-password-hash})
	)

	(defun rotate-guard (your-account:string new-guard:guard)
		(with-read account-table-1 your-account {"guard":=old-guard}
			(enforce-guard old-guard)
		)
		(update account-table-1 your-account {"guard":new-guard})
	)

	(defun toggle-block-new-conversations (your-account:string)
		(with-read account-table-1 your-account {"guard":=guard,"block-new-conversations":=old-block-new-conversations}
			(enforce-guard guard)
			(update account-table-1 your-account {"block-new-conversations":(not old-block-new-conversations)})
		)
	)

	(defun toggle-active-flag (your-account:string conversation:string)
		(with-read account-table-1 your-account {"guard":=guard}
			(enforce-guard guard)
			(with-read conversation-table-1 conversation {"accounts-canonical":=accounts-canonical,"active-flags":=active-flags}
				(enforce (contains your-account accounts-canonical) "Must be a part of the conversation to change an active-flag")
				(let
					(
						(new-active-flags
							(if
								(= (at 0 accounts-canonical) your-account)
								[(not (at 0 active-flags)) (at 1 active-flags)]
								[(at 0 active-flags) (not (at 1 active-flags))]
							)
						)
					)
					(update conversation-table-1 conversation {"active-flags":new-active-flags})
				)
			)
		)
	)

	(defun account-id:string (username:string private-key-hash:string)
		(hash (+ (hash username) private-key-hash))
	)

	(defun conversation-id:string (account1:string account2:string)
		(hash
			(if
				(hash-canonical account1 account2)
				[account1 account2]
				[account2 account1]
			)
		)
	)

	(defun message-id:string (conversation-id:string message-num:integer)
		(hash (+ (hash message-num) conversation-id))
	)

	(defun hash-canonical:bool (first second)
		(<
			(str-to-int 64 (hash first))
			(str-to-int 64 (hash second))
		)
	)

	(defun hash-sort:list (stuff:list)
        (map
            (lambda
                (pair)
                (at "item" pair)
            )
            (sort
                ["hash"]
                (map
                    (lambda
                        (item)
                        {"item":item,"hash":(hash item)}
                    )
                    stuff
                )
            )
        )
    )
)


