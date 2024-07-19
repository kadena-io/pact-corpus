(module e2ee-messaging GOV
	(defcap GOV ()
		(enforce-keyset "free.e2ee-messaging-admin")
	)

	(defschema account-schema
		username:string
		public-key:string
		xor-private-key-password-hash:string
		conversations:list
		block-new-conversations:bool
		guard:guard
	)

	(deftable account-table:{account-schema})

	(defschema conversation-schema
		accounts-canonical:list
		messages-count:integer
		active-flags:[bool]
	)

	(deftable conversation-table:{conversation-schema})

	(defschema message-schema
		direction-is-canonical:bool
		content:[object] ; list of 2 elements, first is the message encrypted with the first accounts key, and second is the message encrypted with the second accounts key.
		time:time
	)

	(deftable message-table:{message-schema})

	(defun create-account (username:string public-key:string private-key-hash:string xor-private-key-password-hash:string guard:guard)
		(insert account-table private-key-hash {"username":username,"public-key":public-key,"xor-private-key-password-hash":xor-private-key-password-hash,"conversations":[],"block-new-conversations":false,"guard":guard})
	)

	(defun create-conversation (your-account:string their-account:string)
		(enforce (not (= your-account their-account)) "Cannot create a conversation with yourself.")
		(let*
			(
				(their-account-data
					(with-read account-table their-account {"conversations":=conversations,"block-new-conversations":=block-new-conversations}
						{"conversations":conversations,"block-new-conversations":block-new-conversations}
					)
				)
				(your-account-data
					(with-read account-table your-account {"conversations":=conversations,"block-new-conversations":=block-new-conversations,"guard":=guard}
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
			(insert conversation-table conversation-id {"accounts-canonical":accounts-canonical,"messages-count":0,"active-flags":active-flags})
			(update account-table your-account {"conversations":(+ (at "conversations" your-account-data) [conversation-id])})
			(update account-table their-account {"conversations":(+ (at "conversations" their-account-data) [conversation-id])})
		)
	)

	(defun create-message (your-account:string conversation-id:string content:[object])
		(let*
			(
				(conversation-data:object
					(read conversation-table conversation-id)
				)
				(is-first:bool
					(= your-account (at 0 (at "accounts-canonical" conversation-data)))
				)
			)
			(with-read account-table your-account {"guard":=guard}
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
			(insert message-table (message-id conversation-id (at "messages-count" conversation-data)) {"direction-is-canonical":is-first,"content":content,"time":(at "block-time" (chain-data))})
			(update conversation-table conversation-id {"messages-count":(+ 1 (at "messages-count" conversation-data))})
		)
	)

	(defun get-chronological-conversations:list (account)
		(let
			(
				(conversations
					(with-read account-table account {"conversations":=conversations1}
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
												(+ (read conversation-table conversation) {"conversation-id":conversation})
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
													(with-read message-table (message-id conversation recent-message-num) {"time":=time}
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
				(read message-table message-id)
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
				(read conversation-table conversation-id)
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
				(read account-table account-id)
			)
			account-ids
		)
	)

	(defun change-password (your-account:string new-xor-private-key-password-hash:string)
		(with-read account-table your-account {"guard":=guard}
			(enforce-guard guard)
		)
		(update account-table your-account {"xor-private-key-password-hash":new-xor-private-key-password-hash})
	)

	(defun rotate-guard (your-account:string new-guard:guard)
		(with-read account-table your-account {"guard":=old-guard}
			(enforce-guard old-guard)
		)
		(update account-table your-account {"guard":new-guard})
	)

	(defun toggle-block-new-conversations (your-account:string)
		(with-read account-table your-account {"guard":=guard,"block-new-conversations":=old-block-new-conversations}
			(enforce-guard guard)
			(update account-table your-account {"block-new-conversations":(not old-block-new-conversations)})
		)
	)

	(defun toggle-active-flag (your-account:string conversation:string)
		(with-read account-table your-account {"guard":=guard}
			(enforce-guard guard)
			(with-read conversation-table conversation {"accounts-canonical":=accounts-canonical,"active-flags":=active-flags}
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
					(update conversation-table conversation {"active-flags":new-active-flags})
				)
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
)


