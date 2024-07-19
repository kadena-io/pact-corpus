(module community 'admin-multi-keyset
  (defcap IS_ADMIN ()
    (enforce-keyset 'admin-1))

  (defcap CAN_CLAIM_REWARD()
    true)

  (defcap REGISTER()
    true)

  (defcap CANT_CLAIM_REWARD()
    true)

  (defschema telegram-user
    account:string
    telegram_hash:string
  )
  (deftable telegram-table:{telegram-user})

  (defschema user
    tickets:integer
    rewards:[string]
    kyc_status:integer ;0 - none; 1 - in progress; 2 - it's done; 3 - has error
  )
  (deftable users-table:{user})

  (defun add-user(account:string)
    (require-capability (IS_ADMIN))
    (format "Hype IDO has been paused" [])
  )

  (defun get-user(account:string)
    (format "Hype IDO has been paused" [])
  )


  ;todo: prevent negative ammount
  (defun add-tickets(account:string tickets:integer)
    (with-capability (IS_ADMIN)
      (format "Hype IDO has been paused" [])
    )
  )

  (defun debit-tickets (account:string tickets:integer)
    (format "Hype IDO has been paused" [])
  )

  (defun get-tickets(account: string)
    (format "Hype IDO has been paused" [])
  )

  ;admin only
  (defun claim-tickets-reward (account:string tickets:integer type:string)
    (require-capability (CAN_CLAIM_REWARD))
    (format "Hype IDO has been paused" [])
  )

  ;admin only
  (defun claim-reward (account:string type:string)
    (require-capability (IS_ADMIN))
    (format "Hype IDO has been paused" [])
  )

  (defun has-reward (account:string type:string)
    (format "Hype IDO has been paused" [])
  )


  (defun has-ga (account:string project:string)
    (format "Hype IDO has been paused" [])
  )

  (defun get-telegram (telegram_hash:string)
    (format "Hype IDO has been paused" [])
  )

  (defun get-telegram-account-key (account:string)
    (format "Hype IDO has been paused" [])
  )

  (defun get-user-telegram(account: string)
    (format "Hype IDO has been paused" [])
  )

  (defun unlink-telegram (account:string telegram_hash:string)
    (require-capability (IS_ADMIN))
    (format "Hype IDO has been paused" [])
  )

  (defun link-telegram (account:string telegram_hash:string)
    (require-capability (IS_ADMIN))
    (format "Hype IDO has been paused" [])
  )

  (defun add-telegram(account:string telegram_hash:string tickets:integer has_ga:bool)
    ;enforce (= guard get guard from coin.details account)
    ;check if telegram hash in our database
    (with-capability (IS_ADMIN)
      (format "Hype IDO has been paused" [])
    )
  )


  ;todo: capabilites to decide if this is available. only if user is winner in any project
  (defun get-kyc (account:string)
    (format "Hype IDO has been paused" [])
  )

  (defun has-kyc(account:string)
    (format "Hype IDO has been paused" [])
  )

  (defun enforce-has-kyc (account:string)
    (format "Hype IDO has been paused" [])
  )

  (defun set-kyc (account:string kyc_status:integer)
    (with-capability (IS_ADMIN)
      (format "Hype IDO has been paused" [])
    )
  )

  (defun details ( account:string )
    (format "Hype IDO has been paused" [])
  )
)


