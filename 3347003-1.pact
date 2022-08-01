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
    (with-default-read users-table account
      {"tickets":0,
        "kyc_status": 0,
        "rewards": []}
      {"tickets" := tickets,
        "kyc_status":=kyc_status,
        "rewards" := rewards}

      (write users-table account
        {"tickets": tickets,
          "kyc_status": kyc_status,
          "rewards": rewards
        }
      )
    )
  )

  (defun get-user(account:string)
    (read users-table account)
  )

  ;todo: prevent negative ammount
  (defun add-tickets(account:string tickets:integer)
    (with-capability (IS_ADMIN)
      (with-read users-table account
        {"tickets" := current_tickets_ammount}
        (enforce (<= 0 (+ tickets current_tickets_ammount)) "not enough tickets")
        (update users-table account {"tickets": (+ tickets current_tickets_ammount)})
      )
    )
  )

  (defun debit-tickets (account:string tickets:integer)
    (enforce-guard (at 'guard (free.hype-coin.details account)))
    (enforce (> tickets 0) "can't debit negative amount")
    (with-read users-table account
      {"tickets" := current_tickets_ammount}
      (enforce (>= (- current_tickets_ammount tickets) 0) "not enough tickets")
      (update users-table account {"tickets": (- current_tickets_ammount tickets)})
    )
  )

  (defun get-tickets(account: string)
    (with-default-read users-table account {"tickets": 0} {"tickets" := tickets}
      tickets
    )
  )

  ;admin only
  (defun claim-tickets-reward (account:string tickets:integer type:string)
    (require-capability (CAN_CLAIM_REWARD))
    (with-read users-table account {"rewards" := rewards}
      (if (= true (contains type rewards))
        false
        ;todo i don't know how to do this better; using let seems like a hack
        (let* (
            (add-reward (update users-table account {"rewards": (+ rewards [type])}))
            (add-tickets (add-tickets account tickets))
          )
          true
        )
      )
    )
  )

  ;admin only
  (defun claim-reward (account:string type:string)
    (require-capability (IS_ADMIN))
    (with-read users-table account {"rewards" := rewards}
      (if (= true (contains type rewards))
        false
        ;todo i don't know how to do this better; using let seems like a hack
        (let* (
            (add-reward (update users-table account {"rewards": (+ rewards [type])}))
          )
          true
        )
      )
    )
  )

  (defun has-reward (account:string type:string)
    (with-default-read users-table account {"rewards": []} {"rewards" := rewards}
      (contains type rewards)
    )
  )

  (defun has-ga (account:string project:string)
      (has-reward account (+ project "_GA"))
  )

  (defun get-telegram (telegram_hash:string)
    (with-default-read telegram-table telegram_hash
      {"account": ""}
      {"account" := existing_telegram_account}

      existing_telegram_account
    )
  )

  (defun get-telegram-account-key (account:string)
      (+ "account:" account)
  )

  (defun get-user-telegram(account: string)
    (with-default-read telegram-table (get-telegram-account-key account) {"telegram_hash": ""} {"telegram_hash":= telegram_hash}
        telegram_hash
    )
  )

  (defun unlink-telegram (account:string telegram_hash:string)
        (require-capability (IS_ADMIN))
        (write telegram-table telegram_hash {"account": "", "telegram_hash": telegram_hash})
        (write telegram-table (get-telegram-account-key account) {"telegram_hash": "", "account": account})
  )

  (defun link-telegram (account:string telegram_hash:string)
      (require-capability (IS_ADMIN))
      (write telegram-table telegram_hash {"account": account, "telegram_hash": telegram_hash})
      (write telegram-table (get-telegram-account-key account) {"account": account, "telegram_hash": telegram_hash})
  )

  (defun add-telegram(account:string telegram_hash:string tickets:integer has_ga:bool)
    ;enforce (= guard get guard from coin.details account)
    ;check if telegram hash in our database
    (with-capability (IS_ADMIN)
      (add-user account)
      (with-default-read telegram-table telegram_hash
        {"account": ""}
        {"account" := existing_telegram_account}
        ;this telegram id is new in our database!
        (with-default-read telegram-table (get-telegram-account-key account)
        {"telegram_hash":""}
        {"telegram_hash":=existing_telegram_hash}
          (unlink-telegram existing_telegram_account telegram_hash)
          (unlink-telegram account existing_telegram_hash)
          (link-telegram account telegram_hash)
          (if (and (= "" existing_telegram_account) (= "" existing_telegram_hash))
                  (with-capability (CAN_CLAIM_REWARD)
                    (claim-tickets-reward account tickets "telegram-login")
                    (if (= true has_ga)
                       (claim-reward account (+ "hype" "_GA"))
                        false
                    )
                  )
                  false
          )
        )
      )
    )
  )

  ;todo: capabilites to decide if this is available. only if user is winner in any project
  (defun get-kyc (account:string)
    (at "kyc_status" (details account))
  )

  (defun has-kyc(account:string)
    (= (get-kyc account) 2)
  )

  (defun enforce-has-kyc (account:string)
    (enforce (= (get-kyc account) 2) "Has kyc")
  )

  (defun set-kyc (account:string kyc_status:integer)
    (with-capability (IS_ADMIN)
      (update users-table account {"kyc_status": kyc_status})
    )
  )

  (defun details ( account:string )
    (with-read users-table account
      { "tickets" := tickets
      , "kyc_status" := kyc_status
      , "rewards" := rewards }
      { "account" : account
      , "tickets" : tickets
      , "kyc_status": kyc_status
      , "rewards": rewards })
  )
)


