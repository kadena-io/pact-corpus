(module prod-hype-locker-v2 GOVERNANCE
  (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard "hypercent.hyper-admin-1-prod")))

  (implements hypercent.callable-v3)

  (defun call:guard (method:string)
    (create-module-guard "locker-guard")
  )
  
  (use hypercent.prod-fungible-util)

  (defschema lock
    account:string
    key:string
    amount:decimal
    unlock_date:time
    unlocked:bool)
  (deftable locks:{lock})

  (defschema lock-journal-item
    created-at:time
    account:string
    project:string
    locks:[string]
    total-amount:decimal
  )
  (deftable lock-journal:{lock-journal-item})

  (defcap CAN_LOCK () true)
  (defcap CAN_UNLOCK () true)

  (defcap HYPE_LOCKED (account:string project:string)
    @doc " For locking \
         \ account = \"k:account\" \
         \ project = \"kongz|bulls|etc\" "
    @event
    true
    )

  (defcap HYPE_UNLOCKED (account:string amount_unlocked:decimal)
    @doc " For locking \
         \ account = \"k:account\" \
         \ amount_unlocked = \"total amount unlocked\" "
    @event
    true
    )

  (defconst LOCK_ACCT:string "LOCK_ACCT_V2")
  (defun lock-guard () (create-module-guard 'LOCK_ACCT_V2))

  ;todo change 10 6
  (defconst LOCKING_SCHEDULE [
        {"days": 6, "percent": 10},
        {"days": 7, "percent": 10},
        {"days": 8, "percent": 10},
        {"days": 9, "percent": 10},
        {"days": 10, "percent": 10},
        {"days": 11, "percent": 10},
        {"days": 12, "percent": 10},
        {"days": 13, "percent": 10},
        {"days": 14, "percent": 10},
        {"days": 15, "percent": 10}
      ]
  )

  (defun init ()
    (hypercent.prod-hype-coin.create-account LOCK_ACCT (lock-guard))
    "empty lock succeeded"
  )

  (defun lock-hype:string (account:string project:string amount:decimal tickets:integer ga:integer)
    @doc
       " Lock specific amount of hype per project \
       \ Capability invoked to succeed nested method calls \
       \ returns true"
    (enforce-valid-transfer account LOCK_ACCT 12 amount)

    (install-capability (hypercent.prod-hype-coin.TRANSFER account LOCK_ACCT amount))
    (hypercent.prod-hype-coin.transfer account LOCK_ACCT amount)

    (with-capability (CAN_LOCK)
      (add-lock-entries account project amount (at 'block-time (chain-data)))
      (emit-event (HYPE_LOCKED account project))

      ; (hypercent.prod-community-v2.add-rewards-for-staking project account amount tickets ga)
      "WIP"
    )
  )

  (defun add-lock-entries (account:string project:string amount:decimal curr_time:time)
    (require-capability (CAN_LOCK))
    (let* ((lock-entries (map (build-lock-entry-json account project curr_time amount) LOCKING_SCHEDULE))
      )
      (insert lock-journal (get-lock-journal-entry-key account project)
        {
          "account": account,
          "project": project,
          "created-at": curr_time,
          "locks": (map (at "key") lock-entries),
          "total-amount": amount
        }
      )
      true
    )
  )

  (defun build-lock-entry-json (account:string project:string curr_time:time amount:decimal locking_schedule)
    @doc
     " Composes individual lock part to be release on specific date \
     \ Capability required to prevent extenal solo calls \
     \ returns {key: hash of [account project unlock_date], account = \"k:account\", \
     \ unlocked: false, unlock_date: 'day of unlock', amount: 10% of overall lock amount} as entry"
    (require-capability (CAN_LOCK))
    (let* (
      (unlock_date (add-days-to-date curr_time (at "days" locking_schedule)))
      (entry  {
          "key": (get-lock-entry-key account project unlock_date),
          "account": account,
          "unlocked": false,
          "unlock_date": unlock_date,
          "amount": (get-amount-for-percent amount (at "percent" locking_schedule))
        }
      ))
      (insert locks (at "key" entry) entry)
      entry
    )
  )

  ;change minutes to days
  (defun add-days-to-date (date:time num_days:integer)
    (add-time date (minutes num_days))
  )

  (defun get-amount-for-percent (amount:decimal percent:integer)
    (round  (* (/ (* percent 1.00) 100) amount) 2)
  )

  (defun get-lock-journal-entry-key (account:string project:string)
    (hash [account project])
  )

  (defun get-lock-entry-key (account:string project:string unlock_date:time)
    @doc
     " Compose what is effectively a compound primary key  \
     \ returns hash of account project unlock_date"
    (hash [account project unlock_date])
  )

  (defun unlock-hype (account:string project:string)
    @doc
     " Unlock all unlockable hype at current type for all projects \
     \ Capability invoked to succeed nested method calls \
     \ returns total amount unlocked"
    (coin.validate-account account)
    (let* (
        (curr_time:time (at 'block-time (chain-data)))
        (entries_to_unlock (get-unlockable-schedule account project curr_time))
        (amount_to_unlock (get-unlockable-amount account project))
      )

      (enforce (> amount_to_unlock 0.0) "Nothing to unlock at this time")

      (install-capability (hypercent.prod-hype-coin.TRANSFER LOCK_ACCT account amount_to_unlock))
      (hypercent.prod-hype-coin.transfer LOCK_ACCT account amount_to_unlock)

      (with-capability (CAN_UNLOCK)
        (map (unlock-entry) (map (at "key") entries_to_unlock))
        (emit-event (HYPE_UNLOCKED account amount_to_unlock))
      )

      amount_to_unlock
    )
  )


  (defun get-unlockable-schedule (account:string project:string curr_time:time)
    @doc
     " Get all lock parts that are unlockable as of current date  \
     \ returns {key: hash of [account project unlock_date], account = \"k:account\",\
     \ unlocked: false, unlock_date: 'day of unlock', amount: 10% of overall lock amount} as entry"
    (let* (
        (schedule (get-locks-for-account account project))
        (valid (filter (not-unlocked) (filter (is-entry-available curr_time) schedule)))
      )
      valid
    )
  )

  (defun get-entry (key:string)
    (read locks key)
  )

  (defun get-locks-for-account (account:string project:string)
    (map (get-entry) (at "locks" (get-lock-journal-entry account project)))
  )

  (defun get-lock-journal-entry (account:string project:string)
    (read lock-journal (get-lock-journal-entry-key account project))
  )

  (defun not-unlocked (entry:object)
    (= false (at "unlocked" entry))
  )

  (defun is-entry-available (curr_time:time entry:object)
    (>= curr_time  (at "unlock_date" entry))
  )

  (defun is-entry-unavailable (curr_time:time entry:object)
    (< curr_time  (at "unlock_date" entry))
  )

  (defun get-unlockable-amount (account:string project:string)
        (let* (
          (curr_time:time (at 'block-time (chain-data)))
          (entries_to_unlock (get-unlockable-schedule account project curr_time))
          (amount_to_unlock (* 1.0 ( fold (+) 0.0 (map (at "amount") entries_to_unlock))))
        )
          amount_to_unlock
      )
  )

  (defun unlock-entry (key:string)
    @doc
       " Simple method unset the lock \
       \ Capability required to prevent extenal solo calls \
       \ returns true"
    (require-capability (CAN_UNLOCK))
    (update locks key {"unlocked": true})
  )

  (defun get-remaining-locked-amount (account:string project:string)
    @doc
     " Get all lock parts that are not unlockable as of current date  \
     \ returns {key: hash of [account project unlock_date], account = \"k:account\",\
     \ unlocked: false, unlock_date: 'day of unlock', amount: 10% of overall lock amount} as entry"
    (let* (
        (schedule (get-locks-for-account account project))
        (entries_locked (filter (not-unlocked) schedule))
        (amount_locked (* 1.0 ( fold (+) 0.0 (map (at "amount") entries_locked))))
      )
      amount_locked
    )
  )
)


