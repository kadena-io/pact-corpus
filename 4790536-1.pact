(module prod-hype-locker 'hyper-admin-multi-keyset-prod
  (implements callable)
  (use hypercent.prod-hype-coin)
  (use hypercent.prod-community-v2)

  (use hypercent.prod-fungible-util)


  (defun call (method:string arguments)
      (create-module-guard "locker-guard")
  )

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
  (defcap IS_ADMIN ()
    (enforce-keyset 'hyper-api-admin-prod))

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

  (defconst LOCK_ACCT:string "LOCK_ACCT")
  (defun lock-guard () (create-module-guard 'LOCK_ACCT))

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
    (enforce-valid-transfer account LOCK_ACCT (precision) amount)

    (install-capability (hypercent.prod-hype-coin.TRANSFER account LOCK_ACCT amount))
    (hypercent.prod-hype-coin.transfer account LOCK_ACCT amount)

    (with-capability (CAN_LOCK)
      (add-lock-entries account project amount (at 'block-time (chain-data)))
      (emit-event (HYPE_LOCKED account project))

      (hypercent.prod-community-v2.add-rewards-for-staking project account amount tickets ga)
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

  (defun add-days-to-date (date:time num_days:integer)
    (add-time date (days num_days))
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

  (defun fix-unlock (account: string)
    (with-capability (IS_ADMIN)
      (let* (
          (locks (get-locks-for-account account))
          (entries_to_lock (filter (should-be-locked) locks))
        )
          (with-capability (CAN_UNLOCK)
            (map (lock-entry) entries_to_lock)
          )
      true
      )
    )
  )

  (defun should-be-locked (lock_entry:object[lock])
    (let* (
      (unlock_date (at "unlock_date" lock_entry))
      (fixed_time (time "2022-07-17T12:00:00Z"))
    )
      (< fixed_time unlock_date)
    )
  )

  (defun lock-entry (entry:object[lock])
    (require-capability (CAN_UNLOCK))
    (update locks (at "key" entry) {"unlocked": false})
  )

  (defun unlock-hype (account:string keys_to_unlock:[string])
    @doc
     " Unlock all unlockable hype at current type for all projects \
     \ Capability invoked to succeed nested method calls \
     \ returns total amount unlocked"
    (coin.validate-account account)
    (let* (
        (curr_time:time (at 'block-time (chain-data)))
        (amount_to_unlock (get-unlockable-amount account keys_to_unlock))
        (entries_to_unlock (filter (not-unlocked-by-key) (filter (is-entry-available curr_time) keys_to_unlock)))
        (valid_entries_to_unlock (filter (is-entry-same-account account) entries_to_unlock))
      )

      (enforce (> amount_to_unlock 0.0) "Nothing to unlock at this time")

      (install-capability (hypercent.prod-hype-coin.TRANSFER LOCK_ACCT account amount_to_unlock))
      (hypercent.prod-hype-coin.transfer LOCK_ACCT account amount_to_unlock)

      (with-capability (CAN_UNLOCK)
        (map (unlock-entry) valid_entries_to_unlock)
        (emit-event (HYPE_UNLOCKED account amount_to_unlock))
      )

      amount_to_unlock
    )
  )

  (defun get-unlockable-schedule (account:string curr_time:time)
    @doc
     " Get all lock parts that are unlockable as of current date  \
     \ returns {key: hash of [account project unlock_date], account = \"k:account\",\
     \ unlocked: false, unlock_date: 'day of unlock', amount: 10% of overall lock amount} as entry"
    (let* (
        (schedule (get-locks-for-account account))
        (valid (filter (not-unlocked) (filter (is-entry-available curr_time) schedule)))
      )
      valid
    )
  )

  (defun get-entry (key:string)
    (read locks key)
  )

  (defun get-locks-for-account (account:string)
    (select locks (where 'account (= account)))
  )

  (defun get-lock-journal-entry (account:string project:string)
    (read lock-journal (get-lock-journal-entry-key account project))
  )

  (defun not-unlocked (entry:object)
    (= false (at "unlocked" entry))
  )

  (defun not-unlocked-by-key (key:string)
    (= false  (at 'unlocked (read locks key)))
  )

  (defun is-entry-available (curr_time:time key:string)
    (>= curr_time  (at 'unlock_date (read locks key)))
  )

  (defun is-entry-same-account (account:string key:string)
    (= account  (at 'account (read locks key)))
  )

  (defun is-entry-unavailable (curr_time:time entry:object)
    (< curr_time  (at "unlock_date" entry))
  )

  (defun get-amount-for-key (key:string)
    (at 'amount (read locks key))
  )

  (defun get-unlockable-amount (account:string keys_to_unlock:[string])
        (let* (
          (curr_time:time (at 'block-time (chain-data)))
          (available_keys (filter (not-unlocked-by-key) (filter (is-entry-available curr_time) keys_to_unlock)))
          (valid (filter (is-entry-same-account account) available_keys))
          (amount_to_unlock (* 1.0 ( fold (+) 0.0 (map (get-amount-for-key) valid))))
        )
          amount_to_unlock
      )
  )

  (defun unlock-entry (entry:object[lock])
    @doc
       " Simple method unset the lock \
       \ Capability required to prevent extenal solo calls \
       \ returns true"
    (require-capability (CAN_UNLOCK))
    (update locks (at "key" entry) {"unlocked": true})
  )

  (defun get-remaining-redeemable-locked-amount (account:string)
    @doc
     " Get all lock parts that are not unlockable as of current date  \
     \ returns {key: hash of [account project unlock_date], account = \"k:account\",\
     \ unlocked: false, unlock_date: 'day of unlock', amount: 10% of overall lock amount} as entry"
    (let* (
        (curr_time:time (at 'block-time (chain-data)))
        (schedule (get-locks-for-account account))
        (entries_locked (filter (not-unlocked) schedule))
        (amount_locked (* 1.0 ( fold (+) 0.0 (map (at "amount") entries_locked))))
      )
      amount_locked
    )
  )

  (defun init-account-demo (account:string guard:guard)
    (coin.validate-account account)
    (let* (
        (init_amount 5000.0)
      )

      (hypercent.prod-hype-coin.create-account account guard)

      (install-capability (hypercent.prod-hype-coin.TRANSFER GENESIS_ACCT account init_amount))
      (hypercent.prod-hype-coin.transfer GENESIS_ACCT account init_amount)

      init_amount
    )
  )
)


