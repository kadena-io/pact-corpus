(module ktoshi-vesting GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard
      (read-keyset 'ktoshi-admin-keyset)))

  (defcap OPS ()
    (enforce-guard
      (read-keyset 'ktoshi-ops-keyset)))

  (defcap CAN_VEST () true)
  (defcap CAN_REDEEM () true)

  (defconst KTOSHI_BANK:string 'ktoshi-bank)

  (defconst VESTING_START_DATE (time "2024-06-18T00:00:00Z"))

  (defconst UNLOCK_PERCENT:decimal (/ 100.0 9.0)) ;; 100% / TOTAL_MONTHS / UNLOCK_INTERVAL

  (defconst VESTING_PERIODS [
    {"months": 2, "percent": UNLOCK_PERCENT},
    {"months": 4, "percent": UNLOCK_PERCENT},
    {"months": 6, "percent": UNLOCK_PERCENT},
    {"months": 8, "percent": UNLOCK_PERCENT},
    {"months": 10, "percent": UNLOCK_PERCENT},
    {"months": 12, "percent": UNLOCK_PERCENT},
    {"months": 14, "percent": UNLOCK_PERCENT},
    {"months": 16, "percent": UNLOCK_PERCENT},
    {"months": 18, "percent": UNLOCK_PERCENT}
  ])

  (defconst VESTING_TYPES {
    "DAO": {
        "total": 4725000000000.0,
        "initial": 945000000000.0 
    },
    "REWARDS": {
        "total": 4725000000000.0,
        "initial": 945000000000.0 
    },
    "TEAM": {
        "total": 2100000000000.0,
        "initial": 420000000000.0 
    },
    "ADVISORS": {
        "total": 1050000000000.0,
        "initial": 1039500000000.0 
    }
  })

  (defschema lockup
    amount:decimal
    redeem_date:time
    redeemed:bool
  )

  (defschema vesting
    type:string
    account:string
    guard:guard
    lockups:[object{lockup}]
  )
  (deftable vestings-table:{vesting})

  (defun details:[object{vesting}] (type:string)
    (select vestings-table (where 'type (= type)))
  )

  (defun add-months-to-date (date:time months:integer)
    (add-time date (days (* 31 months)))
  )

  (defun get-amount-for-percent (amount:decimal percent:decimal)
    (round  (* (/ percent 100) amount) 2)
  )

  (defun build-vesting-entry (account:string guard:guard vesting_type:string amount:decimal vesting_schedule)
    (let* (
        (lockup  {
          "redeemed": false,
          "redeem_date": (add-months-to-date VESTING_START_DATE (at "months" vesting_schedule)),
          "amount": (get-amount-for-percent amount (at "percent" vesting_schedule))
        })
      )
      lockup
    )
  )

  (defun add-account-vesting-entries (account:string guard:guard vesting_type:string)
    (require-capability (CAN_VEST))
    (enforce (contains vesting_type VESTING_TYPES) "Invalid vesting type")
    (let* (
        (vesting_type_data (at vesting_type VESTING_TYPES))
        (initialUnlock (at 'initial vesting_type_data))
        (totalUnlock (at 'total vesting_type_data))
        (amountForUnlocks (- totalUnlock initialUnlock))
        (initialLockup (build-vesting-entry account guard vesting_type initialUnlock {"months": 0, "percent": 100.0} ))
        (restLockups (map (build-vesting-entry account guard vesting_type amountForUnlocks) VESTING_PERIODS))
        (entry  {
          "type": vesting_type,
          "account": account,
          "guard": guard,
          "lockups": (+ [initialLockup] restLockups)
        })
      )
      (insert vestings-table vesting_type entry)
      "added"
    )
  )

  (defun add-vesting-accounts (account:string guard:guard vesting_type:string)
    (with-capability (OPS)
      (with-capability (CAN_VEST)
        (let (
          (typeCompRec (details vesting_type))
        )
          (enforce (= (length typeCompRec) 0) "Vesting type already exist")
          (add-account-vesting-entries account guard vesting_type)
        )
      )
    )
  )

  (defun get-available-positions:[object{vesting}] (type:string)
    (let*
      (
        (vesting_data (read vestings-table type))
        (lockups (at 'lockups vesting_data))
        (unpayed (filter (compose (at 'redeemed) (= false)) lockups))
        (curr_time:time (at 'block-time (chain-data)))
      )
      (filter (compose (at 'redeem_date) (>= curr_time)) unpayed)
    )
  )

  (defun update-lockup-and-transfer (account:string guard:guard curr_time:time pos:object{lockup})
    (require-capability (CAN_REDEEM))
    (let* 
      (
        (amount (at 'amount pos))
        (redeem_date (at 'redeem_date pos))
        (oldRedeemed (at 'redeemed pos))
        (new-lockup {
          "amount": amount,
          "redeem_date": redeem_date,
          "redeemed": (>= curr_time redeem_date)
        })
      )
      (if (and (= oldRedeemed false) (= (at 'redeemed new-lockup) true))
        (let (
          (lockup new-lockup)
        )
          (install-capability (free.ktoshi.TRANSFER KTOSHI_BANK account amount))
          (free.ktoshi.transfer-create KTOSHI_BANK account guard amount)
          lockup
        )
        pos
      )
    )
  )

  (defun redeem-for-type (type:string)
    (enforce (contains type VESTING_TYPES) "Invalid vesting type")
    (with-capability (OPS)
      (with-capability (CAN_REDEEM)
        (let* (
          (vesting_data (read vestings-table type))
          (account (at 'account vesting_data))
          (guard (at 'guard vesting_data))
          (lockups (at 'lockups vesting_data))
          (unpayed (filter (compose (at 'redeemed) (= false)) lockups))
          (curr_time:time (at 'block-time (chain-data)))
          (available (filter (compose (at 'redeem_date) (>= curr_time)) unpayed))
        )
          (enforce (> (length available) 0) "No available")
          (let 
            (
              (updated-lockups (map (update-lockup-and-transfer account guard curr_time) lockups))
            )
            (update vestings-table type { "lockups": updated-lockups })
          )
        )
      )
    )
  )

  (defun get-start-time ()
    VESTING_START_DATE
  )

  (defun get-vesting-types () 
    VESTING_TYPES
  )

  (defun get-vesting-periods () 
    VESTING_PERIODS
  )

)


