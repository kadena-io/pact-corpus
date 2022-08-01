(module vesting 'admin-multi-keyset
  (use free.epyh-coin)

  (defcap IS_ADMIN ()
    (enforce-keyset 'admin-1))

  (defcap CAN_VEST () true)
  (defcap CAN_REDEEM () true)

  (defconst EPYH_BANK:string 'epyh-bank-1)
  (defun epyh-bank-guard () (keyset-ref-guard "admin-multi-keyset" ))

  ;WARNING THIS SHOULD BE CALCULATED
  (defconst EPYH_TOKENS 4000000.0)
  (defconst MAX_EPYH_ALLOCATION 1500.0)
  (defconst MIN_EPYH_ALLOCATION 0.00000001)
  (defconst EPYH_PROJECT_NAME "epyh")
  (defconst EPYH_START_DATE (time "1969-01-01T00:00:00Z"))
  (defconst EPYH_END_DATE (time "2069-01-01T00:00:00Z"))
  (defconst SWAP_RATE:decimal 100000.0)

  (defconst VESTING_TYPES {
    "IDO": [{"months": 0, "percent": 33}, {"months": 2, "percent": 33}, {"months": 5, "percent": 34}],
    "TEAM": [
        {"months": 6, "percent": 10},
        {"months": 7, "percent": 10},
        {"months": 8, "percent": 10},
        {"months": 9, "percent": 10},
        {"months": 10, "percent": 10},
        {"months": 11, "percent": 10},
        {"months": 12, "percent": 10},
        {"months": 13, "percent": 10},
        {"months": 14, "percent": 10},
        {"months": 15, "percent": 10}
    ],
    "ADVISOR": [
        {"months": 6, "percent": 10},
        {"months": 7, "percent": 10},
        {"months": 8, "percent": 10},
        {"months": 9, "percent": 10},
        {"months": 10, "percent": 10},
        {"months": 11, "percent": 10},
        {"months": 12, "percent": 10},
        {"months": 13, "percent": 10},
        {"months": 14, "percent": 10},
        {"months": 15, "percent": 10}
    ],
    "PARTNERSHIP": [
        {"months": 6, "percent": 25},
        {"months": 7, "percent": 5},
        {"months": 8, "percent": 5},
        {"months": 9, "percent": 5},
        {"months": 10, "percent": 5},
        {"months": 11, "percent": 5},
        {"months": 12, "percent": 5},
        {"months": 13, "percent": 5},
        {"months": 14, "percent": 5},
        {"months": 15, "percent": 5},
        {"months": 16, "percent": 5},
        {"months": 17, "percent": 5},
        {"months": 18, "percent": 5},
        {"months": 19, "percent": 5},
        {"months": 20, "percent": 5},
        {"months": 21, "percent": 5}

    ],
    "LIQUIDITY": [
        {"months": 6, "percent": 50},
        {"months": 7, "percent": 8},
        {"months": 8, "percent": 8},
        {"months": 9, "percent": 8},
        {"months": 10, "percent": 8},
        {"months": 11, "percent": 8},
        {"months": 12, "percent": 10}
    ],
    "RESERVE": [
        {"months": 12, "percent": 10},
        {"months": 13, "percent": 10},
        {"months": 14, "percent": 10},
        {"months": 15, "percent": 10},
        {"months": 16, "percent": 10},
        {"months": 17, "percent": 10},
        {"months": 18, "percent": 10},
        {"months": 19, "percent": 10},
        {"months": 20, "percent": 10},
        {"months": 21, "percent": 10}
    ]}
  )

  (defun get-swap-rate()
    SWAP_RATE
  )

  (defschema vesting
    key:string
    account:string
    redeem_date:time
    redeemed:bool
    amount:decimal
  )
  (deftable vestings-table:{vesting})

  (defschema allocation-schema
      account:string
      kda-amount:decimal
      epyh-amount:decimal
      vested:bool
  )
  (deftable allocations-table:{allocation-schema})

  (defun init ()
    (coin.create-account EPYH_BANK (epyh-bank-guard))
  )

  (defun get-min-allocation-amount(max-allocation:decimal)
    (if (< max-allocation MIN_EPYH_ALLOCATION)  max-allocation MIN_EPYH_ALLOCATION)
  )

  (defun get-max-allocation-amount()
    (let* (
        (allocated (fold (+) 0.0 (map (at "epyh-amount") (select allocations-table ["epyh-amount"] (where "epyh-amount" (> 0.0))))))
      )
      (if (> allocated (- EPYH_TOKENS MAX_EPYH_ALLOCATION))
        (- EPYH_TOKENS MAX_EPYH_ALLOCATION) MAX_EPYH_ALLOCATION)
    )
  )

  (defun can-invest (account:string)
    (and (community.has-kyc account) (lottery.is-next-in-queue account EPYH_PROJECT_NAME))
  )

  (defun create-allocation (kda-amount:decimal account:string)
    (let* (
        (epyh-amount (* kda-amount (get-swap-rate)))
        (allowed (can-invest account))
        (max-allocation (get-max-allocation-amount))
        (min-allocation (get-min-allocation-amount max-allocation))
      )

      (enforce (>= max-allocation epyh-amount) "Bigger than max allocation")
      (enforce (<= min-allocation epyh-amount) "Less than min allocation")
      (enforce allowed "User has not fulfilled Kyc or won lottery")

      (insert allocations-table account
        {"account": account,
          "kda-amount": kda-amount,
          "epyh-amount":  (* kda-amount SWAP_RATE),
          "vested": false}
      )

      (install-capability (coin.TRANSFER account EPYH_BANK kda-amount))
      (coin.transfer account EPYH_BANK kda-amount)

      (with-capability (CAN_VEST) (add-account-vesting-entries account epyh-amount "IDO"))
    )
  )

  (defun add-months-to-date (date:time months:integer)
    (add-time date (days (* 31 months)))
  )

  (defun get-amount-for-percent (amount:decimal percent:integer)
    (round  (* (/ (* percent 1.00) 100) amount) 2)
  )

  (defun get-vesting-entry-key (account:string redeem_date:time type:string)
    (hash (list account redeem_date type))
  )

  (defun build-vesting-entry-json (account:string curr_time:time vesting_type:string amount:decimal vesting_schedule)
    (require-capability (CAN_VEST))
    (let ((entry  {
          "key": (get-vesting-entry-key account (add-months-to-date curr_time (at "months" vesting_schedule)) vesting_type),
          "redeemed": false,
          "account": account,
          "redeem_date": (add-months-to-date curr_time (at "months" vesting_schedule)),
          "amount": (get-amount-for-percent amount (at "percent" vesting_schedule))
        }
      ))
      (insert vestings-table (at "key" entry) entry)
    )
  )

  (defun add-account-vesting-entries  (account:string amount:decimal vesting_type:string)
    (require-capability (CAN_VEST))
    (let* ((vesting_schedule (at vesting_type VESTING_TYPES))
        (curr_time:time (at 'block-time (chain-data)))
        (inserts (map (build-vesting-entry-json account curr_time vesting_type amount) vesting_schedule))
      )
      true
    )
  )

  (defun private-add-account-vesting-entries (account:string amount:decimal vesting_type:string)
    (with-capability (IS_ADMIN)
      (with-capability (CAN_VEST)
        (add-account-vesting-entries  account amount vesting_type)
      )
    )
  )

  ;call this function securely from epyh-coin
  (defun redeem-entry (key:string)
    (require-capability (CAN_REDEEM))
    (update vestings-table key {"redeemed": true})
  )

  (defun get-vesting-schedule (account: string curr_time: time)
    (select vestings-table (and? (where "account" (= account)) (and? (where "redeem_date" (>= curr_time)) (where "redeemed" (= false)))   ))
  )

  (defun redeem (account:string)
    (coin.validate-account account)
    (let* (
        (curr_time:time (at 'block-time (chain-data)))
        (vestings_to_redeem (get-vesting-schedule account curr_time))
        (amount_to_redeem (* 1.0 ( fold (+) 0.0 (map (at "amount") vestings_to_redeem))))
      )

      (enforce (> amount_to_redeem 0.0) "Nothing to redeem at this time")

      (install-capability (epyh-coin.TRANSFER GENESIS_ACCT account amount_to_redeem))
      (epyh-coin.transfer GENESIS_ACCT account amount_to_redeem)

      (with-capability (CAN_REDEEM)
        (map (redeem-entry) (map (at "key") vestings_to_redeem))
      )
      amount_to_redeem
    )
  )
)


