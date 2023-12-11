(module vesting 'admin-multi-keyset
  (use free.hype-coin)

  (defcap IS_ADMIN ()
    (enforce-keyset 'admin-1))

  (defcap CAN_VEST () true)
  (defcap CAN_REDEEM () true)

  (defconst HYPE_BANK:string 'hype-bank-6)
  (defun hype-bank-guard () (keyset-ref-guard "admin-multi-keyset" ))

  ;WARNING THIS SHOULD BE CALCULATED
  (defconst HYPE_TOKENS 4000000.0)
  (defconst MAX_HYPE_ALLOCATION 1500.0)
  (defconst MIN_HYPE_ALLOCATION 100.0)
  (defconst HYPE_PROJECT_NAME "hype-6")
  (defconst SWAP_RATE:decimal 100.0)

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

  (defschema ledger-schema
    created-at:time
    type:string
    account:string
    slots:[string]
    amount:decimal
  )
  (deftable ledger-table:{ledger-schema})


  (defschema allocation-schema
      account:string
      kda-amount:decimal
      hype-amount:decimal
  )
  (deftable allocations-table:{allocation-schema})

  (defun init ()
    (coin.create-account HYPE_BANK (hype-bank-guard))
  )

  (defun get-min-allocation-amount(max-allocation:decimal)
    (if (< max-allocation MIN_HYPE_ALLOCATION)  max-allocation MIN_HYPE_ALLOCATION)
  )

  (defun get-max-allocation-amount()
    (let* (
        (allocated (kda-to-hype (coin.get-balance HYPE_BANK)))
      )
      (if (> allocated (- HYPE_TOKENS MAX_HYPE_ALLOCATION))
        (- HYPE_TOKENS MAX_HYPE_ALLOCATION) MAX_HYPE_ALLOCATION)
    )
  )

  (defun kda-to-hype (kda-amount:decimal)
      (* kda-amount (get-swap-rate))
  )

  (defun can-invest (account:string)
    (and (community.has-kyc account) (test-lottery.is-next-in-queue account HYPE_PROJECT_NAME))
  )

  (defun create-allocation (kda-amount:decimal account:string)
    (let* (
        (hype-amount (kda-to-hype kda-amount))
        (allowed (can-invest account))
        (max-allocation (get-max-allocation-amount))
        (min-allocation (get-min-allocation-amount max-allocation))
        (curr_time:time (at 'block-time (chain-data)))
      )

      (enforce (>= max-allocation hype-amount) "Bigger than max allocation")
      (enforce (<= min-allocation hype-amount) "Less than min allocation")
      (enforce allowed "User has not fulfilled Kyc or won lottery")

      (insert allocations-table account
        {"account": account,
          "kda-amount": kda-amount,
          "hype-amount":  (* kda-amount SWAP_RATE)
        }
      )


      (install-capability (coin.TRANSFER account HYPE_BANK kda-amount))
      (coin.transfer account HYPE_BANK kda-amount)

      (with-capability (CAN_VEST) (add-account-vesting-entries account hype-amount "IDO" curr_time))
    )
  )

  (defun get-allocation (account:string)
        (with-default-read allocations-table account {"kda-amount": 0} {"kda-amount":=kda-amount}
          kda-amount
        )
  )

  (defun add-months-to-date (date:time months:integer)
    (add-time date (days (* 31 months)))
  )

  (defun get-amount-for-percent (amount:decimal percent:integer)
    (round  (* (/ (* percent 1.00) 100) amount) 2)
  )

  (defun get-vesting-entry-key (account:string redeem_date:time type:string)
    (hash [account redeem_date type])
  )

  (defun build-vesting-entry-json (account:string curr_time:time vesting_type:string amount:decimal vesting_schedule)
    (require-capability (CAN_VEST))
    (let* (
      (redeem_date (add-months-to-date curr_time (at "months" vesting_schedule)))
      (entry  {
          "key": (get-vesting-entry-key account redeem_date vesting_type),
          "redeemed": false,
          "account": account,
          "redeem_date": redeem_date,
          "amount": (get-amount-for-percent amount (at "percent" vesting_schedule))
        }
      ))
      (insert vestings-table (at "key" entry) entry)
      entry
    )
  )

  (defun add-account-vesting-entries  (account:string amount:decimal vesting_type:string curr_time:time)
    (require-capability (CAN_VEST))
    (let* ((vesting_schedule (at vesting_type VESTING_TYPES))
        (inserts (map (build-vesting-entry-json account curr_time vesting_type amount) vesting_schedule))
      )
      inserts
    )
  )

  (defun team-vesting (account:string amount:decimal vesting_type:string)
        (with-capability (IS_ADMIN)
           (with-capability (CAN_VEST) (add-account-vesting-entries account amount vesting_type (at 'block-time (chain-data))))
        )
  )

  ;call this function securely from hype-coin
  (defun redeem-entry (key:string)
    (require-capability (CAN_REDEEM))
    (update vestings-table key {"redeemed": true})
  )

  (defun get-entry (key:string)
      (read vestings-table key)
  )

  (defun get-total-amount (account:string)
    (at "amount" (read ledger-table account))
  )


  (defun get-available-amount (account:string)
        (let* (
          (curr_time:time (at 'block-time (chain-data)))
          (vestings_to_redeem (get-vesting-schedule account curr_time))
          (amount_to_redeem (* 1.0 ( fold (+) 0.0 (map (at "amount") vestings_to_redeem))))
        )
          amount_to_redeem
      )
  )

  (defun is-entry-available (curr_time:time entry:object)
    (>= curr_time  (at "redeem_date" entry))
  )

  (defun not-redeemed (entry:object)
     (= false (at "redeemed" entry))
  )

  (defun get-vesting-schedule (account: string curr_time:time)
    (let* (
        (entry (read ledger-table account))
        (vesting_schedule (at (at "type" entry) VESTING_TYPES))
        (schedule (map (get-entry) (at "slots" entry)))
        (valid (filter (not-redeemed) (filter (is-entry-available curr_time) schedule)))
      )
      valid
    )
  )

  (defun redeem (account:string)
    (coin.validate-account account)
    (let* (
        (curr_time:time (at 'block-time (chain-data)))
        (vestings_to_redeem (get-vesting-schedule account curr_time))
        (amount_to_redeem (get-available-amount account))
      )

      (enforce (> amount_to_redeem 0.0) "Nothing to redeem at this time")

      (install-capability (hype-coin.TRANSFER GENESIS_ACCT account amount_to_redeem))
      (hype-coin.transfer GENESIS_ACCT account amount_to_redeem)

      (with-capability (CAN_REDEEM)
        (map (redeem-entry) (map (at "key") vestings_to_redeem))
      )
      amount_to_redeem
    )
  )
)


