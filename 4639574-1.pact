(module tfm-mok-vesting 'mfk-hype-admin-multi-keyset
  (use free.tfm-mok-token)
  (defcap CAN_VEST () true)
  (defcap CAN_REDEEM () true)
  (defcap IS_ADMIN ()
    (enforce-keyset 'mfk-hype-admin-1))

  (defconst MOK_BANK:string 'tfm-mok-bank)
  (defun mok-bank-guard () (create-module-guard "MOK_HYPE_ESCROW" ))

  (defconst MOK_HYPE_ESCROW:string 'tfm-mok-hype-escrow)
  (defun mok-hype-escrow-guard () (create-module-guard "MOK_BANK" ))

  (defconst RESERVED_MOK_TOKENS (read-msg 'num-reserved-tokens))
  (defconst MOK_PROJECT_NAME "mok")
  (defconst VESTING_START_DATE:time (time (read-msg 'start-date)))

  (defconst VESTING_TYPES {
    "IDO": [{"months": 0, "percent": 50}, {"months": 1, "percent": 25}, {"months": 2, "percent": 25}],
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
    ]}
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
      mok-amount:decimal
      round:integer
  )
  (deftable allocations-table:{allocation-schema})

  (defschema meta-data-schema
      kda-price:decimal
      mok-kda-parity:decimal
  )
  (deftable meta-data:{meta-data-schema})

  (defun init ()
    (coin.create-account MOK_BANK (mok-bank-guard))
    ;(free.tfm-mok-token.create-account MOK_HYPE_ESCROW (mok-hype-escrow-guard))
  )

  ;"******************* General Util Methods *******************"
  (defun kda-to-mok (kda-amount:decimal)
      (* kda-amount (get-mok-kda-parity))
  )

  (defun mok-to-kda (mok-amount:decimal)
      (/ mok-amount (get-mok-kda-parity))
  )

  (defun get-total-amount (account:string)
    (at "amount" (read ledger-table account))
  )

  ;"******************* Allocation Method *******************"
  (defun get-allocation (account:string)
        (with-read allocations-table account
          {"kda-amount":=kda-amount,
            "mok-amount":=mok-amount}
          { "kda-amount" : kda-amount
            , "mok-amount" : mok-amount })
  )

  (defun create-allocation (kda-amount:decimal account:string)

    (let* (
        (round (free.tfm-community.get-project-round MOK_PROJECT_NAME))
        (mok-amount (kda-to-mok kda-amount))
        (allowed (can-invest MOK_PROJECT_NAME account))
        (max-allocation (get-max-allocation-amount MOK_PROJECT_NAME account))
        (min-allocation (get-min-allocation-amount))
        (curr_time:time VESTING_START_DATE)
      )


      (enforce allowed "User has not fulfilled Kyc or won lottery")
      (enforce (>= max-allocation kda-amount) (format "Bigger than max allocation: Max allowed {} kda" [max-allocation]))
      (enforce (<= min-allocation kda-amount) (format "Less than min allocation: Use {} kda" [min-allocation]))

      (with-default-read allocations-table account {"kda-amount": 0, "round": 0, "mok-amount":0} {"kda-amount":=last-kda-amount, "round":=last-round, "mok-amount":=last-mok-amount}
        (enforce (!= round last-round) "Already created an allocation in this round")
        (write allocations-table account
          {
            "round": round,
            "account": account,
            "kda-amount": (+ kda-amount last-kda-amount),
            "mok-amount":  (* (+ kda-amount last-kda-amount) (get-mok-kda-parity))
          }
        )
        (install-capability (coin.TRANSFER account MOK_BANK kda-amount))
        (coin.transfer account MOK_BANK kda-amount)
        (with-capability (CAN_VEST)
          (add-account-vesting-entries account (+ last-mok-amount mok-amount) "IDO" curr_time)
          (redeem account)
        )
      )
    )
  )


 ;"******************* Allocation Util Methods *******************"
  (defun get-one-ga-mok-worth ()
      (* (get-allocation-price) (get-mok-kda-parity))
  )

  (defun get-mok-left ()
      (- (mok-to-kda RESERVED_MOK_TOKENS) (coin.get-balance MOK_BANK) )
  )

  (defun get-min-allocation-amount()
    (let* (
        (mok-left (get-mok-left))
        (one-ga-mok-worth (get-one-ga-mok-worth))
      )
      (if (> mok-left one-ga-mok-worth)
        (get-allocation-price) (* (/ mok-left (get-mok-kda-parity)) (get-allocation-price)))
    )
  )

  (defun get-participant-ga (project:string account:string)
      (let* (
        (ga (free.tfm-community.get-participant-ga project account))
        (round (free.tfm-community.get-project-round project))
      )
        (if (!= round 1) 1 ga)
      )
  )
  (defun get-max-allocation-amount(project:string account:string)
    (let* (
      (ga (get-participant-ga project account))
      (total-ga-amount (if (= ga 0) (get-allocation-price) (* (get-allocation-price) ga)))
      (mok-worth-kda-left (/ (get-mok-left) (get-mok-kda-parity)))
    )
      (if (> mok-worth-kda-left total-ga-amount) total-ga-amount mok-worth-kda-left)
    )
  )
  (defun can-invest (project:string account:string)
    (free.tfm-community.is-round-winner project account)
  )

  (defun set-allocation-price (kdaPrice:decimal)
    (with-capability (IS_ADMIN)
      (update meta-data 'mok-meta-data
        { "kda-price" : kdaPrice
        })
    )
  )

  (defun get-allocation-price ()
      (at "kda-price" (read meta-data "mok-meta-data"))
  )

  (defun set-mok-kda-parity (parity:decimal)
    (with-capability (IS_ADMIN)
      (update meta-data 'mok-meta-data
        { "mok-kda-parity" : parity
        })
    )
  )

  (defun set-meta-data (parity:decimal kdaPrice:decimal)
    (with-capability (IS_ADMIN)
        (write meta-data 'mok-meta-data
          {
            "mok-kda-parity" : parity,
            "kda-price": kdaPrice
          })
    )
  )

  (defun get-mok-kda-parity ()
      (at "mok-kda-parity" (read meta-data "mok-meta-data" ))
  )

  ;"******************* Vesting Entry Methods *******************"
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
      (write vestings-table (at "key" entry) entry)
      entry
    )
  )

  (defun add-account-vesting-entries (account:string amount:decimal vesting_type:string curr_time:time)
    (require-capability (CAN_VEST))
    (let* ((vesting_schedule (at vesting_type VESTING_TYPES))
        (inserts (map (build-vesting-entry-json account curr_time vesting_type amount) vesting_schedule))
      )
      (write ledger-table account
        {
          "account": account,
          "created-at": curr_time,
          "type": "IDO",
          "slots": (map (at "key") inserts),
          "amount": amount
        }
      )
      true
    )
  )

  ;"******************* Vesting Entry Util Methods *******************"
  (defun add-months-to-date (date:time months:integer)
    (add-time date (days (* 31 months)))
  )

  (defun get-amount-for-percent (amount:decimal percent:integer)
    (round  (* (/ (* percent 1.00) 100) amount) 8)
  )

  (defun get-vesting-entry-key (account:string redeem_date:time type:string)
    (hash [account redeem_date type])
  )

  (defun get-entry (key:string)
      (read vestings-table key)
  )

  ;"******************* Redeem Methods *******************"
  (defun redeem (account:string)
    (coin.validate-account account)
    (let* (
        (curr_time:time (at 'block-time (chain-data)))
        (vestings_to_redeem (get-vesting-schedule account curr_time))
        (amount_to_redeem (get-available-amount account))
      )

      (enforce (> amount_to_redeem 0.0) "Nothing to redeem at this time")

      (install-capability (free.tfm-mok-token.TRANSFER MOK_HYPE_ESCROW account amount_to_redeem))
      (free.tfm-mok-token.transfer MOK_HYPE_ESCROW account amount_to_redeem)

      (with-capability (CAN_REDEEM)
        (map (redeem-entry) (map (at "key") vestings_to_redeem))
      )
      amount_to_redeem
    )
  )

  ;"******************* Redeem Util Methods *******************"
  (defun is-entry-available (curr_time:time entry:object)
    (>= curr_time  (at "redeem_date" entry))
  )

  (defun redeem-entry (key:string)
    (require-capability (CAN_REDEEM))
    (update vestings-table key {"redeemed": true})
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

  (defun get-remaining-redeemable-amount (account:string)
      (let* (
        ;effective hack to pass in a date far into the future to get all possible remaining mok
        (vestings_to_redeem (get-vesting-schedule account (time "2100-01-01T00:00:00Z")))
        (amount_to_redeem (* 1.0 ( fold (+) 0.0 (map (at "amount") vestings_to_redeem))))
      )
        amount_to_redeem
    )
  )
)


