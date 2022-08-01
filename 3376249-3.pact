(module backalley-staking GOVERNANCE

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema ledger-schema
    balance:decimal
    updated:time
  )
  (deftable ledger:{ledger-schema})

  (defschema staking-schema
    reserve:decimal
    balance:decimal
    apr:decimal
    open:bool
  )
  (deftable staking:{staking-schema})

  (defschema rewards-schema
    reward:decimal
    withdrawn:decimal
    updated:time
  )
  (deftable rewards:{rewards-schema})


  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    (enforce-keyset 'backalley-staking-admin)
  )

  (defcap AUTH (account:string)
    @managed
    (enforce-guard (at "guard" (backalley.details account)))
    (with-read staking 'k { "open" := open }
      (enforce open  "Staking is paused")
    )
  )


  ; --------------------------------------------------------------------------
  ; Admin Constants

  (defconst STAKING_ACCT:string "backalley-staking-acct")
  (defconst RESERVE_ACCT:string "backalley-reserve-acct")
  (defconst DAY_IN_SECS:decimal 86400.0)
  (defconst STAKE_LOCKUP_PERIOD:integer 30)
  (defconst REWARD_PRECISION:integer 6)
  (defconst MIN_STAKE_AMOUNT:decimal 250.0)


  ; --------------------------------------------------------------------------
  ; Contract

  (defun staking-guard:guard () (create-module-guard "backalley-staking-guard"))
  (defun reserve-guard:guard () (create-module-guard "backalley-reserve-guard"))


  ; --------------------------------------------------------------------------
  ; Initialize

  (defun init ()
    (with-capability (GOVERNANCE)
      (backalley.create-account STAKING_ACCT (staking-guard))
      (backalley.create-account RESERVE_ACCT (reserve-guard))

      (insert staking 'k
        { "reserve": 0.0
        , "balance": 0.0
        , "apr": 0.0005479452055
        , "open": false }
      )
    )
  )


  ; --------------------------------------------------------------------------
  ; Stake

  (defun add-stake (account:string amount:decimal)
    (with-capability (AUTH account)

      (enforce (>= amount MIN_STAKE_AMOUNT) "Less than minimum stake amount")

      (backalley.transfer account STAKING_ACCT amount)

      ; update staking table
      (with-read staking 'k
        { "reserve" := reserve, "balance" := balance }
        (update staking 'k
          { "reserve": reserve
          , "balance": (+ balance amount) }
        )
      )

      ; update rewards table
      (update-rewards account)

      ; update ledger table
      (with-default-read ledger account
        { "balance": 0.0 } { "balance" := staked }
        (write ledger account
          { "balance": (+ staked amount)
          , "updated": (chain-time) }
        )
      )

    )

  )

  (defun withdraw-stake (account:string)
    (with-capability (AUTH account)

      (with-read ledger account { "balance" := stake, "updated" := last-stake }

        (enforce (>= (days-elapsed (chain-time) last-stake) STAKE_LOCKUP_PERIOD)
          "Must wait stake lockup period to withdraw")

        (install-capability (backalley.TRANSFER STAKING_ACCT account stake))
        (backalley.transfer STAKING_ACCT account stake)
        (update ledger account { "balance": 0.0, "updated": (chain-time) })
        (with-read staking 'k { "balance" := balance }
          (update staking 'k { "balance": (- balance stake) })
        )
      )
    )
  )

  ; --------------------------------------------------------------------------
  ; Rewards

  (defun withdraw-rewards (account:string)
    (with-capability (AUTH account)

      (update-rewards account)
      (with-read rewards account
        { "reward" := reward, "updated" := updated, "withdrawn" := withdrawn }
        (install-capability (backalley.TRANSFER RESERVE_ACCT account reward))
        (backalley.transfer RESERVE_ACCT account reward)
        (update rewards account
          { "reward": 0.0, "withdrawn": (+ withdrawn reward) }
        )
        (with-read staking 'k { "reserve" := reserve }
          (update staking 'k { "reserve": (- reserve reward) })
        )
      )
    )
  )

  (defun update-rewards (account:string)
    (let ((curr-time (chain-time)))
      (with-default-read rewards account
        { "reward": 0.0, "updated": curr-time, "withdrawn": 0.0 }
        { "reward" := reward, "updated" := updated, "withdrawn" := withdrawn }
        (with-default-read ledger account
          { "balance": 0.0 } { "balance" := stake }
          (let
            (
              (elapsed (days-elapsed curr-time updated))
              (apr (at "apr" (read staking 'k)))
            )
            (write rewards account
              { "reward": (floor (+ reward (* stake (* apr elapsed))) REWARD_PRECISION)
              , "updated": (parse-time "%F" (format-time "%F" curr-time))
              , "withdrawn": withdrawn }
            )
          )
        )
      )
    )
  )

  ; --------------------------------------------------------------------------
  ; Reserve

  (defun add-reserve (account:string amount:decimal)
    (with-capability (GOVERNANCE)
      (backalley.transfer account RESERVE_ACCT amount)

      (with-read staking 'k { "reserve" := reserve }
        (update staking 'k { "reserve": (+ reserve amount) })
      )
    )
  )

  (defun withdraw-reserve (account:string amount:decimal)
    (with-capability (GOVERNANCE)
      (install-capability (backalley.TRANSFER RESERVE_ACCT account amount))
      (backalley.transfer RESERVE_ACCT account amount)

      (with-read staking 'k { "reserve" := reserve }
        (update staking 'k { "reserve": (- reserve amount) })
      )
    )
  )

  ; --------------------------------------------------------------------------
  ; Operational

  (defun pause-staking ()
    (with-capability (GOVERNANCE)
      (update staking 'k { "open": false })
    )
  )

  (defun resume-staking ()
    (with-capability (GOVERNANCE)
      (update staking 'k { "open": true })
    )
  )

  (defun update-apr (apr:decimal)
    (with-capability (GOVERNANCE)
      (update staking 'k { "apr": apr })
    )
  )

  (defun update-rewards-admin (account:string)
    (with-capability (GOVERNANCE)
      (update-rewards account)
    )
  )

  (defun update-all-rewards-admin ()
    (with-capability (GOVERNANCE)
      (map (update-rewards-admin) (keys rewards))
    )
  )

  (defun stop-rewards ()
    (with-capability (GOVERNANCE)
      (update-all-rewards-admin)
      (update-apr 0.0)
    )
  )

  (defun resume-rewards (apr:decimal)
    (with-capability (GOVERNANCE)
      (update-all-rewards-admin)
      (update-apr apr)
    )
  )

  ; --------------------------------------------------------------------------
  ; Utilities

  (defun chain-time:time ()
    (at "block-time" (chain-data))
  )

  (defun get-staker (account:string)
    (with-read ledger account
      { "balance" := staked, "updated" := ledger-update }
      (with-read rewards account
        { "reward" := reward
        , "withdrawn" := withdrawn
        , "updated" := rewards-update }

        { "account": account
        , "stakedAmount": staked
        , "ledgerUpdated": ledger-update
        , "rewardsAvailable": reward
        , "rewardsWithdrawn": withdrawn
        , "rewardsUpdated": rewards-update }
      )
    )
  )

  (defun get-stakers ()
    (map (get-staker) (keys ledger))
  )

  (defun get-contract-info ()
    (with-read staking 'k
      { "reserve" := reserve
      , "balance" := balance
      , "apr" := apr
      , "open" := open }

      { "reserve": reserve
      , "totalStaked": balance
      , "dailyApr": apr
      , "isOpen": open
      , "stakeLockupPeriod": STAKE_LOCKUP_PERIOD
      , "rewardsDecimalPrecision": REWARD_PRECISION
      , "minStakeAmount": MIN_STAKE_AMOUNT
      }
    )
  )

  (defun days-elapsed:integer (curr-time:time base-date:time)
    (floor (/ (diff-time curr-time base-date) DAY_IN_SECS))
  )

)


