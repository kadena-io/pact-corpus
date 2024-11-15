(module staking GOVERNANCE

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  ;define stakes schema
  (defschema stakes
    account:string
    stakeAmount:decimal
    stakeDate:time
    rewardedAt:time
  )

  ;define locks schema
  (defschema locks
    account:string
    lockedAmount:decimal
    lockedDate:time
    unlockDate:time
  )

  ;define rewards schema
  (defschema rewards
    account:string
    rewardAmount:decimal
    updatedAt:time
  )

  ; define staking stats schema
  (defschema staking-stats
    totalStaked:decimal
    totalRewarded:decimal
  )

  ; define state schema
  (defschema staking-halted
    halted:bool
  )

  ; define stake-info object schema
  (defschema stake-info
    account     :string
    stakeAmount :decimal
    rewardedAt  :time
  )

  (deftable stakes-table:{stakes})
  (deftable locks-table:{locks})
  (deftable rewards-table:{rewards})
  (deftable staking-stats-table:{staking-stats})
  (deftable staking-halted-table:{staking-halted})


  ; --------------------------------------------------------------------------
  ; Constants

  (defconst MAXIMUM_APR:decimal 9.3 "Maximum apr that will be received for staking")

  (defconst WITHDRAW_FEE:decimal 0.005 "Fee applicable when withdrawing a stake")

  (defconst REWARD_CALCULATION_INTERVAL:decimal 24.0 "Interval for assigning rewards")

  (defconst STAKING_VAULT_ACCOUNT "kdlaunch-staking-vault" "Account holding staked KDL")

  (defconst REWARDS_VAULT_ACCOUNT "kdlaunch-rewards-vault" "Account holding KDL rewards")

  (defconst KDL_MANAGER_ACCOUNT "kdlaunch-token-manager")

  ; --------------------------------------------------------------------------
  ; Utils

  (defun staking-vault-guard:guard () (create-module-guard "staking-vault-guard"))
  (defun rewards-vault-guard:guard () (create-module-guard "rewards-vault-guard"))

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data)))


  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()

    @doc " Give the admin full access to call and upgrade the module. "

    (enforce-keyset 'kdlaunch-admin)
  )

  (defcap MANAGER_GUARD
    ()
    @doc "Restricts access to the KDL manager"
    (enforce-guard (at 'guard (kdlaunch.token.details KDL_MANAGER_ACCOUNT)))
  )

  (defcap ACCT_GUARD (account)
    (enforce-guard (at 'guard (kdlaunch.token.details account)))
  )

  (defcap STAKING_OPEN:bool ()
    (with-read staking-halted-table ""
      {
        "halted":= stakingHalted
      }

      (enforce (= stakingHalted false) "UNABLE TO STAKE, STAKING IS CURRENTLY HALTED")
    )
  )

  (defcap STAKE () true)

  (defcap UNSTAKE () true)

  (defcap REWARDED () true)

  (defcap PROCESSREWARDS () true)


  ; --------------------------------------------------------------------------
  ; One-time initialization

  (defun initialize ()
    @doc " Initialize the staking contract "

    (with-capability (GOVERNANCE)
      (kdlaunch.token.create-account REWARDS_VAULT_ACCOUNT (rewards-vault-guard))
      (kdlaunch.token.create-account STAKING_VAULT_ACCOUNT (staking-vault-guard))
      (insert staking-halted-table "" { "halted": true } )
      (insert staking-stats-table "" {
        "totalStaked"   : 0.0,
        "totalRewarded" : 0.0
      })
    )
  )


  ; --------------------------------------------------------------------------
  ; Rewards vault management - deposit and withdraw tokens

  (defun deposit-tokens (from amount)
    (with-capability (GOVERNANCE)
      (kdlaunch.token.transfer-create from REWARDS_VAULT_ACCOUNT (rewards-vault-guard) amount)
    )
  )

  (defun withdraw-tokens (to amount guard)
    (with-capability (GOVERNANCE)
      (install-capability (kdlaunch.token.TRANSFER REWARDS_VAULT_ACCOUNT to amount))
      (kdlaunch.token.transfer-create REWARDS_VAULT_ACCOUNT to guard amount)
    )
  )


  ; --------------------------------------------------------------------------
  ; Staking functions

  (defun set-staking-halted (halted)
    "Disable or enable the ability to stake"
    (with-capability (GOVERNANCE)
      (write staking-halted-table "" { "halted": halted })
    )
  )

  (defun raise-stake (raiseAmount)
    "Increase the total amount staked in the contract"
    (require-capability (STAKE))
    (with-read staking-stats-table "" {
      "totalStaked":= totalStaked
      }
      (update staking-stats-table "" {
        "totalStaked": (+ totalStaked raiseAmount)
        }))
  )

  (defun reduce-stake (reduceAmount)
    "Decrease the total amount staked in the contract"
    (require-capability (UNSTAKE))
    (with-read staking-stats-table "" {
      "totalStaked":= totalStaked
      }
      (update staking-stats-table "" {
        "totalStaked": (- totalStaked reduceAmount)
        }))
  )

  (defun withdraw-fee:decimal (stakeAmount)
    "Calculates the fee that is applied when withdrawing a stake"
    (* WITHDRAW_FEE stakeAmount)
  )

  (defun stake-amount (account amount)
    (with-capability (ACCT_GUARD account)
      (with-capability (STAKING_OPEN)
        (with-capability (STAKE)
          (enforce (> amount 0.0) "Stake amount must be positive.")

          (with-default-read stakes-table account
            {
              "stakeAmount" : 0.0
            }
            {
              "stakeAmount" := stakeAmount
            }

            ; Store any pending rewards before staking
            (update-user-rewards account)

            (kdlaunch.token.transfer-create account STAKING_VAULT_ACCOUNT (staking-vault-guard) amount)

            (write stakes-table account
              {
                "account"     : account,
                "stakeAmount" : (+ stakeAmount amount),
                "stakeDate"   : (curr-time),
                "rewardedAt"  : (curr-time)
              })
            (raise-stake amount)
          ))))
  )

  (defun unstake-amount (account amount)
    (with-capability (ACCT_GUARD account)
      (with-capability (UNSTAKE)
        (enforce (> amount 0.0) "Unstake amount must be positive.")

        (with-read stakes-table account
          {
            "stakeAmount" := stakeAmount
          }

          (let*
            (
              (lockedAmount (at 'lockedAmount (get-locked-stake account)))
              (withdrawFee (withdraw-fee amount))
              (amountWithoutFee (- amount withdrawFee))
            )

            (enforce (>= stakeAmount amount) "Cannot unstake more than is staked")
            (enforce (>= (- stakeAmount lockedAmount) amount) "Insufficient free stake to unstake")

            ; Transfer amount with fees deducted
            (install-capability (kdlaunch.token.TRANSFER STAKING_VAULT_ACCOUNT account amountWithoutFee))
            (kdlaunch.token.transfer STAKING_VAULT_ACCOUNT account amountWithoutFee)

            ; Transfer fees to rewards vault
            (install-capability (kdlaunch.token.TRANSFER STAKING_VAULT_ACCOUNT REWARDS_VAULT_ACCOUNT withdrawFee))
            (kdlaunch.token.transfer-create STAKING_VAULT_ACCOUNT REWARDS_VAULT_ACCOUNT (rewards-vault-guard) withdrawFee)

            ; Store any pending rewards before unstaking
            (update-user-rewards account)

            (update stakes-table account
              {
                "account"     : account,
                "stakeAmount" : (- stakeAmount amount),
                "stakeDate"   : (curr-time),
                "rewardedAt"  : (curr-time)
              })
            (reduce-stake amount)
          ))))
  )

  (defun get-staking-stats ()
    (with-read staking-stats-table "" {
      "totalStaked":= totalStaked,
      "totalRewarded":= totalRewarded
      }
      { "totalStaked": totalStaked, "totalRewarded": totalRewarded }
    )
  )


  ; --------------------------------------------------------------------------
  ; Reward functions

  (defun raise-rewarded (rewardAmount)
    "Increase the total rewards amount in the contract"
    (require-capability (REWARDED))
    (with-read staking-stats-table "" {
      "totalRewarded":= totalRewarded
      }
      (update staking-stats-table "" {
        "totalRewarded": (+ totalRewarded rewardAmount)
        }))
  )

  (defun claim-rewards (account)
    "claiming accumulated rewards"
    (with-capability (ACCT_GUARD account)
      (with-capability (REWARDED)
        ; Store any pending rewards before claiming
        (update-user-rewards account)

        (with-read rewards-table account {
          "rewardAmount":= rewardAmount
          }

          (enforce (> rewardAmount 0.0) "No rewards to claim")

          (update rewards-table account {
            "rewardAmount": 0.0,
            "updatedAt": (curr-time)
          })

          (install-capability (kdlaunch.token.TRANSFER REWARDS_VAULT_ACCOUNT account rewardAmount))
          (kdlaunch.token.transfer REWARDS_VAULT_ACCOUNT account rewardAmount)
          (raise-rewarded rewardAmount)
          )))
  )

  (defun calculate-interest-yield ()
    "calculate the Annual Percentage Yield (APY) based on APR"
    (round (*(- (^ (+ 1 (/ (/ MAXIMUM_APR 100) 365)) 365) 1) 100) 2)
  )

  (defun calculate-reward (account stakeAmount rewardedAt)
    (let*
      (
        (stakePeriodMinutes (floor (/ (diff-time (curr-time) rewardedAt) 60.0) 0))
        (accumulatedInterestYearly (* (/ stakeAmount 100) MAXIMUM_APR))
        (rewards (round (* (/ accumulatedInterestYearly 525600) stakePeriodMinutes) 12))
      )
      rewards
    )
  )

  (defun process-reward (account stakeAmount rewardedAt)
    (require-capability (PROCESSREWARDS))
    (let
      (
        (rewards (calculate-reward account stakeAmount rewardedAt))
      )

      (if
        (> rewards 0.0)
        (with-default-read rewards-table account
          {
            "rewardAmount" : 0.0
          }
          {
            "rewardAmount":= rewardAmount
          }

          (write rewards-table account
            {
              "account"     : account,
              "rewardAmount" : (+ rewardAmount rewards),
              "updatedAt"   : (curr-time)
            }
          )
          (update stakes-table account {
            "rewardedAt": (curr-time)
          }))
        (format "No rewards to process" [])
      )
    )
  )

  (defun update-user-rewards (account)
    "calculate and store pending rewards for a user"
    (with-capability (PROCESSREWARDS)
      (with-default-read stakes-table account
        {
          "stakeAmount" : 0.0,
          "rewardedAt"  : (curr-time)
        }
        {
          "stakeAmount" := stakeAmount,
          "rewardedAt"  := rewardedAt
        }
        (process-reward account stakeAmount rewardedAt)
      )
    )
  )

  ; --------------------------------------------------------------------------
  ; Locking stake

  (defun get-locked-stake (account)
    "get the locked-stake amount for the account"
    (with-default-read locks-table account
      {
        "lockedAmount" : 0.0,
        "unlockDate": ""
      }
      {
        "lockedAmount":= lockedAmount,
        "unlockDate" := unlockDate
      }

      (if (= unlockDate "")
        { "lockedAmount": 0.0, "unlockDate": "" }
        (if
          (< (curr-time) unlockDate)
          { "lockedAmount": lockedAmount, "unlockDate": unlockDate }
          { "lockedAmount": 0.0, "unlockDate": "" }
        )
      )

    )
  )

  (defun lock-stake (account amount)
    "lock a staked amount for a certain period of time"

    (with-capability (ACCT_GUARD account)
      (with-read stakes-table account
        {
          "stakeAmount" := stakeAmount
        }

        (let ((lockedAmount (at 'lockedAmount (get-locked-stake account))))

          (enforce (>= (- stakeAmount lockedAmount) amount) "Insufficient stake to lock")

          (write locks-table account
            {
              "account"      : account,
              "lockedAmount" : (+ amount lockedAmount),
              "lockedDate"   : (curr-time),
              "unlockDate"   : (add-time (curr-time) (days 10))
            }
          ))))
  )


  ; --------------------------------------------------------------------------
  ; User functions

  (defun get-user-stats (account)
    (with-default-read stakes-table account { "stakeAmount" : 0.0, "rewardedAt" : (curr-time) } { "stakeAmount" := stakeAmount, "rewardedAt" := rewardedAt }
      (with-default-read rewards-table account { "rewardAmount" : 0.0 } { "rewardAmount" := rewardAmount }
        (let*
          (
            (lockedInfo (get-locked-stake account))
            (lockedAmount (at 'lockedAmount lockedInfo))
            (unlockDate (at 'unlockDate lockedInfo))
            (pendingRewards (calculate-reward account stakeAmount rewardedAt))
            (rewards (+ rewardAmount pendingRewards))
          )
          { "staked": stakeAmount, "rewards": rewards, "locked": lockedAmount, "lockedUntill": unlockDate }
        )
      ))
  )

  (defun take-snapshot ()
    (with-capability (MANAGER_GUARD)
      (select stakes-table ['stakeAmount 'account] (constantly true))
    )
  )
)

; --
