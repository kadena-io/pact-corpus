(module staking GOVERNANCE
  (defschema stakes account:string stakeAmount:decimal rewardAmount:decimal stakeDate:time)
  (defschema staking-stats totalStaked:decimal totalRewarded:decimal
                           annualRewardRate:decimal lastRewardTime:time
                           currentAPR:decimal)
  (defschema staking-max-apr maximumAPR:decimal)
  (defschema staking-halted halted:bool )
  (defschema stake-info account:string stakeAmount:decimal rewardAmount:decimal)
  (deftable staking-max-apr-table:{staking-max-apr})
  (deftable stakes-table:{stakes})
  (deftable staking-stats-table:{staking-stats})
  (deftable staking-halted-table:{staking-halted})
  (defconst ANNUAL_DAYS 365.25 "Days in one year")
  (defconst STAKING_VAULT_ACCOUNT "token-staking-vault" "Account holding staked MOK")
  (defconst REWARDS_VAULT_ACCOUNT "token-rewards-vault" "Account holding MOK rewards")

  (defun require-unstake ()
    (require-capability (UNSTAKE))
  )

  (defun require-rewarded ()
    (require-capability (REWARDED))
  )

  (defun curr-time:time () @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data))
  )

  (defcap GOVERNANCE () @doc " Give the admin full access to call and upgrade the module. "
    (enforce-guard 'mok-admin)
    (compose-capability (REWARDED))
  )

  (defcap ACCOUNT_GUARD (account:string)
    (enforce-guard (at 'guard (mok.token.details account)))
  )

  (defcap STAKE ()
    (with-read staking-halted-table "" { "halted":= stakingHalted }
      (enforce (not stakingHalted) "UNABLE TO STAKE, STAKING IS CURRENTLY HALTED")
    )
  )

  (defcap UNSTAKE () true)
  (defcap REWARDED () true)
  (defcap PROCESSREWARDS () true)

  (defun initialize () @doc " Initialize the staking contract "
    (with-capability (GOVERNANCE)
      (mok.token.create-account REWARDS_VAULT_ACCOUNT (create-user-guard (require-rewarded)))
      (mok.token.create-account STAKING_VAULT_ACCOUNT (create-user-guard (require-unstake)))
      (insert staking-halted-table "" { "halted": true } )
      (insert staking-stats-table "" { "totalStaked" : 0.0,
                                       "totalRewarded" : 0.0,
                                       "annualRewardRate": 0.0,
                                       "lastRewardTime":(curr-time),
                                       "currentAPR": 0.0})
      (insert staking-max-apr-table "" {"maximumAPR":0.0})
    )
  )

  (defun deposit-tokens (from:string amount:decimal)
    (with-capability (GOVERNANCE)
      (mok.token.transfer from REWARDS_VAULT_ACCOUNT amount)
    )
  )

  (defun withdraw-tokens (to:string amount:decimal guard:guard)
    (distribute-rewards)
    (with-capability (GOVERNANCE)
      (with-read staking-stats-table "" {"totalStaked":=totalStaked}
        (let* ((pendingRewards (- totalStaked (mok.token.get-balance STAKING_VAULT_ACCOUNT)))
               (rewardVaultBalance (- (mok.token.get-balance REWARDS_VAULT_ACCOUNT) pendingRewards))
              )
              (enforce (<= amount rewardVaultBalance) (format "Maximum withdraw amount is {}" [rewardVaultBalance]))
        )
      )
      (install-capability (mok.token.TRANSFER REWARDS_VAULT_ACCOUNT to amount))
      (mok.token.transfer-create REWARDS_VAULT_ACCOUNT to guard amount)
    )
  )

  (defun set-staking-halted (halted:bool) "Disable or enable the ability to stake"
    (with-capability (GOVERNANCE)
      (write staking-halted-table "" { "halted": halted })
    )
  )

  (defun set-annual-reward-rate (rate:decimal) "Set the annual reward rate (in coins)."
    (distribute-rewards)
    (with-capability (GOVERNANCE)
      (update staking-stats-table "" { "annualRewardRate": rate })
    )
  )

  (defun set-max-APR (rate:decimal) "Set the max APR."
    (distribute-rewards)
    (with-capability (GOVERNANCE)
      (update staking-max-apr-table "" { "maximumAPR": rate })
    )
  )

  (defun raise-stake (raiseAmount:decimal) "Increase the total amount staked in the contract"
    (require-capability (STAKE))
    (with-read staking-stats-table "" { "totalStaked":= totalStaked }
      (update staking-stats-table "" { "totalStaked": (+ totalStaked raiseAmount) })
    )
  )

  (defun reduce-stake (reduceAmount:decimal) "Decrease the total amount staked in the contract"
    (require-capability (UNSTAKE))
    (with-read staking-stats-table "" { "totalStaked":= totalStaked }
      (update staking-stats-table "" { "totalStaked": (- totalStaked reduceAmount) })
    )
  )

  (defun raise-rewarded (rewardAmount:decimal) "Increase the total rewards amount in the contract"
    (require-capability (REWARDED))
    (with-read staking-stats-table "" { "totalRewarded":= totalRewarded }
      (update staking-stats-table "" { "totalRewarded": (+ totalRewarded rewardAmount) })
    )
  )

  (defun stake-amount (account:string amount:decimal)
    (distribute-rewards)
    (with-capability (ACCOUNT_GUARD account)
      (with-capability (STAKE)
        (enforce (> amount 0.0) "Stake amount must be positive.")
        (with-default-read stakes-table account { "stakeAmount": 0.0, "rewardAmount": 0.0 }
                                                { "stakeAmount":=stakeAmount, "rewardAmount":=rewardAmount}
          (mok.token.transfer account STAKING_VAULT_ACCOUNT amount)
          (write stakes-table account { "account" : account,
                                        "stakeAmount" : (+ stakeAmount amount),
                                        "rewardAmount" : rewardAmount,
                                        "stakeDate" : (curr-time) })
          (raise-stake amount)
        )
      )
    )
  )

  (defun unstake-amount (account:string amount:decimal)
    (distribute-rewards)
    (with-capability (ACCOUNT_GUARD account)
      (with-capability (UNSTAKE)
        (enforce (> amount 0.0) "Unstake amount must be positive.")
        (with-read stakes-table account { "stakeAmount" := stakeAmount,
                                          "rewardAmount":= rewardAmount}
          (enforce (>= (+ stakeAmount rewardAmount) amount) "Cannot unstake more than is staked")
          (let* ((claimRewards (min rewardAmount amount))
                 (unstake (max (- amount rewardAmount) 0.0))
                )
                ;claim-reward-part
                (with-capability (REWARDED)
                  (update stakes-table account { "rewardAmount": (- rewardAmount claimRewards) })
                  (install-capability (mok.token.TRANSFER REWARDS_VAULT_ACCOUNT account claimRewards))
                  (mok.token.transfer REWARDS_VAULT_ACCOUNT account claimRewards)
                  (raise-rewarded claimRewards)
                )
                ;unstake part
                (if (> unstake 0.0)
                  [
                    (install-capability (mok.token.TRANSFER STAKING_VAULT_ACCOUNT account unstake))
                    (mok.token.transfer STAKING_VAULT_ACCOUNT account unstake)
                    (update stakes-table account { "stakeAmount" : (- stakeAmount unstake)})
                  ]
                  []
                )
                (reduce-stake amount)
          )
        )
      )
    )
  )

  (defun get-staking-stats ()
    (distribute-rewards)
    (+ (read staking-stats-table "") (read staking-max-apr-table ""))
  )

  (defun calculate-total-reward ()
    "Calculate the amount to be distributed based on the APR and the amount in the vault"
    (require-capability (PROCESSREWARDS))
    (with-read staking-stats-table "" {"annualRewardRate":=annualRewardRate,
                                       "totalStaked":=totalStaked,
                                       "lastRewardTime":=lastRewardTime}
      (with-read staking-max-apr-table "" {"maximumAPR":=maximumAPR}
        (let* ( (prec (mok.token.precision))
                (pendingRewards (- totalStaked (mok.token.get-balance STAKING_VAULT_ACCOUNT)))
                (rewardVaultBalance (- (mok.token.get-balance REWARDS_VAULT_ACCOUNT) pendingRewards))
                (timeElapsed (diff-time (curr-time) lastRewardTime))
                (yearsElapsed (/ timeElapsed (days ANNUAL_DAYS)))
                (maxAPR (/ maximumAPR 100.0))
                (limit (/ annualRewardRate maxAPR))
                (e (exp 1.0))
                (expYears (if (and (> totalStaked 0.0) (< totalStaked limit))
                            (/ (log e (/ limit totalStaked)) maxAPR)
                            0.0
                          )
                )
                (expYears (min yearsElapsed expYears))
                (expReward (* (- (exp (* expYears maxAPR)) 1.0) totalStaked))
                (linearYears (max (- yearsElapsed expYears) 0.0))
                (linearReward (* annualRewardRate linearYears))
                (totalReward (+ expReward linearReward))
                (reward (min totalReward rewardVaultBalance))
                (newTotalStaked (+ totalStaked reward))
                (currentAPR (if (> newTotalStaked limit) (/ annualRewardRate newTotalStaked) maxAPR))
              )
          (update staking-stats-table "" { "currentAPR": currentAPR })
          (floor reward prec)
        )
      )
    )
  )

  (defun process-reward (totalReward:decimal totalStaked:decimal info:object{stake-info})
    (require-capability (PROCESSREWARDS))
    (let* ( (account (at 'account info))
            (stakeAmount (at 'stakeAmount info))
            (rewardAmount (at 'rewardAmount info))
            (compundAmount (+ stakeAmount rewardAmount))
            (portion (/ compundAmount totalStaked))
            (accountReward (* portion totalReward))
            (newRewarded (+ rewardAmount accountReward))
            (newRewarded (floor newRewarded (mok.token.precision)))
          )

          (update stakes-table account {"rewardAmount": newRewarded} )
          (floor accountReward (mok.token.precision))
    )
  )

  (defun distribute-rewards ()
    "calculate and store pending rewards, paid out continuously"
    (with-capability (PROCESSREWARDS)
      (with-read staking-stats-table "" {"totalStaked":=totalStaked,
                                         "lastRewardTime":=lastRewardTime}
        (let* ((currTime (curr-time))
               (totalReward (if (= currTime lastRewardTime)
                              0.0
                              (calculate-total-reward)))

              )
          (update staking-stats-table "" { "totalStaked" : (+ totalStaked totalReward), "lastRewardTime": currTime} )
          (if (> totalReward 0.0)
            (map (process-reward totalReward totalStaked)
                (select stakes-table ['account 'stakeAmount 'rewardAmount]
                (or? (where 'stakeAmount (< 0.0)) (where 'rewardAmount (< 0.0))))
            )
            []
          )
        )
      )
    )
  )

  (defun current-APR:decimal()
    "Get current APR"
    (distribute-rewards)
    (with-read staking-stats-table "" { "currentAPR":= currentAPR }
      (floor currentAPR 4)
    )
  )

  (defun get-user-stats:object{stakes} (account:string)
    (distribute-rewards)
    (read stakes-table account)
  )
)


