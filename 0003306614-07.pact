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
  (defconst PRECISION 12)
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
    ; (distribute-rewards)
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
    ; (distribute-rewards)
    (with-capability (GOVERNANCE)
      (update staking-stats-table "" { "annualRewardRate": rate })
    )
  )

  (defun set-max-APR (rate:decimal) "Set the max APR."
    ; (distribute-rewards)
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
    (update-account account)
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
    (update-account account)
    (with-capability (ACCOUNT_GUARD account)
      (with-capability (UNSTAKE)
        (enforce (> amount 0.0) "Unstake amount must be positive.")
        (with-read stakes-table account { "stakeAmount" := stakeAmount,
                                          "rewardAmount":= rewardAmount}
          (enforce (>= (+ stakeAmount rewardAmount) amount) "Cannot unstake more than is staked")
          (let* ((claimRewards (floor (min rewardAmount amount) PRECISION))
                 (unstake (floor (max (- amount rewardAmount) 0.0) PRECISION))
                )
                ;claim-reward-part
                (if (> claimRewards 0.0)
                  [
                    (with-capability (REWARDED)
                      (update stakes-table account { "rewardAmount": (- rewardAmount claimRewards) })
                      (install-capability (mok.token.TRANSFER REWARDS_VAULT_ACCOUNT account claimRewards))
                      (mok.token.transfer REWARDS_VAULT_ACCOUNT account claimRewards)
                      (raise-rewarded claimRewards)
                    )
                  ]
                  []
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

  (defun all-stakes()
    "Calculate all stakes in contract without the rewards."
    (let
     ((qry (lambda (k obj) true)) ;; select all rows
      (f (lambda (acc x) (at "stakeAmount" x) ))
     )
     (fold (+) 0.0 (fold-db stakes-table (qry) (f)))
    )
  )

  (defun distribute-rewards ()
    (map (update-account) (keys stakes-table))
  )

  (defun update-account (account:string)
    (with-default-read stakes-table account
      {"stakeAmount":-1.0, "rewardAmount":0.0, "stakeDate":(curr-time)}
      {"stakeAmount":=stakeAmount, "rewardAmount":=rewardAmount, "stakeDate":=lastRewardTime}
      (let ( (totalStaked (+ stakeAmount rewardAmount)))
        (if (> totalStaked 0.0)
          (with-read staking-max-apr-table "" {"maximumAPR":=maximumAPR}
            (let* ( (currTime (curr-time))
                    (timeElapsed (diff-time currTime lastRewardTime))
                    (yearsElapsed (floor (/ timeElapsed (days ANNUAL_DAYS)) PRECISION))
                    (maxAPR (/ maximumAPR 100.0))
                    (newReward (* (- (floor (exp (* yearsElapsed maxAPR)) PRECISION) 1.0) totalStaked))
                  )
              (update stakes-table account {"rewardAmount": (+ rewardAmount newReward), "stakeDate": currTime})
            )
          )
          []
        )
      )
    )
  )

  (defun get-user-stats:object{stakes} (account:string)
    (update-account account)
    (read stakes-table account)
  )
)

