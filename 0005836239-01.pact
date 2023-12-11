(module kdswap-staking GOVERNANCE
  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  ;define stakes schema
  (defschema stakes
    stakeId: string
    stakeAmount: decimal
    account: string
    stakeDate: time
    unlockDate: time
    poolId: string
    rewards: decimal
    status: integer
    days: integer
  )

  ;define pools schema
  (defschema pools
    type: string
    size: decimal
    ratio: decimal
    poolId: string
    reserved: decimal
    staked: decimal
    stakeToken:module{fungible-v2}
    rewardToken:module{fungible-v2}
    openDate: time
    halted: bool
  )

  ; define aprs schema
  (defschema aprs
    apr:decimal
  )

  ; define staking stats schema
  (defschema stakes-stats
    totalStaked:decimal
    totalRewarded:decimal
  )

  ; define locks meta schema
  (defschema stakes-meta
    feeAccount:string
    ejectStakeFee:decimal
    ejectRewardsFee:decimal
  )


  (deftable stakes-table:{stakes})
  (deftable pools-table:{pools})
  (deftable stakes-meta-table:{stakes-meta})
  (deftable stakes-stats-table:{stakes-stats})
  (deftable aprs-table:{aprs})

  ; --------------------------------------------------------------------------
  ; Constants

  ; APRS :duration
  (defconst APR30 30)
  (defconst APR90 90)
  (defconst APR182 182)
  (defconst APR365 365)

  ; Stakes statusses
  (defconst STAKED 0)
  (defconst UNSTAKED 1)
  (defconst EJECTED 2)

  ; Stakes vaults
  (defconst REWARDVAULT "reward")
  (defconst STAKINGVAULT "staking")

  ; Pool types
  (defconst UNLIMITED "unlimited" "Pools that don't have an 'unlimited' reward supply")
  (defconst FIXED "fixed" "Pools that have a fixed amount of rewards")

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap STAKING_RESERVE
  (pool-key:string)
  true)


  (defcap GOVERNANCE ()
    @doc " Give the admin full access to call and upgrade the module. "
    (enforce-keyset "kdlaunch.kdlaunch-admin")
  )

  (defcap ACCT_GUARD (account)
    (enforce-guard (at 'guard (coin.details account)))
  )

  (defcap STAKING_OPEN:bool (poolId)
    (with-read pools-table poolId
      {
        "halted"    := halted,
        "openDate"  := openDate
      }
      (enforce (= halted false) "UNABLE TO STAKE/UNSTAKE, STAKING/UNSTAKING IS CURRENTLY HALTED FOR THIS POOL")
      (enforce (< openDate (curr-time)) "STAKING POOL IS NOT YET OPENED")
    )
  )

  (defcap STAKE (
    stakeId:string
  )
    "Private defcap STAKING"
    @event
    true
  )

  (defcap UNSTAKE (
    stakeId:string
  )
    "Private defcap UNSTAKING"
    @event
    true
  )

  (defcap CREATE_POOL (
    poolId:string
  )
    @event
    true
  )

  ; --------------------------------------------------------------------------
  ; Configuration

  ; --------------------------------------------------------------------------
  ; Utils
  (defun enforce-staking-reserve:bool
    (key:string)
  (require-capability (STAKING_RESERVE key)))

  (defun create-staking-guard:guard
    (key:string)
  (create-user-guard (enforce-staking-reserve key)))

  (defun account-key (key:string type:string)
    (format "{}-{}" [key type])
  )

  (defun get-staking-account-principal (key:string)
    (create-principal (create-staking-guard key))
  )

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data))
  )

  (defun create-unique-stake-id:string
    (account:string poolId:string)
    (hash (format "{}{}{}" [account (curr-time) poolId]))
  )

  (defun create-unique-pool-id:string
    (firstVal:module{fungible-v2} secondVal:module{fungible-v2})
    (format "{}-{}-{}" [firstVal secondVal (hash (curr-time))])
  )

  (defun create-apr-pool-id:string
    (poolId:string aprId:integer)
    (format "{}-{}" [poolId aprId])
  )

  (defun set-fee-account (feeAccount:string)
    @doc "Set the account that will store the locking fees"
    (with-capability (GOVERNANCE)
      (update stakes-meta-table "" {
        "feeAccount": feeAccount
      }))
  )


  (defun set-pool-haltstate (poolId: string halted: bool)
    @doc "Update halt-state"
    (with-capability (GOVERNANCE)
      (update pools-table poolId {
        "halted": halted
      }))
  )

  ; --------------------------------------------------------------------------
  ; Stake functions
  (defun calculate-reward (stakeAmount days apr rewardToken:module{fungible-v2})
  "calculate the reward based on amount days and apr"
     (let*
       (
         (accumulatedInterestYearly (* (/ stakeAmount 100) apr))
         (rewards (floor (* (/ accumulatedInterestYearly 365) days) (rewardToken::precision)))
       )
       rewards
     )
  )

  (defun raise-stake (stakeAmount token stakeId)
   "Increase the total amount staked for the token"
   (require-capability (STAKE stakeId))
   (with-default-read stakes-stats-table token
     {
       "totalStaked" : 0.0,
       "totalRewarded" : 0.0
     }
     {
       "totalStaked":= totalStaked,
       "totalRewarded":= totalRewarded
     }
     (write stakes-stats-table token {
       "totalStaked": (+ totalStaked stakeAmount),
       "totalRewarded": totalRewarded
     }))
  )

  (defun raise-rewarded (rewardAmount token stakeId)
   "Increase the total amount rewarded for the token"
   (require-capability (UNSTAKE stakeId))
   (with-default-read stakes-stats-table token
    {
      "totalStaked" : 0.0,
      "totalRewarded" : 0.0
    }
    {
      "totalStaked":= totalStaked,
      "totalRewarded":= totalRewarded
    }
    (write stakes-stats-table token {
      "totalStaked": totalStaked,
      "totalRewarded": (+ totalRewarded rewardAmount)
    }))
  )

  (defun reduce-stake (reduceAmount token stakeId)
   "Decrease the total amount staked for the token"
   (require-capability (UNSTAKE stakeId))
   (with-read stakes-stats-table token {
     "totalStaked":= totalStaked
     }
     (update stakes-stats-table token {
       "totalStaked": (- totalStaked reduceAmount)
    }))
  )

  (defun set-apr (poolId:string apr30:decimal apr90:decimal apr182:decimal apr365:decimal)
    @doc "Set APRs for unlimited pools"
    (with-capability (GOVERNANCE)
      (with-read pools-table poolId
        {
          "type" := type
        }

        (write aprs-table (create-apr-pool-id poolId APR30) {
          "apr": apr30
        })
        (write aprs-table (create-apr-pool-id poolId APR90) {
          "apr": apr90
        })
        (write aprs-table (create-apr-pool-id poolId APR182) {
          "apr": apr182
        })
        (write aprs-table (create-apr-pool-id poolId APR365) {
          "apr": apr365
        })
      )
    )
  )

  (defun get-aprs (poolId)
    @doc "Get APRs for pool"
    (let*
     (
       (apr30 (at "apr" (read aprs-table (create-apr-pool-id poolId APR30))))
       (apr90 (at "apr" (read aprs-table (create-apr-pool-id poolId APR90))))
       (apr182 (at "apr" (read aprs-table (create-apr-pool-id poolId APR182))))
       (apr365 (at "apr" (read aprs-table (create-apr-pool-id poolId APR365))))
     )
     {
       "apr30": apr30,
       "apr90": apr90,
       "apr182": apr182,
       "apr365": apr365
     }
    )
  )


  (defun read-apr (poolAprId:string)
    @doc "Get APR for pool"
    (with-read aprs-table poolAprId {
      "apr":= apr
      }
      apr
    )
  )

  (defun get-apr-poolid-by-days (poolId:string days:integer)
    @doc "Get APR by day for pool"
    (let
     (
      (activePoolId (if (>= days APR365) (create-apr-pool-id poolId APR365) (if (>= days APR182)(create-apr-pool-id poolId APR182) (if (>= days APR90) (create-apr-pool-id poolId APR90) (create-apr-pool-id poolId APR30)))))
     )
     activePoolId
    )
  )

  (defun calculate-penalty-fee:decimal (stakeAmount:decimal stakeToken:module{fungible-v2})
  "Calculates the stake fee that is applied when emergency ejecting a stake"
    (with-read stakes-meta-table "" {
      "ejectStakeFee":= ejectStakeFee
    }
      (floor (* stakeAmount ejectStakeFee) (stakeToken::precision))
    )
  )

  (defun calculate-rewards-with-penalty:decimal (rewards:decimal stakeDate:time days:integer rewardToken:module{fungible-v2})
    "Calculate the reward up until the period of ejecting the stake"
    (with-read stakes-meta-table "" {
      "ejectRewardsFee":= ejectRewardsFee
    }
      (let*
       (
         (daysPassed (floor (/ (diff-time (curr-time) stakeDate) 86400.0) 0))
         (penalty-rewards (* (* (/ daysPassed days) rewards) (- 1 ejectRewardsFee)))
       )
       (floor penalty-rewards (rewardToken::precision))
      )
    )
  )

  (defun get-penalties (poolId:string stakeId:string)
    (with-read pools-table poolId {
        "rewardToken":= rewardToken,
        "stakeToken" := stakeToken
      }
      (with-read stakes-table stakeId {
          "stakeAmount":= stakeAmount,
          "rewards":= rewards,
          "days":= days,
          "stakeDate":= stakeDate
        }
        (let*
          (
            (stakeFee (calculate-penalty-fee stakeAmount stakeToken))
            (penaltiedStakeAmount (- stakeAmount stakeFee))
            (penaltiedRewardAmount (calculate-rewards-with-penalty rewards stakeDate days rewardToken))
            (penaltiedRewardDifferenceAmount (- rewards penaltiedRewardAmount))
          )
          {
            "stakeFee": stakeFee,
            "rewards": rewards,
            "penaltiedStakeAmount": penaltiedStakeAmount,
            "penaltiedRewardAmount": penaltiedRewardAmount,
            "penaltiedRewardDifferenceAmount": penaltiedRewardDifferenceAmount
          }
  ))))

  (defun stake (
    account:string
    amount:decimal
    poolId:string
    amountOfDays:integer)
    "stakes tokens in a pool"
    (with-capability (STAKING_OPEN poolId)

    ; make sure account is a k: account
    (enforce (= (take 2 account) "k:") "Only k:accounts are supported")

    ; make sure there is actually an amount to stake
    (enforce (> amount 0.0) "Stake amount must be positive")

    ; make sure the amount of days is valid
    (enforce (>= amountOfDays 30) "Amount of days should be equal or higher then 30")
    (enforce (<= amountOfDays 365) "Amount of days should be equal or lower then 365")

    ; make sure the pool exists and get data from the pool
    (with-read pools-table poolId
      {
      "type"        := type,
      "size"        := size,
      "ratio"       := ratio,
      "reserved"    := reserved,
      "staked"      := staked,
      "stakeToken"  := stakeToken:module{fungible-v2},
      "rewardToken" := rewardToken:module{fungible-v2}
      }

      ; stakeDate - set stakeDate
      ; unlockDate - add days to stakeDate to get unlockDate
      ; tokenAmountByRatio - calculate the amount of xxx tokens by the given ratio
      ; stakeId - create a unique id for the stake based upon account and poolid ( and time)
      ; rewards - calculate the rewards for the stake
      ; transferAmount - calculate the amount of tokens to transfer to the pool

      (enforce-one "Validate remaining rewards" [
        (enforce (= type UNLIMITED) "Unlimited pools don't need remaining rewards validation")
        (enforce (< reserved size) "No more rewards available")
      ])

      (let*
        (
          (stakeDate (curr-time))
          (tokenAmountByRatio (* amount ratio))
          (stakeId (create-unique-stake-id account poolId))
          (rewards (calculate-reward tokenAmountByRatio amountOfDays (read-apr (get-apr-poolid-by-days poolId amountOfDays)) rewardToken))
          (stakeRewards (if (= type UNLIMITED) rewards (if (<= (+ reserved rewards) size) rewards (- size reserved))))
          (stakeDays (if (= rewards stakeRewards) amountOfDays (ceiling (* (/ stakeRewards rewards) amountOfDays))))
          (unlockDate (add-time stakeDate (days stakeDays)))
        )

        (enforce (> rewards 0.0) "rewards amount must be positive")

        ;transfer the tokens to the pool
        (stakeToken::transfer-create account (get-staking-account-principal (account-key poolId STAKINGVAULT)) (create-staking-guard (account-key poolId STAKINGVAULT)) amount)

        (with-capability (STAKE stakeId)
          ; Create a stake in the database with status STAKED
          (insert stakes-table stakeId
            {
             "stakeId"       : stakeId,
             "stakeAmount"   : amount,
             "account"       : account,
             "stakeDate"     : stakeDate,
             "unlockDate"    : unlockDate,
             "poolId"        : poolId,
             "rewards"       : stakeRewards,
             "status"        : STAKED,
             "days"          : stakeDays
            }
          )

          ; increase total staked for this token in statistics
          (raise-stake amount (format "{}" [stakeToken]) stakeId)

          ; update the pool with the new amount of tokens staked in KDS
          ; update pool with the new amount of tokens reserved in XXX
          (update pools-table poolId
            {
             "reserved" : (+ reserved stakeRewards),
             "staked"   : (+ staked amount)
            })
    ))))
  )

  (defun unstake (
    stakeAccount:string
    poolId:string
    stakeId: string)
    "Unstake tokens from a pool"
    (with-capability (ACCT_GUARD stakeAccount)
    (with-capability (STAKING_OPEN poolId)
    (with-capability (UNSTAKE stakeId)

    (with-read stakes-table stakeId
      {
        "unlockDate"    := unlockDate,
        "rewards"       := rewards,
        "stakeAmount"   := stakeAmount,
        "status"        := status,
        "account"       := account
      }
      (enforce (= status STAKED) "Already unstaked or ejected")
      (enforce (= account stakeAccount) "You can only manage your own stake")
      (enforce (> (curr-time) unlockDate) "Cannot unstake before unlock date")

      (with-read pools-table poolId
        {
          "rewardToken" := rewardToken:module{fungible-v2},
          "stakeToken"  := stakeToken:module{fungible-v2},
          "staked"      := staked
        }
        ; tranfers original funds back to wallet of account
        (install-capability (stakeToken::TRANSFER (get-staking-account-principal (account-key poolId STAKINGVAULT)) account stakeAmount))
        (with-capability (STAKING_RESERVE (account-key poolId STAKINGVAULT))
          (stakeToken::transfer (get-staking-account-principal (account-key poolId STAKINGVAULT)) account stakeAmount)
        )

        ; transfer rewards from vault to wallet of account
        (install-capability (rewardToken::TRANSFER (get-staking-account-principal (account-key poolId REWARDVAULT)) account rewards))
        (with-capability (STAKING_RESERVE (account-key poolId REWARDVAULT))
          (rewardToken::transfer-create (get-staking-account-principal (account-key poolId REWARDVAULT)) account (at 'guard (coin.details account)) rewards)
        )

        ; decrease staked amount in pool
        (update pools-table poolId
          {
            "staked" : (- staked stakeAmount)
          }
        )

        ; decrease total staked for this token in statistics
        (reduce-stake stakeAmount (format "{}" [stakeToken]) stakeId)

        ; increase total rewarded for the reward token in statistics
        (raise-rewarded rewards (format "{}" [rewardToken]) stakeId)

        ; UNSTAKED - set status to unstaked
        (update stakes-table stakeId
          {
          "status" : UNSTAKED
          }
        )
    )))))
  )

  (defun emergency-eject (
    stakeAccount:string
    poolId:string
    stakeId: string)
    "Emergency Unstake tokens from a pool"
    (with-capability (ACCT_GUARD stakeAccount)
    (with-capability (STAKING_OPEN poolId)
    (with-capability (UNSTAKE stakeId)
    (with-read stakes-table stakeId
      {
        "stakeDate"     := stakeDate,
        "unlockDate"    := unlockDate,
        "stakeAmount"   := stakeAmount,
        "rewards"       := rewards,
        "days"          := days,
        "status"        := status,
        "account"       := account
      }

      (enforce (= account stakeAccount) "You can only manage your own stake")
      (enforce (= status STAKED) "Already unstaked or ejected")
      (enforce (< (curr-time) unlockDate) "You can only emergency eject before unlockdate, use normal unstake when unlockdate was exceeded")

      (with-read pools-table poolId
        {
          "stakeToken"  := stakeToken:module{fungible-v2},
          "rewardToken" := rewardToken:module{fungible-v2},
          "staked"      := staked,
          "ratio"       := ratio
        }

        (with-read stakes-meta-table "" {
          "feeAccount":= feeAccount
        }

        ; calculate eject penalty fee
        (let*
          (
            (penalties (get-penalties poolId stakeId))
            (stakeFee (at "stakeFee" penalties))
            (penaltiedStakeAmount (at "penaltiedStakeAmount" penalties))
            (penaltiedRewardAmount (at "penaltiedRewardAmount" penalties))
            (penaltiedRewardDifferenceAmount (at "penaltiedRewardDifferenceAmount" penalties))
          )

          (install-capability (stakeToken::TRANSFER (get-staking-account-principal (account-key poolId STAKINGVAULT)) account penaltiedStakeAmount))
          (install-capability (stakeToken::TRANSFER (get-staking-account-principal (account-key poolId STAKINGVAULT)) feeAccount stakeFee))
          (with-capability (STAKING_RESERVE (account-key poolId STAKINGVAULT))
          ; transfer tokens minus fee back to account
             (stakeToken::transfer (get-staking-account-principal (account-key poolId STAKINGVAULT)) account penaltiedStakeAmount)
          ; transfer stake Fee to fee account
             (stakeToken::transfer-create (get-staking-account-principal (account-key poolId STAKINGVAULT)) feeAccount (at 'guard (coin.details feeAccount)) stakeFee)
          )

          (install-capability (rewardToken::TRANSFER (get-staking-account-principal (account-key poolId REWARDVAULT)) account penaltiedRewardAmount))
          (install-capability (rewardToken::TRANSFER (get-staking-account-principal (account-key poolId REWARDVAULT)) feeAccount penaltiedRewardDifferenceAmount))
          (with-capability (STAKING_RESERVE (account-key poolId REWARDVAULT))
          ; transfer rewards for period
            (if (> penaltiedRewardAmount 0.0)
              (rewardToken::transfer-create (get-staking-account-principal (account-key poolId REWARDVAULT)) account (at 'guard (coin.details account)) penaltiedRewardAmount)
              true
            )
          ;transfer reward Fee to fee account
            (if (> penaltiedRewardDifferenceAmount 0.0)
              (rewardToken::transfer-create (get-staking-account-principal (account-key poolId REWARDVAULT)) feeAccount (at 'guard (coin.details feeAccount)) penaltiedRewardDifferenceAmount)
              true
            )
          )

          (update pools-table poolId
             {
               "staked" : (- staked stakeAmount)
             }
          )

          (update stakes-table stakeId
            {
            "status" : EJECTED
            }
          )

          ; decrease total staked for this token in statistics
          (reduce-stake stakeAmount (format "{}" [stakeToken]) stakeId)

          ; increase total rewarded for the reward token in statistics
          (raise-rewarded rewards (format "{}" [rewardToken]) stakeId)

    )))))))
  )

  (defun create-pool (
    type:string
    size:decimal
    ratio:decimal
    stakeToken:module{fungible-v2}
    rewardToken:module{fungible-v2}
    openDate: time)
    "Adds a pool to the pools table"
    (with-capability (GOVERNANCE)
    (enforce (<= 0.0 size) "Size is not a positive number")
    (enforce (< 0.0 ratio) "Ratio is not a positive number")
    (enforce (> openDate (curr-time)) "Opendate should be in the future")

    (enforce-one "Invalid pool size for unlimited pools" [
      (enforce (= type FIXED) "Fixed pools don't need size validation")
      (enforce (= size 0.0) "Unlimited pools need to have a size of 0.0")
    ])

    (let
      (
        (poolId (create-unique-pool-id stakeToken rewardToken))
        (correctRatio (if (= (format "{}" [stakeToken]) (format "{}" [rewardToken])) 1.0 ratio))
      )

      (with-capability (CREATE_POOL poolId)

        (insert pools-table poolId {
         "type":type,
         "size":size,
         "ratio":correctRatio,
         "poolId":poolId,
         "reserved": 0.0,
         "staked": 0.0,
         "stakeToken":stakeToken,
         "rewardToken":rewardToken,
         "openDate": openDate,
         "halted": true
         })
    ))
  ))

  (defun fund-pool (
    poolId:string
    account:string
    amount:decimal
    )
    "Adds funds to a pool"
    (enforce (> amount 0.0) "Amount is not a positive number")

    (with-read pools-table poolId
      {
        "rewardToken" := rewardToken:module{fungible-v2}
      }

      ;transfer reward tokens to the pool
      (rewardToken::transfer-create account (get-staking-account-principal (account-key poolId REWARDVAULT)) (create-staking-guard (account-key poolId REWARDVAULT)) amount)
    )
  )

  (defun get-remaining-pool-size (poolId:string)
    "Returns the remaining pool size"
    (with-read pools-table poolId
     {
       "size" := size,
       "reserved" := reserved,
       "type" := type
     }
     (enforce (= type FIXED) "Unlimited pools don't have a pool size")
     {
       "size": size,
       "reserved": reserved,
       "remaining": (- size reserved)
     }
    )
  )

  (defun get-meta-data ()
    (with-read stakes-meta-table "" {
       "ejectStakeFee"    := ejectStakeFee,
       "ejectRewardsFee"  := ejectRewardsFee
     }
     { "ejectStakeFee": ejectStakeFee, "ejectRewardsFee": ejectRewardsFee }
    )
  )

  (defun get-staking-stats ()
    (let*
     (
       (allRows (keys stakes-stats-table))
       (read-state-stats (lambda (token-key)
         (with-read stakes-stats-table token-key {
             "totalStaked"   := totalStaked,
             "totalRewarded" := totalRewarded
           }
           { "totalStaked": totalStaked, "totalRewarded": totalRewarded, "token": token-key }
         )))
     )
     (map (read-state-stats) allRows)
     )
  )

  (defun get-stake (stakeId:string)
    "Return stakeinfo"
    (read stakes-table stakeId ["stakeId", "stakeAmount", "account", "stakeDate", "unlockDate", "poolId", "rewards", "status", "days"])
  )

  (defun get-pool (poolId:string)
    "Return poolinfo"
    (read pools-table poolId ["type", "size", "ratio", "poolId", "reserved", "staked", "stakeToken", "rewardToken", "openDate", "halted"])
  )

  (defun get-account-stakes (account:string)
    (select stakes-table [
      'stakeId 'stakeAmount 'account 'stakeDate 'unlockDate 'poolId 'rewards 'status 'days]
      (where 'account (= account))
  ))


  (defun get-all-pools:list ()
    "Read all projects in projects table"
    (select pools-table
      ['type 'size 'ratio 'poolId 'reserved 'staked 'stakeToken 'openDate 'halted 'rewardToken]
      (constantly true)
    )
  )

)
; --------------------------------------------------------------------------
; Deployment

