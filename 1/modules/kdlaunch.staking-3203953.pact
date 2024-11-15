(module staking GOVERNANCE (defschema stakes account:string stakeAmount:decimal stakeDate:time rewardedAt:time ) (defschema locks account:string lockedAmount:decimal lockedDate:time unlockDate:time ) (defschema rewards account:string rewardAmount:decimal updatedAt:time ) (defschema staking-stats totalStaked:decimal totalRewarded:decimal ) (defschema staking-halted halted:bool ) (defschema stake-info account :string stakeAmount :decimal rewardedAt :time ) (deftable stakes-table:{stakes}) (deftable locks-table:{locks}) (deftable rewards-table:{rewards}) (deftable staking-stats-table:{staking-stats}) (deftable staking-halted-table:{staking-halted}) (defconst MAXIMUM_APR:decimal 22.315 "Maximum apr that will be received for staking") (defconst WITHDRAW_FEE:decimal 0.005 "Fee applicable when withdrawing a stake") (defconst REWARD_CALCULATION_INTERVAL:decimal 1.0 "Interval for assinging rewards") (defconst STAKING_VAULT_ACCOUNT "kdlaunch-staking-vault" "Account holding staked KDL") (defconst REWARDS_VAULT_ACCOUNT "kdlaunch-rewards-vault" "Account holding KDL rewards") (defun staking-vault-guard:guard () (create-module-guard "staking-vault-guard")) (defun rewards-vault-guard:guard () (create-module-guard "rewards-vault-guard")) (defun curr-time:time () @doc "Returns current chain's block-time in time type" (at 'block-time (chain-data))) (defcap GOVERNANCE () @doc " Give the admin full access to call and upgrade the module. " (enforce-keyset 'kdlaunch-admin) ) (defcap ACCT_GUARD (account) (enforce-guard (at 'guard (kdlaunch.token.details account))) ) (defcap STAKING_OPEN:bool () (with-read staking-halted-table "" { "halted":= stakingHalted } (enforce (= stakingHalted false) "UNABLE TO STAKE, STAKING IS CURRENTLY HALTED") ) ) (defcap STAKE () true) (defcap UNSTAKE () true) (defcap REWARDED () true) (defcap PROCESSREWARDS () true) (defun initialize () @doc " Initialize the staking contract " (with-capability (GOVERNANCE) (kdlaunch.token.create-account REWARDS_VAULT_ACCOUNT (rewards-vault-guard)) (kdlaunch.token.create-account STAKING_VAULT_ACCOUNT (staking-vault-guard)) (insert staking-halted-table "" { "halted": true } ) (insert staking-stats-table "" { "totalStaked" : 0.0, "totalRewarded" : 0.0 }) ) ) (defun deposit-tokens (from amount) (with-capability (GOVERNANCE) (kdlaunch.token.transfer-create from REWARDS_VAULT_ACCOUNT (rewards-vault-guard) amount) ) ) (defun withdraw-tokens (to amount guard) (with-capability (GOVERNANCE) (install-capability (kdlaunch.token.TRANSFER REWARDS_VAULT_ACCOUNT to amount)) (kdlaunch.token.transfer-create REWARDS_VAULT_ACCOUNT to guard amount) ) ) (defun set-staking-halted (halted) "Disable or enable the ability to stake" (with-capability (GOVERNANCE) (write staking-halted-table "" { "halted": halted }) ) ) (defun raise-stake (raiseAmount) "Increase the total amount staked in the contract" (require-capability (STAKE)) (with-read staking-stats-table "" { "totalStaked":= totalStaked } (update staking-stats-table "" { "totalStaked": (+ totalStaked raiseAmount) })) ) (defun reduce-stake (reduceAmount) "Decrease the total amount staked in the contract" (require-capability (UNSTAKE)) (with-read staking-stats-table "" { "totalStaked":= totalStaked } (update staking-stats-table "" { "totalStaked": (- totalStaked reduceAmount) })) ) (defun withdraw-fee:decimal (stakeAmount) "Calculates the fee that is applied when withdrawing a stake" (* WITHDRAW_FEE stakeAmount) ) (defun stake-amount (account amount) (with-capability (ACCT_GUARD account) (with-capability (STAKING_OPEN) (with-capability (STAKE) (enforce (> amount 0.0) "Stake amount must be positive.") (with-default-read stakes-table account { "stakeAmount" : 0.0 } { "stakeAmount" := stakeAmount } (kdlaunch.token.transfer-create account STAKING_VAULT_ACCOUNT (staking-vault-guard) amount) (write stakes-table account { "account" : account, "stakeAmount" : (+ stakeAmount amount), "stakeDate" : (curr-time), "rewardedAt" : (curr-time) }) (raise-stake amount) )))) ) (defun unstake-amount (account amount) (with-capability (ACCT_GUARD account) (with-capability (UNSTAKE) (enforce (> amount 0.0) "Unstake amount must be positive.") (with-read stakes-table account { "stakeAmount" := stakeAmount } (let* ( (lockedAmount (at 'lockedAmount (get-locked-stake account))) (withdrawFee (withdraw-fee amount)) (amountWithoutFee (- amount withdrawFee)) ) (enforce (>= stakeAmount amount) "Cannot unstake more than is staked") (enforce (>= (- stakeAmount lockedAmount) amount) "Insufficient free stake to unstake") (install-capability (kdlaunch.token.TRANSFER STAKING_VAULT_ACCOUNT account amountWithoutFee)) (kdlaunch.token.transfer STAKING_VAULT_ACCOUNT account amountWithoutFee) (install-capability (kdlaunch.token.TRANSFER STAKING_VAULT_ACCOUNT REWARDS_VAULT_ACCOUNT withdrawFee)) (kdlaunch.token.transfer-create STAKING_VAULT_ACCOUNT REWARDS_VAULT_ACCOUNT (rewards-vault-guard) withdrawFee) (update stakes-table account { "account" : account, "stakeAmount" : (- stakeAmount amount), "stakeDate" : (curr-time), "rewardedAt" : (curr-time) }) (reduce-stake amount) )))) ) (defun get-staking-stats () (with-read staking-stats-table "" { "totalStaked":= totalStaked, "totalRewarded":= totalRewarded } { "totalStaked": totalStaked, "totalRewarded": totalRewarded } ) ) (defun raise-rewarded (rewardAmount) "Increase the total rewards amount in the contract" (require-capability (REWARDED)) (with-read staking-stats-table "" { "totalRewarded":= totalRewarded } (update staking-stats-table "" { "totalRewarded": (+ totalRewarded rewardAmount) })) ) (defun claim-rewards (account) "claiming accumulated rewards" (with-capability (ACCT_GUARD account) (with-capability (REWARDED) (with-read rewards-table account { "rewardAmount":= rewardAmount } (enforce (> rewardAmount 0.0) "No rewards to claim") (update rewards-table account { "rewardAmount": 0.0, "updatedAt": (curr-time) }) (format "{}" [rewardAmount]) (install-capability (kdlaunch.token.TRANSFER REWARDS_VAULT_ACCOUNT account rewardAmount)) (kdlaunch.token.transfer REWARDS_VAULT_ACCOUNT account rewardAmount) (raise-rewarded rewardAmount) ))) ) (defun calculate-interest-rate () "calculate the Annual Percentage Rate (APR) based on vault amount" (let* ( (totalStaked (at 'totalStaked (read staking-stats-table "" ["totalStaked"]))) (rewardVaultBalance (kdlaunch.token.get-balance REWARDS_VAULT_ACCOUNT)) (apr (round (* 2 (* (/ rewardVaultBalance totalStaked) 100)) 2)) ) (if (> apr MAXIMUM_APR) MAXIMUM_APR apr) ) ) (defun calculate-interest-yield () "calculate the Annual Percentage Yield (APY) based on APR" (round (*(- (^ (+ 1 (/ (/ (calculate-interest-rate) 100) 8760)) 8760) 1) 100) 2) ) (defun process-reward (info:object{stake-info}) (require-capability (PROCESSREWARDS)) (let* ( (account (at 'account info)) (stakeAmount (at 'stakeAmount info)) (rewardedAt (at 'rewardedAt info)) (apr (calculate-interest-rate)) (stakePeriodHours (floor (/ (diff-time (curr-time) rewardedAt) 3600.0) 0)) (accumlatedInterestYearly (* (/ stakeAmount 100) apr)) (rewards (round (* (/ accumlatedInterestYearly 8760) stakePeriodHours) 12)) ) (with-default-read rewards-table account { "rewardAmount" : 0.0 } { "rewardAmount":= rewardAmount } (write rewards-table account { "account" : account, "rewardAmount" : (+ rewardAmount rewards), "updatedAt" : (curr-time) } ) (update stakes-table account { "rewardedAt": (curr-time) }))) ) (defun calculate-rewards () "calculate and store pending rewards, paid out once per 24 hours" (with-capability (PROCESSREWARDS) (let ((dayAgo (add-time (curr-time) (hours (- 0 REWARD_CALCULATION_INTERVAL))))) (map (process-reward) (select stakes-table ['account 'stakeAmount 'rewardedAt] (and? (where 'stakeAmount (< 0.0)) (where 'rewardedAt (> dayAgo))))) )) ) (defun get-locked-stake (account) "get the locked-stake amount for the account" (with-default-read locks-table account { "lockedAmount" : 0.0, "unlockDate": "" } { "lockedAmount":= lockedAmount, "unlockDate" := unlockDate } { "lockedAmount": lockedAmount, "unlockDate": unlockDate } ) ) (defun lock-stake (account amount period) "lock a staked amount for a certain period of time" (with-capability (ACCT_GUARD account) (with-read stakes-table account { "stakeAmount" := stakeAmount } (let ((lockedAmount (at 'lockedAmount (get-locked-stake account)))) (enforce (>= (- stakeAmount lockedAmount) amount) "Insufficient stake to lock") (with-default-read locks-table account { "lockedAmount" : 0.0 } { "lockedAmount":= lockedAmount } (write locks-table account { "account" : account, "lockedAmount" : (+ amount lockedAmount), "lockedDate" : (curr-time), "unlockDate" : (add-time (curr-time) (days period)) } ))))) ) (defun get-user-stats (account) (with-default-read stakes-table account { "stakeAmount" : 0.0 } { "stakeAmount" := stakeAmount } (with-default-read rewards-table account { "rewardAmount" : 0.0 } { "rewardAmount" := rewardAmount } (with-default-read locks-table account { "lockedAmount" : 0.0, "unlockDate": "" } { "lockedAmount" := lockedAmount, "unlockDate" := unlockDate } { "staked": stakeAmount, "rewards": rewardAmount, "locked": lockedAmount, "lockedUntill": unlockDate } ))) ) ) 
