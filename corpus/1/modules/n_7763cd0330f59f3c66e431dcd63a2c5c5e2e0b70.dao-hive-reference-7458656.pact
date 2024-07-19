(module dao-hive-reference GOVERNANCE "Swarms.Finance Reference Manager"

  (defcap GOVERNANCE ()
    (enforce-keyset "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.admin")
  )


  (defun kds-swap-exact-in (swap-in-amount:decimal from-pool-tokenA:module{fungible-v2} to-pool-tokenB:module{fungible-v2} from-pool-account:string to-pool-account:string)
    @doc " KDS SWAP "
    (kdlaunch.kdswap-exchange.swap-exact-in swap-in-amount 0.0 [from-pool-tokenA to-pool-tokenB] from-pool-account to-pool-account (at "guard" (to-pool-tokenB::details to-pool-account)) )
  )

  (defun kds-get-pair (from-pool-tokenA:module{fungible-v2} to-pool-tokenB:module{fungible-v2})
    @doc " KDS GET PAIR "
    (kdlaunch.kdswap-exchange.get-pair from-pool-tokenA to-pool-tokenB)
  )

  (defun kds-get-pair-key(tokenA:module{fungible-v2} tokenB:module{fungible-v2})
    @doc " KDS GET PAIR KEY "
    (kdlaunch.kdswap-exchange.get-pair-key tokenA tokenB)
  )

  (defun kds-add-liquidity (tokenA:module{fungible-v2} tokenB:module{fungible-v2} add-amount-A:decimal add-amount-B:decimal new-treasury-account:string pool-guard:guard)
    @doc " KDS ADD LIQUIDITY "
    (kdlaunch.kdswap-exchange.add-liquidity tokenA tokenB add-amount-A add-amount-B 0.0 0.0 new-treasury-account new-treasury-account pool-guard)
  )

  (defun kds-remove-liquidity (tokenA:module{fungible-v2} tokenB:module{fungible-v2} remove-amount:decimal lp-pool-account:string new-treasury-account:string pool-guard:guard lp-pool-pair:string swap-account:string remove-amount:decimal)
    @doc " KDS REMOVE LIQUIDITY "
    (install-capability (kdlaunch.kdswap-exchange-tokens.TRANSFER lp-pool-pair lp-pool-account swap-account remove-amount))
    (kdlaunch.kdswap-exchange.remove-liquidity tokenA tokenB remove-amount 0.0 0.0 lp-pool-account new-treasury-account pool-guard)
  )

  (defun kds-tokens-get-balance (lp-pool-pair:string lp-pool-account:string)
    @doc " KDS TOKENS GET BALANCE "
    (kdlaunch.kdswap-exchange-tokens.get-balance lp-pool-pair lp-pool-account)
  )



)

