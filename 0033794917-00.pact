(module dao-hive-reference GOVERNANCE "Swarms.Finance Reference Manager"

  (defcap GOVERNANCE ()
    (enforce-keyset "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.admin")
  )



  (defun kds-swap-exact-in (swap-in-amount:decimal from-pool-tokenA:module{fungible-v2} to-pool-tokenB:module{fungible-v2} from-pool-account:string to-pool-account:string)
    @doc " SWAP "
    true
  )

  (defun kds-get-pair (from-pool-tokenA:module{fungible-v2} to-pool-tokenB:module{fungible-v2})
    @doc " GET PAIR "
    true
  )

  (defun kds-get-pair-key(tokenA:module{fungible-v2} tokenB:module{fungible-v2})
    @doc " GET PAIR KEY "
    true
  )

  (defun kds-add-liquidity (tokenA:module{fungible-v2} tokenB:module{fungible-v2} add-amount-A:decimal add-amount-B:decimal new-treasury-account:string pool-guard:guard)
    @doc " ADD LIQUIDITY "
    true
  )

  (defun kds-remove-liquidity (tokenA:module{fungible-v2} tokenB:module{fungible-v2} remove-amount:decimal lp-pool-account:string new-treasury-account:string pool-guard:guard)
    @doc " REMOVE LIQUIDITY "
    true
  )

  (defun kds-tokens-get-balance (lp-pool-pair:string lp-pool-account:string)
    @doc " TOKENS GET BALANCE "
    true
  )


)

