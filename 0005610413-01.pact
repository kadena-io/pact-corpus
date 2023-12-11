(module kwUSDC GOVERNANCE

    @doc "kwUSDC token"
  
    ; --------------------------------------------------------------------------
    ; Schemas and Tables
  
    (defschema token-schema
      @doc " An account, holding a token balance. \
           \ \
           \ ROW KEY: accountId. "
      balance:decimal
      guard:guard
    )
    (deftable token-table:{token-schema})
  
    ; --------------------------------------------------------------------------
    ; Capabilities
  
    (defcap GOVERNANCE
      ()
  
      @doc " Give the admin full access to call and upgrade the module. "
  
      (enforce-keyset 'lago-ns-user)
    )
  )
