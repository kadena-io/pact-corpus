(module KDOGE GOVERNANCE


  (defcap GOVERNANCE
    ()

    @doc " Give the admin full access to call and upgrade the module. "

    (enforce-keyset 'kdoge-admin)
  )

)
