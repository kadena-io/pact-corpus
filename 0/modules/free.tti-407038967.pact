(module tti "free.sixdark6lord6-481361622"
  @doc "TTI (TenThousandImmortals) - Module principal"

  (defcap GOVERNANCE ()
    (enforce-keyset "free.sixdark6lord6-481361622"))

  (defcap INTERNAL ()
    true)

  (defconst TOTAL_SUPPLY 10000.0)
  (defconst INITIAL_PRICE 1.0)
  (defconst CONTRACT_ACCOUNT "TTI_CONTRACT")
  (defconst MODULE_ADMIN_ACCOUNT "module-admin-account")

  (defschema balance-schema
    ttibalance:decimal
    kdabalance:decimal
    guard:guard)

  (defschema reserve-info
    kdareserve:decimal
    nextprice:decimal
    ttiincirculation:decimal)

  (deftable balances:{balance-schema})
  (deftable reserve:{reserve-info})

  (defun init ()
    (enforce-guard (keyset-ref-guard "free.sixdark6lord6-481361622"))
    (insert balances CONTRACT_ACCOUNT 
      { "ttibalance": TOTAL_SUPPLY
      , "kdabalance": 0.0
      , "guard": (create-module-guard "tti-contract-guard")
      })
    (insert reserve "info" 
      { "kdareserve": 0.0
      , "nextprice": INITIAL_PRICE
      , "ttiincirculation": 0.0
      })
    "Contract initialized")

  (defun get-balance (account:string)
    (with-read balances account { "ttibalance" := ttibalance, "kdabalance" := kdabalance }
      { "ttibalance": ttibalance, "kdabalance": kdabalance }))

  (defun get-account-guard (account:string)
    (with-read balances account { "guard" := guard }
      guard))

  (defun get-reserve-info ()
    @doc "Get current reserve information"
    (read reserve "info"))

  (defun create-account (account:string guard:guard)
    @doc "Create an account for holding TTI tokens and KDA"
    (enforce-guard guard)
    (insert balances account 
      { "ttibalance": 0.0
      , "kdabalance": 0.0
      , "guard": guard
      })
    account)

  (defun rotate (account:string new-guard:guard)
    (with-read balances account { "guard" := old-guard, "ttibalance" := ttibalance, "kdabalance" := kdabalance }
      (enforce-guard old-guard)
      (update balances account 
        { "guard": new-guard
        , "ttibalance": ttibalance
        , "kdabalance": kdabalance
        })
      "Rotated"))

  (defun update-tti-balance (account:string amount:decimal)
    @doc "Update TTI balance for an account"
    (require-capability (INTERNAL))
    (with-read balances account { "ttibalance" := old-balance, "kdabalance" := kdabalance, "guard" := guard }
      (update balances account
        { "ttibalance": (+ old-balance amount)
        , "kdabalance": kdabalance
        , "guard": guard
        })))

  (defun update-kda-balance (account:string amount:decimal)
    @doc "Update KDA balance for an account"
    (require-capability (INTERNAL))
    (with-read balances account { "ttibalance" := ttibalance, "kdabalance" := old-balance, "guard" := guard }
      (update balances account
        { "ttibalance": ttibalance
        , "kdabalance": (+ old-balance amount)
        , "guard": guard
        })))

  (defun update-reserve (kdareserve:decimal nextprice:decimal ttiincirculation:decimal)
    @doc "Update reserve info"
    (require-capability (INTERNAL))
    (update reserve "info"
      { "kdareserve": kdareserve
      , "nextprice": nextprice
      , "ttiincirculation": ttiincirculation
      }))
)
