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
    @doc "Initialize the contract. Can only be called once."
    (enforce-guard (keyset-ref-guard "free.sixdark6lord6-481361622"))
    (let
      ((bal-exists (try false (with-default-read balances CONTRACT_ACCOUNT { "ttibalance": -1.0 } { "ttibalance" := _ } true)))
       (res-exists (try false (with-default-read reserve "info" { "kdareserve": -1.0 } { "kdareserve" := _ } true))))
      (enforce (and (not bal-exists) (not res-exists)) "Contract already initialized"))
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
    (with-capability (INTERNAL)
      (with-read balances account { "ttibalance" := old-balance, "kdabalance" := kdabalance, "guard" := guard }
        (let ((new-balance (+ old-balance amount)))
          (enforce (>= new-balance 0.0) "Insufficient TTI balance")
          (update balances account
            { "ttibalance": new-balance
            , "kdabalance": kdabalance
            , "guard": guard
            })))))

  (defun update-kda-balance (account:string amount:decimal)
    @doc "Update KDA balance for an account"
    (with-capability (INTERNAL)
      (with-read balances account { "ttibalance" := ttibalance, "kdabalance" := old-balance, "guard" := guard }
        (let ((new-balance (+ old-balance amount)))
          (enforce (>= new-balance 0.0) "Insufficient KDA balance")
          (update balances account
            { "ttibalance": ttibalance
            , "kdabalance": new-balance
            , "guard": guard
            })))))

  (defun update-reserve (kdareserve:decimal nextprice:decimal ttiincirculation:decimal)
    @doc "Update reserve info"
    (with-capability (INTERNAL)
      (update reserve "info"
        { "kdareserve": kdareserve
        , "nextprice": nextprice
        , "ttiincirculation": ttiincirculation
        })))

  (defun transfer-tti (sender:string receiver:string amount:decimal)
    @doc "Transfer TTI tokens between accounts"
    (enforce (!= sender receiver) "Sender and receiver must be different")
    (enforce (> amount 0.0) "Amount must be positive")
    (with-read balances sender { "ttibalance" := sender-balance, "guard" := sender-guard }
      (enforce-guard sender-guard)
      (enforce (<= amount sender-balance) "Insufficient TTI balance")
      (update-tti-balance sender (- amount))
      (update-tti-balance receiver amount))
    "TTI transferred")

  (defun transfer-kda (sender:string receiver:string amount:decimal)
    @doc "Transfer KDA between accounts"
    (enforce (!= sender receiver) "Sender and receiver must be different")
    (enforce (> amount 0.0) "Amount must be positive")
    (with-read balances sender { "kdabalance" := sender-balance, "guard" := sender-guard }
      (enforce-guard sender-guard)
      (enforce (<= amount sender-balance) "Insufficient KDA balance")
      (update-kda-balance sender (- amount))
      (update-kda-balance receiver amount))
    "KDA transferred")

  (defun issue-tti (account:string kda-amount:decimal)
    @doc "Issue TTI tokens in exchange for KDA"
    (enforce (> kda-amount 0.0) "KDA amount must be positive")
    (with-read reserve "info" { "kdareserve" := kda-reserve, "nextprice" := next-price, "ttiincirculation" := tti-in-circulation }
      (enforce (< tti-in-circulation TOTAL_SUPPLY) "TTI supply limit reached")
      (let* ((tti-to-issue (/ kda-amount next-price))
             (new-tti-in-circulation (+ tti-in-circulation tti-to-issue))
             (new-kda-reserve (+ kda-reserve kda-amount))
             (new-next-price (+ next-price 1.0)))
        (enforce (<= new-tti-in-circulation TOTAL_SUPPLY) "Not enough TTI left to issue")
        (transfer-kda account MODULE_ADMIN_ACCOUNT kda-amount)
        (update-tti-balance account tti-to-issue)
        (update-reserve new-kda-reserve new-next-price new-tti-in-circulation)
        tti-to-issue)))

  (defun redeem-tti (account:string tti-amount:decimal)
    @doc "Redeem TTI tokens for KDA"
    (enforce (> tti-amount 0.0) "TTI amount must be positive")
    (with-read reserve "info" { "kdareserve" := kda-reserve, "ttiincirculation" := tti-in-circulation }
      (enforce (<= tti-amount tti-in-circulation) "Not enough TTI in circulation to redeem")
      (let* ((redemption-ratio (/ kda-reserve tti-in-circulation))
             (kda-to-redeem (* tti-amount redemption-ratio))
             (new-tti-in-circulation (- tti-in-circulation tti-amount))
             (new-kda-reserve (- kda-reserve kda-to-redeem)))
        (update-tti-balance account (- tti-amount))
        (update-kda-balance account kda-to-redeem)
        (update-reserve new-kda-reserve (at "nextprice" (get-reserve-info)) new-tti-in-circulation)
        kda-to-redeem)))
)
