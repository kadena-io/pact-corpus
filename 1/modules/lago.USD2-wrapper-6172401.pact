(module USD2-wrapper MINTER-ADMIN

    @doc "USD2 minting and redemption contract"

    ; ---Constants---
    (defconst COLLATERAL-WALLET "k:477e933fa79f25e7dc82952befb8c31613ad9b5a13cbcb937e135900be266f59"
      "Wallet with kwUSDC collateral")
    (defconst CONTRACT_LOCK_KEY 'lock)
    
    ; ---Events---
    (defcap REDEEMED (account amount token)
      @event true
    )

    ; ---Capabilities---
    (defcap MINTER-ADMIN () "Admin-only." (enforce-keyset 'lago-ns-user))
    (defcap INTERNAL () "Private defcap for internal operations." true)
    (defcap MINTER () true)

    ; ---Schemas and Tables---
    (defschema cumulative
        tx-number:integer
        amount:decimal)

    (defschema contract-lock-status
        lock:bool)

    (deftable mint-cumulative:{cumulative})
    (deftable contract-lock:{contract-lock-status})

    ; --- Wrapper Enforcement---
    (defun enforce-minter ()
      (require-capability (MINTER))
    )
    (defun create-wrapper-guard ()
      (create-user-guard (enforce-minter))
    )

    ; --- Global Lock ---
    (defun enforce-global-lock ()
      "Enforces the global lock in the contract."
      (with-read contract-lock CONTRACT_LOCK_KEY { 'lock := lock }
       (enforce (not lock) "Contract is locked")))

    (defun set-contract-lock
       ( lock:bool )
       "Creates a global lock in the contract."
       (enforce-keyset "lago-ns-user")
           (write contract-lock CONTRACT_LOCK_KEY {'lock: lock}))
    
    ; ---Statistics Table Functions---
    (defun redeem-update(token:string decrease:decimal)
        @doc "Updates amount and tx-number. Only called inside the module."
  
        (require-capability (INTERNAL))
        (with-read mint-cumulative token {"amount" := current-amount, "tx-number" := current-tx-number}
            (update mint-cumulative token {"amount": (- current-amount decrease), "tx-number": (+ current-tx-number 1)})
              (enforce (> current-amount 0.0) "Invalid amount")))

    (defun return-supply:object{cumulative}(token:string)
        @doc "Returns circulating supply of USD2."
        
        (with-read mint-cumulative token {"amount":=amount-mint,"tx-number":=tx-number-mint}
        (format "{}" [amount-mint])))

    (defun return-transactions:object{cumulative}(token:string)
        @doc "Returns number of USD2 transactions."
        
        (with-read mint-cumulative token {"amount":=amount-mint,"tx-number":=tx-number-mint}
        (format "{}" [tx-number-mint])))

    (defun record-redeem(account:string amount:decimal token:string)
        @doc "Records data after a USD2 redemption transaction. Only called inside the module"
        
        (require-capability (INTERNAL))
        (redeem-update token amount)
        (emit-event (REDEEMED account amount token))
        (format "{} {} were redeemed from {}" [amount token account]))

    ; ---Transaction validation---
    (defun transaction-validate(account:string amount:decimal)
        @doc "Validates whether the input in the transaction is valid"
        
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce (> amount 0.0) "Amount of tokens to be deposited must be positive"))

    ; ---Redemption function---
    (defun redeem-token(account:string guard:guard amount:decimal)
        @doc "Burns specified amount of USD2 tokens while returning collateral in kwUSDC to the user."
        
        (enforce-global-lock)
        (with-capability (MINTER)
        (transaction-validate account amount)
        (lago.USD2.burn account amount)
        (lago.kwUSDC.transfer-create COLLATERAL-WALLET account guard amount)
        (with-capability (INTERNAL) (record-redeem account amount "lago.USD2"))))

    (defun init (initial-lock:bool)
        @doc "Initialize the contract."
        (enforce-keyset "lago-ns-user")
        (insert contract-lock CONTRACT_LOCK_KEY {'lock: initial-lock})))
