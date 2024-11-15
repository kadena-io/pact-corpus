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
    (defcap MINTED (account amount token)
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
    (defun create-cumulative(token:string initial-amount:decimal)
        @doc "Inserts token and tx-number into mint-cumulative table if it does not exist. Only called inside the module."

        (require-capability (INTERNAL))
        (insert mint-cumulative token {"amount": initial-amount, "tx-number": 1}))

    (defun mint-update(token:string increase:decimal)
        @doc "Updates amount and tx-number. Only called inside the module."

        (require-capability (INTERNAL))
        (with-read mint-cumulative token {"amount" := current-amount, "tx-number" := current-tx-number}
          (update mint-cumulative token {"amount": (+ current-amount increase), "tx-number": (+ current-tx-number 1)})))

    (defun redeem-update(token:string decrease:decimal)
        @doc "Updates amount and tx-number. Only called inside the module."
  
        (require-capability (INTERNAL))
        (with-read mint-cumulative token {"amount" := current-amount, "tx-number" := current-tx-number}
            (update mint-cumulative token {"amount": (- current-amount decrease), "tx-number": (+ current-tx-number 1)})
              (enforce (>= current-amount 0) "Invalid amount")))

    (defun mint-append (token:string amount:decimal)
        @doc "Checks whether token is already present in table. If not - inserts it, otherwise updates it. Only called inside the module."

        (require-capability (INTERNAL))
        (if (contains token (keys mint-cumulative))
            (mint-update token amount) 
            (create-cumulative token amount)))

    (defun return-supply:object{cumulative}(token:string)
        @doc "Returns circulating supply of USD2."
        
        (with-read mint-cumulative token {"amount":=amount-mint,"tx-number":=tx-number-mint}
        (format "{}" [amount-mint])))

    (defun return-transactions:object{cumulative}(token:string)
        @doc "Returns number of USD2 transactions."
        
        (with-read mint-cumulative token {"amount":=amount-mint,"tx-number":=tx-number-mint}
        (format "{}" [tx-number-mint])))

    (defun record-mint(account:string amount:decimal token:string)
        @doc "Records data after a USD2 minting transaction. Only called inside the module"
        
        (require-capability (INTERNAL))
        (mint-append token amount)
        (emit-event (MINTED account amount token))
        (format "{} {} were minted to {}" [amount token account]))

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

    ; ---Minting and Redemption functions---
    (defun mint-token(account:string account-guard:guard amount:decimal)
        @doc "Mints specified amount of USD2 tokens while depositing kwUSDC from user as collateral."
        
        (enforce-global-lock)
        (with-capability (MINTER)
        (transaction-validate account amount)
        (lago.kwUSDC.transfer account COLLATERAL-WALLET amount)
        (lago.USD2.mint-create account account-guard amount)
        (with-capability (INTERNAL) (record-mint account amount "lago.USD2"))))

    (defun redeem-token(account:string amount:decimal)
        @doc "Burns specified amount of USD2 tokens while returning collateral in kwUSDC to the user."
        
        (enforce-global-lock)
        (with-capability (MINTER)
        (transaction-validate account amount)
        (lago.USD2.burn account amount)
        (lago.kwUSDC.transfer COLLATERAL-WALLET account amount)
        (with-capability (INTERNAL) (record-redeem account amount "lago.USD2"))))

    (defun init (initial-lock:bool)
        @doc "Initialize the contract."
        (enforce-keyset "lago-ns-user")
        (insert contract-lock CONTRACT_LOCK_KEY {'lock: initial-lock})))

    
