(module USD2-wrapper MINTER-ADMIN

    @doc "USD2 minting and redemption contract"

    ; ---Constants---
    (defconst COLLATERAL-WALLET "k:05980704ad5f75df37fd3be10529987e1fb3f3a6168a49da4cc25fcea7b09787"
      "Wallet with kwUSDC collateral")

    (defconst COLD-STORAGE "k:0fbfd3c1efc4631d523ef8076ac4573b75543eb9b871e3744eb32ccf4fcd463d"
      "Cold Wallet")
    
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

    (deftable mint-cumulative:{cumulative})

    ; --- Wrapper Enforcement---
    (defun enforce-minter ()
      (require-capability (MINTER))
    )
    (defun create-wrapper-guard ()
      (create-user-guard (enforce-minter))
    )
    
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
            (update mint-cumulative token {"amount": (- current-amount decrease), "tx-number": (+ current-tx-number 1)})))

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
    (defun mint-token(account:string account-guard:guard amount:decimal token:module{lago.fungible-burn-mint})
        @doc "Mints specified amount of USD2 tokens while depositing kwUSDC from user as collateral."
        
        (with-capability (MINTER)
        (transaction-validate account amount)
        (lago.kwUSDC.transfer account COLLATERAL-WALLET amount)
        (token::mint-create account account-guard amount)
        (with-capability (INTERNAL) (record-mint account amount (format "{}" [token])))))

    (defun redeem-token(account:string amount:decimal token:module{lago.fungible-burn-mint})
        @doc "Burns specified amount of USD2 tokens while returning collateral in kwUSDC to the user."
        
        (with-capability (MINTER)
        (transaction-validate account amount)
        (token::burn account amount)
        (lago.kwUSDC.transfer COLLATERAL-WALLET account amount)
        (with-capability (INTERNAL) (record-redeem account amount (format "{}" [token])))))
    
    (defun cold-wallet-storage (amount:decimal)
        (enforce-keyset 'lago-ns-user)
        (with-capability (MINTER)
        (lago.kwUSDC.transfer COLLATERAL-WALLET COLD-STORAGE amount))))


