(module bridge BRIDGE-ADMIN
    ; ---Constants---
    (defconst BRIDGE-WALLET "k:6139c1134fc4bde54b82f35dcac6b2592b8e95428d2563134256a603f39e14cf"
    "Wallet with contracts' tokens")
    (defconst CONTRACT_LOCK_KEY 'lock)

    (bless "KRbsKX-337zzlfTsXJKOiaZrK5ijiGrA3yZQrS-MUD4")
    
    ; ---Events---
    (defcap DEPOSITED (account amount network evm-address token)
    @doc "Event for deposit event."
    @event true
    )
    (defcap WITHDRAWN (account amount reference token)
    @doc "Event for withdraw event."
    @event true
    )
    
    ; ---Capabilities---
    (defcap BRIDGE-ADMIN () (enforce-guard 'lago-ns-user))
    (defcap INTERNAL () "Private defcap for internal operations." true)
   
    ; ---Schemas and tables---
    (defschema references
    account:string
    amount:decimal
    token:string)
   
    (defschema cumulative
    tx-number:integer
    amount:decimal)

    (defschema contract-lock-status
        lock:bool)
   
    (deftable references-table:{references})
    (deftable deposit-cumulative:{cumulative})
    (deftable withdraw-cumulative:{cumulative})
    (deftable contract-lock:{contract-lock-status})

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
    (defun insert-cumulative(token:string initial-amount:decimal table-cumulative:object{cumulative})
    @doc "Inserts token and initial parameters into cumulative table if it does not exist. Only called inside module"
    
    (require-capability (INTERNAL))
    (insert table-cumulative token {"amount": initial-amount, "tx-number": 1}))
   
    (defun update-cumulative(token:string increase:decimal table-cumulative:object{cumulative})
    @doc "Updates token and initial parameters if it does not exist. Only called inside module"
    
    (require-capability (INTERNAL))
    (with-read table-cumulative token {"amount" := current-amount, "tx-number" := current-tx-number}
    (update table-cumulative token {"amount": (+ current-amount increase), "tx-number": (+ current-tx-number 1)})))
    
    (defun append-cumulative (token:string amount:decimal table-cumulative:object{cumulative})
    @doc "Checks whether token is already present in table. If not - inserts it, otherwise updates. Only called inside module."
    
    (require-capability (INTERNAL))
    (if (contains token (keys table-cumulative))
    (update-cumulative token amount table-cumulative) 
    (insert-cumulative token amount table-cumulative)))
    
    (defun reference-info(reference)
    @doc "Get reference info"
    
    (read references-table reference))
   
    (defun get-cumulative:object{cumulative}(token:string)
    @doc "Get total cumulative volume data about a token from all tables."
    
    (with-read deposit-cumulative token {"amount":=amount-deposit,"tx-number":=tx-number-deposit}
    (with-read withdraw-cumulative token {"amount":=amount-withdraw,"tx-number":=tx-number-withdraw}
    {"amount": (+ amount-deposit amount-withdraw), "tx-number":(+ tx-number-deposit tx-number-withdraw)})))

    (defun get-supply:object{cumulative}(token:string)
    @doc "Get total cumulative supply data about a token from all tables."
    
    (with-read deposit-cumulative token {"amount":=amount-deposit,"tx-number":=tx-number-deposit}
    (with-read withdraw-cumulative token {"amount":=amount-withdraw,"tx-number":=tx-number-withdraw}
    {"amount": (- amount-withdraw amount-deposit), "tx-number":(+ tx-number-deposit tx-number-withdraw)})))
    
    ; --- Functions that fixate the data after deposit and withdraw transaction ---
    (defun deposit-fixate(account:string amount:decimal network:string evm-address:string token:string)
    @doc "Fixates data after a deposit transaction. Only called inside module"
    
    (require-capability (INTERNAL))
    (with-capability (INTERNAL) (append-cumulative token amount deposit-cumulative))
    (emit-event (DEPOSITED account amount network evm-address token))
    (format "{} {} were sent to {} by {}" [amount token evm-address account]))
   
    (defun withdraw-fixate(account:string amount:decimal reference:string token:string)
    @doc "Fixates data after a withdraw transaction. Only called inside module"
    
    (require-capability (INTERNAL))
    (insert references-table reference {"account": account, "amount": amount, "token": token})
    (with-capability (INTERNAL) (append-cumulative token amount withdraw-cumulative))
    (emit-event (WITHDRAWN account amount reference token))
    (format "{} {} were given to {} with unique reference {}" [amount token account reference]))
    
    ; --- Validators ---
    (defun deposit-validate(account:string amount:decimal evm-address:string)
    @doc "Validates whether the input in the deposit transaction is valid"
    
    (enforce (= "0x" (take 2 evm-address)) "Address should begin with 0x")
    (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
    (enforce (> amount 0.0) "Amount of tokens to be deposited must be positive"))
   
    (defun withdraw-validate(account:string amount:decimal)
    @doc "Validates whether the input in the withdraw transaction is valid"
    
    (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
    (enforce (> amount 0.0) "Amount of tokens to be withdrawn must be positive"))
    
    ; ---kwUSDC burn/mint functions---
    (defun burn-kwUSDC(account:string amount:decimal network:string evm-address:string)
    @doc "Burns specified amount of kwUSDC tokens from account. Emits corresponding event"
    
    (enforce-global-lock)
    (deposit-validate account amount evm-address)
    (lago.kwUSDC.burn account amount)
    (with-capability (INTERNAL) (deposit-fixate account amount network evm-address "lago.kwUSDC")))
    
    (defun mint-kwUSDC(account:string account-guard:guard amount:decimal reference:string)
    @doc "Mints specified amount of kwUSDC tokens to account. Emits corresponding event"
    
    (enforce-global-lock)
    (withdraw-validate account amount)
    (lago.kwUSDC.mint-create account account-guard amount)
    (with-capability (INTERNAL) (withdraw-fixate account amount reference "lago.kwUSDC")))

    ; ---kwBTC burn/mint functions---
    (defun burn-kwBTC(account:string amount:decimal network:string evm-address:string)
    @doc "Burns specified amount of kwBTC tokens from account. Emits corresponding event"
    
    (enforce-global-lock)
    (deposit-validate account amount evm-address)
    (lago.kwBTC.burn account amount)
    (with-capability (INTERNAL) (deposit-fixate account amount network evm-address "lago.kwBTC")))
    
    (defun mint-kwBTC(account:string account-guard:guard amount:decimal reference:string)
    @doc "Mints specified amount of kwBTC tokens to account. Emits corresponding event"
    
    (enforce-global-lock)
    (withdraw-validate account amount)
    (lago.kwBTC.mint-create account account-guard amount)
    (with-capability (INTERNAL) (withdraw-fixate account amount reference "lago.kwBTC")))
   
   (defun init (initial-lock:bool)
        @doc "Initialize the contract."
        (enforce-keyset "lago-ns-user")
        (insert contract-lock CONTRACT_LOCK_KEY {'lock: initial-lock})))

    
