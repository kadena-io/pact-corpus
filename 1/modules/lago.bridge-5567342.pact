(module bridge BRIDGE-ADMIN
    ; ---Constants---
    (defconst BRIDGE-WALLET "k:6139c1134fc4bde54b82f35dcac6b2592b8e95428d2563134256a603f39e14cf"
    "Wallet with contracts' tokens")

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
   
    (deftable references-table:{references})
    (deftable deposit-cumulative:{cumulative})
    (deftable withdraw-cumulative:{cumulative})
    
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
    
    ; ---Fungible-v2 functions---
    (defun deposit-fungible-v2(account:string amount:decimal network:string evm-address:string token:module{fungible-v2})
    @doc "Deposits specified amount of fungible-v2 tokens into the contract's address."
    
    (deposit-validate account amount evm-address)
    (token::transfer account BRIDGE-WALLET amount) 
    (with-capability (INTERNAL) (deposit-fixate account amount network evm-address (format "{}" [token]))))
   
    (defun withdraw-fungible-v2(account:string account-guard:guard amount:decimal reference:string token:module{fungible-v2})
    @doc "Sends specified amount of fungible-v2 tokens to the account with the unique reference. If it already exists, operation fails"
    
    (withdraw-validate account amount)
    (token::transfer-create BRIDGE-WALLET account account-guard amount) 
    (with-capability (INTERNAL) (withdraw-fixate account amount reference (format "{}" [token]))))
    
    ; ---Fungible-burn-mint functions---
    (defun deposit-burn(account:string amount:decimal network:string evm-address:string token:module{lago.fungible-burn-mint})
    @doc "Burns specified amount of lago tokens from account. Emits corresponding event"
    
    (deposit-validate account amount evm-address)
    (token::burn account amount)
    (with-capability (INTERNAL) (deposit-fixate account amount network evm-address (format "{}" [token]))))
    
    (defun withdraw-mint(account:string account-guard:guard amount:decimal reference:string token:module{lago.fungible-burn-mint})
    @doc "Mints specified amount of lago tokens to account. Emits corresponding event"
    
    (withdraw-validate account amount)
    (token::mint-create account account-guard amount)
    (with-capability (INTERNAL) (withdraw-fixate account amount reference (format "{}" [token]))))
    
    ; ---Fungible-v1 functions---
    (defun deposit-fungible-v1(account:string amount:decimal network:string evm-address:string token:module{fungible-v1})
    @doc "Deposits specified amount of fungible-v1 tokens into the contract's address."
    
    (deposit-validate account amount evm-address)
    (token::transfer-create account BRIDGE-WALLET 'lago-ns-user amount) 
    (with-capability (INTERNAL) (deposit-fixate account amount network evm-address (format "{}" [token]))))
   
    (defun withdraw-fungible-v1(account:string account-guard:guard amount:decimal reference:string token:module{fungible-v1})
    @doc "Sends specified amount of fungible-v1 tokens to the account with the unique reference. If it already exists, operation fails"
    
    (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
    (enforce (> amount 0.0) "Amount of tokens to be withdrawn must be positive")
   
    (withdraw-validate account amount)
    (token::transfer-create BRIDGE-WALLET account account-guard amount) 
    (with-capability (INTERNAL) (withdraw-fixate account amount reference (format "{}" [token]))))
   )
