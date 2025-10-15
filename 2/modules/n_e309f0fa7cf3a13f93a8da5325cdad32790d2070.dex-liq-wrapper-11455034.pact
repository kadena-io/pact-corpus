(module dex-liq-wrapper GOVERNANCE
(use kaddex.exchange [get-pair-key get-pair-by-key reserve-for add-liquidity swap remove-liquidity FEE])

(defcap GOVERNANCE ()
  (enforce-keyset "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.liq"))

(defcap ALLOW-NO-BURN (token:module{fungible-v2})
  true)

(defcap BURN-ADMIN ()
  (enforce-one "Must satisfy one of these guards"
    [(enforce-keyset "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.liq-burn-transfer")
    (enforce-guard (at 'guard (coin.details BURN1)))
    (enforce-guard (at 'guard (coin.details BURN2)))
    ]
  )
)

;; Constants
(defconst BURN1:string "k:dabfcd1efdac0f24942619a11c14561eeeda066650044afe91e23fc81bc2df1a")
(defconst BURN2:string "k:f186668ac03b8b152dc87eac29909f58e58453e2485a6d44046c3413cd77f789")

;; Main Functions

(defun kaddex-add-liquidity (tokenA:module{fungible-v2} tokenB:module{fungible-v2} tokenA-amount:decimal tokenB-amount:decimal min-liquidity:decimal max-amount:decimal account:string g:guard)
    @doc "Wrapper to Add liquidity to the dex"
    (with-capability (ALLOW-NO-BURN tokenA)
       (with-capability (ALLOW-NO-BURN tokenB)
           (kaddex.exchange.add-liquidity tokenA tokenB tokenA-amount tokenB-amount min-liquidity max-amount account account g)))
)

(defun kaddex-remove-liquidity (tokenA:module{fungible-v2} tokenB:module{fungible-v2} tokenA-amount:decimal tokenB-amount:decimal min-liquidity:decimal max-amount:decimal account:string g:guard)
    @doc "Wrapper to Remove liquidity from the dex"
    (with-capability (ALLOW-NO-BURN tokenA)
       (with-capability (ALLOW-NO-BURN tokenB)
           (kaddex.exchange.remove-liquidity tokenA tokenB tokenA-amount tokenB-amount min-liquidity max-amount account account g)))
)

(defun transfer:string (from:string to:string amount:decimal fun:module{fungible-v2})
    @doc "Wrapper to Transfer tokens from one account to another"  
    (with-capability (BURN-ADMIN)
      (fun::transfer from to amount))
      "Transfer successful"
)
  
)

     
