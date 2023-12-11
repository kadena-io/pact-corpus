(module guards AUTONOMOUS

  "Functions for implementing various user guards."

  (defcap AUTONOMOUS ()
    (enforce false "Non-upgradeable"))

  (defun after-date:guard (date:time)
    "Guard to enforce chain time is after DATE."
    (create-user-guard (enforce-after-date date)))

  (defun enforce-after-date:bool (date:time)
    (enforce-time date "after"
                  (> (chain-time) date)))


  (defun at-after-date:guard (date:time)
    "Guard to enforce chain time is at or after DATE."
    (create-user-guard (enforce-at-after-date date)))

  (defun enforce-at-after-date:bool (date:time)
    (enforce-time date "at or after"
                  (>= (chain-time) date)))


  (defun before-date:guard (date:time)
    "Guard to enforce chain time is before DATE."
    (create-user-guard (enforce-before-date date)))

  (defun enforce-before-date:bool (date:time)
    (enforce-time date "before"
                  (< (chain-time) date)))

  (defun at-before-date:guard (date:time)
    "Guard to enforce chain time is at or before DATE."
    (create-user-guard (enforce-at-before-date date)))

  (defun enforce-at-before-date:bool (date:time)
    (enforce-time date "at or before"
                  (<= (chain-time) date)))

  (defun enforce-time:bool (date:time msg:string test:bool)
    (enforce test
             (format "Chain time must be {} {}" [msg date])))

  (defun chain-time:time ()
    (at 'block-time (chain-data)))

  (defun guard-and:guard (a:guard b:guard)
    "Guard to enforce both A and B."
    (create-user-guard (enforce-and a b)))

  (defun enforce-and:bool (a:guard b:guard)
    (enforce-guard a)
    (enforce-guard b))

  (defun guard-or:guard (a:guard b:guard)
    "Guard to enforce A or B."
    (create-user-guard (enforce-or a b)))

  (defun enforce-or:bool (a:guard b:guard)
    (enforce-one
     (format "Enforce {} or {}" [a b])
     [(enforce-guard a)
      (enforce-guard b)]))
  )

;; guards1.pact


(module guards1 'util-ns-admin
  "************************WARNING************************\
  \ This module is currently governed by 'util-ns-admin   \
  \ and should not be in use until the governance is      \
  \ replaced with AUTONOMOUS, meaning that the module     \
  \ will be non-upgradable.                               \
  \ ******************************************************\
  \ Functions for implementing gas guards."

;  (defcap AUTONOMOUS ()
;    (enforce false "Non-upgradeable"))

  (defun guard-all:guard (guards:[guard])
    "Create a guard that only succeeds if every guard in GUARDS is successfully enforced."
    (enforce (< 0 (length guards)) "Guard list cannot be empty")
    (create-user-guard (enforce-guard-all guards)))

  (defun enforce-guard-all:bool (guards:[guard])
    "Enforces all guards in GUARDS"
    (map (enforce-guard) guards)
  )

  (defun guard-any:guard (guards:[guard])
    "Create a guard that succeeds if at least one guard in GUARDS is successfully enforced."
    (enforce (< 0 (length guards)) "Guard list cannot be empty")
    (create-user-guard (enforce-guard-any guards)))

  (defun enforce-guard-any:bool (guards:[guard])
    "Will succeed if at least one guard in GUARDS is successfully enforced."
    (enforce (< 0
      (length
        (filter
          (= true)
          (map (try-enforce-guard) guards))))
      "None of the guards passed")
  )

  (defun try-enforce-guard (g:guard)
    (try false (enforce-guard g))
  )

  (defun max-gas-notional:guard (gasNotional:decimal)
    "Guard to enforce gas price * gas limit is smaller than or equal to GAS"
    (create-user-guard
      (enforce-below-or-at-gas-notional gasNotional)))

  (defun enforce-below-gas-notional (gasNotional:decimal)
    (enforce (< (chain-gas-notional) gasNotional)
      (format "Gas Limit * Gas Price must be smaller than {}" [gasNotional])))

  (defun enforce-below-or-at-gas-notional (gasNotional:decimal)
    (enforce (<= (chain-gas-notional) gasNotional)
      (format "Gas Limit * Gas Price must be smaller than or equal to {}" [gasNotional])))

  (defun max-gas-price:guard (gasPrice:decimal)
    "Guard to enforce gas price is smaller than or equal to GAS PRICE"
    (create-user-guard
      (enforce-below-or-at-gas-price gasPrice)))

  (defun enforce-below-gas-price:bool (gasPrice:decimal)
    (enforce (< (chain-gas-price) gasPrice)
      (format "Gas Price must be smaller than {}" [gasPrice])))

  (defun enforce-below-or-at-gas-price:bool (gasPrice:decimal)
    (enforce (<= (chain-gas-price) gasPrice)
      (format "Gas Price must be smaller than or equal to {}" [gasPrice])))

  (defun max-gas-limit:guard (gasLimit:integer)
    "Guard to enforce gas limit is smaller than or equal to GAS LIMIT"
    (create-user-guard
      (enforce-below-or-at-gas-limit gasLimit)))

  (defun enforce-below-gas-limit:bool (gasLimit:integer)
    (enforce (< (chain-gas-limit) gasLimit)
      (format "Gas Limit must be smaller than {}" [gasLimit])))

  (defun enforce-below-or-at-gas-limit:bool (gasLimit:integer)
    (enforce (<= (chain-gas-limit) gasLimit)
      (format "Gas Limit must be smaller than or equal to {}" [gasLimit])))

  (defun chain-gas-price ()
    "Return gas price from chain-data"
    (at 'gas-price (chain-data)))

  (defun chain-gas-limit ()
    "Return gas limit from chain-data"
    (at 'gas-limit (chain-data)))

  (defun chain-gas-notional ()
    "Return gas limit * gas price from chain-data"
    (* (chain-gas-price) (chain-gas-limit)))
)

;;Create gas station

