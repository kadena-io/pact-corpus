(module lp-locker GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-keyset "n_67b8f433dc11f2dd8d34180c4adf17eafc9cb747.lpgov"))

(defschema lp-lock-schema
  @doc "Key is stringified token name + account"
  account:string
  total:decimal
  token:module{fungible-v2}
  unlockDate:time
)

(defschema total-tokens-schema
  @doc "Key is stringified token name"
  total:decimal
)

(deftable lp-lock:{lp-lock-schema})
(deftable total-tokens:{total-tokens-schema})

; Caps

(defcap VERIFY-LOCKER (account:string)
  (compose-capability (LOCKER account)))

(defcap UPDATES ()
    @doc "Internal Capability for updating the total tokens"
  true)

; Event Caps

(defcap ADD-LP (account:string amount:decimal total:decimal unlock:time tk:string) @event true)

(defcap REMOVE-LP (account:string amount:decimal total:decimal tk:string) @event true)

; Locker Account

(defcap LOCKER (account:string)
  @doc "cap for locker guard"
  (enforce-guard (at 'guard (coin.details account))))

(defun req-LOCKER (account:string)
(require-capability (LOCKER account)))

(defun LOCKER-guard (account:string)
  (create-user-guard (req-LOCKER account)))

(defun get-LOCKER-account (account:string)
  (create-principal (LOCKER-guard account)))

; Constants

(defconst YEAR:integer 31536000) ; 1 year in seconds
(defconst MONTH:integer 15768000) ; 6 months in seconds

; Main Functions

(defun lock-liquidity:string (account:string amount:decimal token:module{fungible-v2})
  @doc "Lock liqudity tokens"
  (let* (
          (name (format-name token account))
          (locker-account (get-LOCKER-account account))
          (lg (LOCKER-guard account))
          (tk (format "coin:{}" [token]))
          (cd (current-date))
        )
  (enforce (>= amount 0.0) "Amount must be positive")
  (enforce (is-principal account) "Must be a principal Account")

  (with-default-read lp-lock name
    { 'account: "", 'total: 0.0, 'token: token, 'unlockDate: (time "1971-01-01T00:00:00Z")}
    { 'account:= acct, 'total:= total, 'token:= token, 'unlockDate:= ud }

    (write lp-lock name {'account: account, 'total: (+ total amount), 'token: token, 'unlockDate: (if (= 0.0 total) (add-time cd YEAR) (add-time ud MONTH))})
      (emit-event (ADD-LP account amount (+ total amount) (add-time (current-date) YEAR) tk)))

  (with-capability (UPDATES)
    (update-tokens amount token))
    
    (kaddex.tokens.transfer-create tk account locker-account lg amount)
  )
)

(defun remove-liquidity:string (account:string amount:decimal token:module{fungible-v2})
  @doc "Remove liquidity tokens from lockup"
  (let* (
          (name (format-name token account))
          (locker-account (get-LOCKER-account account))
          (tk (format "coin:{}" [token]))
        )
  (enforce (>= amount 0.0) "Amount must be positive")
  (enforce (is-principal account) "Must be a principal Account")

  (with-read lp-lock name { 'account:= acct, 'total:= total, 'token:= token, 'unlockDate:= ud }
  (enforce (>= (current-date) ud) "Unable to withdraw before maturity date")
  (enforce (>= total amount) "Not enough tokens in lockup")
  (enforce (= account acct) "Not authorized to withdraw")

  (with-capability (VERIFY-LOCKER account)
    (write lp-lock name {'account: account, 'total: (- total amount), 'token: token, 'unlockDate: ud})

  (with-capability (UPDATES)
      (decrement-tokens amount token))

  (install-capability (kaddex.tokens.TRANSFER tk locker-account account amount))
  (kaddex.tokens.transfer tk locker-account account amount)))
  )
)

; Internal Functions

(defun update-tokens (amount:decimal token:module{fungible-v2})
    @doc "Update total tokens by stringified token name"
    (require-capability (UPDATES))
    (with-default-read total-tokens (format-string token) { 'total: 0.0 } { 'total:= total }
      (enforce (> amount 0.0) "Amount must be positive")

    (let ((tl (+ total amount)))
        (write total-tokens (format-string token) {'total: tl}))))

(defun decrement-tokens (amount:decimal token:module{fungible-v2})
    @doc "Remove total tokens by stringified token name"
    (require-capability (UPDATES))
    (with-default-read total-tokens (format-string token) { 'total: 0.0 } { 'total:= total }
      (enforce (> amount 0.0) "Amount must be positive")

    (let ((tl (- total amount)))
        (enforce (>= tl 0.0) "Total tokens can't be negative")
        (write total-tokens (format-string token) {'total: tl}))))

; Helper Functions

(defun format-name:string (token:module{fungible-v2} account:string)
  @doc "Format the name of the token"
  (format "{}:{}" [token account]))

(defun format-string:string (token:module{fungible-v2})
  @doc "Format the name of the token"
  (format "{}" [token]))

(defun current-date:time ()
  @doc "Get the current date"
  (at 'block-time (chain-data)))

(defun get-lockup (account:string token:module{fungible-v2})
  @doc "Return lockup data"
 (read lp-lock (format-name token account)))

(defun get-total-by-name:object (token:module{fungible-v2})
  @doc "Return total tokens by name"
  (read total-tokens (format-string token)))

(defun get-lockups ()
  (keys lp-lock)) 

(defun get-token-list ()
  (keys total-tokens))

)


