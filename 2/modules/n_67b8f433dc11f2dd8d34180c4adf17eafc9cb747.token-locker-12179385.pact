(module token-locker GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-keyset "n_67b8f433dc11f2dd8d34180c4adf17eafc9cb747.lpgov"))

; Data Structures

(defschema token-lock-schema
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

(deftable token-lock:{token-lock-schema})
(deftable total-tokens:{total-tokens-schema})

; Capabilities

(defcap VERIFY-LOCKER (account:string)
  (compose-capability (LOCKER account)))

(defcap UPDATES ()
    @doc "Internal Capability for updating the total tokens"
  true)

; Event Caps

(defcap ADD-TK (account:string amount:decimal total:decimal unlock:time tk:string) @event true)

(defcap REMOVE-TK (account:string amount:decimal total:decimal tk:string) @event true)

; Locker Account

(defcap LOCKER (account:string)
  @doc "cap for locker guard"
  (enforce-guard (at 'guard (coin.details account))))

(defun req-LOCKER (account:string)
(require-capability (LOCKER account)))

(defun LOCKER-guard ( account:string)
  (create-user-guard (req-LOCKER account)))

(defun get-LOCKER-account ( account:string)
  (create-principal (LOCKER-guard account)))

; Constants

(defconst YEAR:integer 31536000) ; 1 year in seconds
(defconst MONTH:integer 15768000) ; 6 months in seconds

; Main Functions

(defun lock-tokens:string (account:string amount:decimal token:module{fungible-v2})
  @doc "Lock Tokens in a user guarded account"
  (let* (
          (name (format-name token account))
          (locker-account (get-LOCKER-account account))
          (lg (LOCKER-guard account))
          (cd (current-date))
        )
  (enforce (>= amount 0.0) "Amount must be positive")
  (enforce (is-principal account) "Must be a principal Account")

  (with-default-read token-lock name
    { 'account: "", 'total: 0.0, 'token: token, 'unlockDate: 0 }
    { 'account:= acct, 'total:= total, 'token:= token, 'unlockDate:= ud }

    (write token-lock name {'account: account, 'total: (+ total amount), 'token: token, 'unlockDate: (if (= 0.0 total) (add-time cd YEAR) (add-time ud MONTH))})
      (emit-event (ADD-TK account amount (+ total amount) cd (format-string token))))

  (with-capability (UPDATES)
    (update-tokens amount token))

  (token::transfer-create account locker-account lg amount)
  )
)

(defun remove-tokens:string (account:string amount:decimal token:module{fungible-v2})
  @doc "Remove tokens from lockup"
  (let* (
          (name (format-name token account))
          (locker-account (get-LOCKER-account account))
        )
  (enforce (>= amount 0.0) "Amount must be positive")
  (enforce (is-principal account) "Must be a principal Account")

  (with-read token-lock name { 'account:= acct, 'total:= total, 'token:= token, 'unlockDate:= ud }
  (enforce (>= (current-date) ud) "Unable to withdraw before maturity date")
  (enforce (>= total amount) "Not enough tokens in lockup")
  (enforce (= account acct) "Not authorized to withdraw")

  (with-capability (VERIFY-LOCKER account)
    (write token-lock name {'account: account, 'total: (- total amount), 'token: token, 'unlockDate: ud})
    (emit-event (REMOVE-TK account amount (- total amount) (format-string token)))

  (with-capability (UPDATES)
    (decrement-tokens amount token))

  (install-capability (token::TRANSFER locker-account account amount))
  (token::transfer locker-account account amount)))
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
 (read token-lock (format-name token account)))

(defun get-total-by-name:object (token:module{fungible-v2})
  @doc "Return total tokens by name"
  (read total-tokens (format-string token)))

(defun get-lockups ()
  (keys token-lock)) 

(defun get-token-list ()
  (keys total-tokens))

)
