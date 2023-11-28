(module bridge GOVERNANCE

  (bless "RPpLsfG0p3aSfE5OXonpQUQ9oNRukLA4quAF3STJ0qk")

  (defcap GOVERNANCE () (enforce-guard (keyset-ref-guard "n_b742b4e9c600892af545afb408326e82a6c0c6ed.bridge-admin")))

  (defcap BRIDGE_OP () true)
  (defun require-BRIDGE_OP ()
    (require-capability (BRIDGE_OP)))
  (defun create-BRIDGE_OP-guard ()
    (create-user-guard (require-BRIDGE_OP)))

  (defun enforce-bridge-guard ()
    (enforce-guard (create-BRIDGE_OP-guard))
    (enforce-chain)
  )

  ;; key is the string representation of the token module reference (n_b742b4e9c600892af545afb408326e82a6c0c6ed.zUSD)
  (defschema mint-tracking-schema
    total:decimal)
  (deftable mint-tracking:{mint-tracking-schema})

  ;; key is the string representation of the token module reference (n_b742b4e9c600892af545afb408326e82a6c0c6ed.zUSD)
  (defschema burn-tracking-schema
    total:decimal)
  (deftable burn-tracking:{burn-tracking-schema})

  ;; key is the string representation of the token module reference (n_b742b4e9c600892af545afb408326e82a6c0c6ed.zUSD)
  (defschema bridge-schema
    max-amount:decimal
    min-amount:decimal)
  (deftable bridge:{bridge-schema})

  (defcap WRAP (token:module{fungible-v2} account:string amount:decimal info:string)
    @event
    (compose-capability (GOVERNANCE))
    (enforce (= (take 43 (format-token token)) "n_b742b4e9c600892af545afb408326e82a6c0c6ed.") "Only contracts in n_b742b4e9c600892af545afb408326e82a6c0c6ed namespace are supported")
    (enforce-amount token amount)
    (compose-capability (BRIDGE_OP)))

  (defcap UNWRAP (token:module{fungible-v2} account:string amount:decimal info:string)
    @event
    (enforce-amount token amount)
    (compose-capability (BRIDGE_OP)))

  (defun truncate-token:decimal (token:module{fungible-v2} amount:decimal)
    @doc "Truncates the amount to the token's precision"
    (floor amount (token::precision)))

  (defconst BRIDGE_CHAIN:string "2")
   
  ; ---BPS Functions---
  (defun set-min-max-amount:string (token:module{fungible-v2} min-amount:decimal max-amount:decimal)
  @doc "Updates the bridge minimum and maximum amount limits."
  (enforce (and
    (>= min-amount 0.0) (>= max-amount 0.0))
    "Minimum and Maximum amount should be non-negative")
  (with-capability (GOVERNANCE)
    (update bridge (format-token token) {"min-amount": min-amount, "max-amount": max-amount})
    (format "New minimum and maximum amount limits for {} are {} and {}." [(format-token token) min-amount max-amount])))

  (defun enforce-amount (token:module{fungible-v2} amount:decimal)
    @doc "Checks if the amount is valid"
    (with-read bridge (format-token token)
      { 'min-amount:= min-amount,
        'max-amount:= max-amount }
        (enforce
          (and
            (>= (truncate-token token amount) min-amount)
            (<= (truncate-token token amount) max-amount))
          (format "Amount should be between {} and {}" [min-amount max-amount]))))

  (defun enforce-chain:bool ()
    (enforce
      (= BRIDGE_CHAIN (at 'chain-id (chain-data)))
      (format "Bridge is supported only in chain {}" [BRIDGE_CHAIN]))
  )

  (defun format-token:string(token:module{fungible-v2})
    @doc "Returns the string representation of the module reference"
    (format "{}" [token]))

  ; ---Bridge functions---
  (defun wrap (token:module{wrapped-mint-burn} account:string guard:guard amount:decimal info:string)
    @doc "Wrap/mint tokens corresponding to a lockup on another platform."
      (with-capability (WRAP token account amount info)
        (token::mint account guard amount)
        (with-default-read mint-tracking (format-token token) { "total": 0.0 } { "total" := total}
                       (write mint-tracking (format-token token)  { "total": (+ total amount)}))
        (format "Wrapped: {} {} to {} info {}" [amount (format-token token) account info])))

  (defun unwrap (token:module{wrapped-mint-burn} account:string guard:guard amount:decimal info:string)
    @doc "Unwrap tokens corresponding to a release amount on another platform."
      (with-capability (UNWRAP token account amount info)
        (token::burn account amount)
        (with-default-read burn-tracking (format-token token) { "total": 0.0 } { "total" := total}
                       (write burn-tracking (format-token token)  { "total": (+ total amount)}))
        (format "Unwrapped: {} {} by {} info {}" [amount (format-token token) account info])))

  ; ---Bridge utils functions---
  (defun get-total-minted:decimal (token:module{wrapped-mint-burn})
    @doc "Returns how many tokens have been minted total"
      (with-default-read mint-tracking (format-token token) { "total": 0.0 } { "total" := total} total))

  (defun get-total-burned:decimal (token:module{wrapped-mint-burn})
    @doc "Returns how many tokens have been burned total"
      (with-default-read burn-tracking (format-token token) { "total": 0.0 } { "total" := total} total))

  (defun initialize-token (token:module{fungible-v2} min-amount:decimal max-amount:decimal)
    @doc "Initialize the token with min and max limits"
    (enforce (and
      (>= min-amount 0.0) (>= max-amount 0.0))
      "Minimum and Maximum amount should be non-negative")
    (enforce (= (take 43 (format-token token)) "n_b742b4e9c600892af545afb408326e82a6c0c6ed.") "Only contracts in n_b742b4e9c600892af545afb408326e82a6c0c6ed namespace are supported")
      (with-capability (GOVERNANCE)
        (insert bridge (format-token token) {'min-amount:min-amount, 'max-amount:max-amount})
        (format "{} initialized and minimum and maximum amount limits are {} {}" [(format-token token) min-amount max-amount])
      ))
)


