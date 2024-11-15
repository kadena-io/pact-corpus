(module ktoshi GOVERNANCE
  @model
    [ (defproperty conserves-mass
        (= (column-delta token-table 'balance) 0.0))

      (defproperty valid-account (account:string)
        (and
          (>= (length account) 3)
          (<= (length account) 256)))
    ]

  (implements n_625e9938ae84bdb7d190f14fc283c7a6dfc15d58.supply-control-v1)

  (implements fungible-v2)
  (implements fungible-xchain-v1)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema token-schema
    @doc "The KTOSHI contract token schema"
    @model [ (invariant (>= balance 0.0)) ]

    balance:decimal
    guard:guard)

  (deftable token-table:{token-schema})

  (defschema token-supply
      total-burned:decimal
      total-minted:decimal)

  (deftable supply-table:{token-supply})

  (defschema privilege
    guard:guard)

  (deftable privileges:{privilege})

  (defschema mint-cap
    maximum:decimal)

  (deftable mint-cap-table:{mint-cap})

  ;; this is a simple global lock that can be toggled by the operators to pause the contract if necessary
  (defschema contract-lock-status
      lock:bool)
  (deftable contract-lock:{contract-lock-status})
  (defconst CONTRACT_LOCK_KEY 'lock)
  (defun enforce-contract-unlocked ()
    "Asserts that the contract is not in a paused state."
    (with-read contract-lock CONTRACT_LOCK_KEY { 'lock := lock }
      (enforce (not lock) "Contract is paused")))
  (defun set-contract-lock
    ( lock:bool )
    (with-capability (OPS)
      (write contract-lock CONTRACT_LOCK_KEY {'lock: lock })))

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    (enforce-keyset "n_625e9938ae84bdb7d190f14fc283c7a6dfc15d58.ktoshi-admin-keyset"))

  (defcap OPS ()
    (enforce-keyset "n_625e9938ae84bdb7d190f14fc283c7a6dfc15d58.ktoshi-ops-keyset"))

  (defcap BURN
    ( sender:string
      amount:decimal
    )
    "Capability for privileged intra-ecosystem burns"
    (enforce-contract-unlocked)
    (enforce-privilege BURN_PRIVILEGE)
    (compose-capability (DEBIT sender))
  )

  (defcap MINT
    ( receiver:string
      amount:decimal
    )
    (enforce-contract-unlocked)
    (enforce-privilege MINT_PRIVILEGE)
    (compose-capability (CREDIT receiver))
  )

  (defcap DEBIT (sender:string)
    "Capability for managing debiting operations"
    (enforce-contract-unlocked)
    (enforce-guard (at 'guard (read token-table sender)))
    (enforce (!= sender "") "valid sender"))

  (defcap CREDIT (receiver:string)
    "Capability for managing crediting operations"
    (enforce-contract-unlocked)
    (enforce (!= receiver "") "valid receiver"))

  (defcap ROTATE (account:string)
    @doc "Autonomously managed capability for guard rotation"
    @managed
    (enforce-contract-unlocked)
  )

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal
    )
    @doc "Autonomously managed capability for token transfers"
    @managed amount TRANSFER-mgr
    (enforce-contract-unlocked)
    (enforce (!= sender receiver) "same sender and receiver")
    (enforce-unit amount)
    (enforce (> amount 0.0) "Positive amount")
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver))
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )

    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  (defcap TRANSFER_XCHAIN:bool
    ( sender:string
      receiver:string
      amount:decimal
      target-chain:string
    )

    @managed amount TRANSFER_XCHAIN-mgr
    (enforce-contract-unlocked)
    (enforce-valid-chain target-chain)
    (enforce-unit amount)
    (enforce (> amount 0.0) "Cross-chain transfers require a positive amount")
    (compose-capability (DEBIT sender))
  )

  (defun TRANSFER_XCHAIN-mgr:decimal
    ( managed:decimal
      requested:decimal
    )

    (enforce (>= managed requested)
      (format "TRANSFER_XCHAIN exceeded for balance {}" [managed]))
    0.0
  )

  (defcap TRANSFER_XCHAIN_RECD:bool
    ( sender:string
      receiver:string
      amount:decimal
      source-chain:string
    )
    @event true
  )

  ; --------------------------------------------------------------------------
  ; Constants

  (defconst TOKEN_PURPOSES
    [{ 'purpose: 'presale,         'percentage-cap: 0.25 }
     { 'purpose: 'airdrop,         'percentage-cap: 0.05 }
     { 'purpose: 'team,            'percentage-cap: 0.05 }
     { 'purpose: 'treasury,        'percentage-cap: 0.1 }
     { 'purpose: 'liquidity,       'percentage-cap: 0.55 }
     ] ;; the percentage-cap says how much % of the MAX_SUPPLY can be minted for this purpose
    )

  (defconst MAX_SUPPLY:decimal 21000000000000.0
    " Initial supply of 21,000,000,000,000 tokens.")

  (defconst COIN_CHARSET CHARSET_LATIN1
    "The default KTOSHI contract character set")

  (defconst MINIMUM_PRECISION 12
    "Minimum allowed precision for KTOSHI transactions")

  (defconst MINIMUM_ACCOUNT_LENGTH 3
    "Minimum account length admissible for KTOSHI accounts")

  (defconst MAXIMUM_ACCOUNT_LENGTH 256
    "Maximum account name length admissible for KTOSHI accounts")

  (defconst VALID_CHAINS (map (int-to-str 10) (enumerate 0 19))
    "List of currently valid chain ids")

  (defconst ISSUANCE_ACCOUNT 'ktoshi-bank)

  (defconst BURN_PRIVILEGE 'burn)

  (defconst MINT_PRIVILEGE 'mint)

  (defconst MINT_CAP_KEY 'cap)

  ; --------------------------------------------------------------------------
  ; Utilities

  (defun get-purpose-list:[string] ()
    (map (lambda (o) (at 'purpose o)) TOKEN_PURPOSES))

  (defun enforce-valid-purpose (purpose:string)
    (enforce (contains purpose (get-purpose-list)) (format "Invalid token purpose {}" [purpose]))
  )

  (defun get-purpose-max-cap (purpose:string)
    (enforce-valid-purpose purpose)
    (let ( (p (filter (lambda (x) (= purpose (at 'purpose x))) TOKEN_PURPOSES)) )
      (enforce (= (length p) 1) "sanity check: purpose not found or multiple purposes found")
      (* MAX_SUPPLY (at 'percentage-cap (at 0 p)))
    )
  )

  (defun get-mint-cap () (at 'maximum (read mint-cap-table MINT_CAP_KEY)))

  (defun update-mint-cap (new-cap:decimal)
    (enforce (>= new-cap 0.0) "new cap must be nonnegative")
    (with-capability (GOVERNANCE)
      (write mint-cap-table MINT_CAP_KEY { 'maximum: new-cap }))
  )

  (defun update-supply (purpose:string delta:decimal account:string action:string)
    (enforce-valid-purpose purpose)
    (enforce (or (= action 'burn) (= action 'mint)) (format "Invalid supply action {}" [action]))
    (validate-account account)
    (enforce-unit delta)
    (if (= action 'burn)
        (enforce-privilege BURN_PRIVILEGE)
        (require-capability (CREDIT account)))
    (let ((supply (read supply-table purpose)))
      (if (= action 'burn)
          (update supply-table purpose { 'total-burned: (+ delta (at 'total-burned supply)) })
          (let
            ((minted (total-minted))
              (available (get-mint-cap)))
            (enforce (< (+ minted delta) available)
              (format "Already minted {}, minting {} exceeds cap {}"
                [ minted delta available ]))
            (update supply-table purpose { 'total-minted: (+ delta (at 'total-minted supply)) }))
      )
    )
  )

  (defun get-supply (purpose:string)
    (enforce-valid-purpose purpose)
    (let ((supply (read supply-table purpose)))
      (- (at 'total-minted supply) (at 'total-burned supply)))
  )

  (defun get-raw-supply (purpose:string)
    (enforce-valid-purpose purpose)
    (read supply-table purpose)
  )

  (defun total-minted:decimal ()
    (fold (+) 0.0 (map (lambda (purpose) (at 'total-minted (read supply-table purpose))) (get-purpose-list)))
  )

  (defun total-supply:decimal ()
    (fold (+) 0.0 (map (get-supply) (get-purpose-list)))
  )

  (defun enforce-privilege (privilege:string)
    (enforce-guard (at 'guard (read privileges privilege ['guard])))
  )

  (defun assign-privilege
    ( privilege:string
      guard:guard
    )
    (require-capability (GOVERNANCE))
    (write privileges privilege { 'guard: guard })
  )

  (defun enforce-unit:bool (amount:decimal)
    @doc "Enforce minimum precision allowed for KTOSHI transactions"

    (enforce
      (= (floor amount MINIMUM_PRECISION)
         amount)
      (format "Amount violates minimum precision: {}" [amount]))
    )

  (defun validate-account (account:string)
    @doc "Enforce that an account name conforms to the KTOSHI contract \
         \minimum and maximum length requirements, as well as the    \
         \latin-1 character set."

    (enforce
      (is-charset COIN_CHARSET account)
      (format
        "Account does not conform to the KTOSHI contract charset: {}"
        [account]))

    (let ((account-length (length account)))

      (enforce
        (>= account-length MINIMUM_ACCOUNT_LENGTH)
        (format
          "Account name does not conform to the min length requirement: {}"
          [account]))

      (enforce
        (<= account-length MAXIMUM_ACCOUNT_LENGTH)
        (format
          "Account name does not conform to the max length requirement: {}"
          [account]))
      )
  )

  (defun enforce-valid-chain (chain-id:string)
    @doc "Enforce that the target chain id is valid."
    (enforce (!= "" chain-id) "Empty chain ID")
    (enforce (!= (at 'chain-id (chain-data)) chain-id) "Cannot run cross-chain transfers to the same chain")
    (enforce (contains chain-id VALID_CHAINS) (format "Chain ID {} is invalid or unknown" [chain-id]))
  )

  (defun create-account:string (account:string guard:guard)
    @model [ (property (valid-account account)) ]

    (enforce-contract-unlocked)
    (validate-account account)
    (enforce-reserved account guard)

    (insert token-table account
      { "balance" : 0.0
      , "guard"   : guard
      })
    )

  (defun get-balance:decimal (account:string)
    (with-read token-table account
      { "balance" := balance }
      balance
      )
    )

  (defun details:object{fungible-v2.account-details}
    ( account:string )
    (with-read token-table account
      { "balance" := bal
      , "guard" := g }
      { "account" : account
      , "balance" : bal
      , "guard": g })
    )

  (defun rotate:string (account:string new-guard:guard)
    (with-capability (ROTATE account)
      (with-read token-table account
        { "guard" := old-guard }

        (enforce-guard old-guard)

        (update token-table account
          { "guard" : new-guard }
          )))
    )

  (defun precision:integer
    ()
    MINIMUM_PRECISION)

  (defun burn:decimal (purpose:string account:string amount:decimal)
    (validate-account account)
    (enforce-unit amount)

    (enforce-privilege BURN_PRIVILEGE)

    (with-capability (BURN account amount)
      (debit account amount)
      (update-supply purpose amount account 'burn)
      amount
    )
  )

  (defun mint:decimal (purpose:string account:string guard:guard amount:decimal)
    (validate-account account)
    (enforce-unit amount)

    (enforce-privilege MINT_PRIVILEGE)

    (with-capability (MINT account amount)
      (credit account guard amount)
      (update-supply purpose amount account 'mint)
      amount
    )
  )

  (defun transfer:string (sender:string receiver:string amount:decimal)
    @model [ (property conserves-mass)
             (property (> amount 0.0))
             (property (valid-account sender))
             (property (valid-account receiver))
             (property (!= sender receiver)) ]

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")

    (validate-account sender)
    (validate-account receiver)

    (enforce (> amount 0.0)
      "transfer amount must be positive")

    (enforce-unit amount)

    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (with-read token-table receiver
        { "guard" := g }

        (credit receiver g amount))
      )
    )

  (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal )

    @model [ (property conserves-mass) ]

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")

    (validate-account sender)
    (validate-account receiver)

    (enforce (> amount 0.0)
      "transfer amount must be positive")

    (enforce-unit amount)

    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (credit receiver receiver-guard amount))
    )

  (defun debit:string (account:string amount:decimal)
    @doc "Debit AMOUNT from ACCOUNT balance"

    @model [ (property (> amount 0.0))
             (property (valid-account account))
           ]

    (validate-account account)

    (enforce (> amount 0.0)
      "debit amount must be positive")

    (enforce-unit amount)

    (require-capability (DEBIT account))
    (with-read token-table account
      { "balance" := balance }

      (enforce (<= amount balance) "Insufficient funds")

      (update token-table account
        { "balance" : (- balance amount) }
        ))
    )


  (defun credit:string (account:string guard:guard amount:decimal)
    @doc "Credit AMOUNT to ACCOUNT balance"

    @model [ (property (> amount 0.0))
             (property (valid-account account))
           ]

    (validate-account account)

    (enforce (> amount 0.0) "credit amount must be positive")
    (enforce-unit amount)

    (require-capability (CREDIT account))
    (with-default-read token-table account
      { "balance" : -1.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      ; we don't want to overwrite an existing guard with the user-supplied one
      (enforce (= retg guard)
        "account guards do not match")

      (let ((is-new
             (if (= balance -1.0)
                 (enforce-reserved account guard)
                 false)))

        (write token-table account
          { "balance" : (if is-new amount (+ balance amount))
          , "guard"   : retg
          }))
      ))

  (defun check-reserved:string (account:string)
    " Checks ACCOUNT for reserved name and returns type if \
    \ found or empty string. Reserved names start with a \
    \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
    (let ((pfx (take 2 account)))
      (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

  (defun enforce-reserved:bool (account:string guard:guard)
    @doc "Enforce reserved account name protocols."
    (if (validate-principal guard account)
      true
      (let ((r (check-reserved account)))
        (if (= r "")
          true
          (if (= r "k")
            (enforce false "Single-key account protocol violation")
            (enforce false
              (format "Reserved protocol guard violation: {}" [r])))))))

  (defschema crosschain-schema
    @doc "Schema for yielded value in cross-chain transfers"
    receiver:string
    receiver-guard:guard
    amount:decimal
    source-chain:string)

  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )

    @model [ (property (> amount 0.0))
             (property (valid-account sender))
             (property (valid-account receiver))
           ]

    (step
      (with-capability (TRANSFER_XCHAIN sender receiver amount target-chain)

        (validate-account sender)
        (validate-account receiver)

        (enforce-valid-chain target-chain)

        (enforce (> amount 0.0)
          "transfer quantity must be positive")

        (enforce-unit amount)

        ;; step 1 - debit delete-account on current chain
        (debit sender amount)

        (emit-event (TRANSFER sender "" amount))

        (let
          ((crosschain-details:object{crosschain-schema}
            { "receiver": receiver
            , "receiver-guard": receiver-guard
            , "amount": amount
            , "source-chain": (at 'chain-id (chain-data))}))
          (yield crosschain-details target-chain)
        )))

    (step
      (resume
        { "receiver" := receiver
        , "receiver-guard" := receiver-guard
        , "amount" := amount
        , "source-chain" := source-chain
        }
        (emit-event (TRANSFER "" receiver amount))
        (emit-event (TRANSFER_XCHAIN_RECD "" receiver amount source-chain))

        ;; step 2 - credit create account on target chain
        (with-capability (CREDIT receiver)
          (credit receiver receiver-guard amount))
        ))
    )

  (defun init ()
    (insert contract-lock CONTRACT_LOCK_KEY {'lock: false})
    (with-capability (GOVERNANCE)
      (insert token-table ISSUANCE_ACCOUNT
        { 'balance: 0.0
        , 'guard: (read-keyset "ks") })
      (update-mint-cap 21000000000000.0)
      (let
        ((create-supply-table (lambda (purpose)
                                 (insert supply-table purpose
                                         { 'total-minted: 0.0, 'total-burned: 0.0 })))
        )
        (map (create-supply-table) (get-purpose-list))
      )
      (assign-privilege BURN_PRIVILEGE (read-keyset "ks"))
      (assign-privilege MINT_PRIVILEGE (read-keyset "ks"))
    )
  )
)


