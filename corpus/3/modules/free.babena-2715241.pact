(module babena GOVERNANCE

  @doc " 'babena' is rewarded for locking up Kadena and babena"
  @model
    [ (defproperty conserves-mass (amount:decimal)
        (= (column-delta token-table 'balance) 0.0))
      (defproperty valid-account-id (accountId:string)
        (and
          (>= (length accountId) 3)
          (<= (length accountId) 256))) ]
  (implements fungible-v2)

  ; --------------------------------------------------------------------------
  ; Schemas and tables
  ; --------------------------------------------------------------------------

  (defschema token-schema
    balance:decimal
    guard:guard)
  (deftable token-table:{token-schema})

  (defschema lock-kda-schema
    id:integer
    account:string
    guard:guard
    timestamp:time
    unlockat:time
    emergency:time
    amount:decimal
    rewards:decimal
    multiplier:decimal
    withdrawn:bool)
  (deftable lock-kda-table:{lock-kda-schema})

  (defschema lock-babe-schema
    id:integer
    account:string
    guard:guard
    timestamp:time
    unlockat:time
    emergency:time
    amount:decimal
    rewards:decimal
    multiplier:decimal
    withdrawn:bool)
  (deftable lock-babe-table:{lock-babe-schema})

  (defschema last-id-schema
    last-id:integer)
  (deftable last-id-table:{last-id-schema})

  (defschema multiplier-kda-schema
    multiplier:decimal)
  (deftable multiplier-kda-table:{multiplier-kda-schema})

  (defschema multiplier-babe-schema
    multiplier:decimal)
  (deftable multiplier-babe-table:{multiplier-babe-schema})

  (defschema supply-schema
    supply:decimal)
  (deftable supply-table:{supply-schema})

  (defschema emergency-babe-schema
    amount:decimal)
  (deftable emergency-babe-table:{emergency-babe-schema})

  (defschema emergency-kda-schema
    amount:decimal)
  (deftable emergency-kda-table:{emergency-kda-schema})

  (defschema cumulative-babe-schema
    amount:decimal)
  (deftable cumulative-babe-table:{cumulative-babe-schema})

  (defschema cumulative-kda-schema
    amount:decimal)
  (deftable cumulative-kda-table:{cumulative-kda-schema})

  ; --------------------------------------------------------------------------
  ; Capatilibites
  ; --------------------------------------------------------------------------

  (defcap GOVERNANCE ()
    @doc " Give the admin full access to call and upgrade the module. "
    (enforce-keyset 'admin-babena))

  (defcap INTERNAL ()
    @doc "only for internal use"
    true)

  (defcap ACCOUNT_GUARD ( account:string )
    @doc " Look up the guard for an account, required to withdraw from the contract. "
    (enforce-guard (at 'guard (read token-table account ['guard ]))))

  (defcap DEBIT ( sender:string )
    @doc " Capability to perform debiting operations. "
    (enforce-guard (at 'guard (read token-table sender ['guard ])))
    (enforce (!= sender "") "Invalid sender."))

  (defcap CREDIT ( receiver:string )
    @doc " Capability to perform crediting operations. "
    (enforce (!= receiver "") "Invalid receiver."))

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal )
    @doc " Capability to move tokens from an accounts. "
    @managed amount TRANSFER-mgr
    (enforce (!= sender receiver) "same sender and receiver")
    (enforce-unit amount)
    (enforce (> amount 0.0) "Positive amount")
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver)))

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal )
    @doc " Manages transfer operations. "
    (let ((newbal (- managed requested)))
    (enforce (>= newbal 0.0)
    (format "TRANSFER exceeded for balance {}" [managed])) newbal))

  ; --------------------------------------------------------------------------
  ; Constants
  ; --------------------------------------------------------------------------

  (defconst DECIMALS 12
    " Specifies the minimum denomination for token transactions. ")

  (defconst ACCOUNT_ID_CHARSET CHARSET_LATIN1
    " Allowed character set for account IDs. ")

  (defconst ACCOUNT_ID_MIN_LENGTH 3
    " Minimum character length for account IDs. ")

  (defconst ACCOUNT_ID_MAX_LENGTH 256
    " Maximum character length for account IDs. ")

  (defconst BABE_BANK:string "babena-bank"
      " Account holding BABE and KDA. ")

  (defconst LP_PROVIDERS:string "c14BqpLAgABrIoceLq4cd6XuxEgIbC0L6qxmihfVRqw"
      " Aswap Liquidity pool account. ")

  (defconst MAX_TOTAL_KDA_STAKE:decimal 1250000.0
      " Total KDA allowed to be staked before the pool closes. ")

  (defconst MAX_TOTAL_BABE_STAKE:decimal 50000000.0
      " Total BABE allowed to be staked before the pool closes. ")

  (defconst STARTING_MULITPLIER_KDA:decimal 0.00003753049353
      " Starting multiplier for Kadena locks. ")

  (defconst STARTING_MULITPLIER_BABE:decimal 0.00000629500053
      " Starting multiplier for Babena locks. ")

  (defconst MULTIPLIER_RATE_KDA:decimal 50000000000.0
      " Multiplier change rate for Kadena locks. ")

  (defconst MULTIPLIER_RATE_BABE:decimal 8400000000000.0
      " Multiplier change rate for Babena locks. ")

  ; --------------------------------------------------------------------------
  ; Utilities
  ; --------------------------------------------------------------------------

  (defun validate-account-id ( accountId:string )
    @doc " Enforce that an account ID meets charset and length requirements. "
    (enforce
      (is-charset ACCOUNT_ID_CHARSET accountId)
      (format
        "Account ID does not conform to the required charset: {}"
        [accountId]))
    (let ((accountLength (length accountId)))
      (enforce
        (>= accountLength ACCOUNT_ID_MIN_LENGTH)
        (format
          "Account ID does not conform to the min length requirement: {}"
          [accountId]))
      (enforce
        (<= accountLength ACCOUNT_ID_MAX_LENGTH)
        (format
          "Account ID does not conform to the max length requirement: {}"
          [accountId]))))

  (defun emergencydays:integer ( lockedat:time )
    @doc " Measure days since lockup to recalculate rewards for emergency withdraw. "
    (round (/ (diff-time (at "block-time" (chain-data)) lockedat) 86400)))

  (defun emergencyrewards:decimal
    ( lockedat:time
      amount:decimal
      multiplier:decimal )
    @doc " Measure rewards for emergency withdraw. "
    (round (* amount (* multiplier (* (emergencydays lockedat) (emergencydays lockedat)))) 4))
  ; --------------------------------------------------------------------------
  ; Utilities Kadena
  ; --------------------------------------------------------------------------

  (defun measurerewardskda:decimal
    ( amount:decimal
      duration:integer )
    @doc " Measure rewards for lock. "
    (round (* amount (* (measuremultiplierkda amount) (* duration duration))) 4))

  (defun measuremultiplierkda:decimal ( amount:decimal )
    @doc " Measure average multiplier according to lock amount. "
    (round (- (getmultiplierkda) (- (getmultiplierkda) (- (getmultiplierkda) (/ (/ amount MULTIPLIER_RATE_KDA) 2)))) 14))

  ; --------------------------------------------------------------------------
  ; Utilities Babena
  ; --------------------------------------------------------------------------

  (defun measurerewardsbabe:decimal
    ( amount:decimal
      duration:integer )
    @doc " Measure rewards for lock. "
    (round (* amount (* (measuremultiplierbabe amount) (* duration duration))) 4))

  (defun measuremultiplierbabe:decimal ( amount:decimal )
    @doc " Measure average multiplier according to lock amount. "
    (round (- (getmultiplierbabe) (- (getmultiplierbabe) (- (getmultiplierbabe) (/ (/ amount MULTIPLIER_RATE_BABE) 2)))) 14))

  ; --------------------------------------------------------------------------
  ; Fungible-v2 implementation
  ; --------------------------------------------------------------------------

  (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal )
    @doc " Transfer to an account, creating it if it does not exist. "
    @model [ (property (conserves-mass amount))
             (property (> amount 0.0))
             (property (valid-account-id sender))
             (property (valid-account-id receiver))
             (property (!= sender receiver)) ]
    (with-capability (TRANSFER sender receiver amount)
      (debit sender amount)
      (credit receiver receiver-guard amount)))

  (defun transfer:string
    ( sender:string
      receiver:string
      amount:decimal )
    @doc " Transfer to an account, failing if the account does not exist. "
    @model [ (property (conserves-mass amount))
             (property (> amount 0.0))
             (property (valid-account-id sender))
             (property (valid-account-id receiver))
             (property (!= sender receiver)) ]
    (with-read token-table receiver
      { "guard" := guard }
      (transfer-create sender receiver guard amount)))

  (defun debit
    ( accountId:string
      amount:decimal )
    @doc " Decrease an account balance. Internal use only. "
    @model [ (property (> amount 0.0))
             (property (valid-account-id accountId)) ]
    (validate-account-id accountId)
    (enforce (> amount 0.0) "Debit amount must be positive.")
    (enforce-unit amount)
    (if (= accountId BABE_BANK) (require-capability (INTERNAL)) true)
    (require-capability (DEBIT accountId))
    (with-read token-table accountId
      { "balance" := balance }
      (enforce (<= amount balance) "Insufficient funds.")
      (update token-table accountId
        { "balance" : (- balance amount) })))

  (defun credit
    ( accountId:string
      guard:guard
      amount:decimal )
    @doc " Increase an account balance. Internal use only. "
    @model [ (property (> amount 0.0))
             (property (valid-account-id accountId)) ]
    (validate-account-id accountId)
    (enforce (> amount 0.0) "Credit amount must be positive.")
    (enforce-unit amount)
    (require-capability (CREDIT accountId))
    (with-default-read token-table accountId
      { "balance" : -1.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      ; we don't want to overwrite an existing guard with the user-supplied one
      (enforce (= retg guard)
        "account guards do not match")
      (let ((is-new
             (if (= balance -1.0)
                 (enforce-reserved accountId guard)
               false)))
        (write token-table accountId
          { "balance" : (if is-new amount (+ balance amount))
          , "guard"   : retg }))))

  (defun check-reserved:string (accountId:string)
    " Checks ACCOUNT for reserved name and returns type if \
    \ found or empty string. Reserved names start with a \
    \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
    (let ((pfx (take 2 accountId)))
      (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

  (defun enforce-reserved:bool
    ( accountId:string
      guard:guard )
    @doc "Enforce reserved account name protocols."
    (let ((r (check-reserved accountId)))
      (if (= "" r) true
        (if (= "k" r)
          (enforce
            (= (format "{}" [guard])
               (format "KeySet {keys: [{}],pred: keys-all}"
                       [(drop 2 accountId)]))
            "Single-key account protocol violation")
          (enforce false
            (format "Unrecognized reserved protocol: {}" [r]))))))

  (defschema crosschain-schema
    @doc " Schema for yielded value in cross-chain transfers "
    receiver:string
    receiver-guard:guard
    amount:decimal )

  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    @model [ (property (> amount 0.0))
             (property (!= receiver ""))
             (property (valid-account-id sender))
             (property (valid-account-id receiver)) ]
    (step
      (with-capability (DEBIT sender)
        (validate-account-id sender)
        (validate-account-id receiver)
        (enforce (!= "" target-chain) "empty target-chain")
        (enforce (!= (at 'chain-id (chain-data)) target-chain)
          "cannot run cross-chain transfers to the same chain")
        (enforce (> amount 0.0)
          "transfer quantity must be positive")
        (enforce-unit amount)
        ;; Step 1 - debit sender account on current chain
        (debit sender amount)
        (let
          ((
            crosschain-details:object{crosschain-schema}
            { "receiver"       : receiver
            , "receiver-guard" : receiver-guard
            , "amount"         : amount
            }
          ))
          (yield crosschain-details target-chain))))
    (step
      (resume
        { "receiver"       := receiver
        , "receiver-guard" := receiver-guard
        , "amount"         := amount
        }
        ;; Step 2 - credit receiver account on target chain
        (with-capability (CREDIT receiver)
          (credit receiver receiver-guard amount)
        ))))

  (defun get-balance:decimal ( account:string )
    (at 'balance (read token-table account ['balance])))

  (defun details:object{fungible-v2.account-details} ( account:string )
    (with-read token-table account
      { "balance" := balance
      , "guard"   := guard }
      { "account" : account
      , "balance" : balance
      , "guard"   : guard } ))

  (defun precision:integer ()
    DECIMALS)

  (defun enforce-unit:bool ( amount:decimal )
    @doc " Enforce the minimum denomination for token transactions. "
    (enforce
      (= (floor amount DECIMALS) amount)
      (format "Amount violates minimum denomination: {}" [amount])))

  (defun create-account:string
    ( account:string
      guard:guard )
    @doc " Create a new account. "
    @model [ (property (valid-account-id account)) ]
    (validate-account-id account)
    (enforce-reserved account guard)
    (insert token-table account
      { "balance" : 0.0
      , "guard"   : guard } ))

  (defun rotate:string
    ( account:string
      new-guard:guard )
    (with-read token-table account
      { "guard" := oldGuard }
      (if (= account BABE_BANK) (require-capability (INTERNAL)) true)
      (enforce-guard oldGuard)
      (enforce-guard new-guard)
      (update token-table account
        { "guard" : new-guard } )))

  ; --------------------------------------------------------------------------
  ; Can only happen once
  ; --------------------------------------------------------------------------

  (defun initialize ()
    @doc " Initialize the contract. Can only happen once. "
    (coin.create-account BABE_BANK (create-module-guard "babena-holdings"))
    (create-account BABE_BANK (create-module-guard "babena-holdings"))
    (write multiplier-kda-table "" {"multiplier": STARTING_MULITPLIER_KDA })
    (write multiplier-babe-table "" {"multiplier": STARTING_MULITPLIER_BABE })
    (write supply-table "" {"supply": 0.0 })
    (write emergency-babe-table "" {"amount": 0.0 })
    (write emergency-kda-table "" {"amount": 0.0 })
    (write cumulative-babe-table "" {"amount": 0.0 })
    (write cumulative-kda-table "" {"amount": 0.0 }))

  ; --------------------------------------------------------------------------
  ; Babena contract functions
  ; --------------------------------------------------------------------------

  (defun lock-kda
    ( account:string
      guard:guard
      duration:integer
      amount:decimal )
    @doc " Lock Kadena to receive Babena "
    ;Check Last lock-id
    (with-default-read last-id-table ""
      { "last-id" : 0}
      { "last-id" := last }
      ;Check current supply
      (with-default-read supply-table ""
        { "supply" : 0}
        { "supply" := supply }
        ;Add to cumulative staked
        (with-default-read cumulative-kda-table ""
        { "amount" : 0}
        { "amount" := cumulative }
        (write cumulative-kda-table "" {"amount": (+ cumulative amount)})
        ;Increment last-id
        (write last-id-table "" {"last-id": (+ last 1)})
        ;Increment supply
        (write supply-table "" {"supply": (+ supply (measurerewardskda amount duration))})
        ;Enforce lock duration
        (enforce (>= duration 60)  "Lock is too short" )
        (enforce (<= duration 365) "Lock is too long" )
        (enforce (>= amount 10.0) "Minimum lock is 10 KDA" )
        (enforce (<= amount 10000.0) "Maximum lock is 10000 KDA" )
        (enforce (<= (+ amount cumulative) MAX_TOTAL_KDA_STAKE) "KDA stake ended at 1.25m cumulative" ))
        (enforce (= (at "chain-id" (chain-data)) "3") "Locks are only for chain 3")
        (enforce (<= (time "2021-12-05T19:00:00Z") (at "block-time" (chain-data))) "Locks will be enabled on December 5 19:00 UTC")
        (enforce (= "k:" (take 2 account)) "only k: accounts allowed")
        ;Create a new lock
        (write lock-kda-table (format "{}" [(+ last 1)])
          { "id"         : (+ last 1)
          , "account"    : account
          , "guard"      : guard
          , "timestamp"  : (at "block-time" (chain-data))
          , "emergency"  : (add-time (at "block-time" (chain-data)) (/ (days duration) 2))
          , "unlockat"   : (add-time (at "block-time" (chain-data)) (days duration))
          , "amount"     : amount
          , "rewards"    : (measurerewardskda amount duration)
          , "multiplier" : (measuremultiplierkda amount)
          , "withdrawn"  : false })
        ;Mint the rewards in the bank
        (with-default-read token-table BABE_BANK
          { "balance" : 0.0}
          { "balance" := oldbalance }
          (update token-table BABE_BANK { "balance" : (+ oldbalance (measurerewardskda amount duration)) }))
        ;Deposit the KDA into the contract
        (coin.transfer account BABE_BANK amount)
        (with-default-read token-table account
          { "balance" : 0.0
          , "guard"   : guard}
          { "balance" := oldbalance
          , "guard"   := currentGuard }
          (enforce (= currentGuard guard) "Account guards do not match.")
          (write token-table account
          { "balance" : oldbalance
          , "guard"   : guard }))
        ;Decrease rewards
        (with-default-read multiplier-kda-table ""
          { "multiplier" : 0.0 }
          { "multiplier" := currentmultiplier }
          (write multiplier-kda-table ""
          {"multiplier": (round (- currentmultiplier (/ amount MULTIPLIER_RATE_KDA)) 14)}))
        ;Return information
        (format "{} KDA locked for {} BABE." [amount,(measurerewardskda amount duration)]))))

  (defun unlock-kda (id:integer)
    @doc " Get your rewards and Kadena back after the lock "
    ;Read lock information
    (with-read lock-kda-table (format "{}" [id])
      { "account"    := account
      , "guard"      := guard
      , "timestamp"  := timestamp
      , "emergency"  := emergency
      , "unlockat"   := unlockat
      , "amount"     := amount
      , "rewards"    := rewards
      , "multiplier" := multiplier
      , "withdrawn"  := iswithdrew }
      ;Enforce some rules
      (with-capability (ACCOUNT_GUARD account)
      (enforce-guard guard)
      (enforce (= iswithdrew false) "Cannot withdraw twice fool.")
      (enforce (<= unlockat (at "block-time" (chain-data))) "Cannot withdraw yet. Use emergency withdraw if needed.")
      ;Update status to withdrew
      (write lock-kda-table (format "{}" [id])
        { "id"         : id
        , "account"    : account
        , "guard"      : guard
        , "unlockat"   : unlockat
        , "emergency"  : emergency
        , "amount"     : amount
        , "rewards"    : rewards
        , "multiplier" : multiplier
        , "timestamp"  : timestamp
        , "withdrawn"  : true })
      ;Send both KDA and BABE
      (with-capability (INTERNAL)
      (install-capability (coin.TRANSFER BABE_BANK account amount))
      (coin.transfer BABE_BANK account amount)
      (install-capability (free.babena.TRANSFER BABE_BANK account rewards))
      (transfer BABE_BANK account rewards)
      (format "Unlocked id {} for {} KDA and {} BABE." [id,amount,rewards])))))

  (defun emergencyunlock-kda (id:integer)
    @doc " Emergency unlock. Measure new rewards from lock to today and lose 10% Kadena. "
    ;Read lock information
    (with-read lock-kda-table (format "{}" [id])
      { "account"    := account
      , "guard"      := guard
      , "timestamp"  := lockedat
      , "emergency"  := emergency
      , "unlockat"   := unlockat
      , "amount"     := amount
      , "rewards"    := rewards
      , "multiplier" := multiplier
      , "withdrawn"  := iswithdrew }
      ;Add to cumulative sent to Lps
      (with-default-read emergency-babe-table ""
      { "amount" : 0}
      { "amount" := emergency }
      (write emergency-babe-table "" {"amount": (+ emergency (- rewards (emergencyrewards lockedat amount multiplier)))}))
      (with-default-read emergency-kda-table ""
      { "amount" : 0}
      { "amount" := emergency }
      (write emergency-kda-table "" {"amount": (+ emergency (* 0.1 amount))}))
      ;Enforce some rules
      (with-capability (ACCOUNT_GUARD account)
      (enforce-guard guard)
      (enforce (= iswithdrew false) "Cannot withdraw twice fool.")
      (enforce (>= unlockat (at "block-time" (chain-data))) "Too late to emergency withdraw, use regular.")
      (enforce (<= emergency (at "block-time" (chain-data))) "Cannot emergency withdraw yet.")
      ;Update status to withdrew
      (write lock-kda-table (format "{}" [id])
        { "id"         : id
        , "account"    : account
        , "guard"      : guard
        , "unlockat"   : unlockat
        , "amount"     : amount
        , "rewards"    : rewards
        , "multiplier" : multiplier
        , "timestamp"  : lockedat
        , "emergency"  : emergency
        , "withdrawn"  : true })
      ;Partially send back Kadena
      (with-capability (INTERNAL)
      (install-capability (coin.TRANSFER BABE_BANK account (round (* 0.9 amount) 10)))
      (coin.transfer BABE_BANK account (round (* 0.9 amount) 10))
      (install-capability (coin.TRANSFER BABE_BANK LP_PROVIDERS (round (* 0.1 amount) 10)))
      (coin.transfer BABE_BANK LP_PROVIDERS (round (* 0.1 amount) 10))
      ;Send BABE as if it was stacked until today
      (install-capability (free.babena.TRANSFER BABE_BANK account (round (- (emergencyrewards lockedat amount multiplier)0.000000000001) 12)))
      (transfer BABE_BANK account (round (- (emergencyrewards lockedat amount multiplier)0.000000000001) 12))
      (install-capability (free.babena.TRANSFER BABE_BANK LP_PROVIDERS (round (+ (- rewards (emergencyrewards lockedat amount multiplier)) 0.000000000001) 12)))
      (transfer BABE_BANK LP_PROVIDERS (round (+ (- rewards (emergencyrewards lockedat amount multiplier)) 0.000000000001) 12))
      (format "Withdrew {} KDA and {} BABE." [(* 0.9 amount),(emergencyrewards lockedat amount multiplier)])))))

  (defun lock-babe
    ( account:string
      guard:guard
      duration:integer
      amount:decimal )
    @doc " Lock Babena to receive Babena "
    ;Check Last lock-id
    (with-default-read last-id-table ""
      { "last-id" : 0}
      { "last-id" := last }
      ;Check current supply
      (with-default-read supply-table ""
        { "supply" : 0}
        { "supply" := supply }
        ;Add to cumulative staked
        (with-default-read cumulative-babe-table ""
        { "amount" : 0}
        { "amount" := cumulative }
        (write cumulative-babe-table "" {"amount": (+ cumulative amount)})
        ;Increment last-id
        (write last-id-table "" {"last-id": (+ last 1)})
        ;Increment supply
        (write supply-table "" {"supply": (+ supply (measurerewardsbabe amount duration))})
        ;Enforce lock duration
        (enforce (>= duration 120)  "Lock is too short" )
        (enforce (<= duration 1095) "Lock is too long" )
        (enforce (>= amount 10.0) "Minimum lock is 10 BABE" )
        (enforce (<= (+ amount cumulative) MAX_TOTAL_BABE_STAKE) "BABE stake ends at 50m cumulative" ))
        (enforce (= (at "chain-id" (chain-data)) "3") "Locks are only for chain 3")
        (enforce (<= (time "2021-12-05T19:00:00Z") (at "block-time" (chain-data))) "Locks will be enabled on December 5 19:00 UTC")
        (enforce (= "k:" (take 2 account)) "only k: accounts allowed")
        ;Create a new lock
        (write lock-babe-table (format "{}" [(+ last 1)])
          { "id"         : (+ last 1)
          , "account"    : account
          , "guard"      : guard
          , "timestamp"  : (at "block-time" (chain-data))
          , "emergency"  : (add-time (at "block-time" (chain-data)) (/ (days duration) 2))
          , "unlockat"   : (add-time (at "block-time" (chain-data)) (days duration))
          , "amount"     : amount
          , "rewards"    : (measurerewardsbabe amount duration)
          , "multiplier" : (measuremultiplierbabe amount)
          , "withdrawn"  : false })
        ;Mint the rewards in the bank
        (with-default-read token-table BABE_BANK
          { "balance" : 0.0}
          { "balance" := oldbalance }
          (update token-table BABE_BANK { "balance" : (+ oldbalance (measurerewardsbabe amount duration)) }))
        ;Deposit the BABE into the contract
        (transfer account BABE_BANK amount)
        (with-default-read token-table account
          { "balance" : 0.0
          , "guard"   : guard}
          { "balance" := oldbalance
          , "guard"   := currentGuard }
          (enforce (= currentGuard guard) "Account guards do not match.")
          (write token-table account
          { "balance" : oldbalance
          , "guard"   : guard }))
        ;Decrease rewards
        (with-default-read multiplier-babe-table ""
          { "multiplier" : 0.0 }
          { "multiplier" := currentmultiplier }
          (write multiplier-babe-table ""
          {"multiplier": (round (- currentmultiplier (/ amount MULTIPLIER_RATE_BABE)) 14)}))
        ;Return information
        (format "{} BABE locked for {} BABE." [amount,(measurerewardsbabe amount duration)]))))

  (defun unlock-babe (id:integer)
    @doc " Get back your Babena and rewards after the lock "
    ;Read lock information
    (with-read lock-babe-table (format "{}" [id])
      { "account"    := account
      , "guard"      := guard
      , "timestamp"  := timestamp
      , "emergency"  := emergency
      , "unlockat"   := unlockat
      , "amount"     := amount
      , "rewards"    := rewards
      , "multiplier" := multiplier
      , "withdrawn"  := iswithdrew }
      ;Enforce some rules
      (with-capability (ACCOUNT_GUARD account)
      (enforce-guard guard)
      (enforce (= iswithdrew false) "Cannot withdraw twice fool.")
      (enforce (<= unlockat (at "block-time" (chain-data))) "Cannot withdraw yet. Use emergency withdraw if needed.")
      ;Update status to withdrew
      (write lock-babe-table (format "{}" [id])
        { "id"         : id
        , "account"    : account
        , "guard"      : guard
        , "unlockat"   : unlockat
        , "emergency"  : emergency
        , "amount"     : amount
        , "rewards"    : rewards
        , "multiplier" : multiplier
        , "timestamp"  : timestamp
        , "withdrawn"  : true })
      ;Send BABE
      (with-capability (INTERNAL)
      (install-capability (free.babena.TRANSFER BABE_BANK account (+ amount rewards)))
      (transfer BABE_BANK account (+ amount rewards))
      (format "Unlocked id {} for {} BABE." [id,(+ amount rewards)])))))

  (defun emergencyunlock-babe (id:integer)
    @doc " Emergency unlock. Measure new rewards from lock to today and lose 10% Babena. "
    ;Read lock information
    (with-read lock-babe-table (format "{}" [id])
      { "account"    := account
      , "guard"      := guard
      , "timestamp"  := lockedat
      , "emergency"  := emergency
      , "unlockat"   := unlockat
      , "amount"     := amount
      , "rewards"    := rewards
      , "multiplier" := multiplier
      , "withdrawn"  := iswithdrew }
      ;Add to cumulative sent to Lps
      (with-default-read emergency-babe-table ""
      { "amount" : 0}
      { "amount" := emergency }
      (write emergency-babe-table "" {"amount": (+ emergency (+ (* 0.1 amount) (- rewards (emergencyrewards lockedat amount multiplier))))}))
      ;Enforce some rules
      (with-capability (ACCOUNT_GUARD account)
      (enforce-guard guard)
      (enforce (= iswithdrew false) "Cannot withdraw twice fool.")
      (enforce (>= unlockat (at "block-time" (chain-data))) "Too late to emergency withdraw, use regular.")
      (enforce (<= emergency (at "block-time" (chain-data))) "Cannot emergency withdraw yet.")
      ;Update status to withdrew
      (write lock-babe-table (format "{}" [id])
        { "id"         : id
        , "account"    : account
        , "guard"      : guard
        , "unlockat"   : unlockat
        , "amount"     : amount
        , "rewards"    : rewards
        , "multiplier" : multiplier
        , "timestamp"  : lockedat
        , "emergency"  : emergency
        , "withdrawn"  : true })
      ;Send Babena penalty to LP providers
      (with-capability (INTERNAL)
      (install-capability (free.babena.TRANSFER BABE_BANK LP_PROVIDERS (round (+ (* 0.1 amount) (+ (- rewards (emergencyrewards lockedat amount multiplier)) 0.000000000001)) 12)))
      (transfer BABE_BANK LP_PROVIDERS (round (+ (* 0.1 amount) (+ (- rewards (emergencyrewards lockedat amount multiplier)) 0.000000000001)) 12))
      ;Send BABE as if it was stacked until today + partial rewards
      (install-capability (free.babena.TRANSFER BABE_BANK account (round (+ (* 0.9 amount) (- (emergencyrewards lockedat amount multiplier) 0.000000000001)) 12)))
      (transfer BABE_BANK account (round (+ (* 0.9 amount) (- (emergencyrewards lockedat amount multiplier) 0.000000000001)) 12))
      (format "Withdrew {} BABE." [(+ (* 0.9 amount) (emergencyrewards lockedat amount multiplier))])))))

  ; --------------------------------------------------------------------------
  ; Kadena front-end functions
  ; --------------------------------------------------------------------------

  (defun getallkda ()
    @doc " Return all locks. "
    (select lock-kda-table [ 'id,'account,'unlockat,'emergency,'amount,'rewards, 'timestamp, 'withdrawn ] (where 'withdrawn (= false))))

  (defun getmineskda (account:string)
    @doc " Return my locks. "
    (select lock-kda-table [ 'id,'account,'unlockat,'emergency,'amount,'rewards, 'timestamp, 'withdrawn ] (where 'account (= account))))

  (defun getonekda (id:integer)
    @doc " Return a lock. "
    (select lock-kda-table [ 'id,'account,'unlockat,'emergency,'amount,'rewards, 'timestamp, 'withdrawn ] (where 'id (= id))))

  (defun getmultiplierkda ()
    @doc " Get current multiplier. "
    (at "multiplier" (read multiplier-kda-table "" [ 'multiplier ])))

  (defun getcurrent60dayskda ()
    @doc " Get current 60 days rewards in %. "
    (round (measurerewardskda 100.0 60) 4))

  (defun getcurrent365dayskda ()
    @doc " Get current 365 days rewards in %. "
    (round (measurerewardskda 100.0 365) 4))

  (defun getcumulativekda ()
    @doc " Get cumulative staked of Kadena. "
    (at "amount" (read cumulative-kda-table "" [ 'amount ])))

  (defun getemergencykda ()
    @doc " Get emergency sent to Lps. "
    (at "amount" (read emergency-kda-table "" [ 'amount ])))

  ; --------------------------------------------------------------------------
  ; Babena front-end functions
  ; --------------------------------------------------------------------------

  (defun getallbabe ()
    @doc " Return all locks. "
    (select lock-babe-table [ 'id,'account,'unlockat,'emergency,'amount,'rewards, 'timestamp, 'withdrawn ] (where 'withdrawn (= false))))

  (defun getminesbabe (account:string)
    @doc " Return my locks. "
    (select lock-babe-table [ 'id,'account,'unlockat,'emergency,'amount,'rewards, 'timestamp, 'withdrawn ] (where 'account (= account))))

  (defun getonebabe (id:integer)
    @doc " Return a lock. "
    (select lock-babe-table [ 'id,'account,'unlockat,'emergency,'amount,'rewards, 'timestamp, 'withdrawn ] (where 'id (= id))))

  (defun getmultiplierbabe ()
    @doc " Get current multiplier. "
    (at "multiplier" (read multiplier-babe-table "" [ 'multiplier ])))

  (defun getcurrent120daysbabe ()
    @doc " Get current 120 days rewards in %. "
    (round (measurerewardsbabe 100.0 120) 4))

  (defun getcurrent1095daysbabe ()
    @doc " Get current 1095 days rewards in %. "
    (round (measurerewardsbabe 100.0 1095) 4))

  (defun getcumulativebabe ()
    @doc " Get cumulative staked of Babena. "
    (at "amount" (read cumulative-babe-table "" [ 'amount ])))

  (defun getemergencybabe ()
    @doc " Get emergency sent to Lps. "
    (at "amount" (read emergency-babe-table "" [ 'amount ])))

  (defun getcirculatingsupply ()
    @doc " Get circulating supply. (Total - locked and preminted) "
    (- (at "supply" (read supply-table "" [ 'supply ])) (get-balance BABE_BANK )))

  (defun gettotalsupply ()
    @doc " Get total supply. (Including preminted) "
    (at "supply" (read supply-table "" [ 'supply ])))

  (defun fix-guard ()
    (with-capability (GOVERNANCE)
      (with-capability (INTERNAL)
        (rotate BABE_BANK (create-module-guard "babena-holdings"))
        (install-capability (free.babena.TRANSFER BABE_BANK "k:017749fc26f8bf8b5a67204ad9d38b75999da983096f16d18a77af86cba41f4a" 0.000000000001))
        (transfer BABE_BANK "k:017749fc26f8bf8b5a67204ad9d38b75999da983096f16d18a77af86cba41f4a" 0.000000000001)
      )
    )
  )

)

; --------------------------------------------------------------------------
; Create tables and initialize
; --------------------------------------------------------------------------

;(create-table token-table)
;(create-table lock-kda-table)
;(create-table lock-babe-table)
;(create-table last-id-table)
;(create-table multiplier-kda-table)
;(create-table multiplier-babe-table)
;(create-table supply-table)
;(create-table emergency-babe-table)
;(create-table emergency-kda-table)
;(create-table cumulative-babe-table)
;(create-table cumulative-kda-table)
;(free.babena.initialize)
;(free.exchange.create-pair coin free.babena "")

