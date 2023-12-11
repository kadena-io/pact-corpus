(module backalley-dev-da500a02-8b5a-4cc7-9b7f-13b6b826adbb GOVERNANCE

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema transactions-schema
    created:time
    kind:string
    account:string
    amount:decimal)

  (deftable transactions:{transactions-schema})


  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    (enforce false "Enforce non-upgradeability"))


  ; --------------------------------------------------------------------------
  ; Admin Constants

 
  (defconst KIND:string "Fixed"
    "The type of this contract")
 
  (defconst VERSION:string "0.0.1"
    "The version of this contract")

  (defconst POOL_ID:string "da500a02-8b5a-4cc7-9b7f-13b6b826adbb"
    "The Pool ID.")

  (defconst ESCROW_ACCOUNT:string "ba-b-da500a02-8b5a-4cc7-9b7f-13b6b826adbb"
    "Escrow account to hold tokens that will be exchanged.")

  (defconst FEE_ACCOUNT:string "ba-f"
    "Account which will receive the fee payment.")

  (defconst FEE:decimal 0.0
    "Fee to be charged on the raised amount.")


  ; --------------------------------------------------------------------------
  ; Owner Constants

  (defconst OWNER:string "k:8a8293c15a56ff0fafcf16d1b33a9f1636efa4d5ada9ce48ed4ef8efbf46edb8"
    "Account that initiated this pool")

  (defconst TOKEN_AMOUNT:decimal 1.000000000000
    "Amount of tokens for sale.")

  (defconst MIN_RAISE:decimal 0.001000000000
    "Mininimum raise amount necessary for a pool to be considered successful.")

  (defconst MAX_RAISE:decimal 1.000000000000
    "Maximum raise after which the pool will not accept more deposits.")

  (defconst MIN_ALLOCATION:decimal 0.000001000000
    "Minimum individual allocation.")

  (defconst MAX_ALLOCATION:decimal 1.000000000000
    "Maximum individual allocation.")

  (defconst START_DATE:time (time "2022-01-24T19:30:00Z")
    "The date that the sale starts.")

  (defconst END_DATE:time (time "2022-01-24T19:40:00Z")
    "The date that the sale ends.")

  (defconst DISTRIBUTION_DATE:time (time "2022-01-24T19:40:00Z")
    "The date after which tokens will start being available for redeem.")

  (defconst VESTING_SHARE:decimal 0.010000000000
    "Percentage of tokens available for redeem after each vesting period.")

  (defconst VESTING_PERIOD_IN_SECS:integer 2592000
    "Vesting period length.")


  ; --------------------------------------------------------------------------
  ; Contract

  (defun escrow-guard:guard () (create-module-guard "escrow-admin"))


  ; --------------------------------------------------------------------------
  ; Initialize

  (defcap INIT ()
    @event true)

  (defun init ()
    (emit-event (INIT))

    (free.ghi-v3.create-account ESCROW_ACCOUNT (escrow-guard))
    (coin.create-account ESCROW_ACCOUNT (escrow-guard))
  )


  ; --------------------------------------------------------------------------
  ; Fund

  (defcap FUND (account:string tokens-for-sale:decimal)
    @event true)

  (defun fund (account:string amount:decimal)
    @doc "Used to transfer the tokens that will be sold in the pool."

    (let ( (funds (get-funding-amount)) )
      (enforce (< (chain-time) START_DATE) "Cannot fund after start date")
      (enforce (> amount 0.0) "Amount must be positive")
      (enforce (<= (+ funds amount) TOKEN_AMOUNT) "Funding limit exceeded")
      (with-capability (FUND account amount)
        (free.ghi-v3.transfer account ESCROW_ACCOUNT amount)
        (insert transactions (tx-id account amount)
          { 'created: (chain-time)
          , 'kind: "fund"
          , 'account: account
          , 'amount: amount })
      )
    )
  )


  ; --------------------------------------------------------------------------
  ; Deposit

  (defcap DEPOSIT (account:string amount:decimal)
    @event true)

  (defun deposit (account:string amount:decimal)
    @doc "Used to reserve a portion of tokens that are being sold in the pool."

    (emit-event (DEPOSIT account amount))
    (let
      ( (funded (is-funded))
        (deposit-amount (get-deposit-amount-of account))
        (total-deposit-amount (get-deposit-amount))
      )

      (enforce funded "Pool is not funded")
      (enforce (> amount 0.0) "Amount must be positive")
      (enforce (is-open) "Sale is not open")
      (enforce (<= (+ total-deposit-amount amount) MAX_RAISE)
        "Pool above raise allowance"
      )
      (enforce (>= (+ deposit-amount amount) MIN_ALLOCATION)
        "Balance cannot be lower than mininimum allocation"
      )
      (enforce (<= (+ deposit-amount amount) MAX_ALLOCATION)
        "Balance cannot be greater than maximum allocation"
      )

      (coin.transfer account ESCROW_ACCOUNT amount)

      (insert transactions (tx-id account amount)
        { 'created: (chain-time)
        , 'kind: "deposit"
        , 'account: account
        , 'amount: amount })
    )
  )


  ; --------------------------------------------------------------------------
  ; Redeem Tokens

  (defcap REDEEM-TOKENS (from:string to:string amount:decimal)
    @managed (redeemable-tokens from amount))

  (defun redeemable-tokens:bool (account:string amount:decimal)
    @doc "Checks whether reserved tokens are available for redeem."

    (let
      ( (achieved-min-raise (has-achieved-min-raise))
        (redeemable-amount (get-redeemable-amount account))
      )
      (enforce (has-ended) "The sale has not ended")
      (enforce achieved-min-raise "The sale has not achieved the min raise")
      (enforce (>= (chain-time) DISTRIBUTION_DATE) "Before distribution")
      (enforce (> redeemable-amount 0.0) "No tokens to redeem")
      (enforce (> amount 0.0) "Amount must be positive")
      (enforce (<= amount redeemable-amount) "Redeem amount above limit")
    )
  )

  (defun redeem-tokens:string (from-acct:string to-acct:string amount:decimal)
    @doc "Used by the participant to redeem their reserved tokens."

    (with-capability (REDEEM-TOKENS from-acct to-acct amount)
      (let
        ( (redeemable-amount (get-redeemable-amount from-acct))
          (to-guard (at 'guard (coin.details from-acct)))
        )
        (enforce-keyset to-guard)
        (install-capability (free.ghi-v3.TRANSFER ESCROW_ACCOUNT to-acct amount))
        (free.ghi-v3.transfer-create ESCROW_ACCOUNT to-acct to-guard amount)

        (insert transactions (tx-id from-acct amount)
          { 'created: (chain-time)
          , 'kind: "redeem-tokens"
          , 'account: from-acct
          , 'amount: amount })

        (format "Redeemed {} tokens" [amount])
      )
    )
  )


  ; --------------------------------------------------------------------------
  ; Redeem Funds

  (defcap REDEEM-FUNDS (from:string to:string amount:decimal)
    @managed (redeemable-funds amount))

  (defun redeemable-funds:bool (amount:decimal)
    @doc "Checks whether raised funds are redeemable."

    (let
      ( (balance (get-deposit-amount))
        (achieved-min-raise (has-achieved-min-raise))
        (redeemable-funds (get-redeemable-funds))
        (redeemed-funds (has-redeemed-funds))
      )
      (enforce (has-ended) "The sale has not ended")
      (enforce achieved-min-raise "The sale has not achieved the min raise")
      (enforce (> balance 0.0) "No tokens to redeem")
      (enforce (> redeemable-funds 0.0) "No redeemable funds")
      (enforce (> amount 0.0) "Amount must be positive")
      (enforce (<= amount redeemable-funds) "Redeem amount above limit")
    )
  )

  (defun redeem-funds:string (to-acct:string amount:decimal)
    @doc "Used by the pool owner to redeem the deposited tokens."

    (let ( (to-guard (at 'guard (coin.details OWNER))) )
      (with-capability (REDEEM-FUNDS OWNER to-acct amount)
        (enforce-guard to-guard)
        (let ( (payout (* amount (- 1.0 FEE))) )
          (install-capability (coin.TRANSFER ESCROW_ACCOUNT to-acct payout))
          (coin.transfer-create ESCROW_ACCOUNT to-acct to-guard payout)

          (insert transactions (tx-id OWNER payout)
            { 'created: (chain-time)
            , 'kind: "redeem-funds"
            , 'account: OWNER
            , 'amount: payout })

          (format "Redeemed {} tokens" [payout])
        )
      )
    )
  )


  ; --------------------------------------------------------------------------
  ; Redeem Fee

  (defcap REDEEM-FEE (account:string)
    @event true)

  (defun redeem-fee:string ()
    @doc "Used by the admin to redeem the fees."

    (let*
      (
        (deposit-amount (get-deposit-amount))
        (achieved-min-raise (has-achieved-min-raise))
        (fee (* deposit-amount FEE))
        (redeemed (has-redeemed-fee))
      )
      (enforce (has-ended) "The sale has not ended")
      (enforce achieved-min-raise "The sale has not achieved the min raise")
      (enforce (not redeemed) "Funds have been redeemed")
      (enforce (> fee 0.0) "No tokens to redeem")

      (with-capability (REDEEM-FEE FEE_ACCOUNT)
        (install-capability (coin.TRANSFER ESCROW_ACCOUNT FEE_ACCOUNT fee))
        (coin.transfer ESCROW_ACCOUNT FEE_ACCOUNT fee)

        (insert transactions (tx-id FEE_ACCOUNT fee)
          { 'created: (chain-time)
          , 'kind: "redeem-fee"
          , 'account: FEE_ACCOUNT
          , 'amount: fee })

        (format "Redeemed {} tokens" [fee])
      )
    )
  )


  ; --------------------------------------------------------------------------
  ; Withdraw Tokens

  (defcap WITHDRAW-TOKENS (account:string)
    @event (withdrawable-tokens))

  (defun withdrawable-tokens:bool ()
    @doc "Checks whether funded tokens are withdrawable."

    (let
      ( (withdrawn-tokens (has-withdrawn-tokens))
        (unsold-tokens (get-unsold-tokens)) )
      (enforce (has-ended) "The sale has not ended")
      (enforce (not withdrawn-tokens) "Funds have been withdrawn")
      (enforce (> unsold-tokens 0.0) "No tokens to withdraw.")
    )
  )

  (defun withdraw-tokens:string ()
    @doc "Used by the pool owner to withdraw unsold tokens."

    (with-capability (WITHDRAW-TOKENS OWNER)
      (let ( (unsold-tokens (get-unsold-tokens)) )
        (install-capability (free.ghi-v3.TRANSFER ESCROW_ACCOUNT OWNER unsold-tokens))
        (free.ghi-v3.transfer ESCROW_ACCOUNT OWNER unsold-tokens)

        (insert transactions (tx-id OWNER unsold-tokens)
          { 'created: (chain-time)
          , 'kind: "withdraw-tokens"
          , 'account: OWNER
          , 'amount: unsold-tokens })

        (format "Withdrew {} tokens" [unsold-tokens])
      )
    )
  )


  ; --------------------------------------------------------------------------
  ; Withdraw Funds

  (defcap WITHDRAW-FUNDS (account:string)
    @event (withdrawable-funds account))

  (defun withdrawable-funds:bool (account:string)
    @doc "Checks whether account funds are withdrawable."

    (let
      ( (achieved-min-raise (has-achieved-min-raise))
        (withdrawn (has-withdrawn-funds account))
        (deposit-amount (get-deposit-amount-of account))
      )
      (enforce (has-ended) "The sale has not ended")
      (enforce (not achieved-min-raise) "The sale has achieved min raise")
      (enforce (not withdrawn) "Funds have been withdrawn")
      (enforce (> deposit-amount 0.0) "No funds to withdraw")
    )
  )

  (defun withdraw-funds:string (account:string)
    @doc "Used by the participants to withdraw their deposits."

    (with-capability (WITHDRAW-FUNDS account)
      (let ( (amount (get-deposit-amount-of account)) )
        (install-capability (coin.TRANSFER ESCROW_ACCOUNT account amount))
        (coin.transfer ESCROW_ACCOUNT account amount)

        (insert transactions (tx-id account amount)
          { 'created: (chain-time)
          , 'kind: "withdraw-funds"
          , 'account: account
          , 'amount: amount })

        (format "Withdrew {} tokens" [amount])
      )
    )
  )


  ; --------------------------------------------------------------------------
  ; Utilities

  (defun get-transactions:[object{transactions-schema}] ()
    (select transactions (constantly true))
  )

  (defun get-deposit-amount ()
    (fold (+) 0.0 (map (at 'amount)
      (select transactions (where 'kind (= "deposit")))))
  )

  (defun get-deposit-amount-of:decimal (account:string)
    (fold (+) 0.0 (map (at 'amount)
      (filter (where 'kind (= "deposit"))
        (select transactions (where 'account (= account))))
    ))
  )

  (defun get-redeemed-funds:decimal ()
    (fold (+) 0.0 (map (at 'amount)
      (select transactions (where 'kind (= "redeem-funds")))))
  )

  (defun get-redeemed-amount:decimal ()
    (fold (+) 0.0 (map (at 'amount)
      (select transactions (where 'kind (= "redeem-tokens")))))
  )

  (defun get-redeemed-amount-of:decimal (account:string)
    (fold (+) 0.0 (map (at 'amount)
      (filter (where 'kind (= "redeem-tokens"))
        (select transactions (where 'account (= account))))
    ))
  )

  (defun get-funding-amount:decimal ()
    (fold (+) 0.0 (map (at 'amount)
      (select transactions (where 'kind (= "fund")))))
  )

  (defun get-participants:[string] ()
    (distinct (map (at 'account)
      (select transactions (where 'kind (= "deposit"))))
    )
  )

  (defun has-redeemed-fee:bool ()
    (< 0 (length (select transactions (where 'kind (= "redeem-fee")))))
  )

  (defun has-redeemed-tokens:bool (account:string)
    (< 0 (length (filter (where 'kind (= "redeem-tokens"))
      (select transactions (where 'account (= account))))))
  )

  (defun has-redeemed-funds:bool ()
    (< 0 (length (select transactions (where 'kind (= "redeem-funds")))))
  )

  (defun has-withdrawn-tokens:bool ()
    (< 0 (length (select transactions (where 'kind (= "withdraw-tokens")))))
  )

  (defun has-withdrawn-funds:bool (account:string)
    (< 0 (length (filter (where 'kind (= "withdraw-funds"))
      (select transactions (where 'account (= account))))))
  )

  (defun get-withdrawn-amount:decimal ()
    (fold (+) 0.0 (map (at 'amount)
      (select transactions (where 'kind (= "withdraw-funds")))))
  )

  (defun get-redeemable-funds:decimal ()
    (- (get-deposit-amount) (get-redeemed-funds))
  )

  (defun is-funded:bool ()
    (= (get-funding-amount) TOKEN_AMOUNT)
  )

  (defun has-achieved-min-raise:bool ()
    (>= (get-deposit-amount) MIN_RAISE))

  (defun has-started:bool ()
    (>= (chain-time) START_DATE))

  (defun has-ended:bool ()
    (>= (chain-time) END_DATE))

  (defun has-started-distribution:bool ()
    (>= (chain-time) DISTRIBUTION_DATE))

  (defun is-open:bool ()
    (and (has-started) (not (has-ended))))

  (defun get-unsold-tokens:decimal ()
    (if (has-achieved-min-raise)
      (- TOKEN_AMOUNT (* (get-ratio) (get-deposit-amount)))
      (- TOKEN_AMOUNT (get-funding-amount))
    )
  )

  (defun get-redeemable-amount:decimal (account:string)
    (let*
      ( (redeemed-amount (get-redeemed-amount-of account))
        (deposited (get-deposit-amount-of account))
        (reserved (* deposited (get-ratio)))
        (secs-since-distribution (diff-time (chain-time) DISTRIBUTION_DATE))
        (vested-periods
          (floor (+ 1 (/ secs-since-distribution VESTING_PERIOD_IN_SECS))))
        (vested-percent (max 0.0 (min 1.0 (* vested-periods VESTING_SHARE))))
        (vested-amount (* reserved vested-percent))
      )
      (- vested-amount redeemed-amount)
    )
  )

  (defun get-ratio:decimal ()
    (/ TOKEN_AMOUNT MAX_RAISE)
  )

  (defun chain-time:time ()
    (at 'block-time (chain-data)))

  (defun max:decimal (a:decimal b:decimal)
    (if (>= a b) a b)
  )

  (defun min:decimal (a:decimal b:decimal)
    (if (<= a b) a b)
  )

  (defun tx-id:string (account:string amount:decimal)
    (let
      ((h (hash { 'account: account, 'amount: amount, 'salt: (chain-time) })))
      (format "{}-{}" [account h])
    )
  )

  (defun get-pool:object ()
    { "kind": KIND
    , "version": VERSION
    , "pool-id": POOL_ID
    , "escrow-account": ESCROW_ACCOUNT
    , "fee": FEE

    , "owner": OWNER
    , "token-amount": TOKEN_AMOUNT
    , "min-raise": MIN_RAISE
    , "max-raise": MAX_RAISE
    , "min-allocation": MIN_ALLOCATION
    , "max-allocation": MAX_ALLOCATION
    , "start-date": START_DATE
    , "end-date": END_DATE
    , "distribution-date": DISTRIBUTION_DATE
    , "vesting-share": VESTING_SHARE
    , "vesting-period-secs": VESTING_PERIOD_IN_SECS

    , "deposit-amount": (get-deposit-amount)
    , "redeemed-funds": (get-redeemed-funds)
    , "redeemed-amount": (get-redeemed-amount)
    , "funding-amount": (get-funding-amount)
    , "num-participants": (length (get-participants))
    , "has-redeemed-fee": (has-redeemed-fee)
    , "has-redeemed-funds": (has-redeemed-funds)
    , "has-withdrawn-tokens": (has-withdrawn-tokens)
    , "withdrawn-amount": (get-withdrawn-amount)
    , "is-funded": (is-funded)
    , "has-achieved-min-raise": (has-achieved-min-raise)
    , "has-started": (has-started)
    , "has-ended": (has-ended)
    , "has-started-distribution": (has-started-distribution)
    , "is-open": (is-open)
    , "unsold-tokens-amount": (get-unsold-tokens)
    , "ratio": (get-ratio)
    , "reserved-amount": (* (get-ratio) (get-deposit-amount)) }
  )

  (defun get-data-of (account:string)
    { "redeemable-amount-of": (get-redeemable-amount account)
    , "has-withdrawn-funds": (has-withdrawn-funds account)
    , "has-redeemed-tokens": (has-redeemed-tokens account)
    , "redeemed-amount-of": (get-redeemed-amount-of account)
    , "deposit-amount-of": (get-deposit-amount-of account)
    }
  )

)


