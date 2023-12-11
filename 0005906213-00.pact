(module time-lock-test GOVERNANCE

  (use coin)
  (use util.guards)

  (defschema lockup-option
    length:decimal
    return:decimal
  )

  (deftable lockup-options:{lockup-option})

  (defschema lockup
    account:string
    reservation-guard:guard
    payout-account:string
    payout-guard:guard
    request-time:time
    lockup-length:string
    lockup-start-time:time
    lockup-end-time:time
    kdx-locked:decimal
    kda-returned:decimal
    kda-total-return:decimal
    status:string)

  (deftable lockups:{lockup})

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'kdx-lockup-testing-keyset)))

  (defcap OPS ()
    (enforce-guard
      (keyset-ref-guard 'kdx-lockup-testing-keyset)))

  (defcap LOCK
    ( reservation-account:string
      lockup-length:decimal)
    "Lockup KDX rights event"
    @event
    (enforce-guard (before-date END_TIME))
    (enforce-guard (at-after-date START_TIME))
  )

  (defconst KDX_BANK:string 'kdx-lockup-reward-bank)

  (defconst STATUS_REQUESTED:string 'requested)

  (defconst STATUS_APPROVED:string 'approved)

  (defconst STATUS_REJECTED:string 'rejected)

  (defconst LOCKUP_INTERVAL_LENGTH:decimal (days 1))

  (defconst START_TIME:time (time "2021-12-15T00:00:00Z"))

  (defconst END_TIME:time (time "2021-12-31T00:00:00Z"))

  (defun kdx-bank-guard:guard () (create-module-guard "kdx-test-admin"))

  (defun init ()
    (dummy-token.create-account KDX_BANK (kdx-bank-guard))
  )

  (defun lock:string
    ( reservation-account:string
      payout-account:string
      payout-guard:guard
      lockup-time:string)
    (let*
      ( (lockup-length (read lockup-options lockup-time))
        (lockup-seconds (at 'length lockup-length))
        (lockup-return-fraction (at 'return lockup-length))
        (original-reservations (filter
          (compose (at 'status) (= kdx.priv-sale.STATUS_APPROVED))
          (kdx.priv-sale.read-reservations reservation-account)))
        (kda-paid (fold (+) 0.0 (map (at 'amount-kda) original-reservations)))
        (kdx-allocated (fold (+) 0.0 (map (at 'amount-kdx) original-reservations)))
        (kda-return (floor (* kda-paid lockup-return-fraction) dummy-token.MINIMUM_PRECISION))
        (reservation-guard (at 'guard (coin.details reservation-account)))
        (current-time (at 'block-time (chain-data)))
      )
      (enforce-guard reservation-guard)
      (enforce (> kdx-allocated 0.0) "You must have a nonnegative KDX reservation.")
      (with-capability (LOCK reservation-account lockup-seconds)
        (insert lockups reservation-account
          { 'account: reservation-account
          , 'reservation-guard: reservation-guard
          , 'payout-account: payout-account
          , 'payout-guard: payout-guard
          , 'request-time: (at 'block-time (chain-data))
          , 'lockup-length: lockup-time
          , 'lockup-start-time: current-time
          , 'lockup-end-time: (add-time current-time lockup-seconds)
          , 'status: STATUS_REQUESTED
          , 'kdx-locked: kdx-allocated
          , 'kda-returned: 0.0
          , 'kda-total-return: kda-return
          })
        (format "{} requested lockup of {} KDX, with {} fake-KDA returned to {} over {}."
          [reservation-account, kdx-allocated, kda-return, payout-account, lockup-time])
      )
    )
  )

  (defun claim-whole-return:decimal (reservation-account:string)
    (claim-return reservation-account (available-return reservation-account))
  )

  (defun claim-return:decimal
    ( reservation-account:string
      amount-kda:decimal )
    (let*
      ( (maximum-payout (available-return reservation-account))
        (lockup-record (read lockups reservation-account))
      )
      (enforce (<= amount-kda maximum-payout) "Insufficient fake-KDA return.")
      (bind lockup-record
        { 'kda-returned := already-returned
        , 'reservation-guard := reservation-guard
        , 'payout-account := payout-account
        , 'payout-guard := payout-guard }
        (enforce-guard reservation-guard)
        (install-capability (dummy-token.TRANSFER KDX_BANK payout-account amount-kda))
        (dummy-token.transfer-create KDX_BANK payout-account payout-guard amount-kda)
        (update lockups reservation-account
          { 'kda-returned: (+ already-returned amount-kda) })
        amount-kda
      )
    )
  )

  (defun available-return:decimal (reservation-account:string)
    (let
      ( (return-progress (calculate-return
          reservation-account (at 'block-time (chain-data))))
        (withdrawn (at 'kda-returned (read lockups reservation-account)))
      )
      (- return-progress withdrawn)
    )
  )

  (defun calculate-return:decimal
    ( reservation-account:string
      when:time)
    (with-read lockups reservation-account
      { 'status := status
      , 'kda-total-return := kda-total-return
      , 'lockup-start-time := lockup-start
      , 'lockup-end-time := lockup-end }
      (if (!= status STATUS_APPROVED) 0.0
      (if (< when lockup-start) 0.0
        (if (> when lockup-end) kda-total-return
          (let*
            ( (lockup-length:decimal (diff-time lockup-end lockup-start))
              (lockup-progress-seconds:decimal (diff-time when lockup-start))
              (lockup-intervals-total:decimal (/ lockup-length LOCKUP_INTERVAL_LENGTH))
              (lockup-intervals-passed:decimal (floor (/ lockup-progress-seconds LOCKUP_INTERVAL_LENGTH) 0))
              (lockup-progress-fraction:decimal (/ lockup-intervals-passed lockup-intervals-total))
            )
            (floor (* lockup-progress-fraction kda-total-return) (dummy-token.precision))
          )
        )
      ))
    )
  )

  (defun is-kdx-locked:bool (reservation-account:string)
    (with-default-read lockups reservation-account
      { 'status: STATUS_REJECTED
      , 'lockup-end-time: (parse-time "%s" "0") }
      { 'status := status
      , 'lockup-end-time := lockup-end-time }
      (if (!= status STATUS_APPROVED)
        false
        (< (at 'block-time (chain-data)) lockup-end-time))
    )
  )

  (defun approve:string (lockup-id:string)
    (with-capability (OPS)
      (with-read lockups lockup-id
        { "status"     := status }
        (enforce (= status STATUS_REQUESTED) "request is not open")
        (update lockups lockup-id
          { "status" : STATUS_APPROVED })
        (format "request {} approved" [lockup-id])
      )
    )
  )

  (defun reject:string (lockup-id:string)
    (with-capability (OPS)
      (with-read lockups lockup-id
        { "status"     := status }
        (enforce (= status STATUS_REQUESTED) "request is not open")
        (update lockups lockup-id
          { "status" : STATUS_REJECTED })
        (format "request {} rejected" [lockup-id])
      )
    )
  )


  (defun approve-helper:string (lockup-id:string)
    (require-capability (OPS))
    (with-read lockups lockup-id
      { "status"     := status }
      (if (= status STATUS_REQUESTED)
        (update lockups lockup-id
          { "status" : STATUS_APPROVED })
        "skipping case"
      )
    )
  )

  (defun approve-all:[string] ()
    (with-capability (OPS)
      (map (approve-helper) (list-lockup-accounts))
    )
  )

  (defun read-lockup:object{lockup} (reservation-account:string)
    (read lockups reservation-account)
  )

  (defun read-all-lockups:[object{lockup}] ()
    (map (read lockups) (list-lockup-accounts))
  )

  (defun list-lockup-accounts:[string] ()
    (keys lockups)
  )

  (defun read-lockup-option:object{lockup-option} (name:string)
    (read lockup-options name)
  )

  (defun list-lockup-options:[string] ()
    (keys lockup-options)
  )

)


