(module kdswap-token-locker GOVERNANCE
  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  ;define locks schema
  (defschema locks
    lockerAccount:string
    benificiaryAccount:string
    lockAmount:decimal
    interval:integer
    installments:integer
    token:module{fungible-v2}
    firstClaimDate:time
    claimedAmount:decimal
    status:integer
    id:string
  )

  ; define lock stats schema
  (defschema locks-stats
    totalLocked:decimal
  )

  ; define locks meta schema
  (defschema locks-meta
    halted:bool
    lockFee:decimal
    kdsFeeDiscount:decimal
    feeAccount:string
  )

  (deftable locks-table:{locks})
  (deftable locks-stats-table:{locks-stats})
  (deftable locks-meta-table:{locks-meta})

  ; --------------------------------------------------------------------------
  ; Constants

  ; Lock statusses
  (defconst CREATED 0)
  (defconst ACCEPTED 1)
  (defconst CLAIMED 2)
  (defconst CANCELLED 3)

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()

    @doc " Give the admin full access to call and upgrade the module. "

    (enforce-keyset 'kdlaunch-admin)
  )

  (defcap ACCT_GUARD (account)
    (enforce-guard (at 'guard (coin.details account)))
  )

  (defcap LOCKING_OPEN:bool ()
    (with-read locks-meta-table ""
      {
        "halted":= lockingHalted
      }

      (enforce (= lockingHalted false) "UNABLE TO LOCK/UNLOCK, LOCKING/UNLOCKING IS CURRENTLY HALTED")
    )
  )

  (defcap LOCKING_RESERVE
    (token-key:string)
    true)

  (defcap LOCK () true)

  (defcap UNLOCK () true)

  ; --------------------------------------------------------------------------
  ; Configuration

  (defun set-locking-fee (lockFee:decimal kdsFeeDiscount:decimal)
    @doc "Set the locking fee parameters"
    (with-capability (GOVERNANCE)
      (enforce (<= kdsFeeDiscount 1.0) "invalid discount percentage")

      (update locks-meta-table "" {
        "lockFee": lockFee,
        "kdsFeeDiscount": kdsFeeDiscount
      }))
  )

  (defun set-fee-account (feeAccount:string guard:guard)
    @doc "Set the account that will store the locking fees"
    (with-capability (GOVERNANCE)

      (let ((accGuard (at 'guard (coin.details feeAccount))))
        (format "{} {}" [accGuard guard])
        (enforce (= accGuard guard) "account guards do not match")
      )

      (update locks-meta-table "" {
        "feeAccount": feeAccount
      }))
  )

  (defun set-locking-haltstate (halted:bool)
    @doc "Disable or enable the ability to lock"
    (with-capability (GOVERNANCE)
      (update locks-meta-table "" {
        "halted": halted
      }))
  )

  ; --------------------------------------------------------------------------
  ; Utils

  (defun enforce-locking-reserve:bool
    (key:string)
    (require-capability (LOCKING_RESERVE key)))

  (defun create-locking-guard:guard
    (key:string)
    (create-user-guard (enforce-locking-reserve key)))

  (defun get-locking-account-principal (key: string)
    (create-principal (create-locking-guard key))
  )

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data)))

  (defun create-unique-id:string
    (account:string token:module{fungible-v2})
    (hash (format "{}{}{}" [account (at 'block-time (chain-data)) token]))
  )

  ; --------------------------------------------------------------------------
  ; Lock functions

  (defun get-kds-fee:decimal (lockFee:decimal kdsFeeDiscount:decimal)
    "Calculate kds price"
    (let*
      (
        (p (kdlaunch.kdswap-exchange.get-pair coin kdlaunch.kdswap-token))
        (reserveA (kdlaunch.kdswap-exchange.reserve-for p coin))
        (reserveB (kdlaunch.kdswap-exchange.reserve-for p kdlaunch.kdswap-token))
        (kdsAmount (* (/ reserveB reserveA) lockFee))
        (kdsFee (floor (* kdsAmount kdsFeeDiscount) 0))
      )
      kdsFee
    )
  )

  (defun raise-lock (lockAmount token)
    "Increase the total amount locked in the contract"
    (require-capability (LOCK))
    (with-default-read locks-stats-table token
      {
        "totalLocked" : 0.0
      }
      {
        "totalLocked":= totalLocked
      }
      (write locks-stats-table token {
        "totalLocked": (+ totalLocked lockAmount)
      }))
  )

  (defun reduce-lock (reduceAmount token)
    "Decrease the total amount locked in the contract"
    (require-capability (UNLOCK))
    (with-read locks-stats-table token {
      "totalLocked":= totalLocked
      }
      (update locks-stats-table token {
        "totalLocked": (- totalLocked reduceAmount)
        }))
  )

  (defun lock-tokens (
    lockerAccount:string
    benificiaryAccount:string
    amount:decimal
    interval:integer
    installments:integer
    token:module{fungible-v2}
    firstClaimDate:time
    kdsFee:bool)
    (with-capability (LOCKING_OPEN)
      (enforce (> amount 0.0) "Lock amount must be positive.")
      (enforce (= (take 2 benificiaryAccount) "k:") "Only k:accounts are supported")

      ; Transfer the amount of tokens to lock
      (token::transfer-create lockerAccount (get-locking-account-principal (format "{}" [token])) (create-locking-guard (format "{}" [token])) amount)

      ; Transfer the fees, in KDS or KDA
      (with-read locks-meta-table "" {
          "feeAccount":= feeAccount,
          "lockFee":= lockFee,
          "kdsFeeDiscount":= kdsFeeDiscount
        }

        (if (= kdsFee true)
          (kdlaunch.kdswap-token.transfer-create lockerAccount feeAccount (at 'guard (coin.details feeAccount)) (get-kds-fee lockFee kdsFeeDiscount))
        true)
        (if (!= kdsFee true)
          (coin.transfer-create lockerAccount feeAccount (at 'guard (coin.details feeAccount)) lockFee)
        true)
      )

      (let
        (
          (id (create-unique-id benificiaryAccount token))
        )

        (insert locks-table id
          {
            "lockerAccount"       : lockerAccount,
            "benificiaryAccount"  : benificiaryAccount,
            "lockAmount"          : amount,
            "interval"            : interval,
            "installments"        : installments,
            "token"               : token,
            "firstClaimDate"      : firstClaimDate,
            "claimedAmount"       : 0.0,
            "status"              : CREATED,
            "id"                  : id
          })))
  )

  (defun accept-lock (lockId:string)
    (with-capability (LOCK)
      (with-read locks-table lockId {
          "benificiaryAccount":= benificiaryAccount,
          "status":= status,
          "lockAmount":= lockAmount,
          "token":= token:module{fungible-v2}
        }

        (enforce (= status CREATED) "Cannot accept lock, lock already accepted or cancelled")

        (with-capability (ACCT_GUARD benificiaryAccount)
          ; Accept the lock
          (update locks-table lockId {
            "status": ACCEPTED
          })

          ; Update the locked amount
          (raise-lock lockAmount (format "{}" [token]))
        )))
  )

  (defun cancel-lock (lockId:string)
    (with-read locks-table lockId {
      "lockerAccount":= lockerAccount,
      "lockAmount":= lockAmount,
      "token":= token:module{fungible-v2},
      "status":= status
    }

    (enforce (= status CREATED) "Cannot cancel lock, lock already accepted or cancelled")

    (with-capability (ACCT_GUARD lockerAccount)
      ; Cancel the lock
      (update locks-table lockId {
        "status": CANCELLED
      })

      ; Return the funds
      (install-capability (token::TRANSFER (get-locking-account-principal (format "{}" [token])) lockerAccount lockAmount))
      (with-capability (LOCKING_RESERVE (format "{}" [token]))
        (token::transfer (get-locking-account-principal (format "{}" [token])) lockerAccount lockAmount)
      )
    ))
  )

  ; --------------------------------------------------------------------------
  ; Claim functions

  (defun claim-tokens (lockId:string desiredAmount:decimal)
    (with-capability (UNLOCK)
      (with-read locks-table lockId
        {
          "benificiaryAccount" := benificiaryAccount,
          "lockAmount" := lockAmount,
          "token":= token:module{fungible-v2},
          "claimedAmount":= claimedAmount,
          "interval":= interval,
          "installments":= installments,
          "firstClaimDate":= firstClaimDate
        }

        (with-capability (ACCT_GUARD benificiaryAccount)
          (let*
            (
              (claimableAmount (get-claimable-amount lockAmount claimedAmount interval installments firstClaimDate))
            )

            (enforce (> lockAmount 0.0) "Nothing locked")
            (enforce (> claimableAmount 0.0) "Nothing to claim")
            (enforce (<= desiredAmount claimableAmount) "Insufficient tokens available to claim")

            ; Transfer amount
            (install-capability (token::TRANSFER (get-locking-account-principal (format "{}" [token])) benificiaryAccount desiredAmount))
            (with-capability (LOCKING_RESERVE (format "{}" [token]))
              (token::transfer-create (get-locking-account-principal (format "{}" [token])) benificiaryAccount (at 'guard (coin.details benificiaryAccount)) desiredAmount)
            )

            (update locks-table lockId { "claimedAmount" : (+ claimedAmount desiredAmount) })

            (reduce-lock desiredAmount (format "{}" [token]))

            ; Update lock status if all is claimed
            (if (= (+ claimedAmount desiredAmount) lockAmount)
              (update locks-table lockId { "status" : CLAIMED })
            true)
        ))))
  )

  (defun get-account-locks (account:string)
    (select locks-table [
      'lockerAccount 'lockAmount 'interval 'installments 'token 'firstClaimDate 'id 'status 'claimedAmount 'benificiaryAccount
    ] (or? (where 'benificiaryAccount (= account)) (where 'lockerAccount (= account))))
  )

  (defun get-lock (lockId:string)
    (read locks-table lockId)
  )

  (defun get-claimable-amount:decimal (amount:decimal claimedAmount:decimal interval:integer installments:integer firstClaimDate:time)
    (let*
      (
        (daysPassed (floor (/ (diff-time (curr-time) firstClaimDate) 86400.0) 0))
        (claimableIntervals (+ (floor (/ daysPassed interval)) 1))
        (intervalAmount (/ amount installments))
        (isValid (< claimableIntervals installments))
        (validatedIntervals (if isValid claimableIntervals installments))
        (areIntervalsSameAsInstallments (= validatedIntervals installments))
        (unlockedAmount (if areIntervalsSameAsInstallments amount (floor(* validatedIntervals intervalAmount) 4)))
        (toClaim (- unlockedAmount claimedAmount))
      )
      (if (< toClaim 0.0) 0.0 toClaim)
    )
  )

  (defun get-meta-data ()
    (with-read locks-meta-table "" {
        "halted" := halted,
        "lockFee" := lockFee,
        "kdsFeeDiscount":= kdsFeeDiscount,
        "feeAccount":= feeAccount
      }
      (let*
        (
          (kdsFee (get-kds-fee lockFee kdsFeeDiscount))
        )
        { "kdsFee": kdsFee, "lockFee": lockFee, "halted": halted, "kdsFeeDiscount": kdsFeeDiscount, "feeAccount": feeAccount }
      )
    )
  )

  ; --------------------------------------------------------------------------
  ; Generic functions

  (defun get-locking-stats ()
    (let*
      (
        (allRows (keys locks-stats-table))
        (read-lock-stats (lambda (token-key)
          (with-read locks-stats-table token-key {
              "totalLocked":= totalLocked
            }
            { "totalLocked": totalLocked, "token": token-key }
          )))
      )
      (map (read-lock-stats) allRows)
    )
  )
)

; --------------------------------------------------------------------------
; Deployment


