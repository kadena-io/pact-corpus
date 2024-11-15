(module unitt-presale GOVERNANCE
  @doc "Time-locked swapping contract for UNITT presale."

  (use coin)
  (use unitt [ transfer get-balance create-account transfer-create ])
  (use util.guards)

  ;; Constants -----------------------------------------------------------------
  (defconst ADMIN_KEYSET:string "n_ebe54249b2e9d68f5060961f3c419f8288d18dc2.presale-admin-keyset")
  (defconst PRESALE_PROCEEDS_ACCOUNT:string "k:ef46137232f1cb9acebf717f94a5a9083e96db16957da79ed0d1df29b0bb4ff8") ; all KDA from presale goes here
  (defconst PRESALE_REWARDS_ACCOUNT:string "ef46137232f1cb9acebf717f94a5a9083e96db16957da79ed0d1df29b0bb4ff8") ; all UTT rewards to users come from here

  (defconst UTT_USD_ROW_KEY 'uttusd) ; key for UTT/USD price row
  (defconst KDA_USD_ROW_KEY 'kdausd) ; key for KDA/USD price row

  (defconst REWARD_RATIO:decimal (/ 1.0 180.0)) ; reward 1/180 of total earned UTT per day
  (defconst TOTAL_UTT_AVAILABLE:decimal 160000000.0) ; total UTT tokens available for swapping
  (defconst MIN_SWAP_KDA:decimal 1.0) ; minumum KDA for a single transaction
  (defconst MAX_SWAP_KDA:decimal 1000000.0) ; maximum KDA for a single transaction
  (defconst PRECISION 12) ; for rounding

  (defconst PRESALE_START:time (time "2024-05-21T15:00:00Z")) ; presale starts on May 21th, 2024
  (defconst PRESALE_END:time (add-time PRESALE_START (days 31))) ; presale lasts 4 weeks

  (defconst REWARD_START_DAYS:integer 150) ; wait n days after the first swap, then start giving out rewards

  ;; Schemas  ------------------------------------------------------------------
  (defschema swaps
    account:string
    totalSwappedKDA:decimal
    totalEarnedUTT:decimal
    totalClaimedUTT:decimal
    swapTime:time
    guard:guard)

  (defschema swapping-stats
    totalSwappedKDA:decimal
    totalEarnedUTT:decimal
    totalClaimedUTT:decimal)

  (defschema prices
    price:decimal)

  (defschema price-history
    oldPrice:decimal
    newPrice:decimal
    timestamp:time)

  (defschema swapping-halted halted:bool )

  ;; Tables  -------------------------------------------------------------------
  (deftable swaps-table:{swaps})
  (deftable swapping-halted-table:{swapping-halted})
  (deftable swapping-stats-table:{swapping-stats})
  (deftable price-table:{prices})
  (deftable kda-price-history-table:{price-history})
  (deftable utt-price-history-table:{price-history})

  ;; Capabilities  -------------------------------------------------------------
  (defcap GOVERNANCE ()
    @doc "Give the admin full access to call and upgrade the module."
    (enforce-keyset ADMIN_KEYSET)
  )

  (defcap ACCOUNT_GUARD (account:string)
    @doc "Verifies that the account belongs to caller"
    (enforce-guard (at "guard" (coin.details account)))
  )

  (defcap SWAP (amount:decimal)
    @doc "Allow to swap KDA."
    ; check if presale is halted
    (with-read swapping-halted-table "" { "halted":= halted }
      (enforce (not halted) "Presale is temporarily halted.")
    )

    ; check that we're within the presale period
    (enforce-guard (at-after-date PRESALE_START))
    (enforce-guard (before-date PRESALE_END))

    ; check swap amount within bounds
    (enforce (>= amount MIN_SWAP_KDA) "Amount must at least min amount.")
    (enforce (<= amount MAX_SWAP_KDA) "Amount can't be greater than the max amount.")

    ; check there's enough tokens left to swap
    (with-default-read swapping-stats-table ""
      { "totalEarnedUTT": 0.0 } { "totalEarnedUTT":= totalEarnedUTT }
      (enforce (<= (+ amount totalEarnedUTT) TOTAL_UTT_AVAILABLE) "Not enough presale tokens left.")
    )
  )

  (defcap WITHDRAW (account:string amountUTT:decimal)
  	@doc "Allow to withdraw (claim) UTT rewards."
  	; allow admin to automatically claim rewards for the user
    (with-read swaps-table account { "guard" := guard }
      (enforce-one (format "Access denied to account {}" [account])
        [(enforce-keyset ADMIN_KEYSET)
         (enforce-keyset guard)]
      )
    )
    ; check allowed amount
    (let ((maxWithdrawal (account-available account)))
      (enforce (<= amountUTT maxWithdrawal) "Withdrawal amount exceeds the maximum allowed.")
    )
    (compose-capability (BANK_DEBIT))
  )

  ; capability predicate function
  (defun require-BANK_DEBIT ()
    (require-capability (BANK_DEBIT))
  )

  ; guard constructor
  (defun create-BANK_DEBIT-guard ()
    (create-user-guard (require-BANK_DEBIT))
  )

  (defcap BANK_DEBIT () true)

  ;; Admin functions  ----------------------------------------------------------
  (defun initialize ()
    @doc "Initialize the module."
    (with-capability (GOVERNANCE)
      ; set initial values
      (insert swapping-halted-table "" { "halted": true } )
      (insert swapping-stats-table "" { "totalSwappedKDA": 0.0, "totalEarnedUTT": 0.0, "totalClaimedUTT": 0.0 })

      ; these will be updated by the admin after deployment
      (insert price-table UTT_USD_ROW_KEY { "price": 0.00125 })
      (insert price-table KDA_USD_ROW_KEY { "price": 0.9 })

			; create the account for presale rewards protected by a user guard
      (unitt.create-account PRESALE_REWARDS_ACCOUNT (create-BANK_DEBIT-guard))
    )
  )

  (defun get-swaps ()
    @doc "Returns all swaps"
    (map (read swaps-table) (keys swaps-table))
  )

  (defun get-swapping-halted:bool ()
    @doc "Get the halted state"
    (at 'halted (read swapping-halted-table ""))
  )

  (defun set-swapping-halted (halted:bool)
    @doc "Allow admin to disable swapping."
    (with-capability (GOVERNANCE)
      (write swapping-halted-table "" { "halted": halted })
    )
  )

  (defun get-price-kda:decimal ()
    @doc "Get the current KDA/USD price"
    (at 'price (read price-table KDA_USD_ROW_KEY))
  )

  (defun set-price-kda:string (price:decimal)
    @doc "Update KDA/USD price"
    (enforce (< 0.0 price) "Price must be greater than zero.")
    (with-capability (GOVERNANCE)
      (with-read price-table KDA_USD_ROW_KEY { "price":= oldPrice }
        (let ((tx-id (hash {"price": price, "oldPrice": oldPrice, "salt": (get-current-time)})))
          (update price-table KDA_USD_ROW_KEY {"price": price})
          (insert kda-price-history-table tx-id {
              "oldPrice": oldPrice,
              "newPrice": price,
              "timestamp": (get-current-time)
            })
          (format "KDA/USD price updated: old price {} | new price {}" [oldPrice, price])
        )
      )
    )
  )

  (defun get-price-history-kda ()
    @doc "Returns all KDA/USD prices"
    (map (read kda-price-history-table) (keys kda-price-history-table))
  )

  (defun get-price-utt:decimal ()
    @doc "Get the current UTT/USD price"
    (at 'price (read price-table UTT_USD_ROW_KEY))
  )

  (defun set-price-utt:string (price:decimal)
    @doc "Update UTT/USD price"
    (enforce (< 0.0 price) "Price must be greater than zero.")
    (with-capability (GOVERNANCE)
      (with-read price-table UTT_USD_ROW_KEY { "price":= oldPrice }
        (let ((tx-id (hash {"price": price, "oldPrice": oldPrice, "salt": (get-current-time)})))
          (update price-table UTT_USD_ROW_KEY {"price": price})
          (insert utt-price-history-table tx-id {
              "oldPrice": oldPrice,
              "newPrice": price,
              "timestamp": (get-current-time)
            })
          (format "UTT/USD price updated: old price {} | new price {}" [oldPrice, price])
        )
      )
    )
  )

  (defun get-price-history-utt ()
    @doc "Returns all UTT/USD prices"
    (map (read utt-price-history-table) (keys utt-price-history-table))
  )

  (defun calculate-utt-amount:decimal (amountKda:decimal)
    @doc "Calculate the amount of UTT for a given amount of KDA."
    (/ (* amountKda (get-price-kda)) (get-price-utt))
  )

  ;; Public functions  ---------------------------------------------------------
  (defun swap-kda (account:string amountKDA:decimal guard:guard)
    @doc "Lock KDA to presale proceeds account"
    (enforce (> amountKDA 0.0) "Amount must be greater than zero.")
    (with-capability (ACCOUNT_GUARD account)
      (with-capability (SWAP amountKDA)
        (install-capability (coin.TRANSFER account PRESALE_PROCEEDS_ACCOUNT amountKDA))
        ; fetch current swapping and reward values for the user
        (with-default-read swaps-table account
          { "totalSwappedKDA": 0.0,
            "totalEarnedUTT": 0.0,
            "totalClaimedUTT": 0.0,
            "swapTime": 0 }
          { "totalSwappedKDA":= totalSwappedKDA,
            "totalEarnedUTT":= totalEarnedUTT,
            "totalClaimedUTT":= totalClaimedUTT,
            "swapTime":= swapTime }
          ; transfer the swapped amount of KDA from the user to the proceeds account
          (coin.transfer account PRESALE_PROCEEDS_ACCOUNT amountKDA)
          ; calculate amount of earned UTT with current prices
          (let ((earnedUTT (calculate-utt-amount amountKDA)))
            ; update the swap account, setting swapTime only if it's not already set
            (write swaps-table account { "account": account,
                                          "totalSwappedKDA": (+ totalSwappedKDA amountKDA),
                                          "totalEarnedUTT": (+ totalEarnedUTT earnedUTT),
                                          "totalClaimedUTT": totalClaimedUTT,
                                          "swapTime": (if (= swapTime 0) (get-current-time) swapTime),
                                          "guard": guard
                                        })
            ; increase the total amount of presale swaps
            (with-default-read swapping-stats-table ""
              { "totalSwappedKDA": 0.0, "totalEarnedUTT": 0.0 }
              { "totalSwappedKDA":= totalSwappedKDA, "totalEarnedUTT":= totalEarnedUTT }
              (update swapping-stats-table "" {
                "totalSwappedKDA": (+ totalSwappedKDA amountKDA),
                "totalEarnedUTT": (+ totalEarnedUTT earnedUTT)
              })
            )
          )
        )
      )
    )
  )

	; claim earned UTT from the rewards account to the user's corresponding UTT account
  (defun claim-utt (account:string amountUTT:decimal)
    (enforce (> amountUTT 0.0) "Amount must be greater than zero.")
    (with-capability (WITHDRAW account amountUTT)
      (with-read swaps-table account
        { "totalSwappedKDA":= totalSwappedKDA,
          "totalEarnedUTT":= totalEarnedUTT,
          "totalClaimedUTT":= totalClaimedUTT,
          "account":= swapAccount,
          "swapTime":= swapTime,
          "guard":= guard }
        ; transfer UTT to user
        (install-capability (unitt.TRANSFER PRESALE_REWARDS_ACCOUNT account amountUTT))
        (unitt.transfer-create PRESALE_REWARDS_ACCOUNT account guard amountUTT)
        ; update amount rewarded for user
        (write swaps-table account {
          "account": account,
          "totalSwappedKDA": totalSwappedKDA,
          "totalEarnedUTT": totalEarnedUTT,
          "totalClaimedUTT": (+ totalClaimedUTT amountUTT),
          "swapTime": swapTime,
          "guard": guard })
        ; update the total rewards
        (with-default-read swapping-stats-table ""
          { "totalClaimedUTT": 0.0 } { "totalClaimedUTT":= totalClaimedUTT }
          (update swapping-stats-table "" { "totalClaimedUTT": (+ totalClaimedUTT amountUTT) })
        )
      )
    )
  )

  (defun account-swapped:decimal (account:string)
    @doc "Returns the total swapped KDA for given account."
    (with-default-read swaps-table account
      { "totalSwappedKDA": 0.0 } { "totalSwappedKDA":= totalSwappedKDA }
      totalSwappedKDA
    )
  )

  (defun account-earned:decimal (account:string)
    @doc "Returns the total earned UTT for given account."
    (with-default-read swaps-table account
      { "totalEarnedUTT": 0.0 } { "totalEarnedUTT":= totalEarnedUTT }
      totalEarnedUTT
    )
  )

  (defun account-claimed:decimal (account:string)
    @doc "Returns the total claimed UTT for given account."
    (with-default-read swaps-table account
      { "totalClaimedUTT": 0.0 } { "totalClaimedUTT":= totalClaimedUTT }
      totalClaimedUTT
    )
  )

  (defun account-available:decimal (account:string)
    @doc "Check how much UTT is currently available for withdrawal for the user."
    (with-default-read swaps-table account
      { "totalEarnedUTT": 0.0, "totalClaimedUTT": 0.0, "swapTime": 0}
      { "totalEarnedUTT":= totalEarnedUTT, "totalClaimedUTT":= totalClaimedUTT, "swapTime":= swapTime }
      (let ((maxUTT (account-current-rewards account)))
        (let ((availableUTT (- maxUTT totalClaimedUTT)))
          (if (< availableUTT 0.0) 0.0 availableUTT)
        )
      )
    )
  )

  (defun account-current-rewards:decimal (account:string)
    @doc "Returns the total accumulated UTT for given account since start of reward period."
    (with-default-read swaps-table account
      { "totalEarnedUTT": 0.0, "swapTime": (get-current-time)}
			{ "totalEarnedUTT":= totalEarnedUTT, "swapTime":= swapTime }
      (let ((daysSince (days-since swapTime)))
				(if (< daysSince REWARD_START_DAYS) 0.0
					(let ((totalRewards (* (* totalEarnedUTT REWARD_RATIO) daysSince)))
						(if (< totalRewards 0.0) 0.0 totalRewards)
					)
				)
			)
    )
  )

  (defun account-stats:object (account:string)
    @doc "Returns account details."
    (with-read swaps-table account
      { "totalSwappedKDA":= totalSwappedKDA,
        "totalEarnedUTT":= totalEarnedUTT,
        "totalClaimedUTT":= totalClaimedUTT }
      { "totalSwappedKDA": totalSwappedKDA,
        "totalEarnedUTT": totalEarnedUTT,
        "totalClaimedUTT": totalClaimedUTT,
        "availableUTT": (account-available account) }
    )
  )

  ;; Utilities  ----------------------------------------------------------------
  (defun get-current-time:time ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data))
  )

  (defun days-since:integer (t:time)
    @doc "Returns the number of days since the given time"
      (floor (/ (diff-time (get-current-time) t) 86400))
  )
)


