(module babena-pre-sale GOVERNANCE

  (use util.guards)

  (defschema reservation
    account:string
    guard:guard
    amount-kda:decimal
    amount-babena:integer)

  (defschema whitelist-schema
    accounts:[string]
  )

  (defschema sale
    status:string
    price:decimal)

  (deftable reservations:{reservation})
  (deftable whitelists:{whitelist-schema})
  (deftable sale-status:{sale})

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard 'babena-keyset)))

  (defcap OPS ()
    (enforce-guard
      (keyset-ref-guard 'babena-ops)))

  (defcap RESERVE
    ( account:string
      amount-kda:decimal
      amount-babena:integer)
    "Reserve event for babena reservation"
    @event
    (enforce-guard (before-date END_TIME))
    (enforce-one "Sale Not Started"
      [(enforce-guard (at-after-date SALE_START_TIME))
      (enforce-guard (at-after-date WHITELIST_TIME))])
      (let* ( (total-babena-reserved:decimal (get-total-babena-reserved))
              (babena-reserved:integer (get-babena-reserved account))
              (sale-price:decimal (* amount-babena (get-sale-price))))
           (enforce
             (= amount-kda sale-price)
             (format "Invalid KDA/Babena amount {} KDA and {} Babena {}" [sale-price amount-babena amount-kda]))
           (enforce
            (<= (+ amount-babena total-babena-reserved) BABENA_SALE_SUPPLY)
              (format "Reachecd maximum supply {} for private-sale" [BABENA_SALE_SUPPLY]))
           (enforce
            (<= (+ amount-babena babena-reserved) BABENA_PER_USER)
              (format "You can buy only {} tokens" [BABENA_PER_USER]))
      )
    )

  (defconst BABENA_BANK:string "k:d4b836c6b7af8c135b15cc7ce2a6152bc0393f4b32344801fddd273b14d4bc09")
  (defconst BABENA_PER_USER:integer 5)
  (defconst BABENA_SALE_SUPPLY:decimal 1000.0)
  (defconst SALE_START_TIME:time (time "2022-03-25T16:00:00Z"))
  (defconst WHITELIST_TIME:time (time "2022-03-24T16:00:00Z"))
  (defconst END_TIME:time (time "2022-04-02T16:30:00Z"))
  (defconst SALE_STATUS:string 'sale-status)
  (defconst SALE_STARTED "sale-started")

  (defun init (accounts:[string])
    (with-capability (GOVERNANCE)
      (insert whitelists "" {
        'accounts: accounts
        })
      (insert sale-status SALE_STATUS {
        'status: SALE_STARTED,
        'price: 0.0
      })
    )
  )

  (defun enforce-whitelist (account)
    (let ( (accounts:[string] (at 'accounts (read whitelists ""))))
      (enforce (contains account accounts) "You are not whitelisted")
      (enforce-guard (at-after-date WHITELIST_TIME))
      (enforce-guard (at-before-date SALE_START_TIME))
    )
  )

  (defun reserve:string (account:string amount-kda:decimal amount-babena:integer)
    (enforce (<= 0 amount-babena) "amount-babena must atleast be 1")
    (with-capability (RESERVE account amount-kda amount-babena)
      (if (< (diff-time (at 'block-time (chain-data)) SALE_START_TIME) 0.0) (enforce-whitelist account) "Pre-sale for whitelists ended")
      (coin.transfer account BABENA_BANK amount-kda)
      (let
        ( (g (at 'guard (coin.details account)))
          (kda-amount:decimal (+ amount-kda (get-amount-kda account)))
          (babena-amount:integer (+ amount-babena (get-babena-reserved account))))
        (write reservations account
          { "account"    : account
          , "amount-kda" : kda-amount
          , "amount-babena" : babena-amount
          , "guard"      : g
          })
        (format "{} reserved BABENA with {} KDA" [account, amount-kda])
      )
    )
  )

  (defun update-sale-price:string (price:decimal)
    @doc   "Update sale price - Simplified oracle to handle on-chain reservation"
    (enforce (< 0.0 price) "price is not a positive number")
      (with-capability (GOVERNANCE)
        (with-read sale-status SALE_STATUS {
          "price":=oldPrice}
            (update sale-status SALE_STATUS {"price":price})
              (format "Kda/Usd sale price updated: old price {} | new price {}" [oldPrice, price])
        )
      )
  )

  (defun get-sale-price:decimal ()
    (at 'price (read sale-status SALE_STATUS))
  )

  (defun read-reservation (account:string)
    (read reservations account)
  )

  (defun get-accounts ()
    (keys reservations)
  )

  (defun get-total-babena-reserved:decimal ()
    (fold (+) 0.0 (map (get-babena-reserved) (get-accounts)))
  )

  (defun get-total-kda-reserved:decimal ()
    (fold (+) 0.0 (map (get-amount-kda) (get-accounts)))
  )

  (defun get-amount-kda:decimal (account:string)
    (with-default-read reservations account
      { 'amount-kda: 0.0 }
      { 'amount-kda:= amount }
      amount
    )
  )

  (defun get-babena-reserved:integer (account:string)
    (with-default-read reservations account
      { 'amount-babena: 0 }
      { 'amount-babena:= amount }
      amount
    )
  )

)


