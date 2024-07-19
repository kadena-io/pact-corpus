(module fsyc-pre-sale GOVERNANCE

  (use util.guards)

  (defschema reservation
    account:string
    guard:guard
    amount-kda:decimal
    amount-fsyc:integer)

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
      (keyset-ref-guard "free.fsyc-admin")))

  (defcap RESERVE
    ( account:string
      amount-kda:decimal
      amount-fsyc:integer)
    "Reserve event for fsyc reservation"
    @event
    (enforce-guard (before-date END_TIME))
    (enforce-one "Sale Not Started"
      [(enforce-guard (at-after-date SALE_START_TIME))
      (enforce-guard (at-after-date WHITELIST_TIME))])
      (let* ( (total-fsyc-reserved:decimal (get-total-fsyc-reserved))
              (fsyc-reserved:integer (get-fsyc-reserved account))
              (sale-price:decimal (* amount-fsyc (get-sale-price))))
           (enforce
             (= amount-kda sale-price)
             (format "Invalid KDA/FSYC amount {} KDA and {} FSYC {}" [sale-price amount-fsyc amount-kda]))
           (enforce
            (<= (+ amount-fsyc total-fsyc-reserved) FSYC_SALE_SUPPLY)
              (format "Reachecd maximum supply {} for private-sale" [FSYC_SALE_SUPPLY]))
           (enforce
            (<= (+ amount-fsyc fsyc-reserved) FSYC_PER_USER)
              (format "You can buy only {} tokens" [FSYC_PER_USER]))
      )
    )

  (defconst FSYC_BANK:string "k:ffa795e58cf729e065aa618fcc9451a9895a428d7c9894520860c5a63c0fc0f5")
  (defconst FSYC_PER_USER:integer 2500)
  (defconst FSYC_SALE_SUPPLY:decimal 2500.0)
  (defconst SALE_START_TIME:time (time "2023-09-01T06:00:00Z"))
  (defconst WHITELIST_TIME:time (time "2023-09-01T00:00:00Z"))
  (defconst END_TIME:time (time "2023-12-30T00:00:00Z"))
  (defconst SALE_STATUS:string 'sale-status)
  (defconst SALE_STARTED "sale-started")

  (defun init (accounts:[string])
    (with-capability (GOVERNANCE)
      (insert whitelists "" {
        'accounts: accounts
        })
      (insert sale-status SALE_STATUS {
        'status: SALE_STARTED,
        'price: 215.1
      })
    )
  )

  (defun enforce-whitelist (account:string)
    (let ( (accounts:[string] (at 'accounts (read whitelists ""))))
      (enforce (contains account accounts) "You are not whitelisted")
      (enforce-guard (at-after-date WHITELIST_TIME))
      (enforce-guard (at-before-date SALE_START_TIME))
    )
  )

  (defun reserve:string (account:string amount-kda:decimal amount-fsyc:integer)
    (enforce (<= 0 amount-fsyc) "amount-fsyc must atleast be 1")
    (with-capability (RESERVE account amount-kda amount-fsyc)
      (let
        ( (g (at 'guard (coin.details account)))
          (kda-amount:decimal (+ amount-kda (get-amount-kda account)))
          (fsyc-amount:integer (+ amount-fsyc (get-fsyc-reserved account))))
        (enforce (validate-principal g account) "Only k: accounts are supported")
        (if (< (diff-time (at 'block-time (chain-data)) SALE_START_TIME) 0.0) (enforce-whitelist account) "Pre-sale for whitelists ended")
        (coin.transfer account FSYC_BANK amount-kda)
        (write reservations account
          { "account"    : account
          , "amount-kda" : kda-amount
          , "amount-fsyc" : fsyc-amount
          , "guard"      : g
          })
        (format "{} reserved FSYC with {} KDA" [account, amount-kda])
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

  (defun get-total-fsyc-reserved:decimal ()
    (fold (+) 0.0 (map (get-fsyc-reserved) (get-accounts)))
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

  (defun get-fsyc-reserved:integer (account:string)
    (with-default-read reservations account
      { 'amount-fsyc: 0 }
      { 'amount-fsyc:= amount }
      amount
    )
  )

  (defun get-start-time:time ()
    SALE_START_TIME
  )

  (defun get-whitelist-time:time ()
    WHITELIST_TIME
  )

  (defun get-end-time:time ()
    END_TIME
  )

  (defun get-sale-supply:decimal ()
    FSYC_SALE_SUPPLY
  )

  (defun get-fsyc-per-user:decimal ()
    FSYC_PER_USER
  )

)


