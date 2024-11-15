(module fsyc-pre-sale GOVERNANCE

  (use util.guards)

  (defschema reservation
    account:string
    guard:guard
    amount-kda:decimal
    amount-fsyc:integer)

  (defschema revoked
    account:string
    refund-txn:string
  )

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

  (defcap MINT-ADMIN ()
    (enforce-guard
      (keyset-ref-guard "free.fsyc-mint-admin"))
    (compose-capability (PRIVATE)))

  (defcap PRICE-ADMIN ()
    (enforce-one "Only Admin or  Mint admin can update price"
        [(enforce-guard (keyset-ref-guard "free.fsyc-mint-admin"))
         (enforce-guard (keyset-ref-guard "free.fsyc-admin"))])
  )

  (defcap PRIVATE () true)

  (defcap REVOKED (account:string refund-txn:string) @event true)

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

  (defcap TRANSFER (sender:string receiver:string amount-fsyc:integer)
    @event
    true
  )

  (defconst FSYC_BANK:string "k:ffa795e58cf729e065aa618fcc9451a9895a428d7c9894520860c5a63c0fc0f5")
  (defconst FSYC_PER_USER:integer 2300)
  (defconst FSYC_SALE_SUPPLY:decimal 2500.0)
  (defconst SALE_START_TIME:time (time "2022-10-22T18:00:00Z"))
  (defconst WHITELIST_TIME:time (time "2022-10-22T12:00:00Z"))
  (defconst END_TIME:time (time "2024-02-22T00:00:00Z"))
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
      (with-capability (PRICE-ADMIN)
        (with-read sale-status SALE_STATUS {
          "price":=oldPrice}
            (update sale-status SALE_STATUS {"price":price})
              (format "Kda/Usd sale price updated: old price {} | new price {}" [oldPrice, price])
        )
      )
  )

  (defun update-whitelists (new-wls:[string])
    (with-capability (GOVERNANCE)
      (with-read whitelists ""
        {'accounts:= old-wls}
        (update whitelists ""
          {'accounts: (distinct (+ old-wls new-wls))}
        )
      )
    )
  )

  (defun revoke-reserves (revokes:[object{revoked}])
    (with-capability (GOVERNANCE)
      (with-capability (PRIVATE)
        (map (revoke-reserve) revokes)
      )
    )
  )

  (defun revoke-reserve (revoke:object{revoked})
    (require-capability (PRIVATE))
      (update reservations (at 'account revoke)
        {
          'amount-kda: 0.0,
          'amount-fsyc: 0
        }
      )
      (emit-event (REVOKED (at 'account revoke) (at 'refund-txn revoke)))
  )

  (defun transfer-reserve (sender:string receiver:string)
    (let*  ((reserved (read-reservation sender))
          (ak:decimal (at 'amount-kda reserved))
          (af:integer (at 'amount-fsyc reserved))
          (g:guard (at 'guard reserved))
          (r-g:guard (at 'guard (coin.details receiver)) receiver)
          )
      (enforce-guard g)
      (enforce (> af 0) "No reservations")
      (enforce (validate-principal g sender) "Only k: accounts are supported")
      (enforce (validate-principal r-g receiver) "Only k: accounts are supported")
      (with-default-read reservations receiver
        {'amount-fsyc: 0, 'amount-kda: 0.0, 'guard: r-g}
        {'amount-fsyc:=af-r, 'amount-kda:=ak-r, 'guard:=g}
        (enforce (= g r-g) "receiver guard does not match")
        (write reservations receiver
        {
          'amount-fsyc: (+ af af-r),
          'amount-kda: (+ ak ak-r),
          'account: receiver,
          'guard: r-g
        }
      )
      (update reservations sender
        {
          'amount-kda: 0.0,
          'amount-fsyc: 0
        }
      )
      (emit-event (TRANSFER sender receiver af))
    ))
  )

  (defun admin-reserve (acc)
    (require-capability (PRIVATE))
    (bind acc {
      'kAccount:=account,
      'amount-fsyc:=amount-fsyc,
      'amount-kda:=amount-kda
      }
      (enforce (<= 0 amount-fsyc) "amount-fsyc must atleast be 1")
      (with-capability (RESERVE account amount-kda amount-fsyc)
        (let
          ( (g (at 'guard (coin.details account)))
            (kda-amount:decimal (+ amount-kda (get-amount-kda account)))
            (fsyc-amount:integer (+ amount-fsyc (get-fsyc-reserved account))))
          (enforce (validate-principal g account) "Only k: accounts are supported")
          (if (< (diff-time (at 'block-time (chain-data)) SALE_START_TIME) 0.0) (enforce-whitelist account) "Pre-sale for whitelists ended")
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
  )

  (defun admin-reserve-bulk (accounts)
    (with-capability (MINT-ADMIN)
      (map (admin-reserve) accounts)
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

(defun get-total-reservee ()
    (length
      (select reservations (where 'amount-fsyc (< 0)))
      )
  )

  (defun get-wls ()
     (at 'accounts (read whitelists ""))
  )

  (defun get-total-fsyc-reserved:decimal ()
   723
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

  (defun check-whitelist (account:string)
    (let ( (wls:[string] (get-wls)))
          (enforce (contains account wls) "Not whitelisted")
    )
  )

)


