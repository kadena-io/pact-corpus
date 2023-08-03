(module mjc-nft-pre-sale GOVERNANCE

    (use guards)

    (defconst MJ_BANK:string "k:8e804049165d9d8dfab055d1daa5e1b88c3dcf60b6c27694999800f2d14478c1")
    (defconst NFT_PER_USER:integer 9500)
    (defconst MJ_SALE_SUPPLY:decimal 10000.0)
    (defconst SALE_START_TIME:time (time "2023-06-10T18:00:00Z"))
    (defconst WHITELIST_TIME:time (time "2023-06-01T12:00:00Z"))
    (defconst END_TIME:time (time "2024-08-01T12:00:00Z"))
    (defconst SALE_STATUS:string 'sale-status)
    (defconst SALE_STARTED "sale-started")
  
    (defschema reservation
      account:string
      guard:guard
      amount-kda:decimal
      amount-mjc:integer
    )
  
    (defschema revoked
      account:string
      refund-txn:string
    )
  
    (defschema whitelist-schema
      accounts:[string]
    )
  
    (defschema sale
      status:string
      price:decimal
    )
  
    (deftable mjc-reservations-table:{reservation})
    (deftable mjc-mjc-mjc-sale-status-table:{whitelist-schema})
    (deftable mjc-sale-status-table:{sale})
  
    (defcap GOVERNANCE ()
      (enforce-guard
        (keyset-ref-guard "free.mjc-admin-nft-keyset"))
    )
  
    (defcap PRICE-ADMIN ()
        (enforce-guard (keyset-ref-guard "free.mjc-admin-nft-keyset"))
    )
  
    (defcap PRIVATE () true)
  
    (defcap REVOKED (account:string refund-txn:string) @event true)
  
    (defcap RESERVE ( account:string amount:decimal amount-mjc:integer)
      @doc"Reserve NFT for MJC"
      (enforce (< SALE_START_TIME (at 'block-time (chain-data))) (format "Sale has not Started {}" [SALE_START_TIME]))
      (enforce (> END_TIME (at 'block-time (chain-data))) (format "Sale has ended {}" [END_TIME]))
      (enforce (< WHITELIST_TIME (at 'block-time (chain-data))) (format "Whitelisting is still on going. Sale will start on {}" [SALE_START_TIME]))
      (let* 
            ( 
                (total-mjc-reserved:decimal (get-total-mjc-reserved))
                (mjc-reserved:integer (get-mjc-reserved account))
                (sale-price:decimal (* amount-mjc (get-sale-price)))
            )
            (enforce
              (= amount sale-price)
              (format "Invalid KDA/NFT amount {} KDA and {} NFT {}" [sale-price amount-mjc amount]))
            (enforce
              (<= (+ amount-mjc total-mjc-reserved) MJ_SALE_SUPPLY)
              (format "Reachecd maximum supply {} for private-sale" [MJ_SALE_SUPPLY])
            )
            (enforce
            (<= (+ amount-mjc mjc-reserved) NFT_PER_USER)
              (format "You can buy only {} tokens" [NFT_PER_USER]))
      )
    )
  
    (defcap TRANSFER (sender:string receiver:string amount-mjc:integer)
      @event
      true
    )
  
    (defun init (accounts:[string])
      (with-capability (GOVERNANCE)
        (insert mjc-mjc-mjc-sale-status-table "" {
          'accounts: accounts
          })
        (insert mjc-sale-status-table SALE_STATUS {
          'status: SALE_STARTED,
          'price: 50.0
        })
      )
    )
  
    (defun enforce-whitelist (account:string)
      (let 
        ( 
            (accounts:[string] (at 'accounts (read mjc-mjc-mjc-sale-status-table "")))
        )
        (enforce (contains account accounts) "You are not whitelisted")
        (enforce-guard (at-after-date WHITELIST_TIME))
        (enforce-guard (at-before-date SALE_START_TIME))
      )
    )
  
    (defun reserve:string (account:string amount-kda:decimal amount-mjc:integer)
      (enforce (<= 0 amount-mjc) "amount-mjc must atleast be 1")
      (with-capability (RESERVE account amount-kda amount-mjc)
        (let
            ( 
                (g (at 'guard (coin.details account)))
                (kda-amount:decimal (+ amount-kda (get-amount-kda account)))
                (mjc-amount:integer (+ amount-mjc (get-mjc-reserved account)))
            )
          (enforce (validate-principal g account) "Only k: accounts are supported")
          
        )
      )
    )
  
    (defun update-sale-price:string (price:decimal)
      @doc   "Update sale price for NFT presale"
      (enforce (> price 0.0) "Price must be greater than 0")
        (with-capability (PRICE-ADMIN)
          (with-read mjc-sale-status-table SALE_STATUS 
            {
                "price":=oldPrice
            }
            (update mjc-sale-status-table SALE_STATUS {"price":price})
            (format "NFT KDA sale price updated: old price {} | new price {}" [oldPrice, price])
          )
        )
    )
  
    (defun update-whitelists (new-wls:[string])
      (with-capability (GOVERNANCE)
        (with-read mjc-mjc-mjc-sale-status-table ""
          {'accounts:= old-wls}
          (update mjc-mjc-mjc-sale-status-table ""
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
        (update mjc-reservations-table (at 'account revoke)
          {
            'amount-kda: 0.0,
            'amount-fsyc: 0
          }
        )
        (emit-event (REVOKED (at 'account revoke) (at 'refund-txn revoke)))
    )
  
    (defun transfer-reserve (sender:string receiver:string)
        (let*  
            (
                (reserved (read-reservation sender))
                (ak:decimal (at 'amount-kda reserved))
                (af:integer (at 'amount-mjc reserved))
                (g:guard (at 'guard reserved))
                (r-g:guard (at 'guard (coin.details receiver)) receiver)
            )
            (enforce-guard g)
            (enforce (> af 0) "No reservations")
            (enforce (validate-principal g sender) "Only k: accounts are supported")
            (enforce (validate-principal r-g receiver) "Only k: accounts are supported")
            (with-default-read mjc-reservations-table receiver
                {'amount-mjc: 0, 'amount-kda: 0.0, 'guard: r-g}
                {'amount-mjc:=af-r, 'amount-kda:=ak-r, 'guard:=g}
                (enforce (= g r-g) "receiver guard does not match")
                (write mjc-reservations-table receiver
                {
                    'amount-mjc: (+ af af-r),
                    'amount-kda: (+ ak ak-r),
                    'account: receiver,
                    'guard: r-g
                }
                )
                (update mjc-reservations-table sender
                {
                    'amount-kda: 0.0,
                    'amount-mjc: 0
                }
                )
                (emit-event (TRANSFER sender receiver af))
            )
        )
    )
  
    (defun get-sale-price:decimal ()
      (at 'price (read mjc-sale-status-table SALE_STATUS))
    )
  
    (defun read-reservation (account:string)
      (read mjc-reservations-table account)
    )
  
    (defun get-accounts ()
      (keys mjc-reservations-table)
    )
  
    (defun get-total-reservee ()
        (length
            (select mjc-reservations-table (where 'amount-mjc (< 0)))
        )
    )
  
    (defun get-wls ()
       (at 'accounts (read mjc-mjc-mjc-sale-status-table ""))
    )
  
    (defun get-total-mjc-reserved:decimal ()
      (fold (+) 0.0 (map (get-mjc-reserved) (get-accounts)))
    )
  
    (defun get-total-kda-reserved:decimal ()
      (fold (+) 0.0 (map (get-amount-kda) (get-accounts)))
    )
  
    (defun get-amount-kda:decimal (account:string)
      (with-default-read mjc-reservations-table account
        { 'amount-kda: 0.0 }
        { 'amount-kda:= amount }
        amount
      )
    )
  
    (defun get-mjc-reserved:integer (account:string)
      (with-default-read mjc-reservations-table account
        { 'amount-mjc: 0 }
        { 'amount-mjc:= amount }
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
      MJ_SALE_SUPPLY
    )
  
    (defun get-mjc-per-user:decimal ()
      NFT_PER_USER
    )
  
    (defun check-whitelist (account:string)
        (let ( (wls:[string] (get-wls)))
            (enforce (contains account wls) "Not whitelisted")
        )
    )
  
)

;; Read the `upgrade` key from transaction data

