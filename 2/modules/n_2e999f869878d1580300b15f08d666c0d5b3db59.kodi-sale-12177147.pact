(module kodi-sale GOVERNANCE

  (use util.guards)

  (defschema reservation
    account:string
    guard:guard
    time:time
    amount-kda:decimal
    sold:bool)

  (deftable reservations:{reservation})

  (defcap GOVERNANCE ()
    (enforce-keyset "n_2e999f869878d1580300b15f08d666c0d5b3db59.kodi-admin-keyset"))

  (defcap OPS ()
    (enforce-keyset "n_2e999f869878d1580300b15f08d666c0d5b3db59.kodi-ops-keyset"))

  (defcap RESERVE
    ( account:string
      amount-kda:decimal)
    "Reserve event for kodi reservation"
    @event
    (enforce-guard (before-date END_TIME))
    (enforce-guard (at-after-date START_TIME))
  )

  (defcap ENDSALE ()
    @event
    (enforce-guard (at-after-date END_TIME))
  )

  (defconst KODI_BANK:string 'kodi-bank)

  (defconst KODI_SALE_SUPPLY:decimal 1800000000000.0) ;; 9000000000000 * sale supply percent 20% (0.2)

  (defconst KDA_MIN_PER_USER:decimal 1.0)

  (defconst KDA_RECIPIENT:string "k:fd0ad78ac97e6e383a31f3920e3d98f28b1da2987ba356dbf56f6c5a30febefa")

  (defconst START_TIME:time (time "2024-12-19T00:00:00Z"))

  (defconst END_TIME:time (time "2025-01-08T00:00:00Z"))

  (defun reserve:string (account:string amount-kda:decimal)
    (with-capability (RESERVE account amount-kda)
      (let
        ( 
          (g (at 'guard (coin.details account)))
        )
        (with-default-read reservations account
          { "account"       : ""
          , "amount-kda"    : -1.0
          , "time"          : (at "block-time" (chain-data))
          , "guard"         : g
          , "sold"          : false
          }
          { "amount-kda" := kda
          , "guard"      := retg
          }
          ; we don't want to overwrite an existing guard with the user-supplied one
          (enforce (= retg g)
            "account guards do not match")
    
          (let ((is-new
                  (if (= kda -1.0)
                    (enforce-reserved account g)
                    false)))
            (let ((new-amount (if is-new amount-kda (+ kda amount-kda))))
              (enforce (>= new-amount KDA_MIN_PER_USER) "Less than minimum")

              (coin.transfer account KDA_RECIPIENT amount-kda)
              (write reservations account
                { "account"    : account
                , "amount-kda" : new-amount
                , "time"       : (at "block-time" (chain-data))
                , "guard"      : g
                , "sold"      : false
                })
            )
          )
          (format "{} reserved KODI with {} KDA" [account, amount-kda])
        ))))
  

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


  (defun transfer-kodi-amount (kodi-ratio:decimal res:object{reservation})
    (require-capability (OPS))
    (require-capability (ENDSALE))
    
    (let 
      (
        (amount-kda (at 'amount-kda res))
        (account (at 'account res))
        (guard (at 'guard res))
      )
      (install-capability (n_2e999f869878d1580300b15f08d666c0d5b3db59.kodi.TRANSFER KODI_BANK account (floor (* amount-kda kodi-ratio) 15)))
      (n_2e999f869878d1580300b15f08d666c0d5b3db59.kodi.transfer-create KODI_BANK account guard (floor (* amount-kda kodi-ratio) 15))
      (update reservations account { "sold": true })
      "transferred"
    )
  )

  (defun end-sale (n:integer)
    (enforce (> n 0) "n should be positive")
    (with-capability (OPS)
      (with-capability (ENDSALE)
        (let*
          (
            (ratio (get-current-ratio))
            (filtered (select reservations (where 'sold (!= true))))
            (filtered-length (length filtered))
            (taken (take (if (> filtered-length n) n filtered-length) filtered))
          )
          (enforce (> filtered-length 0) "No one rows remaining")
          (map (transfer-kodi-amount ratio) taken)
        )
      )
    )
  )

  (defun get-remaining-rows-count ()
    (length (select reservations (where 'sold (!= true))))
  )

  (defun get-total-kda:decimal ()
    (fold (+) 0.0 (map (at "amount-kda") (read-all-reservations)))
  )

  (defun get-current-ratio:decimal ()
    (let
      (
        (total-kda (get-total-kda))
      )
      (/ KODI_SALE_SUPPLY total-kda)
    )
  )

  (defun read-reservations (account:string)
    (select reservations (where 'account (= account)))
  )

  (defun read-all-reservations ()
    (map (read reservations) (get-accounts))
  )

  (defun get-accounts ()
    (keys reservations)
  )

  (defun get-start-time ()
    START_TIME
  )

  (defun get-end-time ()
    END_TIME
  )

  (defun get-sale-supply ()
    KODI_SALE_SUPPLY
  )

  (defun get-kda-min-per-user ()
    KDA_MIN_PER_USER
  )
)


