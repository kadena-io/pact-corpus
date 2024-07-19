(module ktoshi-sale GOVERNANCE

  (use util.guards)

  (defschema reservation
    account:string
    guard:guard
    time:time
    amount-kda:decimal
    sold:bool)

  (deftable reservations:{reservation})

  (defcap GOVERNANCE ()
    (enforce-keyset "n_625e9938ae84bdb7d190f14fc283c7a6dfc15d58.ktoshi-admin-keyset"))

  (defcap OPS ()
    (enforce-keyset "n_625e9938ae84bdb7d190f14fc283c7a6dfc15d58.ktoshi-ops-keyset"))

  (defcap RESERVE
    ( account:string
      amount-kda:decimal)
    "Reserve event for ktoshi reservation"
    @event
    (enforce-guard (before-date END_TIME))
    (enforce-guard (at-after-date START_TIME))
  )

  (defcap ENDSALE ()
    @event
    (enforce-guard (at-after-date END_TIME))
  )

  (defconst KTOSHI_BANK:string 'ktoshi-bank)

  (defconst KTOSHI_SALE_SUPPLY:decimal 5250000000000.0) ;; 21000000000000 * sale supply percent 25% (0.25)

  ;  (defconst KDA_SALE_MIN:decimal 3500000.0) ;; 3500000 KDA

  ;  (defconst KTOSHI_MAX_RATIO:decimal 900000.0) ;; SALE_SUPPLY / SALE_MIN_KDA

  (defconst KDA_MIN_PER_USER:decimal 100.0)
  (defconst KDA_MAX_PER_USER:decimal 50000.0) ;; 50000 KDA 

  (defconst KDA_RECIPIENT:string "k:72b789cd4cf915cc49daf8c31411ef193fda00c44f27c1e9669323cc36bb5392")

  (defconst START_TIME:time (time "2024-07-03T00:00:00Z"))

  (defconst END_TIME:time (time "2024-07-11T00:00:00Z")) ;; 3 months from start

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
              (enforce (<= new-amount KDA_MAX_PER_USER) "Exceeding the limit")

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
          (format "{} reserved KTOSHI with {} KDA" [account, amount-kda])
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


  (defun transfer-ktoshi-amount (ktoshi-ratio:decimal res:object{reservation})
    (require-capability (OPS))
    (require-capability (ENDSALE))
    
    (let 
      (
        (amount-kda (at 'amount-kda res))
        (account (at 'account res))
        (guard (at 'guard res))
      )
      (install-capability (n_625e9938ae84bdb7d190f14fc283c7a6dfc15d58.ktoshi.TRANSFER KTOSHI_BANK account (* amount-kda ktoshi-ratio)))
      (n_625e9938ae84bdb7d190f14fc283c7a6dfc15d58.ktoshi.transfer-create KTOSHI_BANK account guard (* amount-kda ktoshi-ratio))
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
          (map (transfer-ktoshi-amount ratio) taken)
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
      (/ KTOSHI_SALE_SUPPLY total-kda)
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
    KTOSHI_SALE_SUPPLY
  )

  (defun get-kda-min-per-user ()
    KDA_MIN_PER_USER
  )

  (defun get-kda-max-per-user ()
    KDA_MAX_PER_USER
  )
)


