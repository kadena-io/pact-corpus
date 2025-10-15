(module gas-station GOVERNANCE
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_7d195c48a309af310bc16ced11d156aff8105579.bonder-admin")))

  (implements gas-payer-v1)
  (use coin)
  (use n_7d195c48a309af310bc16ced11d156aff8105579.gas-guards)

  (defconst MIN_GAS_PRICE:decimal 0.0000001)
  (defconst MAX_GAS_LIMIT:integer 100000) ;; TODO: use a better value based on what transactions actually cost
  (defconst MAX_TX_CALLS:integer 5)

  (defcap GAS_PAYER:bool
      ( user:string
        limit:integer
        price:decimal
        )
    (enforce (= "exec" (at "tx-type" (read-msg))) "Inside an exec")
    (enforce (> (length (at "exec-code" (read-msg))) 0) "Tx at least one pact function")
    (enforce (<= (length (at "exec-code" (read-msg))) MAX_TX_CALLS) "Tx has too many pact functions")
    (let
        ( (enforce-ns (lambda (i) (enforce (= "(n_7d195c48a309af310bc16ced11d156aff8105579." (take 44 (at i (at "exec-code" (read-msg))))) "only cabinet namespace on top level")))
         (len (length (at "exec-code" (read-msg))))
          )
      (map (enforce-ns) (enumerate 0 (- len 1)))
      )
    (enforce-below-or-at-gas-price MIN_GAS_PRICE)
    (enforce-below-or-at-gas-limit MAX_GAS_LIMIT)
    (compose-capability (ALLOW_GAS))
    )

  (defcap ALLOW_GAS () true)

  (defun init ()
    (coin.create-account (GAS_STATION)
                         (gas-station-guard))
    )

  (defun create-gas-payer-guard:guard ()
    (create-user-guard (gas-payer-guard))
    )

  (defun gas-payer-guard ()
    (require-capability (GAS))
    (require-capability (ALLOW_GAS))
    )

  (defun gas-station-guard:guard ()
    (guard-any
     [
      (create-gas-payer-guard)
      (keyset-ref-guard "n_7d195c48a309af310bc16ced11d156aff8105579.bonder-admin")
      ]))

  (defun GAS_STATION:string ()
    (create-principal
     (gas-station-guard))
    )

  )


