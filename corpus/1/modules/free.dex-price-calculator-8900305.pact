(module dex-price-calculator GOV

  (defcap GOV ()
    true)

  (defun compute-simple-amount-in
      (
       token-in:module{fungible-v2}
       token-out:module{fungible-v2}
       amount-out:decimal
       )
    (let*
        (
         (p (kdlaunch.kdswap-exchange.get-pair token-in token-out))
         (reserve-in (kdlaunch.kdswap-exchange.reserve-for p token-in))
         (reserve-out (kdlaunch.kdswap-exchange.reserve-for p token-out))
         (frac (/ (* reserve-in reserve-out) (- reserve-out amount-out)))

         )
      (ceiling (* (- frac reserve-in) 1.003) 7)
      )
    )

  (defun compute-simple-amount-out
      (
       token-in:module{fungible-v2}
       token-out:module{fungible-v2}
       amount-in:decimal
       )
    (let*
        (
         (p (kdlaunch.kdswap-exchange.get-pair token-in token-out))
         (reserve-in (kdlaunch.kdswap-exchange.reserve-for p token-in))
         (reserve-out (kdlaunch.kdswap-exchange.reserve-for p token-out))
         (frac (/ (* reserve-in reserve-out) (+ reserve-in (* 0.997 amount-in))))
         )
      (floor (- reserve-out frac) 7)
      )
    )

  )

