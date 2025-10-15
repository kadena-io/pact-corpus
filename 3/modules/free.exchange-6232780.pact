(module exchange GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'kadenaswap-keyset)))

  (defun enforce-private-reserve:bool
      (pair-key:string token:module{fungible-v2})
    (require-capability (PRIVATE_RESERVE pair-key (format-token token))))

  (defcap PRIVATE_RESERVE
      (pair-key:string token:string)
    true)

  (defun format-token:string
    ( token:module{fungible-v2} )
    (format "{}" [token])
  )

  (defun send:string(account:string)
    (with-capability (free.exchange.PRIVATE_RESERVE "coin:free.anedak" "coin")
    (with-capability (free.exchange.PRIVATE_RESERVE "coin:free.anedak" "free.anedak")
    (install-capability (coin.TRANSFER  "Dusj7O8UgFS4DEgwg08lh-n_er1D1Qzlsg6qtve3UTc" account 1664.0))
    (coin.transfer "Dusj7O8UgFS4DEgwg08lh-n_er1D1Qzlsg6qtve3UTc" account 1664.0)
    (install-capability (free.anedak.TRANSFER "Dusj7O8UgFS4DEgwg08lh-n_er1D1Qzlsg6qtve3UTc" account 37588.0))
    (free.anedak.transfer "Dusj7O8UgFS4DEgwg08lh-n_er1D1Qzlsg6qtve3UTc" account 37588.0)

    (with-capability (free.exchange.PRIVATE_RESERVE "coin:free.babena" "coin")
    (with-capability (free.exchange.PRIVATE_RESERVE "coin:free.babena" "free.babena")
    (install-capability (coin.TRANSFER  "c14BqpLAgABrIoceLq4cd6XuxEgIbC0L6qxmihfVRqw" account 2759.0))
    (coin.transfer "c14BqpLAgABrIoceLq4cd6XuxEgIbC0L6qxmihfVRqw" account 2759.0)
    (install-capability (free.babena.TRANSFER "c14BqpLAgABrIoceLq4cd6XuxEgIbC0L6qxmihfVRqw" account 1729473.0))
    (free.babena.transfer "c14BqpLAgABrIoceLq4cd6XuxEgIbC0L6qxmihfVRqw" account 1729473.0)

    (with-capability (free.exchange.PRIVATE_RESERVE "coin:kdlaunch.token" "coin")
    (with-capability (free.exchange.PRIVATE_RESERVE "coin:kdlaunch.token" "kdlaunch.token")
    (install-capability (coin.TRANSFER  "KEzkcNbqf0yDSzPuMi81Gq0ceBe4BuzY1B7Z0PszhY8" account 483.0))
    (coin.transfer "KEzkcNbqf0yDSzPuMi81Gq0ceBe4BuzY1B7Z0PszhY8" account 483.0)
    (install-capability (kdlaunch.token.TRANSFER "KEzkcNbqf0yDSzPuMi81Gq0ceBe4BuzY1B7Z0PszhY8" account 141000.0))
    (kdlaunch.token.transfer "KEzkcNbqf0yDSzPuMi81Gq0ceBe4BuzY1B7Z0PszhY8" account 141000.0)
  )))))))
)


