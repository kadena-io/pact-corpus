(module SUBBER G
    ;; Define governance keyset
    (defconst SUBBER-KEY "n_e096dec549c18b706547e425df9ac0571ebd00b0.dh_master-keyset")
    (defconst G-MD_SUBBER (keyset-ref-guard SUBBER-KEY))
    (defcap G ()
        (enforce-guard G-MD_SUBBER)
    )

    ;; Add two decimal numbers
    (defun Sub (x:decimal y:decimal)
      "Adds two decimal numbers and returns the result."
      (- x y)
    )
)
