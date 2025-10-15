(module ADDER G
    ;; Define governance keyset
    (defconst ADDER-KEY "n_e096dec549c18b706547e425df9ac0571ebd00b0.dh_master-keyset")
    (defconst G-MD_ADDER (keyset-ref-guard ADDER-KEY))
    (defcap G ()
        (enforce-guard G-MD_ADDER)
    )

    ;; Add two decimal numbers
    (defun Add (x:decimal y:decimal)
      "Adds two decimal numbers and returns the result."
      (+ x y)
    )
)
