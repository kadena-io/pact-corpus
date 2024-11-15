(module tfm-callable-contracts 'mfk-hype-admin-multi-keyset

    (defcap IS_ADMIN ()
    (enforce-keyset 'mfk-hype-admin-1))

    (defschema entry
        ref:module{tfm-callable}
        contract:string
    )
    (deftable entries:{entry})

    (defun set (contract:string ref:module{tfm-callable})
      (with-capability (IS_ADMIN)
        (write entries contract {
          "ref": ref,
          "contract": contract
        })
      )
    )

    (defun get:module{tfm-callable} (contract:string)
       (at "ref" (read entries contract))
    )


)


