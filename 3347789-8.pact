(interface callable
  (defun call (method:string arguments) )
)


(module prod-callable-contracts GOVERNANCE
  (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard "hypercent.hyper-admin-1-prod")))

    (defcap IS_ADMIN ()
      (enforce-keyset "hypercent.hyper-api-admin-prod"))

    (defschema entry
        ref:module{callable}
        contract:string
    )
    (deftable entries:{entry})

    (defun set (contract:string ref:module{callable})
      (with-capability (IS_ADMIN)
        (write entries contract {
          "ref": ref,
          "contract": contract
        })
      )
    )

    (defun get:module{callable} (contract:string)
       (at "ref" (read entries contract))
    )
)


