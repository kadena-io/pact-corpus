(interface callable-v2
  (defun call:guard (method:string arguments) )
)


(module prod-callable-contracts GOVERNANCE
  (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard "hypercent.hyper-admin-1-prod")))

    (defcap IS_ADMIN ()
      (enforce-guard (keyset-ref-guard "hypercent.hyper-api-admin-prod")))

    (defschema entry
        ref:module{callable-v2}
        contract:string
    )
    (deftable entries:{entry})

    (defun set (contract:string ref:module{callable-v2})
      (with-capability (IS_ADMIN)
        (write entries contract {
          "ref": ref,
          "contract": contract
        })
      )
    )

    (defun get:module{callable-v2} (contract:string)
       (at "ref" (read entries contract))
    )
)


