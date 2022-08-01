(interface staging11-callable
  (defun call (method:string arguments) )
)


(module staging11-callable-contracts 'hype-admin-local-test-staging

    (defcap IS_ADMIN ()
    (enforce-keyset 'hype-admin-local-test-staging))

    (defschema entry
        ref:module{staging11-callable}
        contract:string
    )
    (deftable entries:{entry})

    (defun set (contract:string ref:module{staging11-callable})
      (with-capability (IS_ADMIN)
        (write entries contract {
          "ref": ref,
          "contract": contract
        })
      )
    )

    (defun get:module{staging11-callable} (contract:string)
       (at "ref" (read entries contract))
    )
    

)


