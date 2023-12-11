(interface callable
  (defun call (method:string arguments) )
)



(module fml-callable-contracts 'admin-multi-keyset

    (defcap IS_ADMIN ()
    (enforce-keyset 'admin-1))

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


