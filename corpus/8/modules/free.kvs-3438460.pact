(module kvs GOVERNANCE
  (defcap GOVERNANCE ()
      false)


    (defschema storage-schema
        value: string  
    )

    (deftable storage-table:{storage-schema})

    (defun get (key:string)
        (at "value" (read storage-table key))
    )

    (defun set (key:string value:string)
            (write storage-table key {"value": value})
    )
  
)

