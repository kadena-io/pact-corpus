(module test-libs GOVERNANCE

;;;;;;;;;;;;;; SCHEMAS ;;;;;;;;;;;;;;

  (defschema data-schema
    @doc "simple data schema"
    data:string
  )

  (deftable secrets-of-the-world:{data-schema})

;;;;;;;;;;;;;; Capabilities ;;;;;;;;;;;;;;

(defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.isoko-admin")))
(defcap ANY_CAP ()
    @doc "private cap for test"
    true)

;;;;;;;;;;;;;; Methods  ;;;;;;;;;;;;;;

    (defun read-data (key:string)
        (read secrets-of-the-world key)
    )

    (defun write-data (key:string data:string)
    
        (write secrets-of-the-world key {"data":data})
            (format "wrote {} under key {}" [data key])
    
    )

    (defpact multi-step-test (key:string value:string amount:integer)
        (step-with-rollback
            (write-data key value)
            (write-data key "")
        )
        (step
            (read-data key)
        )
    )
)






