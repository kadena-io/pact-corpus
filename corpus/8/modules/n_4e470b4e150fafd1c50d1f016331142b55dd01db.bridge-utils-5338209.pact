(module bridge-utils GOVERNANCE

  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))

  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

    ;-----------------------------------------------------------------------------
    ; Tables and schema
    ;-----------------------------------------------------------------------------
    (defschema bridge-target
      ledger:string
      token:string
      chain:string
    )

    (defconst NULL-TARGET:object{bridge-target} {'ledger:"", 'token:"", 'chain:""})

    (defschema bridge-inbound-sch
      guard:guard
      source:object{bridge-target}
    )

    (defschema bridge-outbound-sch
      guard:guard
      dest:object{bridge-target}
    )

    (defun enforce-target-not-null:bool (target:object{bridge-target})
      (enforce (!= target NULL-TARGET) "Bridge disabled"))
)

