(module crankkAffs GOVERNANCE

  (defconst ROW_KEY_SEPARATOR:string "/")

  (defcap GOVERNANCE ()
  (with-read radmin "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
  ))

  (defcap INTERNAL ()
    "mark some functions as internal only...."
    true
  )

  (defun init (aguard:guard)
    "Set admin...."
    (insert radmin "admin" {
      "admin": (at "sender" (chain-data)),
      "aguard": aguard
      } )
  )

  (defschema admin
    admin:string
    aguard:guard
  )

  (deftable radmin:{admin})


  (defun get-time:time ()
    "Get block time"
    (at "block-time" (chain-data))
  )

  (defun get-epoch:time ()
    "Get epoch"
    (time "1970-01-01T00:00:00Z")
  )

  (defun get-formattedTime:string (t:time)
    "Get formatted time"
    (format-time "%Y-%m-%dT%H:%M:%S" t)
  )

  (defun read-admin ()
    "Read admin guard"
    (with-read radmin "admin"
      { "admin":= admin,
      "aguard":= aguard }
    (format "Admin guard is {}, admin {}" [aguard admin]))
  )

  (defschema affiliate
     address: string
     percentage: decimal
  )    

  (deftable affiliates:{affiliate})

  (defun add-affiliate (affCode address percentage)
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (insert affiliates affCode
            {"address": address,
             "percentage": percentage}
        )
      )
  )

  (defun update-affiliate (affCode address percentage)
      (with-read radmin "admin"
        { "aguard" := aguard }
        (enforce-guard aguard)
        (update affiliates affCode
            {"address": address,
             "percentage": percentage}
        )
      )
  )


  (defun compound-key2:string
    ( part1:string
      part2:string )
    (format "{}{}{}" [part1 ROW_KEY_SEPARATOR part2])
  )

  (defun get-affiliate (affCode) 
      (read affiliates affCode)     
  )

  (defun get-affiliates () 
    (select affiliates (constantly true)) 
  )

  (defun get-affcodes () 
    (keys affiliates) 
  )

)
; create-table

