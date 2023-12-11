(module sensor_store7 GOVERNANCE
 @doc "sensor data store."

(use coin)

  (defcap GOVERNANCE ()
    (enforce-keyset 'io_admin_keyset-xyzn_test8))

 (defschema device
        @doc "Device Register"
        device_id:string
        name:string
        status:string
        account:string
        guard:guard
       )

 (defschema device-data
  @doc "Device data"
  data_id:string
  data:string
  device_id:string
 )

 (defschema device-rules
       @doc "Register Rules for device data"
        rule_id:string
        rule_name:string
        rule:string
        action:string
        disabled:bool
        device_id:string
        )

 (deftable device-table:{device})
 (deftable device-data-table:{device-data})
 (deftable device-rules-table:{device-rules})

(defcap DEVICE_GUARD (device_id:string)

 (with-read device-table device_id{"guard":=guard}
   (enforce-guard guard)
   )
)

(defcap ACCOUNT_GUARD(account:string)
@doc "Verifies account meets format and belongs to caller"
(enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
(enforce-guard   
    (at "guard" (coin.details account))
)
)

(defun new-device(device_id:string
         name:string
         status:string
         account:string
         guard:keyset)
(with-capability(ACCOUNT_GUARD account)
(insert device-table device_id {
  "device_id":device_id
  ,"name":name
  ,"status":status
  ,"account":account
  ,"guard":guard
})
)
)

(defun update-device(device_id:string
                name:string
                status:string)
(with-capability (DEVICE_GUARD device_id)
 (update device-table device_id {
               "name":name
               ,"status":status
       })
))

(defun update-device-guard(device_id:string
  guard:keyset)
(with-capability (DEVICE_GUARD device_id)
(update device-table device_id {
 "guard":guard
})
))

 (defun new-device-data (data_id:string
                     data:string
                     device_id:string
                     )
 @doc "update data"
(with-read device-table device_id
  {"status":=status}
 (enforce (= status "active") "device inactive")
(with-capability (DEVICE_GUARD device_id)
 (insert device-data-table data_id
         { "data_id":data_id
         ,"data":data
         ,"device_id":device_id
         })
)))



(defun new-device-rule(rule_id:string
                device_id:string
                rule:string
                rule_name:string
                disabled:bool
                action:string
                )
@doc "create new rules"
(with-read device-table device_id
  {"status":=status}
(enforce (= status "active") "device inactive")
(with-capability (DEVICE_GUARD device_id)
(insert device-rules-table rule_id
    {"rule_id":rule_id
    ,"rule_name":rule_name
    ,"rule": rule
    ,"disabled":disabled
    ,"device_id":device_id
    ,"action": action
    })
)))

(defun update-device-rule (rule_id:string
        device_id:string
        rule:string
        rule_name:string
        disabled:bool
        action:string
        )
@doc "update rules"

(with-read device-table device_id
  {"status":=status}
  (enforce (= status "active") "device inactive")
(with-capability (DEVICE_GUARD device_id)
(update device-rules-table rule_id
{ "rule_name":rule_name
, "rule": rule
, "device_id":device_id
,"disabled":disabled
, "action": action
})
)
  )

)


(defun get-device(device_id:string)
 (with-read device-table device_id {
         "name":=name
         ,"status":=status
 }{"device_id":device_id , "name":name, "status":status })
)

(defun get-account-devices(account:string)
(select device-table ["device_id", "name", "status"] (where 'account (= account))
)
)

 (defun read-data (data_id:string)
  @doc "Read data by id"
  (with-read device-data-table data_id
             {
              "data":=data
             , "device_id":=device_id
             } {"data":data, "device_id": device_id}))

(defun read-rule (rule_id:string)
             @doc "Read rule by id"

             (with-read device-rules-table rule_id
                        {
                         "rule_name":=rule_name,
                         "rule":=rule,
                         "action":=action,
                         "device_id":=device_id
                        } {"rule_name":rule_name, "rule": rule, "action": action, "device_id": device_id})
)

  (defun read-device-data(device_id:string)
  (select device-data-table (where 'device_id (= device_id))
  ))

  (defun read-device-rules(device_id:string)
  (select device-rules-table (where 'device_id (= device_id))
  ))
  )


