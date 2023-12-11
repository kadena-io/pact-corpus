(module raspberrypi-xyzn7 'io_admin_keyset-xyzn7
 @doc "sensor data."



 (defschema device
        @doc "Device Register"
        
        name:string
        keyset:keyset
       )

 (defschema device-data
  @doc "Device data"
  
  data:string
  device_id:string
  keyset:keyset
 )

 (deftable device-table:{device})

 (deftable device-data-table:{device-data})


(defun new-device(device_id:string
         name:string
         keyset:keyset) 
(write device-table device_id {
        "name":name
        ,"keyset":keyset
})
        )


 (defun new_device_data (id:string
                     data:string
                     device_id:string
                     keyset:keyset
                     )
 @doc "updates data"
(with-read device-table device_id
        {"keyset":=device_keyset
} (enforce-keyset device_keyset ) )

 
  (write device-data-table id
         { "data":data
         , "device_id":device_id
         , "keyset":keyset
         }))


(defun get-device(id)
 (with-read device-table id {
         "name":=name
         ,"keyset":=keyset
 }[name keyset])
)


 (defun read-data (id:string)
  @doc "Read only row from table and returns it."
  (with-read device-data-table id
             {  
              "data":=data
             , "device_id":=device_id
             , "keyset":=keyset
             } [data device_id keyset ]))

  (defun read-all-data-by-device(device_id:string)
  (select device-data-table (where 'device_id (= device_id))

  ) 
        )
)


