(interface mailbox-state-iface

   (defschema mailbox-state
      paused:bool
      nonce:integer
      latest-dispatched-id:string
   )

   (defschema dependency
      hook:module{hook-iface}
   )

   (defschema delivery
      block-number:integer
   )
   
   (defschema router-hash
      router-ref:module{router-iface}  
   )
)
