(interface router-iface

    (use token-message [token-message])

    ;; Collateral token state
    (defschema col-state
        token:module{fungible-v2, fungible-xchain-v1}
    )

    (defschema router-address
        remote-address:string
    )

    (defun transfer-remote:string (destination:integer sender:string recipient-tm:string amount:decimal)
        @doc "Execute token transfer from sender to destination"
    )

    (defun handle:bool (origin:integer sender:string chainId:integer reciever:string receiver-guard:guard amount:decimal)
        @doc "Mints or unlocks tokens to recipient when router receives transfer message."
    )
    
    (defun get-adjusted-amount:decimal (amount:decimal) 
        @doc "Returns number of decimals of token"
    )
)
