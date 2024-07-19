(interface kmc-token-policy-v2
    
    (defun get-tokens-owned:list ( account:string )
        @doc "Get all tokens owned by one account."
    )

    (defun get-token:object (token-id:string)
        @doc "gets the details of a token.")

    (defun get-status:object (token-id:string)
        @doc "gets sale/staked status of a token.")

    (defun get-owner:string (token-id:string) 
        @doc "gets the owner of a token.")

)

