(interface kmc-token-policy-v1
    
    (defun get-tokens-owned ( account:string )
        @doc "Get all tokens owned by one account."
    )

    (defun get-token (token-id:string)
        @doc "gets the details of a token.")

    (defun get-status (token-id:string)
        @doc "gets sale/staked status of a token.")

    (defun get-owner (token-id:string) 
        @doc "gets the owner of a token.")

)

