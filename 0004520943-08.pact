(interface kmc-token-policy-v13
    
    (defun get-tokens-owned:list (account:string)
        @doc "Get all tokens owned by one account.")

    (defun get-token:object (token-id:string)
        @doc "gets the details of a token.")

    (defun get-status:object (token-id:string)
        @doc "gets sale/staked status of a token.")

    (defun get-owner:string (token-id:string) 
        @doc "gets the owner of a token.")

    (defun get-all-for-sale:list ()
        @doc "gets all items for sale on the respective policy")

    (defun update-for-sale:bool (token-id:string sale:bool)
        @doc "lists or de-lists a token")

    (defun emit-buy:bool (nft-id:string buyer:string seller:string price:decimal policy:string)
        @doc "emits the buy event from another contract")

    (defcap BUY_KMC:bool (nft-id:string buyer:string seller:string price:decimal policy:string)
        @doc "Emitted when an NFT is sold"
        @event )

)

