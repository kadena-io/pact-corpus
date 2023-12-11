(interface marketplace-v1

  (defun transfer-nft:bool
    (owner:string receiver:string id:string guard:guard)
       @doc "Transfer an NFT to an account."
  )

  (defun put-id-for-sale:bool
    (owner:string id:string price:decimal guard:guard on_auction:bool)
    @doc "Puts an NFT up for sale"
    ; @model [
    ;   (property (> price 0.0))
    ; ]
  )

  (defun remove-id-from-sale:bool
    (owner:string id:string guard:guard)
    @doc "Removes an NFT from sale"
  )

  (defun buy-id-on-sale:bool
    (id:string curr-user:string user_guard:guard)
    @doc "Buys an NFT that was put up for sale"
    @model [
      (property (!= curr-user ""))
    ]
  )

  (defun get-nft-owner:string (id:string)
    @doc "Returns the owner of the nft"
  )

)

