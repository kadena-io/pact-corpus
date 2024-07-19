(interface generic-burnable-nft-v1-beta1

  (defschema burnable-nft-details
    owner:string
    guard:guard
  )

  (defun burn:bool (token-id:string))

  (defun owner-details:object{burnable-nft-details} (token-id:string))
)


