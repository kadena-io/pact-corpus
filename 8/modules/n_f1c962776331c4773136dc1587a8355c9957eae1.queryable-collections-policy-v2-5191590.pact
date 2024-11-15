(interface queryable-collections-policy-v2

  (defun get-nfts-by-owner:list
    ( 
      account:string
    )
    @doc
      " Get nfts owned by a user. "
  )

  (defun get-public-minted-supply:integer ())
  (defun get-minted-supply:integer ())
)
