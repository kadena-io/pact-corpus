(interface kor-percent-payout-source-v1
  
  "Used to add additional sources to kor-payout easily. \
  \ Any module that implements this interface can be added to the \
  \ kor-percent-payout sources table to include it."

  (defschema percent-payout-info
    @doc "The info required to pay the individual."
    account:string
    nft-id:string
    last-payout:time
    guard:guard
  )

  (defun get-percent-payout:decimal ()
    @doc "Gets the percent that each nft in this source will have."
  )

  (defun get-all-percent-payout-infos:[object{percent-payout-info}] ()
    @doc "Get all the percent payout infos for this source."
  )

  (defun get-percent-payout-info:object{percent-payout-info} (id:string)
    @doc "Gets the info for a single nft."
  )

  (defun update-percent-payout-time:string 
    (id:string t:time)
    @doc "Updates the payout time for the given nft"
  )

)
