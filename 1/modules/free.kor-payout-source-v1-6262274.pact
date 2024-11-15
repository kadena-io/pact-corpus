(interface kor-payout-source-v1
  
  "Used to add additional sources to kor-payout easily. \
  \ Any module that implements this interface can be added to the kor-payout \
  \ sources table to include it."

  (defschema payout-info
    @doc "The info required to pay the individual. \
    \ Hashrate is measured in TH. "
    account:string
    nft-id:string
    hashrate:decimal
    last-payout:time
    guard:guard
  )

  (defun get-all-payout-infos:[object{payout-info}] ()
    @doc "Get all the hashrate nft info for this source."
  )

  (defun get-payout-info:object{payout-info} (id:string)
    @doc "Gets the info for a single nft."
  )

  (defun update-payout-time:string 
    (id:string t:time)
    @doc "Updates the payout time for the given nft"
  )

)
