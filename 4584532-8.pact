(module kadena-mining-club GOVERNANCE
  @doc "Kadena Mining Club mint contract."
    (use coin)
    (use marmalade.ledger)
    (use kmc-policy)
    (use kmc-founders-policy)
    (use kmc-vial-policy)
; ============================================
; ==               CONSTANTS                ==
; ============================================
    (defconst BANK_KDA_ACCT "kmc-hashrate")
    (defconst ADMIN_KEYSET "free.kmc-admin")
    (defconst FOUNDERS_CREATED_COUNT "founders-count")
    (defconst MINERS_CREATED_COUNT "miners-count")
    (defconst MINERS_MINTED_COUNT "miners-minted-count") ;the count for front end interface, phase 2
    (defconst CURRENT_MINER_ID_COUNT "current-miner-id-count")
    (defconst GAS_PER_NFT_MINTED "gas")
    (defconst KDA_MINED_UPDATE_COUNT "kda-mined-update-count")
    (defconst MARMALADE_BANK "kmc-marmalade-bank")
    (defconst CLAIM_COUNT "claim-count")

    (defconst TOTAL_VOLUME_KEY "total-volume-count-key")
    
    (defconst CURRENT_OFFER_ID "current-offer-id")

    (defconst NFT_OFFER_ID "nft-offer-id")
    (defconst COLLECTION_OFFER_ACCOUNT "kmc-offers")
    (defconst NFT_OFFER_ACCOUNT "kmc-nft-offers")
    (defconst WEI_ACCOUNT "k:bf994dd0503d36501fd5096982566c9f3b5f9684982d7c20735387b26cdc7103")
    (defconst MEGA_ACCOUNT "k:1b57695390163531852f7724313e3ef9ab4728425fead4d1d120444c33f1aa58")

    (defconst ADMIN_ADDRESS "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst CREATOR_FUND "k:35fe76ea8f40caa2bb660b3236132f339dfdac2586a3d2a9d63ea96ee91202ad")
    (defconst DISCORD_ADDRESS "k:4aab9f08f1bd86c3ce007a9a87225ef061c09e7062efa622e2fd704c24514cfa")
    (defconst FOUNDERS_FUND "k:0ab2f447374b4968abd6e689b9fb00e7e82ffd99bb0543084e9eeaba10651f92")

    (defun init ()
      (coin.create-account MARMALADE_BANK (marm-bank-guard))
      "marm bank created"
    )

; ============================================
; ==            SCHEMA AND TABLES           ==
; ============================================

    (defschema wl-schema
        @doc "Basic schema used for WL members, keys are account ids"
        role:string
    )

    (defschema airdrop-schema
        airdrop:list
    )

    (defschema claim-schema
        claim-count:integer
        account:string
        claim:list
    )

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )

    (defschema price-schema
        @doc "Stores the price of each type of NFT or upgrade"
        price:decimal
    )

    (defschema values-schema
        @doc "Schema used for storing basic values"
        value:string
    )

    (defschema mint-schema
        status:string
    )

    (defschema uri-schema
        @doc "A schema to store all URIs before mint"
        uri:string 
    )

    (defschema wl-tracker-schema
        @doc "stores how many whitelist mints one account has remaining"
        wl-mints-remaining:integer
    )

    (defschema kda-mined-schema
        @doc "stores the datetime and amount of total KDA mined, keys are 1->infinity"
        kda-mined:decimal
        datetime:time
    )

    (defschema listing-schema 
        previous-owner:string
        previous-owner-guard:guard
        id:string
        price:decimal
        updated-at:time
        is-miner:bool
    )

    (defschema listings-schema 
        previous-owner:string
        previous-owner-guard:guard
        id:string
        price:decimal
        updated-at:time
        policy:string
    )

    (defschema collection-offer-schema
        address:string
        valid-until:time
        amount:decimal
        offer-id:integer
        policy:string
    )

    (defschema nft-offer-schema
        address:string ; the address of the account that created the offer
        valid-until:time
        amount:decimal
        offer-id:integer
        nft-id:string
        nft-uri:string
        old-nft-id:string
    )

    (defschema nft-token-pair
        ; key is nft-id
        token-id:string
    )

    ;every entry is one NFT, to be stored on the ledger fledger for Founders Ledger
    (defschema fentry
        @doc "stores information of the Founders NFT collection"
        nft-id:string
        airdrops-remaining:integer
        free-mints-remaining:decimal
        market-price:decimal
    )

    ; every entry is one NFT, to be stored on the ledger "mledger"
    (defschema entry
        ;add market conditions
        @doc "Stores information of the Miners NFT collection"
        nft-id:string
        uri:string
        hashrate:decimal ;hashrate is only updated once per payment cycle
        tied-asic:list
        special-attributes:string
        market-price:decimal
        updated-at:time
        kda-mined-index:integer
        warranty-expiry:time
    )

    (deftable marketplace-table:{listings-schema})
    (deftable listings:{listing-schema})
    (deftable nft-token-pairs:{nft-token-pair})
    (deftable wl-remaining-per-account:{wl-tracker-schema})
    (deftable miner-uri-table:{uri-schema})
    (deftable mledger:{entry}) ;mledger stands for Miners Ledger. Contains info for all 10,000 Miners
    (deftable wl:{wl-schema})
    (deftable fledger:{fentry})
    (deftable counts-table:{counts-schema})
    (deftable price-table:{price-schema})
    (deftable values-table:{values-schema})
    (deftable airdrop-table:{airdrop-schema})
    (deftable claim-table:{claim-schema})
    (deftable kda-mined-table:{kda-mined-schema})
    (deftable collection-offer-table:{collection-offer-schema})
    (deftable nft-offer-table:{nft-offer-schema})

    (defun initialize ()
        @doc "Initialize the module the first time it is deployed" 
        ; (insert counts-table CURRENT_OFFER_ID {"count": 778})
        (insert counts-table CLAIM_COUNT {"count": 0})
        ; (insert counts-table MINERS_CREATED_COUNT {"count": 1})
        ; (insert counts-table KDA_MINED_UPDATE_COUNT {"count": 0})
        ; (insert counts-table NFT_OFFER_ID {"count": 0})
        ; (insert price-table TOTAL_VOLUME_KEY {"price": 630600.5864})
        ; (insert counts-table FOUNDERS_CREATED_COUNT {"count": 0})
    )

; ============================================
; ==              MINT FUNCTIONS            ==
; ============================================
    
    (defun withdraw-contract-funds (account:string)
    	(with-capability (ADMIN)
    	(with-capability (BANK_DEBIT)
    		(coin.transfer account ADMIN_ADDRESS (coin.get-balance account))
    	))
    )

    (defun populate-new-listings (start:integer)
        (with-capability (PRIVATE)
        (with-capability (ADMIN)
            (map (populate-new-listing) (enumerate start (+ 600 start)))
        ))
    )
    
    (defun populate-new-listing (item:integer)
        (require-capability (PRIVATE))
        (update collection-offer-table (int-to-str 10 item)
            { "policy" : "free.kmc-policy" }
        )
    )

    (defun admin-set-staked (ids:list on-off:bool)
        (with-capability (ADMIN)
        (with-capability (PRIVATE)
            (map (set-mining-status on-off) ids)
        ))
    )

    (defun set-mining-status (on-off:bool id:string)
        (with-capability (ADMIN)
            (update mledger id
                { "kda-mined-index": (- (get-count KDA_MINED_UPDATE_COUNT) 1) }
            )
            (with-capability (CALL-POLICY-MODULES)
            (kmc-policy.update-staked id on-off))
        )
    )

    (defun get-token-id (key:string)
        (read nft-token-pairs key ['token-id])
    )

    (defun place-nft-offer (account:string offer:decimal duration:integer nft-id:string )
        @doc "allows a user to place an offer on a specific NFT that is already listed on the marketplace"
        (enforce (> offer 0.0) "Your offer must be greater than 0 KDA")
        (let
            (
                (miner-status (kmc-policy.get-miner-status nft-id))
                (valid-until-time (add-time (at "block-time" (chain-data)) duration))
                (offer-id (get-count NFT_OFFER_ID))
            )
            (with-capability (PRIVATE)
                (with-read mledger nft-id
                    { 'uri := uri }
                    (enforce (= true (at 'for-sale miner-status)) "The NFT you're trying to place an offer on is not currently listed for sale")
                (coin.transfer account NFT_OFFER_ACCOUNT offer)
                (insert nft-offer-table (int-to-str 10 offer-id)
                    { "address": account
                    , "valid-until": valid-until-time
                    , "amount": offer
                    , "offer-id": offer-id
                    , "nft-id": nft-id
                    , "nft-uri": uri 
                    , "old-nft-id": (int-to-str 10 (at 'number (kmc-policy.get-token nft-id))) })
                (increase-count NFT_OFFER_ID)
                (emit-event (PLACE_NFT_OFFER offer (int-to-str 10 offer-id) nft-id))
                (format "Your offer on NFT #{} of {} KDA has been submitted with an expiration time of {} UTC and an ID of {}. You can remove your offer at any time. If your offer expires, you must come back and collect your KDA"
                      [nft-id offer valid-until-time offer-id])
                )
            )
        )
    )

    (defun accept-nft-offer (offer-id:string)
        @doc "Allows a user to accept the current collection offer, sending their chosen NFT to the bidder and collecting the KDA"
        (with-read nft-offer-table offer-id
            { 'valid-until := valid-until
            , 'amount := amount 
            , 'address := new-address
            , 'nft-id := nft-id }
            (enforce (> valid-until (at "block-time" (chain-data))) "offer has expired")
            (with-read marketplace-table nft-id 
                { "previous-owner" := orig-owner }
                (with-capability (ACCOUNT_GUARD orig-owner)
                    (let 
                        (
                            (staked (at 'staked (kmc-policy.get-miner-status nft-id)))
                        )
                        (enforce (= false staked) "Your NFT must be turned off before you can accept an nft offer")
                        (with-capability (PRIVATE)
                        (with-capability (BANK_DEBIT)
                            (set-volume (+ (get-price TOTAL_VOLUME_KEY) amount))
                            (royalty-payouts NFT_OFFER_ACCOUNT orig-owner amount)
                        ))
                    )
                    (install-capability (marmalade.ledger.TRANSFER nft-id MARMALADE_BANK new-address 1.0))
                    (marmalade.ledger.transfer-create nft-id MARMALADE_BANK new-address (at 'guard (coin.details new-address)) 1.0)
                    (update nft-offer-table offer-id
                        { "valid-until" :  (time "2022-01-01T12:00:00Z") }
                    )
                    (emit-event (BUY_MINER nft-id new-address orig-owner amount))
                    (format "Offer accepted successfully. NFT #{} has been transferred to {}, and {} KDA has been sent to {}, minus the 5% marketplace fee"
                      [nft-id new-address amount orig-owner])
                )
            )
            (update marketplace-table nft-id 
                { 'price : 0.0} )
        )
    )

    (defun remove-nft-offer (offer-id:string)
        @doc "Allows a user to remove their collection offer and get their KDA back."
        (with-capability (BANK_DEBIT)
            (with-read nft-offer-table offer-id
                { 'address := offer-creator-address 
                , 'amount := amount 
                , 'valid-until := valid-until
                , 'nft-id := nft-id }
                (with-capability (ACCOUNT_GUARD offer-creator-address)
                    (enforce (> valid-until (time "2022-01-02T12:00:01Z")) "Your offer has already been withdrawn or accepted")
                    (update nft-offer-table offer-id
                        { "valid-until" : (time "2022-01-01T12:00:00Z")}
                    )
                    (coin.transfer NFT_OFFER_ACCOUNT offer-creator-address amount)
                    (emit-event (REMOVE_NFT_OFFER amount offer-id nft-id))
                    (format "Your offer on NFT #{} of {} KDA has been removed, and the KDA has been returned to {} successfully"
                  [nft-id amount offer-creator-address])
                )
            )
        )
    )

    (defun select-offers-on-nfts-owned-by (account:string)
        @doc "Selects offers for NFTs owned by one k:account"
        (filter (!= []) (map (wrapper) (get-user-miners-object account)))
    )

    (defun wrapper (item:object)
        (select-valid-offers-for-id (at 'nft-id item))
    )

    (defun select-valid-offers-for-id (nft-id:string)
        @doc "Selects the current valid offer for an ID"
        (select nft-offer-table 
            (and? (where 'nft-id (= nft-id))
              (where "valid-until" (< (at "block-time" (chain-data))))))
    )

    (defun select-offers-made-by-account (account:string)
        @doc "selects all nft-specific-offers made by an account that have not yet been withdrawn"
        (select nft-offer-table 
            (and? (where 'address (= account))
              (where 'valid-until (!= (time "2022-01-01T12:00:00Z")))))
    )
    
    (defun claim (recipient:string nft-ids:list)
        @doc "allows a user to withdraw x amount of KDA from kmc-hashrate wallet"
        (with-capability (PRIVATE)
        (with-capability (BANK_DEBIT)
        (with-capability (ACCOUNT_GUARD recipient)
            (let 
                (
                  (calculated (private-get-total-earned-kda nft-ids recipient))
                )
                (if (> calculated 0.0)
                    ; (format "claiming temporarily disabled, calculated was {}" [calculated])
                    (claim-work recipient nft-ids calculated)
                    (format "Total claim amount is {}. Claim amount must be above 0.0 KDA, thus no kda was sent" [calculated])
                )
            )
        )))
    )

    (defun claim-to-builders-club (account:string nft-ids:list)
        @doc "When a miner is turned on, any KDA earned while it was off gets sent to the builders club"
        (with-capability (PRIVATE)
        (with-capability (BANK_DEBIT)
            (let
                (
                  (calculated (OFF-private-get-total-earned-kda nft-ids account))
                )
                (if (> calculated 0.0)
                    (claim-work FOUNDERS_FUND nft-ids calculated)        
                    (format "Total claim amount is {}. Claim amount must be above 0.0 KDA, thus no kda was sent" [calculated])
                )
            )
        ))
    )

    (defun claim-work (recipient:string nft-ids:list calculated:decimal)
        @doc "does the work for the claim function"
        (require-capability (PRIVATE))
        (let 
            (
              (previous-balance (coin.get-balance recipient))
              (kda-mined-index (- (get-count KDA_MINED_UPDATE_COUNT) 1))
            )
            (coin.transfer BANK_KDA_ACCT recipient calculated)
            (map (update-kda-mined-index kda-mined-index) nft-ids)
            ; (claim-update-airdrop-table recipient (format "{}" [calculated])) 
            (add-claim2 recipient (format "{}" [calculated]))
            (emit-event (CLAIM recipient calculated))
            (format "Successfully claimed {} kda from {} miners. Old balance: {} KDA and New Balance: {} KDA" [calculated, (length nft-ids), previous-balance, (coin.get-balance recipient)])
        )
    )

    (defun update-kda-mined-index (kda-mined-index:integer nft-id:string)
        @doc "updates the kda-mined-index for one nft"
        (require-capability (PRIVATE))
        (update mledger nft-id 
            { "kda-mined-index": kda-mined-index }
        )
    )

    (defun get-kda-since-last-claim (nft-id:string)
        @doc "For a single NFT, calculates the amount of kda that has been mined since the last claim"
        (with-read mledger nft-id 
          { 'kda-mined-index := kda-mined-index }
            (if (= false (at 'staked (kmc-policy.get-miner-status nft-id))) 
                (- 1 1) ;if staked is false, return 0
                (with-read kda-mined-table (int-to-str 10 kda-mined-index) ;if staked is true, do maths
                  { 'kda-mined := previous-kda-mined }
                    (with-read kda-mined-table (int-to-str 10 (- (get-count KDA_MINED_UPDATE_COUNT) 1) )
                        {'kda-mined := latest-kda-mined }
                            (/ (- latest-kda-mined previous-kda-mined) 10000)
                    )
                )
            )
        )
    )

    (defun enforced-get-kda-since-last-claim (account:string latest-kda-mined:decimal nft-id:string)
        @doc "For a single NFT, calculate kda mined since last claim and only allow calling this function by the NFT owner"
        (with-read mledger nft-id 
          { 'kda-mined-index := kda-mined-index }
            (let 
                (
                    (owner (kmc-policy.get-owner nft-id))
                )
                (enforce (= owner account) "Account is not owner of the NFT")
                (if (= false (at 'staked (kmc-policy.get-miner-status nft-id))) 
                    (- 1 1) ;if staked is false, return 0
                    (with-read kda-mined-table (int-to-str 10 kda-mined-index) ;if staked is true, do maths
                      { 'kda-mined := previous-kda-mined }
                        (/ (- latest-kda-mined previous-kda-mined) 10000)
                    )
                )
            )
        )
    )

    (defun private-get-total-earned-kda (nft-ids:list account:string)
        @doc "gets the total kda earned for a list of NFTs, fails if any NFT is not owned by (account)"
        (with-read kda-mined-table (int-to-str 10 (- (get-count KDA_MINED_UPDATE_COUNT) 1) )
            {'kda-mined := latest-kda-mined }
            (let 
                (
                    (earned-kda-list (map (enforced-get-kda-since-last-claim account latest-kda-mined) nft-ids))
                )
                (fold (+) 0.0 earned-kda-list)
            )
        )
    )    

    (defun OFF-enforced-get-kda-since-last-claim (account:string latest-kda-mined:decimal nft-id:string)
        @doc "For a single NFT, calculate kda mined since the miner was turned off and only allow calling this function by the NFT owner"
        (with-read mledger nft-id
          { 'kda-mined-index := kda-mined-index }
            (let 
                (
                    (owner (kmc-policy.get-owner nft-id))
                )
                (enforce (= owner account) "Account is not owner of the NFT")
                (if (< kda-mined-index 6564)
                    (- 1 1) ; do not do maths on NFTs with mined-indexes prior to 01Feb2023
                        (if (= true (at 'staked (kmc-policy.get-miner-status nft-id)))
                            (- 1 1) ;if staked is true, return 0
                            (with-read kda-mined-table (int-to-str 10 kda-mined-index) ;if staked is false, do maths
                              { 'kda-mined := previous-kda-mined }
                                (/ (- latest-kda-mined previous-kda-mined) 10000)
                            )
                        )
                )
            )
        )
    )

    (defun OFF-private-get-total-earned-kda (nft-ids:list account:string)
        @doc "gets the total kda earned for a list of NFTs, fails if any NFT is not owned by (account)"
        (with-read kda-mined-table (int-to-str 10 (- (get-count KDA_MINED_UPDATE_COUNT) 1) )
            {'kda-mined := latest-kda-mined }
            (let
                (
                    (earned-kda-list (map (OFF-enforced-get-kda-since-last-claim account latest-kda-mined) nft-ids))
                )
                (fold (+) 0.0 earned-kda-list)
            )
        )
    )

    (defun public-get-total-earned-kda (nft-ids:list)
        @doc "gets the total kda earned for a list of NFTs"
        (let 
            (
                (earned-kda-list (map (get-kda-since-last-claim) nft-ids))
            )
            (fold (+) 0.0 earned-kda-list)
        )
    )

    (defun insert-kda-mined-admin (kda-mined:decimal)
        @doc "updates the total kda mined"
        (with-capability (PRIVATE)
        (with-capability (ADMIN)
            (insert kda-mined-table (int-to-str 10 (get-count KDA_MINED_UPDATE_COUNT))
                { "kda-mined": kda-mined
                , "datetime": (at "block-time" (chain-data))}
            )
            (increase-count KDA_MINED_UPDATE_COUNT)
        ))
    )

    (defun insert-kda-mined (kda-mined:decimal)
        @doc "updates the total kda mined"
        (with-capability (PRIVATE)
        (with-capability (ACCOUNT_GUARD DISCORD_ADDRESS)
            (insert kda-mined-table (int-to-str 10 (get-count KDA_MINED_UPDATE_COUNT))
                { "kda-mined": kda-mined
                , "datetime": (at "block-time" (chain-data))}
            )
            (increase-count KDA_MINED_UPDATE_COUNT)
        ))
    )

    (defun update-kda-mined-admin (kda-mined:decimal index:string )
        @doc "updates the total kda mined"
        (with-capability (ADMIN)
            (update kda-mined-table index
                { "kda-mined": kda-mined
                , "datetime": (at "block-time" (chain-data))}
            )
        )
    )

    (defun get-kda-mined-for-index (index:integer)
        @doc "Gets the object containing amount of kda mined and date/time for an index"
        (read kda-mined-table (int-to-str 10 index))
    )
    
    (defun get-latest-kda-mined ()
        (read kda-mined-table (int-to-str 10 (- (get-count KDA_MINED_UPDATE_COUNT) 1)))
    )

    (defun get-count (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counts-table key ['count]))
    )
        
    (defun set-price(key:string value:decimal)
        @doc "Sets the price for a key to store in the price-table"
        (with-capability (ADMIN)
            (update price-table key 
                {"price": value} 
            )
        )
    )

    (defun set-count(key:string value:integer)
        @doc "Sets the count for a key to store in the counts-table"
        (with-capability (ADMIN)
            (update counts-table key 
                {"count": value} 
            )
        )
    )

    (defun royalty-payouts (sending-account:string receiving-account:string amount:decimal)
        (require-capability (PRIVATE))
        (install-capability (coin.TRANSFER sending-account receiving-account (round (* 0.95 amount) 2)))
        (install-capability (coin.TRANSFER sending-account WEI_ACCOUNT (round (* 0.01 amount) 2)))
        (install-capability (coin.TRANSFER sending-account MEGA_ACCOUNT (round (* 0.005 amount) 2)))
        (install-capability (coin.TRANSFER sending-account CREATOR_FUND (round (* 0.035 amount) 2)))
        (coin.transfer sending-account receiving-account (round (* 0.95 amount) 2))
        (coin.transfer sending-account WEI_ACCOUNT (round (* 0.01 amount) 2))
        (coin.transfer sending-account MEGA_ACCOUNT (round (* 0.005 amount) 2))
        (coin.transfer sending-account CREATOR_FUND (round (* 0.035 amount) 2))
    )

    (defun place-collection-offer (account:string offer:decimal duration:integer nft-type:string) ;expiry is number of seconds
        @doc "Transfers KDA from a users wallet to kmc-offers wallet and creates an offer on the collection"
        (coin.transfer account COLLECTION_OFFER_ACCOUNT offer)
        (enforce (> offer 0.0) "Your offer must be greater than 0 KDA")
        (with-capability (PRIVATE)
            (let 
                (
                    (valid-until-time (add-time (at "block-time" (chain-data)) duration))
                    (offer-id (int-to-str 10 (get-count CURRENT_OFFER_ID)))
                )
                (insert collection-offer-table offer-id
                    { "address": account
                    , "valid-until": valid-until-time
                    , "amount": offer
                    , "offer-id": (get-count CURRENT_OFFER_ID)
                    , "policy": nft-type })
                (increase-count CURRENT_OFFER_ID)
                (format "Your offer of {} KDA has been submitted with an expiration time of {} UTC and an ID of {}. You can remove your offer at any time. If your offer expires, you must come back and collect your KDA. This can be done via your inventory screen."
                  [offer valid-until-time offer-id])
                
            )
        )
    )

    (defun accept-collection-offer (account:string nft-id:string offer-id:string)
        @doc "Allows a user to accept the current collection offer, sending their chosen NFT to the bidder and collecting the KDA"
        (with-read collection-offer-table offer-id
            { 'valid-until := valid-until
            , 'amount := amount 
            , 'address:= new-address
            , 'policy := offer-policy}
            (enforce (> valid-until (at "block-time" (chain-data))) "offer has expired")
            (with-capability (PRIVATE)
                (let* (
                        (info (bind (marmalade.ledger.get-policy-info nft-id)
                            { 'policy := policy:module{free.kmc-token-policy-v13}}
                                { "owner" : (policy::get-owner nft-id)
                                , "for-sale": (at 'for-sale (policy::get-status nft-id))
                                , "emitted" : (policy::emit-buy nft-id new-address account amount (format "{}" [policy]))
                                , "policy" : (format "{}" [policy])} )
                                )
                        (for-sale (at 'for-sale info))
                        (owner (at 'owner info))
                        (policy (at 'policy info))
                        (actual (if (= owner MARMALADE_BANK) account owner))

                    )
                    (enforce (= offer-policy policy) "You are trying to accept an offer for a different type of NFT")
                    (set-volume (+ (get-price TOTAL_VOLUME_KEY) amount))
                    (with-capability (BANK_DEBIT)
                        (royalty-payouts COLLECTION_OFFER_ACCOUNT actual amount)
                    )
                    (if (= for-sale false)
                        (marmalade.ledger.transfer-create nft-id owner new-address (at 'guard (coin.details new-address)) 1.0)
                        (transfer-from-marm-bank nft-id new-address account)
                    )
                    (update collection-offer-table offer-id
                        { "valid-until" :  (time "2022-01-01T12:00:00Z") }
                    )
                )
            )
            (format "Offer accepted successfully. NFT #{} has been transferred to {}, and {} KDA has been sent to {}, minus the 5% marketplace fee"
          [nft-id new-address amount account])
        )
    )

    (defun transfer-from-marm-bank (nft-id:string new-address:string account:string)
        (require-capability (PRIVATE))
        (with-read marketplace-table nft-id
            { 'previous-owner := previous-owner }
            (enforce (= previous-owner account) "You are not the owner of this NFT")
            (with-capability (ACCOUNT_GUARD previous-owner)
                (install-capability (marmalade.ledger.TRANSFER nft-id MARMALADE_BANK new-address 1.0))
                (marmalade.ledger.transfer-create nft-id MARMALADE_BANK new-address (at 'guard (coin.details new-address)) 1.0)
                (update marketplace-table nft-id { 'price : 0.0} )
            )
        )
    )

    (defun remove-collection-offer (account:string offer-id:string)
        @doc "Allows a user to remove their collection offer and get their KDA back."
        (with-capability (BANK_DEBIT)
        (with-capability (ACCOUNT_GUARD account)
            (with-read collection-offer-table offer-id
                { 'address := offer-creator-address 
                , 'amount := amount 
                , 'valid-until := valid-until }
                (enforce (= account offer-creator-address) "Your account is not the account that initiated this offer")
                (enforce (> valid-until (time "2022-01-02T12:00:01Z")) "Your offer has already been withdrawn or accepted")
                (update collection-offer-table offer-id
                    { "valid-until" : (time "2022-01-01T12:00:00Z")}
                )
                (coin.transfer COLLECTION_OFFER_ACCOUNT offer-creator-address amount) 
                (emit-event (REMOVE_COLLECTION_OFFER offer-id))
                (format "Your offer of {} KDA has been removed, and the KDA has been returned to {} successfully"
                  [amount offer-creator-address])
            )
        ))
    )

    (defun select-valid-offers2 (type:string)
        @doc "Selects all offers for the collection that have not yet expired"
            (select collection-offer-table 
            (and? (where "valid-until" (< (at "block-time" (chain-data))))
              (where "policy" (= type))))
    
    )

    (defun select-all-collection-offers ()
        (select collection-offer-table (where "address" (!= "nope")))
    )
    
    (defun select-valid-offers ()
        @doc "Selects all offers for the collection that have not yet expired"
        (select collection-offer-table (where "valid-until" (< (at "block-time" (chain-data)))))
    )


    (defun select-offers-for-account (account:string)
        @doc "selects all offers made by an account"
        (select collection-offer-table 
            (and? (where "address" (= account))
              (where "valid-until" (!= (time "2022-01-01T12:00:00Z")))))
    )

    (defun remove-free-mint-founder-multiple (id:list)
        (with-capability (ADMIN)
            (with-capability (PRIVATE)
                (map 
                    (remove-free-mint-founder)
                    id
                )
            )
        )
    )

    (defun remove-free-mint-founder (id:string)
        (with-capability (ADMIN)
            (update fledger id
                { "free-mints-remaining" : 0.0 })
        )
    )

; ============================================
; ==       State-modifying functions        ==
; ============================================

    (defun fix-airdrop (account:string item:list)
        (with-capability (ADMIN)
        (update airdrop-table account
            {"airdrop": item}
        ))
    )

    (defun claim-update-airdrop-table (account:string amount:string)
        @doc "chooses to add a new account or update an old account on the airdrop table"
        (require-capability (PRIVATE))
        (let 
            ( 
              (exists (try false (let ((ok true)) (with-read airdrop-table account {'airdrop := airdrop-temp1}"") ok)))
            )
            (if (= exists true) 
                (update-claim account amount)
                (add-claim account amount)
            )
            (format "claimed {} " [amount])
        )
    )

    (defun add-claim2 (account:string amount:string)
        @doc "Adds a claim to the claim table"
        (insert claim-table (int-to-str 10 (get-count CLAIM_COUNT))
            { "account": account
            , "claim": (make-list 1 {"amount":amount, "tiempo": (at "block-time" (chain-data))})
            , "claim-count": (get-count CLAIM_COUNT) }
        )
        (with-capability (PRIVATE)
        (increase-count CLAIM_COUNT))
    )

    (defun add-claim (account:string amount:string)
        @doc "Adds a claim amount to the airdrop table for a new account"
        (require-capability (PRIVATE))
        (insert airdrop-table account
            {"airdrop":  (make-list 1 {"amount":amount, "tiempo": (at "block-time" (chain-data))}) }
        )
    )

    (defun update-claim (account:string amount:string)
        @doc "Adds a claim amount to the airdrop table for an already existing account"
        (require-capability (PRIVATE))
        (with-read airdrop-table account
            {'airdrop := airdrop-temp }
            (update airdrop-table account
                {"airdrop": (+ airdrop-temp (make-list 1 {"amount":amount, "tiempo":(at "block-time" (chain-data))})) }
            )
        )
    )

    (defun get-airdrop-details-object (account:string)
        @doc "Returns the entire list of claims/airdrops that an account has received"
        { "airdrop": (combine-lists account) }
    )

    (defun get-airdrop-details-select-format (account:string)
        (map (combine-claims) (select claim-table ['claim] (where "account" (= account))))
    )

    (defun combine-claims (item:object)
        (at 0 (at 'claim item))
    )

    (defun combine-lists (account:string)
        @doc "returns airdrops/claims from both tables"
        
        (let 
            (
                (airdrop (with-default-read airdrop-table account
                    { "airdrop": [] }
                    { "airdrop":= airdrop }
                    airdrop))
                (combined-claims (get-airdrop-details-select-format account))
            )
            (+ airdrop combined-claims)
        )
    )

    (defun turn-off-and-claim-multiple (nft-ids:list account:string)
        @doc "Turns off all miners in the list and claims their earned KDA"
        (with-capability (PRIVATE)
            (with-capability (BANK_DEBIT)
            (with-capability (ACCOUNT_GUARD account)
                (claim account nft-ids)
            ))
            (let 
                (
                  (kda-mined-index (- (get-count KDA_MINED_UPDATE_COUNT) 1))
                )
                (map 
                    (turn-off-and-claim account kda-mined-index)
                    nft-ids
                )
            )
            
            (format "Successfully turned off {} miners and claimed their kda" [(length nft-ids)])
        )
    )

    (defun turn-off-and-claim (account:string kda-mined-index:integer nft-id:string)
        (require-capability (PRIVATE))
        (with-capability (MINER_OWNER account nft-id)
            (update mledger nft-id
                { "kda-mined-index": kda-mined-index }
            )
            (with-capability (CALL-POLICY-MODULES)
            (kmc-policy.update-staked nft-id false))
        )
        ; (emit-event (TURN_OFF_MINER nft-id))
    )

    (defun turn-on-miner-multiple1 (nft-ids:list account:string guard:guard)
        @doc "turns on all miners in the list and sets the kda-mined-index for each. Creates a coin account if the caller does not have one yet."
        (claim-to-builders-club account nft-ids)
        (with-capability (PRIVATE)
            (let 
                (
                  (kda-mined-index (- (get-count KDA_MINED_UPDATE_COUNT) 1))
                )
                (map 
                    (turn-on-miner1 kda-mined-index)
                    nft-ids
                )
            )
        )
        (let ((num-miners (length nft-ids)))
            (format "Successfully turned on {} miners" [num-miners])
        )
    )

    (defun turn-on-miner1 (kda-mined-index:integer nft-id:string)
        (require-capability (PRIVATE))
        (let 
            (
              (for-sale (at 'for-sale (kmc-policy.get-miner-status nft-id)))
            )
            (enforce (= for-sale false) "Your NFT is listed on the marketplace, please de-list before turning on your miner")
            (with-capability (MINER_OWNER (kmc-policy.get-owner nft-id) nft-id)
                (update mledger nft-id
                    { "kda-mined-index": kda-mined-index }))
                (with-capability (CALL-POLICY-MODULES)
                (kmc-policy.update-staked nft-id true))
            (emit-event (TURN_ON_MINER nft-id)
            )
        )
    )

    (defun increase-count (key:string)
        ;increase the count of a key in a table by 1
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )

    (defun get-mined-index (nft-id:string)
        @doc "gets the index corresponding to when the nft was turned on"
        (read mledger nft-id ['kda-mined-index])
    )

    (defun set-value(key:string value:string)
        @doc "Sets the value for a key to store in a table"
        (with-capability (ADMIN)
            (update values-table key 
                {"value": value} 
            )
        )
    )
    
; ============================================
; ==     NON STATE-MODIFYING FUNCTIONS      ==
; ============================================

     (defun get-staked-nfts-for-account (account:string)
        @doc "Returns the NFTs that are staked and owned by one account"
        (select mledger ['nft-id]
            (and? (where 'owner-address (= account))
              (where 'staked (= true))))
    ) ;TODO probably delete
 
    (defun all-ids ()
        @doc "Returns all the ids"
        (keys mledger)
    )

    ; (account) inputs are any valid k:address"
    (defun get-user-founders-object (account:string)
        @doc "Returns all founders owned by one address"
        ; (map (get-founder-details-with-object) 
        (map (get-founder-details-with-object) 
            (+ (select marketplace-table ['id] (and? (and? (where "price" (!= 0.0))
                             (where "previous-owner" (= account)) )
                       (where "policy" (= "kmc-founders-policy"))                 ))
            (kmc-founders-policy.get-tokens-owned account)) )
    )

(defun select-test3 (account:string)
(select marketplace-table ['id] (or (where "policy" (= "free.kmc-policy")) (where "policy" (= "kmc-policy"))    )  )
)


    (defun get-founder-details-with-object (item:object)
        (get-founders-details (at 'id item))
    )

    ; (nft-id) inputs are    0<=nft-id<=FOUNDERS_MAX_MINT
    (defun get-founders-details (nft-id:string)
        @doc "Returns the details of a Founder's Pass"
        (with-default-read marketplace-table nft-id
          { "previous-owner": "nope"
          , "price" : 0.0 }
          { "previous-owner":= original-owner
          , "price" := price} 
        {
          "old-nft-id" : (int-to-str 10 (at 'number (kmc-founders-policy.get-token nft-id)))
        , "nft-id" : (at "nft-id" (read fledger nft-id ['nft-id] ))
        , "owner-address" : (kmc-founders-policy.get-owner nft-id)
        , "market-price" : price
        , "price" : price
        , "airdrops-remaining" : (at "airdrops-remaining" (read fledger nft-id ['airdrops-remaining] ))
        , "free-mints-remaining" : (at "free-mints-remaining" (read fledger nft-id ['free-mints-remaining] ))
        , "for-sale" : (at 'for-sale (kmc-founders-policy.get-status nft-id))
        , "original-owner": original-owner }
        )
    )
    
    (defun get-founder-for-sale-true-false:bool (nft-id:string)
        @doc "returns the status of an NFT for sale, true or false."
        (at 'for-sale (kmc-founders-policy.get-status nft-id))
    )
    (defun get-miner-for-sale-true-false:bool (nft-id:string)
        @doc "returns the status of an NFT for sale, true or false."
        (at 'for-sale (kmc-policy.get-miner-status nft-id))
    )

    (defun get-all-miners-for-sale ()
        @doc "Gets all Miner NFTs for sale, indexes are inclusive"
        (map (get-miner-details-with-object2) (kmc-policy.get-all-for-sale))
    )

    (defun get-all-founders-for-sale ()
        (map (get-founder-details-with-object2) (kmc-founders-policy.get-all-for-sale))
    )

    (defun get-founder-details-with-object2 (item:object)
        (get-founders-details (at 'token-id item))
    )

    ; (account) inputs are any valid k:address"
    (defun get-user-miners-object (account:string)
        @doc "Returns all miners owned by one address"
        (map (get-miner-details-with-object) 
            (+ (select marketplace-table ['id] (and? (and? (where "price" (!= 0.0))
                             (where "previous-owner" (= account)) )
                       (where "policy" (= "kmc-policy"))))
            (kmc-policy.get-tokens-owned account)) )
    )

    (defun get-all-vials-for-sale ()
        (map (get-vial-details-with-object2) (kmc-vial-policy.get-all-for-sale))
    )

    (defun get-vial-details-with-object2 (item:object)
        (get-vial-details (at 'token-id item))
    )

    (defun get-vial-details-with-object (item:object)
        (get-vial-details (at 'id item))
    )

    (defun get-vial-details (nft-id:string)
        @doc "Returns the details of an Immersion Vial"
        (with-default-read marketplace-table nft-id
          { "previous-owner": "nope"
          , "price" : 0.0 }
          { "previous-owner":= original-owner
          , "price" := price} 
        {
          "old-nft-id" : (int-to-str 10 (at 'number (kmc-vial-policy.get-token nft-id)))
        , "nft-id" : nft-id
        , "owner-address" : (kmc-vial-policy.get-owner nft-id)
        , "uri" : (at 'assetUri (at 'datum (at 0 (at 'data (at 'manifest (at 'token (marmalade.ledger.get-policy-info nft-id)))))))
        , "market-price" : price
        , "price" : price
        , "for-sale" : (at 'for-sale (kmc-vial-policy.get-status nft-id))
        , "original-owner": original-owner }
        )
    )

    (defun get-user-vials-object (account:string)
        @doc "Returns all vials owned by one address"
        (map (get-vial-details-with-object) 
            (+ (select marketplace-table ['id] (and? (and? (where "price" (!= 0.0))
                             (where "previous-owner" (= account)) )
                       (where "policy" (= "free.kmc-vial-policy"))))
            (kmc-vial-policy.get-tokens-owned account)) )
    )

    (defun read-listing (nft-id:string)
        (read listings nft-id)
    )

    (defun read-marketplace-table (nft-id:string)
        (read marketplace-table nft-id)
    )

    (defun get-miner-details-with-object (item:object)
        (get-miner-details (at 'id item))
    )

    (defun get-miner-details-with-object2 (item:object)
        (get-miner-details (at 'token-id item))
    )

    (defun get-miner-details (nft-id:string)
        (with-default-read marketplace-table nft-id
          { "previous-owner": "nope"
          , "price" : "0.0" }
          { "previous-owner":= original-owner
          , "price" := price }
        {
          "old-nft-id" : (int-to-str 10 (at 'number (kmc-policy.get-token nft-id)))
        , "nft-id" : (at "nft-id" (read mledger nft-id ['nft-id] ))
        , "owner-address" : (kmc-policy.get-owner nft-id)
        , "uri" : (at "uri" (read mledger nft-id ['uri] ))
        , "hashrate" : (at "hashrate" (read mledger nft-id ['hashrate] ))
        , "tied-asic" : (at "tied-asic" (read mledger nft-id ['tied-asic] ))
        , "special-attributes" : (at "special-attributes" (read mledger nft-id ['special-attributes] ))
        , "staked" : (at 'staked (kmc-policy.get-miner-status nft-id))
        , "for-sale" : (at 'for-sale (kmc-policy.get-miner-status nft-id))
        , "market-price" : price
        , "price" : price
        , "kda-mined-index" : (at "kda-mined-index" (read mledger nft-id ['kda-mined-index] ))
        , "original-owner" : original-owner 
        , "warranty-expiry" : (read mledger nft-id ['warranty-expiry])
        }
        )
    )


    (defun get-miner-details-object (nft-id:string)
        @doc "Returns all fields of a miner NFT"
        (read mledger nft-id) ;TODO might need to add for-sale and staked to this object
    )



    ; (nft-id) inputs are    0<=nft-id<MAX_MINT
    (defun get-owner-mledger (nft-id:string)
        @doc "Returns the owner of a particular miner in the Miner Ledger"
        (kmc-policy.get-owner nft-id)
    )

    ; (account) inputs are any valid k:address"
    (defun get-user-miners (account:string)
        @doc "Returns all miners owned by one address"
        (kmc-policy.get-tokens-owned account)
    )

    
    (defun get-price (price-key:string)
        @doc "Gets the price for a key"
        (at "price" (read price-table price-key ["price"]))
    )
    (defun get-value (key:string)
        @doc "Gets value for a key"
        (at "value" (read values-table key ['value]))
    )
    
    (defun get-wl-members ()
        @doc "Returns all addresses currently on the miner whitelist table"
        (keys wl)
    )
    (defun get-all-wl-roles ()
        (select wl ["role"] (where "role" (!= "KMCisthebest")))
    )
    (defun get-wl-role (account:string)
        @doc "Returns the highest whitelist role for a specific account"
        (at "role" (read wl account ['role]))
    )
    (defun get-wl-mints-remaining (account:string)
        @doc "Returns the number of whitelist mints remaining"
        (at "wl-mints-remaining" (read wl-remaining-per-account account ['wl-mints-remaining]))
    )
    ; (key) inputs are the same as get-count
    (defun id-for-next-key (key:string)
        @doc "returns the next id for a given key"
        (int-to-str 10 (get-count key))
    )

    (defun get-user-founders (account:string)
        @doc "Returns all Founder's Passes owned by one address"
        (kmc-founders-policy.get-tokens-owned account)
    )

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )

    (defun get-all-miner-owners ()
        (kmc-policy.get-all-owners)
    )
 
; ============================================
; ==           COIN ACCOUNT CHECKS          ==
; ============================================

    (defun enforce-coin-account-exists (account:string)
        (let ((exist (coin-account-exists account)))
            (enforce exist "Account does not exist in coin contract, please send 0.001 KDA to the address on chain 8 and try again."))
    )

    (defun coin-account-exists:bool (account:string)
        (try false
            (let ((ok true))
                (coin.details account)
                ok))
    )

    (defun coin-account-guard (account:string)
        @doc "enforces coin account guard"
        (at "guard" (coin.details account))
    )

    (defun get-coin-guard (account)
       (format "{}" [(at "guard" (coin.details account))])
    )

    (defun key (id:string account:string)
        @doc "returns id/account data structure"
        (format "{}:{}" [id account])
    )

    (defun set-market-price (nft-id:string price:decimal)
        (with-capability (ADMIN)
            (update mledger nft-id {
              'market-price: price
            })
        )
    )

    (defun marm-bank-guard () 
        (create-module-guard "bank")
    )

    (defun list-marm-bank (nft-id:string owner:string price:decimal)
        (require-capability (PRIVATE))
        (let 
            (
                (exists (try false (let ((ok true)) (with-read marketplace-table nft-id {'previous-owner := temp1}"") ok)))
                (policy (bind (marmalade.ledger.get-policy-info nft-id)
                    { 'policy := policy:module{free.kmc-token-policy-v13}}
                    policy))
            )
            (if (= false exists)
                (insert marketplace-table nft-id { "previous-owner-guard": (at 'guard (coin.details owner)) ,
                    "id": nft-id , "previous-owner": owner , "price": price, "updated-at": (at "block-time" (chain-data)),
                    "policy": (format "{}" [policy])})
                (update marketplace-table nft-id { "previous-owner-guard": (at 'guard (coin.details owner)) ,
                    "previous-owner": owner , "price": price, "updated-at": (at "block-time" (chain-data)) })
            )
        )
        (marmalade.ledger.transfer-create nft-id owner MARMALADE_BANK (marm-bank-guard) 1.0)
    )

    (defun delist-marm-bank (nft-id:string owner:string)
        (require-capability (PRIVATE))
        (with-read marketplace-table nft-id 
            { "previous-owner-guard" := orig-owner-guard
            , "previous-owner" := orig-owner }
            (let 
                (
                    (owner-guard (at 'guard (coin.details owner))) 
                )
                (enforce (and (= orig-owner-guard owner-guard ) (= owner orig-owner)) "You are not the original owner of this NFT")
            )
            (install-capability (marmalade.ledger.TRANSFER nft-id MARMALADE_BANK orig-owner 1.0))
            (marmalade.ledger.transfer-create nft-id MARMALADE_BANK orig-owner (at 'guard (coin.details orig-owner)) 1.0)
        )
        (update marketplace-table nft-id 
            { 'price : 0.0} )
        (format "NFT with id {} has been taken off the market" [nft-id])
    )

    (defun list-on-marketplace (nft-id:string owner:string price:decimal for-sale:bool)
        @doc "Make a new listing or update a listing for a Miner on the marketplace"
        (enforce (and (> price 1.0) (< price 999999999.999)) "Price must be above 1.0 and less than 999,999,999.9 KDA")
        (with-capability (PRIVATE)
        (with-capability (CALL-POLICY-MODULES)
            (if (= for-sale true) 
                (list-marm-bank nft-id owner price)
                (delist-marm-bank nft-id owner)
            )
            (bind (marmalade.ledger.get-policy-info nft-id)
                    { 'policy := policy:module{free.kmc-token-policy-v13}}
                    (policy::update-for-sale nft-id for-sale))
        ))
        (if (= for-sale true)
            (format "NFT with id {} is listed on the market for {} KDA" [nft-id price])
            (format "NFT with id {} has been taken off the market" [nft-id])
        )    
    )

    (defun buy-from-marketplace (nft-id:string buyer:string price:decimal)
        @doc "Buy a NFT from the marketplace"
        (with-read marketplace-table nft-id
            { 'price := ledger-price
            , 'previous-owner := previous-owner }
            (enforce (!= buyer previous-owner) "You cannot buy your own NFT")
            (enforce (= price ledger-price) "Price mismatch")
            (let* 
                (
                    (info (bind (marmalade.ledger.get-policy-info nft-id)
                        { 'policy := policy:module{free.kmc-token-policy-v13}}
                        { "emitted" : (policy::emit-buy nft-id buyer previous-owner price (format "{}" [policy])), "for-sale": (at 'for-sale (policy::get-status nft-id))} ))
                )
                (enforce (= (at 'for-sale info) true)  "You can only purchase an NFT that is for sale." )
                (coin.transfer buyer previous-owner (round (* 0.95 ledger-price) 2))
                (coin.transfer buyer CREATOR_FUND (round (* 0.035 ledger-price) 2))
                (coin.transfer buyer WEI_ACCOUNT (round (* 0.01 ledger-price) 2))
                (coin.transfer buyer MEGA_ACCOUNT (round (* 0.0025 ledger-price) 2))
                (with-capability (PRIVATE)
                (with-capability (CALL-POLICY-MODULES)
                    (set-volume (+ (get-price TOTAL_VOLUME_KEY) ledger-price))
                    (bind (marmalade.ledger.get-policy-info nft-id)
                        { 'policy := policy:module{free.kmc-token-policy-v13}}
                        (policy::update-for-sale nft-id false)) ))
                (update marketplace-table nft-id 
                    { 'price : 0.0} )
                (install-capability (marmalade.ledger.TRANSFER nft-id MARMALADE_BANK buyer 1.0))
                (marmalade.ledger.transfer-create nft-id MARMALADE_BANK buyer (at 'guard (coin.details buyer)) 1.0)
                (format "Purchased a NFT with ID {} for {} KDA" [nft-id price])
            )
        )
    )

    (defun edit-market-price (account:string nft-id:string new-price:decimal)
        @doc "allows the owner of an NFT to edit the price of an NFT if it is already for sale"
        (with-read marketplace-table nft-id 
            { 'previous-owner := original-owner
            , 'previous-owner-guard := original-owner-guard }
            (let 
                (
                    (owner-guard (at 'guard (coin.details account))) 
                )
                (enforce (and (= original-owner-guard owner-guard ) (= account original-owner)) "You are not the original owner of this NFT")
            )
            (with-default-read marketplace-table nft-id
              { "price": 0.0 }
              { "price":= price }
                (if (!= 0.0 price) (update marketplace-table nft-id { "price": new-price } ) (format "NFT is not for sale, please list it on the marketplace before trying to edit the price"))
                (format "Price updated to {} kda" [new-price])
            )
        )
    )

    (defun get-num-founders-per-address (account:string)
        @doc "gets the number of founders that one k:address has minted"
        (let 
            (
                (owned-count (length (kmc-founders-policy.get-tokens-owned account)))
            )
            owned-count
        )
    )

; ============================================
; ==         FMarketplace Functions         ==
; ============================================

    (defun transfer-founder:string (receiver:string id:string)
        @doc "Transfer an NFT to another valid k:address"
        (emit-event (TRANSFER_FOUNDER id (kmc-founders-policy.get-owner id) receiver 1.0))
        (marmalade.ledger.transfer-create id (kmc-founders-policy.get-owner id) receiver (at 'guard (coin.details receiver)) 1.0)
        
    )

    (defun set-volume(value:decimal)
        @doc "Sets the price for a key to store in the price-table"
        (require-capability (PRIVATE))
        (update price-table TOTAL_VOLUME_KEY 
            {"price": value} 
        )
    )


; ============================================
; ==     Miner Marketplace Functions        ==
; ============================================

    (defun transfer-miner-multiple (ids:list receiver:string)
        @doc "transfer multiple miner NFTs to another valid k:address"
        (with-capability (PRIVATE)
            (map 
                (transfer receiver)
                ids
            )
        )
        (format "NFTs with id(s) {} transferred to address {}" [ids, receiver])
    )

    (defun transfer-miner:string (id:string sender:string receiver:string)
        @doc "Transfer a Miner NFT to another valid k:address"
        (enforce-coin-account-exists receiver)
        (emit-event (TRANSFER_MINER id sender receiver))
        (marmalade.ledger.transfer-create id (kmc-policy.get-owner id) receiver (at 'guard (coin.details receiver)) 1.0)
        
        (format "Miner with id #{} has been transferred to {}" [id receiver])
    )

    (defun transfer:string (receiver:string id:string)
        @doc "Transfer a Miner NFT to another valid k:address"
        (enforce-coin-account-exists receiver)
        (bind (marmalade.ledger.get-policy-info id)
            { 'policy := policy:module{free.kmc-token-policy-v13}}
            (marmalade.ledger.transfer-create id (policy::get-owner id) receiver (at 'guard (coin.details receiver)) 1.0))
        
        (format "KMC NFT with id #{} has been transferred to {}" [id receiver])
    )

    (defun get-owner-items-on-sale (owner:string)
        @doc "Returns a specific owner's items on sale"
        (select mledger ["nft-id", "market-price", "updated-at", "owner-address"] (and? (where "for-sale" (= true)) (where "owner-address" (= owner))))
    ) ;TODO check if necessary

    (defun create-simple-user-guard (funder:string amount:decimal account:string)
        (coin.transfer-create funder account 
          (create-BANK_DEBIT-guard) amount)
    )

    ;; Capability user guard: capability predicate function
    (defun require-BANK_DEBIT () 
        (require-capability (BANK_DEBIT))
    )
    
    ;; Capability user guard: guard constructor
    (defun create-BANK_DEBIT-guard ()
        (create-user-guard (require-BANK_DEBIT))
    )

    (defun reg-kmc-policy ()
        (with-capability (ADMIN)
            (kmc-policy.register-guard (create-capability-guard (CALL-POLICY-MODULES)))
        )
    )

    (defun reg-founders-policy ()
        (with-capability (ADMIN)
            (kmc-founders-policy.register-guard (create-capability-guard (CALL-POLICY-MODULES)))
        )
    )

    (defun reg-vial-policy ()
        (with-capability (ADMIN)
            (kmc-vial-policy.register-guard (create-capability-guard (CALL-POLICY-MODULES)))
        )
    )

; ============================================
; ==             CAPABILITIES               ==
; ============================================

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap DISCORD(account:string)
        @doc "Only allows the discord wallet to send information"
        (enforce (= account DISCORD_ADDRESS) "only the administrator discord wallet can call this function")
        (compose-capability (ACCOUNT_GUARD DISCORD_ADDRESS))
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defcap GOVERNANCE()
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
    )

    (defcap FOUNDERS_OWNER (account:string id:string)
        @doc "Enforces that an account owns the particular miner ID"
        (let
            (
                (nft-owner (at "owner-address" (read fledger id ["owner-address"])))
            )
            (enforce (= nft-owner account) "Account is not owner of the NFT")
                (compose-capability (ACCOUNT_GUARD account))
        )
    )

    (defcap MINER_OWNER (account:string id:string)
        @doc "Enforces that an account owns the particular miner ID"
        (let
            (
                (nft-owner (kmc-policy.get-owner id))
            )
            (enforce (= nft-owner account) "Account is not owner of the NFT")
                (compose-capability (ACCOUNT_GUARD account))
        )
    )

    (defcap MINT_FOUNDER (id:string account:string)
        @doc "Emitted event when an NFT is purchased"
        @event true
    )
    
    (defcap LIST_FOUNDER (nft-id:string owner:string price:decimal for-sale:bool)
        @doc " Emitted event when a Founders pass is sold "
        @event true
    )

    (defcap BUY_FOUNDER (nft-id:string buyer:string seller:string price:decimal)
      @doc " Emitted event when a Founders pass is purchased "
      @event true
    )

    (defcap TRANSFER_FOUNDER (id:string sender:string receiver:string amount:decimal)
        @doc "Allows transferring of NFTs between two accounts"
        @event true
    )

    (defcap MINT_MINER (id:string account:string)
        @doc "Emitted event when a Miner NFT is purchased"
        @event true
    )
    
    (defcap LIST_MINER (nft-id:string owner:string price:decimal for-sale:bool)
        @doc " Emitted event when a Miner is sold "
        @event true
    )

    (defcap BUY_MINER (nft-id:string buyer:string seller:string price:decimal)
      @doc " Emitted event when a Miner is purchased "
      @event true
    )

    (defcap TRANSFER_MINER (id:string sender:string receiver:string)
        @doc "Allows transferring of NFTs between two accounts"
        @event true
    )

    (defcap TURN_OFF_MINER (id:string)
        @doc "Emitted event when a Miner NFT is turned off"
        @event true
    )

    (defcap TURN_ON_MINER (id:string)
        @doc "Emitted event when a Miner NFT is turned on"
        @event true
    )

    (defcap AIRDROP (account:string)
        @doc "Emitted event when an airdrop to an account occurs"
        @event true
    )

    (defcap PLACE_COLLECTION_OFFER (amount:decimal id:string)
        @doc "Emitted when a user makes an offer on the KMC collection"
        @event true
    )

    (defcap REMOVE_COLLECTION_OFFER (id:string)
        @doc "Emitted when a user removes an offer on the collection"
        @event true
    )

    (defcap CLAIM (account:string amount:decimal)
        @doc "Emitted event when a user claims KDA from the hashrate wallet"
        @event true
    )

    (defcap PLACE_NFT_OFFER (amount:decimal offer-id:string nft-id:string)
        @doc "Emitted when a user makes an offer on a specific NFT"
        @event true
    )

    (defcap REMOVE_NFT_OFFER (amount:decimal offer-id:string nft-id:string)
        @doc "Emitted when a user removes an offer from a specific NFT"
        @event true
    )

    (defcap PRIVATE ()
        true
    )

    (defcap CALL-POLICY-MODULES () 
        true 
    )

    (defcap BANK_DEBIT () true)
)

; (create-table marketplace-table)



