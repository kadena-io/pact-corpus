(module prod-marketplace GOVERNANCE

    (defcap GOVERNANCE ()
        (enforce-guard (keyset-ref-guard "hypercent.hyper-admin-marketplace-prod")))

    (defcap IS_ADMIN ()
        (enforce-keyset "hypercent.hyper-api-admin-prod"))

        (defcap REFUND () "Internal cap" true)
        (defcap TRANSFER () "Internal cap" true)
        (defcap INTERNAL_CLOSE_SALE () "Internal cap" true)
        
        (defcap OPEN_SALE (nft_id:string id:string type:string owner:string buy_now_price:decimal sale_start_days:integer sale_end_days:integer minimum_bid:decimal) @event true)
        (defcap CLOSE_SALE (nft_id:string id:string seller:string) @event true)
        (defcap CANCEL_BID (nft_id:string id:string account:string seller:string amount:decimal) @event true)
        (defcap DECLINE_BID (nft_id:string id:string account:string seller:string) @event true)
        (defcap BID (nft_id:string id:string buyer:string seller:string amount:decimal bid_expiration:time)  @event true)
        (defcap OUTBID (nft_id:string id:string account:string last_account:string amount:decimal) @event true)
        (defcap BUY (nft_id:string id:string buyer:string seller:string amount:decimal) @event true)
        (defcap ACCEPT_BID (nft_id:string id:string buyer:string seller:string amount:decimal) @event true)
        (defcap SALE_EXTENDED (nft_id:string id:string buyer:string seller:string) @event true)


        (defcap OWNER (account:string)
            (enforce-guard (at "guard" (coin.details account)))
        )

    
        (defconst NFT_BANK_ACCT:string (read-msg 'nft-bank))

        (defun nft-bank-guard () (create-module-guard "bank"))
    
        (defconst SERVICE_FEE:decimal 0.02)
    
        (defschema royalty-schema
            id:string
            percentage:decimal
            account:string
        )
    
        (deftable royalty:{royalty-schema})
    
        (defschema sale-schema
            id:string
            type:string
            owner:string
            closed:bool
            can_buy_now:bool
            buy_now_price:decimal
            sale_start_date:time
            sale_end_date:time
            last_offer:decimal
            last_offer_account:string
            offer_end_date:time
        )
        (deftable sale:{sale-schema})
    
        (defun now () (at "block-time" (chain-data)))
    
        (defun get-royalty (id:string)
            (with-default-read royalty id {"id":id, "percentage":0.0, "account":""} {"id":=id, "percentage":=percentage, "account":=account}
                {
                    "id":id,
                    "percentage":percentage,
                    "account":account
                }
            )
        )
    
        (defun set-royalty (id:string percentage:decimal account:string)
            (with-capability (IS_ADMIN)
                (write royalty id {"id":id, "percentage":percentage, "account":account})
            )
        )
    
        (defun get-sale (id:string)
                (read sale id)
        )
    
        (defun is-rotate-compatible (id:string)
            (not (and (= "t:" (take 2 id)) (< (length id) 30)))
        )


        (defun open-sale (type:string id:string owner:string buy_now_price:decimal sale_start_days:integer sale_end_days:integer minimum_bid:decimal)
            (with-default-read sale id {"closed": true} {"closed":= current_sale_closed}
                (enforce (= true current_sale_closed) "Previous sale not closed")
                (let* (
                                (owner_details (coin.details owner))
                                (token_details (marmalade.ledger.details id owner))
                                (can_buy_now (and (> buy_now_price 0.0) (!= type "time")))
                                (sale_id (hash [id, owner (now)]))
                            )
                            (enforce (> (at "balance" token_details) 0.0) "Token balance is 0")
                        
                            (if (= type "time")
                                (enforce (and (>= sale_end_days 1) (<= sale_end_days 7)) "Auction should last between 1 and 7 days")
                                "Not a time auction"
                            )

                            (if (is-rotate-compatible id)
                                (marmalade.ledger.rotate id owner (nft-bank-guard))
                                (marmalade.ledger.transfer-create id owner NFT_BANK_ACCT (nft-bank-guard) 1.0)
                            )
                            
                            (emit-event (OPEN_SALE id sale_id type owner buy_now_price sale_start_days sale_end_days minimum_bid))
                            (write sale id {
                                "closed": false, 
                                "id": sale_id, 
                                "type": type, 
                                "can_buy_now": can_buy_now,
                                "owner": owner,
                                "buy_now_price": (if (!= type "time") buy_now_price 0.0),
                                "last_offer": (if (= type "time") minimum_bid 0.0),
                                "last_offer_account": "",
                                "sale_start_date":(add-time (now) (days sale_start_days)),
                                "sale_end_date":(add-time (now) (days sale_end_days)),
                                "offer_end_date":(now)
                    })
                )
            )
        )
    
    
        (defun refund-bid (account:string amount:decimal)
            (require-capability (REFUND))
            (install-capability (coin.TRANSFER NFT_BANK_ACCT account (+ amount (* amount SERVICE_FEE))))
            (if (and (> amount 0.0) (!= account ""))
                    (coin.transfer NFT_BANK_ACCT account (+ amount (* amount SERVICE_FEE)))
                    "Nothing to refund "
            )
        )    
    
        (defun transfer (id:string owner:string receiver:string)
                (require-capability (TRANSFER))
                (let* (
                    (from (if (is-rotate-compatible id) owner NFT_BANK_ACCT))
                )
                    (install-capability (marmalade.ledger.TRANSFER id from receiver 1.0))
                    (marmalade.ledger.transfer-create id from receiver (at "guard" (coin.details receiver)) 1.0)
                ) 
        )
    
    
        (defun close-sale (id:string)
        (with-read sale id {"id":=sale_id, "type":=type, "owner":=sale_owner, "closed":=sale_closed, "last_offer_account":=last_offer_account, "last_offer":=last_offer, "sale_end_date":=sale_end_date}
                (enforce (= false sale_closed) "Sale already closed")
                (if 
                    (= "time" type)
                    [(enforce (> (now) sale_end_date) "Action not ended"), (enforce (= last_offer_account "") "Bid existing")]
                    (enforce-guard (at "guard" (coin.details sale_owner)))
                )
                (if (!= last_offer_account "")
                    (emit-event (DECLINE_BID id sale_id last_offer_account sale_owner))
                    "No bids"
                )
                (with-capability (REFUND)
                    (refund-bid last_offer_account last_offer)
                )
                (with-capability (INTERNAL_CLOSE_SALE)
                    (internal-close-sale sale_id id sale_owner)
                )
                
            )
        )
        (defun internal-close-sale-rotate (id:string sale_owner:string)
            (require-capability (INTERNAL_CLOSE_SALE))
            (install-capability (marmalade.ledger.ROTATE id sale_owner))
            (marmalade.ledger.rotate id sale_owner (at "guard" (coin.details sale_owner)))
        )

        (defun internal-close-sale-transfer (id:string sale_owner:string)
            (require-capability (INTERNAL_CLOSE_SALE))
            (install-capability (marmalade.ledger.TRANSFER id NFT_BANK_ACCT sale_owner 1.0))
            (if (= 1.0 (marmalade.ledger.get-balance id NFT_BANK_ACCT))
                (marmalade.ledger.transfer id NFT_BANK_ACCT sale_owner 1.0)
                ""
            )
        )


        (defun internal-close-sale (sale_id:string id:string sale_owner:string)
            (emit-event (CLOSE_SALE  id sale_id sale_owner))
            (require-capability (INTERNAL_CLOSE_SALE))
            (if (is-rotate-compatible id) (internal-close-sale-rotate id sale_owner) (internal-close-sale-transfer id sale_owner))
            (update sale id {"closed": true, "can_buy_now": false, "last_offer": 0.0, "last_offer_account": ""})
        ) 
        
    
    
        (defun accept-last-bid (id:string)
            (with-read sale id {"id":=sale_id, "type":=type, "closed":=closed, "owner":=sale_owner, "last_offer":=last_offer, "last_offer_account":=last_offer_account, "sale_end_date":=sale_end_date}
                (enforce (= false closed) "Sale closed")
                (enforce (and (!= "" last_offer_account) (> last_offer 0.0)) "No bid to accept")
                (if 
                    (= "time" type)
                    (enforce (> (now) sale_end_date) "Auction not ended")
                    (enforce-guard (at "guard" (coin.details sale_owner)))
                )
    
                (let* (
                    (last_offer_whithout_service_fees (- last_offer (* last_offer SERVICE_FEE)))
                    (royalty (get-royalty id))
                    (royalty_payout (* last_offer_whithout_service_fees (at "percentage" royalty)))
                    (seller_payout (-  last_offer_whithout_service_fees royalty_payout))
                    (royalty_account (at "account" royalty))
    
                )
                    
                
                    (if (= sale_owner royalty_account)
                        (install-capability (coin.TRANSFER NFT_BANK_ACCT (at "account" royalty) last_offer_whithout_service_fees))
                        (install-capability (coin.TRANSFER NFT_BANK_ACCT (at "account" royalty) royalty_payout))
                    )
                    (if (> royalty_payout 0.0) (coin.transfer NFT_BANK_ACCT (at "account" royalty) royalty_payout) "No royalty")
    
                   
                    (install-capability (coin.TRANSFER NFT_BANK_ACCT sale_owner seller_payout))
                    (coin.transfer NFT_BANK_ACCT sale_owner seller_payout)
                    (with-capability (TRANSFER)
                        (transfer id sale_owner last_offer_account)
                    )
    
                    (emit-event (ACCEPT_BID id sale_id last_offer_account sale_owner last_offer))
                    
                    (with-capability (INTERNAL_CLOSE_SALE)
                        (internal-close-sale sale_id id sale_owner)
                    )
                    
                )
            )
        )
    
        (defun buy (id:string buyer:string)
                (with-read sale id {"id":=sale_id, "closed":=closed, "can_buy_now":=can_buy_now, "buy_now_price":=buy_now_price, "owner":=owner, "last_offer":=last_offer, "last_offer_account":=last_offer_account}
                    (enforce (= false closed) "Sale is closed")
                    (enforce (= true can_buy_now) "Token not for sale")
                    (enforce (!= owner buyer) "Can't buy your own token")
                    (enforce (!= buyer NFT_BANK_ACCT) "Users only")
                    (let* (

                        (service_fees (* buy_now_price SERVICE_FEE))
                        (buy_now_price_whithout_service_fees (- buy_now_price service_fees))
                        (royalty (get-royalty id))
                        (royalty_account (at "account" royalty))
                        (royalty_payout (* buy_now_price_whithout_service_fees (at "percentage" royalty)))
                        (seller_payout (-  buy_now_price_whithout_service_fees royalty_payout))
                        (seller_service_fee (* seller_payout SERVICE_FEE))
                    )
                        (coin.transfer buyer NFT_BANK_ACCT (* 2 (* buy_now_price SERVICE_FEE)))
                        (coin.transfer buyer owner seller_payout)
                        
                        (if (and (> royalty_payout 0.0) (!= buyer royalty_account)) (coin.transfer buyer royalty_account royalty_payout) "")
                        (with-capability (TRANSFER)
                            (transfer id owner buyer)
                        )
                        (if (> last_offer 0.0)
                            (emit-event (OUTBID id sale_id buyer last_offer_account last_offer))
                            "No outbid"
                        )
                        (with-capability (REFUND)
                            (refund-bid last_offer_account last_offer)
                        )
                        (emit-event (BUY  id sale_id buyer owner buy_now_price))
                        (with-capability (INTERNAL_CLOSE_SALE)
                            (internal-close-sale sale_id id owner)
                        )
                    )
                )
        )
    
        (defun bid (id:string buyer:string amount:decimal bid_days:integer)            
            (with-read sale id {"id":=sale_id, "type":=type, "closed":=closed, "last_offer":=last_offer, "type":=type, "last_offer_account":=last_offer_account, "sale_end_date":=sale_end_date, "owner":=sale_owner}
                (enforce (!= buyer sale_owner) "Can't buy your own token")
                (enforce (= false closed) "Sale is closed")
                (enforce (> amount last_offer) "Amount lower than last offer")
                (enforce (!= buyer NFT_BANK_ACCT) "Users only")
                (if (= "time" type) (enforce (<= (now) sale_end_date) "Action ended") "")

                (coin.transfer buyer NFT_BANK_ACCT (+ amount (* amount SERVICE_FEE)))
                (if (and (> last_offer 0.0) (!= last_offer_account ""))
                    (emit-event (OUTBID id sale_id buyer last_offer_account last_offer))
                    "No outbid"
                )
                (with-capability (REFUND)
                    (refund-bid last_offer_account last_offer)
                )
                
                (let* (
                    (new_sale_end_date (if (= "time" type) (if ( > (add-time (now) (minutes 10)) sale_end_date)  (add-time sale_end_date (minutes 10)) sale_end_date) sale_end_date))
                    (offer_end_date (add-time (now) (days (if (< bid_days 3) 3 bid_days))))
                )
                    (emit-event (BID id sale_id buyer sale_owner amount offer_end_date))
                    (if (= sale_end_date new_sale_end_date)
                        "Sale not extended"
                        (emit-event (SALE_EXTENDED id sale_id buyer sale_owner))
                    )
                    (update sale id {"last_offer": amount, "last_offer_account": buyer, "sale_end_date": new_sale_end_date, "offer_end_date": offer_end_date})
                )
            )
        )
    
        (defun cancel-bid (id:string)
        (with-read sale id {"id":=sale_id, "closed":=closed,  "owner":=sale_owner, "last_offer":=last_offer, "type":=type, "last_offer_account":=last_offer_account, "offer_end_date":=offer_end_date}
                (enforce (!= "time" type) "Can't cancel auction bid")
                (enforce-guard (at "guard" (coin.details last_offer_account)))
                (enforce (> last_offer 0.0) "Bid already canceled")
                (enforce (> (now) offer_end_date) "Can't cancel bid yet")
                (update sale id {"last_offer": 0.0, "last_offer_account": ""})
                (emit-event (CANCEL_BID id sale_id last_offer_account sale_owner last_offer))
                (with-capability (REFUND)
                    (refund-bid last_offer_account last_offer)
                )
            )
        )

        (defun decline-bid (id:string)
            (with-read sale id {"id":=sale_id, "closed":=closed, "last_offer":=last_offer, "type":=type, "last_offer_account":=last_offer_account, "offer_end_date":=offer_end_date, "owner":=sale_owner}
                (enforce-guard (at "guard" (coin.details sale_owner)))
                (enforce (!= "time" type) "Can't decline auction bid")
                (enforce (> last_offer 0.0) "Bid already canceled")
                (update sale id {"last_offer": 0.0, "last_offer_account": ""})
                (emit-event (DECLINE_BID id sale_id last_offer_account sale_owner))
                (with-capability (REFUND)
                    (refund-bid last_offer_account last_offer)
                )
            )
        )
    

        (defun buy-now-payout (id:string)
            (let* (
                (sale (get-sale id))
                (royalty (get-royalty id))
                (buy_now_price (at "buy_now_price" sale))
                (service_fees (* buy_now_price SERVICE_FEE))
                (buy_now_price_whithout_service_fees (- buy_now_price service_fees))
                (royalty_payout (* buy_now_price_whithout_service_fees (at "percentage" royalty)))
                (seller_payout (-  buy_now_price royalty_payout))
                (seller_service_fee (* seller_payout SERVICE_FEE))
            )
                {
                    "service_account": NFT_BANK_ACCT,
                    "service_amount": (* 2 (* buy_now_price SERVICE_FEE)),
                    "royalty_account": (at "account" royalty),
                    "royalty_amount": royalty_payout,
                    "owner_account": (at "owner" sale),
                    "owner_amount": (- seller_payout seller_service_fee)
                }
            )
        )

    
        (defun init ()
          (coin.create-account NFT_BANK_ACCT (nft-bank-guard))
          "empty lock succeeded"
        )

    

  
)



