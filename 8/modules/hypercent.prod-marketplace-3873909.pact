(module prod-marketplace GOVERNANCE

    (defcap GOVERNANCE ()
        (enforce-guard (keyset-ref-guard "hypercent.hyper-admin-marketplace-prod")))

    (defcap IS_ADMIN ()
        (enforce-keyset "hypercent.hyper-api-admin-prod"))

        (defcap REFUND () "Internal cap" true)
        (defcap TRANSFER () "Internal cap" true)
        (defcap INTERNAL_CLOSE_SALE () "Internal cap" true)
        
        (defcap OPEN_SALE (nft_id:string id:string type:string owner:string buy_now_price:decimal sale_start_days:integer sale_end_days:integer) @event true)
        (defcap CLOSE_SALE (nft_id:string id:string) @event true)
        (defcap CANCEL_BID (nft_id:string id:string account:string amount:decimal) @event true)
        (defcap DECLINE_BID (nft_id:string id:string account:string) @event true)
        (defcap BID (nft_id:string id:string buyer:string amount:decimal bid_expiration:time)  @event true)
        (defcap OUTBID (nft_id:string id:string account:string last_account:string amount:decimal) @event true)
        (defcap BUY (nft_id:string id:string buyer:string seller:string amount:decimal) @event true)
        (defcap ACCEPT_BID (nft_id:string id:string buyer:string seller:string amount:decimal) @event true)
    
        (defcap OWNER (account:string)
            (enforce-guard (at "guard" (coin.details account)))
        )
    
        (defconst NFT_BANK_ACCT:string (read-msg 'nft-bank))
        (defun nft-bank-guard () (create-module-guard "bank"))
    
        (defconst SERVICE_FEE:decimal 0.01)
    
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
    
        (defun open-sale (type:string id:string owner:string buy_now_price:decimal sale_start_days:integer sale_end_days:integer)
            (with-default-read sale id {"closed": true} {"closed":= current_sale_closed}
                (enforce (= true current_sale_closed) "Previous sale not closed")
                (let* (
                                (owner_details (coin.details owner))
                                (token_details (marmalade.ledger.details id owner))
                                (can_buy_now (> buy_now_price 0.0))
                                (sale_id (hash [id, owner (now)]))
                            )
                            (enforce (> (at "balance" token_details) 0.0) "Token balance is 0")
                        
    
                            (install-capability (marmalade.ledger.ROTATE id owner))
                            (marmalade.ledger.rotate id owner (nft-bank-guard))
                            (emit-event (OPEN_SALE id sale_id type owner buy_now_price sale_start_days sale_end_days))
                            (write sale id {
                                "closed": false, 
                                "id": sale_id, 
                                "type": type, 
                                "can_buy_now": can_buy_now,
                                "owner": owner,
                                "buy_now_price": buy_now_price,
                                "last_offer":0.0,
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
            (install-capability (coin.TRANSFER NFT_BANK_ACCT account amount))
            (if (> amount 0.0)
                    (coin.transfer NFT_BANK_ACCT account amount)
                    "Nothing to refund "
            )
        )    
    
        (defun transfer (id:string owner:string receiver:string)
            (require-capability (TRANSFER))
                (install-capability (marmalade.ledger.TRANSFER id owner receiver 1.0))
                (marmalade.ledger.transfer-create id owner receiver (at "guard" (coin.details receiver)) 1.0)
            
        )
    
    
        (defun close-sale (id:string)
            (with-read sale id {"id":=sale_id, "owner":=sale_owner, "closed":=sale_closed, "last_offer_account":=last_offer_account, "last_offer":=last_offer}
                (enforce (= false sale_closed) "Sale already closed")
                (enforce-guard (at "guard" (coin.details sale_owner)))
                (if (> last_offer 0.0)
                    (emit-event (DECLINE_BID id sale_id last_offer_account))
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
        (defun internal-close-sale (sale_id:string id:string sale_owner:string)
            (emit-event (CLOSE_SALE  id sale_id))
            (require-capability (INTERNAL_CLOSE_SALE))
            (install-capability (marmalade.ledger.ROTATE id sale_owner))
            (marmalade.ledger.rotate id sale_owner (at "guard" (coin.details sale_owner)))
            (update sale id {"closed": true, "last_offer": 0.0, "last_offer_account": ""})
        )
        
    
    
        (defun accept-last-bid (id:string)
            (with-read sale id {"id":=sale_id, "closed":=closed, "owner":=sale_owner, "last_offer":=last_offer, "last_offer_account":=last_offer_account}
                (enforce (= false closed) "Sale closed")
                (enforce-guard (at "guard" (coin.details sale_owner)))
    
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
                    (let* (
                        (service_fees (* buy_now_price SERVICE_FEE))
                        (buy_now_price_whithout_service_fees (- buy_now_price service_fees))
                        (royalty (get-royalty id))
                        (royalty_payout (* buy_now_price_whithout_service_fees (at "percentage" royalty)))
                        (seller_payout (-  buy_now_price_whithout_service_fees (at "percentage" royalty)))
                    )
                        (coin.transfer buyer NFT_BANK_ACCT service_fees)
                        (coin.transfer buyer owner seller_payout)
                        (if (> 0.0 royalty_payout) (coin.transfer buyer (at "account" royalty) royalty_payout) "")
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
            (with-read sale id {"id":=sale_id, "closed":=closed, "last_offer":=last_offer, "type":=type, "last_offer_account":=last_offer_account, "sale_end_date":=sale_end_date, "owner":=sale_owner}
                (enforce (!= buyer sale_owner) "Can't buy your own token")
                (enforce (= false closed) "Sale is closed")
                (enforce (> amount last_offer) "Amount lower than last offer")
                (coin.transfer buyer NFT_BANK_ACCT amount)
                (if (> last_offer 0.0)
                    (emit-event (OUTBID id sale_id buyer last_offer_account last_offer))
                    "No outbitd"
                )
                (with-capability (REFUND)
                    (refund-bid last_offer_account last_offer)
                )
                
                (let* (
                    (new_sale_end_date (if (= "time" type) (if ( > (add-time (now) (minutes 10)) (time sale_end_date))  (add-time (time sale_end_date) (minutes 10)) (time sale_end_date)) sale_end_date))
                    (offer_end_date (add-time (now) (days (if (< bid_days 3) 3 bid_days))))
                )
                    (emit-event (BID id sale_id buyer amount offer_end_date))
                    (update sale id {"last_offer": amount, "last_offer_account": buyer, "sale_end_date": new_sale_end_date, "offer_end_date": offer_end_date})
                )
            )
        )
    
        (defun cancel-bid (id:string)
            (with-read sale id {"id":=sale_id, "closed":=closed, "last_offer":=last_offer, "type":=type, "last_offer_account":=last_offer_account, "offer_end_date":=offer_end_date}
                (enforce-guard (at "guard" (coin.details last_offer_account)))
                (enforce (> last_offer 0.0) "Bid already canceled")
                (enforce (> (now) offer_end_date) "Can't cancel bid yet")
                (update sale id {"last_offer": 0.0, "last_offer_account": ""})
                (emit-event (CANCEL_BID id sale_id last_offer_account last_offer))
                (with-capability (REFUND)
                    (refund-bid last_offer_account last_offer)
                )
            )
        )

        (defun decline-bid (id:string)
            (with-read sale id {"id":=sale_id, "closed":=closed, "last_offer":=last_offer, "type":=type, "last_offer_account":=last_offer_account, "offer_end_date":=offer_end_date, "owner":=sale_owner}
                (enforce-guard (at "guard" (coin.details sale_owner)))
                (enforce (> last_offer 0.0) "Bid already canceled")
                (update sale id {"last_offer": 0.0, "last_offer_account": ""})
                (emit-event (DECLINE_BID id sale_id last_offer_account))
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
                (seller_payout (-  buy_now_price_whithout_service_fees (at "percentage" royalty)))
            )
                {
                    "service_account": NFT_BANK_ACCT,
                    "service_amount": service_fees,
                    "royalty_account": (at "account" royalty),
                    "royalty_amount": royalty_payout,
                    "owner_account": (at "owner" sale),
                    "owner_amount": seller_payout
                }
            )
        )

    
        (defun init ()
          (coin.create-account NFT_BANK_ACCT (nft-bank-guard))
          "empty lock succeeded"
        )

    

  
)



