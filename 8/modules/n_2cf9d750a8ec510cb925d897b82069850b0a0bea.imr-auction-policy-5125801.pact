(module imr-auction-policy GOVERNANCE
  @doc "Immutable Record auction contract"

  (implements kip.token-policy-v2)

  (defconst ADMIN-KS:string "n_2cf9d750a8ec510cb925d897b82069850b0a0bea.imr-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (defconst AUCTION-TYPE-CONVENTIONAL "conventional")
  (defconst AUCTION-TYPE-DUTCH "dutch")
  (defconst ROYALTY-SPEC-MSG-KEY "royalty_spec" @doc "Payload field for royalty spec")
  (defconst ARTIST-ROYALTY-SPEC-MSG-KEY "artist_royalty_spec" @doc "Payload field for royalty spec")

  (use marmalade-v2.policy-manager)
  (use marmalade-v2.policy-manager [QUOTE-MSG-KEY quote-spec quote-schema])
  (use kip.token-policy-v2 [token-info])

  ; === Schema and table definitions ===

  (defschema auctions-schema
    sale-id:string
    token-id:string
    start-date:integer
    end-date:integer
    highest-bid:decimal
    highest-bid-id:string
    reserve-price:decimal
    type:string
  )

  (defschema auction-identifiers-schema
    last-identifier:integer
  )

  (defschema bids-schema
    bidder:string
    bidder-guard:guard
    bid:decimal
    hash:string
  )

  (defschema identifier-map-schema
    auction-id:integer
  )

  (defschema royalty-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    royalty-rate:decimal
  )

  (defschema artist-royalty-schema
    artist:string
    artist-guard:guard
    royalty-cut:decimal
  )

  (defschema token-state-schema
    sold:bool
  )

  (deftable auctions:{auctions-schema})
  (deftable bids:{bids-schema})
  (deftable auction-identifiers:{auction-identifiers-schema})
  (deftable identifier-map:{identifier-map-schema})
  (deftable token-state:{token-state-schema})
  (deftable royalties:{royalty-schema})
  (deftable artist-royalties:{artist-royalty-schema})

  ; === Capabilities ===

  (defcap AUCTION_CREATED:bool
    ( sale-id:string
      auction-id:integer
      token-id:string
      type:string
    )
    @doc ""
    @event
    true
  )

  (defcap PLACE_BID:bool
    (bidder-guard:guard)
    @doc ""
    (enforce-guard bidder-guard)
  )

  (defcap BID_PLACED:bool
    ( bid-id:string
      bidder:string
      bidder-guard:guard
      bid:decimal
      token-id:string
    )
    @doc ""
    @event
    (enforce-guard bidder-guard)
  )

  (defcap SET_BID_HASH:bool (bid-id:string)
    (with-read bids bid-id { 'bidder-guard:= bidder-guard }
      (enforce-guard bidder-guard)
    )
  )

  (defcap PRICE_ACCEPTED:bool
    ( auction-id:integer
      bidder:string
      token-id:string
      price:decimal
    )
    @event
    true
  )

  (defcap ROYALTY:bool (token-id:string royalty_spec:object{royalty-schema})
    @doc "Emits event with royalty information for discovery"
    @event
    true
  )

  (defcap ROYALTY-PAYOUT:bool
    ( sale-id:string
      token-id:string
      royalty-payout:decimal
      creator:string
    )
    @doc "Emits event with royalty payout information at sale's completion"
    @event
    true
  )

  (defcap ESCROW_CAP:bool (sale-id:string) true)

  ; === Util functions ===

  (defun escrow-account:string (sale-id:string)
    (create-principal (escrow-guard sale-id))
  )

  (defun escrow-guard(sale-id:string)
    (create-capability-guard (ESCROW_CAP sale-id))
  )

  (defun curr-time:integer ()
    (floor (diff-time (at 'block-time (chain-data)) (time "1970-01-01T00:00:00Z")))
  )

  (defun get-royalty:object{royalty-schema} (token:object{token-info})
    (read royalties (at 'id token))
  )

  (defun is-token-sold:bool (token-id:string)
    (at 'sold (read token-state token-id))
  )

  (defun exists-artist-royalty-msg:bool (msg:string)
    @doc "Checks env-data field and see if the msg is a object"
    (let ((o:object (try {} (read-msg msg))))
      (!= o {}))
  )

  (defun has-artist-royalty:bool (token-id:string)
    (with-default-read artist-royalties token-id
      { 'royalty-cut: 0.0 }
      { 'royalty-cut := royalty-cut }
      (> royalty-cut 0.0)
    )
  )


  ; === token-policy-v2 implementation ===

  (defun enforce-init-royalty:bool (token:object{token-info})
    @doc "Required msg-data keys:                                                  \
    \ * royalty_spec:object{royalty-schema} - registers royalty information of \
    \ the created token"
    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) imr-auction-policy))
    (let* ( (spec:object{royalty-schema} (read-msg ROYALTY-SPEC-MSG-KEY))
            (fungible:module{fungible-v2} (at 'fungible spec))
            (creator:string (at 'creator spec))
            (creator-guard:guard (at 'creator-guard spec))
            (royalty-rate:decimal (at 'royalty-rate spec))
            (creator-details:object (fungible::details creator ))
            )
      (enforce (= (format "{}" [fungible]) (format "{}" [coin])) "Royalty support is restricted to coin")
      (enforce (= (at 'guard creator-details) creator-guard) "Creator guard does not match")
      (enforce (and (>= royalty-rate 0.0) (<= royalty-rate 1.0)) "Invalid royalty rate")
      (insert royalties (at 'id token)
        { 'fungible: fungible
        , 'creator: creator
        , 'creator-guard: creator-guard
        , 'royalty-rate: royalty-rate
        })

      (if (exists-artist-royalty-msg ARTIST-ROYALTY-SPEC-MSG-KEY)
        (let* (
          (artist-royalty-spec:object{artist-royalty-schema} (read-msg ARTIST-ROYALTY-SPEC-MSG-KEY))
          (artist:string (at 'artist artist-royalty-spec))
          (artist-guard:guard (at 'artist-guard artist-royalty-spec))
          (royalty-cut:decimal (at 'royalty-cut artist-royalty-spec))
          (artist-details:object (fungible::details artist)))

          (enforce (= (at 'guard artist-details) artist-guard) "Artist guard does not match")
          (enforce (and (>= royalty-cut 0.0) (<= royalty-cut 1.0)) "Invalid royalty cut")

          (insert artist-royalties (at 'id token)
            { 'artist: artist
            , 'artist-guard: artist-guard
            , 'royalty-cut: royalty-cut
            }))
        true
      )
      (emit-event (ROYALTY (at 'id token) spec)))
  )

  (defun enforce-init:bool (token:object{token-info})
    @doc "Executed at `create-token` step of marmalade.ledger.                 \
    \ Validates that policy can only be added by the governance keyset."
    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) imr-auction-policy))
    (with-capability (GOVERNANCE)
      (insert token-state (at "id" token) { "sold": false })
      (enforce-init-royalty token)
    )
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string
    )
    @doc "Executed at `offer` step of marmalade.ledger, invoked through the defpact `sale`       \
    \ Validates if the imr-auction conditions are met if the imr-key is provided"
    (require-capability (OFFER-CALL (at "id" token) seller amount sale-id timeout imr-auction-policy))

    (bind (get-royalty token)
      { 'fungible := fungible:module{fungible-v2} }
      (if (is-token-sold (at "id" token))
        (let* (
            (quote-spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
          )
          (enforce (exists-msg-quote QUOTE-MSG-KEY) "Offer is restricted to quoted sale")
          (enforce (= fungible (at 'fungible quote-spec)) (format "Offer is restricted to sale using fungible: {}" [fungible]))
        )
        (enforce (= false (exists-msg-quote QUOTE-MSG-KEY)) "Quote must not exist")
      )
    )
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string
    )
    @doc "Executed at `withdraw` step of marmalade.ledger, invoked through rollback of the defpact `sale`    \
    \ This is only applicable for the imr sale of the token"
    (require-capability (WITHDRAW-CALL (at "id" token) seller amount sale-id timeout imr-auction-policy))
    (if (is-token-sold (at "id" token))
      true
      (with-read identifier-map sale-id { 'auction-id:= auction-id }
        (with-read auctions (int-to-str 10 auction-id)
            { 'sale-id:= sale-id,
              'token-id:= token-id,
              'start-date:= start-date,
              'end-date:= end-date,
              'highest-bid:= highest-bid,
              'highest-bid-id:= highest-bid-id,
              'type:= type
            }

          (enforce (> (curr-time) end-date) "Auction is still ongoing or hasn't started yet")
          (enforce (= highest-bid 0.0) "Bid has been placed, can't withdraw")
        )
      )
    )
  )

  (defun enforce-buy-royalty:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (require-capability (BUY-CALL (at "id" token) seller buyer amount sale-id imr-auction-policy))
    (bind (get-royalty token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'royalty-rate:= royalty-rate:decimal
      }
      (let* ( (quote-spec:object{quote-schema} (get-quote-info sale-id))
              (sale-price:decimal (at 'sale-price quote-spec))
              (escrow-account:string (at 'account (marmalade-v2.policy-manager.get-escrow-account sale-id)))
              (royalty-payout:decimal (floor (* sale-price royalty-rate) (fungible::precision)))

              )
        (if
          (> royalty-payout 0.0)

          (if (has-artist-royalty (at "id" token))
            (with-read artist-royalties (at "id" token)
              { 'artist:= artist
              , 'artist-guard:= artist-guard
              , 'royalty-cut:= royalty-cut
              }
              (let* (
                (artist-payout:decimal (floor (* royalty-payout royalty-cut) (fungible::precision)))
                (creator-payout:decimal (- royalty-payout artist-payout))
                )
                (install-capability (fungible::TRANSFER escrow-account creator creator-payout))
                (emit-event (ROYALTY-PAYOUT sale-id (at 'id token) royalty-payout creator))
                (fungible::transfer escrow-account creator creator-payout)

                (install-capability (fungible::TRANSFER escrow-account artist artist-payout))
                (emit-event (ROYALTY-PAYOUT sale-id (at 'id token) artist-payout artist))
                (fungible::transfer escrow-account artist artist-payout)
              )
            )
            (let ((_ ""))
              (install-capability (fungible::TRANSFER escrow-account creator sale-price))
              (emit-event (ROYALTY-PAYOUT sale-id (at 'id token) royalty-payout creator))
              (fungible::transfer escrow-account creator royalty-payout)
            )
          )
          "No royalty"
          )))
        true)

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    @doc "Executed at `buy` step of marmalade.ledger, invoked through the defpact `sale`    \
    \ Funds are being transferred from escrow to seller"
    (require-capability (BUY-CALL (at "id" token) seller buyer amount sale-id imr-auction-policy))
    (enforce-sale-pact sale-id)
    (if (is-token-sold (at "id" token))
      (enforce-buy-royalty token seller buyer buyer-guard amount sale-id)
      (with-read identifier-map sale-id { 'auction-id:= auction-id }
        (with-read auctions (int-to-str 10 auction-id)
            { 'sale-id:= sale-id,
              'token-id:= token-id,
              'start-date:= start-date,
              'end-date:= end-date,
              'highest-bid:= highest-bid,
              'highest-bid-id:= highest-bid-id,
              'type:= type
            }
          (if (= type AUCTION-TYPE-CONVENTIONAL)
            (enforce (> (curr-time) end-date) "Auction is still ongoing")
            true
          )
          (enforce (> highest-bid 0.0) "No bids have been placed")
          (with-read bids highest-bid-id
            { 'bidder:= bidder,
              'bidder-guard:= bidder-guard,
              'bid:= bid
            }
            (enforce (= (read-msg "buyer") bidder) "Buyer does not match highest bidder")
            (enforce (= (read-msg "buyer-guard" ) bidder-guard) "Buyer-guard does not match highest bidder-guard")
          )
          ; Transfer amount from escrow account to seller
          (with-capability (ESCROW_CAP sale-id)
            (install-capability (coin.TRANSFER (escrow-account sale-id) seller highest-bid))
            (coin.transfer (escrow-account sale-id) seller highest-bid)
          )
          (write token-state (at "id" token) { "sold": true })
        )
      true
      )
    )
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    true
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce false "Burn prohibited")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal
    )
    (enforce false "Transfer prohibited")
  )

  ; === Auction implementation ===

  (defun validate-auction:bool (
    last-identifier:integer
    start-date:integer
    end-date:integer
    reserve-price:decimal
    type:string)

    (if (= 0 last-identifier)
      true
      (with-read auctions (int-to-str 10 last-identifier)
        { 'type:= prev-auction-type }
        (enforce (!= type prev-auction-type) "Auction types must be alternating"))
    )
    (enforce (> start-date (curr-time)) "Start date must be in the future")
    (enforce (> end-date start-date) "End date must be after start date")
    (enforce (> reserve-price 0.0) "Reserve price must be greater than 0")
  )

  (defun create-auction
    ( sale-id:string
    token-id:string
    start-date:integer
    end-date:integer
    reserve-price:decimal
    type:string )

    (let ((policies:[module{kip.token-policy-v2}] (at "policies" (marmalade-v2.ledger.get-token-info token-id))))
      (enforce (contains imr-auction-policy policies) "Invalid token, policy not attached")
    )

    (with-capability (GOVERNANCE)
      (with-read auction-identifiers ""
        { 'last-identifier:= last-identifier }
        (let ((auction-id:integer (+ last-identifier 1)))

          (validate-auction last-identifier start-date end-date reserve-price type)

          (insert auctions (int-to-str 10 auction-id) {
            "sale-id": sale-id
            ,"token-id": token-id
            ,"start-date": start-date
            ,"end-date": end-date
            ,"highest-bid": 0.0
            ,"highest-bid-id": ""
            ,"reserve-price": reserve-price
            ,"type": type
          })
          (write auction-identifiers "" { "last-identifier": auction-id })
          (insert identifier-map sale-id { "auction-id": auction-id })
          (emit-event (AUCTION_CREATED sale-id auction-id token-id type))
    )))
  )

  (defun update-auction
    ( auction-id:integer
    start-date:integer
    end-date:integer
    reserve-price:decimal
    type:string )

    (with-read auctions (int-to-str 10 auction-id)
      { 'start-date:= curr-start-date }
      (enforce (> curr-start-date (curr-time)) "Can't update auction after it has started")
    )

    (with-capability (GOVERNANCE)
      (update auctions (int-to-str 10 auction-id) {
        "start-date": start-date
        ,"end-date": end-date
        ,"reserve-price": reserve-price
        ,"type": type
      }))
  )

  (defun retrieve-auction (auction-id:integer)
    (read auctions (int-to-str 10 auction-id))
  )

  (defun retrieve-bid (bid-id:string)
    (read bids bid-id)
  )

  (defun get-bid-id:string (sale-id:string bidder:string)
    (hash [sale-id bidder (int-to-str 10 (curr-time))]))


  (defun place-bid:bool
    ( auction-id:integer
      bidder:string
      bidder-guard:guard
      bid:decimal)
    @model [
      (property (is-principal bidder))
    ]
    (with-read auctions (int-to-str 10 auction-id)
      { 'sale-id:= sale-id,
        'token-id:= token-id,
        'start-date:= start-date,
        'end-date:= end-date,
        'highest-bid:= prev-highest-bid,
        'highest-bid-id:= prev-highest-bid-id,
        'reserve-price:= reserve-price,
        'type:= type
      }
      (enforce (= type AUCTION-TYPE-CONVENTIONAL) "Only conventional auctions are supported")
      (enforce (> (curr-time) start-date) "Auction has not started yet")
      (enforce (< (curr-time) end-date) "Auction has ended")
      (enforce (> bid prev-highest-bid) "Bid is not higher than previous highest bid")
      (enforce (> bid reserve-price) "Bid is not higher than reserve price")
      (enforce (validate-principal bidder-guard bidder) "Incorrect account guard, only principal accounts allowed")
      (let ((bid-id:string (get-bid-id sale-id bidder)))
        (with-capability (PLACE_BID bidder-guard)
        ; Return amount to previous bidder if there was one
        (if (> prev-highest-bid 0.0)
          (with-read bids prev-highest-bid-id
            { 'bidder:= previous-bidder,
              'bidder-guard:= previous-bidder-guard
            }
            (with-capability (ESCROW_CAP sale-id)
              (install-capability (coin.TRANSFER (escrow-account sale-id) previous-bidder prev-highest-bid))
              (coin.transfer (escrow-account sale-id) previous-bidder prev-highest-bid)
            )
          )
          ""
        )

        ; Transfer amount from bidder to escrow-account
        (coin.transfer-create bidder (escrow-account sale-id) (escrow-guard sale-id) bid)

        ; Write new bid and store highest bid in auction
        (write bids bid-id {
          "bidder": bidder
          ,"bidder-guard": bidder-guard
          ,"bid": bid,
          "hash": ""
        })
        (update auctions (int-to-str 10 auction-id) { 'highest-bid: bid, 'highest-bid-id: bid-id })
        (emit-event (BID_PLACED bid-id bidder bidder-guard bid token-id))
      ))
      true
    )
  )

  (defun get-dutch-auction-starting-price:decimal (previous-auction-id:integer)
    (with-read auctions (int-to-str 10 previous-auction-id) {
      'highest-bid:= highest-bid,
      'reserve-price:= reserve-price
    }
      (if (= highest-bid 0.0)
        (* 2.0 reserve-price)
        (* 2.0 highest-bid)
      )
    )
  )

  (defun get-current-price:decimal (auction-id:integer)
    @doc "Returns the current price of a dutch auction"
    (with-read auctions (int-to-str 10 auction-id)
      { 'sale-id:= sale-id,
        'token-id:= token-id,
        'start-date:= start-date,
        'end-date:= end-date,
        'reserve-price:= reserve-price,
        'type:= type
      }
      (enforce (= type AUCTION-TYPE-DUTCH) "Only dutch auctions are supported")
      (let* (
        (sale-period-hours:decimal  (* (round (/ (- end-date start-date) 3600.0)) 1.0))
        (period-passed-hours:decimal (* (round (/ (- (curr-time) start-date) 3600.0)) 1.0))
        (previous-identifier:integer (- auction-id 1))
        (starting-price:decimal (get-dutch-auction-starting-price previous-identifier))
        (price-range:decimal (- starting-price reserve-price))
        )
        (if (or (< (curr-time) start-date) (> (curr-time) end-date))
          0.0
          (round (- starting-price (* (/ period-passed-hours sale-period-hours) price-range)) 2)
        )
      )
    )
  )

  (defun accept-price (auction-id:integer bidder:string bidder-guard:guard)
    (with-read auctions (int-to-str 10 auction-id)
      { 'sale-id:= sale-id,
        'token-id:= token-id,
        'start-date:= start-date,
        'end-date:= end-date,
        'highest-bid:= highest-bid,
        'type:= type
      }
      (enforce (= type AUCTION-TYPE-DUTCH) "Only dutch auctions are supported")
      (enforce (> (curr-time) start-date) "Auction has not started yet")
      (enforce (< (curr-time) end-date) "Auction has already ended")
      (enforce (= highest-bid 0.0) "Price has already been accepted")
      (enforce (validate-principal bidder-guard bidder) "Incorrect account guard, only principal accounts allowed")
      (let (
        (bid-id:string (get-bid-id sale-id bidder))
        (current-price:decimal (get-current-price auction-id)))
        ; Store bid
        (write bids bid-id {
          "bidder": bidder
          ,"bidder-guard": bidder-guard
          ,"bid": current-price
          ,"hash": ""
        })
        ; Update auction with highest bid
        (update auctions (int-to-str 10 auction-id) {
          "highest-bid": current-price
          ,"highest-bid-id": bid-id
        })

        ; Transfer amount from bidder to escrow-account
        (coin.transfer-create bidder (escrow-account sale-id) (escrow-guard sale-id) current-price)

        (emit-event (PRICE_ACCEPTED auction-id bidder token-id current-price))
      ))
  )

  (defun set-hash (bid-id:string hash:string)
    (with-capability (SET_BID_HASH bid-id)
      (update bids bid-id { "hash": hash })
    )
  )
)


