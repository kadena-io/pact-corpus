(module nostradamus GOVERNANCE "Nostradamus"
; ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⣀⣀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⠀⠀⠀⣀⣴⠾⠛⠋⠉⠉⠉⠉⢙⣿⣶⣤⡀⠀⠀⠀⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⠀⢀⣼⠟⠁⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣦⡀⠀⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⢠⣿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⢿⣿⣿⣿⡟⣷⡀⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⣾⢇⣤⣶⣶⣦⣤⣀⠀⠀⠀⠀⠀⠀⠙⠛⠛⠁⢹⣇⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣷⣤⡀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⡄⠀⠀⠀⠀⠀⠀⢸⡏⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⠘⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡀⠀⠀⠀⠀⢠⡿⠁⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⠀⠈⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠇⠀⠀⢀⣴⠟⠁⠀⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⠀⣠⣤⡙⠻⢿⣿⣿⣿⣿⣿⣋⣠⣤⡶⠟⢁⣤⡄⠀⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⠀⢿⣿⣿⣷⣤⣈⣉⠉⠛⠛⠉⣉⣠⣤⣾⣿⣿⡟⠀⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⣾⣦⣀⠙⠻⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⠟⢋⣠⣴⣷⠀⠀⠀⠀
;⠀⠀⠀⠀⢿⣿⣿⣿⣷⣶⣤⣬⣭⣉⣉⣉⣩⣭⣥⣤⣶⣾⣿⣿⣿⡿⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⠙⠻⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠟⠋⠀⠀⠀⠀⠀
;⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠛⠛⠛⠛⠛⠛⠛⠋⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀
;Nostradamus Prediction / Betting Platform
;Simple 2 option betting contract v1.0
;Users bet over events created and concluded by admins
;Each event has 2 options users can bet over
;Winners win back their original bet and their proportional share of the losers pot
;5% of winnings go to the nostradamus treasury
;Created by squiegee

    ;;;;; CAPABILITIES ;;;;;;;;;;;;;;

    (defcap GOVERNANCE ()
      @doc "Verifies Contract Governance"
      (enforce-keyset "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.admin")
    )

    (defcap ACCOUNT_GUARD(account:string)
        @doc "Verifies Account Existence"
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defcap ADMIN_GUARD(account:string)
        @doc "Verifies account belongs is admin"
        (let*
                (
                    (admins (read admins-table "ADMINS"))
                    (admin-ids (at "admin_ids" admins))
                )
                (enforce (= (contains account admin-ids) true) "Access not granted")
        )
    )

    (defcap ADD_TREASURY_ACCOUNT
      (token_string:string)
      true
    )

    (defcap CAN_ADD_TREAASURY_ACCOUNT
      (token_string:string)
          (compose-capability (ADD_TREASURY_ACCOUNT token_string))
    )

    ;;;;;;;;;; EVENTS ;;;;;;;;;;;;;;

    (defcap NOSTRADAMUS_NEW_EVENT (event_id:string event_title:string event_end_time:time)
      @doc " A new event is available for prediction "
      @event true
    )

    (defcap NOSTRADAMUS_NEW_BET (event_id:string event_title:string prediction:string amount:decimal token:module{fungible-v2})
      @doc " Someone just made a new prediction "
      @event true
    )

    (defcap NOSTRADAMUS_EVENT_ENDED (event_id:string side_1_wins:bool side_2_wins:bool)
      @doc " An event has concluded "
      @event true
    )

    (defcap NOSTRADAMUS_WINNINGS_CLAIMED (event_id:string account_id:string token:module{fungible-v2} amount:decimal fee:decimal)
      @doc " A winner has claimed their winnings "
      @event true
    )

    ;;;;;;;;;; GUARDS ;;;;;;;;;;;;;;

    (defcap PRIVATE_RESERVE
          (id:string account:string)
        true)

    (defun enforce-private-reserve:bool
        (id:string account:string)
      (require-capability (PRIVATE_RESERVE id account)))

    (defun create-pool-guard:guard
        (id:string account:string)
      (create-user-guard (enforce-private-reserve id account)))

    (defun get-account-principal (id:string account:string)
     (create-principal (create-pool-guard id account))
    )

    ;;;;;;;;;; SCHEMAS AND TABLES ;;;;;;;;;;;;;;

    (defschema event-schema
      event_id:string
      event_title:string
      event_description:string
      event_rules:[string]
      event_side_1_name:string
      event_side_2_name:string
      event_image_1:string
      event_image_2:string
      event_token:module{fungible-v2}
      event_pool:string
      event_side_1_users:integer
      event_side_2_users:integer
      event_side_1_amount:decimal
      event_side_2_amount:decimal
      event_end_time:time
      event_concluded:bool
      event_concluded_description:string
      event_side_1_wins:bool
      event_side_2_wins:bool
      event_refunded:bool
    )

    (defschema admins-schema
      admin_ids:[string]
    )

    (defschema fee-bank-schema
      account_id:string
      token:module{fungible-v2}
      amount:decimal)

    (defschema bet-record-schema
      account:string
      total_amount:decimal
      side_1_amount:decimal
      side_2_amount:decimal
      token:module{fungible-v2}
      event_id:string
      claimed_winnings:bool)

    ;;;;;;;;;;TABLES;;;;;;;;;;

    (deftable events-table:{event-schema})
    (deftable admins-table:{admins-schema})
    (deftable bet-record-table:{bet-record-schema})
    (deftable bank-fee-table:{fee-bank-schema})

    ;;;;;;;;;;;FUNCTIONS;;;;;

    (defun init:string()
      (with-capability (GOVERNANCE)
        (create-admin "k:5c4b5dbbd0a16f18297f3bbeef5c5facd636a2d409887557f8718b419ccc5fee")
        (create-admin "k:52d60e26c1750f19f65eda20e27cb7391af54dd9268d29ca4b05a6e982d6d214")
        (create-admin "k:c8b768b409247fd6a459e6f7af782c955cd274c10a5b585fd5f4798b7192ef7d")
        (create-admin "k:3b148ed63c6ab9ec9b477f71201017fd81dcae20aa3ca36c68d7e988de31e2ee")
      )
    )

    (defun create-treasury-account:string(token:module{fungible-v2})
      @doc " Create a treasury account "
          (require-capability (ADD_TREASURY_ACCOUNT (get-token-key token)))
          (with-default-read bank-fee-table (get-token-key token)
              { "account_id" : "null" }
              { "account_id" := treasury_account_id }
              (if (= treasury_account_id "null")
                [
                  ;Create token account
                  (token::create-account (get-account-principal (get-token-key token) (get-token-key token)) (create-pool-guard (get-token-key token) (get-token-key token)))
                  ;Insert record of new account
                  (insert bank-fee-table (get-token-key token) {
                      "account_id": (get-account-principal (get-token-key token) (get-token-key token)),
                      "token": token,
                      "amount": 0.0
                  })
                ]
                true
              )
          )
          (format "Created new treasury account for token {}" [token])
    )

    (defun withdraw-treasury:string(account_id:string token:module{fungible-v2} amount:decimal)
      @doc " Withdraw treasury tokens "
          (with-capability (ACCOUNT_GUARD account_id)
            (with-capability (ADMIN_GUARD account_id)
              ;Transfer tokens from treasury
              (install-capability (token::TRANSFER (get-account-principal (get-token-key token) (get-token-key token)) account_id amount))
              (with-capability (PRIVATE_RESERVE (get-token-key token) (get-token-key token)) (token::transfer (get-account-principal (get-token-key token) (get-token-key token)) account_id amount))
              ;Record updated amount to treasury records
              (update bank-fee-table (get-token-key token)
                  {
                      "amount": (- (at "amount" (read bank-fee-table (get-token-key token))) amount)
                  }
              )
              ;Return message
              (format "Withdrew {} {} from treasury" [amount token])
            )
          )
    )

    (defun create-admin:string
      (new_member_id:string)
        @doc " Create a new Admin "
          (with-capability (GOVERNANCE)
            (with-default-read admins-table "ADMINS"
              { "admin_ids" : [] }
              { "admin_ids" := t-admin-ids}
              (if (= (contains new_member_id t-admin-ids) false)
                (write admins-table "ADMINS"
                  {
                      "admin_ids": (+ [new_member_id] t-admin-ids )
                  }
                )
                true
              )
            )
            (format "Added new admin {}" [new_member_id])
          )
    )

    (defun create-event:string
      (
        account_id:string
        event_title:string
        event_description:string
        event_side_1_name:string
        event_side_2_name:string
        event_image_1:string
        event_image_2:string
        event_rules:[string]
        event_end_time:time
        event_token:module{fungible-v2}
      )
        @doc " Create a prediction event "
          (with-capability (ACCOUNT_GUARD account_id)
            (with-capability (ADMIN_GUARD account_id)

            (let*
                (
                  (event_id:string (create-id-key account_id event_title))
                  (event_pool:string (get-account-principal event_id (get-token-key event_token)))
                )
                ;Create a treasury pool to hold fee tokens
                (with-capability (CAN_ADD_TREAASURY_ACCOUNT (get-token-key event_token))
                  (create-treasury-account event_token)
                )

                ;Create a pool to hold tokens
                (event_token::create-account (get-account-principal event_id (get-token-key event_token)) (create-pool-guard event_id (get-token-key event_token)))

                ;Create event
                (insert events-table event_id
                  {
                      "event_id": event_id,
                      "event_title": event_title,
                      "event_description": event_description,
                      "event_rules": event_rules,
                      "event_side_1_name": event_side_1_name,
                      "event_side_2_name": event_side_2_name,
                      "event_image_1": event_image_1,
                      "event_image_2": event_image_2,
                      "event_token": event_token,
                      "event_pool": event_pool,
                      "event_side_1_users": 0,
                      "event_side_2_users": 0,
                      "event_side_1_amount": 0.0,
                      "event_side_2_amount": 0.0,
                      "event_end_time": event_end_time,
                      "event_concluded": false,
                      "event_concluded_description": "event not concluded",
                      "event_side_1_wins": false,
                      "event_side_2_wins": false,
                      "event_refunded": false
                  }
                )

                ;Emit event
                (emit-event (NOSTRADAMUS_NEW_EVENT event_id event_title event_end_time))

                ;Return final string message
                (format "Created new event {}" [event_id])
            )
          )
       )
    )

    (defun conclude-event:string
      (
        account_id:string
        event_id:string
        event_side_1_wins:bool
        event_side_2_wins:bool
        event_concluded_description:string
      )
        @doc " Conclude an event "
          (with-capability (ACCOUNT_GUARD account_id)
            (with-capability (ADMIN_GUARD account_id)

            (let*
                (
                  (event-data:object{event-schema} (read events-table event_id))
                  (event_end_time:time (at "event_end_time" event-data))
                  (event_concluded:bool (at "event_concluded" event-data))
                  (event_refunded:bool (at "event_refunded" event-data))
                )

                ;Enforce rules
                (enforce (= event_concluded false) "This event has concluded.")
                (enforce (= event_refunded false) "This event has refunded.")
                (enforce (!= event_side_1_wins event_side_2_wins) "Pick a winning/losing side.")

                ;Conclude event
                (update events-table event_id
                  {
                      "event_concluded": true,
                      "event_side_1_wins": event_side_1_wins,
                      "event_side_2_wins": event_side_2_wins,
                      "event_concluded_description": event_concluded_description
                  }
                )

            )

            ;Emit event
            (emit-event (NOSTRADAMUS_EVENT_ENDED event_id event_side_1_wins event_side_2_wins))

            ;Return message
            (format "Concluded event {}" [event_id])

          )
       )
    )

    (defun refund-event:string
      (
        account_id:string
        event_id:string
        event_concluded_description:string
      )
        @doc " Refund an event "
          (with-capability (ACCOUNT_GUARD account_id)
            (with-capability (ADMIN_GUARD account_id)

            (let*
                (
                  (event-data:object{event-schema} (read events-table event_id))
                  (event_concluded:bool (at "event_concluded" event-data))
                  (event_refunded:bool (at "event_refunded" event-data))
                )

                ;Enforce rules
                (enforce (= event_concluded false) "This event has concluded.")
                (enforce (= event_refunded false) "This event has refunded.")

                ;Conclude event
                (update events-table event_id
                  {
                      "event_refunded": true,
                      "event_concluded_description": event_concluded_description
                  }
                )

            )

            ;Emit event
            (emit-event (NOSTRADAMUS_EVENT_ENDED event_id false false))

            ;Return message
            (format "Refunded event {}" [event_id])

          )
       )
    )

    (defun place-bet:string
      (account_id:string event_id:string side_1:bool side_2:bool amount:decimal)
        @doc " Make a prediction "
        (with-capability (ACCOUNT_GUARD account_id)
            (let*
                (
                    (event-data:object{event-schema} (read events-table event_id))
                    (event_title:string (at "event_title" event-data))
                    (event_pool:string (at "event_pool" event-data))
                    (event_token:module{fungible-v2} (at "event_token" event-data))
                    (event_end_time:time (at "event_end_time" event-data))
                    (event_concluded:bool (at "event_concluded" event-data))
                    (event_refunded:bool (at "event_refunded" event-data))
                    (event_side_1_users:integer (at "event_side_1_users" event-data))
                    (event_side_2_users:integer (at "event_side_2_users" event-data))
                    (event_side_1_name:string (at "event_side_1_name" event-data))
                    (event_side_2_name:string (at "event_side_2_name" event-data))
                    (event_side_1_amount:decimal (at "event_side_1_amount" event-data))
                    (event_side_2_amount:decimal (at "event_side_2_amount" event-data))
                    (user_guard:guard (at "guard" (coin.details account_id)))
                    (treasury_data:object{fee-bank-schema} (read bank-fee-table (get-token-key event_token)))
                    (treasury_amount:decimal (at "amount" treasury_data))
                )

                ;Enforce rules
                (enforce (> amount 0.0) "Can only deposit positive amounts")
                (enforce-unit amount 0)
                (enforce (> (diff-time event_end_time (at "block-time" (chain-data))) 0.0 ) "This event has ended.")
                (enforce (= event_concluded false) "This event has concluded.")
                (enforce (= event_refunded false) "This event has concluded.")
                (enforce (!= side_1 side_2) "Pick a side.")
                (enforce (validate-principal user_guard account_id) (format "Reserved protocol guard violation: {}" [(typeof-principal account_id)]))

                ;Create a record of the bet amount
                (with-default-read bet-record-table (get-2-key event_id account_id)
                  { "total_amount" : 0.0, "claimed_winnings": false, "side_1_amount": 0.0, "side_2_amount": 0.0 }
                  { "total_amount" := t-user-bet-record-total-amount:decimal, "claimed_winnings" := t-user-claimed-winnings:bool, "side_1_amount" := t-user-bet-record-side_1_amount:decimal, "side_2_amount" := t-user-bet-record-side_2_amount:decimal}

                  ;Enforce rule
                  (enforce (= t-user-claimed-winnings false) "This event has concluded.")

                  ;Transfer and record a bet on side 1
                  (if (= side_1 true)
                            [

                            ;Transfer to side 1
                            (event_token::transfer account_id event_pool amount)
                            ;If this is a new user, +1 user count side 1
                            (if (= t-user-bet-record-total-amount 0.0)
                              [(update events-table event_id
                                  {
                                      "event_side_1_users": (+ 1 event_side_1_users)
                                  }
                              )]
                              true
                            )

                            ;Record amount added to side 1
                            (update events-table event_id
                                {
                                    "event_side_1_amount": (+ amount event_side_1_amount)
                                }
                            )

                            ;Record amount to treasury
                            (update bank-fee-table (get-token-key event_token)
                                {
                                    "amount": (+ amount treasury_amount)
                                }
                            )

                            ;Record a record of the users bet
                            (write bet-record-table (get-2-key event_id account_id)
                                {
                                    "account": account_id,
                                    "total_amount": (+ amount t-user-bet-record-total-amount),
                                    "side_1_amount": (+ amount t-user-bet-record-side_1_amount),
                                    "side_2_amount": t-user-bet-record-side_2_amount,
                                    "token": event_token,
                                    "event_id": event_id,
                                    "claimed_winnings": false
                                }
                            )

                            ;Emit Event
                            (emit-event (NOSTRADAMUS_NEW_BET event_id event_title event_side_1_name amount event_token))

                            ]
                            true
                  )

                  ;Transfer to side 2 & count user
                  (if (= side_2 true)
                            [

                            ;Transfer to side 2
                            (event_token::transfer account_id event_pool amount)

                            ;If this is a new user, +1 user count side 2
                            (if (= t-user-bet-record-total-amount 0.0)
                              [(update events-table event_id
                                  {
                                      "event_side_2_users": (+ 1 event_side_2_users)
                                  }
                              )]
                              true
                            )

                            ;Record amount added to side 2
                            (update events-table event_id
                                {
                                    "event_side_2_amount": (+ amount event_side_2_amount)
                                }
                            )

                            ;Record amount to treasury
                            (update bank-fee-table (get-token-key event_token)
                                {
                                    "amount": (+ amount treasury_amount)
                                }
                            )

                            ;Record a record of the users bet
                            (write bet-record-table (get-2-key event_id account_id)
                                {
                                    "account": account_id,
                                    "total_amount": (+ amount t-user-bet-record-total-amount),
                                    "side_1_amount": t-user-bet-record-side_1_amount,
                                    "side_2_amount": (+ amount t-user-bet-record-side_2_amount),
                                    "token": event_token,
                                    "event_id": event_id,
                                    "claimed_winnings": false
                                }
                            )

                            ;Emit Event
                            (emit-event (NOSTRADAMUS_NEW_BET event_id event_title event_side_2_name amount event_token))

                            ]
                            true
                  )

                )

                ;Return message depending on side purchased
                (if (= side_1 true)
                        (format "Purchased {} {} for event {}" [amount event_side_1_name event_id])
                        (format "Purchased {} {} for event {}" [amount event_side_2_name event_id])
                )

            )


        )
    )

    (defun claim-winnings:string
      (account_id:string event_id:string)
        @doc " Claim winnings from a concluded event "
        (with-capability (ACCOUNT_GUARD account_id)
            (let*
                (
                    (event-data:object{event-schema} (read events-table event_id))
                    (event_pool:string (at "event_pool" event-data))
                    (event_token:module{fungible-v2} (at "event_token" event-data))
                    (event_end_time:time (at "event_end_time" event-data))
                    (event_concluded:bool (at "event_concluded" event-data))
                    (event_refunded:bool (at "event_refunded" event-data))
                    (event_side_1_amount:decimal (at "event_side_1_amount" event-data))
                    (event_side_2_amount:decimal (at "event_side_2_amount" event-data))
                    (event_side_1_wins:bool (at "event_side_1_wins" event-data))
                    (event_side_2_wins:bool (at "event_side_2_wins" event-data))
                    (bet-record-data:object{bet-record-schema} (read bet-record-table (get-2-key event_id account_id)))
                    (user_claimed_winnings:bool (at "claimed_winnings" bet-record-data))
                    (user_side_1_amount:decimal (at "side_1_amount" bet-record-data))
                    (user_side_2_amount:decimal (at "side_2_amount" bet-record-data))
                    (user_total_amount:decimal (at "total_amount" bet-record-data))
                )

                ;Enforce rules
                (enforce (= event_concluded true) "This event has not concluded.")
                (enforce (= event_refunded false) "This event has been refunded.")
                (enforce (= user_claimed_winnings false) "You have already claimed your winnings.")
                (enforce (> user_total_amount 0.0) "You have no winnings to claim.")

                ;If side 1 wins, we need to check if the user has a side 1 balance
                ;If the user does have a side 1 balance, they win their proportional share of side 2

                (if  (and (= event_side_1_wins true) (> user_side_1_amount 0.0) )
                    [
                      ;Side 1 has won, and the user has bet on side 1
                      ;But lets make sure there are winnings to transfer from the losing side

                      (if (> event_side_2_amount 0.0)
                        [
                          ;There are tokens from the losers on side 2 for us to distribute to side 1 winners
                          ;Lets calculate winnings, fees, and make transfers
                          (let*
                              (
                                (user_share_side_1:decimal (/ user_side_1_amount event_side_1_amount))
                                (user_winnings_side_2:decimal (floor (* event_side_2_amount user_share_side_1) (event_token::precision) ))
                                (user_total_pay_side_1_no_fee:decimal (floor (+ user_side_1_amount user_winnings_side_2) (event_token::precision) ))
                                (fee_1:decimal (floor (* user_total_pay_side_1_no_fee 0.05) (event_token::precision)))
                                (user_total_pay_side_1:decimal (floor (- user_total_pay_side_1_no_fee fee_1) (event_token::precision) ))
                              )

                              ;Transfer fee to treasury pool
                              (install-capability (event_token::TRANSFER event_pool (get-account-principal (get-token-key event_token) (get-token-key event_token)) fee_1))
                              (with-capability (PRIVATE_RESERVE event_id (get-token-key event_token)) (event_token::transfer event_pool (get-account-principal (get-token-key event_token) (get-token-key event_token)) fee_1))

                              ;Transfer winnings back to user
                              (install-capability (event_token::TRANSFER event_pool account_id user_total_pay_side_1))
                              (with-capability (PRIVATE_RESERVE event_id (get-token-key event_token)) (event_token::transfer event_pool account_id user_total_pay_side_1))

                              (emit-event (NOSTRADAMUS_WINNINGS_CLAIMED event_id account_id event_token user_total_pay_side_1 fee_1))
                          )
                        ]
                        [
                          ;Unfortunately noone bet on side 2, and there are no losers for winners to win tokens from
                          ;Lets transfer back the users original bet since they havent won anything from the losers
                          (install-capability (event_token::TRANSFER event_pool account_id user_side_1_amount))
                          (with-capability (PRIVATE_RESERVE event_id (get-token-key event_token)) (event_token::transfer event_pool account_id user_side_1_amount))

                          ;Emit event
                          (emit-event (NOSTRADAMUS_WINNINGS_CLAIMED event_id account_id event_token user_side_1_amount 0.0))
                        ]
                      )
                    ]
                    true
                )

                ;If side 2 wins, we need to check if the user has a side 2 balance
                ;If the user does have a side 2 balance, they win their proportional share of side 1

                (if  (and (= event_side_2_wins true) (> user_side_2_amount 0.0) )
                    [
                      ;Side 2 has won, and the user has bet on side 2
                      ;But lets make sure there are winnings to transfer from the losing side

                      (if (> event_side_1_amount 0.0)
                        [
                          ;There are tokens from the losers on side 1 for us to distribute to side 2 winners
                          ;Lets calculate winnings, fees, and make transfers
                          (let*
                              (
                                (user_share_side_2:decimal (/ user_side_2_amount event_side_2_amount))
                                (user_winnings_side_1:decimal (floor (* event_side_1_amount user_share_side_2) (event_token::precision) ))
                                (user_total_pay_side_2_no_fee:decimal (floor (+ user_side_2_amount user_winnings_side_1) (event_token::precision) ))
                                (fee_2:decimal (floor (* user_total_pay_side_2_no_fee 0.05) (event_token::precision)))
                                (user_total_pay_side_2:decimal (floor (- user_total_pay_side_2_no_fee fee_2) (event_token::precision) ))
                              )

                              ;Transfer fee to treasury pool
                              (install-capability (event_token::TRANSFER event_pool (get-account-principal (get-token-key event_token) (get-token-key event_token)) fee_2))
                              (with-capability (PRIVATE_RESERVE event_id (get-token-key event_token)) (event_token::transfer event_pool (get-account-principal (get-token-key event_token) (get-token-key event_token)) fee_2))

                              ;Transfer winnings back to user
                              (install-capability (event_token::TRANSFER event_pool account_id user_total_pay_side_2))
                              (with-capability (PRIVATE_RESERVE event_id (get-token-key event_token)) (event_token::transfer event_pool account_id user_total_pay_side_2))

                              ;Emit event
                              (emit-event (NOSTRADAMUS_WINNINGS_CLAIMED event_id account_id event_token user_total_pay_side_2 fee_2))

                          )
                        ]
                        [
                          ;Unfortunately noone bet on side 1, and there are no losers for winners to win tokens from
                          ;Lets transfer back the users original bet since they havent won anything from the losers
                          (install-capability (event_token::TRANSFER event_pool account_id user_side_2_amount))
                          (with-capability (PRIVATE_RESERVE event_id (get-token-key event_token)) (event_token::transfer event_pool account_id user_side_2_amount))

                          ;Emit event
                          (emit-event (NOSTRADAMUS_WINNINGS_CLAIMED event_id account_id event_token user_side_2_amount 0.0))
                        ]
                      )
                    ]
                    true
                )

                ;Update the users bet record so they can no longer claim winnings
                (update bet-record-table (get-2-key event_id account_id)
                    {
                        "claimed_winnings": true,
                        "total_amount": 0.0,
                        "side_1_amount": 0.0,
                        "side_2_amount": 0.0
                    }
                )

                ;Return a final message
                (if  (and (= event_side_2_wins true) (> user_side_2_amount 0.0) )

                      (if (> event_side_1_amount 0.0)

                          (let*
                              (
                                (_user_share_side_2:decimal (/ user_side_2_amount event_side_2_amount))
                                (_user_winnings_side_1:decimal (floor (* event_side_1_amount _user_share_side_2) (event_token::precision) ))
                                (_user_total_pay_side_2_no_fee:decimal (floor (+ user_side_2_amount _user_winnings_side_1) (event_token::precision) ))
                                (_fee_2:decimal (floor (* _user_total_pay_side_2_no_fee 0.05) (event_token::precision)))
                                (_user_total_pay_side_2:decimal (floor (- _user_total_pay_side_2_no_fee _fee_2) (event_token::precision) ))
                              )

                              (format "Claimed {} {} (fee {}) for winning event {}" [_user_total_pay_side_2 event_token _fee_2 event_id])

                          )

                        (format "Returned {} {} because noone else bet against you on event {}" [user_side_2_amount event_token event_id])
                      )


                    (if  (and (= event_side_1_wins true) (> user_side_1_amount 0.0) )

                        (if (> event_side_2_amount 0.0)

                            (let*
                                (
                                  (_user_share_side_1:decimal (/ user_side_1_amount event_side_1_amount))
                                  (_user_winnings_side_2:decimal (floor (* event_side_2_amount _user_share_side_1) (event_token::precision) ))
                                  (_user_total_pay_side_1_no_fee:decimal (floor (+ user_side_1_amount _user_winnings_side_2) (event_token::precision) ))
                                  (_fee_1:decimal (floor (* _user_total_pay_side_1_no_fee 0.05) (event_token::precision)))
                                  (_user_total_pay_side_1:decimal (floor (- _user_total_pay_side_1_no_fee _fee_1) (event_token::precision) ))
                                )
                                (format "Claimed {} {} (fee {}) for winning event {}" [_user_total_pay_side_1  event_token _fee_1 event_id])
                            )

                          (format "Returned {} {} because noone else bet against you on event {}" [user_side_1_amount event_token event_id])
                        )


                      (format "No winnings claimed from event {}" [event_id])
                    )
                )

            )
        )
    )

    (defun withdraw-bet:string
      (account_id:string event_id:string)
        @doc " Withdraw a bet from a refunded event "
        (with-capability (ACCOUNT_GUARD account_id)
            (let*
                (
                    (event-data:object{event-schema} (read events-table event_id))
                    (event_pool:string (at "event_pool" event-data))
                    (event_token:module{fungible-v2} (at "event_token" event-data))
                    (event_end_time:time (at "event_end_time" event-data))
                    (event_refunded:bool (at "event_refunded" event-data))
                    (bet-record-data:object{bet-record-schema} (read bet-record-table (get-2-key event_id account_id)))
                    (user_claimed_winnings:bool (at "claimed_winnings" bet-record-data))
                    (user_total_amount:decimal (at "total_amount" bet-record-data))
                    (user_side_1_amount:decimal (at "side_1_amount" bet-record-data))
                    (user_side_2_amount:decimal (at "side_2_amount" bet-record-data))
                    (event_side_1_users:integer (at "event_side_1_users" event-data))
                    (event_side_2_users:integer (at "event_side_2_users" event-data))
                )

                ;Enforce rules
                (enforce (= event_refunded true) "This event has been refunded.")
                (enforce (= user_claimed_winnings false) "You have already claimed your winnings.")
                (enforce (> user_total_amount 0.0) "You have no tokens to be refunded.")

                ;Adjust side 1 user count records if needed
                (if  (> user_side_1_amount 0.0)
                  [
                    (update events-table event_id
                        {
                            "event_side_1_users": (- event_side_1_users 1)
                        }
                    )
                  ]
                  true
                )

                ;Adjust side 2 user count records if needed
                (if  (> user_side_2_amount 0.0)
                  [
                    (update events-table event_id
                        {
                            "event_side_2_users": (- event_side_2_users 1)
                        }
                    )
                  ]
                  true
                )

                ;Transfer back users tokens
                (install-capability (event_token::TRANSFER event_pool account_id user_total_amount))
                (with-capability (PRIVATE_RESERVE event_id (get-token-key event_token)) (event_token::transfer event_pool account_id user_total_amount))

                ;Update the users bet record
                (update bet-record-table (get-2-key event_id account_id)
                    {
                        "total_amount": 0.0,
                        "side_1_amount": 0.0,
                        "side_2_amount": 0.0
                    }
                )

                ;Return a final message
                (format "Withdrew bet of {} {} from event {}" [user_total_amount event_token event_id])
            )
        )
    )

    ;;///////////////////////
    ;;GETTERS
    ;;//////////////////////

    (defun get-all-events:[string] ()
    @doc " Get list of event keys "
      (keys events-table)
    )

    (defun get-event:object{event-schema} (event-id:string)
    @doc " Get event by ID "
      (read events-table event-id)
    )

    (defun get-event-user:object (account_id:string event_id:string)
    @doc " Get event with user betting records "
      (+
        (read events-table event_id)
        (try {} (read bet-record-table (get-2-key event_id account_id)))
      )
    )

    (defun get-events:[object] ()
    @doc " Get all events "
        (let
          (
              (events (get-all-events))
          )
          (map (get-event) events)
      )
    )

    (defun get-user-events:[object] (account_id:string)
    @doc " Get all events with user betting records "
      (let
          (
              (events (get-all-events))
          )
          (map (get-event-user account_id) events)
      )
    )

    (defun get-bet-record:object{bet-record-schema} (account_id:string event_id:string)
    @doc " Get a user betting record from an event "
      (read bet-record-table (get-2-key event_id account_id))
    )

    (defun get-treasury-account:object{fee-bank-schema} (token:module{fungible-v2})
    @doc " Get treasury account by token "
        (let
            (
                (treasury-data:object{fee-bank-schema} (read bank-fee-table (get-token-key token)))
            )
            treasury-data
        )
    )

    (defun is-admin:bool(account:string)
    @doc " Check if an account is an admin "
        (let*
            (
              (admins_data (read admins-table "ADMINS"))
              (admin_ids (at "admin_ids" admins_data))
            )
            (contains account admin_ids)
        )
    )

    ;;///////////////////////
    ;;UTILITIES
    ;;//////////////////////

    (defun create-id-key:string
      (string_one:string string_two:string)
      @doc " Create hashed key from 2 strings "
      (hash (+ string_one (+ string_two (format "{}" [(at 'block-time (chain-data))]))))
    )

    (defun get-token-key:string
      ( tokenA:module{fungible-v2} )
      @doc " Create key from token "
      (format "{}" [tokenA])
    )

    (defun get-2-key:string ( string_one:string string_two:string )
    @doc " Create key from 2 strings "
      (format "{}:{}" [string_one string_two])
    )

    (defun enforce-unit:bool (amount:decimal precision:integer)
      @doc " Enforces precision "
      (enforce
        (= (floor amount precision)
           amount)
        "Minimum denomination exceeded.")
    )

)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.nostradamus.events-table)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.nostradamus.admins-table)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.nostradamus.bet-record-table)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.nostradamus.bank-fee-table)
;(init)

