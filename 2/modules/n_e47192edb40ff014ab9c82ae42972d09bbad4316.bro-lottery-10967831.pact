(module bro-lottery GOVERNANCE
  (use free.util-time)
  (use free.util-math)
  (use free.util-lists)
  (use n_582fed11af00dc626812cd7890bb88e72067f28c.bro)

  ;-----------------------------------------------------------------------------
  ; CAPS DEFINITIONS
  ;-----------------------------------------------------------------------------
  (defcap GOVERNANCE()
    (enforce-keyset "n_e47192edb40ff014ab9c82ae42972d09bbad4316.admin"))

  (defcap OPERATE ()
    (enforce-keyset "n_e47192edb40ff014ab9c82ae42972d09bbad4316.op"))

  (defcap WITHDRAW-JACKPOT:bool ()
    (compose-capability (GOVERNANCE))
    (compose-capability (JACKPOT-POOL)))

  (defcap CREATE-ROUND:bool ()
    (compose-capability (OPERATE)))

  ;-----------------------------------------------------------------------------
  ; EVENTS
  ;-----------------------------------------------------------------------------
  (defcap LOTTERY-ROUND (id:string end-date:time)
    @event
    true)

  (defcap TICKET-BOUGHT (id:string account:string rank:integer star-number:integer)
    @event
    true)

  (defcap SETTLED (result:object:{lottery-result})
    @event
    true)

  ;-----------------------------------------------------------------------------
  ; DATA DEFINITION
  ;-----------------------------------------------------------------------------
  ; A lottery round => One instance per round is added into the DB.
  (defschema lottery-round
    id:string ; A 8 chars ID
    start-time:time ; Start-time : only informative
    end-time:time ; End-time: end of tickets sales
    ticket-price:decimal ; Ticket price
    tickets-count:integer ; Number of sold tickets, automatically incremented
    tickets-limit:integer ; Limits of tickets being sold for this round
    inner-seed:string ; Internal seed (see doc), automatically updated by each sale
    btc-height:integer ; The minimum BTC height for retriving the external seed
    settlement-tx:string ; Hash of th settlement TX: empty is not settled
  )

  ; Used to intialize the contract
  (defconst NULL-ROUND:object{lottery-round}
    {'id:"",
     'start-time:(genesis),
     'end-time:(genesis),
     'ticket-price:0.0,
     'tickets-count:0,
     'tickets-limit:0,
     'inner-seed:"",
     'btc-height:0,
     'settlement-tx:(hash "NULL-ROUND")}
  )

  (deftable round-table:{lottery-round})

  ; Result object => Created and stored once the lottery has been settled
  (defschema lottery-result
    inner-seed:integer ; Copy of the internal seed of the round
    btc-height:integer ; BTC heigt used for the external seed
    seed:integer ; Seed used for comupting the results (Internal seed XOR external seed)
    star-number:integer ; Drawn STAR Number
    winning-tickets:[integer] ; Drawn winning tickets
    final-round-bal:decimal ; Final balance of the main pool
    final-jackpot-bal:decimal ; Final Jackpot balance
    jackpot-won:bool ; Whether the Jackpot was won
  )

  (deftable result-table:{lottery-result})

  ; A small table to only store the current round
  (defschema current
    round-id:string
  )
  (deftable current-table:{current})

  ; Tickets management
  (defschema ticket
    round-id:string ; Round-id linked to this ticket
    rank:integer ; Rank of the ticket in the round
    account:string ; Account to pay the winnings
    star-number:integer ; STAR Number choosen by the user
  )
  (deftable ticket-table:{ticket})

  ;-----------------------------------------------------------------------------
  ; STATES MANAGEMENT
  ;-----------------------------------------------------------------------------

  ; A round goes through different states:
  ;   - STARTING => Before the start-date (transient state)
  ;   - RUNNING => Between start-date and end-date (Tickets can be sold)
  ;   - ENDED => Tickets sales are ended, we are waiting for a BTC Block that meets pre-conditions
  ;   - SETTLED => The (settle) function has been called, prizes payment have been done.
  ; Special case: when a round has ended without any ticket sold, we consider it as already SETTLED
    (defun round-state:string ()
      @doc "Returns the Round state"
      (bind (current-round) {'start-time:=start-time, 'end-time:=end-time,
                             'tickets-count:=cnt,'settlement-tx:=settled}
        (cond
          ((!= "" settled) "SETTLED")
          ((and (= cnt 0) (is-past end-time)) "SETTLED")
          ((is-past end-time) "ENDED")
          ((is-past start-time) "RUNNING")
          "STARTING"))
    )

    (defun enforce-round-state:bool (target:string)
      @doc "Enforce a round state"
      (let ((state (round-state)))
        (enforce (= target state) (format "Current round is not {}" [target])))
    )

  ;-----------------------------------------------------------------------------
  ; RULES
  ;-----------------------------------------------------------------------------
  ; Part of the ticket price send to the Jackpot pool
  (defconst JACKPOT-POOL-RATIO 0.1)

  ; Part of the ticket price to be send to the main pool
  (defconst MAIN-POOL-RATIO (- 1.0 JACKPOT-POOL-RATIO))

  ; The available STAR numbers
  (defconst STARS [0 1 2 3 4 5 6 7 8 9])

  ; Number of winnign tickets
  (defconst WINNING-TICKETS 3)

  ; Ratio of the MAIN pools to be won for the winning tickets
  (defconst WINNINGS-RATIO [0.5 0.25 0.15])

  ; Fee ratio of the main pool
  (defconst FEE-RATIOS [0.05])

  ; When a Jackpot is won, only 80% is paid, the remaining stays for the next round
  (defconst JACKPOT-WIN-RATIO 0.8)

  ;-----------------------------------------------------------------------------
  ; ACCOUNTS DEFINITION
  ;-----------------------------------------------------------------------------
  ; Output account in case of forced wtihdrawal of the Jackpot
  (defconst JACKPOT-WITHDRAWAL-ACCOUNT "r:n_582fed11af00dc626812cd7890bb88e72067f28c.community")

  ; Output accounts to pay fees
  (defconst FEE-ACCOUNTS ["k:b6575b7c2b165cbc94884311e4c05f59b3e31fa541a63ec9ac5899b3c421ee69"])

  ; Output account to pay community fees
  (defconst COMMUNITY-ACCOUNT "r:n_582fed11af00dc626812cd7890bb88e72067f28c.community")

  ;---------
  ; Ticket TICKET-SALES account
  (defcap TICKET-SALES:bool ()
    true)

  (defconst TICKET-SALES-GUARD (create-capability-guard (TICKET-SALES)))

  (defconst TICKET-SALES-ACCOUNT (create-principal TICKET-SALES-GUARD))

  ; --------
  ; Jackpot account
  (defcap JACKPOT-POOL:bool ()
    true)

  (defconst JACKPOT-GUARD (create-capability-guard (JACKPOT-POOL)))

  (defconst JACKPOT-ACCOUNT (create-principal JACKPOT-GUARD))

  (defun jackpot-balance:decimal ()
    (get-balance JACKPOT-ACCOUNT))

  ; --------
  ; Account dedicated to a lottery round
  (defcap ROUND-MAIN-POOL:bool (id:string)
    true)

  (defun round-guard:guard (id:string)
    (create-capability-guard (ROUND-MAIN-POOL id)))

  (defun round-account:string (id:string)
    (create-principal (round-guard id)))

  (defun round-balance:decimal (id:string)
    (get-balance (round-account id)))

  (defun floor-bro:decimal (x:decimal)
    (floor x (precision)))

  ;-----------------------------------------------------------------------------
  ; UTILS (for internal use)
  ;-----------------------------------------------------------------------------
  (defun create-round-id:string ()
    @doc "Create a lottery ID"
    (take 8 (tx-hash)))

  (defun create-ticket-id:string (round-id:string rank:integer)
    @doc "Create a ticket ID"
    (format "{}:{}" [round-id, rank]))

  (defun enforce-bro-account-exists:bool (account:string)
    @doc "Check that a bro account exists"
    (let ((bal (try -1.0 (get-balance account))))
      (enforce (!= bal -1.0) (format "{} doesn't exist in the BRO contract"  [account])))
  )

  (defun enforce-star-number-valid:bool (in:integer)
    (enforce (contains in STARS) "Invalid Star Number"))

  ;-----------------------------------------------------------------------------
  ; GETTER FUNCTIONS (internal and external use)
  ;-----------------------------------------------------------------------------
  (defun current-round-id:string ()
    @doc "Return the current round ID"
    (with-read current-table "" {'round-id:=di}
      di))

  (defun current-round:object{lottery-round} ()
    @doc "Return the data of the current round"
    (read round-table (current-round-id)))

  (defun get-round:object{lottery-round} (id:string)
    @doc "Return the round for a given ID"
    (read round-table id))

  (defun get-all-rounds:[object{lottery-round}] ()
    @doc "Return all rounds (current and archives)"
    ; Note: We have to filter the intial null round
    (reverse
      (sort ['start-time]
            (select round-table (where 'id (!= "")))))
  )

  (defun get-result:object{lottery-result} (id:string)
    @doc "Return the result of a round: Only for already settled round"
    (read result-table id))

  (defun get-ticket:object{ticket} (round-id:string rank:integer)
    @doc "Return a ticket"
    (read ticket-table (create-ticket-id round-id rank)))

  (defun get-all-tickets:[object{ticket}] (round-id:string)
    (sort ['rank] (select ticket-table (where 'round-id (= round-id))))
  )

  (defun ticket-price:decimal ()
    @doc "Gives the ticket price of the current round"
    (at 'ticket-price (current-round)))

  ;-----------------------------------------------------------------------------
  ; ADMINISTRATIVE FUNCTIONS
  ;-----------------------------------------------------------------------------
  (defun create-round:string (ticket-price:decimal tickets-limit:integer end-time:time)
    @doc "Create a round only for lottery operator"
    ; Check that the previous round has been settled
    (enforce-round-state "SETTLED")
    ; Check input values to avoid mistakes
    (enforce (time-between (from-now (hours 2)) (from-now (days 365)) end-time) "End time invalid")
    (enforce (>= tickets-limit 1) "Tickets limit invalid")
    (enforce (> ticket-price 0.0) "Ticket price invalid")

    ; Create a round requires 3 steps:
    ;  - Create a main pool $BRO account
    ;  - Insert an element into the round table
    ;  - Update the current-round variable
    (with-capability (CREATE-ROUND)
      (let ((id (create-round-id)))
        (create-account (round-account id) (round-guard id))
        (insert round-table id
          {'id:id,
          'start-time:(now),
          'end-time:end-time,
          'ticket-price:ticket-price,
          'tickets-count:0,
          'tickets-limit:tickets-limit,
          'inner-seed:(tx-hash),
          'btc-height: (free.btc-oracle-v5.est-btc-height-at-time end-time),
          'settlement-tx:""
          })
        (update current-table "" {'round-id:id})
        (emit-event (LOTTERY-ROUND id end-time))
        (format "Lottery round created {}" [id])))
  )

  ;-----------------------------------------------------------------------------
  ; BUY A TICKET
  ;-----------------------------------------------------------------------------
  ; For buying a ticket, these pre-conditions must be met
  ;   - account must pre-exist in $BRO contract
  ;   - the $BRO TICKET-SALES-ACCOUNT must be funded with ticket-price
  (defun buy-ticket:string (account:string star-number:integer)
    @doc "Buy a ticket for the given account"
    ; Check that the round is in a correct state
    (enforce-round-state "RUNNING")
    ; Star Number validity
    (enforce-star-number-valid star-number)
    ; We have to be sure that we will have no issue for paying the prizes with
    ; a mssing account.
    (enforce-bro-account-exists account)

    (bind (current-round) {'id:=round-id,
                           'ticket-price:=price,
                           'tickets-count:=cnt,
                           'inner-seed:=last-seed,
                           'tickets-limit:=limit}

      (enforce (< cnt limit) "No more tickets available for this round")

      ; We transfer one part of the TICKET-SALES to the main pool, the other part to the
      ; jackpot pool
      (install-capability (TRANSFER TICKET-SALES-ACCOUNT (round-account round-id) 100.0))
      (install-capability (TRANSFER TICKET-SALES-ACCOUNT JACKPOT-ACCOUNT 100.0))

      (with-capability (TICKET-SALES)
        (transfer TICKET-SALES-ACCOUNT (round-account round-id) (* price MAIN-POOL-RATIO))
        (transfer TICKET-SALES-ACCOUNT JACKPOT-ACCOUNT (* price JACKPOT-POOL-RATIO)))

      ; Create the ticket
      (insert ticket-table (create-ticket-id round-id cnt)
        {'round-id:round-id,
         'rank:cnt,
         'account:account,
         'star-number:star-number})
      ; And update round data (Increment the count + updates internal seed)
      (update round-table round-id
        {'tickets-count:(++ cnt),
         'inner-seed:(hash (concat [last-seed (tx-hash)]))
        })
      (emit-event (TICKET-BOUGHT round-id account cnt star-number))
      (format "Ticket bought for {}" [round-id]))
  )

  ;-----------------------------------------------------------------------------
  ; RESULT COMPUTING FUNCTIONS
  ;-----------------------------------------------------------------------------
  (defun random-value:integer (seed:integer max-value:integer salt:integer)
    @doc "Returns a random integer 0 <= x < max-value, based on seed, salt"
    (mod (str-to-int 64 (hash (xor seed salt))) max-value))

  (defun compute-winning-tickets:[integer] (seed:integer tickets-count:integer)
    @doc "Returns a list of winning tickets up to tickets-count"
    (take WINNING-TICKETS
      (distinct
        (map (random-value seed tickets-count)
             (enumerate 1 16))))
  )

  (defun compute-winning-star(seed:integer)
    @doc "Return the Star Number based on seed"
    (random-value seed 10 888))

  (defun compute-result:object{lottery-result} (in:object{lottery-round})
    @doc "Compute a result object from the round obejct"
    ; We compute the seed + winning ticket + star number exactly as decribed in
    ; the doc: compute-result
    (bind in {'id:=id,
              'inner-seed:=i-seed,
              'tickets-count:=cnt,
              'end-time:=end-time,
              'btc-height:=min-height}
      (bind (free.btc-oracle-v5.select-block min-height (add-time end-time (hours 2)) 1)
        {'header-hash:=o-seed,
         'height:=selected-height}
        (let* ((seed (xor o-seed (str-to-int 64 i-seed)))
               (winning-tickets (compute-winning-tickets seed cnt))
               (star (compute-winning-star seed)))
          {'inner-seed:(str-to-int 64 i-seed),
           'btc-height:selected-height,
           'seed:seed,
           'star-number:star,
           'winning-tickets:winning-tickets,
           'final-round-bal: (round-balance id),
           'final-jackpot-bal: (jackpot-balance),
           'jackpot-won: (= star (at 'star-number (get-ticket id (first winning-tickets))))
          })))
  )

  ;-----------------------------------------------------------------------------
  ; SETTLEMENT FUNCTIONS
  ;-----------------------------------------------------------------------------
  (defun --do-payment:bool (round-id:string account:string amount:decimal)
    (install-capability (TRANSFER (round-account round-id) account 100.0))
    (transfer (round-account round-id) account (floor-bro amount))
    true
  )

  (defun --do-payment-jackpot:bool (ticket:object{ticket})
    (install-capability (TRANSFER JACKPOT-ACCOUNT (at 'account ticket) 100.0))
    (transfer JACKPOT-ACCOUNT (at 'account ticket)
              (floor-bro (* JACKPOT-WIN-RATIO (jackpot-balance))))
    true
  )

  (defun settle-round:string ()
    (enforce-round-state "ENDED")

    (let ((id (current-round-id))
          (result:object{lottery-result} (compute-result (current-round))))
      (bind result {'winning-tickets:=winning-tickets, 'final-round-bal:=total, 'jackpot-won:=jackpot-won}

        (with-capability (ROUND-MAIN-POOL id)
          ; We pay the 3 winners
          (zip (--do-payment id) (map (compose (get-ticket id) (at 'account)) winning-tickets)
                                 (map (* total) WINNINGS-RATIO))
          ; We pay the 5% fees
          (zip (--do-payment id) FEE-ACCOUNTS
                                 (map (* total) FEE-RATIOS))

          ; We transfer all funds left in the pool to the community account.
          ; Should be 5% + Dust coming from previous rounding errors.
          (--do-payment id COMMUNITY-ACCOUNT (round-balance id)))

        ; We pay the Jackpot only if the jackpot-won flag has been set by the
        ; (compute-result) function.
        (if jackpot-won
            (with-capability (JACKPOT-POOL)
              (--do-payment-jackpot (get-ticket id (first winning-tickets))))
            false))

      (update round-table id {'settlement-tx:(tx-hash)})
      (insert result-table id result)
      (emit-event (SETTLED result))
      (format "Round Settled for {}" [id]))
  )


  ;-----------------------------------------------------------------------------
  ; ADMINISTRATIVE FUNCTIONS
  ;-----------------------------------------------------------------------------
  (defun enforce-admin-accounts ()
    @doc "Check that admins accounts all exist. Supposed to be called after each module upgrade"
    (enforce-bro-account-exists JACKPOT-WITHDRAWAL-ACCOUNT)
    (map (enforce-bro-account-exists) FEE-ACCOUNTS)
    (enforce-bro-account-exists COMMUNITY-ACCOUNT)
  )

  (defun init ()
    (insert current-table "" {'round-id: ""})
    (insert round-table "" NULL-ROUND)
    (create-account JACKPOT-ACCOUNT JACKPOT-GUARD)
    (create-account TICKET-SALES-ACCOUNT TICKET-SALES-GUARD)
    ; Check external accounts => I Don't want to have issues later for payments
    (enforce-admin-accounts)
    "Init OK"
  )

  (defun withdraw-jackpot:string ()
    (enforce-round-state "SETTLED")
    (with-capability (WITHDRAW-JACKPOT)
      (let ((bal (get-balance JACKPOT-ACCOUNT)))
        (install-capability (TRANSFER JACKPOT-ACCOUNT JACKPOT-WITHDRAWAL-ACCOUNT bal))
        (transfer JACKPOT-ACCOUNT JACKPOT-WITHDRAWAL-ACCOUNT bal)))
  )
)


