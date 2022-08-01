(module kd5 GOVERNANCE

  @doc " 'kd5' is a contract where you can buy shares of a set of 2 ASIC miners. "

  (use coin)

;; --------------------------------------------------------------------------
;; Schemas and Tables

  ;; Stores clients information and balance
  (defschema clients-schema
    account:string
    shares:decimal
    balance:decimal
    profit:decimal
    guard:guard)
  (deftable clients-table:{clients-schema})

  ;; Stores shares left for sale
  (defschema shares-schema
    shares:decimal)
  (deftable shares-table:{shares-schema})

;; --------------------------------------------------------------------------
;; Capatilibites

  (defcap GOVERNANCE()
    (enforce-keyset 'admin-kd5 ))

  (defcap MOVE_FUNDS ()
    "private cap to move funds"
    true)

;; --------------------------------------------------------------------------
;; Constants

  (defconst OWNER_ACCOUNT:string "017749fc26f8bf8b5a67204ad9d38b75999da983096f16d18a77af86cba41f4a"
    " Account selling the shares. ")

  (defconst MINER_ACCOUNT:string "8ddefd2849d7f93c3674da51a88392e3c19cf2e6567f0003552320146de4e926"
    " Account receiving mining dividends. ")

  (defconst TEMP_ACCOUNT:string 'temporary-holder
    " Account holding distributed funds until withdraw. ")

  (defconst TOTAL_SHARES 35000.0
    " Specifies the value in KDA of the KD5 at the time of the contract creation. ")

  (defconst SHARES_FOR_SALE 15000.0
    " Specifies the amount of shares the owner is willing to sell. ")

;; --------------------------------------------------------------------------
;; Functions

  ;; Initialize required accounts and share amount
  ;; Can only happen once
  (defun initialize
     (account:string
      guard:guard)
      (coin.create-account MINER_ACCOUNT (create-module-guard "miner-account"))
      (coin.create-account TEMP_ACCOUNT (create-module-guard "temp-account"))
      (write clients-table account
        { "account"     : account
        , "shares"   : TOTAL_SHARES
        , "balance"  : 0.0
        , "profit"   : 0.0
        , "guard"    : guard }))

  ;; Buy a share to get mining rewards
  (defun buy
     (account:string
      amount:decimal
      guard:guard)
      ;; Enforce minimal buy amount and send to Owner
      (enforce (>= amount 10.0) "Minimum purchase is 10KDA.")
      (coin.transfer account OWNER_ACCOUNT amount)
      ;; Update shares left for sale and owner shares
      (with-read shares-table ""
        { "shares"  := availShares }
        (enforce (>= availShares amount) "Not enough shares available.")
        (update shares-table ""
          { "shares" : (- availShares amount) }))
      (with-read clients-table OWNER_ACCOUNT
        { "shares"  := shares }
        (update clients-table OWNER_ACCOUNT
          { "shares" : (- shares amount) }))
      ;; Check if user already exists, if it does, enforce guard
      (with-default-read clients-table account
        { "shares"  : 0.0
        , "profit"  : 0.0
        , "balance" : 0.0
        , "guard"   : guard }
        { "shares"  := currentShares
        , "profit"  := currentProfit
        , "balance" := currentBalance
        , "guard"   := currentGuard }
        (enforce (= currentGuard guard) "Account guards do not match.")
      ;; Update user balance and profit based on amount bought
      (write clients-table account
        { "account"     : account
        , "shares"   : (+ currentShares amount)
        , "balance"  : currentBalance
        , "profit"   : (- currentProfit amount)
        , "guard"    : currentGuard })
      (format "Bought {} shares for a new total of {}" [amount,(+ currentShares amount)])))

  ;; Withdraw your mining rewards
  (defun withdraw
     (account:string
      guard:guard)
      (with-read clients-table account
        { "balance" := balance
        , "profit"  := profit
        , "guard"   := currentGuard }
        ;; Enforce user is the owner and require a minimum balance
        (enforce (= currentGuard guard) "Account guards do not match.")
        (enforce (>= balance 0.01) "Minimum withdrawal is 0.01 KDA.")
        ;; Reset client balance to 0 and update profits
        (update clients-table account
          { "balance"  : 0.0
          , "profit"   : (+ profit balance) })
        ;; Send the user their coins
        (coin.transfer TEMP_ACCOUNT account balance)
        (format "Withdrew {} KDA." [balance])))

  ;; Measure payment per user
  (defun payone(account:string)
    (require-capability (MOVE_FUNDS))
      (with-default-read clients-table account
        { "balance"  : 0.0
        , "shares"   : 0.0 }
        { "balance"  := currentBalance
        , "shares"   := userShares }
        (update clients-table account
          { "balance"  : (+ (round (* (/ userShares TOTAL_SHARES) (coin.get-balance MINER_ACCOUNT))12) currentBalance) })))

  ;; Execute the payone function to all accounts in the table
  (defun sendpayment()
    (with-capability (MOVE_FUNDS)
      (map (payone)(listaccounts)))
    (coin.transfer MINER_ACCOUNT TEMP_ACCOUNT (coin.get-balance MINER_ACCOUNT)))

  ;; List client accounts
  (defun listaccounts()
    (sort ['shares] (map (at 'account) (viewclients))))

  ;; Returns a list of all clients and their infos
  (defun viewclients ()
    (map (read clients-table) (keys clients-table)))

  ;; Returns the amount owed to an account
  (defun pendingone (account:string)
    (at 'balance (read clients-table account ['balance ])))

  ;; Returns the amount owed to the group
  (defun pendingall ()
    (round (coin.get-balance MINER_ACCOUNT)12))

  ;; Returns the amount of shares remaining
  (defun remainingshares ()
    (at 'shares (read shares-table "" ['shares ]))))

;; ; --------------------------------------------------------------------------
;; ; Create tables outside of the module

;(create-table clients-table)
;(create-table shares-table)
;(insert shares-table "" { "shares" : SHARES_FOR_SALE })
;(free.kd5.initialize OWNER_ACCOUNT (read-keyset 'admin-kd5))

