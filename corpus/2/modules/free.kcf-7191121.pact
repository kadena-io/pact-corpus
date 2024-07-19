(module kcf GOV
  @doc "Kadena Coin Flip Reborn"

  (defconst GOV_GUARD_DB_KEY:string 'gov )
  (defconst OPS_GUARD_DB_KEY:string 'ops )
  (defconst DEALER_GUARD_DB_KEY:string 'dealer )
  (defconst MIN_COIN_PER_FLIP:decimal 1.0)
  (defconst MAX_COIN_PER_FLIP:decimal 50.0)
  (defconst FEE_RATE:decimal 0.03) ;; (0.0, 1.0), 1.0 = 100%

  ;; Tables

  (defschema m-guards-schema
    @doc "Stores guards for the module"
    guard:guard  
  )
  (deftable m-guards:{m-guards-schema})

  ;; Capabilities

  (defcap GOV ()
    @doc "Can upgrade contract and access DB"
    (enforce-guard (curr-gov))
  )
  
  (defcap OPS ()
    @doc "Can set fees and withdraw from house and fee accounts"
    (enforce-guard (curr-ops))
    (compose-capability (HOUSE_COIN))
    (compose-capability (FEE_COIN))
  )

  (defcap DEALER ()
    @doc "Must sign every (flip) call"
    (enforce-guard (curr-dealer))
  )

  (defcap IN_GAME (amount:decimal)
    @doc "Can only be used during a flip game"
    (enforce (<= MIN_COIN_PER_FLIP amount) "amount smaller than min")
    (enforce (>= MAX_COIN_PER_FLIP amount) "amount larger than max")
    ;; enable house to transfer coin if win
    (compose-capability (HOUSE_COIN))
    (compose-capability (DEALER))
  )

  (defcap HOUSE_COIN ()
    @doc "coin guard for house account"
    true
  )

  (defcap FEE_COIN ()
    @doc "coin guard for fee account"
    true
  )

  ;; Functions

  (defun curr-gov:guard ()
    @doc "Gets the current gov guard"
    (at 'guard (read m-guards GOV_GUARD_DB_KEY ['guard ]))
  )

  (defun curr-ops:guard ()
    @doc "Gets the current ops guard"
    (at 'guard (read m-guards OPS_GUARD_DB_KEY ['guard ]))
  )

  (defun curr-dealer:guard ()
    @doc "Gets the current dealer guard"
    (at 'guard (read m-guards DEALER_GUARD_DB_KEY ['guard ]))
  )

  (defun rotate-ops:string (guard:guard)
    @doc "Changes the ops guard to the provided one."
    (with-capability (OPS)
      (update m-guards OPS_GUARD_DB_KEY
        { "guard": guard }  
      )
    )
  )

  (defun rotate-gov:string (guard:guard)
    @doc "Changes the gov guard to the provided one."
    (with-capability (GOV)
      (update m-guards GOV_GUARD_DB_KEY
        { "guard": guard }  
      )
    )
  )

  (defun rotate-dealer:string (guard:guard)
    @doc "Changes the dealer guard to the provided one."
    (with-capability (GOV)
      (update m-guards DEALER_GUARD_DB_KEY
        { "guard": guard }  
      )
    )
  )

  (defun house-guard:guard ()
    @doc "Creates a guard that is used for the house coin account"
    (create-capability-guard (HOUSE_COIN))
  )

  (defun house-account:string ()
    @doc "Returns the house coin account"
    (create-principal (house-guard))
  )

  (defun withdraw-from-house:decimal (receiver:string amount:decimal)
    @doc "Withdraws and returns the specified amount from house coin account to receiver"
    (with-capability (OPS)
      (coin.transfer (house-account) receiver amount)
      amount
    )
  )

  (defun fee-guard:guard ()
    @doc "Creates a guard that is used for the fee coin account"
    (create-capability-guard (FEE_COIN))
  )

  (defun fee-account:string ()
    @doc "Returns the fee coin account"
    (create-principal (fee-guard))
  )

  (defun withdraw-from-fee:decimal (receiver:string amount:decimal)
    @doc "Withdraws and returns the specified amount from fee coin account to receiver"
    (with-capability (OPS)
      (coin.transfer (fee-account) receiver amount)
      amount
    )
  )

  (defun rand-is-heads-choice:bool (amount:decimal)
    @doc "Generates a 'pseudo-random' bool. true=heads, false=tails"
    ;; yes, miners can totally hack this
    (require-capability (IN_GAME amount))
    (let* 
      (
        (seed1 (tx-hash))
        (seed2 (hash (at 'block-time (chain-data))))
        (res (hash (concat [seed1 seed2])))
      )
      (= 1 (mod (str-to-int 64 res) 2))
    )
  )

  (defun flip:bool ()
    @doc "Play the coin flip game. Returns true if player won, false if lost."
    (let* (
        (player (read-string 'player ))
        (choice (read-integer 'choice ))
        (amount (read-decimal 'amount ))
        (choice-bool (!= 0 choice))
      )
      (with-capability (IN_GAME amount)
        (coin.transfer player (house-account) amount)
        (coin.transfer player (fee-account) (* FEE_RATE amount))
        (if (= choice-bool (rand-is-heads-choice amount))
          (let ((trf-back-amt (* 2.0 amount)))
            (install-capability (coin.TRANSFER (house-account) player trf-back-amt))
            (coin.transfer (house-account) player trf-back-amt)
            true
          )
          false
        )
      )
    )
  )
)

;; Init


