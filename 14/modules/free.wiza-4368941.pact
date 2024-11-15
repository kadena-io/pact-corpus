(module wiza ADMIN
    @doc "WIZA token"
    @model
    [ (defproperty conserves-mass (amount:decimal)
        (= (column-delta token-table 'balance) 0.0))
      (defproperty valid-account-id (accountId:string)
        (and
          (>= (length accountId) 3)
          (<= (length accountId) 256))) ]

    (use coin)
  (implements fungible-v2)
  (implements wiza1-interface-v3)

  ; --------------------------------------------------------------------------
 ; Constants
; --------------------------------------------------------------------------

    (defconst ADMIN_KEYSET "free.wiza-token-keyset")
    (defconst ADMIN_ADDRESS "k:90f45921e0605560ace17ca8fbbe72df95ba7034abeec7a8a7154e9eda7114eb")

    (defconst TOKEN_NAME "WIZA"
        "the name of the token")

    (defconst DECIMALS 12
        "Specifies the minimum denomination for token transactions.")

    (defconst ACCOUNT_ID_CHARSET CHARSET_LATIN1
        "Allowed character set for account IDs.")

    (defconst ACCOUNT_ID_MIN_LENGTH 3
        "Minimum character length for account IDs.")

    (defconst ACCOUNT_ID_MAX_LENGTH 256
        "Maximum character length for account IDs.")

    (defconst STARTING_SUPPLY:decimal 0.0
        "WIZA starting supply")

    (defconst MAXIMUM_SUPPLY:decimal 13240000.0
        "WIZA maximum supply")

    (defconst WIZA_TOKEN_BANK:string "wiza-token-bank")

  ; --------------------------------------------------------------------------
  ; Schemas and tables
  ; --------------------------------------------------------------------------

    (defschema token-schema
        balance:decimal
        guard:guard
    )

    (defschema staked-schema
        idnft:string
        account:string
        timestamp:time
        staked:bool
        multiplier:integer
    )

    (defschema mined-wiza-schema
        amount:decimal
    )

    (defschema base-multiplier-schema
        multiplier:decimal
    )

    (deftable token-table:{token-schema})
    (deftable staked-table:{staked-schema})
    (deftable mined-wiza-table:{mined-wiza-schema})

    (deftable base-multiplier-table:{base-multiplier-schema})

    ; --------------------------------------------------------------------------
 ; Capatilibites
 ; --------------------------------------------------------------------------

    (defcap PRIVATE ()
        @doc "can only be called from a private context"
        true
    )

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defcap ACCOUNT_GUARD(account:string)
        @doc "Verifies account meets format and belongs to caller"
        (enforce (is-principal account) "")
        (enforce-guard (at "guard" (coin.details account)))
    )

    ;; checks the owner of the nft
    (defcap OWNER (account:string id:string owner:string)
        @doc "Enforces that an account owns the nft"
        (enforce (= owner account) "Account is not owner of the NFT")
        (compose-capability (ACCOUNT_GUARD account))
    )

    (defcap DEBIT ( sender:string )
        @doc " Capability to perform debiting operations. "
        (enforce-guard (at 'guard (read token-table sender ['guard ])))
        (enforce (!= sender "") "Invalid sender.")
    )

    (defcap CREDIT ( receiver:string )
        @doc " Capability to perform crediting operations. "
        (enforce (!= receiver "") "Invalid receiver.")
    )

    (defcap TRANSFER:bool
        ( sender:string
            receiver:string
            amount:decimal )
        @doc " Capability to move tokens from an accounts. "
        @managed amount TRANSFER-mgr
        (enforce (!= sender receiver) "same sender and receiver")
        (enforce-unit amount)
        (enforce (> amount 0.0) "Positive amount")
        (compose-capability (DEBIT sender))
        (compose-capability (CREDIT receiver))
    )

    (defun TRANSFER-mgr:decimal
        ( managed:decimal
          requested:decimal )
        @doc " Manages transfer operations. "
        (let ((newbal (- managed requested)))
        (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed])) newbal)
    )

    (defun create-BANK-guard ()
        (create-user-guard (require-PRIVATE))
    )

    (defun require-PRIVATE ()
        (require-capability (PRIVATE))
    )

    (defcap STAKE_NFT (id:string owner:string multiplier:integer)
        @event true
    )

    (defcap UNSTAKE_NFT (id:string owner:string)
        @event true
    )

    (defcap WIZA_REWARD_FROM_STAKE (account:string amount:decimal)
        @event true
    )

    (defcap WIZA_REWARD_GENERAL (account:string amount:decimal)
        @event true
    )

    (defcap WIZA_REWARD_AP_BURN (account:string amount:decimal)
        @event true
    )

    (defcap SPEND_WIZA (account:string amount:decimal)
        @event true
    )

; --------------------------------------------------------------------------
  ; Initialize
  ; --------------------------------------------------------------------------
  (defun initialize ()

    ;(coin.create-account WIZA_TOKEN_BANK (create-BANK-guard))
    ;(create-account WIZA_TOKEN_BANK (create-BANK-guard))

    ;(write mined-wiza-table "" {"amount":0.0})

    (write base-multiplier-table "" {"multiplier": 4.0})
  )

    ; --------------------------------------------------------------------------
 ; Utilities
 ; --------------------------------------------------------------------------
 (defun validate-account-id ( accountId:string )
 @doc " Enforce that an account ID meets charset and length requirements. "
 (enforce
   (is-charset ACCOUNT_ID_CHARSET accountId)
   (format
     "Account ID does not conform to the required charset: {}"
     [accountId]))
 (let ((accountLength (length accountId)))
   (enforce
     (>= accountLength ACCOUNT_ID_MIN_LENGTH)
     (format
       "Account ID does not conform to the min length requirement: {}"
       [accountId]))
   (enforce
     (<= accountLength ACCOUNT_ID_MAX_LENGTH)
     (format
       "Account ID does not conform to the max length requirement: {}"
       [accountId]))))

     (defun transfer-create:string
     ( sender:string
       receiver:string
       receiver-guard:guard
       amount:decimal )
     @doc " Transfer to an account, creating it if it does not exist. "
     @model [ (property (conserves-mass amount))
              (property (> amount 0.0))
              (property (valid-account-id sender))
              (property (valid-account-id receiver))
              (property (!= sender receiver)) ]
     (with-capability (TRANSFER sender receiver amount)
       (debit sender amount)
       (credit receiver receiver-guard amount)))

    (defun transfer:string
     ( sender:string
       receiver:string
       amount:decimal )
     @doc " Transfer to an account, failing if the account does not exist. "
     @model [ (property (conserves-mass amount))
              (property (> amount 0.0))
              (property (valid-account-id sender))
              (property (valid-account-id receiver))
              (property (!= sender receiver)) ]
     (with-read token-table receiver
       { "guard" := guard }
       (transfer-create sender receiver guard amount)))

     (defun debit
     ( accountId:string
       amount:decimal )
     @doc " Decrease an account balance. Internal use only. "
     @model [ (property (> amount 0.0))
              (property (valid-account-id accountId)) ]
     (validate-account-id accountId)
     (enforce (> amount 0.0) "Debit amount must be positive.")
     (enforce-unit amount)
     (if (= accountId WIZA_TOKEN_BANK) (require-capability (PRIVATE)) true)
     (require-capability (DEBIT accountId))
     (with-read token-table accountId
       { "balance" := balance }
       (enforce (<= amount balance) "Insufficient funds.")
       (update token-table accountId
         { "balance" : (- balance amount) })))

    (defun credit
     ( accountId:string
       guard:guard
       amount:decimal )
     @doc " Increase an account balance. Internal use only. "
     @model [ (property (> amount 0.0))
              (property (valid-account-id accountId)) ]
     (validate-account-id accountId)
     (enforce (> amount 0.0) "Credit amount must be positive.")
     (enforce-unit amount)
     (require-capability (CREDIT accountId))
     (with-default-read token-table accountId
       { "balance" : -1.0, "guard" : guard }
       { "balance" := balance, "guard" := retg }
       ; we don't want to overwrite an existing guard with the user-supplied one
       (enforce (= retg guard)
         "account guards do not match")
       (let ((is-new
              (if (= balance -1.0)
                  (enforce-reserved accountId guard)
                false)))
         (write token-table accountId
           { "balance" : (if is-new amount (+ balance amount))
           , "guard"   : retg }))))

     (defun get-balance:decimal ( account:string )
     (at 'balance (read token-table account ['balance])))

    (defun details:object{fungible-v2.account-details} ( account:string )
     (with-read token-table account
       { "balance" := balance
       , "guard"   := guard }
       { "account" : account
       , "balance" : balance
       , "guard"   : guard } ))

    (defun precision:integer ()
     DECIMALS)

    (defun enforce-unit:bool ( amount:decimal )
     @doc " Enforce the minimum denomination for token transactions. "
     (enforce
       (= (floor amount DECIMALS) amount)
       (format "Amount violates minimum denomination: {}" [amount])))

    (defun create-account:string (account:string guard:guard)
      @doc "create new account"
      (enforce-reserved account guard)
      (insert token-table account {
          "balance": 0.0,
          "guard": guard
      })
    )

    (defun enforce-reserved:bool
    ( accountId:string
      guard:guard )
    @doc "Enforce reserved account name protocols."
    (let ((r (check-reserved accountId)))
      (if (= "" r) true
        (if (= "k" r)
          (enforce
            (= (format "{}" [guard])
               (format "KeySet {keys: [{}],pred: keys-all}"
                       [(drop 2 accountId)]))
            "Single-key account protocol violation")
          (enforce false
            (format "Unrecognized reserved protocol: {}" [r]))))))

    (defun check-reserved:string (accountId:string)
        " Checks ACCOUNT for reserved name and returns type if \
        \ found or empty string. Reserved names start with a \
        \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
        (let ((pfx (take 2 accountId)))
          (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

          (defschema crosschain-schema
          @doc " Schema for yielded value in cross-chain transfers "
          receiver:string
          receiver-guard:guard
          amount:decimal )

        (defpact transfer-crosschain:string
          ( sender:string
            receiver:string
            receiver-guard:guard
            target-chain:string
            amount:decimal )
          @model [ (property (> amount 0.0))
                   (property (!= receiver ""))
                   (property (valid-account-id sender))
                   (property (valid-account-id receiver)) ]
          (step
            (with-capability (DEBIT sender)
              (validate-account-id sender)
              (validate-account-id receiver)
              (enforce (!= "" target-chain) "empty target-chain")
              (enforce (!= (at 'chain-id (chain-data)) target-chain)
                "cannot run cross-chain transfers to the same chain")
              (enforce (> amount 0.0)
                "transfer quantity must be positive")
              (enforce-unit amount)
              ;; Step 1 - debit sender account on current chain
              (debit sender amount)
              (let
                ((
                  crosschain-details:object{crosschain-schema}
                  { "receiver"       : receiver
                  , "receiver-guard" : receiver-guard
                  , "amount"         : amount
                  }
                ))
                (yield crosschain-details target-chain))))
          (step
            (resume
              { "receiver"       := receiver
              , "receiver-guard" := receiver-guard
              , "amount"         := amount
              }
              ;; Step 2 - credit receiver account on target chain
              (with-capability (CREDIT receiver)
                (credit receiver receiver-guard amount)
              ))))


    (defun rotate:string
    ( account:string
      new-guard:guard )
    (if (= account WIZA_TOKEN_BANK) (require-capability (PRIVATE)) true)
    (with-read token-table account
      { "guard" := oldGuard }
      (enforce-guard oldGuard)
      (enforce-guard new-guard)
      (update token-table account
        { "guard" : new-guard } )))

  ; --------------------------------------------------------------------------
    ; WIZA functions
    ; --------------------------------------------------------------------------

    (defun stake-all (objects:list m:module{wizarena-interface-v2})
        (map
            (nft-to-stake m)
            objects
        )
    )

    (defun nft-to-stake (m:module{wizarena-interface-v2} obj:object)
        (let (
                (idnft (at "idnft" obj))
                (sender (at "sender" obj))
            )
            (stake idnft sender m)
        )
    )


    (defun stake (idnft:string sender:string m:module{wizarena-interface-v2})
        (enforce (= (at "chain-id" (chain-data)) "1") "Stake is only for chain 1")
        (enforce (= (format "{}" [m]) "free.wiz-arena") "not allowed, security reason")
        (let (
                (data (get-wizard-data idnft m))
            )
            (enforce (= (at "listed" data) false) "A listed wizard cannot be staked")
            (enforce (= (at "confirmBurn" data) false) "You can't stake a wizard in burning queue")
            (enforce (contains 'spellbook data) "no spellbook data")
            (with-capability (OWNER sender idnft (at "owner" data))
                (with-default-read staked-table idnft
                    {"staked": false}
                    {"staked":= staked}
                    (enforce (= staked false) "this wizard is already staked")
                )
                (write staked-table idnft
                    {"idnft": idnft,
                    "account": sender,
                    "timestamp": (at "block-time" (chain-data)),
                    "multiplier": (length(at "spellbook" data)),
                    "staked": true}
                )
                (emit-event (STAKE_NFT idnft sender (length(at "spellbook" data))))
            )
        )
    )

    (defun claim-all-unstake-all (objects:list m:module{wizarena-interface-v2})
        (map
            (claim-all-and-unstake m)
            objects
        )
    )

    (defun claim-all-and-unstake (m:module{wizarena-interface-v2} obj:object)
        (let (
                (idnft (at "idnft" obj))
                (sender (at "sender" obj))
            )
            (unstake idnft sender m)
        )
    )

    (defun unstake (idnft:string sender:string m:module{wizarena-interface-v2})
        (enforce (= (at "chain-id" (chain-data)) "1") "Unstake is only for chain 1")
        (enforce (= (format "{}" [m]) "free.wiz-arena") "not allowed, security reason")
        (let (
                (data (get-nft-staked idnft))
                (data-wizard (get-wizard-data idnft m))
            )
            (enforce (= (at "staked" data) true) "Wizard already unstaked")
            (with-capability (OWNER sender idnft (at "owner" data-wizard))
                (with-read staked-table idnft
                    {"multiplier":= multiplier,
                    "timestamp":= stakedTime,
                    "account":= account
                    "idnft":=id
                    "staked":=staked}
                    (update staked-table idnft
                        {"staked": false}
                    )
                    (with-capability (PRIVATE)
                        (mine-from-stake account multiplier stakedTime)
                    )
                )
                (emit-event (UNSTAKE_NFT idnft sender))
            )
        )
    )

    (defun claim-all (objects:list m:module{wizarena-interface-v2})
        (map
            (claim-all-without-unstake m)
            objects
        )
    )

    (defun claim-all-without-unstake (m:module{wizarena-interface-v2} obj:object)
        (let (
                (idnft (at "idnft" obj))
                (sender (at "sender" obj))
            )
            (claim-without-unstake idnft sender m)
        )
    )

    (defun claim-without-unstake (idnft:string sender:string m:module{wizarena-interface-v2})
        (enforce (= (at "chain-id" (chain-data)) "1") "Claim is only for chain 1")
        (enforce (= (format "{}" [m]) "free.wiz-arena") "not allowed, security reason")
        (let (
                (data (get-wizard-data idnft m))
            )
            (with-capability (OWNER sender idnft (at "owner" data))
                (with-read staked-table idnft
                    {"multiplier":= multiplier,
                    "timestamp":= stakedTime,
                    "account":= account
                    "idnft":=id
                    "staked":=staked}
                    (with-capability (PRIVATE)
                        (mine-from-stake account multiplier stakedTime)
                    )
                    (update staked-table idnft
                        {"timestamp": (at "block-time" (chain-data))}
                    )
                )
            )
        )
    )

    ;;;; ADMIN REQUIRED
    (defun force-claim-all (items:list)
        (with-capability (ADMIN)
            (map
                (force-claim)
                items
            )
        )
    )

    (defun force-claim (idnft:string)
        (require-capability (ADMIN))
        (with-read staked-table idnft
            {"multiplier":= multiplier,
            "timestamp":= stakedTime,
            "account":= account
            "staked":=staked}
            (with-capability (PRIVATE)
                (mine-from-stake account multiplier stakedTime)
            )
            (update staked-table idnft
                {"timestamp": (at "block-time" (chain-data))}
            )
        )
    )


    (defun mine-from-stake (account:string multiplier:integer stakedTime:time)
        (require-capability (PRIVATE))
        (let (
                (days (/ (diff-time (at "block-time" (chain-data)) stakedTime) 86400))
                (guard (at "guard" (coin.details account)))
            )
            (with-default-read token-table account
              {"balance": 0.0,
              "guard": guard}
              {"balance":= oldbalance,
              "guard":= currentGuard}
              (let
                  (
                    (reward (calculate-reward days multiplier))
                    (total-mined (get-total-mined))
                )
                (enforce (<= (+ total-mined reward) MAXIMUM_SUPPLY) "Maximum Supply reached. Can't reward")
                (write token-table account {
                    "balance": (+ oldbalance reward),
                    "guard": guard})
                (write mined-wiza-table "" {"amount": (+ total-mined reward)})
                (emit-event (WIZA_REWARD_FROM_STAKE account reward))
              )
            )
        )
    )

    (defun mine-from-ap-burn (account:string idnft:string aptoburn:integer m:module{wizarena-interface-v2})
        (enforce (= (at "chain-id" (chain-data)) "1") "Ap burn is only for chain 1")
        (enforce (= (format "{}" [m]) "free.wiz-arena") "not allowed, security reason")
        (let (
                (data (get-wizard-data idnft m))
                (ap (at "ap" (get-wizard-data idnft m)))
                (guard (at "guard" (coin.details account)))
            )
            (enforce (>= ap aptoburn) "You don't have enough AP")
            (with-capability (OWNER account idnft (at "owner" data))
                (with-default-read token-table account
                  {"balance": 0.0,
                  "guard": guard}
                  {"balance":= oldbalance,
                  "guard":= currentGuard}
                  (let
                      (
                        (reward (* aptoburn 15.0))
                        (total-mined (get-total-mined))
                    )
                    (enforce (<= (+ total-mined reward) MAXIMUM_SUPPLY) "Maximum Supply reached. Can't reward")
                    (write token-table account {
                        "balance": (+ oldbalance reward),
                        "guard": guard})
                    (write mined-wiza-table "" {"amount": (+ total-mined reward)})
                    (m::spend-ap aptoburn account idnft)
                    (emit-event (WIZA_REWARD_AP_BURN account reward))
                  )
                )
            )
        )
    )

    (defun reward-users (accounts:list)
        (with-capability (ADMIN)
            (map
                (mine-from-reward)
                accounts
            )
        )
    )

    (defun mine-from-reward (reward-data:object)
        (require-capability (ADMIN))
        (let (
                (guard (at "guard" (coin.details (at "account" reward-data))))
                (account (at "account" reward-data))
            )
            (with-default-read token-table account
              {"balance": 0.0,
              "guard": guard}
              {"balance":= oldbalance,
              "guard":= currentGuard}
              (let
                  (
                    (total-mined (get-total-mined))
                    (amount (at "amount" reward-data))
                )
                (enforce (<= (+ total-mined amount) MAXIMUM_SUPPLY) "Maximum Supply reached. Can't reward")
                (write token-table account {
                    "balance": (+ oldbalance amount),
                    "guard": guard})
                (write mined-wiza-table "" {"amount": (+ total-mined amount)})
                (emit-event (WIZA_REWARD_GENERAL account amount))
              )
            )
        )
    )

    (defun spend-wiza:bool (amount:decimal account:string)
        (enforce (= (at "chain-id" (chain-data)) "1") "Spend is only for chain 1")
        (let (
                (balance (get-user-balance account))
            )
            (enforce (<= amount balance) "You don't have enough WIZA")
        )
        (with-capability (TRANSFER account WIZA_TOKEN_BANK amount)
            (with-default-read token-table account
              {"balance": 0.0}
              {"balance":= oldbalance}
              (update token-table account {
                  "balance": (- oldbalance amount)})
            )
            (with-default-read token-table WIZA_TOKEN_BANK
              {"balance": 0.0}
              {"balance":= oldbalance}
              (update token-table WIZA_TOKEN_BANK {
                  "balance": (+ oldbalance amount)})
            )
            (emit-event (SPEND_WIZA account amount))
        )
    )

    (defun get-wiza-from-bank-bulk (accounts:list)
        (with-capability (ADMIN)
            (map
                (get-wiza-from-bank)
                accounts
            )
        )
    )

    (defun get-wiza-from-bank (reward-data:object)
        (require-capability (ADMIN))
        (let (
                (account (at "account" reward-data))
                (amount (at "amount" reward-data))
            )
            (with-default-read token-table account
              {"balance": 0.0}
              {"balance":= oldbalance}
              (update token-table account {
                  "balance": (+ oldbalance amount)})
            )
            (with-default-read token-table WIZA_TOKEN_BANK
              {"balance": 0.0}
              {"balance":= oldbalance}
              (update token-table WIZA_TOKEN_BANK {
                  "balance": (- oldbalance amount)})
            )
        )
    )

    ; --------------------------------------------------------------------------
    ; helpers functions
    ; --------------------------------------------------------------------------
    (defun get-wizard-data (id:string m:module{wizarena-interface-v2})
        (enforce (= (at "chain-id" (chain-data)) "1") "Wizard data is only for chain 1")
        (enforce (= (format "{}" [m]) "free.wiz-arena") "not allowed, security reason")
        (m::get-wizard-fields-for-id (str-to-int id))
    )

    (defun get-nft-staked-mass (nfts:list)
        (map
            (get-nft-staked)
            nfts
        )
    )

    (defun get-nft-staked (idnft:string)
        (with-default-read staked-table idnft
            {"staked": false,
            "multiplier": 1,
            "timestamp":(at "block-time" (chain-data))}
            {"staked":= staked,
            "multiplier":=multiplier,
            "timestamp":= timestamp}
            {"staked":staked, "multiplier":multiplier, "timestamp":timestamp, "idnft":idnft}
        )
    )

    (defun check-nft-is-staked:bool (idnft:string)
        (with-default-read staked-table idnft
            {"staked": false}
            {"staked":= staked}
            staked
        )
    )

    (defun wizards-staked-count ()
        (length (select staked-table (where "staked" (= true))))
    )

    (defun get-unclaimed-mined-1 ()
        (let (
                (staked-nfts (select staked-table (where "staked" (= true))))
            )
            (map
                (get-unclaimed-mined-2)
                staked-nfts
            )
        )
    )

    (defun get-unclaimed-mined-2 (stakednft)
        (let (
                (days (/ (diff-time (at "block-time" (chain-data)) (at "timestamp" stakednft)) 86400))
                (multiplier (at "multiplier" stakednft))
            )
            (calculate-reward (ceiling days 4) multiplier)
        )
    )

    (defun calculate-reward-mass (nfts:list)
        (map
            (calculate-reward-mass-2)
            nfts
        )
    )

    (defun calculate-reward-mass-2 (idnft:string)
        (let (
                (data (get-nft-staked idnft))
            )
            (let (
                    (days (/ (diff-time (at "block-time" (chain-data)) (at "timestamp" data)) 86400))
                    (multiplier (at "multiplier" data))
                )
                (calculate-reward days multiplier)
            )
        )
    )

    (defun calculate-reward (days:decimal multiplier:integer)
        (let (
                (base-multiplier (at "multiplier" (read base-multiplier-table "" ['multiplier])))
            )
            (floor (* days (* multiplier base-multiplier)) DECIMALS)
        )
    )

    (defun get-total-mined ()
        @doc "get total WIZA mined"
        (at "amount" (read mined-wiza-table "" ['amount]))
    )

    (defun get-circulating-supply ()
        @doc "get total mined - wiza in bank"
        (- (get-total-mined) (get-balance WIZA_TOKEN_BANK))
    )

    (defun get-user-balance (address:string)
        (with-default-read token-table address
            {"balance": 0.0}
            {"balance":= balance}
            balance
        )
    )

    (defun set-multiplier (multiplier:decimal)
        (with-capability (ADMIN)
            (write base-multiplier-table ""
                {"multiplier": multiplier}
            )
        )
    )
)


