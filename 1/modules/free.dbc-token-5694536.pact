(module dbc-token GOVERNANCE

  @doc " 'DB COOPER' is a Token, NFT, and Gaming project proudly building on Kadena. "

  @model
    [ (defproperty conserves-mass (amount:decimal)
        (= (column-delta token-table 'balance) 0.0))

      (defproperty valid-account-id (accountId:string)
        (and
          (>= (length accountId) 3)
          (<= (length accountId) 256)))
    ]

  (implements fungible-v2)

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema token-schema
    @doc " An account, holding a token balance. \
         \ \
         \ ROW KEY: accountId. "
    balance:decimal
    guard:guard
  )
  (deftable token-table:{token-schema})


    (defschema wl-schema
        @doc "Basic schema used for WL members, keys are account ids"
        role:string
    )
    (deftable wl-table:{wl-schema})

    (defschema values-schema
        @doc "Basic schema used for storing basic values"
        value:string
    )
    (deftable values-table:{values-schema})

    (defschema counts-schema
        @doc "Basic schema used for counting things"
        count:decimal
    )
    (deftable counts-table:{counts-schema})


  ; --------------------------------------------------------------------------
  ; Capatilibites

  (defcap GOVERNANCE
    ()

    @doc " Give the admin full access to call and upgrade the module. "

    (enforce-keyset "free.dbcooper-keyset")
  )

  (defcap INTERNAL ()
    @doc "only for internal use"
    true
  )

  (defcap ACCOUNT_GUARD
    ( accountId:string )
    @doc " Look up the guard for an account, required to debit from that account. "
    (enforce-guard (at 'guard (read token-table accountId ['guard])))
  )

  (defcap DEBIT
    ( sender:string )

    @doc " Capability to perform debiting operations. "

    (enforce-guard (at 'guard (read token-table sender ['guard])))
    (enforce (!= sender "") "Invalid sender.")
  )

  (defcap CREDIT
    ( receiver:string )

    @doc " Capability to perform crediting operations. "

    (enforce (!= receiver "") "Invalid receiver.")
  )

  (defcap TRANSFER:bool
    ( sender:string
      receiver:string
      amount:decimal )

    @doc " Capability to perform transfer between two accounts. "

    @managed amount TRANSFER-mgr

    (enforce (!= sender receiver) "Sender cannot be the receiver.")
    (enforce-unit amount)
    (enforce (> amount 0.0) "Transfer amount must be positive.")
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver))
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal )

    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal
    )
  )

  ; --------------------------------------------------------------------------
  ; Constants

  (defconst ROOT_ACCOUNT_ID:string 'ROOT
    " ID for the account which initially owns all the tokens. ")

  (defconst ADMIN_ACCOUNT_ID "k:688b1b8531fcfa4b893cd8d7d787e39331746d3cece83523eef30fb21cbfb890"
    " ID for admin accont" )

  (defconst INITIAL_SUPPLY:decimal 1000000000000.0
    " Initial supply of 1 Trillion tokens.")

  (defconst DECIMALS 12
    " Specifies the minimum denomination for token transactions. ")

  (defconst ACCOUNT_ID_CHARSET CHARSET_LATIN1
    " Allowed character set for account IDs. ")

  (defconst ACCOUNT_ID_PROHIBITED_CHARACTER "$")

  (defconst ACCOUNT_ID_MIN_LENGTH 3
    " Minimum character length for account IDs. ")

  (defconst ACCOUNT_ID_MAX_LENGTH 256
    " Maximum character length for account IDs. ")

  (defconst MAIN_WL_KEY "main_wl_role"
    " Value to indicate main wl. ")

 (defconst PUBLIC_WL_KEY "public_wl_role"
    " Value to indicate public wl. ")

  (defconst WL_TABLE_KEY "wl_table_key"
    " Key for the wl value in the values table. ")

  (defconst PREMINE_SALE_AMOUNT:decimal 490000000000.0
    " Amount of tokens to sell during WL.")

  (defconst PREMINE_COUNT_KEY "premine_count_key"
    " Amount of tokens to sell during WL.")

  (defconst MIN_BUY_KDA:decimal 200.0
     " Minimum amount of KDA worth of tokens a user can buy. ")

  (defconst MAX_BUY_KDA:decimal 500.0
    " Maxinimum amount of KDA worth a user can buy. ")

  (defconst DBCS_PER_KDA 7000000.0
    " How much DBC tokens 1 KDA can buy. ")

  ; --------------------------------------------------------------------------
  ; Utilities

  (defun validate-account-id
    ( accountId:string )

    @doc " Enforce that an account ID meets charset and length requirements. "

    (enforce
      (is-charset ACCOUNT_ID_CHARSET accountId)
      (format
        "Account ID does not conform to the required charset: {}"
        [accountId]))

    (enforce
      (not (contains accountId ACCOUNT_ID_PROHIBITED_CHARACTER))
      (format "Account ID contained a prohibited character: {}" [accountId]))

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
          [accountId]))
    )
  )

  ;; ; --------------------------------------------------------------------------
  ;; ; Fungible-v2 Implementation

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
      (credit receiver receiver-guard amount)
    )
  )

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
      (transfer-create sender receiver guard amount)
    )
  )

  (defun debit
    ( accountId:string
      amount:decimal )

    @doc " Decrease an account balance. Internal use only. "

    @model [ (property (> amount 0.0))
             (property (valid-account-id accountId))
           ]

    (validate-account-id accountId)
    (if (= accountId ROOT_ACCOUNT_ID) (require-capability (INTERNAL)) true)
    (enforce (> amount 0.0) "Debit amount must be positive.")
    (enforce-unit amount)
    (require-capability (DEBIT accountId))

    (with-read token-table accountId
      { "balance" := balance }

      (enforce (<= amount balance) "Insufficient funds.")

      (update token-table accountId
        { "balance" : (- balance amount) }
      )
    )
  )

  (defun credit
    ( accountId:string
      guard:guard
      amount:decimal )

    @doc " Increase an account balance. Internal use only. "

    @model [ (property (> amount 0.0))
             (property (valid-account-id accountId))
           ]

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
          , "guard"   : retg
          }))
      ))

  (defun check-reserved:string (accountId:string)
    " Checks ACCOUNT for reserved name and returns type if \
    \ found or empty string. Reserved names start with a \
    \ single char and colon, e.g. 'c:foo', which would return 'c' as type."
    (let ((pfx (take 2 accountId)))
      (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

  (defun enforce-reserved:bool (accountId:string guard:guard)
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

  (defschema crosschain-schema
    @doc " Schema for yielded value in cross-chain transfers "
    receiver:string
    receiver-guard:guard
    amount:decimal
  )

  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )

    @model [ (property (> amount 0.0))
             (property (!= receiver ""))
             (property (valid-account-id sender))
             (property (valid-account-id receiver))
           ]

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
          (yield crosschain-details target-chain)
        )
      )
    )

    (step
      (resume
        { "receiver"       := receiver
        , "receiver-guard" := receiver-guard
        , "amount"         := amount
        }
        ;; Step 2 - credit receiver account on target chain
        (with-capability (CREDIT receiver)
          (credit receiver receiver-guard amount)
        )
      )
    )
  )

  (defun get-balance:decimal
    ( account:string )

    (at 'balance (read token-table account ['balance]))
  )

  (defun details:object{fungible-v2.account-details}
    ( account:string )

    (with-read token-table account
      { "balance" := balance
      , "guard"   := guard
      }
      { "account" : account
      , "balance" : balance
      , "guard"   : guard
      }
    )
  )

  (defun precision:integer
    ()

    DECIMALS
  )

  (defun enforce-unit:bool
    ( amount:decimal )

    @doc " Enforce the minimum denomination for token transactions. "

    (enforce
      (= (floor amount DECIMALS) amount)
      (format "Amount violates minimum denomination: {}" [amount])
    )
  )

  (defun create-account:string
    ( account:string
      guard:guard )

    @doc " Create a new account. "

    @model [ (property (valid-account-id account)) ]

    (validate-account-id account)
    (enforce-reserved account guard)

    (insert token-table account
      { "balance" : 0.0
      , "guard"   : guard
      }
    )
  )

  (defun rotate:string
    ( account:string
      new-guard:guard )

    (with-read token-table account
      { "guard" := oldGuard }

      (if (= account ROOT_ACCOUNT_ID) (require-capability (INTERNAL)) true)
      (enforce-guard oldGuard)
      (enforce-guard new-guard)

      (update token-table account
        { "guard" : new-guard }
      )
    )
  )

  ;; ; --------------------------------------------------------------------------
  ;; ; Custom Functions

  (defun initialize:string
    ()

    @doc " Initialize the contract. \
         \ Admin-only. Should fail if it has been called before. "

    (with-capability (GOVERNANCE)
      (create-account ROOT_ACCOUNT_ID (create-module-guard "root-account"))
      (update token-table ROOT_ACCOUNT_ID { "balance" : INITIAL_SUPPLY })
    )
  )

  (defun move-premine:string
    ( receiver:string
      guard:guard
      amount:decimal )

    @doc " Admin-only. Move the premine. "

    (with-capability (GOVERNANCE)
      (with-capability (INTERNAL)
        (install-capability (TRANSFER ROOT_ACCOUNT_ID receiver amount))
        (transfer-create ROOT_ACCOUNT_ID receiver guard amount)
      )
    )
  )

  (defun join-ido:string
    ( account:string
      guard:guard
      amount-kda:decimal )
    @doc " Function to let people buy the token. "
      (with-capability (INTERNAL)
        (let
          (
              (amount-dbcs (* amount-kda DBCS_PER_KDA ) )
          )
          (enforce-passes-wl account amount-dbcs)
          (increase-premined-amount amount-dbcs)
          (coin.transfer account ADMIN_ACCOUNT_ID amount-kda)
          (install-capability (TRANSFER ROOT_ACCOUNT_ID account amount-dbcs) )
          (transfer-create ROOT_ACCOUNT_ID account guard amount-dbcs )
        )
      )
  )

  (defun set-value(key:string value:string)
    @doc "Sets the value for a key to store in a table"
    (with-capability (GOVERNANCE)
        (write values-table key {"value": value} )
    )
  )

  (defun get-value (key:string)
    @doc "Gets value for a key"
    (at "value" (read values-table key ['value]))
  )

  (defun set-wl-role-for-user(user-id:string role:string)
  @doc "Sets the wl role for a user"
    (with-capability (GOVERNANCE)
        (write wl-table user-id  {"role": role} )
    )
  )

  (defun get-wl-role (user-id:string)
    @doc "Gets wl role for a user"
    ( with-default-read wl-table user-id {"role": ""} {"role" := role} role )
  )

  (defun increase-premined-amount(amount:decimal)
    @doc "Increase how many tokens were premined"
    (enforce-premine-amount-available amount)
    (require-capability (INTERNAL))
    (write counts-table PREMINE_COUNT_KEY
        {"count":
          (+ amount (get-premined-amount) )
        }
    )
  )

  (defun get-premined-amount ()
    @doc "Gets count for key"
    ( with-default-read counts-table PREMINE_COUNT_KEY {"count": 0.0} {"count" := count} count )

  )

  (defun enforce-premine-amount-available (amount:decimal)
    @doc " Enforces there's enough coins left available to pre-mine "
    (let
        (
            (premined-amount (get-premined-amount))
        )
        (enforce
            ( <= (+ amount premined-amount) PREMINE_SALE_AMOUNT )
            " Amount is unavailable to premine "
        )
    )
  )

  (defun enforce-passes-wl (user-id:string buy-amount:decimal)
    @doc "Gets wl role for a user"
    (let
        (
            (user-wl-role (get-wl-role user-id))
            (curr-wl-role (get-curr-wl-value ))
        )
        (if
            (!= PUBLIC_WL_KEY curr-wl-role)
            (enforce-wl-buy-conditions user-id buy-amount user-wl-role)
            "Public sale enabled"
        )
    )
  )

  (defun enforce-wl-buy-conditions (user-id:string buy-amount:decimal user-wl-role:string)
    @doc "Verifies wl buy conditions"
    ; Verify WL
    (enforce (= user-wl-role MAIN_WL_KEY) "User not white listed" )
    ; Verify balance within min max
    (let
        (
            (new-balance (+ buy-amount (safe-get-balance  user-id)) )
        )
        (enforce (>= new-balance (min-buy-dbcs)) " Must buy above minimum amount" )
        (enforce (<= new-balance (max-buy-dbcs)) " Cannot buy above maximum amount" )
    )
  )

  (defun safe-get-balance (user-id:string)
     @doc " Gets balance for user and defaults to 0 if not found "
        (try
            0
            (get-balance user-id)
        )
  )

  (defun min-buy-dbcs ()
    @doc " Minimum amount in DBCs a user can buy "
    (* MIN_BUY_KDA DBCS_PER_KDA)
  )

  (defun max-buy-dbcs ()
    @doc " Minimum amount in DBCs a user can buy "
    (* MAX_BUY_KDA DBCS_PER_KDA)
  )

  (defun get-curr-wl-value ()
    @doc "Gets current value of WL role that can mint"
    (at "value" (read values-table WL_TABLE_KEY ['value] ) )
  )
)

;(create-table token-table)
;(create-table wl-table)
;(create-table values-table)
;(create-table counts-table)
