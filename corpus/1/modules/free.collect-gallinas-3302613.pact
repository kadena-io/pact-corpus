(module collect-gallinas GOVERNANCE

  @doc "Kadena Gallinas - Gallina collectors game."
  @model
    [ (defproperty conserves-mass (amount:decimal)
        (= (column-delta gledger 'balance) 0.0))
      (defproperty valid-account-id (accountId:string)
        (and
          (>= (length accountId) 3)
          (<= (length accountId) 256)))
      (defproperty valid-gallina-name (petName:string)
        (and
          (>= (length petName) 3)
          (<= (length petName) 30))) ]

  (implements gallinas-poly-fungible-v1)

; --------------------------------------------------------------------------
; Schemas and tables
; --------------------------------------------------------------------------

  ;Account/token ledger
  (defschema entry
    id:string
    account:string
    balance:decimal
    guard:guard
    )
  ;Account/token ledger
  (deftable gledger:{entry})

  ;Supply/token table schema
  (defschema supply
    supply:decimal
  )

  ;Supply/token table
  (deftable supplies-table:{supply})

  ;Uri table schema
  (defschema uri-html
    uri:string
  )

  ;Uri table
  (deftable uri-table:{uri-html})

  ;gallina table schema
  (defschema gallina
    id:string
    name:string
    generation:integer
    gender:string
    mother-id:string
    father-id:string
    birthday:time
    next-breed-time:time
    special:integer
    gene-1:integer
    gene-1-p:integer
    gene-1-h1:integer
    gene-1-h2:integer
    gene-1-h3:integer
    gene-2:integer
    gene-2-p:integer
    gene-2-h1:integer
    gene-2-h2:integer
    gene-2-h3:integer
    gene-3:integer
    gene-3-p:integer
    gene-3-h1:integer
    gene-3-h2:integer
    gene-3-h3:integer
    gene-4:integer
    gene-4-p:integer
    gene-4-h1:integer
    gene-4-h2:integer
    gene-4-h3:integer
    gene-5:integer
    gene-5-p:integer
    gene-5-h1:integer
    gene-5-h2:integer
    gene-5-h3:integer
    gene-6:integer
    gene-6-p:integer
    gene-6-h1:integer
    gene-6-h2:integer
    gene-6-h3:integer
    gene-7:integer
    gene-7-p:integer
    gene-7-h1:integer
    gene-7-h2:integer
    gene-7-h3:integer
    gene-8:integer
    gene-8-p:integer
    gene-8-h1:integer
    gene-8-h2:integer
    gene-8-h3:integer
    gene-9:integer
    gene-9-p:integer
    gene-9-h1:integer
    gene-9-h2:integer
    gene-9-h3:integer
    gene-10:integer
    gene-10-p:integer
    gene-10-h1:integer
    gene-10-h2:integer
    gene-10-h3:integer
    gene-11:integer
    gene-11-p:integer
    gene-11-h1:integer
    gene-11-h2:integer
    gene-11-h3:integer
    gene-12:integer
    gene-12-p:integer
    gene-12-h1:integer
    gene-12-h2:integer
    gene-12-h3:integer
  )

  ;gallinas table
  (deftable gallinas-table:{gallina})

  ;total gallinas table schema - gallina count only (no eggs included)
  (defschema all-gallinas
    total-count:integer
  )

  ;total gallinas count table
  (deftable total-gallinas-table:{all-gallinas})

  ;gallina marketplace table schema
  (defschema gallinas-forsale
    id:string
    account:string
    price:decimal
    forsale:bool
  )

  ;gallina marketplace table
  (deftable marketplace-table:{gallinas-forsale})

; --------------------------------------------------------------------------
; Constants
; --------------------------------------------------------------------------

  ;Whole eggs and whole gallinas only
  (defconst MINIMUM_PRECISION 0
    " Specifies the minimum denomination for token transactions. ")

  (defconst EGG_PRICE 1.00
    " The cost of 1 Gallina Egg" )

  (defconst BREED_PRICE 0.05
    " The cost of 1 Gallina Egg" )

  (defconst GALLINA_BANK:string "gallinas-io-bank"
    " Official account that holds Eggs and Gallinas. ")

  (defconst ACCOUNT_ID_CHARSET CHARSET_LATIN1
    " Allowed character set for Account IDs. ")

  (defconst ACCOUNT_ID_MIN_LENGTH 3
    " Minimum character length for account IDs. ")

  (defconst ACCOUNT_ID_MAX_LENGTH 256
    " Maximum character length for account IDs. ")

  (defconst GALLINA_NAME_MIN_LENGTH 3
    " Minimum character length for account IDs. ")

  (defconst GALLINA_NAME_MAX_LENGTH 30
    " Maximum character length for account IDs. ")



; --------------------------------------------------------------------------
; Capatilibites
; --------------------------------------------------------------------------

  (defcap GOVERNANCE ()
    @doc " Give the admin full access to call and upgrade the module. "
    (enforce-keyset 'admin-gallina)
  )

  (defcap ACCOUNT_GUARD ( id:string account:string )
    @doc " Look up the guard for an account. "
    (enforce-guard
      (at 'guard
      (read gledger (key id account))))
  )

  (defcap DEBIT (id:string sender:string)
   @doc " Capability to perform debiting operations. "
    (enforce-guard
      (at 'guard
        (read gledger (key id sender))))
  )

  (defcap CREDIT (id:string receiver:string)
    @doc " Capability to perform crediting operations. "
    true)

  (defcap URI:bool (id:string uri:string)
    @doc " Emitted event when URI is changed "
    @event true
  )

  (defcap SUPPLY:bool (id:string supply:decimal)
    @doc " Emitted event when supply is changed "
    @event true
  )

  (defcap INTERNAL ()
    @doc "For Internal Use"
    true)

; --------------------------------------------------------------------------
; Utilities
; --------------------------------------------------------------------------

  (defun key ( id:string account:string )
    @doc " Returns id/account data structure "
    (format "{}:{}" [id account])
  )

  (defun get-ids ()
    @doc " Returns a list of all Gallina IDs "
    (keys supplies-table)
  )

  (defun set-uri (newuri:string)
  @doc " Changes URI "
  (with-capability (GOVERNANCE)
    (write uri-table ""
    {"uri":newuri}))
  )

  (defun set-special (gid:string special:integer)
    @doc " Sets a Gallinas Special Gene "
    (with-capability (GOVERNANCE)
    (update gallinas-table gid
    {"special":special}))
  )

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

  (defun validate-gallina-name ( petName:string )
    @doc " Enforce that a Gallina Name meets charset and length requirements. "
    (enforce
      (is-charset ACCOUNT_ID_CHARSET petName)
      (format
        "Gallina Name does not conform to the required charset: {}"
        [petName]))
    (let ((nameLength (length petName)))
      (enforce
        (>= nameLength GALLINA_NAME_MIN_LENGTH)
        (format
          "Gallina Name does not conform to the min length requirement: {}"
          [petName]))
      (enforce
        (<= nameLength ACCOUNT_ID_MAX_LENGTH)
        (format
          "Gallina Name does not conform to the max length requirement: {}"
          [petName]))))

  (defun enforce-valid-amount
    ( precision:integer
      amount:decimal
    )
    @doc " Enforces positive amounts "
    (enforce (> amount 0.0) "Positive non-zero amounts only.")
    (enforce-precision precision amount)
  )

  (defun enforce-precision
    ( precision:integer
      amount:decimal
    )
    @doc " Enforces whole numbers "
    (enforce
      (= (floor amount precision) amount)
      "Whole Eggs and Gallinas only.")
  )

  (defun enforce-valid-transfer
    ( sender:string
      receiver:string
      precision:integer
      amount:decimal)
      @doc " Enforces transfer rules "
    (enforce (!= sender receiver)
      "You must make a transfer to someone else besides your self.")
    (enforce-valid-amount precision amount)
    (enforce (= amount 1.0)
      "You may only transfer 1 Gallina or Egg at a time.")
    (validate-account-id sender)
    (validate-account-id receiver)
  )

  (defun reduce-under-10:integer
    ( reduceme:integer  )
    @doc " Reduces a gene to a value under 10 "
    (if (< reduceme 10) reduceme (let ((reduceme2 (/ reduceme 2)))
                                        (if (< reduceme2 10) reduceme2 (let ((reduceme3 (/ reduceme2 3)))
                                                                            (if (< reduceme3 10) reduceme3 (let ((reduceme4 (/ reduceme3 2)))
                                                                                                                  (if (< reduceme4 10) reduceme4 (let ((reduceme5 (/ reduceme4 3)))
                                                                                                                                                       (if (< reduceme5 10) reduceme5 1))))))))    )
  )

  (defun reduce-under-20:integer
    ( reduceme:integer  )
    @doc " Reduces a gene to a value under 20 "
    (if (< reduceme 20) reduceme (let ((reduceme2 (/ reduceme 2)))
                                        (if (< reduceme2 20) reduceme2 (let ((reduceme3 (/ reduceme2 3)))
                                                                            (if (< reduceme3 20) reduceme3 (let ((reduceme4 (/ reduceme3 2)))
                                                                                                                  (if (< reduceme4 20) reduceme4 (let ((reduceme5 (/ reduceme4 3)))
                                                                                                                                                       (if (< reduceme5 20) reduceme5 1))))))))    )
  )

  (defun coin-account-exists:bool (account:string)
    @doc "Returns true if account exists on coin contract"
	(try false
	     (let ((ok true))
		      (coin.details account)
			  ok)))

  (defun enforce-coin-account-exists (account:string)
    @doc "Enforces coin account existance"
     (let ((exist (coin-account-exists account)))
	      (enforce exist "Account does not exist in coin contract")))

  (defun coin-account-guard (account:string)
    @doc "Enforces coin account guard"
    (at "guard" (coin.details account)))

  (defun can-breed (id1:string id2:string)
    @doc "Returns true if two Gallinas can breed"
    (let ((ok true))
		      (enforce (!= id1 "Egg")  "Eggs dont breed." )
          (enforce (!= id2 "Egg")  "Eggs dont breed." )
          (with-read gallinas-table id1
            { "id" := gid1
            , "name" := name1
            , "generation" := generation1
            , "gender" := gender1
            , "mother-id" := mother-id1
            , "father-id" := father-id1
            , "birthday" := birthday1
            , "next-breed-time" := next-breed-time1
            }
            (with-read gallinas-table id2
            { "id" := gid2
            , "name" := name2
            , "generation" := generation2
            , "gender" := gender2
            , "mother-id" := mother-id2
            , "father-id" := father-id2
            , "birthday" := birthday2
            , "next-breed-time" := next-breed-time2
            }
            (enforce (!= gid1 gid2)  "Gallinas cannot breed by themselves.")
            (enforce (!= gid1 mother-id1)  "These Gallinas cannot breed with each other." )
            (enforce (!= gid1 father-id1)  "These Gallinas cannot breed with each other." )
            (enforce (!= gid2 mother-id2)  "These Gallinas cannot breed with each other." )
            (enforce (!= gid2 father-id2)  "These Gallinas cannot breed with each other." )
            (enforce (!= gender1 gender2)  "Gallinas must be of separate genders to breed." )
            (enforce (>= (round (/ (diff-time (at "block-time" (chain-data)) birthday1) 86400)) 7)  "Gallinas must be at least 7 days old to breed." )
            (enforce (>= (round (/ (diff-time (at "block-time" (chain-data)) birthday2) 86400)) 7)  "Gallinas must be at least 7 days old to breed." )
            (enforce (<= next-breed-time1 (at "block-time" (chain-data))) "One of your Gallinas is not ready to breed yet.")
            (enforce (<= next-breed-time2 (at "block-time" (chain-data))) "One of your Gallinas is not ready to breed yet.")
           ok)
           )
      		 )
  )

  (defun get-gallina-id-exists
    ( gid:string )
    @doc " Enforces a unique Gallina ID "
    (enforce (!= gid "Egg")  "Eggs dont get new IDs." )
    (with-default-read gallinas-table gid
        { 'id: "nonulls" }
        { 'id := id }
        (enforce (= "nonulls" id) "This Gallina ID already exists.")
    )
  )

  (defun get-promo-ids ()
    @doc " Get the list of Gallinas ids for promotions "
    (with-capability (GOVERNANCE)
      (select gledger ["id", "account"] (where "balance" (< 0.0)))
    )
  )

; --------------------------------------------------------------------------
; gallina-poly-fungible-v1 implementation
; --------------------------------------------------------------------------

  (defcap TRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @doc " Allows transfering of Gallina tokens "
    @managed amount TRANSFER-mgr
    (enforce-unit id amount)
    (enforce (= amount 1.0) "You may only transfer 1 Gallina or Egg at a time.")
    (compose-capability (DEBIT id sender))
    (compose-capability (CREDIT id receiver))
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    @doc " Transfer manager "

    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  (defun enforce-unit:bool (id:string amount:decimal)
    @doc " Enforces precision "
    (enforce
      (= (floor amount (precision id))
         amount)
      "Whole Eggs and Gallinas only.")
  )

  (defun create-account:string
    ( id:string
      account:string
      guard:guard
    )
    @doc " Creates an account "
    (validate-account-id account)

    (enforce-coin-account-exists account)
	  (let ((cur_guard (coin-account-guard account)))
    (enforce (= cur_guard guard) "Gallina account guards must match their coin contract account guards."))

    (insert gledger (key id account)
      { "balance" : 0.0
      , "guard"   : guard
      , "id" : "Egg"
      , "account" : account
      })
  )

  (defun get-balance:decimal (id:string account:string)
    @doc " Returns a users token balance "
    (at 'balance (read gledger (key id account)))
  )

  (defun details:object{gallinas-poly-fungible-v1.account-details}
    ( id:string account:string )
    @doc " Returns a tokens details "
    (read gledger (key id account))
  )

  (defun rotate:string (id:string account:string new-guard:guard)
    @doc " Safely rotates a users guard "
    (with-read gledger (key id account)
      { "guard" := old-guard }

      (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
      (enforce-guard old-guard)

      (update gledger (key id account)
        { "guard" : new-guard }))
  )

  (defun precision:integer (id:string)
    @doc " Enforces precision "
    MINIMUM_PRECISION)

  (defun transfer:string
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @doc " Transfer to an account, failing if the account does not exist. "
    @model [ (property (conserves-mass amount))
             (property (= amount 1.0))
             (property (valid-account-id sender))
             (property (valid-account-id receiver))
             (property (!= sender receiver)) ]

    (enforce (!= sender receiver)
      "You can only transfer to other accounts.")
    (enforce-valid-transfer sender receiver (precision id) amount)
    (with-capability (TRANSFER id sender receiver amount)
      (with-read gledger (key id receiver)
        { "guard" := g }
        (transfer-create id sender receiver g amount)
        )
      )
  )

  (defun transfer-create:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )
    @doc " Transfer to an account, creating it if it does not exist. "
    @model [ (property (conserves-mass amount))
             (property (= amount 1.0))
             (property (valid-account-id sender))
             (property (valid-account-id receiver))
             (property (!= sender receiver)) ]
    (enforce (!= sender receiver)
      "You can only transfer to other accounts.")

    (enforce-valid-transfer sender receiver (precision id) amount)
    (if (= sender GALLINA_BANK) (require-capability (INTERNAL)) true)

    (enforce-coin-account-exists receiver)
	  (let ((cur_guard (coin-account-guard receiver)))
    (enforce (= cur_guard receiver-guard) "Receiver guard must match their guard in the coin contract."))

    (with-capability (TRANSFER id sender receiver amount)
      (debit id sender amount)
      (credit id receiver receiver-guard amount))
  )

  ;Cross chain transfer unsupported
  (defpact transfer-crosschain:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
      @doc " Crosschain transfers are not supported "
    (step (format "{}" [(enforce false "Cross Chain transfers are not supported.")]))
    )

  (defun debit:string
    ( id:string
      account:string
      amount:decimal
    )
    @doc " Debits a token from an account "
    (require-capability (DEBIT id account))
    (enforce-unit id amount)
    (enforce (= amount 1.0)  "You can only debit whole Gallinas or Eggs." )
    (with-read gledger (key id account)
      { "balance" := balance }
      (enforce (<= amount balance) "Insufficient funds.")
      (update gledger (key id account)
        { "balance" : (- balance amount) }
        ))
    (with-default-read supplies-table id
      { 'supply: 0.0 }
      { 'supply := s }
      (write supplies-table id {'supply: (- s amount)}))
  )

  (defun credit:string
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )
    @doc " Credits a token to an account "
    (require-capability (CREDIT id account))
    (enforce-unit id amount)
    (enforce (= amount 1.0)  "You can only credit whole Gallinas or Eggs." )
    (with-default-read gledger (key id account)
      { "balance" : 0.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      (enforce (= retg guard)
        "Account guards do not match.")
      ;update market-place-table in case gallina was for sale
      (write marketplace-table id
          { "id"      : id
          , "account" : account
          , "price"   : 0.0
          , "forsale" : false
          }
      )
      (write gledger (key id account)
        { "balance" : (+ balance amount)
        , "guard"   : retg
        , "id"   : id
        , "account" : account
        })
      (with-default-read supplies-table id
      { 'supply: 0.0 }
      { 'supply := s }
      (write supplies-table id {'supply: (+ s amount)}))
    )
  )

  (defun total-supply:decimal (id:string)
    @doc " Returns total supply of a token "
    (with-default-read supplies-table id
      { 'supply : 0.0 }
      { 'supply := s }
    s)
  )

  (defun uri:string (id:string)
    @doc " Returns a tokens URI "
    (with-default-read uri-table ""
    {"uri":"http"}
    {"uri" := uri}
    (format "{}{}" [uri id]))
  )


; --------------------------------------------------------------------------
; Gallina contract functions
; --------------------------------------------------------------------------

  ;Eggs are always id "Egg"
  (defun buy-egg
    ( account:string
      guard:guard
      amount:decimal )
    @doc " Buy a Gallina Egg to Hatch a Gallina "
    ;Check current supply of Eggs
    (with-default-read supplies-table "Egg"
          { 'supply: 0.0 }
          { 'supply := egg-supply }
        ;Enforce rules
        (enforce (= amount EGG_PRICE)  "You can only purchase 1 Egg for 1 KDA at a time" )
        (validate-account-id account)
        (enforce (= "k:" (take 2 account)) "Only k: Prefixed Accounts")
        (enforce-coin-account-exists account)
    	  (let ((cur_guard (coin-account-guard account)))
        (enforce (= cur_guard guard) "Gallina account guards must match the same account guard in the coin contract."))
        ;Deposit the KDA into the contract
        (coin.transfer account GALLINA_BANK amount)
        ;Update account ledger with 1 egg
        (with-default-read gledger (key "Egg" account)
          { "balance" : 0.0 }
          { "balance" := balance }
          (write gledger (key "Egg" account)
          { "balance" : (+ balance 1.0)
          , "guard"   : guard
          , "id"   : "Egg"
          , "account" : account
          })
        )
        ;Increment egg supply
        (write supplies-table "Egg" {"supply": (+ egg-supply 1)})
        ;Return information
        (format "1 Egg Purchased for {} KDA." [amount])
    )
  )

  (defun hatch-egg
    ( account:string
      name:string )
    @doc " Hatch A Gallina From An Egg "
    @model [ (property (valid-account-id account))
             (property (valid-gallina-name name)) ]
    ;Get current supply of Eggs
    (with-default-read supplies-table "Egg"
          { 'supply: 0.0 }
          { 'supply := egg-supply }
          (with-default-read total-gallinas-table ""
               { "total-count" : 0 }
               { "total-count" := total-count}
               ;Enforce rules
               (validate-gallina-name name)
               (enforce (= "k:" (take 2 account)) "Only k: Prefixed Accounts.")
               (with-read gledger (key "Egg" account)
                    { "balance" := balance-egg
                    , "guard" := guard }
                    ;Enforce rules
                    (enforce (> balance-egg 0.0) "You dont have any Eggs to hatch.")
                    (with-capability (ACCOUNT_GUARD "Egg" account)
                      (enforce-guard guard)
                    ;Generate a new Gallina
                    (let ((gid
                      ;Generate a new id for the new gallina
                      (create-random-id name account)))
                      (get-gallina-id-exists gid)
                      (with-default-read gledger (key gid account)
                          { "balance" : 1.0, "guard" : guard }
                          { "balance" := balance, "guard" := retg }
                          (enforce (= retg guard)
                          "account guards do not match")
                          ;Update account ledger with new gallina
                          (write gledger (key gid account)
                          { "balance" : balance
                          , "guard"   : retg
                          , "id"   : gid
                          , "account" : account
                          })
                          ;Reduce total egg supply
                          (write supplies-table "Egg" {"supply": (- egg-supply 1)})
                          ;Reduce user egg balance
                          (update gledger (key "Egg" account)
                          { "balance" : (- balance-egg 1.0) }
                          )
                          ;Gather entropy
                          (let ((seed (abs(- (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data))))))) (str-to-int 64 (base64-encode (take 1 (at "prev-block-hash" (chain-data)))))))))
                                ;Spread entropy per gene
                                (let ((seedmap (map (- seed) [0
                                                        (str-to-int 64 (base64-encode (take 1 gid)))
                                                        (str-to-int 64 (base64-encode(take 1 (drop 1 gid))))
                                                        (str-to-int 64 (base64-encode(take 1 (drop 2 gid))))
                                                        (str-to-int 64 (base64-encode(take 1 (drop 3 gid))))
                                                        (str-to-int 64 (base64-encode(take 1 (drop 4 gid))))
                                                        (str-to-int 64 (base64-encode(take 1 (drop 5 gid))))
                                                        (str-to-int 64 (base64-encode(take 1 (drop 6 gid))))
                                                        (str-to-int 64 (base64-encode(take 1 (drop 7 gid))))
                                                        (str-to-int 64 (base64-encode(take 1 (drop 8 gid))))
                                                        (str-to-int 64 (base64-encode(take 1 (drop 9 gid))))
                                                        (str-to-int 64 (base64-encode(take 1 (drop 10 gid))))
                                                        ])))
                                                        ;Limit entropy range
                                                        (let (
                                                              (r0 (abs(at 0 seedmap)))
                                                              (r1 (abs(at 1 seedmap)))
                                                              (r2 (abs(at 2 seedmap)))
                                                              (r3 (abs(at 3 seedmap)))
                                                              (r4 (abs(at 4 seedmap)))
                                                              (r5 (abs(at 5 seedmap)))
                                                              (r6 (abs(at 6 seedmap)))
                                                              (r7 (abs(at 7 seedmap)))
                                                              (r8 (abs(at 8 seedmap)))
                                                              (r9 (abs(at 9 seedmap)))
                                                              (r10 (abs(at 10 seedmap)))
                                                              (r11 (abs(at 11 seedmap)))
                                                             );Cook egg and insert
                                                              (insert gallinas-table gid
                                                                { "id" : gid
                                                                , "name" : name
                                                                , "generation" : 0
                                                                , "gender" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1(hash (at "block-time" (chain-data)))))))) 2)0) "Male" "Female")
                                                                , "mother-id" : "Egg"
                                                                , "father-id" : "Egg"
                                                                , "birthday" : (at "block-time" (chain-data))
                                                                , "next-breed-time" : (at "block-time" (chain-data))
                                                                , "special": 0
                                                                , "gene-1": 0
                                                                , "gene-1-p" : (if (= r0 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r0 0) 2 (if (< r0 70) 1 0)))
                                                                , "gene-1-h1" : (if (= r0 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r0 0) 2 (if (< r0 70) 1 0)))
                                                                , "gene-1-h2" : (if (= r0 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r0 0) 2 (if (< r0 70) 1 0)))
                                                                , "gene-1-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 1 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 1 gid))))))2))
                                                                , "gene-2": 0
                                                                , "gene-2-p" : (if (= r1 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r1 0) 2 (if (< r1 70) 1 0)))
                                                                , "gene-2-h1" : (if (= r1 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r1 0) 2 (if (< r1 70) 1 0)))
                                                                , "gene-2-h2" : (if (= r1 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r1 0) 2 (if (< r1 70) 1 0)))
                                                                , "gene-2-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 2 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 2 gid))))))2))
                                                                , "gene-3": 0
                                                                , "gene-3-p" : (if (= r2 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r2 0) 2 (if (< r2 70) 1 0)))
                                                                , "gene-3-h1" : (if (= r2 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r2 0) 2 (if (< r2 70) 1 0)))
                                                                , "gene-3-h2" : (if (= r2 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r2 0) 2 (if (< r2 70) 1 0)))
                                                                , "gene-3-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 3 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 3 gid))))))2))
                                                                , "gene-4": 0
                                                                , "gene-4-p" : (if (= r3 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r3 0) 2 (if (< r3 70) 1 0)))
                                                                , "gene-4-h1" : (if (= r3 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r3 0) 2 (if (< r3 70) 1 0)))
                                                                , "gene-4-h2" : (if (= r3 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r3 0) 2 (if (< r3 70) 1 0)))
                                                                , "gene-4-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 4 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 4 gid))))))2))
                                                                , "gene-5": 0
                                                                , "gene-5-p" : (if (= r4 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r4 0) 2 (if (< r4 70) 1 0)))
                                                                , "gene-5-h1" : (if (= r4 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r4 0) 2 (if (< r4 70) 1 0)))
                                                                , "gene-5-h2" : (if (= r4 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r4 0) 2 (if (< r4 70) 1 0)))
                                                                , "gene-5-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 5 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 5 gid))))))2))
                                                                , "gene-6": 0
                                                                , "gene-6-p" : (if (= r5 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r5 0) 2 (if (< r5 70) 1 0)))
                                                                , "gene-6-h1" : (if (= r5 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r5 0) 2 (if (< r5 70) 1 0)))
                                                                , "gene-6-h2" : (if (= r5 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r5 0) 2 (if (< r5 70) 1 0)))
                                                                , "gene-6-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 6 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 6 gid))))))2))
                                                                , "gene-7": 0
                                                                , "gene-7-p" : (if (= r6 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r6 0) 2 (if (< r6 70) 1 0)))
                                                                , "gene-7-h1" : (if (= r6 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r6 0) 2 (if (< r6 70) 1 0)))
                                                                , "gene-7-h2" : (if (= r6 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r6 0) 2 (if (< r6 70) 1 0)))
                                                                , "gene-7-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 7 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 7 gid))))))2))
                                                                , "gene-8": 0
                                                                , "gene-8-p" : (if (= r7 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r7 0) 2 (if (< r7 70) 1 0)))
                                                                , "gene-8-h1" : (if (= r7 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r7 0) 2 (if (< r7 70) 1 0)))
                                                                , "gene-8-h2" : (if (= r7 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r7 0) 2 (if (< r7 70) 1 0)))
                                                                , "gene-8-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 8 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 8 gid))))))2))
                                                                , "gene-9": 0
                                                                , "gene-9-p" : (if (= r8 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r8 0) 2 (if (< r8 70) 1 0)))
                                                                , "gene-9-h1" : (if (= r8 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r8 0) 2 (if (< r8 70) 1 0)))
                                                                , "gene-9-h2" : (if (= r8 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r8 0) 2 (if (< r8 70) 1 0)))
                                                                , "gene-9-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 9 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 9 gid))))))2))
                                                                , "gene-10": 0
                                                                , "gene-10-p" : (if (= r9 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r9 0) 2 (if (< r9 70) 1 0)))
                                                                , "gene-10-h1" : (if (= r9 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r9 0) 2 (if (< r9 70) 1 0)))
                                                                , "gene-10-h2" : (if (= r9 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r9 0) 2 (if (< r9 70) 1 0)))
                                                                , "gene-10-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 10 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 10 gid))))))2))
                                                                , "gene-11": 0
                                                                , "gene-11-p" : (if (= r10 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r10 0) 2 (if (< r10 70) 1 0)))
                                                                , "gene-11-h1" : (if (= r10 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r10 0) 2 (if (< r10 70) 1 0)))
                                                                , "gene-11-h2" : (if (= r10 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r10 0) 2 (if (< r10 70) 1 0)))
                                                                , "gene-11-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 11 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 11 gid))))))2))
                                                                , "gene-12": 0
                                                                , "gene-12-p" : (if (= r11 77) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r11 0) 2 (if (< r11 70) 1 0)))
                                                                , "gene-12-h1" : (if (= r11 55) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r11 0) 2 (if (< r11 70) 1 0)))
                                                                , "gene-12-h2" : (if (= r11 66) (reduce-under-10 (str-to-int 64 (base64-encode (take 1 gid)))) (if (= r11 0) 2 (if (< r11 70) 1 0)))
                                                                , "gene-12-h3" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 12 gid))))))1) (-(abs(reduce-under-10 (str-to-int 64 (base64-encode (take 1 (drop 12 gid))))))2))
                                                              }
                                                              )
                                                                ;Update total gallina supply
                                                                (write total-gallinas-table "" {"total-count": (+ total-count 1)})
                                                                ;Update supply table with new token id and balance
                                                                (write supplies-table gid {"supply": 1.0})
                                                                ;Return result
                                                                (format "Hatched 1 new Gallina with the ID {} " [gid]))
                                                       )
                           )

                        )
                      )
                    )
               )
          )
     )
  )

  (defun sell-my-gallina ( id:string account:string price:decimal forsale:bool)
    @doc " Put a Gallina on the Market or update one already for sale "
    @model [ (property (> price 0.0))
             (property (valid-account-id account))
             (property (!= id "Egg"))]
    (enforce (!= id "Egg")  "Eggs cannot be sold here." )
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (with-read gledger (key id account)
      { 'id := l_id
      , 'account := l_account
      , 'balance := l_balance
      , 'guard := l_guard }
      ;Enforce rules
      (with-capability (ACCOUNT_GUARD id l_account)
        (enforce-guard l_guard)
        (enforce (= account l_account) "Account Owners dont match.")
        (enforce (> l_balance 0.0) "No Gallina found in account with that ID.")
        (enforce (> price 0.0)  "Positive decimal sell prices only." )
        ;Write marketplace table changes
        (write marketplace-table id
          { 'id: id
          , 'account: account
          , 'price: price
          , 'forsale: forsale}
        )
        ;Return result
        (if (= forsale true) (format "Gallina with ID {} is now for sale for {}" [id price]) (format "Gallina with ID {} is no longer for sale" [id]))
      )
    )
  )

  (defun buy-gallina-off-market
    ( id:string
      account:string
      guard:guard
      amount:decimal )
    @doc " Buy a Gallina off the Market "
    @model [ (property (> amount 0.0))
             (property (valid-account-id account)) ]
    ;Get current supply of Eggs
    (with-read marketplace-table id
          { 'id := m_id
          , 'account := m_account
          , 'price := m_price
          , 'forsale := m_forsale }
        ;Enforce rules
        (enforce (= m_forsale true)  "You can only purchase a Gallina that is for sale." )
        (enforce (= amount m_price) "Insufficient funds.")
        (enforce (!= account m_account) "You cannot buy your own Gallina.")
        (enforce (= "k:" (take 2 account)) "Only k: Prefixed Accounts.")
        (enforce-coin-account-exists account)
    	  (let ((cur_guard (coin-account-guard account)))
        (enforce (= cur_guard guard) "Gallina account guards must match the same account guard in the coin contract."))
        ;Deposit the KDA into the contract
        (coin.transfer account m_account amount)
        ;Update seller's gallina balance
        (update gledger (key id m_account)
          { "balance" : 0.0 })
          ;Write new gallina to reciever
          (write gledger (key id account)
            { "balance" : 1.0
            , "guard"   : guard
            , "id"   : id
            , "account" : account
            }
          )
          ;Update market-place-table with changes
          (update marketplace-table id
            { "id"      : id
            , "account" : account
            , "price"   : 0.0
            , "forsale" : false
            }
          )
        ;Return result
        (format " Purchased a Gallina with the ID {} for {} KDA " [id amount])
    )
  )

  (defun breed-gallinas
    ( gid1:string
      gid2:string
      account:string
      amount:decimal )
    @doc " Breed 2 Gallinas and Hatch The Egg "
    @model [ (property (= amount 1.0))
             (property (valid-account-id account))
             (property (!= gid1 "Egg"))
             (property (!= gid2 "Egg"))]
    (enforce (!= gid1 "Egg")  "Eggs dont breed." )
    (enforce (!= gid2 "Egg")  "Eggs dont breed." )
    ;Get both gallinas info
    (with-read gallinas-table gid1
      { "id" := id1
      , "name" := name1
      , "generation" := generation1
      , "gender" := gender1
      , "mother-id" := motherid1
      , "father-id" := fatherid1
      , "birthday" := birthday1
      , "next-breed-time" := next-breed-time1
      , "special" := special1
      , "gene-1" := gene-1-1
      , "gene-1-p" := gene-1-p-1
      , "gene-1-h1" := gene-1-h1-1
      , "gene-1-h2" := gene-1-h2-1
      , "gene-1-h3" := gene-1-h3-1
      , "gene-2" := gene-2-1
      , "gene-2-p" := gene-2-p-1
      , "gene-2-h1" := gene-2-h1-1
      , "gene-2-h2" := gene-2-h2-1
      , "gene-2-h3" := gene-2-h3-1
      , "gene-3" := gene-3-1
      , "gene-3-p" := gene-3-p-1
      , "gene-3-h1" := gene-3-h1-1
      , "gene-3-h2" := gene-3-h2-1
      , "gene-3-h3" := gene-3-h3-1
      , "gene-4" := gene-4-1
      , "gene-4-p" := gene-4-p-1
      , "gene-4-h1" := gene-4-h1-1
      , "gene-4-h2" := gene-4-h2-1
      , "gene-4-h3" := gene-4-h3-1
      , "gene-5" := gene-5-1
      , "gene-5-p" := gene-5-p-1
      , "gene-5-h1" := gene-5-h1-1
      , "gene-5-h2" := gene-5-h2-1
      , "gene-5-h3" := gene-5-h3-1
      , "gene-6" := gene-6-1
      , "gene-6-p" := gene-6-p-1
      , "gene-6-h1" := gene-6-h1-1
      , "gene-6-h2" := gene-6-h2-1
      , "gene-6-h3" := gene-6-h3-1
      , "gene-7" := gene-7-1
      , "gene-7-p" := gene-7-p-1
      , "gene-7-h1" := gene-7-h1-1
      , "gene-7-h2" := gene-7-h2-1
      , "gene-7-h3" := gene-7-h3-1
      , "gene-8" := gene-8-1
      , "gene-8-p" := gene-8-p-1
      , "gene-8-h1" := gene-8-h1-1
      , "gene-8-h2" := gene-8-h2-1
      , "gene-8-h3" := gene-8-h3-1
      , "gene-9" := gene-9-1
      , "gene-9-p" := gene-9-p-1
      , "gene-9-h1" := gene-9-h1-1
      , "gene-9-h2" := gene-9-h2-1
      , "gene-9-h3" := gene-9-h3-1
      , "gene-10" := gene-10-1
      , "gene-10-p" := gene-10-p-1
      , "gene-10-h1" := gene-10-h1-1
      , "gene-10-h2" := gene-10-h2-1
      , "gene-10-h3" := gene-10-h3-1
      , "gene-11" := gene-11-1
      , "gene-11-p" := gene-11-p-1
      , "gene-11-h1" := gene-11-h1-1
      , "gene-11-h2" := gene-11-h2-1
      , "gene-11-h3" := gene-11-h3-1
      , "gene-12" := gene-12-1
      , "gene-12-p" := gene-12-p-1
      , "gene-12-h1" := gene-12-h1-1
      , "gene-12-h2" := gene-12-h2-1
      , "gene-12-h3" := gene-12-h3-1
    };Get Gallina 2
      (with-read gallinas-table gid2
        { "id" := id2
        , "name" := name2
        , "generation" := generation2
        , "gender" := gender2
        , "mother-id" := motherid2
        , "father-id" := fatherid2
        , "birthday" := birthday2
        , "next-breed-time" := next-breed-time2
        , "special" := special2
        , "gene-1" := gene-1-2
        , "gene-1-p" := gene-1-p-2
        , "gene-1-h1" := gene-1-h1-2
        , "gene-1-h2" := gene-1-h2-2
        , "gene-1-h3" := gene-1-h3-2
        , "gene-2" := gene-2-2
        , "gene-2-p" := gene-2-p-2
        , "gene-2-h1" := gene-2-h1-2
        , "gene-2-h2" := gene-2-h2-2
        , "gene-2-h3" := gene-2-h3-2
        , "gene-3" := gene-3-2
        , "gene-3-p" := gene-3-p-2
        , "gene-3-h1" := gene-3-h1-2
        , "gene-3-h2" := gene-3-h2-2
        , "gene-3-h3" := gene-3-h3-2
        , "gene-4" := gene-4-2
        , "gene-4-p" := gene-4-p-2
        , "gene-4-h1" := gene-4-h1-2
        , "gene-4-h2" := gene-4-h2-2
        , "gene-4-h3" := gene-4-h3-2
        , "gene-5" := gene-5-2
        , "gene-5-p" := gene-5-p-2
        , "gene-5-h1" := gene-5-h1-2
        , "gene-5-h2" := gene-5-h2-2
        , "gene-5-h3" := gene-5-h3-2
        , "gene-6" := gene-6-2
        , "gene-6-p" := gene-6-p-2
        , "gene-6-h1" := gene-6-h1-2
        , "gene-6-h2" := gene-6-h2-2
        , "gene-6-h3" := gene-6-h3-2
        , "gene-7" := gene-7-2
        , "gene-7-p" := gene-7-p-2
        , "gene-7-h1" := gene-7-h1-2
        , "gene-7-h2" := gene-7-h2-2
        , "gene-7-h3" := gene-7-h3-2
        , "gene-8" := gene-8-2
        , "gene-8-p" := gene-8-p-2
        , "gene-8-h1" := gene-8-h1-2
        , "gene-8-h2" := gene-8-h2-2
        , "gene-8-h3" := gene-8-h3-2
        , "gene-9" := gene-9-2
        , "gene-9-p" := gene-9-p-2
        , "gene-9-h1" := gene-9-h1-2
        , "gene-9-h2" := gene-9-h2-2
        , "gene-9-h3" := gene-9-h3-2
        , "gene-10" := gene-10-2
        , "gene-10-p" := gene-10-p-2
        , "gene-10-h1" := gene-10-h1-2
        , "gene-10-h2" := gene-10-h2-2
        , "gene-10-h3" := gene-10-h3-2
        , "gene-11" := gene-11-2
        , "gene-11-p" := gene-11-p-2
        , "gene-11-h1" := gene-11-h1-2
        , "gene-11-h2" := gene-11-h2-2
        , "gene-11-h3" := gene-11-h3-2
        , "gene-12" := gene-12-2
        , "gene-12-p" := gene-12-p-2
        , "gene-12-h1" := gene-12-h1-2
        , "gene-12-h2" := gene-12-h2-2
        , "gene-12-h3" := gene-12-h3-2
      };Get ledger info for Gallina 1 and 2
        (with-read gledger (key gid1 account)
          { "balance" := balance1l
          , "guard"   := guard1l
          , "id"   := id1l
          , "account" := account1l }
          (with-read gledger (key gid2 account)
            { "balance" := balance2l
            , "guard"   := guard2l
            , "id"   := id2l
            , "account" := account2l }
            ;Enforce rules
            (with-capability (ACCOUNT_GUARD id1l account1l)
              (enforce-guard guard1l)
              (enforce (= guard1l guard2l)  "You can only breed Gallinas you currently own." )
              (enforce (> balance1l 0.0)  "You can only breed Gallinas you currently own." )
              (enforce (> balance2l 0.0)  "You can only breed Gallinas you currently own." )
              (enforce (!= gid1 gid2)  "A Gallina cannot breed with itself." )
              (enforce (!= gid1 motherid1)  "These Gallinas cannot breed with each other." )
              (enforce (!= gid1 fatherid1)  "These Gallinas cannot breed with each other." )
              (enforce (!= gid2 motherid2)  "These Gallinas cannot breed with each other." )
              (enforce (!= gid2 fatherid2)  "These Gallinas cannot breed with each other." )
              (enforce (!= gender1 gender2)  "Gallinas must be of separate genders to breed." )
              (enforce (> amount 0.0)  "The cost to breed Gallinas is 0.05 KDA." )
              (enforce (= amount BREED_PRICE)  "The cost to breed Gallinas is 0.05 KDA." )
              (enforce (>= (round (/ (diff-time (at "block-time" (chain-data)) birthday1) 86400)) 7)  "Gallinas must be at least 7 days old to breed." )
              (enforce (>= (round (/ (diff-time (at "block-time" (chain-data)) birthday2) 86400)) 7)  "Gallinas must be at least 7 days old to breed." )
              (enforce (<= next-breed-time1 (at "block-time" (chain-data))) "One of your Gallinas is not ready to breed yet.")
              (enforce (<= next-breed-time2 (at "block-time" (chain-data))) "One of your Gallinas is not ready to breed yet.")
              (enforce (= "k:" (take 2 account)) "Only k: Prefixed Accounts")
              ;Transfer the 0.05 breeding fee
              (coin.transfer account GALLINA_BANK amount)
              ;Source entropy
              (let ((seed (abs(- (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data))))))) (str-to-int 64 (base64-encode (take 1 (at "prev-block-hash" (chain-data)))))))))
                                  ;Spread entropy per gene
                                  (let ((seedmap (map (- seed) [(str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1 (hash (at "block-time" (chain-data)))))))) 2)0) id2 id1))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -2 (hash (at "block-time" (chain-data)))))))) 2)0) id1 id2))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -4 (hash (at "block-time" (chain-data)))))))) 2)0) id2 id1))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -5 (hash (at "block-time" (chain-data)))))))) 2)0) id1 id2))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -7 (hash (at "block-time" (chain-data)))))))) 2)0) id2 id1))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -8 (hash (at "block-time" (chain-data)))))))) 2)0) id1 id2))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -10 (hash (at "block-time" (chain-data)))))))) 2)0) id2 id1))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -11 (hash (at "block-time" (chain-data)))))))) 2)0) id1 id2))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -13 (hash (at "block-time" (chain-data)))))))) 2)0) id2 id1))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -14 (hash (at "block-time" (chain-data)))))))) 2)0) id1 id2))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -16 (hash (at "block-time" (chain-data)))))))) 2)0) id2 id1))))
                                                          (str-to-int 64 (base64-encode(take 1 (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -17 (hash (at "block-time" (chain-data)))))))) 2)0) id1 id2))))
                                                          ])))
                                                          ;Limit entropy + create and confirm new gallina id
                                                          (let (
                                                                (r0 (abs(at 0 seedmap)))
                                                                (r1 (abs(at 1 seedmap)))
                                                                (r2 (abs(at 2 seedmap)))
                                                                (r3 (abs(at 3 seedmap)))
                                                                (r4 (abs(at 4 seedmap)))
                                                                (r5 (abs(at 5 seedmap)))
                                                                (r6 (abs(at 6 seedmap)))
                                                                (r7 (abs(at 7 seedmap)))
                                                                (r8 (abs(at 8 seedmap)))
                                                                (r9 (abs(at 9 seedmap)))
                                                                (r10 (abs(at 10 seedmap)))
                                                                (r11 (abs(at 11 seedmap)))
                                                                (newid (create-random-id (+ gid1 gid2) account ))
                                                               )
                                                               (get-gallina-id-exists newid)
                                                               ;Cook a new gallina salad and insert
                                                                (insert gallinas-table newid
                                                                  { "id" : newid
                                                                  , "name" : newid
                                                                  , "generation" : (if (> generation1 generation2) (+ generation1 1) (+ generation2 1))
                                                                  , "gender" : (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1(hash (at "block-time" (chain-data)))))))) 2)0) "Male" "Female")
                                                                  , "mother-id" : (if (= gender1 "Female") gid1 gid2 )
                                                                  , "father-id" : (if (= gender1 "Male") gid1 gid2 )
                                                                  , "birthday" : (at "block-time" (chain-data))
                                                                  , "next-breed-time" : (add-time (at "block-time" (chain-data)) (days 7))
                                                                  , "special" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r0 77) (special1) 0) (if (= r0 0) (special2) 0))
                                                                  , "gene-1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r0 77) (+ gene-1-1 1) gene-1-1) (if (= r0 77) (+ gene-1-2 1) gene-1-2))
                                                                  , "gene-1-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r0 77) gene-1-h3-1 (if (= r0 0) gene-1-h2-1 (if (< r0 50) gene-1-h1-1 gene-1-p-1))) (if (= r0 77) gene-1-h3-2 (if (= r0 0) gene-1-h2-2 (if (< r0 50) gene-1-h1-2 gene-1-p-2))))
                                                                  , "gene-1-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r0 66) (reduce-under-20 (+ gene-1-h1-1 gene-1-h1-2)) (if (= r0 1) (reduce-under-20 (+ gene-1-h1-1 gene-1-h1-1)) (if (< r0 50) (reduce-under-20 (+ gene-1-h1-1 1)) gene-1-h1-1))) (if (= r0 66) (reduce-under-20 (+ gene-1-h1-1 gene-1-h1-2)) (if (= r0 1) (reduce-under-20 (+ gene-1-h1-2 gene-1-h1-2)) (if (< r0 50) (reduce-under-20 (+ gene-1-h1-2 1)) gene-1-h1-2))))
                                                                  , "gene-1-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r0 88) (reduce-under-20 (+ gene-1-h2-1 gene-1-h2-2)) (if (= r0 2) (reduce-under-20 (+ gene-1-h2-1 gene-1-h2-1)) (if (> r0 50) (reduce-under-20 (+ gene-1-h2-1 1)) gene-1-h2-1))) (if (= r0 88) (reduce-under-20 (+ gene-1-h2-1 gene-1-h2-2)) (if (= r0 2) (reduce-under-20 (+ gene-1-h2-2 gene-1-h2-2)) (if (> r0 50) (reduce-under-20 (+ gene-1-h2-2 1)) gene-1-h2-2))))
                                                                  , "gene-1-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-1-h3-1 gene-1-h3-2))2)) (reduce-under-20 (/ (abs(- gene-1-h3-1 gene-1-h3-2))2)) )
                                                                  , "gene-2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r1 77) (+ gene-2-1 1) gene-2-1) (if (= r1 77) (+ gene-2-2 1) gene-2-2))
                                                                  , "gene-2-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r1 77) gene-2-h3-1 (if (= r1 0) gene-2-h2-1 (if (< r1 50) gene-2-h1-1 gene-2-p-1))) (if (= r1 77) gene-2-h3-2 (if (= r1 0) gene-2-h2-2 (if (< r1 50) gene-2-h1-2 gene-2-p-2))))
                                                                  , "gene-2-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r1 66) (reduce-under-20 (+ gene-2-h1-1 gene-2-h1-2)) (if (= r1 1) (reduce-under-20 (+ gene-2-h1-1 gene-2-h1-1)) (if (< r1 50) (reduce-under-20 (+ gene-2-h1-1 1)) gene-2-h1-1))) (if (= r1 66) (reduce-under-20 (+ gene-2-h1-1 gene-2-h1-2)) (if (= r1 1) (reduce-under-20 (+ gene-2-h1-2 gene-2-h1-2)) (if (< r1 50) (reduce-under-20 (+ gene-2-h1-2 1)) gene-2-h1-2))))
                                                                  , "gene-2-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r1 88) (reduce-under-20 (+ gene-2-h2-1 gene-2-h2-2)) (if (= r1 2) (reduce-under-20 (+ gene-2-h2-1 gene-2-h2-1)) (if (> r1 50) (reduce-under-20 (+ gene-2-h2-1 1)) gene-2-h2-1))) (if (= r1 88) (reduce-under-20 (+ gene-2-h2-1 gene-2-h2-2)) (if (= r1 2) (reduce-under-20 (+ gene-2-h2-2 gene-2-h2-2)) (if (> r1 50) (reduce-under-20 (+ gene-2-h2-2 1)) gene-2-h2-2))))
                                                                  , "gene-2-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-2-h3-1 gene-2-h3-2))2)) (reduce-under-20 (/ (abs(- gene-2-h3-1 gene-2-h3-2))2)) )
                                                                  , "gene-3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r2 77) (+ gene-3-1 1) gene-3-1) (if (= r2 77) (+ gene-3-2 1) gene-3-2))
                                                                  , "gene-3-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r2 77) gene-3-h3-1 (if (= r2 0) gene-3-h2-1 (if (< r2 50) gene-3-h1-1 gene-3-p-1))) (if (= r2 77) gene-3-h3-2 (if (= r2 0) gene-3-h2-2 (if (< r2 50) gene-3-h1-2 gene-3-p-2))))
                                                                  , "gene-3-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r2 66) (reduce-under-20 (+ gene-3-h1-1 gene-3-h1-2)) (if (= r2 1) (reduce-under-20 (+ gene-3-h1-1 gene-3-h1-1)) (if (< r2 50) (reduce-under-20 (+ gene-3-h1-1 1)) gene-3-h1-1))) (if (= r2 66) (reduce-under-20 (+ gene-3-h1-1 gene-3-h1-2)) (if (= r2 1) (reduce-under-20 (+ gene-3-h1-2 gene-3-h1-2)) (if (< r2 50) (reduce-under-20 (+ gene-3-h1-2 1)) gene-3-h1-2))))
                                                                  , "gene-3-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r2 88) (reduce-under-20 (+ gene-3-h2-1 gene-3-h2-2)) (if (= r2 2) (reduce-under-20 (+ gene-3-h2-1 gene-3-h2-1)) (if (> r2 50) (reduce-under-20 (+ gene-3-h2-1 1)) gene-3-h2-1))) (if (= r2 88) (reduce-under-20 (+ gene-3-h2-1 gene-3-h2-2)) (if (= r2 2) (reduce-under-20 (+ gene-3-h2-2 gene-3-h2-2)) (if (> r2 50) (reduce-under-20 (+ gene-3-h2-2 1)) gene-3-h2-2))))
                                                                  , "gene-3-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-3-h3-1 gene-3-h3-2))2)) (reduce-under-20 (/ (abs(- gene-3-h3-1 gene-3-h3-2))2)) )
                                                                  , "gene-4" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r3 77) (+ gene-4-1 1) gene-4-1) (if (= r3 77) (+ gene-4-2 1) gene-4-2))
                                                                  , "gene-4-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r3 77) gene-4-h3-1 (if (= r3 0) gene-4-h2-1 (if (< r3 50) gene-4-h1-1 gene-4-p-1))) (if (= r3 77) gene-4-h3-2 (if (= r3 0) gene-4-h2-2 (if (< r3 50) gene-4-h1-2 gene-4-p-2))))
                                                                  , "gene-4-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r3 66) (reduce-under-20 (+ gene-4-h1-1 gene-4-h1-2)) (if (= r3 1) (reduce-under-20 (+ gene-4-h1-1 gene-4-h1-1)) (if (< r3 50) (reduce-under-20 (+ gene-4-h1-1 1)) gene-4-h1-1))) (if (= r3 66) (reduce-under-20 (+ gene-4-h1-1 gene-4-h1-2)) (if (= r3 1) (reduce-under-20 (+ gene-4-h1-2 gene-4-h1-2)) (if (< r3 50) (reduce-under-20 (+ gene-4-h1-2 1)) gene-4-h1-2))))
                                                                  , "gene-4-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r3 88) (reduce-under-20 (+ gene-4-h2-1 gene-4-h2-2)) (if (= r3 2) (reduce-under-20 (+ gene-4-h2-1 gene-4-h2-1)) (if (> r3 50) (reduce-under-20 (+ gene-4-h2-1 1)) gene-4-h2-1))) (if (= r3 88) (reduce-under-20 (+ gene-4-h2-1 gene-4-h2-2)) (if (= r3 2) (reduce-under-20 (+ gene-4-h2-2 gene-4-h2-2)) (if (> r3 50) (reduce-under-20 (+ gene-4-h2-2 1)) gene-4-h2-2))))
                                                                  , "gene-4-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-4-h3-1 gene-4-h3-2))2)) (reduce-under-20 (/ (abs(- gene-4-h3-1 gene-4-h3-2))2)) )
                                                                  , "gene-5" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r4 77) (+ gene-5-1 1) gene-5-1) (if (= r4 77) (+ gene-5-2 1) gene-5-2))
                                                                  , "gene-5-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r4 77) gene-5-h3-1 (if (= r4 0) gene-5-h2-1 (if (< r4 50) gene-5-h1-1 gene-5-p-1))) (if (= r4 77) gene-5-h3-2 (if (= r4 0) gene-5-h2-2 (if (< r4 50) gene-5-h1-2 gene-5-p-2))))
                                                                  , "gene-5-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r4 66) (reduce-under-20 (+ gene-5-h1-1 gene-5-h1-2)) (if (= r4 1) (reduce-under-20 (+ gene-5-h1-1 gene-5-h1-1)) (if (< r4 50) (reduce-under-20 (+ gene-5-h1-1 1)) gene-5-h1-1))) (if (= r4 66) (reduce-under-20 (+ gene-5-h1-1 gene-5-h1-2)) (if (= r4 1) (reduce-under-20 (+ gene-5-h1-2 gene-5-h1-2)) (if (< r4 50) (reduce-under-20 (+ gene-5-h1-2 1)) gene-5-h1-2))))
                                                                  , "gene-5-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r4 88) (reduce-under-20 (+ gene-5-h2-1 gene-5-h2-2)) (if (= r4 2) (reduce-under-20 (+ gene-5-h2-1 gene-5-h2-1)) (if (> r4 50) (reduce-under-20 (+ gene-5-h2-1 1)) gene-5-h2-1))) (if (= r4 88) (reduce-under-20 (+ gene-5-h2-1 gene-5-h2-2)) (if (= r4 2) (reduce-under-20 (+ gene-5-h2-2 gene-5-h2-2)) (if (> r4 50) (reduce-under-20 (+ gene-5-h2-2 1)) gene-5-h2-2))))
                                                                  , "gene-5-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-5-h3-1 gene-5-h3-2))2)) (reduce-under-20 (/ (abs(- gene-5-h3-1 gene-5-h3-2))2)) )
                                                                  , "gene-6" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r5 77) (+ gene-6-1 1) gene-6-1) (if (= r5 77) (+ gene-6-2 1) gene-6-2))
                                                                  , "gene-6-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r5 77) gene-6-h3-1 (if (= r5 0) gene-6-h2-1 (if (< r5 50) gene-6-h1-1 gene-6-p-1))) (if (= r5 77) gene-6-h3-2 (if (= r5 0) gene-6-h2-2 (if (< r5 50) gene-6-h1-2 gene-6-p-2))))
                                                                  , "gene-6-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r5 66) (reduce-under-20 (+ gene-6-h1-1 gene-6-h1-2)) (if (= r5 1) (reduce-under-20 (+ gene-6-h1-1 gene-6-h1-1)) (if (< r5 50) (reduce-under-20 (+ gene-6-h1-1 1)) gene-6-h1-1))) (if (= r5 66) (reduce-under-20 (+ gene-6-h1-1 gene-6-h1-2)) (if (= r5 1) (reduce-under-20 (+ gene-6-h1-2 gene-6-h1-2)) (if (< r5 50) (reduce-under-20 (+ gene-6-h1-2 1)) gene-6-h1-2))))
                                                                  , "gene-6-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r5 88) (reduce-under-20 (+ gene-6-h2-1 gene-6-h2-2)) (if (= r5 2) (reduce-under-20 (+ gene-6-h2-1 gene-6-h2-1)) (if (> r5 50) (reduce-under-20 (+ gene-6-h2-1 1)) gene-6-h2-1))) (if (= r5 88) (reduce-under-20 (+ gene-6-h2-1 gene-6-h2-2)) (if (= r5 2) (reduce-under-20 (+ gene-6-h2-2 gene-6-h2-2)) (if (> r5 50) (reduce-under-20 (+ gene-6-h2-2 1)) gene-6-h2-2))))
                                                                  , "gene-6-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-6-h3-1 gene-6-h3-2))2)) (reduce-under-20 (/ (abs(- gene-6-h3-1 gene-6-h3-2))2)) )
                                                                  , "gene-7" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r6 77) (+ gene-7-1 1) gene-7-1) (if (= r6 77) (+ gene-7-2 1) gene-7-2))
                                                                  , "gene-7-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r6 77) gene-7-h3-1 (if (= r6 0) gene-7-h2-1 (if (< r6 50) gene-7-h1-1 gene-7-p-1))) (if (= r6 77) gene-7-h3-2 (if (= r6 0) gene-7-h2-2 (if (< r6 50) gene-7-h1-2 gene-7-p-2))))
                                                                  , "gene-7-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r6 66) (reduce-under-20 (+ gene-7-h1-1 gene-7-h1-2)) (if (= r6 1) (reduce-under-20 (+ gene-7-h1-1 gene-7-h1-1)) (if (< r6 50) (reduce-under-20 (+ gene-7-h1-1 1)) gene-7-h1-1))) (if (= r6 66) (reduce-under-20 (+ gene-7-h1-1 gene-7-h1-2)) (if (= r6 1) (reduce-under-20 (+ gene-7-h1-2 gene-7-h1-2)) (if (< r6 50) (reduce-under-20 (+ gene-7-h1-2 1)) gene-7-h1-2))))
                                                                  , "gene-7-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r6 88) (reduce-under-20 (+ gene-7-h2-1 gene-7-h2-2)) (if (= r6 2) (reduce-under-20 (+ gene-7-h2-1 gene-7-h2-1)) (if (> r6 50) (reduce-under-20 (+ gene-7-h2-1 1)) gene-7-h2-1))) (if (= r6 88) (reduce-under-20 (+ gene-7-h2-1 gene-7-h2-2)) (if (= r6 2) (reduce-under-20 (+ gene-7-h2-2 gene-7-h2-2)) (if (> r6 50) (reduce-under-20 (+ gene-7-h2-2 1)) gene-7-h2-2))))
                                                                  , "gene-7-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-7-h3-1 gene-7-h3-2))2)) (reduce-under-20 (/ (abs(- gene-7-h3-1 gene-7-h3-2))2)) )
                                                                  , "gene-8" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r7 77) (+ gene-8-1 1) gene-8-1) (if (= r7 77) (+ gene-8-2 1) gene-8-2))
                                                                  , "gene-8-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r7 77) gene-8-h3-1 (if (= r7 0) gene-8-h2-1 (if (< r7 50) gene-8-h1-1 gene-8-p-1))) (if (= r7 77) gene-8-h3-2 (if (= r7 0) gene-8-h2-2 (if (< r7 50) gene-8-h1-2 gene-8-p-2))))
                                                                  , "gene-8-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r7 66) (reduce-under-20 (+ gene-8-h1-1 gene-8-h1-2)) (if (= r7 1) (reduce-under-20 (+ gene-8-h1-1 gene-8-h1-1)) (if (< r7 50) (reduce-under-20 (+ gene-8-h1-1 1)) gene-8-h1-1))) (if (= r7 66) (reduce-under-20 (+ gene-8-h1-1 gene-8-h1-2)) (if (= r7 1) (reduce-under-20 (+ gene-8-h1-2 gene-8-h1-2)) (if (< r7 50) (reduce-under-20 (+ gene-8-h1-2 1)) gene-8-h1-2))))
                                                                  , "gene-8-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r7 88) (reduce-under-20 (+ gene-8-h2-1 gene-8-h2-2)) (if (= r7 2) (reduce-under-20 (+ gene-8-h2-1 gene-8-h2-1)) (if (> r7 50) (reduce-under-20 (+ gene-8-h2-1 1)) gene-8-h2-1))) (if (= r7 88) (reduce-under-20 (+ gene-8-h2-1 gene-8-h2-2)) (if (= r7 2) (reduce-under-20 (+ gene-8-h2-2 gene-8-h2-2)) (if (> r7 50) (reduce-under-20 (+ gene-8-h2-2 1)) gene-8-h2-2))))
                                                                  , "gene-8-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-8-h3-1 gene-8-h3-2))2)) (reduce-under-20 (/ (abs(- gene-8-h3-1 gene-8-h3-2))2)) )
                                                                  , "gene-9" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r8 77) (+ gene-9-1 1) gene-9-1) (if (= r8 77) (+ gene-9-2 1) gene-9-2))
                                                                  , "gene-9-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r8 77) gene-9-h3-1 (if (= r8 0) gene-9-h2-1 (if (< r8 50) gene-9-h1-1 gene-9-p-1))) (if (= r8 77) gene-9-h3-2 (if (= r8 0) gene-9-h2-2 (if (< r8 50) gene-9-h1-2 gene-9-p-2))))
                                                                  , "gene-9-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r8 66) (reduce-under-20 (+ gene-9-h1-1 gene-9-h1-2)) (if (= r8 1) (reduce-under-20 (+ gene-9-h1-1 gene-9-h1-1)) (if (< r8 50) (reduce-under-20 (+ gene-9-h1-1 1)) gene-9-h1-1))) (if (= r8 66) (reduce-under-20 (+ gene-9-h1-1 gene-9-h1-2)) (if (= r8 1) (reduce-under-20 (+ gene-9-h1-2 gene-9-h1-2)) (if (< r8 50) (reduce-under-20 (+ gene-9-h1-2 1)) gene-9-h1-2))))
                                                                  , "gene-9-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r8 88) (reduce-under-20 (+ gene-9-h2-1 gene-9-h2-2)) (if (= r8 2) (reduce-under-20 (+ gene-9-h2-1 gene-9-h2-1)) (if (> r8 50) (reduce-under-20 (+ gene-9-h2-1 1)) gene-9-h2-1))) (if (= r8 88) (reduce-under-20 (+ gene-9-h2-1 gene-9-h2-2)) (if (= r8 2) (reduce-under-20 (+ gene-9-h2-2 gene-9-h2-2)) (if (> r8 50) (reduce-under-20 (+ gene-9-h2-2 1)) gene-9-h2-2))))
                                                                  , "gene-9-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-9-h3-1 gene-9-h3-2))2)) (reduce-under-20 (/ (abs(- gene-9-h3-1 gene-9-h3-2))2)) )
                                                                  , "gene-10" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r9 77) (+ gene-10-1 1) gene-10-1) (if (= r9 77) (+ gene-10-2 1) gene-10-2))
                                                                  , "gene-10-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r9 77) gene-10-h3-1 (if (= r9 0) gene-10-h2-1 (if (< r9 50) gene-10-h1-1 gene-10-p-1))) (if (= r9 77) gene-10-h3-2 (if (= r9 0) gene-10-h2-2 (if (< r9 50) gene-10-h1-2 gene-10-p-2))))
                                                                  , "gene-10-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r9 66) (reduce-under-20 (+ gene-10-h1-1 gene-10-h1-2)) (if (= r9 1) (reduce-under-20 (+ gene-10-h1-1 gene-10-h1-1)) (if (< r9 50) (reduce-under-20 (+ gene-10-h1-1 1)) gene-10-h1-1))) (if (= r9 66) (reduce-under-20 (+ gene-10-h1-1 gene-10-h1-2)) (if (= r9 1) (reduce-under-20 (+ gene-10-h1-2 gene-10-h1-2)) (if (< r9 50) (reduce-under-20 (+ gene-10-h1-2 1)) gene-10-h1-2))))
                                                                  , "gene-10-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r9 88) (reduce-under-20 (+ gene-10-h2-1 gene-10-h2-2)) (if (= r9 2) (reduce-under-20 (+ gene-10-h2-1 gene-10-h2-1)) (if (> r9 50) (reduce-under-20 (+ gene-10-h2-1 1)) gene-10-h2-1))) (if (= r9 88) (reduce-under-20 (+ gene-10-h2-1 gene-10-h2-2)) (if (= r9 2) (reduce-under-20 (+ gene-10-h2-2 gene-10-h2-2)) (if (> r9 50) (reduce-under-20 (+ gene-10-h2-2 1)) gene-10-h2-2))))
                                                                  , "gene-10-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-10-h3-1 gene-10-h3-2))2)) (reduce-under-20 (/ (abs(- gene-10-h3-1 gene-10-h3-2))2)) )
                                                                  , "gene-11" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r10 77) (+ gene-11-1 1) gene-11-1) (if (= r10 77) (+ gene-11-2 1) gene-11-2))
                                                                  , "gene-11-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r10 77) gene-11-h3-1 (if (= r10 0) gene-11-h2-1 (if (< r10 50) gene-11-h1-1 gene-11-p-1))) (if (= r10 77) gene-11-h3-2 (if (= r10 0) gene-11-h2-2 (if (< r10 50) gene-11-h1-2 gene-11-p-2))))
                                                                  , "gene-11-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r10 66) (reduce-under-20 (+ gene-11-h1-1 gene-11-h1-2)) (if (= r10 1) (reduce-under-20 (+ gene-11-h1-1 gene-11-h1-1)) (if (< r10 50) (reduce-under-20 (+ gene-11-h1-1 1)) gene-11-h1-1))) (if (= r10 66) (reduce-under-20 (+ gene-11-h1-1 gene-11-h1-2)) (if (= r10 1) (reduce-under-20 (+ gene-11-h1-2 gene-11-h1-2)) (if (< r10 50) (reduce-under-20 (+ gene-11-h1-2 1)) gene-11-h1-2))))
                                                                  , "gene-11-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r10 88) (reduce-under-20 (+ gene-11-h2-1 gene-11-h2-2)) (if (= r10 2) (reduce-under-20 (+ gene-11-h2-1 gene-11-h2-1)) (if (> r10 50) (reduce-under-20 (+ gene-11-h2-1 1)) gene-11-h2-1))) (if (= r10 88) (reduce-under-20 (+ gene-11-h2-1 gene-11-h2-2)) (if (= r10 2) (reduce-under-20 (+ gene-11-h2-2 gene-11-h2-2)) (if (> r10 50) (reduce-under-20 (+ gene-11-h2-2 1)) gene-11-h2-2))))
                                                                  , "gene-11-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-11-h3-1 gene-11-h3-2))2)) (reduce-under-20 (/ (abs(- gene-11-h3-1 gene-11-h3-2))2)) )
                                                                  , "gene-12" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r11 77) (+ gene-12-1 1) gene-12-1) (if (= r11 77) (+ gene-12-2 1) gene-12-2))
                                                                  , "gene-12-p" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r11 77) gene-12-h3-1 (if (= r11 0) gene-12-h2-1 (if (< r11 50) gene-12-h1-1 gene-12-p-1))) (if (= r11 77) gene-12-h3-2 (if (= r11 0) gene-12-h2-2 (if (< r11 50) gene-12-h1-2 gene-12-p-2))))
                                                                  , "gene-12-h1" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (if (= r11 66) (reduce-under-20 (+ gene-12-h1-1 gene-12-h1-2)) (if (= r11 1) (reduce-under-20 (+ gene-12-h1-1 gene-12-h1-1)) (if (< r11 50) (reduce-under-20 (+ gene-12-h1-1 1)) gene-12-h1-1))) (if (= r11 66) (reduce-under-20 (+ gene-12-h1-1 gene-12-h1-2)) (if (= r11 1) (reduce-under-20 (+ gene-12-h1-2 gene-12-h1-2)) (if (< r11 50) (reduce-under-20 (+ gene-12-h1-2 1)) gene-12-h1-2))))
                                                                  , "gene-12-h2" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday2)) 1)) 2)0) (if (= r11 88) (reduce-under-20 (+ gene-12-h2-1 gene-12-h2-2)) (if (= r11 2) (reduce-under-20 (+ gene-12-h2-1 gene-12-h2-1)) (if (> r11 50) (reduce-under-20 (+ gene-12-h2-1 1)) gene-12-h2-1))) (if (= r11 88) (reduce-under-20 (+ gene-12-h2-1 gene-12-h2-2)) (if (= r11 2) (reduce-under-20 (+ gene-12-h2-2 gene-12-h2-2)) (if (> r11 50) (reduce-under-20 (+ gene-12-h2-2 1)) gene-12-h2-2))))
                                                                  , "gene-12-h3" : (if(=(mod (round (/ (abs (diff-time (at "block-time" (chain-data)) birthday1)) 1)) 2)0) (reduce-under-20 (/ (abs(+ gene-12-h3-1 gene-12-h3-2))2)) (reduce-under-20 (/ (abs(- gene-12-h3-1 gene-12-h3-2))2)) )
                                                                }
                                                                )
                                                                ;Update total gallina supply
                                                                (with-read total-gallinas-table ""
                                                                  {"total-count":=total-count}
                                                                  (write total-gallinas-table "" {"total-count": (+ total-count 1)})
                                                                  ;Update supply table with new token id and balance
                                                                  (write supplies-table newid {"supply": 1.0})
                                                                  ;Update user account ledger with 1 new Gallina
                                                                  (write gledger (key newid account)
                                                                  { "balance" : 1.0
                                                                  , "guard"   : guard1l
                                                                  , "id"   : newid
                                                                  , "account" : account
                                                                  })
                                                                  ;Update next-breed-time for bother parents
                                                                  (update gallinas-table gid1
                                                                    {
                                                                      "next-breed-time" : (add-time (at "block-time" (chain-data)) (days (* 3 generation1)))
                                                                    }
                                                                  )
                                                                  (update gallinas-table gid2
                                                                    {
                                                                      "next-breed-time" : (add-time (at "block-time" (chain-data)) (days (* 3 generation2)))
                                                                    }
                                                                  )
                                                                  ;Mark both parents as not for sale on the market in case they were
                                                                  (write marketplace-table gid1
                                                                      { "id"      : gid1
                                                                      , "account" : account
                                                                      , "price"   : 0.0
                                                                      , "forsale" : false
                                                                      }
                                                                  )
                                                                  (write marketplace-table gid2
                                                                      { "id"      : gid2
                                                                      , "account" : account
                                                                      , "price"   : 0.0
                                                                      , "forsale" : false
                                                                      }
                                                                  )
                                                                  ;Display result
                                                                  (format " Hatched 1 new Gallina with ID {} from parents {} and {} " [newid name1 name2])
                                                                )
                                                          )
                      )
                  )
              )
          )
       )
      )
    )
  )

  (defun create-random-id:string
    ( name:string
      account:string
    )
    @doc " Creates a unique ID for a newly hatched Gallina "
    (let ((x (base64-encode name))
          (y (base64-encode account))
          (z (base64-encode (format-time "%c" (at "block-time" (chain-data))))))
          (hash (+ x (+ y z)))
    )
  )

  (defun change-gallina-name
    ( gid:string
      account:string
      newname:string
    )
    @doc " Change your Gallinas name "
    @model [ (property (valid-account-id account))
             (property (valid-gallina-name newname))
             (property (!= gid "Egg")) ]
    (enforce (!= gid "Egg")  "Eggs dont have names." )
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (with-read gledger (key gid account)
      { "balance" := balance, "guard" := guard }
      (with-capability (ACCOUNT_GUARD gid account)
        (enforce-guard guard)
        (enforce (> balance 0.0) "You can only rename a Gallina that you currently own.")
        (validate-gallina-name newname)
        (update gallinas-table gid
        { "name" : newname })
        (format "Changed the name of Gallina with the ID {} to {}" [gid newname])
      )
    )
  )

  (defun get-total-gallinas:integer
    ()
    @doc " Get the total number of Gallinas "
    (with-default-read total-gallinas-table ""
      { 'total-count : 0 }
      { 'total-count := count }
      count)
  )

  (defun get-total-eggs:integer
    ()
    @doc " Get the total number of Eggs "
    (with-default-read supplies-table "Egg"
      { 'supply : 0.0 }
      { 'supply := count }
      count)
  )

  (defun get-gallina-details
    ( id:string )
    @doc " Get the details of a Gallina "
    (enforce (!= id "Egg")  "Eggs dont have details." )
    (with-read gallinas-table id
      { "id" := gid
      , "name" := name
      , "generation" := generation
      , "gender" := gender
      , "mother-id" := mother-id
      , "father-id" := father-id
      , "birthday" := birthday
      , "next-breed-time" := next-breed-time
      , "special" := special
      , "gene-1" := gene-1
      , "gene-1-p" := gene-1-p
      , "gene-1-h1" := gene-1-h1
      , "gene-1-h2" := gene-1-h2
      , "gene-1-h3" := gene-1-h3
      , "gene-2" := gene-2
      , "gene-2-p" := gene-2-p
      , "gene-2-h1" := gene-2-h1
      , "gene-2-h2" := gene-2-h2
      , "gene-2-h3" := gene-2-h3
      , "gene-3" := gene-3
      , "gene-3-p" := gene-3-p
      , "gene-3-h1" := gene-3-h1
      , "gene-3-h2" := gene-3-h2
      , "gene-3-h3" := gene-3-h3
      , "gene-4" := gene-4
      , "gene-4-p" := gene-4-p
      , "gene-4-h1" := gene-4-h1
      , "gene-4-h2" := gene-4-h2
      , "gene-4-h3" := gene-4-h3
      , "gene-5" := gene-5
      , "gene-5-p" := gene-5-p
      , "gene-5-h1" := gene-5-h1
      , "gene-5-h2" := gene-5-h2
      , "gene-5-h3" := gene-5-h3
      , "gene-6" := gene-6
      , "gene-6-p" := gene-6-p
      , "gene-6-h1" := gene-6-h1
      , "gene-6-h2" := gene-6-h2
      , "gene-6-h3" := gene-6-h3
      , "gene-7" := gene-7
      , "gene-7-p" := gene-7-p
      , "gene-7-h1" := gene-7-h1
      , "gene-7-h2" := gene-7-h2
      , "gene-7-h3" := gene-7-h3
      , "gene-8" := gene-8
      , "gene-8-p" := gene-8-p
      , "gene-8-h1" := gene-8-h1
      , "gene-8-h2" := gene-8-h2
      , "gene-8-h3" := gene-8-h3
      , "gene-9" := gene-9
      , "gene-9-p" := gene-9-p
      , "gene-9-h1" := gene-9-h1
      , "gene-9-h2" := gene-9-h2
      , "gene-9-h3" := gene-9-h3
      , "gene-10" := gene-10
      , "gene-10-p" := gene-10-p
      , "gene-10-h1" := gene-10-h1
      , "gene-10-h2" := gene-10-h2
      , "gene-10-h3" := gene-10-h3
      , "gene-11" := gene-11
      , "gene-11-p" := gene-11-p
      , "gene-11-h1" := gene-11-h1
      , "gene-11-h2" := gene-11-h2
      , "gene-11-h3" := gene-11-h3
      , "gene-12" := gene-12
      , "gene-12-p" := gene-12-p
      , "gene-12-h1" := gene-12-h1
      , "gene-12-h2" := gene-12-h2
      , "gene-12-h3" := gene-12-h3
      }
      { "id" : gid
      , "name" : name
      , "generation" : generation
      , "gender" : gender
      , "mother-id" : mother-id
      , "father-id" : father-id
      , "birthday" : birthday
      , "next-breed-time" : next-breed-time
      , "special" : special
      , "gene-1": gene-1
      , "gene-1-p": gene-1-p
      , "gene-1-h1": gene-1-h1
      , "gene-1-h2": gene-1-h2
      , "gene-1-h3": gene-1-h3
      , "gene-2": gene-2
      , "gene-2-p": gene-2-p
      , "gene-2-h1": gene-2-h1
      , "gene-2-h2": gene-2-h2
      , "gene-2-h3": gene-2-h3
      , "gene-3": gene-3
      , "gene-3-p": gene-3-p
      , "gene-3-h1": gene-3-h1
      , "gene-3-h2": gene-3-h2
      , "gene-3-h3": gene-3-h3
      , "gene-4": gene-4
      , "gene-4-p": gene-4-p
      , "gene-4-h1": gene-4-h1
      , "gene-4-h2": gene-4-h2
      , "gene-4-h3": gene-4-h3
      , "gene-5": gene-5
      , "gene-5-p": gene-5-p
      , "gene-5-h1": gene-5-h1
      , "gene-5-h2": gene-5-h2
      , "gene-5-h3": gene-5-h3
      , "gene-6": gene-6
      , "gene-6-p": gene-6-p
      , "gene-6-h1": gene-6-h1
      , "gene-6-h2": gene-6-h2
      , "gene-6-h3": gene-6-h3
      , "gene-7": gene-7
      , "gene-7-p": gene-7-p
      , "gene-7-h1": gene-7-h1
      , "gene-7-h2": gene-7-h2
      , "gene-7-h3": gene-7-h3
      , "gene-8": gene-8
      , "gene-8-p": gene-8-p
      , "gene-8-h1": gene-8-h1
      , "gene-8-h2": gene-8-h2
      , "gene-8-h3": gene-8-h3
      , "gene-9": gene-9
      , "gene-9-p": gene-9-p
      , "gene-9-h1": gene-9-h1
      , "gene-9-h2": gene-9-h2
      , "gene-9-h3": gene-9-h3
      , "gene-10": gene-10
      , "gene-10-p": gene-10-p
      , "gene-10-h1": gene-10-h1
      , "gene-10-h2": gene-10-h2
      , "gene-10-h3": gene-10-h3
      , "gene-11": gene-11
      , "gene-11-p": gene-11-p
      , "gene-11-h1": gene-11-h1
      , "gene-11-h2": gene-11-h2
      , "gene-11-h3": gene-11-h3
      , "gene-12": gene-12
      , "gene-12-p": gene-12-p
      , "gene-12-h1": gene-12-h1
      , "gene-12-h2": gene-12-h2
      , "gene-12-h3": gene-12-h3
      } )
  )

  (defun get-user-gallinas
    ( account:string )
    @doc " Get a list of Gallinas owned by a user "
    @model [ (property (valid-account-id account)) ]
      (select gledger ['id]
        (and? (where 'account (= account))
          (where 'balance (< 0.0))))
  )

  (defun get-gallina-owner
    ( id:string )
    @doc " Get the owner of a Gallina "
    (select gledger ['account]
        (and? (where 'id (= id))
          (where 'balance (< 0.0))))
  )

  (defun get-gallinas-for-sale ()
    @doc " Get the list of Gallinas currently for sale "
    (sort ['price] (select marketplace-table ["id", "price", "account"] (where "forsale" (= true))))
  )

; --------------------------------------------------------------------------
; Initialization
; --------------------------------------------------------------------------

  (defun initialize ()
    @doc " Initialize the contract. Can only happen once. "
    (coin.create-account GALLINA_BANK (create-module-guard "gallina-holdings"))
    (create-account "Egg" GALLINA_BANK (create-module-guard "gallina-holdings"))
  )

)

; --------------------------------------------------------------------------
; Create tables and initialize
; --------------------------------------------------------------------------

;(create-table free.collect-gallinas.gledger)
;(create-table free.collect-gallinas.supplies-table)
;(create-table free.collect-gallinas.uri-table)
;(create-table free.collect-gallinas.gallinas-table)
;(create-table free.collect-gallinas.total-gallinas-table)
;(create-table free.collect-gallinas.marketplace-table)
;(free.collect-gallinas.initialize)

