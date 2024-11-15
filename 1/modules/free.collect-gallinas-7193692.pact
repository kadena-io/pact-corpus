(module collect-gallinas GOVERNANCE

  @doc "Kadena Gallinas - Gallina collectors game."

  (implements gallinas-poly-fungible-v1)
  (use free.collect-gallinas-util)

; --------------------------------------------------------------------------
; Schemas and tables
; --------------------------------------------------------------------------

  (defschema entry
    id:string
    account:string
    balance:decimal
    guard:guard
  )

  (deftable gledger:{entry})

  (defschema supply
    supply:decimal
  )

  (deftable supplies-table:{supply})

  (defschema uri-html
    uri:string
  )

  (deftable uri-table:{uri-html})

  (defschema gallina-generation generation:integer)
  (deftable gallinas-generation-table:{gallina-generation})

  (defschema gallina-gender gender:string)
  (deftable gallinas-gender-table:{gallina-gender})

  (defschema gallina-motherid motherid:string)
  (deftable gallinas-motherid-table:{gallina-motherid})

  (defschema gallina-fatherid fatherid:string)
  (deftable gallinas-fatherid-table:{gallina-fatherid})

  (defschema gallina-birthday birthday:time)
  (deftable gallinas-birthday-table:{gallina-birthday})

  (defschema gallina-nextbreed nextbreed:time)
  (deftable gallinas-nextbreed-table:{gallina-nextbreed})

  (defschema gallina-special special:integer)
  (deftable gallinas-special-table:{gallina-special})

  (defschema all-gallinas
    total-count:integer
  )

  (deftable total-gallinas-table:{all-gallinas})

  (defschema gallinasiomarketoffer
    account:string
    forsale:bool
    price:decimal
  )

  (deftable gallinasio-marketplace:{gallinasiomarketoffer})

  (defschema gift-voucher
    balance:decimal
  )

  (deftable gift-voucher-table:{gift-voucher})

  (defschema gift-schedule
    gift:integer
    start:time
    end:time
  )

  (deftable gift-release-table:{gift-schedule})

  (defschema set-breed
    count:integer
  )

  (deftable set-breed-table:{set-breed})

  (defschema current-gift
    gift:integer
  )

  (deftable current-gift-table:{current-gift})

; --------------------------------------------------------------------------
; Constants
; --------------------------------------------------------------------------

  (defconst EGG_PRICE 1.00
    " The cost of 1 Gallina Egg" )

  (defconst BREED_PRICE 1.00
    " The cost of 1 Gallina Egg" )

  (defconst GALLINA_BANK:string "gallinas-io-bank"
    " Official account that holds Eggs and Gallinas. ")

  (defconst MAX_SUPPLY 500
    " The max supply of the first Gallina Set" )

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
    true
  )

  (defcap INTERNAL ()
    @doc "For Internal Use"
    true
  )

  (defcap MARKET (id:string account:string)
    @doc " Capability to perform market operations. "
    (enforce-guard
      (at 'guard
      (read gledger (key id account))))
    (let ((gbalance  (at 'balance
        (read gledger (key id account)))  ))
  (enforce (> gbalance 0.0) "You can only update Gallinas you own."))
  )

  (defcap URI:bool (id:string uri:string)
    @doc " Emitted event when URI is changed "
    @event true
  )

  (defcap SUPPLY:bool (id:string supply:decimal)
    @doc " Emitted event when supply is changed "
    @event true
  )

  (defcap GALLINASIO_BUY_EGG (id:string account:string)
    @doc " Emitted event when an Egg is purchased "
    @event true
  )

  (defcap GALLINASIO_HATCH (id:string account:string name:string gender:string birthday:time nextbreed:time)
    @doc " Emitted event when an Egg is hatched "
    @event true
  )

  (defcap GALLINASIO_BREED (account:string gid1:string gid2:string newid:string gender:string birthday:time nextbreed1:time nextbreed2:time nextbreed3:time special:integer generation:integer)
    @doc " Emitted event when Gallinas breed "
    @event true
  )

  (defcap GALLINASIO_SELL (id:string account:string price:decimal forsale:bool)
    @doc " Emitted event when a Gallina is sold "
    @event true
  )

  (defcap GALLINASIO_BUY (id:string buyer:string seller:string price:decimal)
    @doc " Emitted event when a Gallina is purchased "
    @event true
  )

  (defcap GALLINASIO_TRANSFER (id:string sender:string receiver:string)
    @doc " Emitted event when a Gallina is transfered "
    @event true
  )

  (defcap GALLINASIO_BURN (id:string account:string)
    @doc " Emitted event when a Gallina is burned "
    @event true
  )

  (defcap GALLINASIO_QUICK (id:string account:string gene:integer parent:integer)
    @doc " Emitted event during Gallina breeding "
    @event true
  )

  (defcap GALLINASIO_GIFT (id:string account:string gift:integer)
    @doc " Emitted event when a Gallina receives a gift "
    @event true
  )

  (defcap GALLINASIO_REMINT (id:string account:string birthday:time gender:string)
    @doc " Emitted event when a Gallina is reminted "
    @event true
  )

  (defcap GALLINASIO_NAME (id:string account:string name:string)
    @doc " Emitted event when a Gallina is named "
    @event true
  )

  (defcap GALLINASIO_RESOLVE (id:string account:string generation:integer gender:string motherid:string fatherid:string birthday:time nextbreed:time special:integer)
    @doc " Emitted event when a Gallina is resolved "
    @event true
  )

; --------------------------------------------------------------------------
; Utilities
; --------------------------------------------------------------------------

  (defun get-ids ()
    @doc " Returns a list of all Gallina IDs "
    (keys supplies-table)
  )

  (defun get-ids-special ()
    @doc " Returns a list of all Gallina IDs "
    (keys gallinas-special-table)
  )

  (defun setkey:string (
    set:integer)
    @doc " Returns setkey structure "
    (format "{}" [set])
  )

  (defun get-set-breed-count
    ( key:string )
    @doc " Gets count of Gallinas breeds in a set "
    (with-default-read set-breed-table key
              { "count" : 0 }
              { "count" := setcount }
              setcount
            )
  )

  (defun set-uri (newuri:string)
  @doc " Changes URI "
  (with-capability (GOVERNANCE)
    (write uri-table ""
    {"uri":newuri})
    (emit-event (URI "ALL" newuri)))
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

  (defun set-special-promo (gid:string special:integer generation:integer)
    @doc " Sets a Gallinas Special Gene For Promotions "
    (with-capability (GOVERNANCE)
    (update gallinas-special-table gid {"special":special})
    (update gallinas-generation-table gid {"generation":generation})
    (with-default-read set-breed-table (setkey special)
           { "count" : 0 }
           { "count" := countset}
          (write set-breed-table (setkey special) {"count": (+ countset 1)})
    )
    (with-default-read set-breed-table (setkey 0)
           { "count" : 0 }
           { "count" := countset0}
          (write set-breed-table (setkey 0) {"count": (if (= countset0 0) 0 (- countset0 1))})
    )
    )
  )

  (defun set-vouchers (id:string amount:decimal)
    @doc " Sets a Gallinas Vouchers for Promotions "
    (with-capability (GOVERNANCE)
    (with-default-read gift-voucher-table id
           { "balance" : 0.0 }
           { "balance" := gvbalance}
          (write gift-voucher-table id {"balance": (+ gvbalance amount)})
    )
    )
  )

  (defun set-setbreed-count (gene:string count:integer)
    @doc " Sets Gallina Breed Count "
    (with-capability (GOVERNANCE)
    (write set-breed-table gene {"count": count})
    )
  )

  (defun key ( id:string account:string )
    @doc " Returns id/account data structure "
    (format "{}:{}" [id account])
  )

  (defun coin-account-exists:bool (account:string)
    @doc "Returns true if account exists on coin contract"
	(try false
	     (let ((ok true))
		      (coin.details account)
			  ok))
  )

  (defun get-gallina-id-exists
    ( gid:string )
    @doc " Enforces a unique Gallina ID "
    (enforce (!= gid "Egg")  "Eggs dont get new IDs." )
    (with-default-read supplies-table gid
        { 'supply: 0.1 }
        { 'supply := supply }
        (enforce (= 0.1 supply) "This Gallina ID already exists.")
    )
  )

  (defun get-gallina-id-exists-remint
    ( gid:string )
    @doc " Determines of a Gallina can be reminted "
    (with-default-read gallinas-gender-table gid
        { 'gender: "?" }
        { 'gender := gender }
        (enforce (= "?" gender) "This Gallina ID has already been reminted.")
    )
    (with-default-read supplies-table gid
        { 'supply: 0.1 }
        { 'supply := supply }
        (enforce (!= 0.1 supply) "This Gallina ID doesnt exist.")
        (enforce (!= 0.0 supply) "This Gallina cannot be reminted.")
    )
  )

  (defun update-marketplace (id:string account:string forsale:bool price:decimal)
  (with-capability (MARKET id account)
      (write gallinasio-marketplace id
          { 'account: account
          , 'price: price
          , 'forsale: forsale
          })

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

  (defun get-user-gallinas
    ( account:string )
    @doc " Get a list of Gallina IDs and Eggs owned by a user "
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
    (sort ['price] (select gallinasio-marketplace (where "forsale" (= true))))
  )

  (defun set-new-release-schedule (start:time end:time gift:integer id:string)
  @doc " Adds new gift to voucher store "
  (with-capability (GOVERNANCE)
    (write gift-release-table id
    { "gift" : gift
    , "start" : start
    , "end" : end }))
  )

  ; (defun get-gift-store-weeks:integer ()
  ;  @doc " Gets number of weeks since gift voucher store start "
  ;   (let ((giftgenisis (time "2022-03-30T00:00:00Z") ))
  ;     (if (= (floor (/ (diff-time (at "block-time" (chain-data)) giftgenisis ) 604800)) 2) 5 (if (>= (floor (/ (diff-time (at "block-time" (chain-data)) giftgenisis ) 604800)) 5) (+ (floor (/ (diff-time (at "block-time" (chain-data)) giftgenisis ) 604800)) 1) (floor (/ (diff-time (at "block-time" (chain-data)) giftgenisis ) 604800))) )
  ;   )
  ; )

  (defun get-gift-store-weeks:integer ()
   @doc " Gets number of weeks since gift voucher store start "
    2
  )

  (defun get-current-gift-end:time ()
    @doc " Gets the end time for the current gift "
    (let ((currenttime (at "block-time" (chain-data))))
      (let ((x (select gift-release-table (and? (where 'start (>= currenttime))
            (where 'end (< currenttime))))))
             (at "end" (at 0 x)) )
    )
  )

  ; (defun get-current-gift:integer ()
  ;   @doc " Gets the current Gallina store gift from gift release table "
  ;   (let ((currenttime (at "block-time" (chain-data))))
  ;     (let ((x (select gift-release-table (and? (where 'start (>= currenttime))
  ;           (where 'end (< currenttime))))))
  ;            (at "gift" (at 0 x)) )
  ;   )
  ; )

  (defun get-current-gift:integer ()
    @doc " Gets the current Gallina store gift from gift release table "
    2
  )

  (defun get-gift:integer ()
    @doc " Gets the current Gallina store gift from gift table for promotions "
    (at 'gift (read current-gift-table ""))
  )

  (defun set-current-gift (gift:integer)
  @doc " Sets current gift in gift store "
  (with-capability (GOVERNANCE)
    (write current-gift-table ""
      { "gift" : gift }
    )
  )
  )

  (defun get-gift-vouchers:decimal (account:string)
  @doc " Gets a users gift voucher count "
  (with-default-read gift-voucher-table account
      { "balance" : 0.0 }
      { "balance" := vcount }
      vcount
    )
  )

  (defun get-gallina-details
    (id:string)
    @doc " Gets a Gallinas details"
    {
      "gender" : (at 'gender (read gallinas-gender-table id))
    , "generation" : (at 'generation (read gallinas-generation-table id))
    , "motherid" : (at 'motherid (read gallinas-motherid-table id))
    , "fatherid" : (at 'fatherid (read gallinas-fatherid-table id))
    , "birthday" : (at 'birthday (read gallinas-birthday-table id))
    , "nextbreed" : (at 'nextbreed (read gallinas-nextbreed-table id))
    , "special" : (at 'special (read gallinas-special-table id))
    }
  )

  (defun can-breed (id1:string id2:string)
    @doc "Returns true if two Gallinas can breed"
    (let ((ok true)
          (gdetails1 (get-gallina-details id1))
          (gdetails2 (get-gallina-details id2)) )
		      (enforce (!= id1 "Egg")  "Eggs dont breed." )
          (enforce (!= id2 "Egg")  "Eggs dont breed." )
           (enforce (!= id1 id2)  "Gallinas cannot breed by themselves.")
           (enforce (!= id1 (at 'motherid gdetails1))  "These Gallinas cannot breed with each other." )
           (enforce (!= id1 (at 'fatherid gdetails1))  "These Gallinas cannot breed with each other." )
           (enforce (!= id2 (at 'motherid gdetails2))  "These Gallinas cannot breed with each other." )
           (enforce (!= id2 (at 'fatherid gdetails2))  "These Gallinas cannot breed with each other." )
           (enforce (!= (at 'gender gdetails1) (at 'gender gdetails2))  "Gallinas must be of separate genders to breed." )
           (enforce (>= (round (/ (diff-time (at "block-time" (chain-data)) (at 'birthday gdetails1)) 86400)) 7)  "Gallinas must be at least 7 days old to breed." )
           (enforce (>= (round (/ (diff-time (at "block-time" (chain-data)) (at 'birthday gdetails2)) 86400)) 7)  "Gallinas must be at least 7 days old to breed." )
           (enforce (<= (at 'nextbreed gdetails1) (at "block-time" (chain-data))) "One of your Gallinas is not ready to breed yet.")
           (enforce (<= (at 'nextbreed gdetails2) (at "block-time" (chain-data))) "One of your Gallinas is not ready to breed yet.")
           ok)
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
    (enforce (= amount 1.0) "You may only transfer 1 Gallina at a time.")
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
    (enforce (!= sender receiver)
      "You can only transfer to other accounts.")
    (enforce-valid-transfer sender receiver (precision id) amount)
    (if (= sender GALLINA_BANK) (require-capability (INTERNAL)) true)
    (enforce-coin-account-exists receiver)
	  (let ((cur_guard (coin-account-guard receiver)))
    (enforce (= cur_guard receiver-guard) "Receiver guard must match their guard in the coin contract."))
    (emit-event (GALLINASIO_TRANSFER id sender receiver))
    (with-capability (TRANSFER id sender receiver amount)
      (debit id sender amount)
      (credit id receiver receiver-guard amount)
    )
  )

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
    @doc " Debits a Gallina from an account "
    (require-capability (DEBIT id account))
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (enforce-unit id amount)
    (enforce (= amount 1.0)  "You can only debit whole Gallinas." )
    (write gallinasio-marketplace id
        { 'account: account
        , 'price: 98765.43
        , 'forsale: false
        })
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
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (enforce-unit id amount)
    (enforce (= amount 1.0)  "You can only credit whole Gallinas or Eggs." )
    (with-default-read gledger (key id account)
      { "balance" : 0.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      (enforce (= retg guard)
        "Account guards do not match.")
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

  (defun change-gallina-name
    ( gid:string
      account:string
      newname:string
    )
    @doc " Change a Gallinas name "
    (enforce (!= gid "Egg")  "Eggs dont have names." )
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (with-read gledger (key gid account)
      { "balance" := balance, "guard" := guard }
      (with-capability (ACCOUNT_GUARD gid account)
        (enforce-guard guard)
        (enforce (> balance 0.0) "You can only rename a Gallina that you currently own.")
        (emit-event (GALLINASIO_NAME gid account newname))
        (format "Changed the name of Gallina with the ID {} to {}" [gid newname])
      )
    )
  )

  (defun buy-gift-voucher
    ( account:string
      id:string )
    @doc " Turn a Gallina into Gift Vouchers "
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (enforce (!= id "Egg")  "Eggs cannot be traded for gift vouchers." )
    (with-read gledger (key id account)
      { 'id := l_id
      , 'account := l_account
      , 'balance := l_balance
      , 'guard := l_guard }
        (with-capability (ACCOUNT_GUARD id l_account)
          (enforce-guard l_guard)
          (enforce (> l_balance 0.0) "You can only trade Gallinas you own.")
          (update-marketplace id account false 6543.21)
          (write gledger (key id account)
            { "balance" : 0.0
            , "guard"   : l_guard
            , "id"   : l_id
            , "account" : l_account
            })
            (with-default-read gift-voucher-table account
              { "balance" : 0.0 }
              { "balance" := gvbalance }
              (let ((x (if (= (at 'generation (read gallinas-generation-table id)) 0) 0.0 1.0)))
                    (write gift-voucher-table account
                    { "balance" : (+ gvbalance x) })
              )
            )
          (write supplies-table id {"supply": 0.0})
          (with-read total-gallinas-table ""
            {"total-count":=total-count}
            (write total-gallinas-table "" {"total-count": (- total-count 1)})
          )
          (with-default-read set-breed-table (setkey 0)
           { "count" : 0 }
           { "count" := countset}
          (write set-breed-table (setkey (at 'special (read gallinas-special-table id)) ) {"count": (if (= countset 0) 0 (- countset 1))})
          )
          (emit-event (GALLINASIO_BURN id account))
          (emit-event (SUPPLY id 0.0))
          (format "Traded the Gallina with ID {} for 1 Gift Voucher" [id])
        )
    )
  )

  (defun spend-10-gift-vouchers
    ( account:string
      id:string )
    @doc " Spend Gift Vouchers on a Gallina "
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (enforce (!= id "Egg")  "Eggs cannot receive gifts." )
    (with-read gledger (key id account)
      { 'id := l_id
      , 'account := l_account
      , 'balance := l_balance
      , 'guard := l_guard }
        (with-capability (ACCOUNT_GUARD id l_account)
          (enforce-guard l_guard)
          (enforce (> l_balance 0.0) "You can only gift Gallinas you own.")
          (update-marketplace id account false 6543.21)
          (with-read gift-voucher-table account
              { "balance" := gvbalance }
              (enforce (>= gvbalance 1.0) "Not enough gift vouchers.")
              (write gift-voucher-table account
              { "balance" : (- gvbalance 1.0) })
          )
          (let ( (gdetails (get-gallina-details id)) )
                 (enforce (= (at 'special gdetails) 0) "This Gallina already has a gift.")
                 (enforce (> (at 'generation gdetails) 0) "Generation 0 Gallinas cannot have gifts.")
          )
          (with-default-read set-breed-table (setkey (get-gift-store-weeks))
           { "count" : 0 }
           { "count" := countset}
          (enforce (< countset MAX_SUPPLY) "The gift shop has run out of this gift!")
          (write set-breed-table (setkey (get-gift-store-weeks)) {"count": (+ countset 1)})
          )
          (update gallinas-special-table id {"special": (get-gift-store-weeks)} )
          (emit-event (GALLINASIO_GIFT id account (get-gift-store-weeks)))
          (format "Gifted the Gallina with ID {} with Gift ID {}" [id (get-gift)])
        )
    )
  )

  (defun buy-egg
    ( account:string
      guard:guard
      amount:decimal )
    @doc " Buy a Gallina Egg to Hatch a Gallina "
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (with-default-read supplies-table "Egg"
          { 'supply: 0.0 }
          { 'supply := egg-supply }
        (enforce (= amount EGG_PRICE)  "You can only purchase 1 Egg for 1 KDA at a time" )
        (validate-account-id account)
        (enforce (= "k:" (take 2 account)) "Only k: Prefixed Accounts")
        (enforce-coin-account-exists account)
    	  (let ((cur_guard (coin-account-guard account)))
        (enforce (= cur_guard guard) "Gallina account guards must match the same account guard in the coin contract."))
        (coin.transfer account GALLINA_BANK amount)
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
        (write supplies-table "Egg" {"supply": (+ egg-supply 1.0)})
        (emit-event (SUPPLY "Egg" (+ egg-supply 1.0)))
        (emit-event (GALLINASIO_BUY_EGG "Egg" account))
        (format "1 Egg Purchased for {} KDA." [amount])
    )
  )

  (defun remint
    ( id:string
      account:string )
      @doc " Remints a Gallina "
      (enforce (!= id "Egg")  "Eggs dont get reminted." )
      (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
      (with-capability (ACCOUNT_GUARD id account)
        (get-gallina-id-exists-remint id)
        (let ((newgender (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1(hash (at "block-time" (chain-data)))))))) 2)0) "Male" "Female") ) )
          (insert gallinas-gender-table id {"gender" : newgender })
          (emit-event (GALLINASIO_REMINT id account (at "block-time" (chain-data)) newgender))
        )
        (insert gallinas-generation-table id {"generation" : 1 })
        (insert gallinas-motherid-table id {"motherid" : "Egg" })
        (insert gallinas-fatherid-table id {"fatherid" : "Egg" })
        (insert gallinas-birthday-table id {"birthday" : (at "block-time" (chain-data)) })
        (insert gallinas-nextbreed-table id {"nextbreed" : (add-time (at "block-time" (chain-data)) (days 7)) })
        (insert gallinas-special-table id {"special" : 0 })
      )
      (emit-event (SUPPLY id 1.0))
      (format "Updated Gallina {}" [id])
  )

  (defun hatch-egg
    ( account:string
      name:string )
    @doc " Hatch A Gallina From An Egg "
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (with-default-read supplies-table "Egg"
          { 'supply: 0.0 }
          { 'supply := egg-supply }
          (with-default-read total-gallinas-table ""
               { "total-count" : 0 }
               { "total-count" := total-count}
               (validate-gallina-name name)
               (enforce (= "k:" (take 2 account)) "Only k: Prefixed Accounts.")
               (with-read gledger (key "Egg" account)
                    { "balance" := balance-egg
                    , "guard" := guard }
                    (enforce (> balance-egg 0.0) "You dont have any Eggs to hatch.")
                    (with-capability (ACCOUNT_GUARD "Egg" account)
                    (enforce-guard guard)
                    (let ((gid
                      (create-random-id name account)))
                      (get-gallina-id-exists gid)
                      (with-default-read gledger (key gid account)
                          { "balance" : 1.0, "guard" : guard }
                          { "balance" := balance, "guard" := retg }
                          (enforce (= retg guard)
                          "account guards do not match")
                          (write gledger (key gid account)
                          { "balance" : balance
                          , "guard"   : retg
                          , "id"   : gid
                          , "account" : account
                          })
                          (write supplies-table "Egg" {"supply": (- egg-supply 1)})
                          (update gledger (key "Egg" account)
                          { "balance" : (- balance-egg 1.0) }
                          )
                          (insert gallinas-generation-table gid {"generation" : 0 })
                          (insert gallinas-motherid-table gid {"motherid" : "Egg" })
                          (insert gallinas-fatherid-table gid {"fatherid" : "Egg" })
                          (insert gallinas-birthday-table gid {"birthday" : (at "block-time" (chain-data)) })
                          (insert gallinas-nextbreed-table gid {"nextbreed" : (add-time (at "block-time" (chain-data)) (days 7)) })
                          (insert gallinas-special-table gid {"special" : 0 })
                          (write total-gallinas-table "" {"total-count": (+ total-count 1)})
                          (write supplies-table gid {"supply": 1.0})
                          (let ( (newgender (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1(hash (at "block-time" (chain-data)))))))) 2)0) "Male" "Female") ) )
                            (insert gallinas-gender-table gid {"gender" : newgender })
                            (emit-event (GALLINASIO_HATCH gid account name newgender (at "block-time" (chain-data)) (add-time (at "block-time" (chain-data)) (days 7))))
                          )
                          (with-default-read set-breed-table "0"
                             { "count" : 0 }
                             { "count" := countset}
                            (write set-breed-table "0"
                              {"count": (+ countset 1)}
                            )
                          )
                          (emit-event (SUPPLY "Egg" (- egg-supply 1.0)))
                          (emit-event (SUPPLY gid 1.0))
                          (format "Hatched 1 new Gallina with the ID {} " [gid])
                        )
                      )
                    )
               )
          )
     )
  )

  (defun sell-my-gallina ( id:string account:string price:decimal forsale:bool)
    @doc " Put a Gallina on the Market or update one already for sale "
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
        (update-marketplace id account forsale price)
        (emit-event (GALLINASIO_SELL id account price forsale))
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
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (with-read gallinasio-marketplace id
          { 'account := m_account
          , 'price := m_price
          , 'forsale := m_forsale }
        (enforce (= m_forsale true)  "You can only purchase a Gallina that is for sale." )
        (enforce (= amount m_price) "Insufficient funds.")
        (enforce (!= account m_account) "You cannot buy your own Gallina.")
        (enforce (= "k:" (take 2 account)) "Only k: Prefixed Accounts.")
        (enforce-coin-account-exists account)
    	  (let ((cur_guard (coin-account-guard account)))
        (enforce (= cur_guard guard) "Gallina account guards must match the same account guard in the coin contract."))
        (coin.transfer account m_account amount)
        (update gledger (key id m_account)
          { "balance" : 0.0 })
          (write gledger (key id account)
            { "balance" : 1.0
            , "guard"   : guard
            , "id"   : id
            , "account" : account
            }
          )
          (write gallinasio-marketplace id
              { 'account: account
              , 'price: 98765.43
              , 'forsale: false
              })
        (emit-event (GALLINASIO_BUY id account m_account amount))
        (format " Purchased a Gallina with the ID {} for {} KDA " [id amount])
    )
  )

  (defun breed-gallinas
    ( gid1:string
      gid2:string
      account:string
      amount:decimal
      gene:integer
      parent:integer )
    @doc " Breed 2 Gallinas and Hatch The Egg "
    (enforce (!= gid1 "Egg")  "Eggs dont breed." )
    (enforce (!= gid2 "Egg")  "Eggs dont breed." )
    (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
    (enforce (= "k:" (take 2 account)) "Only k: Prefixed Accounts")
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
            (with-capability (ACCOUNT_GUARD id1l account1l)
            (enforce-guard guard1l)
            (enforce (= guard1l guard2l)  "You can only breed Gallinas you currently own." )
            (enforce (> balance1l 0.0)  "You can only breed Gallinas you currently own." )
            (enforce (> balance2l 0.0)  "You can only breed Gallinas you currently own." )
            (enforce (!= gid1 gid2)  "A Gallina cannot breed with itself." )
            (enforce (> amount 0.0)  "The cost to breed Gallinas is 1 KDA." )
            (enforce (= amount BREED_PRICE)  "The cost to breed Gallinas is 1 KDA." )
            (let ((gdetails1 (get-gallina-details gid1))
                  (gdetails2 (get-gallina-details gid2)))
                  (enforce (!= gid1 (at 'motherid gdetails1))  "These Gallinas cannot breed with each other." )
                  (enforce (!= gid1 (at 'fatherid gdetails1))  "These Gallinas cannot breed with each other." )
                  (enforce (!= gid2 (at 'motherid gdetails2))  "These Gallinas cannot breed with each other." )
                  (enforce (!= gid2 (at 'fatherid gdetails2))  "These Gallinas cannot breed with each other." )
                  (enforce (!= (at 'gender gdetails1) (at 'gender gdetails2))  "Gallinas must be of separate genders to breed." )
                  (enforce (>= (round (/ (diff-time (at "block-time" (chain-data)) (at 'birthday gdetails1)) 86400)) 7)  "Gallinas must be at least 7 days old to breed." )
                  (enforce (>= (round (/ (diff-time (at "block-time" (chain-data)) (at 'birthday gdetails2)) 86400)) 7)  "Gallinas must be at least 7 days old to breed." )
                  (if (!= gene 0) true (enforce (<= (at 'nextbreed gdetails1) (at "block-time" (chain-data)))  "Gallina 1 is not ready to breed yet." ))
                  (if (!= gene 0) true (enforce (<= (at 'nextbreed gdetails2) (at "block-time" (chain-data)))  "Gallina 2 is not ready to breed yet." ))
              )
              (coin.transfer account GALLINA_BANK amount)
              (let ((newid (create-random-id (+ gid1 gid2) account )))
                (get-gallina-id-exists newid)
                (if (= gene 0) true (with-read gift-voucher-table account
                    { "balance" := gvbalance }
                    (enforce (>= gvbalance 1.0) "Not enough gift vouchers to quick breed.")
                    (write gift-voucher-table account
                    { "balance" : (- gvbalance 1.0) })
                    (emit-event (GALLINASIO_QUICK newid account gene parent)) )
                )
                (insert gallinas-motherid-table newid {"motherid" : (if (= (at 'gender (read gallinas-gender-table gid1)) "Female") gid1 gid2 ) } )
                (insert gallinas-fatherid-table newid {"fatherid" : (if (= (at 'gender (read gallinas-gender-table gid1)) "Male") gid1 gid2 ) })
                (insert gallinas-birthday-table newid {"birthday" : (at "block-time" (chain-data)) })
                (insert gallinas-nextbreed-table newid {"nextbreed" : (add-time (at "block-time" (chain-data)) (days 7)) })
                (with-read total-gallinas-table ""
                  {"total-count":=total-count}
                  (write total-gallinas-table "" {"total-count": (+ total-count 1)})
                  (write supplies-table newid {"supply": 1.0})
                  (write gledger (key newid account)
                  { "balance" : 1.0
                  , "guard"   : guard1l
                  , "id"   : newid
                  , "account" : account
                  })
                  (let ( (newgender (if(=(mod (abs (str-to-int 64 (base64-encode (take -1 (drop -1(hash (at "block-time" (chain-data)))))))) 2)0) "Male" "Female") )
                         (newspecial (if (= (at 'special (read gallinas-special-table gid1)) (at 'special (read gallinas-special-table gid2))) (if (> (at 'special (read gallinas-special-table gid1)) 0) (at 'special (read gallinas-special-table gid1)) 0) 0) )
                         (newgeneration (if (> (at 'generation (read gallinas-generation-table gid1)) (at 'generation (read gallinas-generation-table gid2))) (+ (at 'generation (read gallinas-generation-table gid1)) 1) (+ (at 'generation (read gallinas-generation-table gid2)) 1)))
                         )
                            (insert gallinas-gender-table newid {"gender" : newgender })
                            (insert gallinas-generation-table newid {"generation" : newgeneration })
                            (with-default-read set-breed-table (setkey newspecial)
                             { "count" : 0 }
                             { "count" := countset}
                              (with-default-read set-breed-table (setkey 0)
                                 { "count" : 0 }
                                 { "count" := countset0}
                                 (if (= newspecial 0) (insert gallinas-special-table newid {"special" : 0 }) (if (< countset (* newspecial MAX_SUPPLY)) (insert gallinas-special-table newid {"special" : newspecial }) (insert gallinas-special-table newid {"special" : 0 }) ) )
                                 (if (= newspecial 0) (write set-breed-table (setkey 0) {"count": (+ countset0 1)}) (if (< countset (* newspecial MAX_SUPPLY)) (write set-breed-table (setkey newspecial) {"count": (+ countset 1)}) (write set-breed-table (setkey 0) {"count": (+ countset0 1)}) ) )
                              )
                            )
                            (emit-event (GALLINASIO_BREED account gid1 gid2 newid newgender (at "block-time" (chain-data)) (add-time (at "block-time" (chain-data)) (days (* 3 (at 'generation (read gallinas-generation-table gid1))))) (add-time (at "block-time" (chain-data)) (days (* 3 (at 'generation (read gallinas-generation-table gid2))))) (add-time (at "block-time" (chain-data)) (days 7)) newspecial newgeneration))
                  )
                  (update gallinas-nextbreed-table gid1 { "nextbreed" : (add-time (at "block-time" (chain-data)) (days (* 3 (at 'generation (read gallinas-generation-table gid1))))) })
                  (update gallinas-nextbreed-table gid2 { "nextbreed" : (add-time (at "block-time" (chain-data)) (days (* 3 (at 'generation (read gallinas-generation-table gid2))))) })
                  (update-marketplace gid1 account false 96543.21)
                  (update-marketplace gid2 account false 96543.21)
                  (emit-event (SUPPLY newid 1.0))
                  (format " Hatched 1 new Gallina with ID {} from parents {} and {} " [newid gid1 gid2])
                )
              )
            )
          )
       )
  )

  (defun resolve
    ( id:string
      account:string )
      @doc " Remints a Gallina "
      (enforce (!= id "Egg")  "Eggs dont get resolved." )
      (if (= account GALLINA_BANK) (require-capability (INTERNAL)) true)
      (with-capability (ACCOUNT_GUARD id account)
        (emit-event (GALLINASIO_RESOLVE id account (at 'generation (read gallinas-generation-table id)) (at 'gender (read gallinas-gender-table id)) (at 'motherid (read gallinas-motherid-table id)) (at 'fatherid (read gallinas-fatherid-table id)) (at 'birthday (read gallinas-birthday-table id)) (at 'nextbreed (read gallinas-nextbreed-table id)) (at 'special (read gallinas-special-table id)) ))
      )
      (format "Updated Gallina {}" [id])
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

;(create-table free.collect-gallinas.current-gift-table)
;(create-table free.collect-gallinas.set-breed-table)
;(create-table free.collect-gallinas.gallinas-gender-table)
;(create-table free.collect-gallinas.gallinas-generation-table)
;(create-table free.collect-gallinas.gallinas-motherid-table)
;(create-table free.collect-gallinas.gallinas-fatherid-table)
;(create-table free.collect-gallinas.gallinas-birthday-table)
;(create-table free.collect-gallinas.gallinas-nextbreed-table)
;(create-table free.collect-gallinas.gallinas-special-table)
;(create-table free.collect-gallinas.gift-voucher-table)
;(create-table free.collect-gallinas.gift-release-table)
;(create-table free.collect-gallinas.gallinasio-marketplace)

;(free.collect-gallinas.set-new-release-schedule (time "2022-03-30T00:00:02Z") (time "2022-04-06T00:00:01Z") 5 "1")

;(free.collect-gallinas.set-new-release-schedule (time "2022-04-06T00:00:02Z") (time "2022-04-13T00:00:01Z") 3 "2")

;(create-table free.collect-gallinas.gmarketplace)
;(create-table free.collect-gallinas.gledger)
;(create-table free.collect-gallinas.supplies-table)
;(create-table free.collect-gallinas.uri-table)
;(create-table free.collect-gallinas.gallinas-table)
;(create-table free.collect-gallinas.total-gallinas-table)
;(create-table free.collect-gallinas.marketplace-table)
;(free.collect-gallinas.initialize)

