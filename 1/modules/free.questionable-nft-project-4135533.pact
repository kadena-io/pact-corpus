(module questionable-nft-project "questionable-nft-admin"
  "My first NFT, which will likely be very questionable. Jad_KDAMining"

  (defconst MINERS_CREATED_COUNT_KEY "miners-count-key")
  (defconst PRICE_KEY "price-key")
  (defconst ADMIN_KEYSET (read-keyset 'questionable-nft-admin))
  ;(defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
  (defconst MINERS_URI_KEY "miners-uri-key")
  (defconst ADMIN-ADDRESS "k:0f3ad275ed9a9ea995aa26747a85c297cbbdd84f4239b1372c48b3392fc2d511")

  (defcap PRIVATE ()
    true
  )

  (defcap ACCOUNT_GUARD(account:string)
    ;@doc "verifies account is a k: account and belongs to caller"
    ;take 2 means to create a new temporary object that only has the first two characters of a string
    ; in this case, it is hoping to see "k:", if it doesn't then don't continue
    (enforce (= "k:" (take 2 account)) "For security, only k: accounts are supported")
    ;(at "guard" (coin.details account)) ;enable this once I understand coin.
  )

  ; (defcap OWNER (account:string id:string)
  ;   ;@doc "Enforces that an account owns the particular miner ID"
  ;   (let
  ;     (
  ;         (nft-owner )
  ;     )
  ;   )
  ; )

  (defcap ADMIN()
    ; @doc "Only allows admin to call these"
    (enforce-keyset ADMIN_KEYSET)
  )

  (defschema counts-schema
    ; a schema to keep track of how many things there are
    count:integer
  )
  (defschema value-schema
    value:string
  )
  (defschema price-schema
    price:string
  )

  (defschema nft-main-schema
    id:string
    generation:integer
    hashrate-multiplier:decimal
    owner-address:string
  )

  (deftable nft-main-table:{nft-main-schema})
  (deftable counts-table:{counts-schema})
  (deftable value-table:{value-schema})
  (deftable price-table:{price-schema})

  (defun initialize ()
    ;initialize module on first load
    (insert counts-table MINERS_CREATED_COUNT_KEY {"count": 0})
    (insert price-table PRICE_KEY {"price": "0.001"})
  )

  (defun mint-nft()
    ; mints an nft
    ;(require-capability (ACCOUNT_GUARD owner))
    (let ((id (id-for-new-nft)))
      (insert nft-main-table id {
          "id":id,
          "generation":0,
          "hashrate-multiplier":1.0,
          "owner-address":(at "sender" (chain-data))
        })
    )
    (increase-count MINERS_CREATED_COUNT_KEY)
  )


  (defun set-owner(owner-address:string nft-id:string)
    ; ensure only the admin can set ownership of an NFT
    (enforce-keyset (read-keyset "questionable-nft-admin"))
    ; insert a value into the table "owners"
    ; nft-id is the key in the table. keys must always be entered as strings
    ;  key        entity-name
    ; nft-id     owner-address
    ;; "owner-address" is referring to the field name in the schema
    ;; owner-address is referring to the passed-in argument
    (insert nft-main-table nft-id {"owner-address": owner-address})
  )

  (defun get-owner (nft-id:string)
    ; get data from the owners table
    ; "at" grabs the field name from a table. In this case, there is only one field name
    ; "owner-address" is the field name in the schema
    ; owners is the name of the table
    ; nft-id is referring to the passed in argument
    ; 'owner-address is inside [] because we want to generate a list, even
    ;                          if that list is a single item.
    ;       If our schema was more complex, we could return the whole list
    ; to read from a table, use "at" or "select"
    (at "owner-address" (read nft-main-table nft-id ['owner-address] ))
  )

  (defun get-uri:string (id:string)
    " Get URI for a given ID. If not supported, return \"\" (empty string)"
    ; contatenate string, normally there would be an NFT-id between
    ;      a database link and .jpg
    (+ "https://imgur.com/IgIR8Bu" ".jpg")
  )

  (defun get-count (key:string)
    ; gets the count for a key
    (at "count" (read counts-table key ['count]))
  )
  (defun get-value (key:string)
    ; gets the value for a key
    (at "value" (read value-table key ['value]))
  )
  (defun get-price ()
    ; gets the price for a key
    (at "price" (read price-table PRICE_KEY ["price"]))
  )


  (defun increase-count (key:string)
    ;increase the count of a key in a table by 1
    (update counts-table key {"count": (+ 1 (get-count key))})
  )

  (defun id-for-new-nft ()
    ; determine the next id for minting
    (int-to-str 10 (get-count MINERS_CREATED_COUNT_KEY))
  )
)


;(create-table nft-main-table)
;(create-table counts-table)
;(create-table value-table)
;(create-table price-table)
;(initialize)

