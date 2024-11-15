(module smashed-spiders "smashed-admin"

  (defconst PRICE_KEY "price-key")
  (defconst TOKEN_COUNT_KEY "token-count-key")
  (defconst NAMES_COUNT_KEY "names-count-key")
  (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
  (defconst MAX_SUPPLY 6969)
  (defconst ADMIN_KEYSET (read-keyset "smashed-admin"))
  (defconst ADMIN_ADDRESS "k:abc72f31ba043e999c80fc0dc1441a090e5e22f29ab25d37fb56642520a6e813")

  ;======= CAPABILITIES =======;
  (defcap PRIVATE ()
    @doc "can only be called from a private context"
    true
  )

  ; (defcap ACCOUNT_GUARD(account:string)
  ;   @doc "Ensure that only k: addresses are used and belongs to caller."
  ;   (enforce (= "k:" (take 2 account)))
  ;   (enforce-guard
  ;     (at "guard" (coin.details account))
  ;   )
  ; )

  (defcap ADMIN() ; Used for admin functions
    @doc "Only allows admin to call these"
    (enforce-keyset ADMIN_KEYSET)
    (compose-capability (PRIVATE))
    ; (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS)) ; TODO enable
  )

  (defcap OWNER (account:string id:string)
    @doc "Enforces that an account owns a the token with the given ID"
    (let (
        (nft-owner (at "owner" (read nfts id ["owner"])))
      )
      (enforce (= nft-owner account) "Account is not owner of the NFT")
      ; (compose-capability (ACCOUNT_GUARD account)) ; TODO enable
    )
  )

  ;======= SCHEMAS + TABLES =======;
  (defschema counts-schema
    @doc "Schema for counting"
    count:integer
  )

  (defschema names-schema
    @doc "Names schema"
    name:string
  )

  (defschema nfts-schema
    @doc "All information about NFTs"
    id:string
    name:string
    owner:string
  )

  (defschema price-schema
    @doc "Prices schema"
    price:decimal
  )

  (defschema values-schema
    @doc "Schema for storing basic values"
    value:string
  )

  ; The schema defined what the table should look like
  ; Now we define the actual table to store the data
  (deftable counts:{counts-schema})
  (deftable names-counts:{counts-schema})
  (deftable names:{names-schema})
  (deftable nfts:{nfts-schema})
  (deftable price:{price-schema})
  (deftable values:{values-schema})


  ;======= MAIN FUNCTIONS =======;
  ; Get the image of your NFT
  (defun uri:string (id:string)
    @doc "Return URI for a token ID"
    (format "https://api.smashed-spiders.com/images/{}.png" [id])
  )

  ; Mint new token
  (defun mint (owner:string number:integer)
    @doc "Mints a new token"
    (enforce (= number 1) "Number enforced to be 1 to avoid confusion but allow mapping to work")
    (require-capability (PRIVATE))
    ; (require-capability (ACCOUNT_GUARD owner)) ; TODO enable

    ; (let ((mint-chain-id (get-value MINT_CHAIN_ID_KEY))) ; TODO enable
    ;   (enforce (= (get-current-chain-id) mint-chain-id) "Can only mint on Chain 1")
    ; )

    ; Get new token ID
    (let (
        (id (new-token-id))
      )

      ; Add token to list of nfts
      (insert nfts id {
        "id": id,
        "name": (get-name (int-to-str 10 ( get-count TOKEN_COUNT_KEY))),
        "owner": owner})

      (increase-count TOKEN_COUNT_KEY)
    )

  )

  (defun mint-batch (owner:string amount:integer)
    @doc "Mint multiple Spiders at once as long as supply lasts"
    (enforce (>= amount 1) "Amount must be 1 or more")

    (let (
        (spiders-minted (get-token-count))
      )
      (enforce (<= (+ amount spiders-minted) MAX_SUPPLY) "Max supply exceeded")
    )

    ; Handle payment
    (if
      (!= owner ADMIN_ADDRESS)
      (coin.transfer owner ADMIN_ADDRESS (* (get-price) amount))
      "Admin account"
    )

    ; (with-capability (ACCOUNT_GUARD owner) ; TODO enable
      (with-capability (PRIVATE)
        (map
          (mint owner)
          (make-list amount 1)
        )
      )
    ; ) ; closing bracket acc guard
  )

  (defun transfer:string (id:string sender:string receiver:string amount:integer)
    @doc "Allow the owner to transfer one token to another k: wallet"
    (enforce (= amount 1) "Can only transfer one token at a time")
    ; (enforce-account-exists receiver "Receiving account does not exist")
    (enforce (= sender ADMIN_ADDRESS)
      "Can only send tokens from admin account for now; will be changed post mint"
    )
    ; todo: Allow owners to transfer tokens

    ; Update table with new owner
    (with-capability (ADMIN)
      (with-capability (OWNER sender id)
        (update nfts id {"owner": receiver})
      )
    )
  )

  ;======= HELPER FUNCTIONS =======;
  (defun get-token-count:integer ()
  @doc "Return the number of minted tokens"
    (get-count TOKEN_COUNT_KEY)
  )

  (defun get-current-chain-id ()
    @doc "Return current chain ID"
    (at "chain-id" (chain-data)) ; Pact standard information
  )

  (defun get-price:decimal ()
    @doc "Return last mint price"
    (at "price" (read price PRICE_KEY ["price"]))
  )

  (defun get-nft (id:string)
    @doc "Return full details of a specific NFT"
    (read nfts id)
  )

  (defun get-all-nfts:list ()
    @doc "Return all IDs from the nfts table"
    (keys nfts)
  )

  (defun get-owner (id:string)
    @doc "Return the owner of a specific NFT"
    (read nfts id ["owner"])
  )

  (defun get-owner-nfts (owner:string)
    @doc "Return all NFTs and details of an owner"
    (select nfts ['id, 'name] (where 'owner (= owner)))
  )

  (defun get-all-name-ids:list ()
    @doc "Return all IDs from the names table"
    (keys names)
  )

  (defun get-name (key:string)
    @doc "Return the name of a specific NFT"
    (at "name" (read names key ["name"]))
  )

  (defun get-count (key:string)
    @doc "Return count for a given key in the counts table"
    (at "count" (read counts key ["count"]))
  )

  (defun get-names-count (key:string)
    @doc "Gets names count for a given key in the names-count table"
    (at "count" (read names-counts key ["count"]))
  )

  ; (defun enforce-account-exists (account:string)
  ;   @doc "Enforces that an account exists in the coin table"
  ;   (let ((coin-account (at "account" (coin.details account))))
  ;     (enforce (= coin-account account) "Account was not found")
  ;   )
  ; )

  ;======= PRIVATE & ADMIN FUNCTIONS =======;
  (defun increase-count (key:string)
    @doc "Increase count by 1"
    (require-capability (PRIVATE))
    (update counts key
      {"count": (+ 1 (get-count key))}
    )
  )

  (defun increase-names-count (key:string)
    @doc "Increase count by 1"
    (require-capability (PRIVATE))
    (update names-counts key
      {"count": (+ 1 (get-names-count key))})
  )

  (defun add-name (entry:string)
    @doc "Adds a single name to the names table and increases the counter"
    (require-capability (PRIVATE))
    (insert names (int-to-str 10 (get-names-count NAMES_COUNT_KEY)) {"name": entry})
    (with-capability (PRIVATE)
      (increase-names-count NAMES_COUNT_KEY)
    )
  )

  (defun create-names-table (entries:list)
    @doc "Create table from list of names"
    (enforce-keyset ADMIN_KEYSET)
    (with-capability (PRIVATE)
      (map (add-name) entries)
    )
  )

  (defun new-token-id ()
    @doc "Returns an id for a new token to be minted"
    (require-capability (PRIVATE))
    (+ (+ (get-current-chain-id) ":") (int-to-str 10 (get-count TOKEN_COUNT_KEY)))
  )

  (defun set-price (new-price:decimal)
    @doc "Change the current token price"
    (enforce-keyset ADMIN_KEYSET)
    (update price PRICE_KEY {"price": new-price})
  )

  (defun initialize ()
    @doc "initialize contract on first load"
    (enforce-keyset ADMIN_KEYSET)
    (insert counts TOKEN_COUNT_KEY {"count": 1})
    (insert names-counts NAMES_COUNT_KEY {"count": 1})
    (insert values MINT_CHAIN_ID_KEY {"value": "1"})
    (insert price PRICE_KEY {"price": 1.0})
  )

)

; Acknowledgements
; Thank you Kitty Kad for laying the groundwork to all of this. We have
; studied your contract and applications for countless hours and none of this
; would have been possible without your efforts 🕷 💜 🐈


(cr
