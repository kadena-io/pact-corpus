(module bubblegum GOVERNANCE

; --------------------------------------------------------------------------
; Tables/Schemas
; --------------------------------------------------------------------------

  (defschema entry
    id:string
    account:string
    balance:decimal
    guard:guard
  )

  (deftable ledger:{entry})

  (implements n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.poly-fungible-v1)

  (defschema supply
    supply:decimal
  )

  (deftable supplies:{supply})

  (defschema metadata
    id:string
    meta:string
    bg:integer
  )

  (deftable metadata-table:{metadata})

  (defschema market
    id:string
    forsale:bool
    price:decimal
    owner:string
    )

  (deftable marketplace:{market})

  (defschema vault-count
    id:string
    count:integer)

  (deftable vault-count-table:{vault-count})

  (defschema vault
    nftid:string
    vault:string
    token:module{fungible-v2}
    account:string)

  (deftable vault-table:{vault})

  (defschema chains
    id:string
    chains:[string])

  (deftable nft-chains-table:{chains})

  (defschema whitelist
    account:string
    purchased:bool )

  (deftable whitelist-table:{whitelist})

  (defschema endtime
    ended:bool)

  (deftable endtime-table:{endtime})

  (defschema mints
    account:string
    mintfree:bool )

  (deftable mint-table:{mints})

; --------------------------------------------------------------------------
; Constants
; --------------------------------------------------------------------------

  (defconst NFT_PRICE 20.00
    " The cost of 1 Bubblegum" )

  (defconst BUBBLEGUM_BANK:string "nzo0mbid_04G8HJ6qmmIIkTjR3BOyH7aEtEDRg_X-4E"
    " Bubblegum Bank ")

  (defconst MAX_SUPPLY 1000.0
    " The max supply of bubblegum" )

  (defconst MINIMUM_PRECISION 0)

  (defconst MAIN_CHAIN "10")

; --------------------------------------------------------------------------
; Capabilities
; --------------------------------------------------------------------------

  (defcap GOVERNANCE ()
      (enforce-keyset "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.admin")
    )

  (defcap ACCOUNT_BALANCE_GUARD(id:string account:string)
        @doc "Verifies Bubblegum NFT Account Ownership"
        (let
            (
              (balance (at "balance" (read ledger (key id account))))
            )
            (enforce-guard
                (at "guard" (read ledger (key id account)))
            )
            (enforce (> balance 0.0) "Insufficient balance")
        )
  )

  (defcap DEBIT (id:string sender:string)
    (enforce-guard
      (at 'guard
        (read ledger (key id sender)))))

  (defcap CREDIT (id:string receiver:string) true)


  (defcap ISSUE ()
    true
  )

  (defcap ADD_UPDATE
    (id:string)
    true
  )

  (defcap CAN_UPDATE
    (id:string)
    (compose-capability (ADD_UPDATE id)))

  (defcap URI:bool (id:string uri:string) @event true)

  (defcap SUPPLY:bool (id:string supply:decimal) @event true)

  (defcap PRIVATE_RESERVE
          (id:string)
        true)

  (defun enforce-private-reserve:bool
      (id:string)
    (require-capability (PRIVATE_RESERVE id)))

  (defun create-pool-guard:guard
      (id:string)
    (create-user-guard (enforce-private-reserve id)))

  (defun get-account-principal (id:string)
   (create-principal (create-pool-guard id))
  )



; --------------------------------------------------------------------------
; Utilities
; --------------------------------------------------------------------------

  (defun key ( id:string account:string )
    (format "{}:{}" [id account])
  )


  (defun dec-key ( count:decimal subject:string )
    (format "{}:{}" [(floor count 0) subject])
  )

  (defun int-key ( count:integer subject:string )
    (format "{}:{}" [count subject])
  )

  (defun create-account-key:string
    ( pool_id:string account_id:string)
    (hash (+ account_id (+ pool_id (format "{}" [(at 'block-time (chain-data))]))))
  )

  (defun get-token-key
    ( tokenA:module{fungible-v2} )
    (format "{}" [tokenA])
  )

  (defun total-supply:decimal (id:string)
    (with-default-read supplies "bubblegum"
      { 'supply : 0 }
      { 'supply := s }
      s)
  )

  (defun enforce-unit:bool (id:string amount:decimal)
    (enforce
      (= (floor amount (precision id))
         amount)
      "precision violation")
  )

  (defun truncate:decimal (id:string amount:decimal)
    (floor amount (precision id))
  )

  (defun precision:integer (id:string)
    MINIMUM_PRECISION)


  (defun enforce-valid-amount
    ( precision:integer
      amount:decimal
    )
    (enforce (> amount 0.0) "Positive non-zero amount")
    (enforce-precision precision amount)
  )

  (defun enforce-valid-account (account:string)
    (enforce (> (length account) 2) "minimum account length")
  )

  (defun enforce-precision
    ( precision:integer
      amount:decimal
    )
    (enforce
      (= (floor amount precision) amount)
      "precision violation")
  )

  (defun enforce-valid-transfer
    ( sender:string
      receiver:string
      precision:integer
      amount:decimal)
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-amount precision amount)
    (enforce-valid-account sender)
    (enforce-valid-account receiver)
  )


  (defun get-all-nfts
    ( )
    (let
        (
            (nfts (select ledger (constantly true)))
        )
        (map (get-nft-details) nfts)
      )
  )

  (defun get-user-nfts
    ( account:string )
    (let
        (
            (nfts (select ledger
                  (and? (where 'account (= account))
                    (where 'balance (< 0.0)))))
        )
        (map (get-nft-details) nfts)
      )
  )


  (defun get-martketplace-nfts
    ( )
    (let
        (
            (nfts (select marketplace
                  (where 'forsale (= true))))
        )
        (map (get-marketplace-details) nfts)
      )
  )

  (defun get-marketplace-details (nft:object{market})
    (bind nft {
                "id" := t_id,
                "forsale" := t_forsale,
                "price" := t_price,
                "owner" := t_owner
              }
              (let
                  (
                      (metadata (read metadata-table t_id))
                      (marketdata (read marketplace t_id))
                  )
                  {
                    "id":t_id,
                    "owner":t_owner,
                    "forsale": t_forsale,
                    "price": t_price,
                    "meta": (at "meta" metadata),
                    "bg": (at "bg" metadata)
                  }
              )
    )
  )


  (defun get-nft-details (nft:object{entry})
    (bind nft {
                "id" := t_id,
                "account" := t_account,
                "balance" := t_balance,
                "guard" := t_guard
              }
              (let
                  (
                      (metadata (read metadata-table t_id))
                      (marketdata (read marketplace t_id))
                      (nftchains (read nft-chains-table t_id))
                  )
                  {
                    "id":t_id,
                    "owner":t_account,
                    "forsale": (at "forsale" marketdata),
                    "price": (at "price" marketdata),
                    "meta": (at "meta" metadata),
                    "bg": (at "bg" metadata),
                    "chains": (at "chains" nftchains)
                  }
              )
    )
  )

  (defun get-nft (id:string)
              (let
                  (
                      (metadata (read metadata-table id))
                      (marketdata (read marketplace id))
                      (nftchains (read nft-chains-table id))
                  )
                  {
                    "id":id,
                    "owner": (at "owner" marketdata),
                    "forsale": (at "forsale" marketdata),
                    "price": (at "price" marketdata),
                    "meta": (at "meta" metadata),
                    "bg": (at "bg" metadata),
                    "chains": (at "chains" nftchains)
                  }
              )
  )

  (defun update-supply (id:string amount:decimal)
  (require-capability (ADD_UPDATE id))
    (with-default-read supplies "bubblegum"
      { 'supply: 0 }
      { 'supply := s }
      (write supplies id {'supply: (+ s amount)}))
  )

  (defun uri:string (id:string)
    (read metadata-table id)
  )


  (defun get-balance:decimal (id:string account:string)
    (at 'balance (read ledger (key id account)))
  )

  (defun details:object{n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.poly-fungible-v1.account-details}
    ( id:string account:string )
    (read ledger (key id account))
  )

  (defun rotate:string (id:string account:string new-guard:guard)
    (with-capability (ACCOUNT_BALANCE_GUARD id account)
    (with-read ledger (key id account)
      { "guard" := old-guard }

      (enforce-guard old-guard)

      (enforce (= (at 'chain-id (chain-data)) MAIN_CHAIN)
            "invalid chain")

      (update ledger (key id account)
        { "guard" : new-guard })
     )
     )
  )

  (defun get-vault (id:string count:integer)
    (let*
          (
              (vault-data (read vault-table (int-key count id)))
              (vaulttoken:module{fungible-v2} (at "token" vault-data))
              (vaultaccount (at "account" vault-data))
              (balance (vaulttoken::get-balance vaultaccount))
          )
            (+ (read vault-table (int-key count id)) {"balance": balance})
    )
  )

  (defun get-nft-vaults (id:string)
    (let*
        (
            (vault-count (at "count" (read vault-count-table id)) )
            (enum-count (enumerate 1 vault-count))
        )
        (map (get-vault id) enum-count)
    )
  )

; --------------------------------------------------------------------------
; Bubblegum
; --------------------------------------------------------------------------

  (defschema crosschain-schema
    id:string
    owner:string
    guard:guard
    )

  (defpact yank-crosschain:string
    ( account:string id:string target-chain:string )
    @doc "Copy a Bubblegum NFT to other chain"
    (step
      (with-capability (ACCOUNT_BALANCE_GUARD id account)

          (enforce (!= "" target-chain) "empty target-chain")
          (enforce (!= (at 'chain-id (chain-data)) target-chain)
            "cannot run cross-chain transfers to the same chain")
          (enforce (= (at 'chain-id (chain-data)) MAIN_CHAIN)
            "can only cross-chain from chain 1")

          (let*
              (
                  (ledger-data (read ledger (key id account)))
              )
              ;Log new chain
              (with-default-read nft-chains-table id
                { "chains" : [] }
                { "chains" := t-active_chains}
                (if (= (contains target-chain t-active_chains) false)
                  (update nft-chains-table id
                    {
                        "chains": (+ [target-chain] t-active_chains )
                    }
                  )
                  true
                )
              )

              (let
                ((crosschain-details:object{crosschain-schema}
                  { "id"       : id
                  , "owner" : account
                  , "guard" : (at "guard" ledger-data)
                  }
                ))
                (yield crosschain-details target-chain)
              )
           )

      )
    )

    (step
      (resume
        { "id"       := c_id
        , "owner" := c_account
        , "guard" := c_guard
        }
          (write ledger (key c_id c_account)
            {
                "id": c_id,
                "account": c_account,
                "balance": 2.0,
                "guard": c_guard
            }
          )
          (write metadata-table c_id
            { "id" : c_id
            , "meta"   : c_id
            , "bg" : 0
            })
          (write nft-chains-table c_id {"id":c_id, "chains": [MAIN_CHAIN]})
          (write marketplace c_id
            { "id" : c_id
            , "forsale"   : false
            , "owner" : c_account
            , "price" : 0.0
            })
      )
    )
  )

  (defcap TRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-unit id amount)
    (enforce (> amount 0.0) "Positive amount")
    (compose-capability (DEBIT id sender))
    (compose-capability (CREDIT id receiver))
  )

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )

  (defun create-account:string
    ( id:string
      account:string
      guard:guard
    )
    (enforce-valid-account account)
    (enforce (= (at 'chain-id (chain-data)) MAIN_CHAIN)
            "invalid chain")
    (insert ledger (key id account)
      { "balance" : 0.0
      , "guard"   : guard
      , "id" : id
      , "account" : account
      })
    )

  (defun transfer:string
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @doc "Transfer a Bubblegum NFT"
    (with-capability (ACCOUNT_BALANCE_GUARD id sender)
      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")
      (enforce-valid-transfer sender receiver (precision id) amount)

      (enforce (= (at 'chain-id (chain-data)) MAIN_CHAIN)
              "invalid chain")

      (update marketplace id
            { "id" : id
            , "forsale"   : false
            , "owner" : receiver
            , "price" : 0.0
            })

      (with-capability (TRANSFER id sender receiver amount)
        (debit id sender amount)
        (with-read ledger (key id receiver)
          { "guard" := g }
          (credit id receiver g amount))
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
    @doc "Transfer a Bubblegum NFT, creating a account as well"
    (with-capability (ACCOUNT_BALANCE_GUARD id sender)
      (update marketplace id
            { "id" : id
            , "forsale"   : false
            , "owner" : receiver
            , "price" : 0.0
            })

      (enforce (= (at 'chain-id (chain-data)) MAIN_CHAIN)
              "invalid chain")
      (enforce (!= sender receiver)
        "sender cannot be the receiver of a transfer")
      (enforce-valid-transfer sender receiver (precision id) amount)

      (with-capability (TRANSFER id sender receiver amount)
        (debit id sender amount)
        (credit id receiver receiver-guard amount))
     )
  )

  (defun debit:string
    ( id:string
      account:string
      amount:decimal
    )

    (require-capability (DEBIT id account))

    (enforce-unit id amount)

    (with-read ledger (key id account)
      { "balance" := balance }

      (enforce (<= amount balance) "Insufficient funds")

      (update ledger (key id account)
        { "balance" : (- balance amount) }
        ))
      (with-capability (CAN_UPDATE id)
        (update-supply id (- amount))
      )
  )

  (defun credit:string
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )

    (require-capability (CREDIT id account))

    (enforce-unit id amount)

    (with-default-read ledger (key id account)
      { "balance" : 0.0, "guard" : guard }
      { "balance" := balance, "guard" := retg }
      (enforce (= retg guard)
        "account guards do not match")

      (write ledger (key id account)
        { "balance" : (+ balance amount)
        , "guard"   : retg
        , "id"   : id
        , "account" : account
        })
        (with-capability (CAN_UPDATE id)
          (update-supply id amount)
        )
  ))

  (defpact transfer-crosschain:string
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step (enforce false "cross chain not supported"))
  )

  (defun mint
    ( account:string
      guard:guard
      meta:string
      bg:integer )
    @doc " Mint a Bubblegum NFT "
    ;Get supply
    (with-default-read supplies "bubblegum"
          { 'supply: 0.0 }
          { 'supply := total-supply }
        ;Enforce rules
        (enforce (< total-supply MAX_SUPPLY)  "Max supply reached" )
        (enforce (= (at 'chain-id (chain-data)) MAIN_CHAIN)
            "invalid chain")
        ;Enforce whitelist and payment
        (with-default-read whitelist-table account
          { 'account: account, 'purchased: true }
          { 'account := t_account, 'purchased := t_purchased }
          (let*
              (
                  (endtime-data (read endtime-table "bubblegum"))
                  (endtime:bool (at "ended" endtime-data))
              )
              (if (= endtime false)
                [(enforce (= t_purchased false) "Not on whitelist")
                (write whitelist-table account {"account": account, "purchased": true})]
                true
              )
              (with-default-read mint-table account
                { 'account: account, 'mintfree: false}
                { 'account := m_account, 'mintfree := m_mintfree }
                (if (= m_mintfree true)
                  (write mint-table account {"account": account, "mintfree": false})
                  (coin.transfer account BUBBLEGUM_BANK NFT_PRICE)
                )
              )
          )
        )
        ;Create nft and mint to buyer
        (let
            (
                (newid (hash (dec-key (+ total-supply 1.0) (format-time "%c" (at "block-time" (chain-data))))))
            )
            ;update ledger with buyer
            (write ledger (key newid account)
                { "balance" : 1.0
                , "guard"   : guard
                , "id"   : newid
                , "account" : account
                }
              )
              ;update metadata
              (insert metadata-table newid
                { "id" : newid
                , "meta"   : meta
                , "bg" : bg
                })
              ;update chains
              (insert nft-chains-table newid {"id":newid, "chains": [MAIN_CHAIN]})
              ;update marketplace
              (insert marketplace newid
                { "id" : newid
                , "forsale"   : false
                , "owner" : account
                , "price" : 0.0
                })
              ;update total supply
              (write supplies "bubblegum" {"supply": (+ total-supply 1.0)})
              (emit-event (SUPPLY "bubblegum" (+ total-supply 1.0)))
              (format "1 Bubblegum Purchased for {} KDA." [NFT_PRICE])
        )
    )
  )

  (defun sell
    ( account:string
      id:string
      price:decimal
      forsale:bool )
    @doc " Sell a Bubblegum NFT "
    (with-capability (ACCOUNT_BALANCE_GUARD id account)
    (enforce (= (at 'chain-id (chain-data)) MAIN_CHAIN)
            "invalid chain")
    ;update marketplace
      (update marketplace id
        { "id" : id
        , "forsale"   : forsale
        , "owner" : account
        , "price" : price
        })
        (format "Updated Market for Bubblegum {}." [id])
    )
  )

  (defun buy
    ( id:string
      account:string
      guard:guard )
    @doc " Buy a Bubblegum NFT "
    ;Get supply
    (with-read marketplace id
    { 'forsale := t_forsale
      ,"price" := t_price
      ,"owner" := t_seller }
    ;Enforce rules
    (enforce (= t_forsale true)  "NFT not for sale" )
    (enforce (= (at 'chain-id (chain-data)) MAIN_CHAIN)
            "invalid chain")
    ;Transfer payment
    (coin.transfer account t_seller t_price)
    ;Add bubblegum to buyer
    (write ledger (key id account)
      { "balance" : 1.0
      , "guard"   : guard
      , "id"   : id
      , "account" : account
      }
    )
    ;Remove bubblegum from seller
    (update ledger (key id t_seller)
      { "balance" : 0.0 }
    )
    ;Update marketplace
    (update marketplace id
      { "id" : id
      , "forsale"   : false
      , "owner" : account
      , "price" : 0.0
      })
    (format "1 Bubblegum Purchased for {} KDA." [t_price])
    )
  )

  (defun create-vault
    ( account:string
      id:string
      token:module{fungible-v2} )
    @doc " Create a vault inside a Bubblegum NFT "
    (with-capability (ACCOUNT_BALANCE_GUARD id account)
    (let
        (
          (new-vault-account:string (create-account-key id (get-token-key token)))
          (nft-data (read ledger (key id account)))
        )
        (with-default-read vault-count-table id
          { 'count: 0 }
          { 'count := vault-count }
          ;Create vault account
          (token::create-account new-vault-account (create-pool-guard id))
          ;insert vault
          (insert vault-table (int-key (+ vault-count 1) id)
            { "nftid" : id
            , "vault": (int-key (+ vault-count 1) id)
            , "account"   : new-vault-account
            , "token" : token
            })
          ;update vault count
          (write vault-count-table id
            { "id" : id
            , "count"   : (+ vault-count 1)
            })
          (format "Created Vault for Bubblegum {}." [id])
        )
    )
    )
  )

  (defun withdraw-vault
    ( account:string
      id:string
      vault:string
      amount:decimal )
    @doc " Withdraw tokens from a Bubblegum NFT "
    (with-capability (ACCOUNT_BALANCE_GUARD id account)
      (let*
          (
              (vault-data (read vault-table vault))
              (vault-account (at "account" vault-data))
              (vault-token:module{fungible-v2} (at "token" vault-data))
          )
          (install-capability (vault-token::TRANSFER vault-account account amount))
          (with-capability (PRIVATE_RESERVE id) (vault-token::transfer vault-account account amount))
          (format "Withdrew {} from Bubblegum {}." [amount id])
      )
    )
  )

  (defun deposit-vault
    ( account:string
      vault:string
      amount:decimal )
    @doc " Deposits tokens into a Bubblegum NFT "
      (let*
          (
              (vault-data (read vault-table vault))
              (vault-account (at "account" vault-data))
              (vault-token:module{fungible-v2} (at "token" vault-data))
          )
          (vault-token::transfer account vault-account amount)
          (format "Deposited {} tokens." [amount])
      )
  )

  (defun add-whitelist (account:string)
    (with-capability (GOVERNANCE)
    (write whitelist-table account
            { "account" : account
            , "purchased" : false })
    )
  )

  (defun end-whitelist ()
    (with-capability (GOVERNANCE)
    (write endtime-table "bubblegum"
            { "ended" : true})
    )
  )

  (defun get-whitelist (account:string)
    (let*
          (
              (wl-data (read whitelist-table account))
              (gw-data (read mint-table account))
          )
          (+ wl-data gw-data)
      )
  )

  (defun giveaway (account:string)
    (with-capability (GOVERNANCE)
    (write mint-table account
            { "account" : account
            , "mintfree" : true })
    )
  )

  (defun initialize ()
    (with-capability (GOVERNANCE)
      (insert endtime-table "bubblegum"
              { "ended" : false})
    )
  )


)

;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.ledger)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.supplies)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.metadata-table)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.marketplace)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.vault-table)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.vault-count-table)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.nft-chains-table)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.whitelist-table)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.endtime-table)
;(create-table n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.mint-table)
;(n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum.initialize)

