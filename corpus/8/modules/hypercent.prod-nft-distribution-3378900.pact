(module prod-nft-distribution GOVERNANCE
  (defcap GOVERNANCE ()
      (enforce-guard (keyset-ref-guard "hypercent.hyper-admin-1-prod")))

  (defcap IS_ADMIN ()
      (enforce-keyset "hypercent.hyper-api-admin-prod"))

  (implements hypercent.callable-v3)

  (defun call:guard (method:string)
    (create-module-guard "locker-guard")
  )

  (defschema nft-index
     key: string  
  )

  (deftable nft-index-table:{nft-index})

  (defschema account-pricing
     project:string
     price:decimal  
  )
  (deftable account-pricing-table:{account-pricing})

  (defcap NFT_DISTRIBUTION () true)
  (defschema allocation
    project:string
    account:string
    round:integer
    nfts:[integer]
  )

  (deftable allocations-table:{allocation})

  (defschema project-metadata
    project:string
    seed:string
    nfts_created:integer
    nfts_sold:integer
    nft_price:decimal
    default_price:decimal
    redeem_enabled:bool
    random:[integer]
    nfts_reserved:[integer]

  )
  
  (deftable project-metadata-table:{project-metadata})

  (defun account-pricing-key (project:string account:string)
    (+ project account)
  )

  
  (defun distribution-guard:guard ()
    @doc "Distribution module guard for policies to be able to validate access to policy operations."
    (create-user-guard 'distribution-guard)
  )

  (defun set-account-price (project:string account:string price:decimal)
    (with-capability (IS_ADMIN)
        (write account-pricing-table (account-pricing-key project account) {"project": project, "price": price})
    )
  )

  (defun set-project-redeemable (project:string redeem_enabled:bool)
      (with-capability (IS_ADMIN)
        (update project-metadata-table project {"redeem_enabled":  redeem_enabled})
      )
  )

  ;todo remove
  ;  (defun get-project (project:string)
  ;    (read project-metadata-table project)
  ;  )

  (defun create-project (project:string nfts_reserved:[integer])
    (with-capability (IS_ADMIN)
        (insert project-metadata-table project {
            "project": project,
            "seed": project,
            "nfts_created": 0,
            "nfts_sold": 0,
            "nft_price": 0.0,
            "default_price": 0.0,
            "redeem_enabled": false,
            "random":[],
            "nfts_reserved": nfts_reserved
      
        })
    )
  )

  (defun set-nft-price (project:string price:decimal)
      (with-capability (IS_ADMIN)
        (update project-metadata-table project {"default_price": price})
      )
  )

  (defun get-project-seed (project:string)
      (at "seed" (read project-metadata-table project))
  )

  (defconst NFT_BANK_ACCT:string (read-msg 'nft-bank))
  (defun nft-bank-guard () (keyset-ref-guard "hypercent.hyper-admin-1-prod"))

  (defun nft-index-key (project:string index:integer)
        (format "{}:{}" [index project])
  )

  (defun insert-nft (project:string nft:string)
    (with-capability (IS_ADMIN)
        (with-read project-metadata-table project {"nfts_created":= nfts_created}
            ;we use nfts_created as an index key
            (insert nft-index-table (nft-index-key project nfts_created) {"key": nft})
            (update project-metadata-table project {"nfts_created": (+ 1 nfts_created)})
        )
    )
  )

  (defun set-random-mapping (project:string nfts:[integer])
    (with-capability (IS_ADMIN)
          (update  project-metadata-table project {"random": nfts})
    )
  )

  (defun get-random-mapping (project_name:string)
    (let* (
        (project (read project-metadata-table project_name))
        (nfts_reserved (at "nfts_reserved" project))
        (nfts_sold (at "nfts_sold" project))
        (seed (at "seed" project))
        (size (at "nfts_created" project))
        (empty_arr (enumerate 0 (- size 1)))
        (randomizer (lambda (seed)  (let* ((seed2 (* 11795372955171141389 (+ seed 6971258582664805397)))(m1 (* 1946526487930394057 (xor (shift seed2 -16) seed2)))) (& (shift (+ (* 214013 (xor (shift m1 -64) m1)) 2531011) -64) 327670))))
        (random (fold (lambda (result curr) (+ result [(randomizer (fold (*) 1 (take -1 result)))])) [(randomizer (str-to-int 64 (hash seed)))] empty_arr))) 
        (take nfts_sold (filter (lambda (id) (= false (contains id nfts_reserved))) (map (at "i") (sort ["rand"] (map (lambda (i) {"i": i, "rand": (at i random)}) empty_arr)))))
      
    )
  )


  (defun admin-mint (key:string k_account:string)
      (with-capability (IS_ADMIN)
        (install-capability (marmalade.ledger.MINT key k_account 1.0))
        (marmalade.ledger.mint key k_account (at "guard" (coin.details k_account)) 1.0)
      )
  )


  (defun redeem-nft (project:string account:string)
    (with-read allocations-table (allocation-key project account) {"nfts":= nfts}
                (enforce (> (length nfts) 0) "Nothing to redeem")
                (with-read project-metadata-table project { "redeem_enabled" := redeem_enabled, "random" := random}
                        (enforce (= true redeem_enabled) "You can't redeem yet")
                        (enforce (!= [] random) "You can't redeem yet")
                        (let* (
                          (tokens (map 
                            (lambda 
                                (index) 
                                (bind (read nft-index-table (nft-index-key project (at index random))) {
                                  "key":=key
                                  }
                                  key
                                )
                            )
                           nfts
                          ))
                        )
                        
                        (hypercent.prod-nft-sale-royality-policy.allow-mint account project tokens)
                        (map 
                          (lambda 
                              (key) 

                              (install-capability (marmalade.ledger.MINT key account 1.0))
                              (marmalade.ledger.mint key account (at "guard" (coin.details account)) 1.0)
                          )
                         tokens
                        )
          
                        )
                        (update allocations-table (allocation-key project account) {"nfts": []})
                )
      )
  )

  (defun can-invest (project:string account:string)
    (hypercent.prod-community-v2.is-nft-round-winner project account)
  )

  (defun get-nft-price:decimal (project:string account:string)
       (let* ((round (hypercent.prod-community-v2.get-project-round project)))
        (with-read project-metadata-table project {"default_price":= default_price}
            (if (= round 1) 
              (with-default-read account-pricing-table (account-pricing-key project account) {"price": default_price} {"price":= price}
                price
              ) 
              default_price
            )
        )
       )  
  )

  (defun get-participant-ga (project:string account:string)
      (hypercent.prod-community-v2.get-nft-participant-ga project account)
  )

  (defun allocation-key (project:string account:string)
        (format "{}:{}" [account, project])
  )

  (defun buy-nfts (project_name:string account:string nfts:integer)
        (let* (
            (round (hypercent.prod-community-v2.get-project-round project_name))
            (project (read project-metadata-table project_name))
            (nfts_sold (at "nfts_sold" project))
            (nfts_created (at "nfts_created" project))
            (nfts_reserved (at "nfts_reserved" project))
            (ga (get-participant-ga project_name account))
            (allowed (can-invest project_name account))
            (price (* nfts (get-nft-price project_name account)))
        )
            (enforce (= true (at "redeem_enabled" project)) "Sale closed")
            (enforce (>= (- nfts_created (length nfts_reserved)) (+ nfts_sold nfts)) "Amount not available")
            (enforce allowed "User has not fulfilled Kyc or won lottery")
            (enforce (>= ga nfts) "Too many nfts")
            (with-default-read allocations-table account {"round": 0, "nfts":[]} { "round":=last-round, "nfts":=last-nfts}
                (enforce (!= round last-round) "Already created an allocation in this round")
                (write allocations-table (allocation-key project_name account)
                {
                    "project": project_name,
                    "round": round,
                    "account": account,
                    "nfts":  (+ last-nfts (enumerate nfts_sold (+ nfts_sold (- nfts 1))))
                }
                )
                (update project-metadata-table project_name {"nfts_sold": (+ nfts nfts_sold), "seed": (hash [(at "seed" project), account, nfts, (at "block-time" (chain-data))]) })
                (install-capability (coin.TRANSFER account NFT_BANK_ACCT price))
                (coin.transfer account NFT_BANK_ACCT price)
                ;(+ last-nfts (enumerate nfts_sold (+ nfts_sold (- nfts 1))))
                nfts
            )
        )
    )

    (defun get-allocated-nfts:object{allocation} (project:string account:string)
        (read allocations-table (allocation-key project account))
    )

    (defun can-redeem-nft (project:string)
        (at "redeem_enabled" (read project-metadata-table project))
    )

    (defun get-account-collection-tokens (project:string account:string)
      (map (marmalade.ledger.get-manifest) (filter (lambda (id) (> (marmalade.ledger.get-balance id account) 0.0)) (hypercent.prod-nft-sale-royality-policy.get-account-minted-nfts account project)))
    )

    (defun init ()
      (coin.create-account NFT_BANK_ACCT (nft-bank-guard))
      "empty lock succeeded"
    )
)



