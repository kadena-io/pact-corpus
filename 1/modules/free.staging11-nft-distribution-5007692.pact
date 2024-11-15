(module staging11-nft-distribution 'hype-admin-local-test-staging

  (implements staging11-callable)



  (defun call (method:string arguments)
      (create-module-guard "distribution-guard")
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
    nfts:integer
  )

  (deftable allocations-table:{allocation})

  (defschema project-metadata
    project:string
    nfts_minted:integer
    nfts_sold:integer
    nft_redeemed:integer
    nft_price:decimal
    default_price:decimal
    redeem_enabled:bool
  )
  
  (defcap IS_ADMIN ()
    (enforce-keyset 'hype-admin-local-test-staging))

  
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

  (defun create-project (project:string)
    (with-capability (IS_ADMIN)
        (insert project-metadata-table project {
            "project": project,
            "nfts_minted": 0,
            "nfts_sold": 0,
            "nft_redeemed": 0,
            "nft_price": 0.0,
            "default_price": 0.0,
            "redeem_enabled": false
      
        })
    )
  )

  (defconst NFT_BANK:string (read-msg 'nft-bank))

  (defun nft-index-key (project:string index:integer)
        (format "{}:{}" [index project])
  )

  (defun insert-nft (project:string nft:string)
    (with-capability (IS_ADMIN)
        (with-read project-metadata-table project {"nfts_minted":= nfts_minted}
            ;we use nfts_minted as an index key
            (insert nft-index-table (nft-index-key project nfts_minted) {"key": nft})
            (update project-metadata-table project {"nfts_minted": (+ 1 nfts_minted)})
        )
    )
  )

  (defun redeem-nft (project:string account:string)
    (with-read allocations-table (allocation-key project account) {"nfts":= nfts}
                (enforce (> nfts 0) "Nothing to redeem")
                (with-read project-metadata-table project {"nft_redeemed":= nfts_redeemed, "redeem_enabled" := redeem_enabled}
                        (enforce (= true redeem_enabled) "You can't redeem yet")
                        (map 
                          (lambda 
                              (index) 

                              (bind (read nft-index-table (nft-index-key project (+ nfts_redeemed index))) {
                                "key":=key
                                }
                                (install-capability (free.marmalade-staging11-ledger.MINT key account 1.0))
                                (free.marmalade-staging11-ledger.mint key account (at "guard" (coin.details account)) 1.0)
                              )
                          )
                         (enumerate 0 (- nfts 1))
                        )
                        (update project-metadata-table project {"nft_redeemed": (+ nfts_redeemed nfts)})
                        (update allocations-table (allocation-key project account) {"nfts": 0})
                )
      )
  )

  (defun can-invest (project:string account:string)
    (free.staging11-community-v2.is-round-winner project account)
  )

  (defun get-nft-price:decimal (project:string account:string)
        (with-read project-metadata-table project {"default_price":= default_price}
            (with-default-read account-pricing-table (account-pricing-key project account) {"price": default_price} {"price":= price}
                price
            )
        )
  )

  (defun get-participant-ga (project:string account:string)
      (free.staging11-community-v2.get-participant-ga project account)
  )

  (defun allocation-key (project:string account:string)
        (format "{}:{}" [account, project])
  )

  (defun buy-nfts (project_name:string account:string guard:guard nfts:integer)
        (let* (
            (round (free.staging11-community-v2.get-project-round project_name))
            (project (read project-metadata-table project_name))
            (nfts_sold (at "nfts_sold" project))
            (nfts-minted (at "nfts_minted" project))
            (ga (get-participant-ga project_name account))
            (allowed (can-invest project_name account))
            (price (* nfts (get-nft-price project_name account)))
        )
            (enforce (>= nfts-minted (+ nfts_sold nfts)) "Amount not available")
            (enforce allowed "User has not fulfilled Kyc or won lottery")
            (enforce (>= ga nfts) "Too many nfts")
            (with-default-read allocations-table account {"round": 0, "nfts":0} { "round":=last-round, "nfts":=last-nfts}
                (enforce (!= round last-round) "Already created an allocation in this round")
                (write allocations-table (allocation-key project_name account)
                {
                    "project": project_name,
                    "round": round,
                    "account": account,
                    "nfts":  (+ last-nfts nfts)
                }
                )
                (update project-metadata-table project_name {"nfts_sold": (+ nfts nfts_sold)})
                ;(install-capability (coin.TRANSFER account NFT_BANK price))
                ;(coin.transfer account NFT_BANK price)
                nfts
                
            )
        )
    )
)



