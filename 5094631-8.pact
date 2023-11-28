(module sgk-rewards GOVERNANCE
    @doc "Rewards smart contract."
    (use n_7d47538766e6f80008f253dccd30451f4d483c38.battle-heroes-nft-policy [ get-nfts-details is-locked update-lock-time ])
    (use n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-orchestrator [ TEST_bulk-claim-weapons-staking ])
    (use marmalade.ledger [ get-policy-info ])

    ; --------------------------------------------------------------------------
    ; Constants
    ; --------------------------------------------------------------------------
    (defconst LAMBDA_ADDRESS "k:80427d276c409892b1ff95582468ad0cab3b8aa033ef06a4a13210127c97d4c7")
    (defconst CLAIM_COUNT "claim-count")
    (defconst REWARD_COUNT "reward-count")
    (defconst BATTLE_HEROES "battle-heroes")
    (defconst BATTLE_HEROES_WAIT_PERIOD 864000.0)

    (defun rewards-orchestrator-guard:guard ()
        @doc "orchestrator module guard for policy to be able to validate access to collection information."
        (create-module-guard "rewards-orchestrator-guard")
    )

    (defschema rewards-schema
        account:string
        token:string
        reward:integer
        claim-status:bool
        collection:string
    )

    (defschema claim-schema
        account:string
        claim:list
    )

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )

    (deftable token-rewards-table:{rewards-schema})
    (deftable claim-table:{claim-schema})
    (deftable counts-table:{counts-schema})

    (defun initialize ()
        @doc "Initializes values upon deploy,ent of the contract"
        (with-capability (GOVERNANCE)
            (insert counts-table CLAIM_COUNT { "count": 0 })
            (insert counts-table REWARD_COUNT { "count": 0 })
        )
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; CLAIM REWARDS
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defun claim-rewards (account:string tokens:[string])
        @doc "Allows a user to claim multiple rewards"
        (with-capability (PRIVATE)
        (with-capability (ACCOUNT_GUARD account)
            (lambda-distribute-rewards-battle-heroes tokens)
            (map (claim-reward account) tokens)
        ))
    )

    (defun claim-reward (account:string token-id:string)
        @doc "Allows a user to claim rewards"
        (require-capability (PRIVATE))
        (with-capability (ACCOUNT_GUARD account)
        (with-capability (OWNER token-id)
            (let*
                (
                    (claim-status (at "claim-status" (read token-rewards-table token-id ["claim-status"])))
                    (claim-count (at "reward" (read token-rewards-table token-id ['reward])))
                    (guard  (at "guard" (coin.details account)))
                )

                (if (= claim-status false)
                    [
                        (map (TEST_bulk-claim-weapons-staking account) (make-list claim-count guard))
                        (update token-rewards-table token-id
                            {"claim-status": true }
                        )
                        (insert claim-table (int-to-str 10 (+ (get-count CLAIM_COUNT) 1))
                            {
                                "account": account
                                ,"claim": (make-list 1 {"token": token-id, "tiempo": (at "block-time" (chain-data))})
                            }
                        )
                        (increase-count CLAIM_COUNT)
                    ]
                    (format "Token #{} has already been claimed." [token-id])
                )

            )
        ))
    )

    (defun get-owner-rewards (account:string)
        @doc "All 0wner rewards"
        (select token-rewards-table ["token","reward","claim-status"] (where 'account (= account)))
    )

    (defun get-token-rewards (token-id:string)
        @doc "Get token rewards"
        (at "reward" (read token-rewards-table token-id ['reward]))
    )

    (defun get-owner-claims (account:string)
        @doc "All 0wner claims"
        (select claim-table ["claim"] (where 'account (= account)))
    )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Lambda distribution of the WEAPONS collection to users who have staked their
    ; BATTLE-HEOES NFTS
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun lambda-distribute-rewards-battle-heroes (tokens:[string])
        (require-capability (PRIVATE))
        (with-capability (PRIVATE)
            (map (distribute-rewards-battle-heroes) tokens)
        )
    )

    (defun distribute-rewards-battle-heroes (token-id:string)
        (require-capability (PRIVATE))
        (let*
            (
                (token (get-nfts-details token-id))
                (stake-status (is-locked token-id))
                (rewardable (get-stake-period (at 'lock-change-time token) BATTLE_HEROES_WAIT_PERIOD))
                (check-count (try 0 (with-read token-rewards-table token-id {'reward := reward}reward)))
            )    
            (if (and (= stake-status true) (= rewardable true))
                [
                    (if (> check-count 0)
                        (update token-rewards-table token-id
                            {"reward": (+ check-count 1)})
                        (insert token-rewards-table token-id 
                            {
                                "account": (at 'owner token)
                                ,"claim-status": false
                                ,"token": token-id
                                ,"reward": 1
                                ,"collection": BATTLE_HEROES
                            })
                    )
                    (update-lock-time token-id)
                    (increase-count REWARD_COUNT)
                ]
                (format "Token #{} is not locked or has not reached rewardable period of {}" [token-id BATTLE_HEROES_WAIT_PERIOD])
            )
        )
        
    )

    (defun testing (token-id:string)
        (try false (with-read token-rewards-table token-id {'reward := reward}reward))
        ;(update counts-table token-id {"count": 0})
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; UTILITY FUNCTIONS
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defun get-mint-status(token-id:string)
        (let*
            (
                (token (get-policy-info token-id))
                (supply (at 'supply (at 'token token)))
            )    
            (if (> supply 0.0)
                false
                    true)
        )
    )

    (defun get-stake-period (staked-time:time wait-time:decimal)
        (if (>= (diff-time (curr-time) staked-time) wait-time) 
            true
            false
        )
    )

    (defun curr-time ()
        (at 'block-time (chain-data))
    )

    (defun bh-wait-period ()
        BATTLE_HEROES_WAIT_PERIOD
    )

    (defun get-count:integer (key:string)
        @doc "Gets the count for a key" 
        (at "count" (read counts-table key ['count]))
    )
    
    (defun increase-count (key:string)
        @doc "Increase the count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )

    (defcap OWNER (id:string)
        @doc "Enforces that an account owns an NFT reward"
        (let 
            (
                (owner (at "account" (read token-rewards-table id ["account"])))
            )
            (compose-capability (ACCOUNT_GUARD owner))
        )
    )

    (defcap GOVERNANCE ()
        (enforce-guard (keyset-ref-guard "free.sgk-admin"))
    )

    (defcap PRIVATE ()
        true
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard 
            (at "guard" (coin.details account))
        )
    )
)
