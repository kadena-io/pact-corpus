(module gen-0-rewards-orchestrator GOVERNANCE
    @doc "Rewards smart contract."
    (use marmalade.ledger [ get-policy-info ])

    (use n_7d47538766e6f80008f253dccd30451f4d483c38.sgk-gen-0-policy [get-nfts-details is-locked])
    (use n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-orchestrator [TEST_bulk-claim-weapons-staking])

    ; --------------------------------------------------------------------------
    ; Constants
    ; --------------------------------------------------------------------------
    (defconst LAMBDA_ADDRESS "k:80427d276c409892b1ff95582468ad0cab3b8aa033ef06a4a13210127c97d4c7")
    (defconst CLAIM_COUNT "claim-count")
    (defconst REWARD_COUNT "reward-count")
    (defconst BATTLE_HEROES "battle-heroes")
    (defconst BATTLE_HEROES_WAIT_PERIOD 864000.0)

    (defcap REWARDS(owner:string token-ids:[string])
        @managed
        (compose-capability (PRIVATE))
        (compose-capability (REWARD-USER))
    )

    (defcap REWARD-USER ()
        true
    )

    (defcap GOVERNANCE ()
        (enforce-one "Any Guard passes" [(enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
        (enforce-guard (at "guard" (coin.details "k:67a044c7585344504de4d3ae116b866e5929031113ee24f7d48fa4013dd67c4c")))
        (enforce-guard (at "guard" (coin.details "k:a3ecc7fc15052ea4ffecad3035bad35c8e3b20a70ddb5227e4c35d227e4c0d13")))])
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
        (with-capability (REWARDS account tokens)
        (with-capability (ACCOUNT_GUARD account)
            (map (claim-reward account) tokens)
            (TEST_bulk-claim-weapons-staking account (length tokens) (at "guard" (coin.details account)))
        ))
    )

    (defun claim-reward (account:string token-id:string)
        @doc "Allows a user to claim rewards"
        (require-capability (PRIVATE))
        (with-capability (ACCOUNT_GUARD account)
            (let*
                (
                    (token (sgk-gen-0-policy.get-nfts-details token-id))
                    (stake-status (sgk-gen-0-policy.is-locked token-id))
                    (rewardable (get-stake-period (at 'lock-change-time token) BATTLE_HEROES_WAIT_PERIOD))
                )
                (enforce (= account (at 'owner token)) "You must be the owner of the token")
                (enforce (and (= stake-status true) (= rewardable true)) "Token must be staked and rewardable")
                (increase-count CLAIM_COUNT)

            )
        )
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


)

    
