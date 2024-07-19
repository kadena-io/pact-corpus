(module sgk-rewards-policies GOVERNANCE
    @doc "Rewards smart contract."

    ; --------------------------------------------------------------------------
    ; Constants
    ; --------------------------------------------------------------------------
    (defconst LAMBDA_ADDRESS "k:6c9473355b7f5bb66c25006cfe2f3b7d2630dc032ed6dbc5e0a372bbeabd35a5")
    (defconst CLAIM_COUNT "claim-count")
    (defconst REWARD_COUNT "reward-count")
    (defconst GEN_ZERO "gen-zero")
    (defconst BATTLE_HEROES "gen-zero")
    (defconst GEN_ZERO_WAIT_PERIOD 604800.0)
    (defconst BATTLE_HEROES_WAIT_PERIOD 864000.0)

    (use free.test-sgk-battle-heros-policy)
    (use marmalade.ledger)
    (use n_7d47538766e6f80008f253dccd30451f4d483c38.sgk-gen-0-policy) 

    (defschema rewards-schema
        account:string
        token:string
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

    (deftable reward-table:{rewards-schema})
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
    (defun claim-rewards (account:string ids:[string])
        @doc "Allows a user to claim rewards"
        (with-capability (ACCOUNT_GUARD account)
        (with-capability (PRIVATE)
            (map (claim-reward account) ids)
            (format "Claimed ids {}" [ids])
        ))
        
    )

    (defun claim-reward (account:string id:string)
        (require-capability (PRIVATE))
        (with-capability (OWNER id)
            ;TO BE DONE
            ;Check if the token id exist on the weapon smart contract and it has not been minted yet, if yes then mint the NFT for the user
            (let*
                (
                    (claim-status (at "claim-status" (read reward-table id ["claim-status"])))
                )

                (if (= (claim-status false))
                    [
                        ;mint NFT based on the collection type on weapon smart contract
                        (update reward-table id
                            {"claim-status": true }
                        )
                        (insert claim-table (int-to-str 10 (+ (get-count CLAIM_COUNT) 1))
                            {
                                "account": account
                                ,"claim": (make-list 1 {"token": id, "tiempo": (at "block-time" (chain-data))})
                            }
                        )
                        (increase-count CLAIM_COUNT)
                    ]
                    (format "Token #{} has already been claimed." [id])
                )

            )
        )
    )

    (defun get-owner-rewards (account:string)
        @doc "All 0wner rewards"
        (select reward-table ["token","claim-status"] (where 'account (= account)))
    )

    (defun get-token-rewards (token:string)
        @doc "All 0wner rewards"
        (select reward-table ["claim-status"] (where 'token (= token)))
    )

    (defun get-owner-claims (account:string)
        @doc "All 0wner claims"
        (select claim-table ["claim"] (where 'account (= account)))
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Lambda distribution of the WEAPONS collection to users who have staked their
    ; GEN-0 NFTS
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun lambda-mint-rewards-gen-zero ()
        (with-capability (ACCOUNT_GUARD LAMBDA_ADDRESS)
        (with-capability (PRIVATE)
            (let
                (
                    (tokens (test-sgk-battle-heros-policy.get-all-nfts))
                )
                (map (mint-rewards-gen-zero) tokens)
            )
        ))
    )

    (defun mint-rewards-gen-zero (token:object)
        (require-capability (PRIVATE))
        (let*
            (
                (stake-status (test-sgk-battle-heros-policy (at 'token-id token)))
                (rewardable (get-stake-period (at 'lock-change-time token) GEN_ZERO_WAIT_PERIOD))
            )    
            (if (and (= stake-status true) (= rewardable true))
                [
                    ;TODO: Mint not yet available
                    (insert reward-table (int-to-str 10 (+ (get-count REWARD_COUNT) 1))
                        {
                            "account": (at 'owner token)
                            ,"claim-status": false
                            ,"token": (at 'token-id token)
                            ,"collection": GEN_ZERO
                        }
                    )
                    (increase-count REWARD_COUNT)
                ]
                (format "Token #{} is not staked" [(at 'token-id token)])
            )
        )
        
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Lambda distribution of the WEAPONS collection to users who have staked their
    ; BATTLE-HEOES NFTS
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defun lambda-mint-rewards-battle-heroes ()
        (with-capability (ACCOUNT_GUARD LAMBDA_ADDRESS)
        (with-capability (PRIVATE)
            (let
                (
                    (tokens (test-sgk-battle-heros-policy.get-all-nfts))
                )
                (map (mint-rewards-battle-heroes) tokens)
            )
        ))
    )

    (defun mint-rewards-battle-heroes (token:object)
        (require-capability (PRIVATE))
        (let*
            (
                (stake-status (test-sgk-battle-heros-policy.is-locked (at 'token-id token)))
                (rewardable (get-stake-period (at 'lock-change-time token) BATTLE_HEROES_WAIT_PERIOD))
            )    
            (if (and (= stake-status true) (= rewardable true))
                [
                    ;TODO: Weapons/Gears mint
                    (insert reward-table (int-to-str 10 (+ (get-count REWARD_COUNT) 1))
                        {
                            "account": (at 'owner token)
                            ,"claim-status": false
                            ,"token": (at 'token-id token)
                            ,"collection": BATTLE_HEROES
                        }
                    )
                    (increase-count REWARD_COUNT)
                ]
                (format "Token #{} is not staked" [(at 'token-id token)])
            )
        )
        
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; UTILITY FUNCTIONS
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defun mint(owner:string guard:guard token-id:string)
        (install-capability (marmalade.ledger.MINT token-id owner 1.0))
        (marmalade.ledger.mint token-id owner guard 1.0)
    )

    (defun get-token-info(token-id:string)
        (let*
            (
                (token (marmalade.ledger.get-policy-info token-id))
                (token-info (at 'token token))
            )    
            token-info
        )
    )

    (defun get-stake-period (staked-time:decimal wait-time:decimal)
        (if (>= (diff-time (curr-time) staked-time) wait-time) 
            true
            false
        )
    )

    (defun curr-time ()
        (at 'block-time (chain-data))
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
                (owner (at "account" (read reward-table id ["account"])))
            )
            (compose-capability (ACCOUNT_GUARD owner))
        )
    )

    (defun rewards-orchestrator-guard:guard ()
        @doc "orchestrator module guard for policy to be able to validate access to collection information."
        (create-module-guard "rewards-orchestrator-guard")
    )

    (defcap GOVERNANCE ()
        (enforce-guard (keyset-ref-guard "free.sgk-keyset"))
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
