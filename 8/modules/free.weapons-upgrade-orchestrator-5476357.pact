(module weapons-upgrade-orchestrator GOVERNANCE
    @doc "Weapons Upgrade smart contract."
    (use marmalade.ledger [ get-policy-info details ])
    (use n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-orchestrator [get-next-non-minted-token mint])

    ; --------------------------------------------------------------------------
    ; Constants
    ; --------------------------------------------------------------------------
    (defconst SGK_ADDRESS "k:36990b871267ec4532551e505260806d7f39378cebb5ea2c998c80301c5a100f");"k:80427d276c409892b1ff95582468ad0cab3b8aa033ef06a4a13210127c97d4c7"
    (defconst BURN_ADDRESS "k:58b04710353d49920f30b49f163034017cd2f081bd4714737178f848c93e38a7")
    (defconst UPGRADE_COUNT "upgrade-count")
    (defconst UPGRADE_FEE 0.5);KDA

    ; --------------------------------------------------------------------------
    ; Utilities
    ; --------------------------------------------------------------------------
    (defun upgrade-orchestrator-guard:guard ()
        @doc "orchestrator module guard for policy to be able to validate access to collection information."
        (create-module-guard "upgrade-orchestrator-guard")
    )

    (defun enforce-upgrade(account:string tk-id-one:string tk-id-two:string)
        (require-capability (PRIVATE))
        (let*
            (
                (token-one (details tk-id-one account))
                (token-two (details tk-id-two account))
                (trait-one (at 'trait_type (get-token-info tk-id-one)))
                (trait-two (at 'trait_type (get-token-info tk-id-two)))
                (rarity-one (at 'rarity (get-token-info tk-id-one)))
                (rarity-two (at 'rarity (get-token-info tk-id-two)))
            )   
            (enforce (and (= account (at 'account token-one)) (= account (at 'account token-two))) "You must be the owner of the two tokens")
            (enforce (= trait-one trait-two) "Weapons must be of the same trait for upgrade") 
            (enforce (= rarity-one rarity-two) "Weapons must be of the same rarity for upgrade") 
        )
    )

    (defcap UPGRADE(account:string tk-id-one:string tk-id-two:string)
        @managed
        (compose-capability (PRIVATE))
        (compose-capability (UPGRADE-USER))
    )

    (defcap UPGRADE-USER ()
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

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )

    (defschema burn-history-schema
        @doc "Stores information about all burning activities."
        account:string
        info:object
    )

    (deftable key-counts-table:{counts-schema})
    (deftable burn-table:{burn-history-schema})

    (defun initialize ()
        @doc "Initializes values upon deployment of the contract"
        (with-capability (GOVERNANCE)
            (insert key-counts-table UPGRADE_COUNT { "count": 1 })
        )
    )

    (defun create-counts ()
        (map (create-count) ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32"])
    )

    (defun create-count (collection:string)
        [
            (insert key-counts-table (concat ["sgk-weapons-" collection "-2"]) {"count": 0})
            (insert key-counts-table (concat ["sgk-weapons-" collection "-3"]) {"count": 0})
            (insert key-counts-table (concat ["sgk-weapons-" collection "-4"]) {"count": 0})
        ]
    )

    (defun get-count:integer (key:string)
        @doc "Gets the count for a key" 
        (at "count" (read key-counts-table key ['count]))
    )
    
    (defun increase-count (key:string)
        @doc "Increase the count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update key-counts-table key {"count": (+ 1 (get-count key))})
    )

    (defun get-all-burn-history ()
        @doc "Returns all burn history"
        (select burn-table (where "account" (!= "")))
    )

    (defun get-burn-history (account:string)
        @doc "Returns account burn history"
        (select burn-table (where "account" (= account)))
    )

    ; --------------------------------------------------------------------------
    ; Functions
    ; --------------------------------------------------------------------------
    (defun upgrade (account:string tk-id-one:string tk-id-two:string)
        @doc "Allows a user to upgrade weapons"
        (with-capability (ACCOUNT_GUARD account)
        (with-capability (PRIVATE)
            (enforce-upgrade account tk-id-one tk-id-two)
            (let*
                (
                    (date (at "block-time" (chain-data)))
                    (event (format "Burned {} + {}" [tk-id-one tk-id-two]))
                    (trait_type (at 'trait_type (get-token-info tk-id-one)))
                    (rarity (at 'rarity (get-token-info tk-id-one)))
                    (next-mint-token (get-next-mint-token tk-id-one))
                )
                (marmalade.ledger.transfer-create tk-id-one account BURN_ADDRESS (at "guard" (coin.details BURN_ADDRESS)) 1.0)
                (marmalade.ledger.transfer-create tk-id-two account BURN_ADDRESS (at "guard" (coin.details BURN_ADDRESS)) 1.0)
                (marmalade.ledger.mint next-mint-token account (at "guard" (coin.details account)) 1.0)
                (coin.transfer account SGK_ADDRESS UPGRADE_FEE)
                (insert burn-table (int-to-str 10 (+ 1 (get-count UPGRADE_COUNT))) {
                    'account:account
                    ,'info:{"date": date, "event": event, "token": next-mint-token, "trait_type": trait_type, "rarity": rarity}
                })
                (increase-count UPGRADE_COUNT)
            )
        ))
    )

    (defun get-token-info (token:string)
        @doc "Gets token rarerity"
        (let*
            (
                (data (at 'attributes (at 'datum (at 0 (at 'data (at 'manifest (at 'token (get-policy-info token))))))))
                (rarity (at 'value (at 1 data)))
                (trait_type (at 'value (at 0 data)))
            ) 
            {"rarity": rarity, "trait_type": trait_type}
        )
        
    )

    (defun get-next-mint-token (token-id:string)
        (with-capability (PRIVATE)
            (let*
                (
                    (data (at 'token (get-policy-info token-id)))
                    (id (at 'id (at 'datum (at 0 (at 'data (at 'manifest data))))))
                    (col-name (drop (- (+ 1 (length id))) (at 'id data)))
                    (next-id (+ 1 (str-to-int (take -1 col-name))))
                    (next-col-id (concat [(drop -1 col-name) (int-to-str 10 next-id)]))
                    (next-mint-id (+ 1 (get-count next-col-id)))
                    (next-mint-token (concat [next-col-id ":" (int-to-str 10 next-mint-id)]))
                )  
                (increase-count next-col-id) 
                next-mint-token
            )
        )
    )

)
