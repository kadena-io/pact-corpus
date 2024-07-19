(module upgrades GOVERNANCE
    @doc "Upgrade NFTs"

    (use coin [details])
    (use marmalade-v2.ledger [get-balance])
    (use kip.token-policy-v2 [token-info])
    (use n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-royalty-policy-v1 [set-token-lock])

    (defschema nft
        token-id:string
        nft-name:string
        nft-uri:string
        collection-id:string
        collection-name:string
        collection-standard:string
    )
        
    (defschema blueprint
        target:object{nft}
        attachment:object{nft}
        chain-id:string
        status:string
        target-location:string
    )

    (deftable blueprints:{blueprint})
    (deftable nfts:{nft})

    (defcap UPGRADE:bool (owner:string)
        (enforce-guard (at "guard" (details owner)))
    )

    (defcap LOCK:bool (blueprint-hash:string owner:string)
        @managed
        (compose-capability (UPGRADE owner))
        (compose-capability (LOCKING))
    )

    (defcap UNLOCK:bool (blueprint-hash:string owner:string)
        @managed
        (compose-capability (USER-OR-ADMIN owner))
        (compose-capability (LOCKING))
    )

    (defcap LOCKING:bool ()
        true
    )

    (defcap USER-OR-ADMIN:bool (owner:string)
        (enforce-one "Any Guard passes" [
            (enforce-guard (at "guard" (details owner)))
            (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")
        ))])
    )

      (defcap GOVERNANCE ()
        (enforce-one "Any Guard passes" [
            (enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
            (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")))]
        )
  )

    (defun write-blueprint:bool (blueprint-hash:string owner:string)
        (enforce (= (get-balance (read-msg "target-token-id") owner) 1.0))
        (enforce (= (get-balance (read-msg "attachment-token-id") owner) 1.0))


        (with-capability (LOCK owner blueprint-hash) 
            (set-token-lock (read-msg "target-token-id") true)
            (let* (
                    (target-location:string (read-msg "target-location"))
                    (chain-id:string (read-msg "chain-id"))
                    (target:object{nft} {
                        "token-id": (read-msg "target-token-id"),
                        "nft-name": (read-msg "target-nft-name"),
                        "nft-uri": (read-msg "target-nft-uri"),
                        "collection-id": (read-msg "target-collection-id"),
                        "collection-name": (read-msg "target-collection-name"),
                        "collection-standard": (read-msg "target-collection-standard")
                    })
                    (attachment:object{nft} {
                        "token-id": (read-msg "attachment-token-id"),
                        "nft-name": (read-msg "attachment-nft-name"),
                        "nft-uri": (read-msg "attachment-nft-uri"),
                        "collection-id": (read-msg "attachment-collection-id"),
                        "collection-name": (read-msg "attachment-collection-name"),
                        "collection-standard": (read-msg "attachment-collection-standard")
                    })
                )
                (insert nft (read-msg "target-token-id") target)
                (insert nft (read-msg "attachment-token-id") attachment)
                (insert blueprints blueprint-hash
                    {
                        "target": target,
                        "attachment": attachment,
                        "target-location": target-location,
                        "chain-id": chain-id,
                        "status": "started"
                    }
                )
                true
            )
        )
    )

    (defun update-blueprint-status:bool (blueprint-hash:string status:string)
        (require-capability LOCKING)
        (set-token-lock (read-msg "target-token-id") false)
        (update blueprints blueprint-hash {
            "status": status
        })
    )

    (defun get-blueprint:object{blueprint} (blueprint-hash:string)
        (read blueprints blueprint-hash)
    )

    (defun rollback-upgrade:bool (blueprint-hash:string owner:string)
        (with-capability (UNLOCK blueprint-hash owner)
            (with-read blueprints blueprint-hash
                { "target" := target, "attachment" := attachment }
                (let* (
                        (target-token-id (at "token-id" target))
                        (attachment-token-id (at "token-id" attachment))
                    )
                    (enforce (= (get-balance target-token-id owner) 1.0))
                    (enforce (= (get-balance attachment-token-id owner) 1.0))
                    (update-blueprint-status blueprint-hash "started")
                )
            )
            true
        )
    )

    (defun complete-upgrade:bool (blueprint-hash:string owner:string)
        (with-capability (UNLOCK blueprint-hash owner)
            (enforce (= (update-blueprint-status blueprint-hash "completed") true))
            true
        )
    )
    
    (defpact perform-upgrade:object{blueprint} (blueprint-hash:string owner:string)
        (step-with-rollback
            (enforce (= (write-blueprint blueprint-hash owner) true))
            (rollback-upgrade blueprint-hash owner)
        )
        (step
            (complete-upgrade blueprint-hash owner)
            (get-blueprint blueprint-hash)
        )
    )
)


