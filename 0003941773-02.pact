(module bulk-increase-count-test1 'admin-keyset-888564447
  
    (defconst COUNT1-KEY "count1-key")
    (defconst COUNT2-KEY "nft-count-key")

    (defschema nft-schema
        id:string
        owner:string
    )

    (deftable nft-table:{nft-schema})
    (deftable second-nft-table:{nft-schema})

    ; (defun initialize ()
    ;     ; (insert counts-table COUNT1-KEY {"count": 0})
    ;     ; (insert counts-table COUNT2-KEY {"count": 1})
    ; )

    (defun create-nft-to-mint-multiple (accounts:list)
        (map 
            (create-nft-to-mint)
            accounts
        )
        (format "created for {}" [accounts])
    )

    (defun create-nft-to-mint (account:string)
        (insert nft-table account
            { "id": "placeholder"
            , "owner": account })
    )

    (defun create-nft-to-mint-multiple2 (accounts:list)
        (map 
            (create-nft-to-mint2)
            accounts
        )
        (format "created for {}" [accounts])
    )

    (defun create-nft-to-mint2 (account:string)
        (insert second-nft-table account
            { "id": "placeholder"
            , "owner": account })
    )




)


