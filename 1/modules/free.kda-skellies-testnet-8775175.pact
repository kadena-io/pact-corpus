(module kda-skellies-testnet GOV
	"These are not the Skellies you are looking for"
  
    (defconst ADMIN_KEYSET "free.skellies-admin-keys")
	(defconst ADMIN_ADDRESS "k:471957f94679ce95351ff042789ddda969b2c51788ee0a9b468bfae5c2506664")


    (defcap GOV () 
        (enforce-keyset ADMIN_KEYSET)
    ) 

    (defcap PRIVATE () 
        @doc "Can only be called from a private context"
        true
    ) 

    (defcap ACCOUNT_GUARD(account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard   
            (at "guard" (coin.details account))
        )
    )
  
    (defcap ADMIN() 
        @doc "Only allows admin to call these"
        (enforce-keyset  ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
	)
)
