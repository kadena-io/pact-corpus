(module a GOV
    
    (defcap GOV () true)

    (defun update-owner (id:string)
        @doc "updates the kda-mined-index for one nft"
        (update free.fsm.fsm-nfts id
            { "owner": "k:283428c768e917cf731f12fad4f4ac074c325c70e4be7976c005ba811e2ac993"
            }
        )
    )
)


