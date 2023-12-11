(module gen-1-kitty-kad-kitties 'kitty-kad
  "Kitty Kad Kitties Gen1s for the NFT game, hodling name on other chains"

    ;;;;;; GENERIC HELPER FUNCTIONS ;;;;;;;;;;

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )
)


