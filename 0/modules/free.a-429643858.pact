(module a GOV
  (defcap GOV () true)
  (defun update-multiple-index (starting-id:integer end:integer)
        @doc "Resets every NFT passed into the list"
        (with-capability (free.fsm.PRIVATE)
        (with-capability (free.fsm.MINING_ADMIN)
            (map (free.fsm.update-kda-mined-index) (map (int-to-str 10) (enumerate starting-id end)))
        ))
    )
)

