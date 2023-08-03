(interface wiza1-interface-v3

    (defun spend-wiza:bool (amount:decimal account:string))

    (defun check-nft-is-staked:bool (idnft:string))

    (defcap TRANSFER:bool
      ( sender:string
        receiver:string
        amount:decimal
      )
      @doc " Managed capability sealing AMOUNT for transfer from SENDER to \
           \ RECEIVER. Permits any number of transfers up to AMOUNT."
      @managed amount TRANSFER-mgr
      )

      (defun TRANSFER-mgr:decimal
        ( managed:decimal
          requested:decimal
        )
        @doc " Manages TRANSFER AMOUNT linearly, \
             \ such that a request for 1.0 amount on a 3.0 \
             \ managed quantity emits updated amount 2.0."
        )

    (defun transfer-create:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal ))
)

