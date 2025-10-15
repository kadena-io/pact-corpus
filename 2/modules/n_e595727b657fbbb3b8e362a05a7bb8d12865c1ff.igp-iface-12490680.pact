(interface igp-iface

  (defschema igp-state
    treasury:string
  )

  (defschema remote-gas-amount-input
    domain:integer
    gas-amount:decimal
  )

  (defschema remote-gas-amount
    gas-amount:decimal
  )
  
  (defun pay-for-gas:bool (id:string domain:integer gas-amount:decimal)
    @doc "Deposits tokens as a payment for the relaying of a message to its destination chain."
  )

  (defun quote-gas-payment:decimal (domain:integer)
    @doc "Quotes the amount of native tokens to pay for interchain gas."
  )
)
  
