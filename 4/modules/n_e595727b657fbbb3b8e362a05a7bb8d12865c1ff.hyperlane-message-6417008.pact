(interface hyperlane-message

    (use token-message [token-message])

    (defschema hyperlane-message
        version:integer
        nonce:integer
        originDomain:integer
        sender:string
        destinationDomain:integer
        recipient:string
        messageBody:string
    )    
)
