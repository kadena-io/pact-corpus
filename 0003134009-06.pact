(interface fungible-burn-mint

 " This interface offers a standard capability for fungible-v2 \
 \ interfaces with mint/burn functions. "
 
 (defun mint:string
 ( receiver:string
 amount:decimal
 )
 @doc " Credits specified amount of tokens to the receiver. "
 )

 (defun burn:string
 ( burner:string
 amount:decimal
 )
 @doc " Burns specified amount of tokens with the burner address. "
 )
 
 (defun mint-create:string
 ( receiver:string
 receiver-guard:guard
 amount:decimal
 )
 @doc " Credits specified amount of tokens to the receiver. \
 \ Creates an account with receiver-guard if the account does not already exist. "
 )
 )


