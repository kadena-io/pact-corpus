(interface wrapped-mint-burn

    (defun mint:bool (account:string guard:guard amount:decimal)
      @doc " Credits specified amount of tokens to the receiver. "
    )
  
    (defun burn:bool (account:string amount:decimal)
      @doc " Burns specified amount of tokens with the burner address. "
    )
  )
