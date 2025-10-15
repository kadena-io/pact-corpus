(module cwc-treasury GOVERNANCE
  (use kaddex.exchange [get-pair-key get-pair-by-key reserve-for])
  (use free.util-math)
  (use free.util-time)
  (use free.util-lists)

  ;-----------------------------------------------------------------------------
  ; Administrative and ops capabilities
  ;-----------------------------------------------------------------------------
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "n_c89f6bb915bf2eddf7683fdea9e40691c840f2b6.governance")))

  (defcap OPERATOR ()
    (enforce-guard (keyset-ref-guard "n_c89f6bb915bf2eddf7683fdea9e40691c840f2b6.operator")))

  (defcap OPERATE-TREASURY ()
    (compose-capability (OPERATOR))
    (compose-capability (TREASURY)))

  
  ;-----------------------------------------------------------------------------
  ; Caps and internal accounts management
  ;-----------------------------------------------------------------------------
  (defcap TREASURY () true)
  (defconst TREASURY-GUARD (create-capability-guard (TREASURY)))
  (defconst TREASURY-ACCOUNT (create-principal TREASURY-GUARD))

  ; Using a defconst for the DEX key saves gas
  (defconst DEX-KEY (get-pair-key coin cwc))

  ; Maximum amount of CWC that can be distributed at once to an individual
  (defconst MAX-DISTRIBUTE:decimal 100.0)

  ;-----------------------------------------------------------------------------
  ; Utility functions
  ;-----------------------------------------------------------------------------
  (defun div:decimal (num:decimal den:decimal)
    @doc "Divide and limit numbers of decimal (gas optimisation)"
    (floor (/ num den) 24))

  (defun get-pair ()
    (get-pair-by-key DEX-KEY))

  (defun dex-reserves:[decimal] ()
    @doc "Returns the Reserve of the DEXs to an array [Token $KDA]"
    (let ((pair (get-pair)))
      [(reserve-for pair cwc), (reserve-for pair coin)]))

  (defun dex-account:string ()
    @doc "Returns the DEX's pair account"
    (at 'account (get-pair)))

  (defun liquidity-ratio:decimal ()
    @doc "Returns the ratio (Geom Mean of liquidity) / (LP total supply)"
    (div (sqrt (prod (dex-reserves)))
         (kaddex.tokens.total-supply DEX-KEY)))

  (defun kda-balance:decimal ()
    @doc "Coin balance of the Treasury account"
    (coin.get-balance TREASURY-ACCOUNT))

  (defun cwc-balance:decimal ()
    @doc "Token balance of the Treasury account"
    (cwc.get-balance TREASURY-ACCOUNT))


  ;-----------------------------------------------------------------------------
  ; Public functions
  ;-----------------------------------------------------------------------------

  (defun bulk-send-cwc (accounts:[string] amounts:[decimal])
    @doc "Send CWC to multiple receivers with varying amounts"
    
    (enforce (= (length accounts) (length amounts)) "Number of accounts must match number of amounts")
    (enforce (> (length accounts) 0) "Must provide at least one account")
    (enforce (<= (length accounts) 50) "Too many accounts are being sent to")

    (let* ((total-to-send (fold (+) 0.0 amounts))
          (treasury-balance (cwc-balance)))
      
      (enforce (<= total-to-send treasury-balance) "Insufficient Treasury balance for bulk transfer")
      (enforce (<= total-to-send (* (dec (length accounts)) MAX-DISTRIBUTE)) "Total transfer exceeds maximum allowed")
      
      (with-capability (OPERATE-TREASURY)
        ; Install capabilities
        (zip (lambda (account amount)
              (install-capability (cwc.TRANSFER TREASURY-ACCOUNT account amount)))
            accounts
            amounts)
        
        ; Perform transfers
        (zip (lambda (account amount)
              (cwc.transfer-create TREASURY-ACCOUNT account 
                (at 'guard (coin.details account)) amount))
            accounts
            amounts))
    )
  )

  (defun send-cwc (receiver:string amount:decimal)
    @doc "Send CWC to the receiver based on NFT level"
    (let* (
      (g (at 'guard (coin.details receiver)))
      (b (cwc-balance)))
    (with-capability (OPERATE-TREASURY)
    (enforce (<= amount b) "Insufficient Treasury balance")
    (enforce (and (> amount 0.0) (<= amount MAX-DISTRIBUTE)) "Invalid amount")
    (install-capability (cwc.TRANSFER TREASURY-ACCOUNT receiver amount))
    (cwc.transfer-create TREASURY-ACCOUNT receiver g amount)
    ))
  )

)

