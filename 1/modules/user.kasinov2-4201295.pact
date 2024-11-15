(module kasinov2 GOVERNANCE

  "'kasinov2' represents Kasino Contract."

  ;; TODO - use hashed import
  (use coin)

  ; --------------------------------------------------------------------------
  ; Capabilities
  ; --------------------------------------------------------------------------

  (defcap GOVERNANCE ()
    (enforce-guard (at 'guard (details 'kasino-games-admin )))
  true)

  (defcap PRIVATE ()
    true
  )
  ; --------------------------------------------------------------------------
  ; Schemas and Tables
  ; --------------------------------------------------------------------------

  (defschema history
    @doc "Table to record the behavior of addresses. Last transaction time,       \
    \ total coins earned, and total coins returned are inserted or updated at     \
    \ transaction. "
    total-coins-earned:decimal
    total-coins-returned:decimal
    last-request-time:time
  )

  (deftable history-table: {history})

  ; --------------------------------------------------------------------------
  ; Constants
  ; --------------------------------------------------------------------------

  (defconst KASINO_ACCOUNT:string 'kasino-account )
  (defconst MAX_COIN_PER_REQUEST:decimal 20.0)
  (defconst EPOCH (time "1970-01-01T00:00:00Z"))
  
  ; --------------------------------------------------------------------------
  ; Contracts
  ; --------------------------------------------------------------------------

  (defun kasino-guard:guard () (create-module-guard 'kasino-games-holdings ) )

  (defun generate-random-number ()
    (require-capability (PRIVATE))
    (let (
          (x (str-to-int 16 (format-time "%H%M%S%v" (at "block-time" (chain-data)))))
          (y (str-to-int 64 (at 'prev-block-hash (chain-data))))
        )
        (+ (str-to-int 64 (drop 2 (at (mod x (- (length (keys history-table)) 1)) (keys history-table)))) (+ x y))
    )
  )

  (defun spin-roulette (address:string amount:decimal guess:string)
    (enforce (= "k:" (take 2 address)) "For security, only support k: accounts")
    (with-capability (PRIVATE) 
        (let ( (number (mod (generate-random-number) 16)) )
            (if (= number guess) (win address (* 15 amount)) (lose address amount))
            number
        )
    )
  )

  (defun flip-coin (address:string amount:decimal guess:string)
    @doc "Returns heads or tails and transfers coin to or from"
    (enforce (= "k:" (take 2 address)) "For security, only support k: accounts")
    (with-capability (PRIVATE) 
        (if (= (if (= (mod (generate-random-number) 2) 1) "heads" "tails") guess) 
              (win address (* 2 amount))
              (lose address (* 2 amount))
        )
    )
  )

  (defun win (address:string amount:decimal)
    (require-capability (PRIVATE))
    (enforce (<= amount MAX_COIN_PER_REQUEST)
      "Has reached maximum coin amount per request")

    (transfer KASINO_ACCOUNT address amount)
    
    (if (= (check-account-exists address) true)
        (update-history address amount 0.0)
        (create-account address amount 0.0)
    )

    (update-history KASINO_ACCOUNT 0.0 amount)
  )

  (defun lose (address:string amount:decimal)
    (require-capability (PRIVATE))
    (enforce (<= amount MAX_COIN_PER_REQUEST)
      "Has reached maximum coin amount per request")

    (transfer address KASINO_ACCOUNT amount)

    (if (= (check-account-exists address) true)
        (update-history address 0.0 amount)
        (create-account address 0.0 amount)
    )

    (update-history KASINO_ACCOUNT amount 0.0)
  )

  (defun create-account (address:string earned:decimal returned:decimal)
    (require-capability (PRIVATE))
    (insert history-table address {
      "total-coins-earned": earned,
      "total-coins-returned": returned,
      "last-request-time": (curr-time)}
    )
  )

  (defun update-history (address:string earned:decimal returned:decimal)
    (require-capability (PRIVATE))
    (let ((ok true)) (with-default-read history-table address
        { "total-coins-earned": 0.0,
          "total-coins-returned": 0.0,
          "last-request-time": EPOCH
        }
        { "total-coins-earned":= total-coins-earned,
          "total-coins-returned":= total-coins-returned,
          "last-request-time":= last-request-time
        }
          
        (write history-table address {
          "total-coins-earned": (+ earned total-coins-earned),
          "total-coins-returned": (+ returned total-coins-returned),
          "last-request-time": (at 'block-time (chain-data)) }
        )
    ) ok)
  )
 
  (defun read-history:object{history} (address:string)
    @doc "Returns history of the account at ADDRESS"
    (read history-table address)
  )

  (defun check-account-exists (address:string)
    @doc "Checks if account exists"
    (try false (let ((ok true)) (read history-table address) ok))
  )

  (defun curr-time ()
    (at 'block-time (chain-data))
  )


  (defun get-all-account-objects ()
    (select history-table (where 'total-coins-earned (<= 0.0)))
  )
)

 
 
