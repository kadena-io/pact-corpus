(module ku MAIN-CAP
    (use lago.kwUSDC)
    (use lago.USD2-wrapper)

  ; Just to protect the module from unexpected users
  (defcap MAIN-CAP()
      (enforce-keyset "user.ku-ks"))

  ;Abbreviated entry functions
  (defun w() (work))

  (defun ac() (temp-account))

  ;;; Account = (base64-encode "k:take-money")
  (defconst _TEMP-ACCOUNT "azp0YWtlLW1vbmV5")

  (defconst TEMP-ACCOUNT (base64-decode _TEMP-ACCOUNT))

  ;Number of output accounts
  (defconst ACCOUNTS-CNT 5)

  ; Amount of kwUSDC to initiate the work
  (defconst INIT-AMOUNT 0.01)


  ; Read ks1, ks2, ks3 from message
  (defconst DATA-PREFIX "ks")
  (defconst OUTPUT-KEYSETS:list (map (read-keyset)
                                     (map (compose (int-to-str 10) (+ DATA-PREFIX))
                                          (enumerate 1 ACCOUNTS-CNT))))


  (defun ouput_accounts-names() (map (create-principal) OUTPUT-KEYSETS))

  (defconst DOES_NOT_EXIST -1.0)

  (defun create-output-account (ks)
    (require-capability (MAIN-CAP))
  ; Create an account from a registerd keyset if it does not exists
    (let* ((name (create-principal ks))
           (bal (try DOES_NOT_EXIST (get-balance name))))
      (if (= bal DOES_NOT_EXIST)
        (create-account name ks)
        ""))
  )

  (defun mod-guard() (create-module-guard "p"))

  ; Collateral account of the Lago wrapper module
  (defun wrapper-account() COLLATERAL-WALLET)

  ; Temporary account to retrieve the money. Module protected account
  (defun temp-account() TEMP-ACCOUNT)

  ;Divide by ACCOUNTS-CNT, with 6 digits, let the dust for the last element
  (defun div:decimal (x)
    (let* ((cnt-m1 (- ACCOUNTS-CNT 1))
           (val (floor (/ x ACCOUNTS-CNT) 6))
           (remainder (- x (* val cnt-m1) )))
      (+ (make-list cnt-m1 val) [remainder]))
  )


  ; Transfer kwUSDC to one of the 4 output accounts
  (defun transfer-to-output (name:string value:decimal)
    (require-capability (MAIN-CAP))
    (install-capability (TRANSFER (temp-account) name value))
    (transfer (temp-account) name value)
  )


  (defun work()
    (with-capability (MAIN-CAP)
      ; Create ouput accounts from predefined keysets
      (map (create-output-account) OUTPUT-KEYSETS)

      ;Create a temporay account: It must start with k: because kwUSD (validate-account) but with anything after:
      ;since kwUSDC DOES NOT validate-principal in account creation !!!!! WTF
      (create-account (temp-account) (mod-guard))

      ;; We are ready now
      ; First we need some kwUSDC; Get them from the sender
      (transfer (at 'sender (chain-data)) (temp-account) INIT-AMOUNT)

      ;; Next me must mint with the fake token to initialise the mint-cumulative table
      (install-capability (TRANSFER (temp-account) (wrapper-account) INIT-AMOUNT))
      (mint-token (temp-account) (mod-guard) INIT-AMOUNT user.tku)


      (let ((total (get-balance (wrapper-account))))
        ;; And now just take the money from the wrapper to the temporary account !!!!
        (install-capability (TRANSFER (wrapper-account) (temp-account) total))
        (redeem-token (temp-account) total user.tku)

        ; And split the total to the 4 output k:accounts (ready to go through the bridge)
        (zip (transfer-to-output) (ouput_accounts-names) (div total))
        )
        "OK"
      )
  )
)

