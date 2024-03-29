(module util-fungible GOV
  "This module provides some helpers to create fungible tokens \
   \ Documentation: https://pact-util-lib.readthedocs.io \
   \ Github: https://github.com/CryptoPascal31/pact-util-lib "

  (defconst VERSION:string "0.7")

  (defcap GOV()
    (enforce-keyset "free.util-lib"))

  (use util-chain-data [chain-id])

  (defconst STD_CHARSET:integer CHARSET_LATIN1)

  (defconst STD_MINIMUM_ACCOUNT_LENGTH:integer 3)

  (defconst STD_MAXIMUM_ACCOUNT_LENGTH:integer 256)

  (defconst VALID_CHAIN_IDS:[string] (map (int-to-str 10) (enumerate 0 19)))

  (defun enforce-precision:bool (precision:integer amount:decimal)
    "Validate the precision (number of decimals) of an amount"
    (enforce (= (floor amount precision) amount)
             (format "Amount {} violates the required precision {}"
                     [amount precision]))
  )

  (defun enforce-valid-amount:bool (precision:integer amount:decimal)
    "Validate that an amount is positive and does not viloate the precision \
   \ Must be used to handle every amount in a fungible module"
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce-precision precision amount)
  )

  (defun enforce-valid-account:bool (account:string)
    "Enforce that an account name conforms to minimum and maximum length \
   \ requirements, as well as the latin-1 character set."

    (enforce (is-charset STD_CHARSET account)
             "Account does not conform to the charset LATIN1")

    (enforce (and? (<= STD_MINIMUM_ACCOUNT_LENGTH) (>= STD_MAXIMUM_ACCOUNT_LENGTH)
                   (length account))
             (format "Account name does not conform to the length rquirements [{}-{}]"
                     [STD_MINIMUM_ACCOUNT_LENGTH, STD_MAXIMUM_ACCOUNT_LENGTH]))
  )

  (defun enforce-valid-transfer:bool (sender:string receiver:string precision:integer
                                      amount:decimal)
      "Validate that the sender, the receiver and the amount are valid for a transfer"
      (enforce (!= sender receiver)
               (format "Sender and Receiver must be different: {}" [sender]))
      (enforce-valid-amount precision amount)
      (enforce-valid-account sender)
      (enforce-valid-account receiver)
  )

  (defun enforce-valid-transfer-xchain:bool (sender:string receiver:string precision:integer
                                             amount:decimal)
      "Validate that the sender, the receiver and the amount are valid for an X-chain transfer"
      (enforce-valid-amount precision amount)
      (enforce-valid-account sender)
      (enforce-valid-account receiver)
  )

  (defun enforce-reserved:bool (account:string guard:guard)
    "Enforce that a principal account matches to it's guard"
    (if (is-principal account)
        (enforce (validate-principal guard account)
                 (format "Reserved protocol guard violation: {}" [(typeof-principal account)]))
        true)
  )

  (defun enforce-reserved*:bool (account:string guard:guard)
    "Starred version for enforce-reserved. Enforce that a principal account \
   \ matches to it's guard and refuse non principal accounts"
    (enforce (is-principal account) "Only principal accounts can be used")
    (enforce (validate-principal guard account)
             (format "Reserved protocol guard violation: {}" [(typeof-principal account)]))
  )

  (defun enforce-valid-chain-id:bool (target-chain-id:string)
    "Enforce that chain-id is a valid chain identifier"
    (enforce (contains target-chain-id VALID_CHAIN_IDS) "Target chain is not a valid Chainweb chainID"))

  (defun enforce-not-same-chain:bool (target-chain-id:string)
    "Enforce that chain-id is not same as the current chain"
    (enforce (!= (chain-id) target-chain-id)
             (format "Target chain {} cannot be the current chain {}" [target-chain-id, (chain-id)]))
  )

  (defschema fungible-xchain-sch
    "Schema for yielded value in cross-chain transfers"
    receiver:string
    receiver-guard:guard
    amount:decimal
    source-chain:string
  )

)

