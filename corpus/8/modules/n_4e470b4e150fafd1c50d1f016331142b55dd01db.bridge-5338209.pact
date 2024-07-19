(module bridge GOVERNANCE
  (use bridge-utils)
  (use free.util-strings [to-string])
  (use free.util-chain-data)
  (use free.util-fungible [enforce-valid-chain-id enforce-not-same-chain])

  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))

  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Capabilties (API exposed to policies)
  ;-----------------------------------------------------------------------------
  (defcap ALLOW-BURN (ledger:module{n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1}
                      token-id:string dest:object{bridge-target})
    true)

  (defcap ALLOW-BURN-V1 (ledger:module{kip.poly-fungible-v2}
                         token-id:string dest:object{bridge-target})
    true)

  (defcap ALLOW-BURN-GENERIC (ledger:module{generic-burnable-nft-v1-beta1}
                              token-id:string dest:object{bridge-target})
    true)

  (defcap ALLOW-MINT (ledger:module{n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1}
                      token-id:string amount:decimal
                      source:object{bridge-target})
    true)

  ;-----------------------------------------------------------------------------
  ; Util functions
  ;-----------------------------------------------------------------------------
  (defun enforce-balance:bool (balance:decimal required:decimal)
    @doc "Enforce that required is lower than current balance, or throw an error"
    (enforce (>= balance required) "Insuficient balance in the source ledger"))

  (defun make-target:object{bridge-target} (ledger token-id:string)
    @doc "Build a *same-chain* target from a ledger modref and a token-id"
    {'ledger: (to-string ledger), 'token:token-id, 'chain:""})

  (defun make-target-x:object{bridge-target} (ledger token-id:string chain:string)
    @doc "Build a *X-chain* target from a ledger modref and a token-id"
    {'ledger: (to-string ledger), 'token:token-id, 'chain:chain})

  ;-----------------------------------------------------------------------------
  ; Bridge NG -> NG
  ;-----------------------------------------------------------------------------
  (defun bridge-ng-to-ng:bool (src-ledger:module{n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1}
                               dst-ledger:module{n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1}
                               token-id:string account:string amount:decimal)

    (enforce (!= src-ledger dst-ledger) "Source and dest ledgers must be different")
    ; We assume here that token-id is common
    ; Recover the amount and the guard from the ledger
    (bind (src-ledger::details token-id account) {'guard:=guard, 'balance:=src-balance}
      ; Sanity check => Verify that the current token balance can fullfil the bridging request
      (enforce-balance src-balance amount)

      ; Burn the token
      (with-capability (ALLOW-BURN src-ledger token-id (make-target dst-ledger token-id))
        (with-capability (ALLOW-BURN src-ledger token-id (make-target dst-ledger ""))
          (src-ledger::burn token-id account amount)))

      ; Mint the token
      (with-capability (ALLOW-MINT dst-ledger token-id amount (make-target src-ledger token-id))
        (dst-ledger::mint token-id account guard amount)))
  )

  ;-----------------------------------------------------------------------------
  ; Bridge V1 -> NG
  ;-----------------------------------------------------------------------------
  (defun bridge-v1-to-ng:bool (src-ledger:module{kip.poly-fungible-v2}
                               src-helper:module{marmalade-burn-helper-v1-beta1}
                               dst-ledger:module{n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1}
                               src-token-id:string dst-token-id:string
                               account:string amount:decimal)

    (bind (src-ledger::details src-token-id account) {'guard:=guard, 'balance:=src-balance}
      ; Sanity check => Verify that the current token balance can fullfil the bridging request
      (enforce-balance src-balance amount)

      ; Burn the token
      (with-capability (ALLOW-BURN-V1 src-ledger src-token-id (make-target dst-ledger dst-token-id))
        (with-capability (ALLOW-BURN-V1 src-ledger src-token-id (make-target dst-ledger ""))
          (src-helper::burn src-token-id account amount)))

      ; Mint the token
      (with-capability (ALLOW-MINT dst-ledger dst-token-id amount (make-target src-ledger src-token-id))
        (dst-ledger::mint dst-token-id account guard amount)))
  )

  ;-----------------------------------------------------------------------------
  ; Bridge NG Loopback
  ;-----------------------------------------------------------------------------
  (defun bridge-ng-loop:bool (ledger:module{n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1}
                              src-token-id:string dst-token-id:string
                              account:string amount:decimal)
    (enforce (!= src-token-id dst-token-id) "Source and destiantion tokens must be different")

    (bind (ledger::details src-token-id account) {'guard:=guard, 'balance:=src-balance}
      ; Sanity check => Verify that the current token balance can fullfil the bridging request
      (enforce-balance src-balance amount)

      ; Burn the token
      (with-capability (ALLOW-BURN ledger src-token-id (make-target ledger dst-token-id))
        (ledger::burn src-token-id account amount))

      ; Mint the token
      (with-capability (ALLOW-MINT ledger dst-token-id amount (make-target ledger src-token-id))
        (ledger::mint dst-token-id account guard amount)))
  )

  ;-----------------------------------------------------------------------------
  ; Bridge Generic -> NG
  ;-----------------------------------------------------------------------------
  (defun bridge-generic-to-ng:bool (src-ledger:module{generic-burnable-nft-v1-beta1}
                                    dst-ledger:module{n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1}
                                    src-token-id:string dst-token-id:string)

    (bind (src-ledger::owner-details src-token-id) {'owner:=owner, 'guard:=guard}
      ; Burn the token
      (with-capability (ALLOW-BURN-GENERIC src-ledger src-token-id (make-target dst-ledger dst-token-id))
        (with-capability (ALLOW-BURN-GENERIC src-ledger src-token-id (make-target dst-ledger ""))
          (src-ledger::burn src-token-id)))

      ; Mint the token
      ; Since we only support NON fungible tokens in this mode, we hardcode the amount to 1.0
      (with-capability (ALLOW-MINT dst-ledger dst-token-id 1.0 (make-target src-ledger src-token-id))
        (dst-ledger::mint dst-token-id owner guard 1.0)))
  )

  ;-----------------------------------------------------------------------------
  ; Bridge NG -> NG (X-chain)
  ;-----------------------------------------------------------------------------
  (defschema x-chain-sch
    ledger:module{n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1}
    source-chain:string
    token-id:string
    account:string
    guard:guard
    amount:decimal
  )

  (defpact bridge-ng-xchain (ledger:module{n_4e47017ea6449649e13d79906dd1697fb1fe1d7a.ng-poly-fungible-v1} dst-chain:string
                             token-id:string account:string amount:decimal)
    (step
      (bind (ledger::details token-id account) {'guard:=src-guard, 'balance:=src-balance}
        ; Sanity check => Verify that the current token balance can fullfil the bridging request
        (enforce-balance src-balance amount)
        ; Verify the chain dst chain id
        (enforce-valid-chain-id dst-chain)
        (enforce-not-same-chain dst-chain)

        ; Burn the token
        (with-capability (ALLOW-BURN ledger token-id (make-target-x ledger token-id dst-chain))
          (with-capability (ALLOW-BURN ledger token-id (make-target-x ledger "" dst-chain))
            (ledger::burn token-id account amount)))

        ; Yield the object for the opposite chain
        (let ((xchain-obj:object{x-chain-sch} {'ledger:ledger,
                                               'source-chain:(chain-id),
                                               'token-id:token-id,
                                               'account:account,
                                               'guard:src-guard,
                                               'amount:amount}))
          (yield xchain-obj dst-chain)))
    )

    ; ---------------------
    ; Defpact continutation
    (step
      ; Recover the object yielded from the source chain
      (resume {'ledger:=ledger,
               'source-chain:=src-chain,
               'token-id:=token-id,
               'account:=account,
               'guard:=dst-guard,
               'amount:=amount}
        ; Mint the token
        (with-capability (ALLOW-MINT ledger token-id amount (make-target-x ledger token-id src-chain))
          (ledger::mint token-id account dst-guard amount)))
    )
  )
)

