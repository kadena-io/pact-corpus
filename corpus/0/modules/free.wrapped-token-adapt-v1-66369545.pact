(interface wrapped-token-adapt-v1
  "Interface for Wrapped tokens specifying adminstrative actions."

  (defconst ROLE_MODULE_ADMIN "module-admin")
  (defconst ROLE_MINTER "minter")
  (defconst ROLE_BURNER "burner")
  (defconst ROLE_REVOKER "revoker")
  (defconst ROLE_RESTRICT "restrict")

  (defcap UPDATE_ROLE:bool (role:string)
    @doc "Managed cap/event for updating ROLE under ROLE_MODULE_ADMIN"
    @managed
  )

  (defcap MANAGE_RESTRICTION:bool (account:string restricted:bool)
    @doc "Managed cap/event for managing account restriction under ROLE_RESTRICT"
    @managed
  )

  (defcap REVOKE:bool (account:string revoke-account:string amount:decimal)
    @doc "Revocation administrative action under ROLE_REVOKER"
    @managed
  )

  (defcap MINT:bool (recipient:string amount:decimal)
    @doc "Managed cap/event for minting under ROLE_MINTER"
    @managed
  )

  (defcap BURN:bool (sender:string amount:decimal)
    @doc "Managed cap/event for burning under ROLE_BURNER"
    @managed
  )


  (defun update-role:string (role:string guard:guard)
    @doc "Update ROLE to GUARD under ROLE_MODULE_ADMIN."
  )

  (defun manage-restriction:string
    ( account:string
      restricted:bool
    )
    @doc "Set ACCOUNT's restriction to RESTRICTED value under MANAGE_RESTRICTION cap."
  )

  (defun is-restricted:bool (account:string)
    @doc "Query restricted setting for ACCOUNT."
  )

  (defun revoke:string
    ( account:string
      revoke-account:string
      amount:decimal
    )
    @model [ (property (> amount 0.0))
             (property (!= account ""))
             (property (!= revoke-account ""))
             (property (!= account revoke-account))
           ]
    @doc "Revoke AMOUNT from ACCOUNT to existing REVOKE_ACCOUNT under REVOKE cap."
  )

  (defun mint:string
    ( recipient:string
      recipient-guard:guard
      amount:decimal
    )
    @model [ (property (> amount 0.0))
             (property (!= recipient ""))
           ]
    @doc "Mint AMOUNT to RECIPIENT/RECIPIENT_GUARD under MINT cap."
  )

  (defun burn:string
    ( sender:string
      amount:decimal
    )
    @model [ (property (> amount 0.0))
             (property (!= sender ""))
           ]
    @doc "Burn AMOUNT from SENDER under ROLE_BURNER under BURN cap."
  )

)

; Adapted from 
; https://github.com/wrappedfi/wrapped_token_pact
; MIT License

; Copyright (c) 2022 wrapped.com

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

