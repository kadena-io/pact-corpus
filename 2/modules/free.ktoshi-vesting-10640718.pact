(module ktoshi-vesting GOVERNANCE

  ; This module ha been locked by a 3rd party team for protecting new investors from using it.
  ;   - we believe that it may be a rug-pull (funds transferred to a personal account = red flag)
  ;   - lockable contracts
  ;   - anyway, the contract was unsafe and we found out many issues, and user funds are at risk

  ; The funds are currently on the creator account: k:72b789cd4cf915cc49daf8c31411ef193fda00c44f27c1e9669323cc36bb5392
  ; and we have no control on this account.
  ;
  ; We hope that the founder will return funds back to users.
  ; Contact: ktoshi_scam_protection@pm.me

  (defcap GOVERNANCE ()
    (enforce-keyset "user.ktoshi-scam-protection"))
)
