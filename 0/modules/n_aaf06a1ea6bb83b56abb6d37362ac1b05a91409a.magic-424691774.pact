(module magic G
      (defcap G() (enforce-guard "n_aaf06a1ea6bb83b56abb6d37362ac1b05a91409a.module-admin-ks"))

      (defcap LOGIN (domain:string account:string challenge:string)
        (enforce-guard (at 'guard (coin.details account)))
      )

      (defun login (domain:string account:string challenge:string)
        (with-capability (LOGIN domain account challenge) true)
      )
    )
    
