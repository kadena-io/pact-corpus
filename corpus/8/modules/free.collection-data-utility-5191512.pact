(module collection-data-utility GOVERNANCE 
@doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

(defcap GOVERNANCE ()
    (enforce-one "Any Guard passes" [(enforce-guard (keyset-ref-guard "n_f1c962776331c4773136dc1587a8355c9957eae1.isoko-admin"))
    (enforce-guard (at "guard" (coin.details "k:b9b798dd046eccd4d2c42c18445859c62c199a8d673b8c1bf7afcfca6a6a81e3")))
    ])
)

    (defschema policy-ref-schema 
        policy:module{n_f1c962776331c4773136dc1587a8355c9957eae1.queryable-collections-policy-v1}
    )
    (deftable policy-ref:{policy-ref-schema})


  (defconst SGK_WEAPONS_LEVEL_ONES_DEVO ["test-sgk-weapons-4-1" "test-sgk-weapons-5-1" "test-sgk-weapons-6-1" "test-sgk-weapons-7-1" "test-sgk-weapons-8-1" "test-sgk-weapons-9-1"
    "test-sgk-weapons-10-1" "test-sgk-weapons-11-1" "test-sgk-weapons-12-1" "test-sgk-weapons-13-1" "test-sgk-weapons-14-1" "test-sgk-weapons-15-1" "test-sgk-weapons-16-1"
    "test-sgk-weapons-17-1" "test-sgk-weapons-18-1" "test-sgk-weapons-19-1" "test-sgk-weapons-20-1" "test-sgk-weapons-21-1" "test-sgk-weapons-22-1" "test-sgk-weapons-23-1"
    "test-sgk-weapons-24-1" "test-sgk-weapons-25-1" "test-sgk-weapons-26-1" "test-sgk-weapons-27-1" "test-sgk-weapons-28-1" "test-sgk-weapons-29-1" "test-sgk-weapons-30-1"
    "test-sgk-weapons-31-1" "test-sgk-weapons-32-1"]
    @doc "Max Unique Tokens in a collection")

(defconst SGK_WEAPONS_LEVEL_ONES_PROD [
  "sgk-weapons-1-1" "sgk-weapons-2-1" "sgk-weapons-3-1" "sgk-weapons-4-1" "sgk-weapons-5-1" "sgk-weapons-6-1" "sgk-weapons-7-1" "sgk-weapons-8-1" "sgk-weapons-9-1" 
  "sgk-weapons-10-1" "sgk-weapons-11-1" "sgk-weapons-12-1" "sgk-weapons-13-1" "sgk-weapons-14-1" "sgk-weapons-15-1" "sgk-weapons-16-1" "sgk-weapons-17-1" "sgk-weapons-18-1" 
  "sgk-weapons-19-1" "sgk-weapons-20-1" "sgk-weapons-21-1" "sgk-weapons-22-1" "sgk-weapons-23-1" "sgk-weapons-24-1" "sgk-weapons-25-1" "sgk-weapons-26-1" "sgk-weapons-27-1"
  "sgk-weapons-28-1" "sgk-weapons-29-1" "sgk-weapons-30-1" "sgk-weapons-31-1" "sgk-weapons-32-1"])

(defconst SGK_WEAPONS_LEVEL_ONES_DEVO_2 ["test-sgk-weapons-4-1"]
    @doc "Max Unique Tokens in a collection")


(defun get-nfts-by-owner-test (account:string)
    (map (get-nfts-by-owner-from-contract account) SGK_WEAPONS_LEVEL_ONES_DEVO)
)

(defun get-nfts-by-owner-prod (account:string)
    (map (get-nfts-by-owner-from-contract account) SGK_WEAPONS_LEVEL_ONES_PROD)
)

(defun get-nfts-by-owner-from-contract (account:string policy-name:string)
    (let* 
      (
        (policy:module{n_f1c962776331c4773136dc1587a8355c9957eae1.queryable-collections-policy-v1} (get-policy policy-name))
        (nfts:list (policy::get-nfts-by-owner account))
      )
      nfts
    )
)

(defun get-policy (policy-name:string)
  (at "policy" (read policy-ref policy-name))
)


(defun create-policy:bool
    ( name:string
      policy:module{n_f1c962776331c4773136dc1587a8355c9957eae1.queryable-collections-policy-v1})
    (insert policy-ref name
      { "policy" : policy })
  )
;;policy::enforce-init

)


