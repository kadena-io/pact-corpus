(module mintit-royalty GOVERNANCE

  ; mintit-royalty 

  (defcap GOVERNANCE ()
    (enforce-guard (read-keyset 'admin-keyset))
  )

  (defconst EXC_INVALID_DESCRIPTION:string "invalid-description")

  (defconst EXC_INVALID_STAKEHOLDER:string "invalid-stakeholder")

  (defconst EXC_INVALID_RATE:string "invalid-rate")

  (defconst EXC_INVALID_PAYOUT:string "invalid-payout")

  (defconst EXC_DUPLICATED_STAKEHOLDER:string "duplicated-stakeholder")
  
  (defconst EXC_DUPLICATED_DESCRIPTION:string "duplicated-description")

  (defconst EXC_INVALID_RATE_SUM:string "invalid-rate-sum")

  (defconst EXC_INVALID_PAYOUT_SUM:string "invalid-payout-sum")

  (defconst EXC_MISMATCHED_STAKEHOLDER_GUARD:string "mismatched-stakeholder-guard")

  ;;; RATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defschema royalty-rate 
    description:string 
    stakeholder:string ; k: account-id
    stakeholder-guard:guard 
    rate:decimal 
  )

  (defschema royalty-rates
    rates:[object{royalty-rate}]
    fungible:module{fungible-v2}
  )

  (defun no-rates:object{royalty-rates}
    ( fungible:module{fungible-v2}
    )
    { 'rates: []
    , 'fungible: fungible 
    } 
  )

  (defun create-rates:object{royalty-rates}
    ( fungible:module{fungible-v2}
      rates:[object{royalty-rate}]
    )
    { 'rates: rates
    , 'fungible: fungible 
    } 
  )

  (defun verify-rates:bool 
    ( o:object{royalty-rates}
    )
    (bind o  
      { 'rates := rates 
      , 'fungible := fungible 
      }
      (map (verify-rate fungible) rates)
      (enforce (unique-field rates 'stakeholder) EXC_DUPLICATED_STAKEHOLDER)
      (enforce (unique-field rates 'description) EXC_DUPLICATED_DESCRIPTION)
      (enforce (>= (sum-field rates 'rate) 0.0) EXC_INVALID_RATE_SUM)
      (enforce (<= (sum-field rates 'rate) 1.0) EXC_INVALID_RATE_SUM)
    )
  )

  (defun verify-rate:bool 
    ( fungible:module{fungible-v2} 
      o:object{royalty-rate}
    )
    (bind o  
      { 'description := description 
      , 'stakeholder := stakeholder  
      , 'stakeholder-guard := stakeholder-guard  
      , 'rate := rate 
      }
      (enforce (!= "" description) EXC_INVALID_DESCRIPTION)
      (enforce (!= "" stakeholder) EXC_INVALID_STAKEHOLDER)
      (enforce (>= rate 0.0) EXC_INVALID_RATE)
      (enforce (<= rate 1.0) EXC_INVALID_RATE)
      (verify-stakeholder-guard fungible stakeholder stakeholder-guard)
    )
  )

  (defun verify-stakeholder-guard:bool 
    ( fungible:module{fungible-v2} 
      stakeholder:string
      stakeholder-guard:guard
    )
    (bind (fungible::details stakeholder)  
      { 'guard := fungible-guard 
      }
      (enforce (= fungible-guard stakeholder-guard) EXC_MISMATCHED_STAKEHOLDER_GUARD)
    )
  )

  ;;; PAYOUTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defschema royalty-payout
    stakeholder:string ; k: account-id
    rate:decimal 
    payout:decimal 
  )

  (defschema royalty-payouts
    payouts:[object{royalty-payout}]
    from:string ; k: account-id 
    fungible:module{fungible-v2}
  )
  
  (defun create-execute-payouts:bool
    ( rates:object{royalty-rates}
      from:string 
      amount:decimal
    )
    (execute-payouts (create-payouts rates from amount))
  )

  (defun create-payouts:object{royalty-payouts}
    ( o:object{royalty-rates}
      from:string 
      amount:decimal
    )
    (bind o 
      { 'rates := rates 
      , 'fungible := fungible:module{fungible-v2}
      }
      { 'payouts: (map (create-payout amount fungible) rates)
      , 'from: from 
      , 'fungible: fungible
      }
    )
  )

  (defun create-payout:object{royalty-payout}  
    ( amount:decimal
      fungible:module{fungible-v2}
      o:object{royalty-rate}
    )
    (bind o 
      { 'stakeholder := to 
      , 'rate := rate 
      }
      { 'stakeholder: to  
      , 'rate: rate 
      , 'payout: (floor (* amount rate) (fungible::precision))
      }
    )
  )

  (defun execute-payouts:bool 
    ( o:object{royalty-payouts}
    )
    (bind o 
      { 'payouts := payouts:[object{royalty-payout}]
      , 'from := from:string
      , 'fungible := fungible:module{fungible-v2}
      }
      (map (execute-payout from fungible) payouts)
      true
    )
  )

  (defun execute-payout:bool
    ( from:string 
      fungible:module{fungible-v2}
      o:object{royalty-payout}
    )
    (bind o 
      { 'stakeholder := to:string 
      , 'rate := rate:decimal 
      , 'payout := payout:decimal 
      }
      (if (> payout 0.0) (fungible::transfer from to payout) "NO ROYALTY")
      true
    )
  )

  (defun total-payout:decimal
    ( o:object{royalty-payouts}
    )
    (sum-field (at 'payouts o) 'payout)
  )  

  ; TODO move this to utils module, or use pact properties and invariants

  (defun unique-field 
    ( xs:[object] 
      k:string 
    )
    (let 
      ( (ys (map (at k) xs)) )
      (= (distinct ys) ys) 
    )
  )

  (defun sum-field:decimal  
    ( xs:[object] 
      k:string 
    )
    (fold (+) 0.0 (map (at k) xs))
  )
)

