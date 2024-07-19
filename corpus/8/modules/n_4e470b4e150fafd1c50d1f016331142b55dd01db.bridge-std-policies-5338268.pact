(module bridge-std-policies GOVERNANCE

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Helper function
  ;-----------------------------------------------------------------------------
  (defun bridging-policies:[module{n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1}] (type:string)
    (cond
      ((= type "") [])
      ((= type "OUTBOUND") [policy-bridge-outbound])
      ((= type "INBOUND-NO-MINT") [policy-bridge-inbound])
      ((= type "INBOUND-INSTANT-MINT") [policy-bridge-inbound-instant-mint])
      ((= type "INBOUND-GUARD-MINT") [policy-bridge-inbound-guard-mint])
      ((= type "BIDIR-NO-MINT") [policy-bridge-outbound policy-bridge-inbound])
      ((= type "BIDIR-INSTANT-MINT") [policy-bridge-outbound policy-bridge-inbound-instant-mint])
      ((= type "BIDIR-GUARD-MINT") [policy-bridge-outbound policy-bridge-inbound-guard-mint])
      [(enforce false "Unrecognized bridge type")])
  )

  (defun from-bridging-policies:string (policies:[module{n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1}])
    (let ((cp (lambda (x) (contains x policies))))
      (if (cp policy-bridge-outbound)
        (cond
          ((cp policy-bridge-inbound) "BIDIR-NO-MINT")
          ((cp policy-bridge-inbound-instant-mint) "BIDIR-INSTANT-MINT")
          ((cp policy-bridge-inbound-guard-mint) "BIDIR-GUARD-MINT")
          "OUTBOUND")
        (cond
          ((cp policy-bridge-inbound) "INBOUND-NO-MINT")
          ((cp policy-bridge-inbound-instant-mint) "INBOUND-INSTANT-MINT")
          ((cp policy-bridge-inbound-guard-mint) "INBOUND-GUARD-MINT")
          "")
      ))
  )
)

