(module policy-bridge-inbound-instant-mint GOVERNANCE
  (implements n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1)
  (use n_4e470a97222514a8662dd1219000a0431451b0ee.token-policy-ng-v1 [token-info])
  (use n_4e470a97222514a8662dd1219000a0431451b0ee.util-policies)
  (use n_4e470b4e150fafd1c50d1f016331142b55dd01db.bridge-utils)
  (use n_4e470b4e150fafd1c50d1f016331142b55dd01db.bridge)

  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;-----------------------------------------------------------------------------
  ; Schemas and Tables
  ;-----------------------------------------------------------------------------
  (defschema bridge-instant-mint-sch
    guard:guard
    source:object{bridge-target}
    creation-tx:string
  )

  (deftable bridge-data:{bridge-instant-mint-sch})

  ;-----------------------------------------------------------------------------
  ; Capabilities
  ;-----------------------------------------------------------------------------
  (defcap UPDATE-BRIDGE (token-id:string)
    @event
    (with-read bridge-data token-id {'guard:=g}
      (enforce-guard g))
  )

  ;-----------------------------------------------------------------------------
  ; Input data
  ;-----------------------------------------------------------------------------
  (defun read-bridge-inbound-msg:object{bridge-inbound-sch} (token:object{token-info})
    (enforce-get-msg-data "bridge_inbound" token))

  ;-----------------------------------------------------------------------------
  ; Policy hooks
  ;-----------------------------------------------------------------------------
  (defun rank:integer ()
    RANK-HIGH-PRIORITY)

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (n_4e470a97222514a8662dd1219000a0431451b0ee.ledger.POLICY-ENFORCE-INIT token policy-bridge-inbound-instant-mint))
    (bind (read-bridge-inbound-msg token) {'guard:=g, 'source:=src}
      (insert bridge-data (at 'id token) {'guard:g,
                                          'source:src,
                                          'creation-tx:(tx-hash)}))
    true
  )

  (defun --enforce-same-trx:bool (ref-trx:string)
    (enforce (= ref-trx (tx-hash)) "Token must be minted in the create transaction"))

  (defun --enforce-allowed:bool (token-id:string amount:decimal src-target:object{bridge-target})
    (enforce-target-not-null src-target)
    (require-capability (ALLOW-MINT n_4e470a97222514a8662dd1219000a0431451b0ee.ledger token-id amount src-target))
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (bind token {'id:=token-id}
      (with-read bridge-data token-id {'source:=src-target, 'creation-tx:=tx}
        (enforce-one "Not allowed to mint"
                     [(--enforce-same-trx tx), (--enforce-allowed token-id amount src-target)])))
  )

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    true)

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    true)

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    false)

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    true)

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    true)

  (defun enforce-sale-settle:bool (token:object{token-info})
    true)


  (defun get-data:object{bridge-inbound-sch} (token-id:string)
    (with-read bridge-data token-id {'guard:=g, 'source:=src}
      {'guard:g, 'source:src})
  )

  (defun set-source:bool (token-id:string source:object{bridge-target})
    (with-capability (UPDATE-BRIDGE token-id)
      (update bridge-data token-id {'source:source})
      true)
  )

  (defun remove-source:bool (token-id:string)
    (set-source token-id NULL-TARGET)
  )
)

