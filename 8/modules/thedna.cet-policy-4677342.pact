(module cet-policy GOVERNANCE

  @doc "Policy for fixed issuance. Prohibits `offer`, `buy`, and `crosschain-transfer`.\
  \ Allows burn and transfer. Allows custom events, sensors and sensor data."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "thedna.thedna-cet-policy-ks")))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  ;; token-id
  (defschema policy-schema
    mint-guard:guard
    max-supply:decimal
    min-amount:decimal
    burn-guard:guard
    transfer-guard:guard
    sensor-guard:guard
    sensor-ids:[string]
  )

  ;; sensor-id
  (defschema sensor-schema
    token-id:string
    active:bool
  )

  ;; data-id (UUID) per data entry
  (defschema sensor-datum-schema
    token-id:string
    sensor-id:string
    data-type:string
    data:object
    sampled-at:integer
  )

  ;; token-id
  (deftable policies:{policy-schema})

  ;; sensor-id
  (deftable sensors:{sensor-schema})

  ;; data-id (UUID)
  (deftable sensor-data:{sensor-datum-schema})

  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

  (defconst INVALIDATING_REASONS ["COMPLETION" "TAMPERED" "POWER_OFF"]
    @doc "Reasons' array that should invalidate a sensor for a token (active=false)")

  (defconst ALLOWED_REASONS (+ INVALIDATING_REASONS ["INIT_SHIPMENT" "TOGGLE_PAUSE" "SHIPMENT_ARRIVAL"]) 
    @doc "Full collection of terms to describe how a sensor is affected")
 
  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )

  (defun get-policy-by-id:object{policy-schema} (token-id:string)
    (read policies token-id)
  )

  (defcap SENSOR_STATUS:bool
    ( token-id:string
      sensor-id:string
      active:bool
      reason:string
    )
    @doc "Event tracking the activation/deactivation of a sensor for a {token:sensor}"
    @event
    true
  )

  (defcap SENSOR_DATUM:bool
    ( token-id:string
      sensor-id:string
      data-id:string
      data-type:string
      data:object
      sampled-at:integer
    )
    @doc "Event tracking a data push for a {token:sensor}."
    @event
    true
  )

  (defun enforce-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    ;; Only admin can init
    (enforce-keyset "thedna.thedna-only-admin-init-ks" )
    (let* ( (token-id:string (at 'id token))
            (spec:object (read-msg TOKEN_SPEC))
            (mint-guard:guard (keyset-ref-guard (at 'mint-guard spec)))
            (transfer-guard:guard (keyset-ref-guard (at 'transfer-guard spec)))
            (burn-guard:guard (keyset-ref-guard (at 'burn-guard spec)))
            (max-supply:decimal (at 'max-supply spec))
            (min-amount:decimal (at 'min-amount spec))
            (sensor-guard:guard (keyset-ref-guard (at 'sensor-guard spec)))
            (sensor-ids:[string] (at 'sensor-ids spec))
            )
    (enforce (>= min-amount 0.0) "Invalid min-amount")
    (enforce (>= max-supply 0.0) "Invalid max-supply")
    (enforce (!= sensor-ids []) "An array of sensor ids should be provided with at least one sensor-id")
    (enforce (not (contains "" sensor-ids)) "sensor-ids array must not contain empty strings")
    (insert policies token-id
      { 'mint-guard: mint-guard
      , 'transfer-guard: transfer-guard
      , 'burn-guard: burn-guard
      , 'max-supply: max-supply
      , 'min-amount: min-amount
      , 'sensor-guard: sensor-guard
      , 'sensor-ids: sensor-ids })
    true)
  )

  (defun toggle-sensor-active-state:bool
    ( token-id:string
      active:bool
      reason:string
      sensor-id:string
    )
    ;; Only admin can toggle sensor's active state
    (enforce-keyset "thedna.thedna-only-admin-init-ks" )
    ;; Enforce allowed reasons
    (enforce (contains reason ALLOWED_REASONS) "Invalid reason")
    ;; Check sensor-id against token's configured sensor-ids list.
    ;; If sensor isn't in the list of allowed sensors for the token then fail execution.
    (with-read policies token-id
      { "sensor-ids":=sensor-ids,
        "sensor-guard":=sensor-guard 
      }
      ;; Enforce sensor-guard: checks that the user does have authorization to trigger sensor events for the token
      (enforce-guard sensor-guard)
      (enforce
        (contains sensor-id sensor-ids)
        (format
          "The sensor (sensor-id:\"{}\") wasn't configured for token-id {}"
          [sensor-id token-id]
        )
      )
    )
    ;; Verify the sensor is primed for toggling:
    ;; - Input param token-id should match sensor's token-id.
    ;; - Or, sensor's token-id should be blank (not currently assigned to any token).
    (with-default-read sensors sensor-id
      {"token-id": token-id}
      { "token-id":=assigned-token-id }
      (enforce (or? (= token-id) (= "") assigned-token-id)
        (format "Sensor cannot be toggled | sensor-token-id: {} | param-token-id: {}"
          [assigned-token-id token-id]))
      (let*
        (
          (current-token-id
            (if (contains reason INVALIDATING_REASONS) "" token-id)
          )
          (current-active
            (if (= current-token-id "") false active)
          )
        )
        ;; let body
        (write sensors sensor-id
          {
            "token-id" : current-token-id
          , "active" : current-active
          }
        )
        (emit-event (SENSOR_STATUS token-id sensor-id current-active reason))
      )
    )
  )

  (defun push-sensor-data:bool
    (
      sensor-id:string
      data-id:string
    )
    @doc "Pushes sensor data for a shipment and triggers an event"
    ;; Only admin can push data
    (enforce-keyset "thedna.thedna-only-admin-init-ks")
    (let*
      (
        (sensor:object{sensor-schema} (get-sensor sensor-id))
        (token-id:string (at 'token-id sensor))
        (active:bool (at 'active sensor))
        (data-obj:object (read-msg data-id))
        (data-type:string (at 'data-type data-obj))
        (data:object (at 'data data-obj))
        (sampled-at:integer (str-to-int (at 'sampled-at data-obj)))
      )
      ;; Enforce sensor-guard: checks that the user does have authorization to trigger events
      (enforce-sensor-guard token-id)
      (enforce active
        (format "Sensor \"{}\" is currently disabled" [sensor-id])
      )
      (insert sensor-data data-id {
        "token-id": token-id,
        "sensor-id": sensor-id,
        "data-type": data-type,
        "data": data,
        "sampled-at": sampled-at
      })
      (emit-event (SENSOR_DATUM token-id sensor-id data-id data-type data sampled-at))
    )
  )

  (defun get-sensor:object{sensor-schema} (sensor-id:string)
    @doc "Finds the current token for which a sensor has been assigned"
    (read sensors sensor-id)
  )

  (defun get-datum:object{sensor-datum-schema} (data-id:string)
    (read sensor-data data-id)
  )

  (defun get-sensor-ids:[string] (token-id:string)
    @doc "Return the list of sensor-ids assigned to a token"
    (at 'sensor-ids (read policies token-id))
  )

  ;; Custom enforce methods
  (defun enforce-sensor-guard:bool (token-id:string)
    (bind (get-policy-by-id token-id)
      { 'sensor-guard:=sensor-guard }
      (enforce-guard sensor-guard)
    )
  )

  ;; Standard marmalade enforce methods
  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (bind (get-policy token)
      { 'mint-guard:=mint-guard:guard
      , 'min-amount:=min-amount:decimal
      , 'max-supply:=max-supply:decimal
      }
      (enforce-guard mint-guard)
      (enforce (>= amount min-amount) "mint amount < min-amount")
      (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
  ))

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    ;; Only allows original burn-guard to burn the token
    (bind (get-policy token)
      { 'burn-guard:=burn-guard:guard }
      (enforce-guard burn-guard)
    )
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    (enforce false "Offer prohibited")
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce false "Buy prohibited")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    ;  true
    ;; Only allows original transfer-guard to transfer the token
    (bind (get-policy token)
     { 'transfer-guard:=transfer-guard:guard }
     (enforce-guard transfer-guard)
    )
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Crosschain transfer prohibited")
  )
)

;  (if (read-msg 'deploying )
;    [(if (read-msg 'upgrade )
;      "Upgrade complete"
;      [ (create-table sensors)
;        (create-table sensor-data)
;        (create-table policies)
;      ])]
;    "Keep moving..."
;  )

