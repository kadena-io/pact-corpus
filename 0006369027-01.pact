(module kor-payout GOV
  @doc "This code is meant to be copy-pasted into a different smart contract."

  ;; -------------------------------
  ;; Governance and Permissions

  (defconst GOV_GUARD:string "gov")
  (defconst OPS_GUARD:string "ops")

  (defcap GOV ()
    (enforce-guard (at "guard" (read m-guards GOV_GUARD ["guard"])))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap OPS ()
    (enforce-guard (at "guard" (read m-guards OPS_GUARD ["guard"])))
    (compose-capability (OPS_INTERNAL))
  )

  (defcap OPS_INTERNAL ()
    (compose-capability (INCREMENT))
  )

  (defschema m-guard ;; ID is a const: OPS_GUARD, GOV_GUARD etc.
    @doc "Stores guards for the module"
    guard:guard  
  )
  (deftable m-guards:{m-guard})

  (defun rotate-gov:string (guard:guard)
    @doc "Requires GOV. Changes the gov guard to the provided one."

    (with-capability (GOV)
      (update m-guards GOV_GUARD
        { "guard": guard }  
      )

      "Rotated GOV to a new guard"
    )
  )

  (defun rotate-ops-from-gov (guard:guard)
    @doc "Requires GOV. Changes the ops guard to the provided one."

    (with-capability (GOV)
      (rotate-ops-internal guard)
    )
  )

  (defun rotate-ops:string (guard:guard)
    @doc "Requires OPS. Changes the ops guard to the provided one."

    (with-capability (OPS)
      (rotate-ops-internal guard)
    )
  )

  (defun rotate-ops-internal:string (guard:guard)
    @doc "Requires GOV. Changes the ops guard to the provided one."
    (require-capability (OPS_INTERNAL))

    (update m-guards OPS_GUARD
      { "guard": guard }  
    )

    "Rotated OPS to a new guard"
  )

  (defun get-gov-guard:guard ()
    @doc "Gets the current gov guard and returns it"
    (at "guard" (read m-guards GOV_GUARD))
  )

  (defun get-ops-guard:guard ()
    @doc "Gets the current ops guard and returns it"
    (at "guard" (read m-guards OPS_GUARD))
  )

  (defun init-perms:string (gov:guard ops:guard)
    @doc "Initializes the guards and creates the tables for the module"

    ;; This is only vulnerable if GOV_GUARD doesn't exist
    ;; Which means it's only vulnerable if you don't call 
    ;; init when you deploy the contract.
    ;; So let us be sure that init is called. =)
    (insert m-guards GOV_GUARD
      { "guard": gov }  
    )
    (insert m-guards OPS_GUARD
      { "guard": ops }  
    )
  )

  ;; -------------------------------
  ;; Decimal Values

  (defschema decimal-value
    @doc "Stores decimal values"
    value:decimal
  )
  (deftable decimal-values:{decimal-value})

  (defun update-decimal-value (val-id:string value:decimal)
    @doc "Updates the account for the bank"

    (with-capability (OPS)
      (write decimal-values val-id
        { "value": value }
      )
    )
  )

  (defun get-decimal-value:decimal (val-id:string)
    @doc "Gets the value with the provided id"

    (at "value" (read decimal-values val-id ["value"]))
  )

  ;; -------------------------------
  ;; Counter

  (defcap INCREMENT ()
    @doc "Private capability for incrementing the counter"
    true
  )

  (defschema counter
    @doc "Stores counters"
    counter:integer
  )
  (deftable counters:{counter})

  (defun increment-counter:integer (counter-id:string)
    @doc "Increments the given counter and returns the new count"

    (require-capability (INCREMENT))

    (with-read counters counter-id
      { "counter" := count }
      (let
        (
          (increment (+ count 1))
        )
        (update counters counter-id
          { "counter": increment }  
        )

        increment
      )
    )
  )

  (defun get-counter:integer (counter-id:string)
    (at "counter" (read counters counter-id ["counter"]))
  )

  (defun init-counter:string (counter-id:string)
    @doc "Initializes the guards and creates the tables for the module"
    (with-capability (OPS)
      (insert counters counter-id
        { "counter": 0 }  
      )
    )
  )

  ;; -------------------------------
  ;; Payout Information and Tracking

  (defconst PAYOUT_COUNTER:string "PAYOUT_COUNTER")

  (defschema payout
    @doc "Stores each payout time. Used to know when the last payout was. \
    \ ID is a counter."
    t:time
    total-kda:decimal
  )
  (deftable payouts:{payout})

  (defun add-payout (total-kda:decimal payout-time:time)
    @doc "Adds a payout by incrementing the counter and using block time."
    (require-capability (OPS))

    (insert payouts (int-to-str 10 (increment-counter PAYOUT_COUNTER))
      { "t": payout-time, "total-kda": total-kda }
    )
  )

  (defun get-last-payout:object{payout} ()
    @doc "Gets the last payout using the counter"
    (read payouts (int-to-str 10 (get-counter PAYOUT_COUNTER)))
  )

  (defun init-payout (t:string)
    @doc "Adds a payout by incrementing the counter and using block time."
    (with-capability (OPS)
      (insert payouts "0"
        { "t": (time t), "total-kda": 0.0 }
      )
    )
  )

  ;; -------------------------------
  ;; Sources

  (use free.kor-payout-source-v1 [payout-info])

  (defconst SOURCE_ACTIVE:string "ACTIVE")
  (defconst SOURCE_INACTIVE:string "INACTIVE")

  (defschema payout-source
    @doc "A payout source that implements kor-payout-source-vX, id is the name"
    name:string
    source:module{free.kor-payout-source-v1}
    status:string
  )
  (deftable payout-sources:{payout-source})

  (defun add-payout-source 
    (
      name:string 
      source:module{free.kor-payout-source-v1}
    )
    @doc "Adds a payout source, defaulting it to active."

    (with-capability (OPS)
      (insert payout-sources name
        { "name": name
        , "source": source
        , "status": SOURCE_ACTIVE
        }
      )
    )
  ) 

  (defun get-all-payout-sources ()
    @doc "Gets all payout sources"
    (select payout-sources (constantly true))
  )

  (defun get-active-payout-sources ()
    @doc "Gets all the active payout sources"
    (select payout-sources (where "status" (= SOURCE_ACTIVE)))
  )

  (defun get-active-payout-infos ()
    @doc "Gets all the payout infos from each source and \
    \ returns them as a list."
    (let
      (
        (concat-source 
          (lambda 
            (
              root:[object:{payout-info}] 
              p-source:object{payout-source}
            )
            (let
              (
                (source:module{free.kor-payout-source-v1}
                  (at "source" p-source))
              )
              (+ root (source::get-all-payout-infos))
            )
          )
        )
      )

      (fold (concat-source) [] (get-active-payout-sources))
    )
  )

  (defun set-source-status (name:string new-status:string)
    @doc "Updates the status of the given source. new-status must be ACTIVE or INACTIVE."
    (enforce 
      (or (= new-status SOURCE_ACTIVE) (= new-status SOURCE_INACTIVE)) 
      "Status must be ACTIVE or INACTIVE"
    )

    (with-capability (OPS)
      (update payout-sources name
        { "status": new-status }
      )
    )
  )

  ;; -------------------------------
  ;; Percent Sources

  (use free.kor-percent-payout-source-v1 [percent-payout-info])

  (defschema percent-payout-source
    @doc "A payout source that implements kor-payout-source-vX, id is the name"
    name:string
    source:module{free.kor-percent-payout-source-v1}
    status:string
  )
  (deftable percent-payout-sources:{percent-payout-source})

  (defun add-percent-payout-source 
    (
      name:string 
      source:module{free.kor-percent-payout-source-v1}
    )
    @doc "Adds a percent payout source, defaulting it to active."

    (with-capability (OPS)
      (insert percent-payout-sources name
        { "name": name
        , "source": source
        , "status": SOURCE_ACTIVE
        }
      )
    )
  ) 

  (defun get-all-percent-payout-sources:[object{percent-payout-source}] ()
    @doc "Gets all payout sources"
    (select percent-payout-sources (constantly true))
  )

  (defun get-active-percent-payout-sources:[object{percent-payout-source}] ()
    @doc "Gets all the active percent payout sources"
    (select percent-payout-sources (where "status" (= SOURCE_ACTIVE)))
  )

  (defun get-active-percent-payout-infos:[object{percent-payout-info}] ()
    @doc "Gets all the percent payout infos from each source and \
    \ returns them as a list."
    (let
      (
        (concat-source 
          (lambda 
            (
              root:[object:{payout-info}] 
              source:module{free.kor-percent-payout-source-v1}
            )
            (+ root (source::get-all-percent-payout-infos))
          )
        )
      )

      (fold (concat-source) [] (get-active-percent-payout-sources))
    )
  )

  (defun set-percent-source-status (name:string new-status:string)
    @doc "Updates the status of the given source. new-status must be ACTIVE or INACTIVE."
    (enforce 
      (or (= new-status SOURCE_ACTIVE) (= new-status SOURCE_INACTIVE)) 
      "Status must be ACTIVE or INACTIVE"
    )

    (with-capability (OPS)
      (update percent-payout-sources name
        { "status": new-status }
      )
    )
  )

  ;; -------------------------------
  ;; Payouts

  (defconst MANAGEMENT_ACCOUNT_KEY:string "MANAGEMENT_ACCOUNT_KEY")
  (defconst REMAINING_ACCOUNT_KEY:string "REMAINING_ACCOUNT_KEY")
  (defconst MANAGEMENT_FEE_KEY:string "MANAGED_FEE_KEY")

  (defschema in-payout
    source:module{free.kor-payout-source-v1}  
    payout-infos:[object{payout-info}]
  )

  (defschema in-percent-payout
    source:module{free.kor-percent-payout-source-v1}  
    percent-payout-infos:[object{percent-payout-info}]
  )

  (defun update-payout-times:[object] 
    (
      kda-per-th-per-day:decimal
      total-kda-payout:decimal
      total-percent:decimal
      payout-time:time
      in-payouts:[object{in-payout}]
      in-percent-payouts:[object{in-percent-payout}] 
    )
    @doc "Updates the list of payout infos provided, either percent or normal \
    \ with the payout time given. Returns each account, guard, and payout amount."

    (with-capability (OPS)
      (let 
        (
          (management-fee:decimal (get-decimal-value MANAGEMENT_FEE_KEY))
        )

        ; Calculate all necessary values to send appropriate payouts
        (let
          (
            ; Run hashrate payouts
            (payout-info-list
              (map 
                (handle-th-update 
                  kda-per-th-per-day 
                  management-fee
                  total-percent
                  payout-time) 
                in-payouts
              )
            )
            ; Run percent payouts
            (percent-payout-info-list
              (map 
                (handle-percent-update total-kda-payout payout-time) 
                in-percent-payouts
              )
            )
          )

          (add-payout total-kda-payout payout-time)

          ; Return useful information
          { "payout-info-list": payout-info-list
          , "percent-payout-info-list": percent-payout-info-list
          }
        )
      )
    )
  )

  (defun handle-th-update:object
    (
      kda-per-th-per-day:decimal
      management-fee:decimal
      total-percent:decimal
      payout-time:time
      in:object{in-payout}
    )
    @doc "Private function used to update payout time for a source and get the amount to pay it"
    (require-capability (OPS_INTERNAL))

    (let*
      (
        (source:module{free.kor-payout-source-v1} (at "source" in))
        (pay 
          (lambda (info:object{payout-info})
            (let
              (
                (total-amount:decimal (get-payout-for-info kda-per-th-per-day info payout-time)) 
              )
        
              ; Update the payout time
              (source::update-payout-time 
                (at "nft-id" info)
                payout-time
              )
              
              { "hashrate": (at "hashrate" info)
              , "account": (at "account" info)
              , "guard": (at "guard" info)
              , "amount": (round (* total-amount (- 1.0 management-fee)) (coin.precision))
              , "management-amount": (round (* total-amount (- management-fee total-percent)) (coin.precision))
              }
            )
          )
        )
      )
      (map (pay) (at "payout-infos" in))
    )
  )

  (defun handle-percent-update:[object]
    (
      total-kda:decimal
      payout-time:time
      in:object{in-percent-payout}
    )
    @doc "Pays each account from the percent source \
    \ based on percentage ownership. Percent is split between all \
    \ accounts in the source."

    (require-capability (OPS_INTERNAL))

    (let*
      (
        (source:module{free.kor-percent-payout-source-v1} (at "source" in))
        (percent (source::get-percent-payout))
        (percent-payout-infos (at "percent-payout-infos" in))
        (kda-per-account:decimal 
          (/ 
            (* total-kda percent) 
            (length percent-payout-infos)
          )
        )
        (pay 
          (lambda (info:object{percent-payout-info})
            ; Update the payout time
            (source::update-percent-payout-time 
              (at "nft-id" info)
              payout-time
            )
            
            { "percent": percent
            , "account": (at "account" info)
            , "guard": (at "guard" info)
            , "amount": (round kda-per-account (coin.precision))
            }
          )
        )
      )
      (map (pay) percent-payout-infos)
    )
  )

  (defun get-payout-for-info:decimal 
    (
      kda-per-th-per-day:decimal 
      info:object{payout-info}
      payout-time:time
    )
    @doc "Gets the amount to pay based on the info provided."
    (* 
      kda-per-th-per-day 
      (* 
        (at "hashrate" info) 
        (get-days-between-times (at "last-payout" info) payout-time)
      )
    )
  )

  (defun get-total-active-hashrate ()
    @doc "Sums the total hashrate from all sources"
    (let
      (
        (sum 
          (lambda (prev:decimal payout-value:object{payout-info})
            (+ prev (at "hashrate" payout-value))
          )
        )
      )
      (fold (sum) 0.0 (get-active-payout-infos))
    )
  )

  (defun get-total-to-pay (kda-per-th-per-day:decimal payout-time:time)
    @doc "Gets the total that will have to be paid based on kda per th"
    (let
      (
        (sum 
          (lambda (prev:decimal info:object{payout-info})
            (+ 
              prev 
              (get-payout-for-info kda-per-th-per-day info payout-time)
            )
          )
        )
      )
      (fold (sum) 0.0 (get-active-payout-infos))
    )
  )

  (defun get-total-percent:decimal ()
    @doc "Gets the total percent taken from manager to be split among sources"
    (let
      (
        (sum 
          (lambda (prev:decimal p-source:object{percent-payout-source})
            (let
              (
                (source:module{free.kor-percent-payout-source-v1} 
                  (at "source" p-source))  
              )
              (+ 
                prev 
                (source::get-percent-payout)
              )
            )
          )
        )
      )
      (fold (sum) 0.0 (get-active-percent-payout-sources))
    )
  )

  (defun get-all-in-payouts:[object{in-payout}] ()
    @doc "Takes each payout source and turns it into an object that can be \
    \ passed to run-payout"
    (let
      (
        (construct 
          (lambda (p-source:object{payout-source})
            (let
              (
                (source:module{free.kor-payout-source-v1} 
                  (at "source" p-source))  
              )
              { "source": source
              , "payout-infos": (source::get-all-payout-infos)
              }
            )
          )
        )
      )
      (map (construct) (get-active-payout-sources))
    )
  )

  (defun get-all-in-percent-payouts:[object{in-percent-payout}] ()
    @doc "Takes each percent payout source and turns it into an object that can be \
    \ passed to run-payout"
    (let
      (
        (construct 
          (lambda (p-source:object{percent-payout-source})
            (let
              (
                (source:module{free.kor-percent-payout-source-v1} 
                  (at "source" p-source))  
              )
              { "source": source
              , "percent-payout-infos": (source::get-all-percent-payout-infos)
              }
            )
          )
        )
      )
      (map (construct) (get-active-percent-payout-sources))
    )       
  )

  ;; -------------------------------
  ;; Util

  (defun get-days-between-times:decimal (t1:time t2:time)  
    @doc "Gets the number of days in decimal since the last pay. \
    \ Returns 0 if t1 is greater than t2."
    (if (>= t1 t2)
      0.0
      (/ 
        (diff-time t2 t1) 
        86400.0 
      )
    )
  )

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"

    (at "block-time" (chain-data))
  )
)


