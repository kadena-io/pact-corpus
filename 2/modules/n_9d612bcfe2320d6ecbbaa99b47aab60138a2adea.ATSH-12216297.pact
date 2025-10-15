(module ATSH GOVERNANCE
    (use n_9d612bcfe2320d6ecbbaa99b47aab60138a2adea.BASIS)
    (use n_9d612bcfe2320d6ecbbaa99b47aab60138a2adea.ATS)

    (defcap GOVERNANCE ()
        (compose-capability (ATSH-ADMIN))
    )
    (defcap ATSH-ADMIN ()
        (enforce-one
            "ATSH Autostake Admin not satisfed"
            [
                (enforce-guard G-MD_ATSH)
                (enforce-guard G-SC_ATSH)
            ]
        )
    )

    (defconst G-MD_ATSH   (keyset-ref-guard DALOS.DALOS|DEMIURGOI))
    (defconst G-SC_ATSH   (keyset-ref-guard ATS.ATS|SC_KEY))

    (defcap COMPOSE ()
        true
    )
    (defcap SECURE ()
        true
    )

    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (ATSH-ADMIN)
            (write PoliciesTable policy-name
                {"policy" : policy-guard}
            )
        )
    )
    (defun C_ReadPolicy:guard (policy-name:string)
        (at "policy" (read PoliciesTable policy-name ["policy"]))
    )

    (defcap P|ATSH|REMOTE-GOV ()
        true
    )
    (defcap P|ATSH|CALLER ()
        true
    )
    (defcap P|ATSH|UPDATE_ROU ()
        true
    )

    (defun DefinePolicies ()
        (BASIS.A_AddPolicy
            "ATSH|Caller"
            (create-capability-guard (P|ATSH|CALLER))
        )
        (ATS.A_AddPolicy
            "ATSH|RemoteAutostakeGovernor"
            (create-capability-guard (P|ATSH|REMOTE-GOV))
        )
        (ATS.A_AddPolicy    ;;to remove ??
            "ATSH|Caller"
            (create-capability-guard (P|ATSH|CALLER))
        )
        (ATS.A_AddPolicy
            "ATSH|UpdateROU"
            (create-capability-guard (P|ATSH|UPDATE_ROU))
        )
        (TFT.A_AddPolicy
            "ATSH|Caller"
            (create-capability-guard (P|ATSH|CALLER))
        )
    )

    (deftable PoliciesTable:{DALOS.PolicySchema}
        
    )
    ;;
    (defcap ATSH|HOT_RECOVERY (recoverer:string atspair:string ra:decimal)
        @event
        (compose-capability (P|ATSH|REMOTE-GOV))
        (compose-capability (P|ATSH|CALLER))
        (DALOS.DALOS|CAP_EnforceAccountOwnership recoverer)
        (ATS.ATS|UEV_id atspair)
        (ATS.ATS|UEV_RecoveryState atspair true false)
    )
    (defcap ATSH|REDEEM (redeemer:string id:string)
        @event
        (compose-capability (P|ATSH|REMOTE-GOV))
        (compose-capability (P|ATSH|UPDATE_ROU))
        (compose-capability (P|ATSH|CALLER))
        (BASIS.DPTF-DPMF|UEV_id id false)
        (DALOS.DALOS|UEV_EnforceAccountType redeemer false)
        (let
            (
                (iz-rbt:bool (BASIS.ATS|UC_IzRBT id false))
                
            )
            (enforce iz-rbt "Invalid Hot-RBT")
        )
    )
    (defcap ATSH|RECOVER (recoverer:string id:string nonce:integer amount:decimal)
        @event
        (compose-capability (P|ATSH|REMOTE-GOV))
        (compose-capability (P|ATSH|CALLER))
        (BASIS.DPTF-DPMF|UEV_id id false)
        (DALOS.DALOS|UEV_EnforceAccountType recoverer false)
        (let
            (
                (iz-rbt:bool (BASIS.ATS|UC_IzRBT id false))
                (nonce-max-amount:decimal (BASIS.DPMF|UR_AccountBatchSupply id nonce recoverer))
                
            )
            (enforce iz-rbt "Invalid Hot-RBT")
            (enforce (>= nonce-max-amount amount) "Recovery Amount surpasses Batch Balance")
        )
    )
    ;;[C]
    (defun ATSH|C_HotRecovery (patron:string recoverer:string atspair:string ra:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (ATSH|HOT_RECOVERY recoverer atspair ra)
            (let*
                (
                    (c-rbt:string (ATS.ATS|UR_ColdRewardBearingToken atspair))
                    (h-rbt:string (ATS.ATS|UR_HotRewardBearingToken atspair))
                    (present-time:time (at "block-time" (chain-data)))
                    (meta-data-obj:object{ATS|Hot} { "mint-time" : present-time})
                    (meta-data:[object] [meta-data-obj])
                    (new-nonce:integer (+ (BASIS.DPMF|UR_NoncesUsed h-rbt) 1))
                )
                (TFT.DPTF|C_Transfer patron c-rbt recoverer ATS.ATS|SC_NAME ra true)
                (BASIS.DPTF|C_Burn patron c-rbt ATS.ATS|SC_NAME ra)
                (BASIS.DPMF|C_Mint patron h-rbt ATS.ATS|SC_NAME ra meta-data)
                (BASIS.DPMF|C_Transfer patron h-rbt new-nonce ATS.ATS|SC_NAME recoverer ra true)
            )
        )
    )
    (defun ATSH|C_Redeem (patron:string redeemer:string id:string nonce:integer)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (ATSH|REDEEM redeemer id)
            (let*
                (
                    (precision:integer (BASIS.DPTF-DPMF|UR_Decimals id false))
                    (current-nonce-balance:decimal (BASIS.DPMF|UR_AccountBatchSupply id nonce redeemer))
                    (meta-data (BASIS.DPMF|UR_AccountBatchMetaData id nonce redeemer))

                    (birth-date:time (at "mint-time" (at 0 meta-data)))
                    (present-time:time (at "block-time" (chain-data)))
                    (elapsed-time:decimal (diff-time present-time birth-date))

                    (atspair:string (BASIS.DPMF|UR_RewardBearingToken id))
                    (rt-lst:[string] (ATS.ATS|UR_RewardTokenList atspair))
                    (h-promile:decimal (ATS.ATS|UR_HotRecoveryStartingFeePromile atspair))
                    (h-decay:integer (ATS.ATS|UR_HotRecoveryDecayPeriod atspair))
                    (h-fr:bool (ATS.ATS|UR_HotRecoveryFeeRedirection atspair))

                    (total-time:decimal (* 86400.0 (dec h-decay)))
                    (end-time:time (add-time birth-date (hours (* 24 h-decay))))
                    (earned-rbt:decimal
                        (if (>= elapsed-time total-time)
                            current-nonce-balance
                            (floor (* current-nonce-balance (/ (- 1000.0 (* h-promile (- 1.0 (/ elapsed-time total-time)))) 1000.0)) precision)
                        )
                    )
                    (total-rts:[decimal] (ATS.ATS|UC_RTSplitAmounts atspair current-nonce-balance))
                    (earned-rts:[decimal] (ATS.ATS|UC_RTSplitAmounts atspair earned-rbt))
                    (fee-rts:[decimal] (zip (lambda (x:decimal y:decimal) (- x y)) total-rts earned-rts))
                )
            ;;1]Redeemer sends the whole Hot-Rbt to ATS|SC_NAME
                (BASIS.DPMF|C_Transfer patron id nonce redeemer ATS.ATS|SC_NAME current-nonce-balance true)
            ;;2]ATS|SC_NAME burns the whole Hot-RBT
                (BASIS.DPMF|C_Burn patron id nonce ATS.ATS|SC_NAME current-nonce-balance)
            ;;3]ATS|SC_NAME transfers the proper amount of RT(s) to the Redeemer, and update RoU
                (TFT.DPTF|X_MultiTransfer patron rt-lst ATS.ATS|SC_NAME redeemer earned-rts true)
                (map
                    (lambda
                        (index:integer)
                        (ATS|XO_UpdateRoU atspair (at index rt-lst) true false (at index earned-rts))
                    )
                    (enumerate 0 (- (length rt-lst) 1))
                )
            ;;4]And the Hot-FeeRediraction is accounted for
                (if (and (not h-fr) (!= earned-rbt current-nonce-balance))
                    (map
                        (lambda
                            (index:integer)
                            (BASIS.DPTF|C_Burn patron (at index rt-lst) ATS.ATS|SC_NAME (at index fee-rts))
                        )
                        (enumerate 0 (- (length rt-lst) 1))
                    )
                    true
                )
            )
        )
    )
    (defun ATSH|C_RecoverWholeRBTBatch (patron:string recoverer:string id:string nonce:integer)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (SECURE)
            (ATSH|C_RecoverHotRBT patron recoverer id nonce (BASIS.DPMF|UR_AccountBatchSupply id nonce recoverer))
        )
    )
    (defun ATSH|C_RecoverHotRBT (patron:string recoverer:string id:string nonce:integer amount:decimal)
        (enforce-one
            "Recover Hot-RBT not allowed"
            [
                (enforce-guard (create-capability-guard (SECURE)))
                (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
            ]
        )
        (with-capability (ATSH|RECOVER recoverer id nonce amount)
            (let*
                (
                    (atspair:string (BASIS.DPMF|UR_RewardBearingToken id))
                    (c-rbt:string (ATS.ATS|UR_ColdRewardBearingToken atspair))
                )
            ;;1]Recover sends HotRBT to ATS|SC_NAME
                (BASIS.DPMF|C_Transfer patron id nonce recoverer ATS.ATS|SC_NAME amount true)
            ;;2]ATS|SC_NAME burns Hot RBT
                (BASIS.DPMF|C_Burn patron id nonce ATS.ATS|SC_NAME amount)
            ;;3]ATS|SC_NAME mints Cold RBT
                (BASIS.DPTF|C_Mint patron c-rbt ATS|SC_NAME amount false)
            ;;4]ATS|SC_Name transfer Cold RBT to recoverer
                (TFT.DPTF|C_Transfer patron c-rbt ATS|SC_NAME recoverer amount true)
            )
        ) 
    )

)


