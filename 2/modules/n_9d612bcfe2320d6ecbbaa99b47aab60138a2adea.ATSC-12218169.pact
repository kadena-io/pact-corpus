(module ATSC GOVERNANCE
    (defcap GOVERNANCE ()
        (compose-capability (ATSC-ADMIN))
    )
    (defcap ATSC-ADMIN ()
        (enforce-one
            "ATSC Autostake Admin not satisfed"
            [
                (enforce-guard G-MD_ATSC)
                (enforce-guard G-SC_ATSC)
            ]
        )
    )

    (defconst G-MD_ATSC   (keyset-ref-guard DALOS.DALOS|DEMIURGOI))
    (defconst G-SC_ATSC   (keyset-ref-guard ATS.ATS|SC_KEY))

    (defcap COMPOSE ()
        true
    )
    (defcap SECURE ()
        true
    )

    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (ATSC-ADMIN)
            (write PoliciesTable policy-name
                {"policy" : policy-guard}
            )
        )
    )
    (defun C_ReadPolicy:guard (policy-name:string)
        (at "policy" (read PoliciesTable policy-name ["policy"]))
    )

    (defcap P|ATSC|REMOTE-GOV ()
        true
    )
    (defcap P|ATSC|CALLER ()
        true
    )
    (defcap P|ATSC|UPDATE_ROU ()
        true
    )
    (defcap P|ATSC|UPDATE_UNSTAKE-POS ()
        true
    )
    
    (defun DefinePolicies ()
        (BASIS.A_AddPolicy
            "ATSC|Caller"
            (create-capability-guard (P|ATSC|CALLER))
        )
        (ATS.A_AddPolicy
            "ATSC|RemoteAutostakeGovernor"
            (create-capability-guard (P|ATSC|REMOTE-GOV))
        )
        (ATS.A_AddPolicy
            "ATSC|Caller"
            (create-capability-guard (P|ATSC|CALLER))
        )
        (ATS.A_AddPolicy
            "ATSC|UpdateROU"
            (create-capability-guard (P|ATSC|UPDATE_ROU))
        )
        (ATS.A_AddPolicy
            "ATSC|UpUnsPos"
            (create-capability-guard (P|ATSC|UPDATE_UNSTAKE-POS))
        )
        (TFT.A_AddPolicy
            "ATSC|Caller"
            (create-capability-guard (P|ATSC|CALLER))
        )
    )

    (deftable PoliciesTable:{DALOS.PolicySchema})

    ;;
    (defcap ATSC|COLD_RECOVERY (recoverer:string atspair:string ra:decimal)
        @event
        (compose-capability (P|ATSC|REMOTE-GOV))
        (compose-capability (P|ATSC|CALLER))
        (compose-capability (P|ATSC|UPDATE_ROU))
        (compose-capability (ATSC|DEPLOY atspair recoverer))
        (compose-capability (P|ATSC|UPDATE_UNSTAKE-POS))
        (DALOS.DALOS|CAP_EnforceAccountOwnership recoverer)
        (ATS.ATS|UEV_RecoveryState atspair true true)
    )
    (defcap ATSC|CULL (culler:string atspair:string)
        @event
        (compose-capability (P|ATSC|REMOTE-GOV))
        (compose-capability (P|ATSC|CALLER))
        (compose-capability (P|ATSC|UPDATE_ROU))
        (compose-capability (P|ATSC|UPDATE_UNSTAKE-POS))
        (compose-capability (ATSC|NORMALIZE_LEDGER atspair culler))
        (DALOS.DALOS|CAP_EnforceAccountOwnership culler)
        (ATS.ATS|UEV_id atspair)
    )
    (defcap ATSC|DEPLOY (atspair:string account:string)
        (DALOS.DALOS|UEV_EnforceAccountExists account)
        (ATS.ATS|UEV_id atspair)
        (compose-capability (ATSC|NORMALIZE_LEDGER atspair account))
    )
    (defcap ATSC|NORMALIZE_LEDGER (atspair:string account:string)
        (ATS.ATS|UEV_id atspair)
        (let*
            (
                (dalos-admin:guard G-MD_ATSC)
                (autos-admin:guard G-SC_ATSC)
                (account-g:guard (DALOS.DALOS|UR_AccountGuard account))
                (sov:string (DALOS.DALOS|UR_AccountSovereign account))
                (sov-g:guard (DALOS.DALOS|UR_AccountGuard sov))
                (gov-g:guard (DALOS.DALOS|UR_AccountGovernor account))
            )
            (enforce-one
                "Invalid permission for normalizing ATS|Ledger Account Operations"
                [
                    (enforce-guard dalos-admin)
                    (enforce-guard autos-admin)
                    (enforce-guard account-g)
                    (enforce-guard sov-g)
                    (enforce-guard gov-g)
                ]
            )
        )
        (compose-capability (P|ATSC|UPDATE_UNSTAKE-POS))
    )
    ;;
    (defun ATSC|C_ColdRecovery (patron:string recoverer:string atspair:string ra:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (ATSC|COLD_RECOVERY recoverer atspair ra)
            (ATSC|X_DeployAccount atspair recoverer)
            (let*
                (
                    (rt-lst:[string] (ATS.ATS|UR_RewardTokenList atspair))
                    (c-rbt:string (ATS.ATS|UR_ColdRewardBearingToken atspair))
                    (c-rbt-precision:integer (BASIS.DPTF-DPMF|UR_Decimals c-rbt true))
                    (usable-position:integer (ATS.ATS|UC_WhichPosition atspair ra recoverer))
                    (fee-promile:decimal (ATS.ATS|UC_ColdRecoveryFee atspair ra usable-position))
                    (c-rbt-fee-split:[decimal] (UTILS.ATS|UC_PromilleSplit fee-promile ra c-rbt-precision))
                    (c-fr:bool (ATS.ATS|UR_ColdRecoveryFeeRedirection atspair))
                    (cull-time:time (ATS.ATS|UC_CullColdRecoveryTime atspair recoverer))
                    ;;for false
                    (ng-c-fr:[decimal] (ATS.ATS|UC_RTSplitAmounts atspair (at 1 c-rbt-fee-split)))    ;;feeul
                    ;;for true
                    (positive-c-fr:[decimal] (ATS.ATS|UC_RTSplitAmounts atspair (at 0 c-rbt-fee-split)))    ;;remainderu
                )
                (TFT.DPTF|C_Transfer patron c-rbt recoverer ATS.ATS|SC_NAME ra true)
                (BASIS.DPTF|C_Burn patron c-rbt ATS.ATS|SC_NAME ra)
                (map
                    (lambda
                        (index:integer)
                        (ATS.ATS|XO_UpdateRoU atspair (at index rt-lst) false true (at index positive-c-fr))
                        (ATS.ATS|XO_UpdateRoU atspair (at index rt-lst) true false (at index positive-c-fr))
                    )
                    (enumerate 0 (- (length rt-lst) 1))
                )
                (if (not c-fr)
                    (map
                        (lambda
                            (index:integer)
                            (BASIS.DPTF|C_Burn patron (at index rt-lst) ATS.ATS|SC_NAME (at index ng-c-fr))
                        )
                        (enumerate 0 (- (length rt-lst) 1))
                    )
                    true
                )
                (ATSC|X_StoreUnstakeObject atspair recoverer usable-position 
                    { "reward-tokens"   : positive-c-fr 
                    , "cull-time"       : cull-time}
                )
            )
            (ATSC|X_Normalize atspair recoverer)
        )
    )
    ;;
    (defun ATSC|C_Cull:[decimal] (patron:string culler:string atspair:string)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (ATSC|CULL culler atspair)
            (let*
                (
                    (rt-lst:[string] (ATS.ATS|UR_RewardTokenList atspair))
                    (c0:[decimal] (ATSC|X_MultiCull atspair culler))
                    (c1:[decimal] (ATSC|X_SingleCull atspair culler 1))
                    (c2:[decimal] (ATSC|X_SingleCull atspair culler 2))
                    (c3:[decimal] (ATSC|X_SingleCull atspair culler 3))
                    (c4:[decimal] (ATSC|X_SingleCull atspair culler 4))
                    (c5:[decimal] (ATSC|X_SingleCull atspair culler 5))
                    (c6:[decimal] (ATSC|X_SingleCull atspair culler 6))
                    (c7:[decimal] (ATSC|X_SingleCull atspair culler 7))
                    (ca:[[decimal]] [c0 c1 c2 c3 c4 c5 c6 c7])
                    (cw:[decimal] (UTILS.UTILS|UC_AddHybridArray ca))
                )
                (map
                    (lambda
                        (idx:integer)
                        (if (!= (at idx cw) 0.0)
                            (with-capability (COMPOSE)
                                (ATS.ATS|XO_UpdateRoU atspair (at idx rt-lst) false false (at idx cw))
                                (TFT.DPTF|C_Transfer patron (at idx rt-lst) ATS.ATS|SC_NAME culler (at idx cw) true)
                            )
                            true
                        )
                    )
                    (enumerate 0 (- (length rt-lst) 1))
                )
                (ATSC|X_Normalize atspair culler)
                cw
            )
        )
    )
    ;;
    (defun ATSC|X_MultiCull:[decimal] (atspair:string account:string)
        (let*
            (
                (zr:object{ATS.Awo} (ATS.ATS|UCC_MakeZeroUnstakeObject atspair))
                (ng:object{ATS.Awo} (ATS.ATS|UCC_MakeNegativeUnstakeObject atspair))
                (p0:[object{ATS.Awo}] (ATS.ATS|UR_P0 atspair account))
                (p0l:integer (length p0))
                (boolean-lst:[bool]
                    (fold
                        (lambda
                            (acc:[bool] item:object{ATS.Awo})
                            (UTILS.LIST|UC_AppendLast acc (ATS.ATS|UC_IzCullable item))
                        )
                        []
                        p0
                    )
                )
                (zr-output:[decimal] (make-list (length (ATS.ATS|UR_RewardTokenList atspair)) 0.0))
                (cullables:[integer] (UTILS.LIST|UC_Search boolean-lst true))
                (immutables:[integer] (UTILS.LIST|UC_Search boolean-lst false))
                (how-many-cullables:integer (length cullables))
            )
            (if (= how-many-cullables 0)
                zr-output
                (let*
                    (
                        (after-cull:[object{ATS.Awo}]
                            (if (< how-many-cullables p0l)
                                (fold
                                    (lambda
                                        (acc:[object{ATS.Awo}] idx:integer)
                                        (UTILS.LIST|UC_AppendLast acc (at (at idx immutables) p0))
                                    )
                                    []
                                    (enumerate 0 (- (length immutables) 1))
                                )
                                [zr]
                            )
                        )
                        (to-be-culled:[object{ATS.Awo}]
                            (fold
                                (lambda
                                    (acc:[object{ATS.Awo}] idx:integer)
                                    (UTILS.LIST|UC_AppendLast acc (at (at idx cullables) p0))
                                )
                                []
                                (enumerate 0 (- (length cullables) 1))
                            )
                        )
                        (culled-values:[[decimal]]
                            (fold
                                (lambda
                                    (acc:[[decimal]] idx:integer)
                                    (UTILS.LIST|UC_AppendLast acc (ATS.ATS|UC_CullValue atspair (at idx to-be-culled)))
                                )
                                []
                                (enumerate 0 (- (length to-be-culled) 1))
                            )
                        )
                        (summed-culled-values:[decimal] (UTILS.UTILS|UC_AddHybridArray culled-values))
                    )
                    (ATS.X_UpP0 atspair account after-cull)
                    summed-culled-values
                )
            )
        )
    )
    (defun ATSC|X_SingleCull:[decimal] (atspair:string account:string position:integer)
        (let*
            (
                (rt-lst:[string] (ATS.ATS|UR_RewardTokenList atspair))
                (l:integer (length rt-lst))
                (empty:[decimal] (make-list l 0.0))
                (zr:object{ATS.Awo} (ATS.ATS|UCC_MakeZeroUnstakeObject atspair))
                (unstake-obj:object{ATS.Awo} (ATS.ATS|UR_P1-7 atspair account position))
                (rt-amounts:[decimal] (at "reward-tokens" unstake-obj))
                (cull-output:[decimal] (ATS.ATS|UC_CullValue atspair unstake-obj))
            )
            (if (!= cull-output empty)
                (ATSC|X_StoreUnstakeObject atspair account position zr)
                true
            )
            cull-output
        )
    )
    (defun ATSC|X_StoreUnstakeObject (atspair:string account:string position:integer obj:object{ATS.Awo})
        (let
            (
                (p0:[object{ATS.Awo}] (ATS.ATS|UR_P0 atspair account))
            )
            (if (= position -1)
                (if (and 
                        (= (length p0) 1)
                        (= 
                            (at 0 p0) 
                            (ATS.ATS|UCC_MakeZeroUnstakeObject atspair)
                        )
                    )
                    (ATS.X_UpP0 atspair account [obj])
                    (ATS.X_UpP0 atspair account (UTILS.LIST|UC_AppendLast p0 obj))
                )
                (if (= position 1)
                    (ATS.X_UpP1 atspair account obj)
                    (if (= position 2)
                        (ATS.X_UpP2 atspair account obj)
                        (if (= position 3)
                            (ATS.X_UpP3 atspair account obj)
                            (if (= position 4)
                                (ATS.X_UpP4 atspair account obj)
                                (if (= position 5)
                                    (ATS.X_UpP5 atspair account obj)
                                    (if (= position 6)
                                        (ATS.X_UpP6 atspair account obj)
                                        (ATS.X_UpP7 atspair account obj)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (defun ATSC|X_DeployAccount (atspair:string account:string)
        (require-capability (ATSC|DEPLOY atspair account))
        (ATS.ATS|X_SpawnAutostakeAccount atspair account)
        (ATSC|X_Normalize atspair account)
    )
    (defun ATSC|X_Normalize (atspair:string account:string)
        (require-capability (ATSC|NORMALIZE_LEDGER atspair account))
        (let*
            (
                (p0:[object{ATS.Awo}] (ATS.ATS|UR_P0 atspair account))
                (p1:object{ATS.Awo} (ATS.ATS|UR_P1-7 atspair account 1))
                (p2:object{ATS.Awo} (ATS.ATS|UR_P1-7 atspair account 2))
                (p3:object{ATS.Awo} (ATS.ATS|UR_P1-7 atspair account 3))
                (p4:object{ATS.Awo} (ATS.ATS|UR_P1-7 atspair account 4))
                (p5:object{ATS.Awo} (ATS.ATS|UR_P1-7 atspair account 5))
                (p6:object{ATS.Awo} (ATS.ATS|UR_P1-7 atspair account 6))
                (p7:object{ATS.Awo} (ATS.ATS|UR_P1-7 atspair account 7))

                (zr:object{ATS.Awo} (ATS.ATS|UCC_MakeZeroUnstakeObject atspair))
                (ng:object{ATS.Awo} (ATS.ATS|UCC_MakeNegativeUnstakeObject atspair))
                (positions:integer (ATS.ATS|UR_ColdRecoveryPositions atspair))
                (elite:bool (ATS.ATS|UR_EliteMode atspair))
                (major-tier:integer (DALOS.DALOS|UR_Elite-Tier-Major account))

                (p0-znn:[object{ATS.Awo}] (if (and (!= p0 [zr]) (!= p0 [ng])) p0 [ng]))
                (p0-znz:[object{ATS.Awo}] (if (and (!= p0 [zr]) (!= p0 [ng])) p0 [zr]))
                (p1-znn:object{ATS.Awo} (if (and (!= p1 zr) (!= p1 ng)) p1 ng))
                (p1-znz:object{ATS.Awo} (if (and (!= p1 zr) (!= p1 ng)) p1 zr))
                (p2-znn:object{ATS.Awo} (if (and (!= p2 zr) (!= p2 ng)) p2 ng))
                (p2-znz:object{ATS.Awo} (if (and (!= p2 zr) (!= p2 ng)) p2 zr))
                (p3-znn:object{ATS.Awo} (if (and (!= p3 zr) (!= p3 ng)) p3 ng))
                (p3-znz:object{ATS.Awo} (if (and (!= p3 zr) (!= p3 ng)) p3 zr))
                (p4-znn:object{ATS.Awo} (if (and (!= p4 zr) (!= p4 ng)) p4 ng))
                (p4-znz:object{ATS.Awo} (if (and (!= p4 zr) (!= p4 ng)) p4 zr))
                (p5-znn:object{ATS.Awo} (if (and (!= p5 zr) (!= p5 ng)) p5 ng))
                (p5-znz:object{ATS.Awo} (if (and (!= p5 zr) (!= p5 ng)) p5 zr))
                (p6-znn:object{ATS.Awo} (if (and (!= p6 zr) (!= p6 ng)) p6 ng))
                (p6-znz:object{ATS.Awo} (if (and (!= p6 zr) (!= p6 ng)) p6 zr))
                (p7-znn:object{ATS.Awo} (if (and (!= p7 zr) (!= p7 ng)) p7 ng))
                (p7-znz:object{ATS.Awo} (if (and (!= p7 zr) (!= p7 ng)) p7 zr))

                (p2-zne:object{ATS.Awo} (if (and (!= p2 zr) (!= p2 ng)) p2 (if (>= major-tier 2) zr ng)))
                (p3-zne:object{ATS.Awo} (if (and (!= p3 zr) (!= p3 ng)) p3 (if (>= major-tier 3) zr ng)))
                (p4-zne:object{ATS.Awo} (if (and (!= p4 zr) (!= p4 ng)) p4 (if (>= major-tier 4) zr ng)))
                (p5-zne:object{ATS.Awo} (if (and (!= p5 zr) (!= p5 ng)) p5 (if (>= major-tier 5) zr ng)))
                (p6-zne:object{ATS.Awo} (if (and (!= p6 zr) (!= p6 ng)) p6 (if (>= major-tier 6) zr ng)))
                (p7-zne:object{ATS.Awo} (if (and (!= p7 zr) (!= p7 ng)) p7 (if (>= major-tier 7) zr ng)))
            )
            (if (= positions -1)
                (with-capability (COMPOSE)
                    (ATS.X_UpP0 atspair account p0-znz)
                    (ATS.X_UpP1 atspair account p1-znn)
                    (ATS.X_UpP2 atspair account p2-znn)
                    (ATS.X_UpP3 atspair account p3-znn)
                    (ATS.X_UpP4 atspair account p4-znn)
                    (ATS.X_UpP5 atspair account p5-znn)
                    (ATS.X_UpP6 atspair account p6-znn)
                    (ATS.X_UpP7 atspair account p7-znn)
                )
                (if (= positions 1)
                    (with-capability (COMPOSE)
                        (ATS.X_UpP0 atspair account p0-znn)
                        (ATS.X_UpP1 atspair account p1-znz)
                        (ATS.X_UpP2 atspair account p2-znn)
                        (ATS.X_UpP3 atspair account p3-znn)
                        (ATS.X_UpP4 atspair account p4-znn)
                        (ATS.X_UpP5 atspair account p5-znn)
                        (ATS.X_UpP6 atspair account p6-znn)
                        (ATS.X_UpP7 atspair account p7-znn)
                    )
                    (if (= positions 2)
                        (with-capability (COMPOSE)
                            (ATS.X_UpP0 atspair account p0-znn)
                            (ATS.X_UpP1 atspair account p1-znz)
                            (ATS.X_UpP2 atspair account p2-znz)
                            (ATS.X_UpP3 atspair account p3-znn)
                            (ATS.X_UpP4 atspair account p4-znn)
                            (ATS.X_UpP5 atspair account p5-znn)
                            (ATS.X_UpP6 atspair account p6-znn)
                            (ATS.X_UpP7 atspair account p7-znn)
                        )
                        (if (= positions 3)
                            (with-capability (COMPOSE)
                                (ATS.X_UpP0 atspair account p0-znn)
                                (ATS.X_UpP1 atspair account p1-znz)
                                (ATS.X_UpP2 atspair account p2-znz)
                                (ATS.X_UpP3 atspair account p3-znz)
                                (ATS.X_UpP4 atspair account p4-znn)
                                (ATS.X_UpP5 atspair account p5-znn)
                                (ATS.X_UpP6 atspair account p6-znn)
                                (ATS.X_UpP7 atspair account p7-znn)
                            )
                            (if (= positions 4)
                                (with-capability (COMPOSE)
                                    (ATS.X_UpP0 atspair account p0-znn)
                                    (ATS.X_UpP1 atspair account p1-znz)
                                    (ATS.X_UpP2 atspair account p2-znz)
                                    (ATS.X_UpP3 atspair account p3-znz)
                                    (ATS.X_UpP4 atspair account p4-znz)
                                    (ATS.X_UpP5 atspair account p5-znn)
                                    (ATS.X_UpP6 atspair account p6-znn)
                                    (ATS.X_UpP7 atspair account p7-znn)
                                )
                                (if (= positions 5)
                                    (with-capability (COMPOSE)
                                        (ATS.X_UpP0 atspair account p0-znn)
                                        (ATS.X_UpP1 atspair account p1-znz)
                                        (ATS.X_UpP2 atspair account p2-znz)
                                        (ATS.X_UpP3 atspair account p3-znz)
                                        (ATS.X_UpP4 atspair account p4-znz)
                                        (ATS.X_UpP5 atspair account p5-znz)
                                        (ATS.X_UpP6 atspair account p6-znn)
                                        (ATS.X_UpP7 atspair account p7-znn)
                                    )
                                    (if (= positions 6)
                                        (with-capability (COMPOSE)
                                            (ATS.X_UpP0 atspair account p0-znn)
                                            (ATS.X_UpP1 atspair account p1-znz)
                                            (ATS.X_UpP2 atspair account p2-znz)
                                            (ATS.X_UpP3 atspair account p3-znz)
                                            (ATS.X_UpP4 atspair account p4-znz)
                                            (ATS.X_UpP5 atspair account p5-znz)
                                            (ATS.X_UpP6 atspair account p6-znz)
                                            (ATS.X_UpP7 atspair account p7-znn)
                                        )
                                        (if (not elite)
                                            (with-capability (COMPOSE)
                                                (ATS.X_UpP0 atspair account p0-znn)
                                                (ATS.X_UpP1 atspair account p1-znz)
                                                (ATS.X_UpP2 atspair account p2-znz)
                                                (ATS.X_UpP3 atspair account p3-znz)
                                                (ATS.X_UpP4 atspair account p4-znz)
                                                (ATS.X_UpP5 atspair account p5-znz)
                                                (ATS.X_UpP6 atspair account p6-znz)
                                                (ATS.X_UpP7 atspair account p7-znz)
                                            )
                                            (with-capability (COMPOSE)
                                                (ATS.X_UpP0 atspair account p0-znn)
                                                (ATS.X_UpP1 atspair account p1-znz)
                                                (ATS.X_UpP2 atspair account p2-zne)
                                                (ATS.X_UpP3 atspair account p3-zne)
                                                (ATS.X_UpP4 atspair account p4-zne)
                                                (ATS.X_UpP5 atspair account p5-zne)
                                                (ATS.X_UpP6 atspair account p6-zne)
                                                (ATS.X_UpP7 atspair account p7-zne)
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)
