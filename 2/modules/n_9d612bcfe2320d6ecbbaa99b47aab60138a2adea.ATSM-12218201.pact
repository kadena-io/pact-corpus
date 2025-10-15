(module ATSM GOVERNANCE
    (defcap GOVERNANCE ()
        (compose-capability (ATSM-ADMIN))
    )
    (defcap ATSM-ADMIN ()
        (enforce-one
            "ATSM Autostake Admin not satisfed"
            [
                (enforce-guard G-MD_ATSM)
                (enforce-guard G-SC_ATSM)
            ]
        )
    )

    (defconst G-MD_ATSM   (keyset-ref-guard DALOS.DALOS|DEMIURGOI))
    (defconst G-SC_ATSM   (keyset-ref-guard ATS.ATS|SC_KEY))

    (defcap COMPOSE ()
        true
    )
    (defcap SECURE ()
        true
    )

    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (ATSM-ADMIN)
            (write PoliciesTable policy-name
                {"policy" : policy-guard}
            )
        )
    )
    (defun C_ReadPolicy:guard (policy-name:string)
        (at "policy" (read PoliciesTable policy-name ["policy"]))
    )
    (defcap P|ATSM|REMOTE-GOV ()
        true
    )
    (defcap P|ATSM|UPDATE_ROU ()
        true
    )
    (defcap P|ATSM|CALLER ()
        true
    )
    (defcap P|ATSM|RESHAPE ()
        true
    )
    (defcap P|ATSM|RM_SECONDARY_RT ()
        true
    )
    (defcap P|DIN ()
        true
    )
    (defcap P|IC ()
        true
    )
    (defcap P|ATSM ()
        (compose-capability (P|ATSM|CALLER))
        (compose-capability (P|DIN))
        (compose-capability (P|IC))
    )

    (defun DefinePolicies ()
        (DALOS.A_AddPolicy
            "ATSM|PlusDalosNonce"
            (create-capability-guard (P|DIN))
        )
        (DALOS.A_AddPolicy
            "ATSM|GasCol"
            (create-capability-guard (P|IC))
        )
        (BASIS.A_AddPolicy
            "ATSM|Caller"
            (create-capability-guard (P|ATSM|CALLER))
        )
        (ATS.A_AddPolicy
            "ATSM|Caller"
            (create-capability-guard (P|ATSM|CALLER))
        )
        (ATSI.A_AddPolicy
            "ATSM|Caller"
            (create-capability-guard (P|ATSM|CALLER))
        )
        (ATS.A_AddPolicy
            "ATSM|RemoteAutostakeGovernor"
            (create-capability-guard (P|ATSM|REMOTE-GOV))
        )
        (ATS.A_AddPolicy
            "ATSM|UpdateROU"
            (create-capability-guard (P|ATSM|UPDATE_ROU))
        )
        (ATS.A_AddPolicy
            "ATSM|ReshapeUnstakeAccount"
            (create-capability-guard (P|ATSM|RESHAPE))
        )
        (ATS.A_AddPolicy
            "ATSM|RemoveSecondaryRT"
            (create-capability-guard (P|ATSM|RM_SECONDARY_RT))
        )
        (TFT.A_AddPolicy
            "ATSM|Caller"
            (create-capability-guard (P|ATSM|CALLER))
        )
    )

    (deftable PoliciesTable:{DALOS.PolicySchema})
    ;;
    (defcap ATSM|REMOVE_SECONDARY (atspair:string reward-token:string)
        @event
        (compose-capability (P|ATSM|REMOTE-GOV))
        (compose-capability (P|ATSM|UPDATE_ROU))
        (compose-capability (P|ATSM|RESHAPE))
        (compose-capability (P|ATSM|RM_SECONDARY_RT))
        (compose-capability (P|ATSM|CALLER))
        (ATS.ATS|CAP_Owner atspair)
        (ATS.ATS|UEV_UpdateColdAndHot atspair)
        (ATS.ATS|UEV_ParameterLockState atspair false)
        (let
            (
                (rt-position:integer (ATS.ATS|UC_RewardTokenPosition atspair reward-token))
            )
            (enforce (> rt-position 0) "Primal RT cannot be removed")
        )
    )
    ;;
    (defun ATSM|C_ChangeOwnership (patron:string atspair:string new-owner:string)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_BIGGEST)
                true
            )
            (ATS.ATS|X_ChangeOwnership atspair new-owner)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun ATSM|C_ToggleParameterLock (patron:string atspair:string toggle:bool)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (let
            (
                (ZG:bool (DALOS.IGNIS|URC_IsVirtualGasZero))
                (NZG:bool (DALOS.IGNIS|URC_IsNativeGasZero))
                (atspair-owner:string (ATS.ATS|UR_OwnerKonto atspair))
            )
            (with-capability (P|ATSM)
                (if (not ZG)
                    (DALOS.IGNIS|X_Collect patron atspair-owner DALOS.GAS_SMALL)
                    true
                )
                (let*
                    (
                        (toggle-costs:[decimal] (ATS.ATS|X_ToggleParameterLock atspair toggle))
                        (gas-costs:decimal (at 0 toggle-costs))
                        (kda-costs:decimal (at 1 toggle-costs))
                    )
                    (if (> gas-costs 0.0)
                        (with-capability (COMPOSE)
                            (if (not ZG)
                                (DALOS.IGNIS|X_Collect patron atspair-owner gas-costs)
                                true
                            )
                            (if (not NZG)
                                (DALOS.DALOS|C_TransferRawDalosFuel patron kda-costs)
                                true
                            )
                            (ATS.ATS|X_IncrementParameterUnlocks atspair)
                        )
                        true
                    )
                )
                (DALOS.DALOS|X_IncrementNonce patron)
            )
        )
    )
    (defun ATSM|C_UpdateSyphon (patron:string atspair:string syphon:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_SMALL)
                true
            )
            (ATS.ATS|X_UpdateSyphon atspair syphon)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun ATSM|C_ToggleSyphoning (patron:string atspair:string toggle:bool)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_SMALL)
                true
            )
            (ATS.ATS|X_ToggleSyphoning atspair toggle)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun ATSM|C_ToggleFeeSettings (patron:string atspair:string toggle:bool fee-switch:integer)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_SMALL)
                true
            )
            (ATS.ATS|X_ToggleFeeSettings atspair toggle fee-switch)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun ATSM|C_SetCRD (patron:string atspair:string soft-or-hard:bool base:integer growth:integer)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_SMALL)
                true
            )
            (ATS.ATS|X_SetCRD atspair soft-or-hard base growth)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun ATSM|C_SetColdFee (patron:string atspair:string fee-positions:integer fee-thresholds:[decimal] fee-array:[[decimal]])
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_SMALL)
                true
            )
            (ATS.ATS|X_SetColdFee atspair fee-positions fee-thresholds fee-array)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun ATSM|C_SetHotFee (patron:string atspair:string promile:decimal decay:integer)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_SMALL)
                true
            )
            (ATS.ATS|X_SetHotFee atspair promile decay)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun ATSM|C_ToggleElite (patron:string atspair:string toggle:bool)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_SMALL)
                true
            )
            (ATS.ATS|X_ToggleElite atspair toggle)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun ATSM|C_TurnRecoveryOn (patron:string atspair:string cold-or-hot:bool)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_SMALL)
                true
            )
            (ATS.ATS|X_TurnRecoveryOn atspair cold-or-hot)
            (DALOS.DALOS|X_IncrementNonce patron)
            (ATSI.ATSI|XC_EnsureActivationRoles patron atspair cold-or-hot)
        )
    )
    (defun ATSM|C_AddSecondary (patron:string atspair:string reward-token:string rt-nfr:bool)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_ISSUE)
                true
            )
            (BASIS.DPTF-DPMF|C_DeployAccount reward-token ATS.ATS|SC_NAME true)
            (ATS.ATS|X_AddSecondary atspair reward-token rt-nfr)
            (DALOS.DALOS|X_IncrementNonce patron)
            (BASIS.DPTF|XO_UpdateRewardToken atspair reward-token true)      
            (ATSI.ATSI|XC_EnsureActivationRoles patron atspair true)
            (if (= (ATS.ATS|UC_IzPresentHotRBT atspair) true)
                (ATSI.ATSI|XC_EnsureActivationRoles patron atspair false)
                true
            )
        )
    )
    (defun ATSM|C_RemoveSecondary (patron:string remover:string atspair:string reward-token:string)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (ATSM|REMOVE_SECONDARY atspair reward-token)
            (let*
                (
                    (rt-lst:[string] (ATS.ATS|UR_RewardTokenList atspair))
                    (remove-position:integer (at 0 (UTILS.LIST|UC_Search rt-lst reward-token)))
                    (primal-rt:string (at 0 rt-lst))
                    (resident-sum:decimal (at remove-position (ATS.ATS|UR_RoUAmountList atspair true)))
                    (unbound-sum:decimal (at remove-position (ATS.ATS|UR_RoUAmountList atspair false)))
                    (remove-sum:decimal (+ resident-sum unbound-sum))

                    (accounts-with-atspair-data:[string] (TFT.DPTF-DPMF-ATS|UR_FilterKeysForInfo atspair 3 false))
                )
            ;;1]The RT to be removed, is transfered to the remover, from the ATS|SC_NAME
                (TFT.DPTF|C_Transfer patron reward-token ATS.ATS|SC_NAME remover remove-sum true)
            ;;2]The amount removed is added back as Primal-RT
                (TFT.DPTF|C_Transfer patron primal-rt remover ATS.ATS|SC_NAME remove-sum true)
            ;;3]ROU Table is updated with the new DATA, now as primal RT
                (ATS.ATS|XO_UpdateRoU atspair primal-rt true true resident-sum)
                (ATS.ATS|XO_UpdateRoU atspair primal-rt false true unbound-sum)
            ;;4]Client Accounts are modified to remove the RT Token and update balances with Primal RT
                (map
                    (lambda
                        (kontos:string)
                        (ATS.ATS|XO_ReshapeUnstakeAccount atspair kontos remove-position)
                    )
                    accounts-with-atspair-data
                )
            ;;5]Actually Remove the RT from the ATS-Pair
                (ATS.ATS|XO_RemoveSecondary atspair reward-token)
            ;;6]Update Data in the DPTF Token Properties
                (BASIS.DPTF|XO_UpdateRewardToken atspair reward-token false)
            )
        )
    )
    (defun ATSM|C_AddHotRBT (patron:string atspair:string hot-rbt:string)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (P|ATSM)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (ATS.ATS|UR_OwnerKonto atspair) DALOS.GAS_ISSUE)
                true
            )
            (BASIS.DPTF-DPMF|C_DeployAccount hot-rbt ATS.ATS|SC_NAME false)
            (ATS.ATS|X_AddHotRBT atspair hot-rbt)
            (DALOS.DALOS|X_IncrementNonce patron)
            (BASIS.DPTF-DPMF|XO_UpdateRewardBearingToken hot-rbt atspair false)
            (ATSI.ATSI|XC_EnsureActivationRoles patron atspair false)
        )
    )
    ;;Usage
    (defcap ATSM|FCC (atspair:string fcc-token:string)
        (ATS.ATS|UEV_RewardTokenExistance atspair fcc-token true)
        (compose-capability (P|ATSM|REMOTE-GOV))
        (compose-capability (P|ATSM|UPDATE_ROU))
        (compose-capability (P|ATSM|CALLER))
    )
    (defcap ATSM|FUEL (atspair:string reward-token:string)
        @event
        (compose-capability (ATSM|FCC atspair reward-token))
        (let
            (
                (index:decimal (ATS.ATS|UC_Index atspair))
            )
            (enforce (>= index 0.1) "Fueling cannot take place on a negative Index")
        )
    )
    (defcap ATSM|COIL_OR_CURL (atspair:string coil-token:string)
        @event
        (compose-capability (ATSM|FCC atspair coil-token))
    )
    (defcap ATSM|SYPHON (atspair:string syphon-amounts:[decimal])
        @event
        (compose-capability (P|ATSM|REMOTE-GOV))
        (compose-capability (P|ATSM|UPDATE_ROU))
        (compose-capability (P|ATSM|CALLER))
        (ATS.ATS|CAP_Owner atspair)
        (ATS.ATS|UEV_SyphoningState atspair true)
        (let*
            (
                (rt-lst:[string] (ATS.ATS|UR_RewardTokenList atspair))
                (l0:integer (length syphon-amounts))
                (l1:integer (length rt-lst))
                (max-syphon:[decimal] (ATS.ATS|URC_MaxSyphon atspair))
                (max-syphon-sum:decimal (fold (+) 0.0 max-syphon))
                (input-syphon-sum:decimal (fold (+) 0.0 syphon-amounts))

                (resident-amounts:[decimal] (ATS.ATS|UR_RoUAmountList atspair true))
                (supply-check:[bool] (zip (lambda (x:decimal y:decimal) (<= x y)) syphon-amounts resident-amounts))
                (tr-nr:integer (length (UTILS.LIST|UC_Search supply-check true)))
            )
            (enforce (= l0 l1) "Invalid Amounts of Syphon Values")
            (enforce (> input-syphon-sum 0.0) "Invalid Syphon Amounts")
            (map
                (lambda
                    (sv:decimal)
                    (enforce (>= sv 0.0) "Unallowed Negative Syphon Values Detected !")
                )
                syphon-amounts
            )
            (enforce (<= input-syphon-sum max-syphon-sum) "Syphon Amounts surpassing pairs Syphon-Index")
            (enforce (= l0 tr-nr) "Invalid syphon amounts surpassing present resident Amounts")
        )
    )
    (defcap ATS|KICKSTART (kickstarter:string atspair:string rt-amounts:[decimal] rbt-request-amount:decimal)
        @event
        (compose-capability (P|ATSM|REMOTE-GOV))
        (compose-capability (P|ATSM|UPDATE_ROU))
        (compose-capability (P|ATSM|CALLER))
        (let*
            (
                (index:decimal (ATS.ATS|UC_Index atspair))
                (rt-lst:[string] (ATS.ATS|UR_RewardTokenList atspair))
                (l1:integer (length rt-amounts))
                (l2:integer (length rt-lst))
                (owner:string (ATS.ATS|UR_OwnerKonto atspair))

            )
            (enforce (= index -1.0) "Kickstarting can only be done on ATS-Pairs with -1 Index")
            (enforce (= l1 l2) "RT-Amounts list does not correspond with the Number of the ATS-Pair Reward Tokens")
            (enforce (> rbt-request-amount 0.0) "RBT Request Amount must be greater than zero!")
            (DALOS.DALOS|CAP_EnforceAccountOwnership owner)
            (DALOS.DALOS|UEV_EnforceAccountType kickstarter false)
        )
    )
    (defun ATSM|C_Syphon (patron:string syphon-target:string atspair:string syphon-amounts:[decimal])
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (ATSM|SYPHON atspair syphon-amounts)
            (let
                (
                    (rt-lst:[string] (ATS.ATS|UR_RewardTokenList atspair))
                )
                (map
                    (lambda
                        (index:integer)
                        (if (> (at index syphon-amounts) 0.0)
                            (with-capability (COMPOSE)
                                (TFT.DPTF|C_Transfer patron (at index rt-lst) ATS.ATS|SC_NAME syphon-target (at index syphon-amounts) true)
                                (ATS.ATS|XO_UpdateRoU atspair (at index rt-lst) true false (at index syphon-amounts))
                            )
                            true
                        )
                    )
                    (enumerate 0 (- (length rt-lst) 1))
                )
            )
        )
    )
    (defun ATSM|C_KickStart (patron:string kickstarter:string atspair:string rt-amounts:[decimal] rbt-request-amount:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (ATS|KICKSTART kickstarter atspair rt-amounts rbt-request-amount)
            (let
                (
                    (rbt-id:string (ATS.ATS|UR_ColdRewardBearingToken atspair))
                    (rt-lst:[string] (ATS.ATS|UR_RewardTokenList atspair))
                )
                (map
                    (lambda
                        (index:integer)
                        (with-capability (COMPOSE)
                            (TFT.DPTF|C_Transfer patron (at index rt-lst) kickstarter ATS.ATS|SC_NAME (at index rt-amounts) true)
                            (ATS.ATS|XO_UpdateRoU atspair (at index rt-lst) true true (at index rt-amounts))
                        )
                        
                    )
                    (enumerate 0 (- (length rt-lst) 1))
                )
                (BASIS.DPTF|C_Mint patron rbt-id ATS.ATS|SC_NAME rbt-request-amount false)
                (TFT.DPTF|C_Transfer patron rbt-id ATS.ATS|SC_NAME kickstarter rbt-request-amount true)
            )
        )
    )
    (defun ATSM|C_Fuel (patron:string fueler:string atspair:string reward-token:string amount:decimal)
        (enforce-one
            "Fueling not allowed"
            [
                (enforce-guard (C_ReadPolicy "OUROBOROS|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
            ]
        )
        (with-capability (ATSM|FUEL atspair reward-token)
            (TFT.DPTF|C_Transfer patron reward-token fueler ATS.ATS|SC_NAME amount true)
            (ATS.ATS|XO_UpdateRoU atspair reward-token true true amount)
        )
    )
    
    (defun ATSM|C_Coil:decimal (patron:string coiler:string atspair:string rt:string amount:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (ATSM|COIL_OR_CURL atspair rt)
            (let
                (
                    (c-rbt:string (ATS.ATS|UR_ColdRewardBearingToken atspair))
                    (c-rbt-amount:decimal (ATS.ATS|UC_RBT atspair rt amount))
                )
                (TFT.DPTF|C_Transfer patron rt coiler ATS.ATS|SC_NAME amount true)
                (ATS.ATS|XO_UpdateRoU atspair rt true true amount)
                (BASIS.DPTF|C_Mint patron c-rbt ATS.ATS|SC_NAME c-rbt-amount false)
                (TFT.DPTF|C_Transfer patron c-rbt ATS.ATS|SC_NAME coiler c-rbt-amount true)
                c-rbt-amount
            )
        )
    )
    (defun ATSM|C_Curl:decimal (patron:string curler:string atspair1:string atspair2:string rt:string amount:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (ATSM|COIL_OR_CURL atspair1 rt)
            (let*
                (
                    (c-rbt1:string (ATS.ATS|UR_ColdRewardBearingToken atspair1))
                    (c-rbt1-amount:decimal (ATS.ATS|UC_RBT atspair1 rt amount))
                    (c-rbt2:string (ATS.ATS|UR_ColdRewardBearingToken atspair2))
                    (c-rbt2-amount:decimal (ATS.ATS|UC_RBT atspair2 c-rbt1 c-rbt1-amount))
                )
                (TFT.DPTF|C_Transfer patron rt curler ATS.ATS|SC_NAME amount true)
                (ATS.ATS|XO_UpdateRoU atspair1 rt true true amount)
                (BASIS.DPTF|C_Mint patron c-rbt1 ATS.ATS|SC_NAME c-rbt1-amount false)
                (ATS.ATS|XO_UpdateRoU atspair2 c-rbt1 true true c-rbt1-amount)
                (BASIS.DPTF|C_Mint patron c-rbt2 ATS.ATS|SC_NAME c-rbt2-amount false)
                (TFT.DPTF|C_Transfer patron c-rbt2 ATS.ATS|SC_NAME curler c-rbt2-amount true)
                c-rbt2-amount
            )
        )
    )
)
