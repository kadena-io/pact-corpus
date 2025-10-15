(module ATS GOVERNANCE
    (defcap GOVERNANCE ()
        (compose-capability (ATS-ADMIN))
    )
    (defcap ATS-ADMIN ()
        (enforce-one
            "ATS Autostake Admin not satisfed"
            [
                (enforce-guard G-MD_ATS)
                (enforce-guard G-SC_ATS)
            ]
        )
    )

    (defconst G-MD_ATS   (keyset-ref-guard DALOS.DALOS|DEMIURGOI))
    (defconst G-SC_ATS   (keyset-ref-guard ATS|SC_KEY))
    
    (defconst ATS|SC_KEY 
        (+ UTILS.NS_USE ".dh_sc_autostake-keyset")
    )
    (defconst ATS|SC_NAME "Σ.ëŤΦșźUÉM89ŹïuÆÒÕ£żíëцΘЯнŹÿxжöwΨ¥Пууhďíπ₱nιrŹÅöыыidõd7ì₿ипΛДĎĎйĄшΛŁPMȘïõμîμŻIцЖljÃαbäЗŸÖéÂЫèpAДuÿPσ8ÎoŃЮнsŤΞìтČ₿Ñ8üĞÕPșчÌșÄG∇MZĂÒЖь₿ØDCПãńΛЬõŞŤЙšÒŸПĘЛΠws9€ΦуêÈŽŻ")     ;;Former DalosAutostake
    (defconst ATS|SC_KDA-NAME "k:89faf537ec7282d55488de28c454448a20659607adc52f875da30a4fd4ed2d12")
    
    (defcap COMPOSE ()
        true
    )
    (defcap SECURE ()
        true
    )
    (defcap ATS|RESHAPE ()
        true
    )
    (defcap ATS|RM_SECONDARY_RT ()
        true
    )
    (defcap ATS|UPDATE_ROU ()
        true
    )
    (defcap DALOS|EXECUTOR ()
        true
    )
    (defcap SUMMONER ()
        true
    )
    
    (defcap P|DIN ()
        true
    )
    (defcap P|IC ()
        true
    )
    
    (defcap P|DINIC ()
        (compose-capability (P|DIN))
        (compose-capability (P|IC))
    )
    
    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (ATS-ADMIN)
            (write PoliciesTable policy-name
                {"policy" : policy-guard}
            )
        )
    )
    (defun C_ReadPolicy:guard (policy-name:string)
        (at "policy" (read PoliciesTable policy-name ["policy"]))
    )

    (defun DefinePolicies ()             
        (DALOS.A_AddPolicy
            "ATS|PlusDalosNonce"
            (create-capability-guard (P|DIN))
        )
        (DALOS.A_AddPolicy 
            "ATS|Sum"
            (create-capability-guard (SUMMONER))
        )
        (DALOS.A_AddPolicy
            "ATS|GasCol"
            (create-capability-guard (P|IC))
        )
        ;;
        (BASIS.A_AddPolicy
            "ATS|Sum"
            (create-capability-guard (SUMMONER))
        )
    )

    (defcap ATS|GOV ()
        @doc "Autostake Module Governor Capability"
        true
    )
    (defun ATS|SetGovernor (patron:string)
        (with-capability (SUMMONER)
            (DALOS.DALOS|C_RotateGovernor
                patron
                ATS|SC_NAME
                (UTILS.GUARD|UEV_Any
                    [
                        (create-capability-guard (ATS|GOV))
                        (C_ReadPolicy "ATSC|RemoteAutostakeGovernor")
                        (C_ReadPolicy "ATSH|RemoteAutostakeGovernor")
                        (C_ReadPolicy "ATSM|RemoteAutostakeGovernor")
                    ]
                )
            )
        )
    )

    (defconst NULLTIME (time "1984-10-11T11:10:00Z"))
    (defconst ANTITIME (time "1983-08-07T11:10:00Z"))

    (defschema ATS|PropertiesSchema
        owner-konto:string
        can-change-owner:bool
        parameter-lock:bool
        unlocks:integer

        pair-index-name:string
        index-decimals:integer
        syphon:decimal
        syphoning:bool

        reward-tokens:[object{ATS|RewardTokenSchema}]

        c-rbt:string
        c-nfr:bool
        c-positions:integer
        c-limits:[decimal]
        c-array:[[decimal]]
        c-fr:bool
        c-duration:[integer]
        c-elite-mode:bool

        h-rbt:string
        h-promile:decimal
        h-decay:integer
        h-fr:bool

        cold-recovery:bool
        hot-recovery:bool
    )
    (defschema ATS|RewardTokenSchema
        token:string
        nfr:bool
        resident:decimal
        unbonding:decimal
    )

    (defschema ATS|BalanceSchema
        @doc "Key = <ATS-Pair> + UTILS.BAR + <account>"
        P0:[object{Awo}]
        P1:object{Awo}
        P2:object{Awo}
        P3:object{Awo}
        P4:object{Awo}
        P5:object{Awo}
        P6:object{Awo}
        P7:object{Awo}
    )
    ;;Awo - Schema for Autostake withdraw Object
    (defschema Awo
        reward-tokens:[decimal]
        cull-time:time
    )
    (defschema ATS|Hot
        mint-time:time
    )

    (deftable PoliciesTable:{DALOS.PolicySchema})
    (deftable ATS|Pairs:{ATS|PropertiesSchema})
    (deftable ATS|Ledger:{ATS|BalanceSchema})

    ;;ATS
    (defun ATS|CAP_Owner (atspair:string)
        (DALOS.DALOS|CAP_EnforceAccountOwnership (ATS|UR_OwnerKonto atspair))
    )
    (defcap ATS|CF|OWNER (atspair:string)
        (ATS|CAP_Owner atspair)
    )
    (defun ATS|UEV_CanChangeOwnerON (atspair:string)
        (ATS|UEV_id atspair)
        (let
            (
                (x:bool (ATS|UR_CanChangeOwner atspair))
            )
            (enforce (= x true) (format "ATS Pair {} ownership cannot be changed" [atspair]))
        )
    )
    (defun ATS|UEV_RewardTokenExistance (atspair:string reward-token:string existance:bool)
        (let
            (
                (existance-check:bool (BASIS.ATS|UC_IzRTg atspair reward-token))
            )
            (enforce (= existance-check existance) (format "{} Existance isnt verified for Token {} as RT with ATS Pair {}" [existance reward-token atspair]))
        )
    )
    (defun ATS|UEV_RewardBearingTokenExistance (atspair:string reward-bearing-token:string existance:bool cold-or-hot:bool)
        (let
            (
                (existance-check:bool (BASIS.ATS|UC_IzRBTg atspair reward-bearing-token cold-or-hot))
            )
            (enforce (= existance-check existance) (format "{} Existance isnt verified for Token {} as RBT with ATS Pair {}" [existance reward-bearing-token atspair]))
        )
    )
    (defun ATS|UEV_HotRewardBearingTokenPresence (atspair:string enforced-presence:bool)
        (let
            (
                (presence-check:bool (ATS|UC_IzPresentHotRBT atspair))
            )
            (enforce (= presence-check enforced-presence) (format "ATS Pair {} cant verfiy {} presence for a Hot RBT Token" [atspair enforced-presence]))
        )
    )
    (defun ATS|UEV_ParameterLockState (atspair:string state:bool)
        (let
            (
                (x:bool (ATS|UR_Lock atspair))
            )
            (enforce (= x state) (format "Parameter-lock for ATS Pair {} must be set to {} for this operation" [atspair state]))
        )
    )
    (defun ATS|UEV_SyphoningState (atspair:string state:bool)
        (let
            (
                (x:bool (ATS|UR_Syphoning atspair))
            )
            (enforce (= x state) (format "Syphoning for ATS Pair {} must be set to {} for this operation" [atspair state]))
        )
    )
    (defun ATS|UEV_FeeState (atspair:string state:bool fee-switch:integer)
        (let
            (
                (x:bool (ATS|UR_ColdNativeFeeRedirection atspair))
                (y:bool (ATS|UR_ColdRecoveryFeeRedirection atspair))
                (z:bool (ATS|UR_HotRecoveryFeeRedirection atspair))
            )
            (if (= fee-switch 0)
                (enforce (= x state) (format "Cold-NFR for ATS Pair {} must be set to {} for this operation" [atspair state]))
                (if (= fee-switch 1)
                    (enforce (= y state) (format "Cold-RFR for ATS Pair {} must be set to {} for this operation" [atspair state]))
                    (enforce (= z state) (format "Hot-RFR for ATS Pair {} must be set to {} for this operation" [atspair state]))
                )
            )
        )
    )
    (defun ATS|UEV_EliteState (atspair:string state:bool)
        (let
            (
                (x:bool (ATS|UR_EliteMode atspair))
            )
            (enforce (= x state) (format "Elite-Mode for ATS Pair {} must be set to {} for this operation" [atspair state]))
        )
    )
    (defun ATS|UEV_RecoveryState (atspair:string state:bool cold-or-hot:bool)
        (let
            (
                (x:bool (ATS|UR_ToggleColdRecovery atspair))
                (y:bool (ATS|UR_ToggleHotRecovery atspair))
            )
            (if cold-or-hot
                (enforce (= x state) (format "Cold-recovery for ATS Pair {} must be set to {} for this operation" [atspair state]))
                (enforce (= y state) (format "Hot-recovery for ATS Pair {} must be set to {} for this operation" [atspair state]))
            )
        )
    )
    (defun ATS|UEV_UpdateColdOrHot (atspair:string cold-or-hot:bool)
        (ATS|UEV_ParameterLockState atspair false)
        (if cold-or-hot
            (ATS|UEV_RecoveryState atspair false true)
            (ATS|UEV_RecoveryState atspair false false)
        )
    )
    (defun ATS|UEV_UpdateColdAndHot (atspair:string)
        (ATS|UEV_ParameterLockState atspair false)
        (ATS|UEV_RecoveryState atspair false true)
        (ATS|UEV_RecoveryState atspair false false)
    )
    (defun ATS|UEV_id (atspair:string)
        (UTILS.DALOS|UEV_UniqueAtspair atspair)
        (with-default-read ATS|Pairs atspair
            { "unlocks" : -1 }
            { "unlocks" := u }
            (enforce
                (>= u 0)
                (format "ATS-Pair {} does not exist." [atspair])
            )
        )
    )
    (defun ATS|UEV_IzTokenUnique (atspair:string reward-token:string)
        (BASIS.DPTF-DPMF|UEV_id reward-token true)
        (let
            (
                (rtl:[string] (ATS|UR_RewardTokenList atspair))
            )
            (enforce 
                (= (contains reward-token rtl) false) 
                (format "Token {} already exists as Reward Token for the ATS Pair {}" [reward-token atspair]))     
        )
    )
    
    (defcap ATS|X_OWNERSHIP_CHANGE (atspair:string new-owner:string)
        (DALOS.DALOS|UEV_SenderWithReceiver (ATS|UR_OwnerKonto atspair) new-owner)
        (DALOS.DALOS|UEV_EnforceAccountExists new-owner)
        (ATS|CAP_Owner atspair)
        (ATS|UEV_CanChangeOwnerON atspair)
    )
    (defcap ATS|X_TOGGLE_PARAMETER-LOCK (atspair:string toggle:bool)
        (ATS|CAP_Owner atspair)
        (ATS|UEV_ParameterLockState atspair (not toggle))
        (let
            (
                (x:bool (ATS|UR_ToggleColdRecovery atspair))
                (y:bool (ATS|UR_ToggleHotRecovery atspair))
            )
            (enforce-one
                (format "ATS <parameter-lock> cannot be toggled when both <cold-recovery> and <hot-recovery> are set to false")
                [
                    (enforce (= x true) (format "Cold-recovery for ATS Pair {} must be set to <true> for this operation" [atspair]))
                    (enforce (= y true) (format "Hot-recovery for ATS Pair {} must be set to <true> for this operation" [atspair]))
                ]
            )
        )
    )
    (defcap ATS|X_SYPHON (atspair:string syphon:decimal)
        (enforce (>= syphon 0.1) "Syphon cannot be set lower than 0.1")
        (ATS|CAP_Owner atspair)
        (let
            (
                (precision:integer (ATS|UR_IndexDecimals atspair))
            )
            (enforce
                (= (floor syphon precision) syphon)
                (format "The syphon value of {} is not a valid Index Value for the {} ATS Pair" [syphon atspair])
            )
        )
    )
    (defcap ATS|X_SYPHONING (atspair:string toggle:bool)
        (ATS|CAP_Owner atspair)
        (ATS|UEV_SyphoningState atspair (not toggle))
        (ATS|UEV_ParameterLockState atspair false)
    )
    (defcap ATS|X_TOGGLE_FEE (atspair:string toggle:bool fee-switch:integer)
        (enforce (contains fee-switch (enumerate 0 2)) "Integer not a valid fee-switch integer")
        (ATS|CAP_Owner atspair)
        (ATS|UEV_FeeState atspair (not toggle) fee-switch)
        (ATS|UEV_ParameterLockState atspair false)
        (if (or (= fee-switch 0)(= fee-switch 1))
            (ATS|UEV_UpdateColdOrHot atspair true)
            (ATS|UEV_UpdateColdOrHot atspair false)
        )
    )
    (defcap ATS|X_SET_CRD (atspair:string soft-or-hard:bool base:integer growth:integer)
        (ATS|CAP_Owner atspair)
        (ATS|UEV_UpdateColdOrHot atspair true)
        (ATS|UEV_ParameterLockState atspair false)
        (if (= soft-or-hard true)
            (enforce 
                (and 
                    (= (mod base growth) 0)
                    (= (mod growth 3) 0)
                ) 
                (format "{} as base and {} as growth are incompatible with the Soft Method for generation of CRD" [base growth])
            )
            (enforce 
                (= (mod base growth) 0)
                (format "{} as base and {} as growth are incompatible with the Hard Method for generation of CRD" [base growth])    
            )
        )
    )
    (defcap ATS|X_SET_COLD_FEE (atspair:string fee-positions:integer fee-thresholds:[decimal] fee-array:[[decimal]])
        (ATS|CAP_Owner atspair)
        (ATS|UEV_UpdateColdOrHot atspair true)
        (ATS|UEV_ParameterLockState atspair false)
        (enforce 
            (or 
                (= fee-positions -1)
                (contains fee-positions (enumerate 1 7))
            )
            "The Number of Fee Positions must be either -1 or between 1 and 7"
        )
        (enforce 
            (and
                (>= (length fee-thresholds) 1)
                (<= (length fee-thresholds) 100)
            )
            "No More than 100 Fee Threhsolds can be set"
        )
        (fold
            (lambda
                (acc:bool idx:integer)
                (let*
                    (
                        (current:decimal (at idx fee-thresholds))
                        (precision:integer (BASIS.DPTF-DPMF|UR_Decimals (ATS|UR_ColdRewardBearingToken atspair) true))
                    )
                    (if (<= idx (- (length fee-thresholds) 2))
                        (let
                            (
                                (next:decimal (at (+ idx 1) fee-thresholds))
                            )
                            (enforce 
                                (< current next)
                                (format "Current Amount {} must be smaller than the next Amount in the Threhsold List" [current next])
                            )
                        )
                        true
                    )
                    (enforce
                        (= (floor current precision) current)
                        (format "The Amount of {} does not conform with the CRBT decimals number" [current])
                    )
                    acc
                )
            )
            true
            (enumerate 0 (- (length fee-thresholds) 1))
        )
        (if (= (ATS|UC_ZeroColdFeeExceptionBoolean fee-thresholds fee-array) true)
            (if (= fee-positions -1)
                (enforce (= (length fee-array) 1) "The input <fee-array> must be of length 1")
                (enforce (= (length fee-array) fee-positions) (format "The input <fee-array> must be of length {}" [fee-positions]))
            )
            true
        )
        (UTILS.UTILS|UEV_DecimalArray fee-array)
        (if (= (ATS|UC_ZeroColdFeeExceptionBoolean fee-thresholds fee-array) true)
            (enforce
                (= (length (at 0 fee-array)) (+ (length fee-thresholds) 1))
                "Inner Lists of the <fee-array> are incompatible with the <fee-thresholds> length"
            )
            true
        )
        (map
            (lambda 
                (inner-lst:[decimal])
                (map
                    (lambda 
                        (fee:decimal)
                        (UTILS.DALOS|UEV_Fee fee)
                    )
                    inner-lst
                )
            )
            fee-array
        )
    )
    (defcap ATS|X_SET_HOT_FEE (atspair:string promile:decimal decay:integer)
        (ATS|CAP_Owner atspair)
        (ATS|UEV_UpdateColdOrHot atspair false)
        (ATS|UEV_ParameterLockState atspair false)
        (UTILS.DALOS|UEV_Fee promile)
        (enforce 
            (and
                (>= decay 1)
                (<= decay 9125)
            )
            "No More than 25 years (9125 days) can be set for Decay Period"
        )
    )
    (defcap ATS|X_TOGGLE_ELITE (atspair:string toggle:bool)
        (ATS|CAP_Owner atspair)
        (ATS|UEV_UpdateColdOrHot atspair true)
        (ATS|UEV_EliteState atspair (not toggle))
        (ATS|UEV_ParameterLockState atspair false)
        (if (= toggle true)
            (let
                (
                    (x:integer (ATS|UR_ColdRecoveryPositions atspair))
                )
                (enforce (= x 7) (format "Cold Recovery Positions for ATS Pair {} must be set to 7 for this operation" [atspair]))
            )
            true
        )
    )
    (defcap ATS|X_RECOVERY-ON (atspair:string cold-or-hot:bool)
        @event
        (ATS|CAP_Owner atspair)
        (ATS|UEV_ParameterLockState atspair false)
        (ATS|UEV_RecoveryState atspair false cold-or-hot)
    )
    (defcap ATS|X_RECOVERY-OFF (atspair:string cold-or-hot:bool)
        @event
        (ATS|CAP_Owner atspair)
        (ATS|UEV_ParameterLockState atspair false)
        (ATS|UEV_RecoveryState atspair true cold-or-hot)
    )
    
    (defcap ATS|X_ADD_SECONDARY (atspair:string reward-token:string token-type:bool)
        (BASIS.DPTF-DPMF|CAP_Owner reward-token token-type)
        (ATS|CAP_Owner atspair)
        (ATS|UEV_UpdateColdAndHot atspair)
        (ATS|UEV_ParameterLockState atspair false)
        (if (= token-type true)
            (compose-capability (ATS|ADD_SECONDARY_RT atspair reward-token))
            (compose-capability (ATS|ADD_SECONDARY_RBT atspair reward-token))
        )
    )
    (defcap ATS|ADD_SECONDARY_RT (atspair:string reward-token:string)
        @event
        (ATS|UEV_IzTokenUnique atspair reward-token)
        (ATS|UEV_RewardTokenExistance atspair reward-token false)
        (compose-capability (SUMMONER))
    )
    (defcap ATS|ADD_SECONDARY_RBT (atspair:string hot-rbt:string)
        @event
        (ATS|UEV_HotRewardBearingTokenPresence atspair false)   
        (compose-capability (SUMMONER))
        (BASIS.VST|UEV_Existance hot-rbt false false)
    )
    ;;
    (defun ATS|P-KEYS:[string] ()
        (keys ATS|Pairs)
    )
    (defun ATS|KEYS:[string] ()
        (keys ATS|Ledger)
    )
    (defun ATS|UR_OwnerKonto:string (atspair:string)
        (at "owner-konto" (read ATS|Pairs atspair ["owner-konto"]))
    )
    (defun ATS|UR_CanChangeOwner:bool (atspair:string)
        (at "can-change-owner" (read ATS|Pairs atspair ["can-change-owner"]))
    )
    (defun ATS|UR_Lock:bool (atspair:string)
        (at "parameter-lock" (read ATS|Pairs atspair ["parameter-lock"]))
    )
    (defun ATS|UR_Unlocks:integer (atspair:string)
        (at "unlocks" (read ATS|Pairs atspair ["unlocks"]))
    )
    (defun ATS|UR_IndexName:string (atspair:string)
        (at "pair-index-name" (read ATS|Pairs atspair ["pair-index-name"]))
    )
    (defun ATS|UR_IndexDecimals:integer (atspair:string)
        (at "index-decimals" (read ATS|Pairs atspair ["index-decimals"]))
    )
    (defun ATS|UR_Syphon:decimal (atspair:string)
        (at "syphon" (read ATS|Pairs atspair ["syphon"]))
    )
    (defun ATS|UR_Syphoning:bool (atspair:string)
        (at "syphoning" (read ATS|Pairs atspair ["syphoning"]))
    )
    (defun ATS|UR_RewardTokens:[object{ATS|RewardTokenSchema}] (atspair:string)
        (at "reward-tokens" (read ATS|Pairs atspair ["reward-tokens"]))
    )
    (defun ATS|UR_ColdRewardBearingToken:string (atspair:string)
        (at "c-rbt" (read ATS|Pairs atspair ["c-rbt"]))
    )
    (defun ATS|UR_ColdNativeFeeRedirection:bool (atspair:string)
        (at "c-nfr" (read ATS|Pairs atspair ["c-nfr"]))
    )
    (defun ATS|UR_ColdRecoveryPositions:integer (atspair:string)
        (at "c-positions" (read ATS|Pairs atspair ["c-positions"]))
    )
    (defun ATS|UR_ColdRecoveryFeeThresholds:[decimal] (atspair:string)
        (at "c-limits" (read ATS|Pairs atspair ["c-limits"]))
    )
    (defun ATS|UR_ColdRecoveryFeeTable:[[decimal]] (atspair:string)
        (at "c-array" (read ATS|Pairs atspair ["c-array"]))
    )
    (defun ATS|UR_ColdRecoveryFeeRedirection:bool (atspair:string)
        (at "c-fr" (read ATS|Pairs atspair ["c-fr"]))
    )
    (defun ATS|UR_ColdRecoveryDuration:[integer] (atspair:string)
        (at "c-duration" (read ATS|Pairs atspair ["c-duration"]))
    )
    (defun ATS|UR_EliteMode:bool (atspair:string)
        (at "c-elite-mode" (read ATS|Pairs atspair ["c-elite-mode"]))
    )
    (defun ATS|UR_HotRewardBearingToken:string (atspair:string)
        (at "h-rbt" (read ATS|Pairs atspair ["h-rbt"]))
    )
    (defun ATS|UR_HotRecoveryStartingFeePromile:decimal (atspair:string)
        (at "h-promile" (read ATS|Pairs atspair ["h-promile"]))
    )
    (defun ATS|UR_HotRecoveryDecayPeriod:integer (atspair:string)
        (at "h-decay" (read ATS|Pairs atspair ["h-decay"]))
    )
    (defun ATS|UR_HotRecoveryFeeRedirection:bool (atspair:string)
        (at "h-fr" (read ATS|Pairs atspair ["h-fr"]))
    )
    (defun ATS|UR_ToggleColdRecovery:bool (atspair:string)
        (at "cold-recovery" (read ATS|Pairs atspair ["cold-recovery"]))
    )
    (defun ATS|UR_ToggleHotRecovery:bool (atspair:string)
        (at "hot-recovery" (read ATS|Pairs atspair ["hot-recovery"]))
    )
    (defun ATS|UR_RewardTokenList:[string] (atspair:string)
        (fold
            (lambda
                (acc:[string] item:object{ATS|RewardTokenSchema})
                (UTILS.LIST|UC_AppendLast acc (at "token" item))
            )
            []
            (ATS|UR_RewardTokens atspair)
        )
    )
    (defun ATS|UR_RoUAmountList:[decimal] (atspair:string rou:bool)
        (fold
            (lambda
                (acc:[decimal] item:object{ATS|RewardTokenSchema})
                (if rou
                    (UTILS.LIST|UC_AppendLast acc (at "resident" item))
                    (UTILS.LIST|UC_AppendLast acc (at "unbonding" item))
                )
            )
            []
            (ATS|UR_RewardTokens atspair)
        )
    )
    (defun ATS|UR_RT-Data (atspair:string reward-token:string data:integer)
        (ATS|UEV_id atspair)
        (UTILS.UTILS|UEV_PositionalVariable data 3 "Invalid Data Integer")
        (let*
            (
                (rt:[object{ATS|RewardTokenSchema}] (ATS|UR_RewardTokens atspair))
                (rtp:integer (ATS|UC_RewardTokenPosition atspair reward-token))
                (rto:object{ATS|RewardTokenSchema} (at rtp rt))
            )
            (cond
                ((= data 1) (at "nfr" rto))
                ((= data 2) (at "resident" rto))
                ((= data 3) (at "unbonding" rto))
                true
            )
        )
    )
    (defun ATS|UR_RtPrecisions:[integer] (atspair:string)
        (fold
            (lambda
                (acc:[integer] rt:string)
                (UTILS.LIST|UC_AppendLast acc (BASIS.DPTF-DPMF|UR_Decimals rt true))
            )
            []
            (ATS|UR_RewardTokenList atspair)
        )
    )
    (defun ATS|UR_P0:[object{Awo}] (atspair:string account:string)
        (at "P0" (read ATS|Ledger (concat [atspair UTILS.BAR account]) ["P0"]))
    )
    (defun ATS|UR_P1-7:object{Awo} (atspair:string account:string position:integer)
        (UTILS.UTILS|UEV_PositionalVariable position 7 "Invalid Position Number")
        (cond
            ((= position 1) (at "P1" (read ATS|Ledger (concat [atspair UTILS.BAR account]) ["P1"])))
            ((= position 2) (at "P2" (read ATS|Ledger (concat [atspair UTILS.BAR account]) ["P2"])))
            ((= position 3) (at "P3" (read ATS|Ledger (concat [atspair UTILS.BAR account]) ["P3"])))
            ((= position 4) (at "P4" (read ATS|Ledger (concat [atspair UTILS.BAR account]) ["P4"])))
            ((= position 5) (at "P5" (read ATS|Ledger (concat [atspair UTILS.BAR account]) ["P5"])))
            ((= position 6) (at "P6" (read ATS|Ledger (concat [atspair UTILS.BAR account]) ["P6"])))
            ((= position 7) (at "P7" (read ATS|Ledger (concat [atspair UTILS.BAR account]) ["P7"])))
            true
        )
    )
    ;;
    (defun ATS|URC_MaxSyphon:[decimal] (atspair:string)
        (let*
            (
                (index:decimal (ATS|UC_Index atspair))
                (syphon:decimal (ATS|UR_Syphon atspair))
                (resident-rt-amounts:[decimal] (ATS|UR_RoUAmountList atspair true))
                (precisions:[integer] (ATS|UR_RtPrecisions atspair))
                (max-precision:integer (UTILS.UTILS|UC_MaxInteger precisions))
                (max-pp:integer (at 0 (UTILS.LIST|UC_Search precisions max-precision)))
                (pair-rbt-supply:decimal (ATS|UCX_PairRBTSupply atspair))
            )
            (if (<= index syphon)
                (make-list (length precisions) 0.0)
                (let*
                    (
                        (index-diff:decimal (- index syphon))
                        (rbt:string (ATS|UR_ColdRewardBearingToken atspair))
                        (rbt-precision:integer (BASIS.DPTF-DPMF|UR_Decimals rbt true))
                        (max-sum:decimal (floor (* pair-rbt-supply index-diff) rbt-precision))
                        (prelim:[decimal]
                            (fold
                                (lambda
                                    (acc:[decimal] idx:integer)
                                    (UTILS.LIST|UC_AppendLast acc 
                                        (floor (/ (* (- index syphon) (at idx resident-rt-amounts)) index) (at idx precisions))
                                    )
                                )
                                []
                                (enumerate 0 (- (length precisions) 1))
                            )
                        )
                        (prelim-sum:decimal (fold (+) 0.0 prelim))
                        (diff:decimal (- max-sum prelim-sum))
                    )
                    (if (= diff 0.0)
                        prelim
                        (UTILS.LIST|UC_ReplaceAt prelim max-pp (+ diff (at max-pp prelim)))
                    )
                )
            )
        )
    )
    (defun ATS|UC_IzCullable:bool (input:object{Awo})
        (let*
            (
                (present-time:time (at "block-time" (chain-data)))
                (stored-time:time (at "cull-time" input))
                (diff:decimal (diff-time present-time stored-time))
            )
            (if (>= diff 0.0)
                true
                false
            )
        )
    )
    (defun ATS|UC_CullValue:[decimal] (atspair:string input:object{Awo})
        (let*
            (
                (rt-lst:[string] (ATS|UR_RewardTokenList atspair))
                (rt-amounts:[decimal] (at "reward-tokens" input))
                (l:integer (length rt-lst))
                (iz:bool (ATS|UC_IzCullable input))
            )
            (if iz
                rt-amounts
                (make-list l 0.0)
            )
        )
    )
    (defun ATS|UC_AccountUnbondingBalance (atspair:string account:string reward-token:string)
        (+
            (fold
                (lambda
                    (acc:decimal item:object{Awo})
                    (+ acc (ATS|UCX_UnstakeObjectUnbondingValue atspair reward-token item))
                )
                0.0
                (ATS|UR_P0 atspair account)
            )
            (fold
                (lambda
                    (acc:decimal item:integer)
                    (+ acc (ATS|UCX_UnstakeObjectUnbondingValue atspair reward-token (ATS|UR_P1-7 atspair account item)))
                )
                0.0
                (enumerate 1 7)
            )
        )
    )
    (defun ATS|UCX_UnstakeObjectUnbondingValue (atspair:string reward-token:string io:object{Awo})
        (let*
            (
                (rtp:integer (ATS|UC_RewardTokenPosition atspair reward-token))
                (rt:[decimal] (at "reward-tokens" io))
                (rb:decimal (at rtp rt))
            )
            (if (= rb -1.0)
                0.0
                rb
            )
        )
    )
    (defun ATS|UC_WhichPosition:integer (atspair:string c-rbt-amount:decimal account:string)
        (let
            (
                (elite:bool (ATS|UR_EliteMode atspair))
            )
            (if elite
                (ATS|UC_ElitePosition atspair c-rbt-amount account)
                (ATS|UC_NonElitePosition atspair account)
            )
        )
    )
    (defun ATS|UC_ElitePosition:integer (atspair:string c-rbt-amount:decimal account:string)
        (let
            (
                (positions:integer (ATS|UR_ColdRecoveryPositions atspair))
                (c-rbt:string (ATS|UR_ColdRewardBearingToken atspair))
                (ea-id:string (DALOS.DALOS|UR_EliteAurynID))
            )
            (if (!= ea-id UTILS.BAR)
                (let*
                    (
                        (iz-ea-id:bool (if (= ea-id c-rbt) true false))
                        (pstate:[integer] (ATS|UC_PSL atspair account))
                        (met:integer (DALOS.DALOS|UR_Elite-Tier-Major account))
                        (ea-supply:decimal (BASIS.DPTF-DPMF|UR_AccountSupply ea-id account true))
                        (t-ea-supply:decimal (BASIS.DALOS|URC_EliteAurynzSupply account))
                        (virtual-met:integer (str-to-int (take 1 (at "tier" (UTILS.ATS|UCC_Elite (- t-ea-supply c-rbt-amount))))))
                        (available:[integer] (if iz-ea-id (take virtual-met pstate) (take met pstate)))
                        (search-res:[integer] (UTILS.LIST|UC_Search available 1))
                    )
                    (if iz-ea-id
                        (enforce (<= c-rbt-amount ea-supply) "Amount of EA used for Cold Recovery cannot be greater than what exists on Account")
                        true
                    )
                    (if (= (length search-res) 0)
                        0
                        (+ (at 0 search-res) 1)
                    )
                )
                (ATS|UC_NonElitePosition atspair account)
            )
        )
    )
    (defun ATS|UC_NonElitePosition:integer (atspair:string account:string)
        (let
            (
                (positions:integer (ATS|UR_ColdRecoveryPositions atspair))
            )
            (if (= positions -1)
                -1
                (let*
                    (
                        (pstate:[integer] (ATS|UC_PSL atspair account))
                        (available:[integer] (take positions pstate))
                        (search-res:[integer] (UTILS.LIST|UC_Search available 1))
                    )
                    (if (= (length search-res) 0)
                        0
                        (+ (at 0 search-res) 1)
                    )
                )
            )
        )
    )
    (defun ATS|UC_PSL:[integer] (atspair:string account:string)
        (let
            (
                (p1s:integer (ATS|UC_PositionState atspair account 1))
                (p2s:integer (ATS|UC_PositionState atspair account 2))
                (p3s:integer (ATS|UC_PositionState atspair account 3))
                (p4s:integer (ATS|UC_PositionState atspair account 4))
                (p5s:integer (ATS|UC_PositionState atspair account 5))
                (p6s:integer (ATS|UC_PositionState atspair account 6))
                (p7s:integer (ATS|UC_PositionState atspair account 7))
            )
            [p1s p2s p3s p4s p5s p6s p7s]
        )
    )
    (defun ATS|UC_PositionState:integer (atspair:string account:string position:integer)
        (UTILS.UTILS|UEV_PositionalVariable position 7 "Input Position out of bounds")
        (with-read ATS|Ledger (concat [atspair UTILS.BAR account])
            { "P1"  := p1 , "P2"  := p2 , "P3"  := p3, "P4"  := p4, "P5"  := p5, "P6"  := p6, "P7"  := p7 }
            (let
                (
                    (ps1:integer (ATS|UC_PositionalObjectState atspair p1))
                    (ps2:integer (ATS|UC_PositionalObjectState atspair p2))
                    (ps3:integer (ATS|UC_PositionalObjectState atspair p3))
                    (ps4:integer (ATS|UC_PositionalObjectState atspair p4))
                    (ps5:integer (ATS|UC_PositionalObjectState atspair p5))
                    (ps6:integer (ATS|UC_PositionalObjectState atspair p6))
                    (ps7:integer (ATS|UC_PositionalObjectState atspair p7))
                )
                (cond
                    ((= position 1) ps1)
                    ((= position 2) ps2)
                    ((= position 3) ps3)
                    ((= position 4) ps4)
                    ((= position 5) ps5)
                    ((= position 6) ps6)
                    ps7
                )
            )
        )
    )
    (defun ATS|UC_PositionalObjectState:integer (atspair:string input-obj:object{Awo})
        @doc "occupied(0), opened(1), closed (-1)"
        (let
            (
                (zero:object{Awo} (ATS|UCC_MakeZeroUnstakeObject atspair))
                (negative:object{Awo} (ATS|UCC_MakeNegativeUnstakeObject atspair))
            )
            (if (= input-obj zero)
                1
                (if (= input-obj negative)
                    -1
                    0
                )
            )
        )
    )
    (defun ATS|UC_ColdRecoveryFee (atspair:string c-rbt-amount:decimal input-position:integer)
        (let*
            (
                (ats-positions:integer (ATS|UR_ColdRecoveryPositions atspair))
                (ats-limit-values:[decimal] (ATS|UR_ColdRecoveryFeeThresholds atspair))
                (ats-limits:integer (length ats-limit-values))
                (ats-fee-array:[[decimal]] (ATS|UR_ColdRecoveryFeeTable atspair))
                (ats-fee-array-length:integer (length ats-fee-array))
                (ats-fee-array-length-length:integer (length (at 0 ats-fee-array)))
                (zc1:bool (if (= ats-limits 1) true false))
                (zc2:bool (if (= (at 0 ats-limit-values) 0.0) true false))
                (zc3:bool (and zc1 zc2))
                (zc4:bool (if (= ats-fee-array-length 1) true false))
                (zc5:bool (if (= ats-fee-array-length-length 1) true false))
                (zc6:bool (and zc4 zc5))
                (zc7:bool (if (= (at 0 (at 0 ats-fee-array)) 0.0) true false))
                (zc8:bool (and zc6 zc7))
                (zc9:bool (and zc3 zc8))
            )
            (enforce (<= input-position ats-positions) "Input position out of bounds")
            (if zc9
                0.0
                (let
                    (
                        (limit:integer
                            (fold
                                (lambda
                                    (acc:integer tv:decimal)
                                    (if (< c-rbt-amount tv)
                                        acc
                                        (+ acc 1)
                                    )
                                )
                                0
                                ats-limit-values
                            )
                        )
                        (qlst:[decimal] 
                            (if (= input-position -1)
                                (at 0 ats-fee-array)
                                (at (- input-position 1) ats-fee-array)
                            )
                        )
                    )
                    (at limit qlst)
                )
            )
        )
    )
    (defun ATS|UC_CullColdRecoveryTime:time (atspair:string account:string)
        (let*
            (
                (major:integer (DALOS.DALOS|UR_Elite-Tier-Major account))
                (minor:integer (DALOS.DALOS|UR_Elite-Tier-Minor account))
                (position:integer 
                    (if (= major 0)
                        0
                        (+ (* (- major 1) 7) minor)
                    )
                )
                (crd:[integer] (ATS|UR_ColdRecoveryDuration atspair))
                (h:integer (at position crd))
                (present-time:time (at "block-time" (chain-data)))
            )
            (add-time present-time (hours h))
        )
    )
    (defun ATS|UC_RTSplitAmounts:[decimal] (atspair:string rbt-amount:decimal)
        (let
            (
                (rbt-supply:decimal (ATS|UCX_PairRBTSupply atspair))
                (index:decimal (ATS|UC_Index atspair))
                (resident-amounts:[decimal] (ATS|UR_RoUAmountList atspair true))
                (rt-precision-lst:[integer] (ATS|UR_RtPrecisions atspair))
            )
            (enforce (<= rbt-amount rbt-supply) "Cannot compute for amounts greater than the pairs rbt supply")
            (UTILS.ATS|UC_SplitByIndexedRBT rbt-amount rbt-supply index resident-amounts rt-precision-lst)
        )
    )
    (defun ATS|UC_Index (atspair:string)
        (let
            (
                (p:integer (ATS|UR_IndexDecimals atspair))
                (rs:decimal (ATS|UC_ResidentSum atspair))
                (rbt-supply:decimal (ATS|UCX_PairRBTSupply atspair))
            )
            (if
                (= rbt-supply 0.0)
                -1.0
                (floor (/ rs rbt-supply) p)
            )
        )
    )
    (defun ATS|UCX_PairRBTSupply:decimal (atspair:string)
        (let*
            (
                (c-rbt:string (ATS|UR_ColdRewardBearingToken atspair))
                (c-rbt-supply:decimal (BASIS.DPTF-DPMF|UR_Supply c-rbt true))
            )
            (if (= (ATS|UC_IzPresentHotRBT atspair) false)
                c-rbt-supply
                (let*
                    (
                        (h-rbt:string (ATS|UR_HotRewardBearingToken atspair))
                        (h-rbt-supply:decimal (BASIS.DPTF-DPMF|UR_Supply h-rbt false))
                    )
                    (+ c-rbt-supply h-rbt-supply)
                )
            )
        )
    )
    (defun ATS|UC_RBT:decimal (atspair:string rt:string rt-amount:decimal)
        (let*
            (
                (index:decimal (abs (ATS|UC_Index atspair)))
                (c-rbt:string (ATS|UR_ColdRewardBearingToken atspair))
                (p-rbt:integer (BASIS.DPTF-DPMF|UR_Decimals c-rbt true))
            )
            (enforce
                (= (floor rt-amount p-rbt) rt-amount)
                (format "The entered amount of {} must have at most, the precision c-rbt token {} which is {}" [rt-amount c-rbt p-rbt])
            )
            (floor (/ rt-amount index) p-rbt)
        )
    )
    (defun ATS|UC_ResidentSum:decimal (atspair:string)
        (fold (+) 0.0 (ATS|UR_RoUAmountList atspair true)) 
    )
    (defun ATS|UC_IzPresentHotRBT:bool (atspair:string)
        (if (= (ATS|UR_HotRewardBearingToken atspair) UTILS.BAR)
            false
            true
        )
    )
    (defun ATS|UC_RewardTokenPosition:integer (atspair:string reward-token:string)
        (let
            (
                (existance-check:bool (BASIS.ATS|UC_IzRTg atspair reward-token))
            )
            (enforce (= existance-check true) (format "{} Existance isnt verified for Token {} as RT with ATS Pair {}" [true reward-token atspair]))
            (at 0 (UTILS.LIST|UC_Search (ATS|UR_RewardTokenList atspair) reward-token))
        )
    )
    (defun ATS|UC_ZeroColdFeeExceptionBoolean:bool (fee-thresholds:[decimal] fee-array:[[decimal]])
        (not 
            (UTILS.UTILS|UC_TripleAnd
                (= (length fee-thresholds) 1)
                (= (at 0 fee-thresholds) 0.0)
                (UTILS.UTILS|UC_TripleAnd
                    (= (length fee-array) 1)
                    (= (length (at 0 fee-array)) 1)
                    (= (at 0 (at 0 fee-array)) 0.0)
                )
            )
        )
    )
    ;;CPF-Computers
    
    
    
    (defun ATS|UC_SolidifyUO:object{Awo} (input:object{Awo} remove-position:integer)
        (let*
            (
                (values:[decimal] (at "reward-tokens" input))
                (cull-time:time (at "cull-time" input))
                (how-many-rts:integer (length values))
            )
            (enforce (and (> remove-position 0) (< remove-position how-many-rts)) "Invalid <remove-position>")
            (let*
                (
                    (primal:decimal (at 0 (at "reward-tokens" input)))
                    (removee:decimal (at remove-position (at "reward-tokens" input)))
                    (remove-lst:[decimal] (UTILS.LIST|UC_RemoveItemAt values remove-position))
                    (new-values:[decimal] (UTILS.LIST|UC_ReplaceAt remove-lst 0 (+ primal removee)))
                )
                { "reward-tokens"   : new-values
                , "cull-time"       : cull-time}
            )
        )
    )
    (defun ATS|UC_IzUnstakeObjectValid:bool (input:object{Awo})
        (let*
            (
                (values:[decimal] (at "reward-tokens" input))
                (sum-values:decimal (fold (+) 0.0 values))
            )
            (if (> sum-values 0.0)
                true
                false
            )
        )
    )
    (defun ATS|UC_ReshapeUO:object{Awo} (input:object{Awo} remove-position:integer)
        (let
            (
                (is-valid:bool (ATS|UC_IzUnstakeObjectValid input))
            )
            (if is-valid
                (ATS|UC_SolidifyUO input remove-position)
                input
            )
        )
    )
    (defun ATS|UC_MultiReshapeUO:[object{Awo}] (input:[object{Awo}] remove-position:integer)
        (fold
            (lambda
                (acc:[object{Awo}] item:object{Awo})
                (UTILS.LIST|UC_AppendLast acc (ATS|UC_ReshapeUO item remove-position))
            )
            []
            input
        )
    )
    ;;
    (defun ATS|UCC_MakeUnstakeObject:object{Awo} (atspair:string time:time)
        { "reward-tokens"   : (make-list (length (ATS|UR_RewardTokenList atspair)) 0.0)
        , "cull-time"       : time}
    )
    (defun ATS|UCC_MakeZeroUnstakeObject:object{Awo} (atspair:string)
        (ATS|UCC_MakeUnstakeObject atspair NULLTIME)
    )
    (defun ATS|UCC_MakeNegativeUnstakeObject:object{Awo} (atspair:string)
        (ATS|UCC_MakeUnstakeObject atspair ANTITIME)
    )
    (defun ATS|UCC_ComposePrimaryRewardToken:object{ATS|RewardTokenSchema} (token:string nfr:bool)
        (ATS|UCC_RT token nfr 0.0 0.0)
    )
    (defun ATS|UCC_RT:object{ATS|RewardTokenSchema} (token:string nfr:bool r:decimal u:decimal)
        (enforce (>= r 0.0) "Negative Resident not allowed")
        (enforce (>= u 0.0) "Negative Unbonding not allowed")
        {"token"                    : token
        ,"nfr"                      : nfr
        ,"resident"                 : r
        ,"unbonding"                : u}
    )
    ;;[X]
    (defun ATS|X_ChangeOwnership (atspair:string new-owner:string)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_OWNERSHIP_CHANGE atspair new-owner)
            (update ATS|Pairs atspair
                {"owner-konto"                      : new-owner}
            )
        )
    )
    (defun ATS|X_ToggleParameterLock:[decimal] (atspair:string toggle:bool)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_TOGGLE_PARAMETER-LOCK atspair toggle)
            (update ATS|Pairs atspair
                { "parameter-lock" : toggle}
            )
            (if (= toggle true)
                [0.0 0.0]
                (UTILS.ATS|UC_UnlockPrice (ATS|UR_Unlocks atspair))
            )
        )
    )
    (defun ATS|X_IncrementParameterUnlocks (atspair:string)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-read ATS|Pairs atspair
            { "unlocks" := u }
            (update ATS|Pairs atspair
                {"unlocks" : (+ u 1)}
            )
        )
    )
    (defun ATS|X_UpdateSyphon (atspair:string syphon:decimal)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_SYPHON atspair syphon)
            (update ATS|Pairs atspair
                {"syphon"                           : syphon}
            )
        )
    )
    (defun ATS|X_ToggleSyphoning (atspair:string toggle:bool)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_SYPHONING atspair toggle)
            (update ATS|Pairs atspair
                {"syphoning"                        : toggle}
            )
        )
    )
    (defun ATS|X_ToggleFeeSettings (atspair:string toggle:bool fee-switch:integer)
        (enforce-guard (C_ReadPolicy "ATSI|Caller"))
        (with-capability (ATS|X_TOGGLE_FEE atspair toggle fee-switch)
            (if (= fee-switch 0)
                (update ATS|Pairs atspair
                    { "c-nfr" : toggle}
                )
                (if (= fee-switch 1)
                    (update ATS|Pairs atspair
                        { "c-fr" : toggle}
                    )
                    (update ATS|Pairs atspair
                        { "h-fr" : toggle}
                    )
                )
            )
        )
        
    )
    (defun ATS|X_SetCRD (atspair:string soft-or-hard:bool base:integer growth:integer)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_SET_CRD atspair soft-or-hard base growth)
            (if (= soft-or-hard true)
                (update ATS|Pairs atspair
                    { "c-duration" : (UTILS.ATS|UC_MakeSoftIntervals base growth)}
                )
                (update ATS|Pairs atspair
                    { "c-duration" : (UTILS.ATS|UC_MakeHardIntervals base growth)}
                )
            )
        )
    )
    (defun ATS|X_SetColdFee (atspair:string fee-positions:integer fee-thresholds:[decimal] fee-array:[[decimal]])
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_SET_COLD_FEE atspair fee-positions fee-thresholds fee-array)
            (update ATS|Pairs atspair
                { "c-positions"     : fee-positions
                , "c-limits"        : fee-thresholds 
                , "c-array"         : fee-array}
            )
        )
    )
    (defun ATS|X_SetHotFee (atspair:string promile:decimal decay:integer)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_SET_HOT_FEE atspair promile decay)
            (update ATS|Pairs atspair
                { "h-promile"       : promile
                , "h-decay"         : decay}
            )
        )
    )
    (defun ATS|X_ToggleElite (atspair:string toggle:bool)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_TOGGLE_ELITE atspair toggle)
            (update ATS|Pairs atspair
                { "c-elite-mode" : toggle}
            )
        )
    )
    (defun ATS|X_TurnRecoveryOn (atspair:string cold-or-hot:bool)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_RECOVERY-ON atspair cold-or-hot)
            (if (= cold-or-hot true)
                (update ATS|Pairs atspair
                    { "cold-recovery" : true}
                )
                (update ATS|Pairs atspair
                    { "hot-recovery" : true}
                )
            )
        )
    )
    (defun ATS|X_TurnRecoveryOff (atspair:string cold-or-hot:bool)
        (enforce-guard (C_ReadPolicy "ATSI|Caller"))
        (with-capability (ATS|X_RECOVERY-OFF atspair cold-or-hot)
            (if (= cold-or-hot true)
                (update ATS|Pairs atspair
                    { "cold-recovery" : false}
                )
                (update ATS|Pairs atspair
                    { "hot-recovery" : false}
                )
            )
        )
        
    )
    (defun ATS|X_InsertNewATSPair (account:string atspair:string index-decimals:integer reward-token:string rt-nfr:bool reward-bearing-token:string rbt-nfr:bool)
        (enforce-guard (C_ReadPolicy "ATSI|Caller"))
        (insert ATS|Pairs (UTILS.DALOS|UCC_Makeid atspair)
            {"owner-konto"                          : account
            ,"can-change-owner"                     : true
            ,"parameter-lock"                       : false
            ,"unlocks"                              : 0

            ,"pair-index-name"                      : atspair
            ,"index-decimals"                       : index-decimals
            ,"syphon"                               : 1.0
            ,"syphoning"                            : false

            ,"reward-tokens"                        : [(ATS|UCC_ComposePrimaryRewardToken reward-token rt-nfr)]

            ,"c-rbt"                                : reward-bearing-token
            ,"c-nfr"                                : rbt-nfr
            ,"c-positions"                          : -1
            ,"c-limits"                             : [0.0]
            ,"c-array"                              : [[0.0]]
            ,"c-fr"                                 : true
            ,"c-duration"                           : (UTILS.ATS|UC_MakeSoftIntervals 300 6)
            ,"c-elite-mode"                         : false

            ,"h-rbt"                                : UTILS.BAR
            ,"h-promile"                            : 100.0
            ,"h-decay"                              : 1
            ,"h-fr"                                 : true

            ,"cold-recovery"                        : false
            ,"hot-recovery"                         : false
            }
        )
    )
    (defun ATS|X_AddSecondary (atspair:string reward-token:string rt-nfr:bool)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_ADD_SECONDARY atspair reward-token true)
            (with-read ATS|Pairs atspair
                { "reward-tokens" := rt }
                (update ATS|Pairs atspair
                    {"reward-tokens" : (UTILS.LIST|UC_AppendLast rt (ATS|UCC_ComposePrimaryRewardToken reward-token rt-nfr))}
                )
            )
        )
    )
    (defun ATS|X_AddHotRBT (atspair:string hot-rbt:string)
        (enforce-guard (C_ReadPolicy "ATSM|Caller"))
        (with-capability (ATS|X_ADD_SECONDARY atspair hot-rbt false)
            (update ATS|Pairs atspair
                {"h-rbt" : hot-rbt}
            )
        )
    )
    (defun ATS|XO_ReshapeUnstakeAccount (atspair:string account:string rp:integer)
        (enforce-guard (C_ReadPolicy "ATSM|ReshapeUnstakeAccount"))
        (with-capability (ATS|RESHAPE)
            (ATS|XP_ReshapeUnstakeAccount atspair account rp)
        )
    )
    (defun ATS|XP_ReshapeUnstakeAccount (atspair:string account:string rp:integer)
        (require-capability (ATS|RESHAPE))
        (with-read ATS|Ledger (concat [atspair UTILS.BAR account])
            { "P0"      := p0 
            , "P1"      := p1
            , "P2"      := p2
            , "P3"      := p3
            , "P4"      := p4
            , "P5"      := p5
            , "P6"      := p6
            , "P7"      := p7}
            (update ATS|Ledger (concat [atspair UTILS.BAR account])
                { "P0"  : (ATS|UC_MultiReshapeUO p0 rp)
                , "P1"  : (ATS|UC_ReshapeUO p1 rp)
                , "P2"  : (ATS|UC_ReshapeUO p2 rp)
                , "P3"  : (ATS|UC_ReshapeUO p3 rp)
                , "P4"  : (ATS|UC_ReshapeUO p4 rp)
                , "P5"  : (ATS|UC_ReshapeUO p5 rp)
                , "P6"  : (ATS|UC_ReshapeUO p6 rp)
                , "P7"  : (ATS|UC_ReshapeUO p7 rp)}
            )
        )
    )
    (defun ATS|XO_RemoveSecondary (atspair:string reward-token:string)
        (enforce-guard (C_ReadPolicy "ATSM|RemoveSecondaryRT"))
        (with-capability (ATS|RM_SECONDARY_RT)
            (ATS|XP_RemoveSecondary atspair reward-token)
        )
    )
    (defun ATS|XP_RemoveSecondary (atspair:string reward-token:string)
        (require-capability (ATS|RM_SECONDARY_RT))
        (with-read ATS|Pairs atspair
            { "reward-tokens" := rt }
            (update ATS|Pairs atspair
                {"reward-tokens" : 
                    (UTILS.LIST|UC_RemoveItem  rt (at (ATS|UC_RewardTokenPosition atspair reward-token) rt))
                }
            )
        )
    )
    (defun ATS|XO_UpdateRoU (atspair:string reward-token:string rou:bool direction:bool amount:decimal)
        (enforce-one
            "Update RoU not allowed"
            [
                (enforce-guard (create-capability-guard (ATS|UPDATE_ROU)))
                (enforce-guard (C_ReadPolicy "TFT|UpdateROU"))
                (enforce-guard (C_ReadPolicy "ATSC|UpdateROU"))
                (enforce-guard (C_ReadPolicy "ATSH|UpdateROU"))
                (enforce-guard (C_ReadPolicy "ATSM|UpdateROU"))
            ]
        )
        (let*
            (
                (rtp:integer (ATS|UC_RewardTokenPosition atspair reward-token))
                (nfr:bool (ATS|UR_RT-Data atspair reward-token 1))
                (resident:decimal (ATS|UR_RT-Data atspair reward-token 2))
                (unbonding:decimal (ATS|UR_RT-Data atspair reward-token 3))
                (new-rt:object{ATS|RewardTokenSchema} 
                    (if (= rou true)
                        (if (= direction true)
                            (ATS|UCC_RT reward-token nfr (+ resident amount) unbonding)
                            (ATS|UCC_RT reward-token nfr (- resident amount) unbonding)
                        )
                        (if (= direction true)
                            (ATS|UCC_RT reward-token nfr resident (+ unbonding amount))
                            (ATS|UCC_RT reward-token nfr resident (- unbonding amount))
                        )
                    )
                )
            )
            (with-read ATS|Pairs atspair
                { "reward-tokens" := rt }
                (update ATS|Pairs atspair
                    { "reward-tokens" : (UTILS.LIST|UC_ReplaceItem rt (at rtp rt) new-rt)}
                )
            )
        )
    )
    (defun X_UpP0 (atspair:string account:string obj:[object{Awo}])
        (enforce-guard (C_ReadPolicy "ATSC|UpUnsPos"))
        (update ATS|Ledger (concat [atspair UTILS.BAR account])
            { "P0" : obj}
        )
    )
    (defun X_UpP1 (atspair:string account:string obj:object{Awo})
        (enforce-guard (C_ReadPolicy "ATSC|UpUnsPos"))
        (update ATS|Ledger (concat [atspair UTILS.BAR account])
            { "P1"  : obj}
        )
    )
    (defun X_UpP2 (atspair:string account:string obj:object{Awo})
        (enforce-guard (C_ReadPolicy "ATSC|UpUnsPos"))
        (update ATS|Ledger (concat [atspair UTILS.BAR account])
            { "P2"  : obj}
        )
    )
    (defun X_UpP3 (atspair:string account:string obj:object{Awo})
        (enforce-guard (C_ReadPolicy "ATSC|UpUnsPos"))
        (update ATS|Ledger (concat [atspair UTILS.BAR account])
            { "P3"  : obj}
        )
    )
    (defun X_UpP4 (atspair:string account:string obj:object{Awo})
        (enforce-guard (C_ReadPolicy "ATSC|UpUnsPos"))
        (update ATS|Ledger (concat [atspair UTILS.BAR account])
            { "P4"  : obj}
        )
    )
    (defun X_UpP5 (atspair:string account:string obj:object{Awo})
        (enforce-guard (C_ReadPolicy "ATSC|UpUnsPos"))
        (update ATS|Ledger (concat [atspair UTILS.BAR account])
            { "P5"  : obj}
        )
    )
    (defun X_UpP6 (atspair:string account:string obj:object{Awo})
        (enforce-guard (C_ReadPolicy "ATSC|UpUnsPos"))
        (update ATS|Ledger (concat [atspair UTILS.BAR account])
            { "P6"  : obj}
        )
    )
    (defun X_UpP7 (atspair:string account:string obj:object{Awo})
        (enforce-guard (C_ReadPolicy "ATSC|UpUnsPos"))
        (update ATS|Ledger (concat [atspair UTILS.BAR account])
            { "P7"  : obj}
        )
    )
    (defun ATS|X_SpawnAutostakeAccount (atspair:string account:string)
        (enforce-guard (C_ReadPolicy "ATSC|Caller"))
        (let
            (
                (zero:object{Awo} (ATS|UCC_MakeZeroUnstakeObject atspair))
                (negative:object{Awo} (ATS|UCC_MakeNegativeUnstakeObject atspair))
            )
            (with-default-read ATS|Ledger (concat [atspair UTILS.BAR account])
                { "P0"  : [zero]
                , "P1"  : negative
                , "P2"  : negative
                , "P3"  : negative
                , "P4"  : negative
                , "P5"  : negative
                , "P6"  : negative
                , "P7"  : negative
                }
                { "P0"  := p0
                , "P1"  := p1
                , "P2"  := p2
                , "P3"  := p3
                , "P4"  := p4
                , "P5"  := p5
                , "P6"  := p6
                , "P7"  := p7
                }
                (write ATS|Ledger (concat [atspair UTILS.BAR account])
                    { "P0"  : p0
                    , "P1"  : p1
                    , "P2"  : p2
                    , "P3"  : p3
                    , "P4"  : p4
                    , "P5"  : p5
                    , "P6"  : p6
                    , "P7"  : p7
                    }
                )
            )
        )
    )
)
