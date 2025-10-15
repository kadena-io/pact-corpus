(module LIQUID GOVERNANCE
    (defcap GOVERNANCE ()
        (compose-capability (LIQUID-ADMIN))
    )
    (defcap LIQUID-ADMIN ()
        (enforce-one
            "Liquid Admin not satisfed"
            [
                (enforce-guard G-MD_LIQUID)
                (enforce-guard G-SC_LIQUID)
            ]
        )
    )

    (defconst G-MD_LIQUID   (keyset-ref-guard DALOS.DALOS|DEMIURGOI))
    (defconst G-SC_LIQUID   (keyset-ref-guard LIQUID|SC_KEY))

    (defconst LIQUID|SC_KEY
        (+ UTILS.NS_USE ".dh_sc_kadenaliquidstaking-keyset")
    )
    (defconst LIQUID|SC_NAME DALOS.LIQUID|SC_NAME)
    (defconst LIQUID|SC_KDA-NAME (create-principal LIQUID|GUARD))

    (defcap SUMMONER ()
        true
    )
    (defcap SECURE ()
        true
    )
    
    ;;Policies

    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (LIQUID-ADMIN)
            (write PoliciesTable policy-name
                {"policy" : policy-guard}
            )
        )
    )
    (defun C_ReadPolicy:guard (policy-name:string)
        @doc "Reads the guard of a stored policy"
        (at "policy" (read PoliciesTable policy-name ["policy"]))
    )

    (defun DefinePolicies ()
        @doc "Add the Policy that allows running external Functions from this Module"                
        (DALOS.A_AddPolicy
            "LIQUID|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (BASIS.A_AddPolicy
            "LIQUID|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (ATS.A_AddPolicy
            "LIQUID|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (TFT.A_AddPolicy
            "LIQUID|Summoner"
            (create-capability-guard (SUMMONER))
        )
    )

    (defcap LIQUID|GOV ()
        true
    )
    (defun LIQUID|SetGovernor (patron:string)
        (with-capability (SUMMONER)
            (DALOS.DALOS|C_RotateGovernor
                patron
                LIQUID|SC_NAME
                (create-capability-guard (LIQUID|GOV))
            )
        )
    )
    (defcap LIQUID|NATIVE-AUTOMATIC ()
        true
    )
    (defconst LIQUID|GUARD (create-capability-guard (LIQUID|NATIVE-AUTOMATIC)))

    (deftable PoliciesTable:{DALOS.PolicySchema})

    ;;LIQUID
    (defun LIQUID|CAP_IzLiquidStakingLive ()
        (let
            (
                (w-kda:string (DALOS.DALOS|UR_WrappedKadenaID))
                (l-kda:string (DALOS.DALOS|UR_LiquidKadenaID))
            )
            (enforce (!= w-kda UTILS.BAR) "Wrapped-Kadena is not set")
            (enforce (!= l-kda UTILS.BAR) "Liquid-Kadena is not set")
            (let
                (
                    (w-kda-as-rt:[string] (BASIS.DPTF|UR_RewardToken w-kda))
                    (l-kda-as-rbt:[string] (BASIS.DPTF|UR_RewardBearingToken l-kda))
                )
                (enforce (= (length w-kda-as-rt) 1) "Wrapped-Kadena cannot ever be part of another ATS-Pair")
                (enforce (= (length l-kda-as-rbt) 1) "Liquid-Kadena cannot ever be part of another ATS-Pair")
                (enforce (= (at 0 w-kda-as-rt) (at 0 l-kda-as-rbt)) "Wrappedn and Liquid Kadena are not part of the same ASTS Pair")
            )
        )
    )
    ;;
    (defcap LIQUID|CF|LIVE ()
        @doc "Capability that enforces Liquid Staking is live with an existing Autostake Pair"
        (LIQUID|CAP_IzLiquidStakingLive)
    )
    ;;
    (defcap LIQUID|WRAP ()
        @doc "Capability needed to wrap KDA to DWK"
        @event
        (compose-capability (LIQUID|GOV))
        (compose-capability (SUMMONER))
        (compose-capability (LIQUID|CF|LIVE))
    )
    (defcap LIQUID|UNWRAP ()
        @doc "Capability needed to unwrap KDA to DWK"
        @event
        (compose-capability (LIQUID|GOV))
        (compose-capability (LIQUID|CF|LIVE))
        (compose-capability (LIQUID|NATIVE-AUTOMATIC))
    )
    ;;
    (defun LIQUID|C_WrapKadena (patron:string wrapper:string amount:decimal)
        (enforce-one
            "Update RoU not allowed"
            [
                (enforce-guard (C_ReadPolicy "OUROBOROS|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
            ]
        )
        (with-capability (LIQUID|WRAP)
            (let
                (
                    (kadena-patron:string (DALOS.DALOS|UR_AccountKadena wrapper))
                    (w-kda-id:string (DALOS.DALOS|UR_WrappedKadenaID))
                )
                (DALOS.DALOS|C_TransferDalosFuel kadena-patron LIQUID|SC_KDA-NAME amount)
                (BASIS.DPTF|C_Mint patron w-kda-id LIQUID|SC_NAME amount false)
                (TFT.DPTF|C_Transfer patron w-kda-id LIQUID|SC_NAME wrapper amount true)
            )
        )
    )
    (defun LIQUID|C_UnwrapKadena (patron:string unwrapper:string amount:string)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (LIQUID|UNWRAP)
            (let
                (
                    (kadena-patron:string (DALOS.DALOS|UR_AccountKadena unwrapper))
                    (w-kda-id:string (DALOS.DALOS|UR_WrappedKadenaID))
                )
                (TFT.DPTF|C_Transfer patron w-kda-id unwrapper LIQUID|SC_NAME amount true)
                (BASIS.DPTF|C_Burn patron w-kda-id)
                (DALOS.DALOS|C_TransferDalosFuel LIQUID|SC_KDA-NAME kadena-patron amount)
            )
        )
    )
)
