(module OUROBOROS GOVERNANCE
    (defcap GOVERNANCE ()
        (compose-capability (OUROBOROS-ADMIN))
    )
    (defcap OUROBOROS-ADMIN ()
        (enforce-one
            "Ouroboros Admin not satisfed"
            [
                (enforce-guard G-MD_OUROBOROS)
                (enforce-guard G-SC_OUROBOROS)
            ]
        )
    )

    (defconst G-MD_OUROBOROS (keyset-ref-guard DALOS.DALOS|DEMIURGOI))
    (defconst G-SC_OUROBOROS (keyset-ref-guard OUROBOROS|SC_KEY))

    (defconst OUROBOROS|SC_KEY
        (+ UTILS.NS_USE ".dh_sc_ouroboros-keyset")
    )
    (defconst OUROBOROS|SC_NAME DALOS.OUROBOROS|SC_NAME)
    (defconst OUROBOROS|SC_KDA-NAME (create-principal OUROBOROS|GUARD))

    (defcap COMPOSE ()
        true
    )
    (defcap SECURE ()
        true
    )
    (defcap SUMMONER ()
        true
    )
    ;;
    (defun DefinePolicies ()
        (DALOS.A_AddPolicy
            "OUROBOROS|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (BASIS.A_AddPolicy
            "OUROBOROS|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (TFT.A_AddPolicy
            "OUROBOROS|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (ATSM.A_AddPolicy
            "OUROBOROS|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (LIQUID.A_AddPolicy
            "OUROBOROS|Summoner"
            (create-capability-guard (SUMMONER))
        )
    )
    ;;
    (defcap OUROBOROS|GOV ()
        @doc "Autostake Module Governor Capability for its Smart DALOS Account"
        true
    )
    (defun OUROBOROS|SetGovernor (patron:string)
        (with-capability (SUMMONER)
            (DALOS.DALOS|C_RotateGovernor
                patron
                OUROBOROS|SC_NAME
                (create-capability-guard (OUROBOROS|GOV))
            )
        )
    )
    (defcap OUROBOROS|NATIVE-AUTOMATIC ()
        @doc "Capability needed for auto management of the <kadena-konto> associated with the Ouroboros Smart DALOS Account"
        true
    )
    (defconst OUROBOROS|GUARD (create-capability-guard (OUROBOROS|NATIVE-AUTOMATIC)))
    ;;P
    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (OUROBOROS-ADMIN)
            (write PoliciesTable policy-name
                {"policy" : policy-guard}
            )
        )
    )
    (defun C_ReadPolicy:guard (policy-name:string)
        (at "policy" (read PoliciesTable policy-name ["policy"]))
    )
    ;;
    (defconst NULLTIME (time "1984-10-11T11:10:00Z"))
    (defconst ANTITIME (time "1983-08-07T11:10:00Z"))
    ;;
    (defschema VST|MetaDataSchema
        release-amount:decimal
        release-date:time
    )
    (deftable PoliciesTable:{DALOS.PolicySchema})
    ;;
    ;;[UR] & [UC]
    (defun DPTF-DPMF-ATS|UR_OwnedTokens (account:string table-to-query:integer)
        (UTILS.UTILS|UEV_PositionalVariable table-to-query 3 "Invalid Ownership Position")
        (let*
            (
                (keyz:[string] (TFT.DPTF-DPMF-ATS|UR_TableKeys table-to-query true))
                (owners-lst:[string]
                    (fold
                        (lambda
                            (acc:[string] item:string)
                            (UTILS.LIST|UC_AppendLast 
                                acc
                                (if (= table-to-query 1)
                                    (BASIS.DPTF-DPMF|UR_Konto item true)
                                    (if (= table-to-query 2)
                                        (BASIS.DPTF-DPMF|UR_Konto item false)
                                        (ATS.ATS|UR_OwnerKonto item)
                                    )
                                )
                            )
                        )
                        []
                        keyz
                    )
                )
                (owner-pos:[integer] (UTILS.LIST|UC_Search owners-lst account))
            )
            (fold
                (lambda
                    (acc:[string] idx:integer)
                    (UTILS.LIST|UC_AppendLast acc (at (at idx owner-pos) keyz))
                )
                []
                (enumerate 0 (- (length owner-pos) 1))
            )
            
        )
    )
    (defun ATS|UC_RT-Unbonding (atspair:string reward-token:string)
        (ATS.ATS|UEV_RewardTokenExistance atspair reward-token true)
        (fold
            (lambda
                (acc:decimal account:string)
                (+ acc (ATS.ATS|UC_AccountUnbondingBalance atspair account reward-token))
            )
            0.0
            (TFT.DPTF-DPMF-ATS|UR_FilterKeysForInfo atspair 3 false)
        )
    )
    ;;OUROBOROS
    (defun DALOS|UEV_Live ()
        (let
            (
                (ouro-id:string (DALOS.DALOS|UR_OuroborosID))
                (gas-id:string (DALOS.DALOS|UR_IgnisID))
            )
            (enforce (!= ouro-id UTILS.BAR) "Ouroboros is not set")
            (enforce (!= gas-id UTILS.BAR) "Ignis is not set")
        )
    )
    (defun IGNIS|UEV_Exchange ()
        (DALOS|UEV_Live)
        (let*
            (
                (ouro-id:string (DALOS.DALOS|UR_OuroborosID))
                (gas-id:string (DALOS.DALOS|UR_IgnisID))
                (o-rm:bool (BASIS.DPTF|UR_AccountRoleMint ouro-id OUROBOROS|SC_NAME))
                (o-rb:bool (BASIS.DPTF-DPMF|UR_AccountRoleBurn ouro-id OUROBOROS|SC_NAME true))
                (t1:bool (and o-rm o-rb))
                (g-rm:bool (BASIS.DPTF|UR_AccountRoleMint gas-id OUROBOROS|SC_NAME))
                (g-rb:bool (BASIS.DPTF-DPMF|UR_AccountRoleBurn gas-id OUROBOROS|SC_NAME true))
                (t2:bool (and g-rm g-rb))
                (t3:bool (and t1 t2))
            )
            (enforce t3 "Permission invalid for Ignis Exchange")
        )
    )
    ;;[CAP]
    (defcap IGNIS|SUBLIMATE (patron:string client:string target:string)
        @event
        (compose-capability (OUROBOROS|GOV))
        (compose-capability (SUMMONER))
        (IGNIS|UEV_Exchange)
        (DALOS.DALOS|UEV_EnforceAccountType patron false)
        (DALOS.DALOS|UEV_EnforceAccountType client false)
        (DALOS.DALOS|UEV_EnforceAccountType target false)
    )
    (defcap IGNIS|COMPRESS (patron:string client:string)
        @event
        (compose-capability (OUROBOROS|GOV))
        (compose-capability (SUMMONER))
        (IGNIS|UEV_Exchange)
        (DALOS.DALOS|UEV_EnforceAccountType patron false)
        (DALOS.DALOS|UEV_EnforceAccountType client false)
    )
    (defcap OUROBOROS|WITHDRAW (patron:string id:string target:string)
        @event
        (compose-capability (OUROBOROS|GOV))
        (compose-capability (SUMMONER))
        (BASIS.DPTF-DPMF|CAP_Owner id true)
        (DALOS.DALOS|UEV_EnforceAccountType target false)
    )
    (defcap OUROBOROS|ADMIN_FUEL ()
        @event
        (compose-capability (OUROBOROS|GOV))
        (compose-capability (OUROBOROS|NATIVE-AUTOMATIC))
        (compose-capability (SUMMONER))
    )
    ;;[URC] & [UC]
    (defun IGNIS|UC_Sublimate:decimal (ouro-amount:decimal)
        (enforce (>= ouro-amount 1.00) "Only amounts greater than or equal to 1.0 can be used to make gas!")
        (let
            (
                (ouro-id:string (DALOS.DALOS|UR_OuroborosID))
                
            )
            (BASIS.DPTF-DPMF|UEV_Amount ouro-id ouro-amount true)
            (let*
                (
                    (ouro-price:decimal (DALOS.DALOS|UR_OuroborosPrice))
                    (ouro-price-used:decimal (if (<= ouro-price 1.00) 1.00 ouro-price))
                    (ignis-id:string (DALOS.DALOS|UR_IgnisID))
                )
                (enforce (!= ignis-id UTILS.BAR) "Gas Token isnt properly set")
                (let*
                    (
                        (ignis-precision:integer (BASIS.DPTF-DPMF|UR_Decimals ignis-id true))
                        (raw-ignis-amount-per-unit:decimal (floor (* ouro-price-used 100.0) ignis-precision))
                        (raw-ignis-amount:decimal (floor (* raw-ignis-amount-per-unit ouro-amount) ignis-precision))
                        (output-ignis-amount:decimal (floor raw-ignis-amount 0))
                    )
                    output-ignis-amount
                )
            )
        )
    )
    (defun IGNIS|UC_Compress (ignis-amount:decimal)
        (enforce (= (floor ignis-amount 0) ignis-amount) "Only whole Units of GAS(Ignis) can be compressed")
        (enforce (>= ignis-amount 1.00) "Only amounts greater than or equal to 1.0 can be used to compress gas")
        (let*
            (
                (ouro-id:string (DALOS.DALOS|UR_OuroborosID))
                (ouro-price:decimal (DALOS.DALOS|UR_OuroborosPrice))
                (ouro-price-used:decimal (if (<= ouro-price 1.00) 1.00 ouro-price))
                (ignis-id:string (DALOS.DALOS|UR_IgnisID))
            )
            (BASIS.DPTF-DPMF|UEV_Amount ignis-id ignis-amount true)
            (let*
                (
                    (ouro-precision:integer (BASIS.DPTF-DPMF|UR_Decimals ouro-id true))
                    (raw-ouro-amount:decimal (floor (/ ignis-amount (* ouro-price-used 100.0)) ouro-precision))
                    (promile-split:[decimal] (UTILS.ATS|UC_PromilleSplit 15.0 raw-ouro-amount ouro-precision))
                    (ouro-remainder-amount:decimal (floor (at 0 promile-split) ouro-precision))
                    (ouro-fee-amount:decimal (at 1 promile-split))
                )
                [ouro-remainder-amount ouro-fee-amount]
            )
        )
    )
    ;;[C]
    (defun OUROBOROS|C_FuelLiquidStakingFromReserves (patron:string)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (OUROBOROS|ADMIN_FUEL)
            (let*
                (
                    (present-kda-balance:decimal (at "balance" (coin.details (DALOS.DALOS|UR_AccountKadena OUROBOROS|SC_NAME))))
                    (w-kda:string (DALOS.DALOS|UR_WrappedKadenaID))
                    (w-kda-as-rt:[string] (BASIS.DPTF|UR_RewardToken w-kda))
                    (liquid-idx:string (at 0 w-kda-as-rt))
                )
                (if (> present-kda-balance 0.0)
                    (with-capability (COMPOSE)
                ;;Use existing Native Kadena Stock for wrapping int DWK
                        (LIQUID.LIQUID|C_WrapKadena patron OUROBOROS|SC_NAME present-kda-balance)
                ;;Use the generated DWK to fuel its Autostake Pair
                        (ATSM.ATSM|C_Fuel patron OUROBOROS|SC_NAME liquid-idx w-kda present-kda-balance)
                    )
                    true
                )
            )
        )
    )
    (defun OUROBOROS|C_WithdrawFees (patron:string id:string target:string)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (OUROBOROS|WITHDRAW patron id target)
            (let
                (
                    (withdraw-amount:decimal (BASIS.DPTF-DPMF|UR_AccountSupply id OUROBOROS|SC_NAME true))
                )
                (enforce (> withdraw-amount 0.0) (format "There are no {} fees to be withdrawn from {}" [id OUROBOROS|SC_NAME]))
            ;;1]Patron withdraws Fees from Ouroboros Smart DALOS Account to a target Normal DALOS Account
                (TFT.DPTF|C_Transfer patron id OUROBOROS|SC_NAME target withdraw-amount true)
            )
        )
    )
    (defun IGNIS|C_Sublimate:decimal (patron:string client:string target:string ouro-amount:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (IGNIS|SUBLIMATE patron client target)
            (let*
                (
                    (ignis-id:string (DALOS.DALOS|UR_IgnisID))
                    (ouro-id:string (DALOS.DALOS|UR_OuroborosID))
                    (ouro-precision:integer (BASIS.DPTF-DPMF|UR_Decimals ouro-id true))
                    (ouro-split:[decimal] (UTILS.ATS|UC_PromilleSplit 10.0 ouro-amount ouro-precision))
                    (ouro-remainder-amount:decimal (at 0 ouro-split))
                    (ouro-fee-amount:decimal (at 1 ouro-split))
                    (ignis-amount:decimal (IGNIS|UC_Sublimate ouro-remainder-amount))
                )
            ;;01]Client sends OURO <ouro-amount> to the Ouroboros Smart DALOS Account
                (TFT.DPTF|C_Transfer patron ouro-id client OUROBOROS|SC_NAME ouro-amount true)
            ;;02]Ouroboros burns OURO <ouro-remainder-amount>
                (BASIS.DPTF|C_Burn patron ouro-id OUROBOROS|SC_NAME ouro-remainder-amount)
            ;;03]Ouroboros mints GAS(Ignis) <ignis-amount>
                (BASIS.DPTF|C_Mint patron ignis-id OUROBOROS|SC_NAME ignis-amount false)
            ;;04]Ouroboros transfers GAS(Ignis) <ignis-amount> to <target>
                (TFT.DPTF|C_Transfer patron ignis-id OUROBOROS|SC_NAME target ignis-amount true)
            ;;05]Ouroboros transmutes OURO <ouro-fee-amount>
                (TFT.DPTF|C_Transmute patron ouro-id OUROBOROS|SC_NAME ouro-fee-amount)
                ignis-amount
            )
        )
    )
    (defun IGNIS|C_Compress:decimal (patron:string client:string ignis-amount:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (IGNIS|COMPRESS patron client)
            (let*
                (
                    (ouro-id:string (DALOS.DALOS|UR_OuroborosID))
                    (ignis-id:string (DALOS.DALOS|UR_IgnisID))
                    (ignis-to-ouro:[decimal] (IGNIS|UC_Compress ignis-amount))
                    (ouro-remainder-amount:decimal (at 0 ignis-to-ouro))
                    (ouro-fee-amount:decimal (at 1 ignis-to-ouro))
                    (total-ouro:decimal (+ ouro-remainder-amount ouro-fee-amount))
                )
            ;;01]Client sends GAS(Ignis) <ignis-amount> to the Ouroboros Smart DALOS Account
                (TFT.DPTF|C_Transfer patron ignis-id client OUROBOROS|SC_NAME ignis-amount true)
            ;;02]Ouroboros burns GAS(Ignis) <ignis-amount>
                (BASIS.DPTF|C_Burn patron ignis-id OUROBOROS|SC_NAME ignis-amount)
            ;;03]Ouroboros mints OURO <total-ouro>
                (BASIS.DPTF|C_Mint patron ouro-id OUROBOROS|SC_NAME total-ouro false)
            ;;04]Ouroboros transfers OURO <ouro-remainder-amount> to <client>
                (TFT.DPTF|C_Transfer patron ouro-id OUROBOROS|SC_NAME client ouro-remainder-amount true)
            ;;05]Ouroboros transmutes OURO <ouro-fee-amount>
                (TFT.DPTF|C_Transmute patron ouro-id OUROBOROS|SC_NAME ouro-fee-amount)
                ouro-remainder-amount
            )
        )
    )
)
