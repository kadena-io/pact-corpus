(module VESTING GOVERNANCE
    (use n_9d612bcfe2320d6ecbbaa99b47aab60138a2adea.UTILS)
    (use n_9d612bcfe2320d6ecbbaa99b47aab60138a2adea.DALOS)
    (use n_9d612bcfe2320d6ecbbaa99b47aab60138a2adea.BASIS)
    (use n_9d612bcfe2320d6ecbbaa99b47aab60138a2adea.ATS)

    (defcap GOVERNANCE ()
        (compose-capability (VESTG-ADMIN))
    )

    (defcap VESTG-ADMIN ()
        (enforce-one
            "Vesting Admin not satisfed"
            [
                (enforce-guard G-MD_VST)
                (enforce-guard G-SC_VST)
            ]
        )
    )

    (defconst G-MD_VST   (keyset-ref-guard DALOS.DALOS|DEMIURGOI))
    (defconst G-SC_VST   (keyset-ref-guard VST|SC_KEY))

    (defconst VST|SC_KEY
        (+ UTILS.NS_USE ".dh_sc_vesting-keyset")
    )
    (defconst VST|SC_NAME "Σ.şZïζhЛßdяźπПЧDΞZülΦпφßΣитœŸ4ó¥ĘкÌЦ₱₱AÚюłćβρèЬÍŠęgĎwтäъνFf9źdûъtJCλúp₿ÌнË₿₱éåÔŽvCOŠŃpÚKюρЙΣΩìsΞτWpÙŠŹЩпÅθÝØpтŮыØșþшу6GтÃêŮĞбžŠΠŞWĆLτЙđнòZЫÏJÿыжU6ŽкЫVσ€ьqθtÙѺSô€χ")       ;;Former DalosVesting
    (defconst VST|SC_KDA-NAME "k:4728327e1b4790cb5eb4c3b3c531ba1aed00e86cd9f6252bfb78f71c44822d6d")

    (defcap COMPOSE ()
        true
    )
    (defcap SECURE ()
        true
    )
    (defcap SUMMONER ()
        true
    )
    (defcap VST|DEFINE ()
        true
    )
    (defcap P|DIN ()
        true
    )
    (defcap P|IC ()
        true
    )
    (defcap P|VST|UPDATE ()
        true
    )
    (defcap P|DINIC ()
        (compose-capability (P|DIN))
        (compose-capability (P|IC))
    )
    (defcap SSVD ()
        @event
        (compose-capability (SUMMONER))
        (compose-capability (VST|DEFINE))
    )

    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (VESTG-ADMIN)
            (write PoliciesTable policy-name
                {"policy" : policy-guard}
            )
        )
    )
    (defun C_ReadPolicy:guard (policy-name:string)
        (at "policy" (read PoliciesTable policy-name ["policy"]))
    )

    (defcap VST|GOV ()
        @doc "Vesting Module Governor Capability"
        true
    )
    (defun VST|SetGovernor (patron:string)
        (with-capability (SUMMONER)
            (DALOS.DALOS|C_RotateGovernor
                patron
                VST|SC_NAME
                (create-capability-guard (VST|GOV))
            )
        )
    )
    (defun DefinePolicies ()
        (DALOS.A_AddPolicy
            "VST|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (DALOS.A_AddPolicy
            "VST|PlusDalosNonce"
            (create-capability-guard (P|DIN))
        )
        (DALOS.A_AddPolicy
            "VST|GasCol"
            (create-capability-guard (P|IC))
        )
        (BASIS.A_AddPolicy
            "VST|UpVes"
            (create-capability-guard (P|VST|UPDATE))
        )
        (BASIS.A_AddPolicy
            "VST|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (ATS.A_AddPolicy
            "VST|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (ATSI.A_AddPolicy
            "VST|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (TFT.A_AddPolicy
            "VST|Summoner"
            (create-capability-guard (SUMMONER))
        )
    )

    (defschema VST|MetaDataSchema
        release-amount:decimal
        release-date:time
    )

    (deftable PoliciesTable:{DALOS.PolicySchema})

    ;;VESTING
    (defun VST|UEV_Active (dptf:string dpmf:string)
        (let
            (
                (dptf-fee-exemption:bool (BASIS.DPTF|UR_AccountRoleFeeExemption dptf VST|SC_NAME))
                (create-role:bool (BASIS.DPMF|UR_AccountRoleCreate dpmf VST|SC_NAME))
                (add-q-role:bool (BASIS.DPMF|UR_AccountRoleNFTAQ dpmf VST|SC_NAME))
                (burn-role:bool (BASIS.DPTF-DPMF|UR_AccountRoleBurn dpmf VST|SC_NAME false))
                (transfer-role:bool (BASIS.DPTF-DPMF|UR_AccountRoleTransfer dpmf VST|SC_NAME false))
            )
            (enforce (= dptf-fee-exemption true) "<role-fee-exemption> needed on DalosVesting")
            (enforce (= create-role true) "<role-nft-create> needed on DalosVesting")
            (enforce (= add-q-role true) "<role-nft-add-quantity> needed on DalosVesting")
            (enforce (= burn-role true) "<role-nft-burn> needed on DalosVesting")
            (enforce (= transfer-role true) "<role-transfer> needed on DalosVesting")
        )
    )
    (defcap VST|VEST (vester:string target-account:string id:string)
        @event
        (compose-capability (VST|GOV))
        (compose-capability (SUMMONER))
        (compose-capability (SECURE))
        (BASIS.DPTF-DPMF|CAP_Owner id true)
        (DALOS.DALOS|UEV_EnforceAccountType vester false)
        (DALOS.DALOS|UEV_EnforceAccountType target-account false)
        (BASIS.VST|UEV_Existance id true true)
        (VST|UEV_Active id (BASIS.DPTF-DPMF|UR_Vesting id true))
    )
    (defcap VST|CULL (culler:string id:string)
        @event
        (compose-capability (VST|GOV))
        (compose-capability (SUMMONER))
        (compose-capability (SECURE))
        (DALOS.DALOS|UEV_EnforceAccountType culler false)
        (BASIS.VST|UEV_Existance id false true)
        (VST|UEV_Active (BASIS.DPTF-DPMF|UR_Vesting id false) id)
    )
    (defcap VST|UPDATE_VESTING (dptf:string dpmf:string)
        (compose-capability (VST|X_UPDATE_VESTING dptf dpmf))
        (compose-capability (P|DINIC))
    )
    (defcap VST|X_UPDATE_VESTING (dptf:string dpmf:string)
        (BASIS.DPTF-DPMF|UEV_id dptf true)
        (BASIS.DPTF-DPMF|UEV_id dpmf false)
        (BASIS.DPTF-DPMF|CAP_Owner dptf true)
        (BASIS.DPTF-DPMF|CAP_Owner dpmf false)
        (VST|UEV_Active dptf dpmf)
        (let
            (
                (tf-vesting-id:string (BASIS.DPTF-DPMF|UR_Vesting dptf true))
                (mf-vesting-id:string (BASIS.DPTF-DPMF|UR_Vesting dpmf false))
                (iz-hot-rbt:bool (BASIS.ATS|UC_IzRBT dpmf false))
            )
            (enforce 
                (and (= tf-vesting-id UTILS.BAR) (= mf-vesting-id UTILS.BAR) )
                "Vesting Pairs are immutable !"
            )
            (enforce (= iz-hot-rbt false) "A DPMF defined as a hot-rbt cannot be used as Vesting Token in Vesting pair")
        )
    )
    ;;
    (defun VST|UC_SplitBalanceForVesting:[decimal] (id:string amount:decimal milestones:integer)
        (UTILS.VST|UC_SplitBalanceForVesting (BASIS.DPTF-DPMF|UR_Decimals id true) amount milestones)
    )
    (defun VST|UC_MakeVestingDateList:[time] (offset:integer duration:integer milestones:integer)
        (UTILS.VST|UC_MakeVestingDateList offset duration milestones)
    )
    (defun VST|UC_CullMetaDataAmount:decimal (client:string id:string nonce:integer)
        (BASIS.VST|UC_HasVesting id false)
        (let*
            (
                (meta-data:[object{VST|MetaDataSchema}] (BASIS.DPMF|UR_AccountBatchMetaData id nonce client))
                (culled-amount:decimal
                    (fold
                        (lambda
                            (acc:decimal item:object{VST|MetaDataSchema})
                            (let*
                                (
                                    (balance:decimal (at "release-amount" item))
                                    (date:time (at "release-date" item))
                                    (present-time:time (at "block-time" (chain-data)))
                                    (t:decimal (diff-time present-time date))
                                )
                                (if (>= t 0.0)
                                    (+ acc balance)
                                    acc
                                )
                            )
                        )
                        0.0
                        meta-data
                    )
                )
            )
            culled-amount
        )
    )
    (defun VST|UC_CullMetaDataObject:[object{VST|MetaDataSchema}] (client:string id:string nonce:integer)
        (BASIS.VST|UC_HasVesting id false)
        (let*
            (
                (meta-data:[object{VST|MetaDataSchema}] (BASIS.DPMF|UR_AccountBatchMetaData id nonce client))
                (culled-object:[object{VST|MetaDataSchema}]
                    (fold
                        (lambda
                            (acc:[object{VST|MetaDataSchema}] item:object{VST|MetaDataSchema})
                            (let*
                                (
                                    (date:time (at "release-date" item))
                                    (present-time:time (at "block-time" (chain-data)))
                                    (t:decimal (diff-time present-time date))
                                )
                                (if (< t 0.0)
                                    (UTILS.LIST|UC_AppendLast acc item)
                                    acc
                                )
                            )
                        )
                        []
                        meta-data
                    )
                )
            )
            culled-object
        )
    )
    ;;
    (defun VST|UCC_ComposeVestingMetaData:[object{VST|MetaDataSchema}] (id:string amount:decimal offset:integer duration:integer milestones:integer)
        (BASIS.DPTF-DPMF|UEV_Amount id amount true)
        (UTILS.VST|UEV_MilestoneWithTime offset duration milestones)
        (let*
            (
                (amount-lst:[decimal] (VST|UC_SplitBalanceForVesting id amount milestones))
                (date-lst:[time] (VST|UC_MakeVestingDateList offset duration milestones))
                (meta-data:[object{VST|MetaDataSchema}] (zip (lambda (x:decimal y:time) { "release-amount": x, "release-date": y }) amount-lst date-lst))
            )
            meta-data
        )
    )
    ;;
    (defun VST|C_CreateVestingLink:string (patron:string dptf:string)
        (enforce-one
            "Creating Vesting Link not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
                (enforce-guard (C_ReadPolicy "DEPLOYER|Summoner"))
            ]
        )
        (with-capability (SSVD)
            (let*
                (
                    (ZG:bool (DALOS.IGNIS|URC_IsVirtualGasZero))
                    (dptf-owner:string (BASIS.DPTF-DPMF|UR_Konto dptf true))
                    (dptf-name:string (BASIS.DPTF-DPMF|UR_Name dptf true))
                    (dptf-ticker:string (BASIS.DPTF-DPMF|UR_Ticker dptf true))
                    (dptf-decimals:integer (BASIS.DPTF-DPMF|UR_Decimals dptf true))
                    (dpmf-name:string (+ "Vested" (take 44 dptf-name)))
                    (dpmf-ticker:string (+ "Z" (take 19 dptf-ticker)))
                    (dpmf-l:[string]
                        (BASIS.DPMF|C_Issue
                            patron
                            dptf-owner
                            [dpmf-name]
                            [dpmf-ticker]
                            [dptf-decimals]
                            [false]
                            [false]
                            [true]
                            [false]
                            [false]
                            [false]
                            [true]
                        )
                    )
                    (dpmf:string (at 0 dpmf-l))
                )
                (BASIS.DPTF-DPMF|C_DeployAccount dptf VST|SC_NAME true)
                (BASIS.DPTF-DPMF|C_DeployAccount dpmf VST|SC_NAME false)
                (BASIS.DPTF-DPMF|C_ToggleTransferRole patron dpmf VST|SC_NAME true false)
                (ATSI.DPTF-DPMF|C_ToggleBurnRole patron dpmf VST|SC_NAME true false)
                (ATSI.DPTF|C_ToggleFeeExemptionRole patron dptf VST|SC_NAME true)
                (ATSI.DPMF|C_MoveCreateRole patron dpmf VST|SC_NAME)
                (ATSI.DPMF|C_ToggleAddQuantityRole patron dpmf VST|SC_NAME true)
                (VST|X_DefineVestingPair patron dptf dpmf)
                dpmf
            )
        )
    )
    (defun VST|C_Vest (patron:string vester:string target-account:string id:string amount:decimal offset:integer duration:integer milestones:integer)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (VST|VEST vester target-account id)
            (let*
                (
                    (dpmf-id:string (BASIS.DPTF-DPMF|UR_Vesting id true))
                    (meta-data:string (VST|UCC_ComposeVestingMetaData id amount offset duration milestones))
                    (nonce:integer (+ (BASIS.DPMF|UR_NoncesUsed id) 1))
                )
                (BASIS.DPMF|C_Mint patron dpmf-id VST|SC_NAME amount meta-data)
                (TFT.DPTF|C_Transfer patron id vester VST|SC_NAME amount true)
                (BASIS.DPMF|C_Transfer patron dpmf-id nonce VST|SC_NAME target-account amount true)
            )
        )
    )
    (defun VST|C_Cull (patron:string culler:string id:string nonce:integer)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (with-capability (VST|CULL culler id)
            (let*
                (
                    (dptf-id:string (BASIS.DPTF-DPMF|UR_Vesting id false))
                    (initial-amount:decimal (BASIS.DPMF|UR_AccountBatchSupply id nonce culler))
                    (culled-amount:decimal (VST|UC_CullMetaDataAmount culler id nonce))
                    (return-amount:decimal (- initial-amount culled-amount))
                )
                (if (= return-amount 0.0)
                    (TFT.DPTF|C_Transfer patron dptf-id VST|SC_NAME culler initial-amount true)
                    (let*
                        (
                            (remaining-vesting-meta-data:[object{VST|MetaDataSchema}] (VST|UC_CullMetaDataObject culler id nonce))
                            (new-nonce:integer (+ (BASIS.DPMF|UR_NoncesUsed id) 1))
                        )
                        (BASIS.DPMF|C_Mint patron id VST|SC_NAME return-amount remaining-vesting-meta-data)
                        (TFT.DPTF|C_Transfer patron dptf-id VST|SC_NAME culler culled-amount true)
                        (BASIS.DPMF|C_Transfer patron id new-nonce VST|SC_NAME culler return-amount true)
                    )
                )
                (BASIS.DPMF|C_Transfer patron id nonce culler VST|SC_NAME initial-amount true)
                (BASIS.DPMF|C_Burn patron id nonce VST|SC_NAME initial-amount)
            )
        )
    )
    ;;
    (defun VST|X_DefineVestingPair (patron:string dptf:string dpmf:string)
        (require-capability (VST|DEFINE))
        (let
            (
                (ZG:bool (DALOS.IGNIS|URC_IsVirtualGasZero))
                (dptf-owner:string (BASIS.DPTF-DPMF|UR_Konto dptf true))
            )
            (with-capability (VST|UPDATE_VESTING dptf dpmf)
                (if (= ZG false)
                    (DALOS.IGNIS|X_Collect patron dptf-owner DALOS.GAS_BIGGEST)
                    true
                )
                (VST|X_UpdateVesting dptf dpmf)
                (DALOS.DALOS|X_IncrementNonce patron)
            )
        )
    )
    (defun VST|X_UpdateVesting (dptf:string dpmf:string)
        (require-capability (VST|X_UPDATE_VESTING dptf dpmf))
        (with-capability (P|VST|UPDATE)
            (BASIS.DPTF-DPMF|XO_UpdateVesting dptf dpmf)
        )
    )
)


