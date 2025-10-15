(module TFT GOVERNANCE
    (defcap GOVERNANCE ()
        (compose-capability (TFT-ADMIN))
    )
    (defcap TFT-ADMIN ()
        (enforce-guard G-MD_TFT)
    )

    (defconst G-MD_TFT  (keyset-ref-guard DALOS.DALOS|DEMIURGOI))

    (defcap COMPOSE ()
        true
    )
    (defcap SECURE ()
        true
    )
    (defcap DALOS|EXECUTOR ()
        true
    )
    (defcap P|IC ()
        true
    )
    (defcap P|DIN ()
        true
    )
    (defcap P|T|UF ()
        true
    )
    (defcap P|DPTF|DEBIT ()
        true
    )
    (defcap P|DPTF|CREDIT ()
        true
    )
    (defcap P|DALOS|UPDATE_ELITE ()
        true
    )
    (defcap P|ATS|UPDATE_ROU ()
        true
    )
    (defcap P|DPTF|BURN ()
        true
    )

    (defcap DPTF|CPF_CREDIT-FEE ()
        true
    )
    (defcap DPTF|CPF_STILL-FEE ()
        true
    )
    (defcap DPTF|CPF_BURN-FEE ()
        true
    )

    (defcap P|DINIC ()
        (compose-capability (P|DIN))
        (compose-capability (P|IC))
    )

    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (TFT-ADMIN)
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
            "TFT|GasCol"
            (create-capability-guard (P|IC))
        )
        (DALOS.A_AddPolicy
            "TFT|PlusDalosNonce"
            (create-capability-guard (P|DIN))
        )
        (DALOS.A_AddPolicy 
            "TFT|UpdElite"
            (create-capability-guard (P|DALOS|UPDATE_ELITE))
        )
        (BASIS.A_AddPolicy 
            "TFT|DbTF"
            (create-capability-guard (P|DPTF|DEBIT))
        )
        (BASIS.A_AddPolicy 
            "TFT|CrTF"
            (create-capability-guard (P|DPTF|CREDIT))
        )
        (BASIS.A_AddPolicy 
            "TFT|UpFees"
            (create-capability-guard (P|T|UF))
        )
        (BASIS.A_AddPolicy
            "TFT|BrTF"
            (create-capability-guard (P|DPTF|BURN))
        )
        (ATS.A_AddPolicy 
            "TFT|UpdateROU"
            (create-capability-guard (P|ATS|UPDATE_ROU))
        )
    )

    (defschema DPTF|ID-Amount
        id:string
        amount:decimal
    )
    (defschema DPTF|Receiver-Amount
        receiver:string
        amount:decimal
    )

    (deftable PoliciesTable:{DALOS.PolicySchema})

    ;;[UR]
    (defun DPTF-DPMF-ATS|UR_TableKeys:[string] (position:integer poi:bool)
        (UTILS.UTILS|UEV_PositionalVariable position 3 "Invalid Ownership Position")
        (if poi
            (if (= position 1)
                (BASIS.DPTF|P-KEYS)
                (if (= position 2)
                    (BASIS.DPMF|P-KEYS)
                    (ATS.ATS|P-KEYS)
                )
            )
            (if (= position 1)
                (BASIS.DPTF|KEYS)
                (if (= position 2)
                    (BASIS.DPMF|KEYS)
                    (ATS.ATS|KEYS)
                )
            )
        )
    )
    (defun DPTF-DPMF-ATS|UR_FilterKeysForInfo:[string] (account-or-token-id:string table-to-query:integer mode:bool)
        @doc "Returns a List of either: \
            \       Direct-Mode(true):      <account-or-token-id> is <account> Name: \
            \                               Returns True-Fungible, Meta-Fungible IDs or ATS-Pairs held by an Accounts <account> OR \
            \       Inverse-Mode(false):    <account-or-token-id> is DPTF|DPMF|ATS-Pair Designation Name \
            \                               Returns Accounts that exists for a specific True-Fungible, Meta-Fungible or ATS-Pair \
            \       MODE Boolean is only used for proper validation, to accees the needed table, use the proper integer: \
            \ Table-to-query: \
            \ 1 (DPTF|BalanceTable), 2(DPMF|BalanceTable), 3(ATS|Ledger) "
        (UTILS.UTILS|UEV_PositionalVariable table-to-query 3 "Table To Query can only be 1 2 or 3")
        (if mode
            (DALOS.GLYPH|UEV_DalosAccount account-or-token-id)
            (with-capability (COMPOSE)
                (if (= table-to-query 1)
                    (BASIS.DPTF-DPMF|UEV_id account-or-token-id true)
                    (if (= table-to-query 2)
                        (BASIS.DPTF-DPMF|UEV_id account-or-token-id false)
                        (ATS.ATS|UEV_id account-or-token-id) 
                    )
                )
            )
        )
        (let*
            (
                (keyz:[string] (DPTF-DPMF-ATS|UR_TableKeys table-to-query false))
                (listoflists:[[string]] (map (lambda (x:string) (UTILS.LIST|UC_SplitString UTILS.BAR x)) keyz))
                (output:[string] (UTILS.DALOS|UC_Filterid listoflists account-or-token-id))
            )
            output
        )
    )
    ;;[UEV]
    (defun DPTF-DPMF|UEV_AmountCheck:bool (id:string amount:decimal token-type:bool)
        (let*
            (
                (decimals:integer (BASIS.DPTF-DPMF|UR_Decimals id token-type))
                (decimal-check:bool (if (= (floor amount decimals) amount) true false))
                (positivity-check:bool (if (> amount 0.0) true false))
                (result:bool (and decimal-check positivity-check))
            )
            result
        )
    )
    (defun DPTF|UEV_Pair_ID-Amount:bool (id-lst:[string] transfer-amount-lst:[decimal])
        (fold
            (lambda
                (acc:bool item:object{DPTF|ID-Amount})
                (and acc (DPTF-DPMF|UEV_AmountCheck (at "id" item) (at "amount" item) true))
            )
            true
            (DPTF|UCC_Pair_ID-Amount id-lst transfer-amount-lst)
        )
    )
    (defun DPTF|UEV_Pair_Receiver-Amount:bool (id:string receiver-lst:[string] transfer-amount-lst:[decimal])
        (BASIS.DPTF-DPMF|UEV_id id true)
        (and
            (fold
                (lambda
                    (acc:bool item:object{DPTF|Receiver-Amount})
                    (let*
                        (
                            (receiver-account:string (at "receiver" item))
                            (receiver-check:bool (DALOS.GLYPH|UEV_DalosAccountCheck receiver-account))
                        )
                        (and acc receiver-check)
                    )
                )
                true
                (DPTF|UCC_Pair_Receiver-Amount receiver-lst transfer-amount-lst)
            )
            (fold
                (lambda
                    (acc:bool item:object{DPTF|Receiver-Amount})
                    (let*
                        (
                            (transfer-amount:decimal (at "amount" item))
                            (check:bool (DPTF-DPMF|UEV_AmountCheck id transfer-amount true))
                        )
                        (and acc check)
                    )
                )
                true
                (DPTF|UCC_Pair_Receiver-Amount receiver-lst transfer-amount-lst)
            )
        )
    )
    ;;[UCC]
    (defun DPTF|UCC_Pair_ID-Amount:[object{DPTF|ID-Amount}] (id-lst:[string] transfer-amount-lst:[decimal])
        (let
            (
                (id-length:integer (length id-lst))
                (amount-length:integer (length transfer-amount-lst))
            )
            (enforce (= id-length amount-length) "ID and Transfer-Amount Lists are not of equal length")
            (zip (lambda (x:string y:decimal) { "id": x, "amount": y }) id-lst transfer-amount-lst)
        )
    )
    (defun DPTF|UCC_Pair_Receiver-Amount:[object{DPTF|Receiver-Amount}] (receiver-lst:[string] transfer-amount-lst:[decimal])
        (let
            (
                (receiver-length:integer (length receiver-lst))
                (amount-length:integer (length transfer-amount-lst))
            )
            (enforce (= receiver-length amount-length) "Receiver and Transfer-Amount Lists are not of equal length")
            (zip (lambda (x:string y:decimal) { "receiver": x, "amount": y }) receiver-lst transfer-amount-lst)
        )
    )
    ;;[CAP]
    (defcap IGNIS|MATRON_STRONG (id:string client:string target:string)
        (if (DALOS.IGNIS|URC_ZeroGAZ id client target)
            true
            (compose-capability (P|IC))
        )
    )
    (defcap DPTF|TRANSFER (patron:string id:string sender:string receiver:string transfer-amount:decimal method:bool)
        @event
        (compose-capability (DPTF|X_TRANSFER id sender receiver transfer-amount method))
        (compose-capability (IGNIS|MATRON_STRONG id sender receiver))
        (compose-capability (P|DIN))
        (compose-capability (DALOS|EXECUTOR))
    )
    (defcap DPTF|X_TRANSFER (id:string sender:string receiver:string transfer-amount:decimal method:bool)
        (DALOS.DALOS|CAP_EnforceAccountOwnership sender)
        (if (and method (DALOS.DALOS|UR_AccountType receiver))
            (DALOS.DALOS|CAP_EnforceAccountOwnership receiver)
            true
        )
        (BASIS.DPTF-DPMF|UEV_Amount id transfer-amount true)
        (if (not (BASIS.DPTF|UC_TransferFeeAndMinException id sender receiver))
            (BASIS.DPTF|UEV_EnforceMinimumAmount id transfer-amount)
            true
        )
        (DALOS.DALOS|UEV_EnforceTransferability sender receiver method)
        (BASIS.DPTF-DPMF|UEV_PauseState id false true)
        (BASIS.DPTF-DPMF|UEV_AccountFreezeState id sender false true)
        (BASIS.DPTF-DPMF|UEV_AccountFreezeState id receiver false true)
        (if 
            (and 
                (> (BASIS.DPTF-DPMF|UR_TransferRoleAmount id true) 0) 
                (not (or (= sender DALOS.OUROBOROS|SC_NAME)(= sender DALOS.DALOS|SC_NAME)))
            )
            (let
                (
                    (s:bool (BASIS.DPTF-DPMF|UR_AccountRoleTransfer id sender true))
                    (r:bool (BASIS.DPTF-DPMF|UR_AccountRoleTransfer id receiver true))
                )
                (enforce-one
                    (format "Neither the sender {} nor the receiver {} have an active transfer role" [sender receiver])
                    [
                        (enforce (= s true) (format "TR doesnt check for sender {}" [sender]))
                        (enforce (= r true) (format "TR doesnt check for receiver {}" [receiver]))
                    ]

                )
            )
            (format "No transfer restrictions for {} from {} to {}" [id sender receiver])
        )
        ;;
        (compose-capability (P|DPTF|DEBIT))
        (compose-capability (P|DPTF|CREDIT))
        ;;
        (compose-capability (P|DALOS|UPDATE_ELITE))
        ;;
        (compose-capability (P|T|UF))
        (compose-capability (DPTF|CREDIT_PRIMARY-FEE))
    )
    (defcap DPTF|CREDIT_PRIMARY-FEE ()
        (compose-capability (P|ATS|UPDATE_ROU))
        (compose-capability (DPTF|CPF_CREDIT-FEE))
        (compose-capability (DPTF|CPF_STILL-FEE))
        (compose-capability (DPTF|CPF_BURN-FEE))
        (compose-capability (P|DPTF|CREDIT))
        (compose-capability (P|DPTF|BURN))
    )
    (defcap DPTF|TRANSMUTE ()
        @event
        (compose-capability (DALOS|EXECUTOR))
        (compose-capability (P|DINIC))
        (compose-capability (DPTF|X_TRANSMUTE))
    )
    (defcap DPTF|X_TRANSMUTE ()
        (compose-capability (P|DPTF|DEBIT))
        (compose-capability (DPTF|CREDIT_PRIMARY-FEE))
    )
    ;;Transfer Function
    (defun DPTF|C_Transfer (patron:string id:string sender:string receiver:string transfer-amount:decimal method:bool)
        (enforce-one
            "DPTF Transfer not permitted"
            [
                (enforce-guard (create-capability-guard (SECURE)))
                (enforce-guard (C_ReadPolicy "ATSC|Caller"))
                (enforce-guard (C_ReadPolicy "ATSH|Caller"))
                (enforce-guard (C_ReadPolicy "ATSM|Caller"))
                (enforce-guard (C_ReadPolicy "VST|Summoner"))
                (enforce-guard (C_ReadPolicy "LIQUID|Summoner"))
                (enforce-guard (C_ReadPolicy "OUROBOROS|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
            ]
        )
        (with-capability (DPTF|TRANSFER patron id sender receiver transfer-amount method)
            (DPTF|XK_Transfer patron id sender receiver transfer-amount method)
        )
    )
    (defun DPTF|XK_Transfer (patron:string id:string sender:string receiver:string transfer-amount:decimal method:bool)
        (require-capability (DALOS|EXECUTOR))
        (if (not (and (= id (DALOS.DALOS|UR_UnityID))(>= transfer-amount 10)))
            (if (not (DALOS.IGNIS|URC_ZeroGAZ id sender receiver))
                (DALOS.IGNIS|X_Collect patron sender DALOS.GAS_SMALLEST)
                true
            )
            true
        )
        (DPTF|X_Transfer id sender receiver transfer-amount method)
        (DALOS.DALOS|X_IncrementNonce sender)
    )
    (defun DPTF|X_Transfer (id:string sender:string receiver:string transfer-amount:decimal method:bool)
        (require-capability (DPTF|X_TRANSFER id sender receiver transfer-amount method))
        (BASIS.DPTF|XO_DebitStandard id sender transfer-amount)
        (let*
            (
                (ea-id:string (DALOS.DALOS|UR_EliteAurynID))
                (fee-toggle:bool (BASIS.DPTF|UR_FeeToggle id))
                (iz-exception:bool (BASIS.DPTF|UC_TransferFeeAndMinException id sender receiver))
                (fees:[decimal] (BASIS.DPTF|UC_Fee id transfer-amount))
                (primary-fee:decimal (at 0 fees))
                (secondary-fee:decimal (at 1 fees))
                (remainder:decimal (at 2 fees))
                (iz-full-credit:bool 
                    (or 
                        (or 
                            (= fee-toggle false) 
                            (= iz-exception true)
                        ) 
                        (= primary-fee 0.0)
                    )
                )
            )
            (if iz-full-credit
                (BASIS.DPTF|XO_Credit id receiver transfer-amount)
                (if (= secondary-fee 0.0)
                    (with-capability (COMPOSE)
                        (DPTF|X_CreditPrimaryFee id primary-fee true)
                        (BASIS.DPTF|XO_Credit id receiver remainder)
                    )
                    (with-capability (COMPOSE)
                        (DPTF|X_CreditPrimaryFee id primary-fee true)
                        (BASIS.DPTF|XO_Credit id DALOS.DALOS|SC_NAME secondary-fee)
                        (BASIS.DPTF|XO_UpdateFeeVolume id secondary-fee false)
                        (BASIS.DPTF|XO_Credit id receiver remainder)
                    )
                )
            )
            (BASIS.DPTF-DPMF|X_UpdateElite id sender receiver)
        )
    )
    (defun DPTF|X_CreditPrimaryFee (id:string pf:decimal native:bool)
        (let
            (
                (rt:bool (BASIS.ATS|UC_IzRT id))
                (rbt:bool (BASIS.ATS|UC_IzRBT id true))
                (target:string (BASIS.DPTF|UR_FeeTarget id))
            )
            (if (and rt rbt)
                (let*
                    (
                        (v:[decimal] (TFT|CPF_RT-RBT id pf))
                        (v1:decimal (at 0 v))
                        (v2:decimal (at 1 v))
                        (v3:decimal (at 2 v))
                    )
                    (DPTF|X_CPF_StillFee id target v1)
                    (DPTF|X_CPF_CreditFee id target v2)
                    (DPTF|X_CPF_BurnFee id target v3)
                )
                (if rt
                    (let*
                        (
                            (v1:decimal (TFT|CPF_RT id pf))
                            (v2:decimal (- pf v1))
                        )
                        (DPTF|X_CPF_StillFee id target v1)
                        (DPTF|X_CPF_CreditFee id target v2)
                    )
                    (if rbt
                        (let*
                            (
                                (v1:decimal (TFT|CPF_RBT id pf))
                                (v2:decimal (- pf v1))
                            )
                            (DPTF|X_CPF_StillFee id target v1)
                            (DPTF|X_CPF_BurnFee id target v2)
                        )
                        (BASIS.DPTF|XO_Credit id target pf false)
                    )
                )
            )
        )
        (if native
            (BASIS.DPTF|XO_UpdateFeeVolume id pf true)
            true
        )
    )
    (defun DPTF|X_CPF_StillFee (id:string target:string still-fee:decimal)
        (require-capability (DPTF|CPF_STILL-FEE))
        (if (!= still-fee 0.0)
            (BASIS.DPTF|XO_Credit id target still-fee false)
            true
        )
    )
    (defun DPTF|X_CPF_BurnFee (id:string target:string burn-fee:decimal)
        (require-capability (DPTF|CPF_BURN-FEE))
        (if (!= burn-fee 0.0)
            (with-capability (COMPOSE)
                (BASIS.DPTF|XO_Credit id ATS.ATS|SC_NAME burn-fee)
                (BASIS.DPTF|XO_Burn id ATS.ATS|SC_NAME burn-fee)
            )
            true
        )
    )
    (defun DPTF|X_CPF_CreditFee (id:string target:string credit-fee:decimal)
        (require-capability (DPTF|CPF_CREDIT-FEE))
        (if (!= credit-fee 0.0)
            (BASIS.DPTF|XO_Credit id ATS.ATS|SC_NAME credit-fee)
            true
        )
    )
    ;;CPF-Computers
    (defun TFT|CPF_RT-RBT:[decimal] (id:string native-fee-amount:decimal)
        (let*
            (
                (rt-ats-pairs:[string] (BASIS.DPTF|UR_RewardToken id))
                (rbt-ats-pairs:[string] (BASIS.DPTF|UR_RewardBearingToken id))
                (length-rt:integer (length rt-ats-pairs))
                (length-rbt:integer (length rbt-ats-pairs))
                (rt-boolean:[bool] (TFT|NFR-Boolean_RT-RBT id rt-ats-pairs true))
                (rbt-boolean:[bool] (TFT|NFR-Boolean_RT-RBT id rbt-ats-pairs false))
                (rt-milestones:integer (length (UTILS.LIST|UC_Search rt-boolean true)))
                (rbt-milestones:integer (length (UTILS.LIST|UC_Search rbt-boolean true)))
                (milestones:integer (+ rt-milestones rbt-milestones))
            )
            (if (!= milestones 0)
                (let*
                    (
                        (truths:[bool] (+ rt-boolean rbt-boolean))
                        (split-with-truths:[decimal] (TFT|UC_BooleanDecimalCombiner id native-fee-amount milestones truths))
                    )
                    (if (!= rt-milestones 0)
                        (let
                            (
                                (credit-sum:decimal
                                    (fold
                                        (lambda
                                            (acc:decimal index:integer)
                                            (if (at index rt-boolean)
                                                (with-capability (COMPOSE)
                                                    (ATS.ATS|XO_UpdateRoU (at index rt-ats-pairs) id true true (at index split-with-truths))
                                                    (+ acc (at index split-with-truths))
                                                )
                                                acc
                                            )
                                        )
                                        0.0
                                        (enumerate 0 (- (length rt-ats-pairs) 1))
                                    )
                                )
                            )
                            (if (= credit-sum 0.0)
                                [0.0 0.0 native-fee-amount]
                                [0.0 credit-sum (- native-fee-amount credit-sum)]
                            )
                        )
                        [0.0 0.0 native-fee-amount]
                    )
                )
                [native-fee-amount 0.0 0.0]
            )
        )
    )
    (defun TFT|CPF_RBT:decimal (id:string native-fee-amount:decimal)
        (let*
            (
                (ats-pairs:[string] (BASIS.DPTF|UR_RewardBearingToken id))
                (ats-pairs-bool:[bool] (TFT|NFR-Boolean_RT-RBT id ats-pairs false))
                (milestones:integer (length (UTILS.LIST|UC_Search ats-pairs-bool true)))
            )
            (if (!= milestones 0)
                0.0
                native-fee-amount
            )
        )
    )
    (defun TFT|CPF_RT:decimal (id:string native-fee-amount:decimal)
        (let*
            (
                (ats-pairs:[string] (BASIS.DPTF|UR_RewardToken id))
                (ats-pairs-bool:[bool] (TFT|NFR-Boolean_RT-RBT id ats-pairs true))
                (milestones:integer (length (UTILS.LIST|UC_Search ats-pairs-bool true)))  
            )
            (if (!= milestones 0)
                (let*
                    (
                        (rt-split-with-boolean:[decimal] (TFT|UC_BooleanDecimalCombiner id native-fee-amount milestones ats-pairs-bool))
                        (number-of-zeroes:integer (length (UTILS.LIST|UC_Search rt-split-with-boolean 0.0)))
                    )
                    (map
                        (lambda
                            (index:integer)
                            (if (at index ats-pairs-bool)
                                (ATS.ATS|XO_UpdateRoU (at index ats-pairs) id true true (at index rt-split-with-boolean))
                                true
                            )
                        )
                        (enumerate 0 (- (length ats-pairs) 1))
                    )
                    0.0
                )
                native-fee-amount
            )
        )
    )
    (defun TFT|NFR-Boolean_RT-RBT:[bool] (id:string ats-pairs:[string] rt-or-rbt:bool)
        @doc "Makes a [bool] using RT or RBT <nfr> values from a list of ATS Pair"
        (fold
            (lambda
                (acc:[bool] index:integer)
                (if rt-or-rbt
                    (if (ATS.ATS|UR_RT-Data (at index ats-pairs) id 1)
                        (UTILS.LIST|UC_AppendLast acc true)
                        (UTILS.LIST|UC_AppendLast acc false)
                    )
                    (if (ATS.ATS|UR_ColdNativeFeeRedirection (at index ats-pairs))
                        (UTILS.LIST|UC_AppendLast acc true)
                        (UTILS.LIST|UC_AppendLast acc false)
                    )
                )
            )
            []
            (enumerate 0 (- (length ats-pairs) 1))
        )
    )
    (defun TFT|UC_BooleanDecimalCombiner:[decimal] (id:string amount:decimal milestones:integer boolean:[bool])
        (UTILS.ATS|UC_SplitBalanceWithBooleans (BASIS.DPTF-DPMF|UR_Decimals id true) amount milestones boolean)
    )
    ;;Transmute
    (defun DPTF|C_Transmute (patron:string id:string transmuter:string transmute-amount:decimal)
        (enforce-one
            "Transmute not permitted"
            [
                (enforce-guard (C_ReadPolicy "OUROBOROS|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
            ]
        )
        (with-capability (DPTF|TRANSMUTE)
            (DPTF|XK_Transmute patron id transmuter transmute-amount)
        )
    )
    (defun DPTF|XK_Transmute (patron:string id:string transmuter:string transmute-amount:decimal)
        (require-capability (DALOS|EXECUTOR))
        (if (not (and (= id (DALOS.DALOS|UR_UnityID))(>= transmute-amount 10)))
            (if (not (DALOS.IGNIS|URC_ZeroGAS id transmuter))
                (DALOS.IGNIS|X_Collect patron transmuter DALOS.GAS_SMALLEST)
                true
            )
            true
        )    
        (DPTF|X_Transmute id transmuter transmute-amount)
        (DALOS.DALOS|X_IncrementNonce transmuter)
    )
    (defun DPTF|X_Transmute (id:string transmuter:string transmute-amount:decimal)
        (require-capability (DPTF|X_TRANSMUTE))
        (BASIS.DPTF|XO_DebitStandard id transmuter transmute-amount)
        (DPTF|X_CreditPrimaryFee id transmute-amount false)
    )
    ;;DPTF Multi-Transfers
    (defun DPTF|X_MultiTransfer (patron:string id-lst:[string] sender:string receiver:string transfer-amount-lst:[decimal] method:bool)
        (enforce-one
            "DPTF Multi Transfer not permitted"
            [
                (enforce-guard (C_ReadPolicy "ATSH|Caller"))
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
            ]
        )
        (with-capability (SECURE)
            (let
                (
                    (pair-validation:bool (DPTF|UEV_Pair_ID-Amount id-lst transfer-amount-lst))
                )
                (enforce (= pair-validation true) "Input Lists <id-lst>|<transfer-amount-lst> cannot make a valid pair list for Multi Transfer Processing")
                (let
                    (
                        (pair:[object{DPTF|ID-Amount}] (DPTF|UCC_Pair_ID-Amount id-lst transfer-amount-lst))
                    )
                    (map (lambda (x:object{DPTF|ID-Amount}) (DPTF|X_MultiTransferPaired patron sender receiver x method)) pair)
                )
            )
        )
    )
    (defun DPTF|X_MultiTransferPaired (patron:string sender:string receiver:string id-amount-pair:object{DPTF|ID-Amount} method:bool)
        (require-capability (SECURE))
        (let
            (
                (id:string (at "id" id-amount-pair))
                (amount:decimal (at "amount" id-amount-pair))
            )
            (with-capability (SECURE)
                (DPTF|C_Transfer patron id sender receiver amount method)
            )
        )
    )
    (defun DPTF|X_BulkTransfer (patron:string id:string sender:string receiver-lst:[string] transfer-amount-lst:[decimal] method:bool)
        (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
        (with-capability (SECURE)
            (let
                (
                    (pair-validation:bool (DPTF|UEV_Pair_Receiver-Amount id receiver-lst transfer-amount-lst))
                )
                (enforce (= pair-validation true) "Input Lists <receiver-lst>|<transfer-amount-lst> cannot make a valid pair list with the <id> for Bulk Transfer Processing")
                (let
                    (
                        (pair:[object{DPTF|Receiver-Amount}] (DPTF|UCC_Pair_Receiver-Amount receiver-lst transfer-amount-lst))
                    )
                    (map (lambda (x:object{DPTF|Receiver-Amount}) (DPTF|X_BulkTransferPaired patron id sender x method)) pair)
                )
            )
        )
    )
    (defun DPTF|X_BulkTransferPaired (patron:string id:string sender:string receiver-amount-pair:object{DPTF|Receiver-Amount} method:bool)
        (require-capability (SECURE))
        (let
            (
                (receiver:string (at "receiver" receiver-amount-pair))
                (amount:decimal (at "amount" receiver-amount-pair))
            )
            (with-capability (SECURE)
                (DPTF|C_Transfer patron id sender receiver amount method)
            )
        )
    )
    ;;DPMF Multi-Transfers
    (defun DPMF|X_MultiBatchTransfer (patron:string id:string nonces:[integer] sender:string receiver:string method:bool)
        (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
        (let*
            (
                (account-nonces:[integer] (BASIS.DPMF|UR_AccountNonces id sender))
                (contains-all:bool (UTILS.UTILS|UEV_ContainsAll account-nonces nonces))
            )
            (enforce contains-all "Invalid Nonce List for DPTf Multi Batch Transfer")
            (map
                (lambda
                    (single-nonce:integer)
                    (with-capability (SECURE)
                        (DPMF|X_SingleBatchTransfer patron id single-nonce sender receiver method)
                    )
                )
                nonces
            )
        )
    )
    (defun DPMF|X_SingleBatchTransfer (patron:string id:string nonce:integer sender:string receiver:string method:bool)
        (enforce-one
            "DPMF Transfer not permitted"
            [
                (enforce-guard (create-capability-guard (SECURE)))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (let
            (
                (balance:decimal (BASIS.DPMF|UR_AccountBatchSupply id nonce))
            )
            (BASIS.DPMF|C_Transfer patron id nonce sender receiver balance method)
        )
    )
)
