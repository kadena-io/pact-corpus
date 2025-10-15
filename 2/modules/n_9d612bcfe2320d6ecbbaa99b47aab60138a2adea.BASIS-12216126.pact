(module BASIS GOVERNANCE
    (use n_9d612bcfe2320d6ecbbaa99b47aab60138a2adea.UTILS)
    (use n_9d612bcfe2320d6ecbbaa99b47aab60138a2adea.DALOS)

    (defcap GOVERNANCE ()
        (compose-capability (BASIS-ADMIN))
    )
    (defcap BASIS-ADMIN ()
        (enforce-guard G-MD_BASIS)
    )
    (defconst G-MD_BASIS   (keyset-ref-guard DALOS.DALOS|DEMIURGOI))

    (defcap COMPOSE ()
        true
    )
    ;;
    (defcap SECURE ()
        true
    )
    (defcap ATS|UPDATE_RBT ()
        true
    )
    (defcap DALOS|EXECUTOR ()
        true
    )
    (defcap DPMF|CREDIT ()
        true
    )
    (defcap DPMF|DEBIT ()
        true
    )
    (defcap DPMF|DEBIT_PUR ()
        true
    )
    (defcap DPMF|INCREMENT_NONCE ()
        true
    )
    (defcap DPTF|CREDIT ()
        true
    )
    (defcap DPTF|CREDIT_PUR ()
        true
    )
    (defcap DPTF|DEBIT ()
        true
    )
    (defcap DPTF|DEBIT_PUR ()
        true
    )
    (defcap DPTF|INCREMENT-LOCKS ()
        true
    )
    (defcap DPTF|UPDATE_FEES_PUR ()
        true
    )
    (defcap DPTF|UPDATE_RT ()
        true
    )
    (defcap DPTF-DPMF|UPDATE_ROLE-TRANSFER-AMOUNT ()
        true
    )
    (defcap DPTF-DPMF|UPDATE_SUPPLY () 
        true
    )
    (defcap VST|UPDATE ()
        true
    )
    ;;
    (defcap SUMMONER ()
        true
    )
    (defcap P|DIN ()
        true
    )
    (defcap P|DALOS|UP_BALANCE ()
        true
    )
    (defcap P|DALOS|UP_DATA ()
        true
    )
    (defcap P|DALOS|UPDATE_ELITE ()
        true
    )
    (defcap P|IC ()
        true
    )
    (defcap P|DINIC ()
        (compose-capability (P|DIN))
        (compose-capability (P|IC))
    )
    ;;
    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (BASIS-ADMIN)
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
            "BASIS|Summoner"
            (create-capability-guard (SUMMONER))
        )
        (DALOS.A_AddPolicy 
            "BASIS|IncrementDalosNonce"
            (create-capability-guard (P|DIN))
        )
        (DALOS.A_AddPolicy 
            "BASIS|UpdatePrimordialBalance"
            (create-capability-guard (P|DALOS|UP_BALANCE))
        )
        (DALOS.A_AddPolicy 
            "BASIS|UpdatePrimordialData"
            (create-capability-guard (P|DALOS|UP_DATA))
        )
        (DALOS.A_AddPolicy 
            "BASIS|UpdateElite"
            (create-capability-guard (P|DALOS|UPDATE_ELITE))
        )
        (DALOS.A_AddPolicy 
            "BASIS|GasCollection"
            (create-capability-guard (P|IC))
        )
    )
    ;;
    
    ;;[T] DPTF Schemas
    (defschema DPTF|PropertiesSchema
        branding:object{DALOS.BrandingSchema}
        branding-pending:object{DALOS.BrandingSchema}
        owner-konto:string
        name:string
        ticker:string
        decimals:integer
        ;;TM
        can-change-owner:bool                 
        can-upgrade:bool
        can-add-special-role:bool
        can-freeze:bool
        can-wipe:bool
        can-pause:bool
        is-paused:bool
        ;;Supply
        supply:decimal
        origin-mint:bool
        origin-mint-amount:decimal
        ;;Roles
        role-transfer-amount:integer
        ;;Fee Management
        fee-toggle:bool
        min-move:decimal
        fee-promile:decimal
        fee-target:string
        fee-lock:bool
        fee-unlocks:integer
        primary-fee-volume:decimal
        secondary-fee-volume:decimal
        ;;ATS-VST
        reward-token:[string]
        reward-bearing-token:[string]
        vesting:string
    )
    ;;DPTF|BalanceSchema defined in DALOS Module
    (defschema DPTF|RoleSchema
        r-burn:[string]
        r-mint:[string]
        r-fee-exemption:[string]
        r-transfer:[string]
        a-frozen:[string]
    )
    ;;[M] DPMF Schemas
    (defschema DPMF|PropertiesSchema
        branding:object{DALOS.BrandingSchema}
        branding-pending:object{DALOS.BrandingSchema}
        owner-konto:string
        name:string
        ticker:string
        decimals:integer
        ;;TM
        can-change-owner:bool                  
        can-upgrade:bool
        can-add-special-role:bool
        can-freeze:bool
        can-wipe:bool
        can-pause:bool
        is-paused:bool
        can-transfer-nft-create-role:bool
        ;;Supply
        supply:decimal
        ;;Roles
        create-role-account:string
        role-transfer-amount:integer
        ;;Units
        nonces-used:integer
        ;;ATS-VST
        reward-bearing-token:string
        vesting:string
    )
    (defschema DPMF|BalanceSchema
        @doc "Key = <DPMF id> + UTILS.BAR + <account>"
        unit:[object{DPMF|Schema}]
        ;;Special Roles
        role-nft-add-quantity:bool
        role-nft-burn:bool
        role-nft-create:bool
        role-transfer:bool
        ;;States
        frozen:bool
    )
    (defschema DPMF|RoleSchema
        r-nft-burn:[string]
        r-nft-create:[string]
        r-nft-add-quantity:[string]
        r-transfer:[string]
        a-frozen:[string]
    )
    (defschema DPMF|Schema
        nonce:integer
        balance:decimal
        meta-data:[object]
    )
    (defconst DPMF|NEUTRAL
        { "nonce": 0
        , "balance": 0.0
        , "meta-data": [{}] }
    )
    (defconst DPMF|NEGATIVE
        { "nonce": -1
        , "balance": -1.0
        , "meta-data": [{}] }
    )
    (defschema DPMF|Nonce-Balance
        nonce:integer
        balance:decimal
    )

    (deftable PoliciesTable:{DALOS.PolicySchema}) 
    (deftable DPTF|PropertiesTable:{DPTF|PropertiesSchema})
    (deftable DPTF|BalanceTable:{DALOS.DPTF|BalanceSchema})
    (deftable DPTF|RoleTable:{DPTF|RoleSchema})
    (deftable DPMF|PropertiesTable:{DPMF|PropertiesSchema})
    (deftable DPMF|BalanceTable:{DPMF|BalanceSchema})
    (deftable DPMF|RoleTable:{DPMF|RoleSchema})
    ;;[CAP]
    (defun DPTF-DPMF|CAP_Owner (id:string token-type:bool)
        (DALOS.DALOS|CAP_EnforceAccountOwnership (DPTF-DPMF|UR_Konto id token-type))
    )
    ;;[CF]
    (defcap DPTF-DPMF|CF|OWNER (id:string token-type:bool)
        (DPTF-DPMF|CAP_Owner id token-type)
    )
    ;;[UEV]
    (defun ATS|UEV_UpdateRewardBearingToken (id:string token-type:bool)
        (if (= token-type false)
            (let
                (
                    (rbt:string (DPMF|UR_RewardBearingToken id))
                )
                (enforce (= rbt UTILS.BAR) "RBT for a DPMF is immutable")
            )
            true
        )
    )
    (defun DPTF-DPMF|UEV_CanChangeOwnerON (id:string token-type:bool)
        (let
            (
                (x:bool (DPTF-DPMF|UR_CanChangeOwner id token-type))
            )
            (enforce (= x true) (format "DPTF|DPMF Token {} ownership cannot be changed" [id]))
        )
    )
    (defun DPTF-DPMF|UEV_CanUpgradeON (id:string token-type:bool)
        (let
            (
                (x:bool (DPTF-DPMF|UR_CanUpgrade id token-type))
            )
            (enforce (= x true) (format "DPTF|DPMF Token {} properties cannot be upgraded" [id])
            )
        )
    )
    (defun DPTF-DPMF|UEV_CanAddSpecialRoleON (id:string token-type:bool)
        (let
            (
                (x:bool (DPTF-DPMF|UR_CanAddSpecialRole id token-type))
            )
            (enforce (= x true) (format "For DPTF|DPMF Token {} no special roles can be added" [id])
            )
        )
    )
    (defun DPTF-DPMF|UEV_CanFreezeON (id:string token-type:bool)
        (let
            (
                (x:bool (DPTF-DPMF|UR_CanFreeze id token-type))
            )
            (enforce (= x true) (format "DPTF|DPMF Token {} cannot be freezed" [id])
            )
        )
    )
    (defun DPTF-DPMF|UEV_CanWipeON (id:string token-type:bool)
        (let
            (
                (x:bool (DPTF-DPMF|UR_CanWipe id token-type))
            )
            (enforce (= x true) (format "DPTF|DPMF Token {} cannot be wiped" [id])
            )
        )
    )
    (defun DPTF-DPMF|UEV_CanPauseON (id:string token-type:bool)
        (let
            (
                (x:bool (DPTF-DPMF|UR_CanPause id token-type))
            )
            (enforce (= x true) (format "DPTF|DPMF Token {} cannot be paused" [id])
            )
        )
    )
    (defun DPTF-DPMF|UEV_PauseState (id:string state:bool token-type:bool)
        (let
            (
                (x:bool (DPTF-DPMF|UR_Paused id token-type))
            )
            (if (= state true)
                (enforce (= x true) (format "DPTF|DPMF Token {} is already unpaused" [id]))
                (enforce (= x false) (format "DPTF|DPMF Token {} is already paused" [id]))
            )
        )
    )
    (defun DPTF-DPMF|UEV_AccountBurnState (id:string account:string state:bool token-type:bool)
        (let
            (
                (x:bool (DPTF-DPMF|UR_AccountRoleBurn id account token-type))
            )
            (enforce (= x state) (format "Burn Role for {} on Account {} must be set to {} for this operation" [id account state]))
        )
    )
    (defun DPTF-DPMF|UEV_AccountTransferState (id:string account:string state:bool token-type:bool)
        (let
            (
                (x:bool (DPTF-DPMF|UR_AccountRoleTransfer id account token-type))
            )
            (enforce (= x state) (format "Transfer Role for {} on Account {} must be set to {} for this operation" [id account state]))
        )
    )
    (defun DPTF-DPMF|UEV_AccountFreezeState (id:string account:string state:bool token-type:bool)
        (let
            (
                (x:bool (DPTF-DPMF|UR_AccountFrozenState id account token-type))
            )
            (enforce (= x state) (format "Frozen for {} on Account {} must be set to {} for this operation" [id account state]))
        )
    )
    (defun DPTF-DPMF|UEV_Amount (id:string amount:decimal token-type:bool)
        (let
            (
                (decimals:integer (DPTF-DPMF|UR_Decimals id token-type))
            )
            (enforce
                (= (floor amount decimals) amount)
                (format "The amount of {} does not conform with the {} decimals number" [amount id])
            )
            (enforce
                (> amount 0.0)
                (format "The amount of {} is not a Valid Transaction amount" [amount])
            )
        )
    )
    (defun DPTF-DPMF|UEV_CheckID:bool (id:string token-type:bool)
        (if token-type
            (with-default-read DPTF|PropertiesTable id
                { "supply" : -1.0 }
                { "supply" := s }
                (if (>= s 0.0)
                    true
                    false
                )
            )
            (with-default-read DPMF|PropertiesTable id
                { "supply" : -1.0 }
                { "supply" := s }
                (if (>= s 0.0)
                    true
                    false
                )
            )
        )
    )
    (defun DPTF-DPMF|UEV_id (id:string token-type:bool)
        (if token-type
            (with-default-read DPTF|PropertiesTable id
                { "supply" : -1.0 }
                { "supply" := s }
                (enforce
                    (>= s 0.0)
                    (format "DPTF Token with id {} does not exist." [id])
                )
            )
            (with-default-read DPMF|PropertiesTable id
                { "supply" : -1.0 }
                { "supply" := s }
                (enforce
                    (>= s 0.0)
                    (format "DPMF Token with id {} does not exist." [id])
                )
            )
        )
    )
    (defun DPTF|UEV_Virgin (id:string)
        (let
            (
                (om:bool (DPTF|UR_OriginMint id))
                (oma:decimal (DPTF|UR_OriginAmount id))
            )
            (enforce
                (and (= om false) (= oma 0.0))
                (format "Origin Mint for DPTF {} cannot be executed any more !" [id])
            )
        )
    )
    (defun DPTF|UEV_FeeLockState (id:string state:bool)
        (let
            (
                (x:bool (DPTF|UR_FeeLock id))
            )
            (enforce (= x state) (format "Fee-lock for {} must be set to {} for this operation" [id state]))
        )
    )
    (defun DPTF|UEV_FeeToggleState (id:string state:bool)
        (let
            (
                (x:bool (DPTF|UR_FeeToggle id))
            )
            (enforce (= x state) (format "Fee-Toggle for {} must be set to {} for this operation" [id state]))
        )
    )
    (defun DPTF|UEV_AccountMintState (id:string account:string state:bool)
        (let
            (
                (x:bool (DPTF|UR_AccountRoleMint id account))
            )
            (enforce (= x state) (format "Mint Role for {} on Account {} must be set to {} for this operation" [id account state]))
        )
    )
    (defun DPTF|UEV_AccountFeeExemptionState (id:string account:string state:bool)
        (let
            (
                (x:bool (DPTF|UR_AccountRoleFeeExemption id account))
            )
            (enforce (= x state) (format "Fee-Exemption Role for {} on Account {} must be set to {} for this operation" [id account state]))
        )
    )
    (defun DPTF|UEV_EnforceMinimumAmount (id:string transfer-amount:decimal)
        (let*
            (
                (min-move-read:decimal (DPTF|UR_MinMove id))
                (precision:integer (DPTF-DPMF|UR_Decimals id true))
                (min-move:decimal 
                    (if (= min-move-read -1.0)
                        (floor (/ 1.0 (^ 10.0 (dec precision))) precision)
                        min-move-read
                    )
                )
            )
            (enforce (>= transfer-amount min-move) (format "The transfer-amount of {} is not a valid {} transfer amount" [transfer-amount id]))
        )
    )
    (defun VST|UEV_Existance (id:string token-type:bool existance:bool)
        (let
            (
                (has-vesting:bool (VST|UC_HasVesting id token-type))
            )
            (enforce (= has-vesting existance) (format "Vesting for the Token {} is not satisfied with existance {}" [id existance]))
        )
    )
    (defun VST|UC_HasVesting:bool (id:string token-type:bool)
        (if (= (DPTF-DPMF|UR_Vesting id token-type) UTILS.BAR)
            false
            true
        )
    )
    (defun DPMF|UEV_CanTransferNFTCreateRoleON (id:string)
        (let
            (
                (x:bool (DPMF|UR_CanTransferNFTCreateRole id))
            )
            (enforce (= x true) (format "DPMF Token {} cannot have its create role transfered" [id])
            )
        )
    )
    (defun DPMF|UEV_AccountAddQuantityState (id:string account:string state:bool)
        (let
            (
                (x:bool (DPMF|UR_AccountRoleNFTAQ id account))
            )
            (enforce (= x state) (format "Add Quantity Role for {} on Account {} must be set to {} for this operation" [id account state]))
        )
    )
    (defun DPMF|UEV_AccountCreateState (id:string account:string state:bool)
        (let
            (
                (x:bool (DPMF|UR_AccountRoleCreate id account))
            )
            (enforce (= x state) (format "Create Role for {} on Account {} must be set to {} for this operation" [id account state]))
        )
    )
    ;;[CAP]
    (defcap DPTF-DPMF|BURN (id:string client:string amount:decimal token-type:bool)
        @event
        (DALOS.DALOS|CAP_EnforceAccountOwnership client)
        (compose-capability (DPTF-DPMF|X_BURN id client amount token-type))
        (compose-capability (IGNIS|MATRON_SOFT id client))
        (compose-capability (P|DIN))
    )
    (defcap DPTF-DPMF|X_BURN (id:string client:string amount:decimal token-type:bool)
        (DPTF-DPMF|UEV_Amount id amount token-type)
        (DPTF-DPMF|UEV_AccountBurnState id client true token-type)
        (if token-type
            (compose-capability (DPTF|DEBIT))
            (compose-capability (DPMF|DEBIT))
        )
        (compose-capability (DPTF-DPMF|UPDATE_SUPPLY))
    )
    (defcap DPTF-DPMF|CONTROL (id:string token-type:bool)
        @event
        (compose-capability (DPTF-DPMF|X_CONTROL id token-type))
        (compose-capability (P|DINIC))
    )
    (defcap DPTF-DPMF|X_CONTROL (id:string token-type:bool)
        (DPTF-DPMF|CAP_Owner id token-type)
        (DPTF-DPMF|UEV_CanUpgradeON id token-type)
    )
    (defcap DPTF-DPMF|FROZEN-ACCOUNT (id:string account:string frozen:bool token-type:bool)
        @event
        (compose-capability (DPTF-DPMF|X_FROZEN-ACCOUNT id account frozen token-type))
        (compose-capability (BASIS|X_WRITE-ROLES id account 5 token-type))
        (compose-capability (P|DINIC))
    )
    (defcap DPTF-DPMF|X_FROZEN-ACCOUNT (id:string account:string frozen:bool token-type:bool)
        (DPTF-DPMF|CAP_Owner id token-type)
        (DPTF-DPMF|UEV_CanFreezeON id token-type)
        (DPTF-DPMF|UEV_AccountFreezeState id account (not frozen) token-type)
    )
    (defcap DPTF-DPMF|ISSUE ()
        (compose-capability (P|DINIC))
    )
    (defcap DPTF|ISSUE (account:string name:[string] ticker:[string] decimals:[integer] can-change-owner:[bool] can-upgrade:[bool] can-add-special-role:[bool] can-freeze:[bool] can-wipe:[bool] can-pause:[bool])
        @event
        (let*
            (
                (l1:integer (length name))
                (l2:integer (length ticker))
                (l3:integer (length decimals))
                (l4:integer (length can-change-owner))
                (l5:integer (length can-upgrade))
                (l6:integer (length can-add-special-role))
                (l7:integer (length can-freeze)) 
                (l8:integer (length can-wipe))
                (l9:integer (length can-pause))
                (lengths:[integer] [l1 l2 l3 l4 l5 l6 l7 l8 l9])
            )
            (UTILS.UTILS|UEV_EnforceUniformIntegerList lengths)
            (UTILS.LIST|UC_IzUnique name)
            (UTILS.LIST|UC_IzUnique ticker)
            (DALOS.DALOS|CAP_EnforceAccountOwnership account)
        )
    )
    (defcap DPMF|ISSUE (account:string name:[string] ticker:[string] decimals:[integer] can-change-owner:[bool] can-upgrade:[bool] can-add-special-role:[bool] can-freeze:[bool] can-wipe:[bool] can-pause:[bool] can-transfer-nft-create-role:[bool])
        @event
        (let*
            (
                (l1:integer (length name))
                (l2:integer (length ticker))
                (l3:integer (length decimals))
                (l4:integer (length can-change-owner))
                (l5:integer (length can-upgrade))
                (l6:integer (length can-add-special-role))
                (l7:integer (length can-freeze)) 
                (l8:integer (length can-wipe))
                (l9:integer (length can-pause))
                (l0:integer (length can-transfer-nft-create-role))
                (lengths:[integer] [l1 l2 l3 l4 l5 l6 l7 l8 l9 l0])
            )
            (UTILS.UTILS|UEV_EnforceUniformIntegerList lengths)
            (UTILS.LIST|UC_IzUnique name)
            (UTILS.LIST|UC_IzUnique ticker)
            (DALOS.DALOS|CAP_EnforceAccountOwnership account)
        )
    )
    (defcap DPTF-DPMF|OWNERSHIP-CHANGE (id:string new-owner:string token-type:bool)
        @event
        (compose-capability (DPTF-DPMF|X_OWNERSHIP-CHANGE id new-owner token-type))
        (compose-capability (P|DINIC))
    )
    (defcap DPTF-DPMF|X_OWNERSHIP-CHANGE (id:string new-owner:string token-type:bool)
        (DALOS.DALOS|UEV_SenderWithReceiver (DPTF-DPMF|UR_Konto id token-type) new-owner)
        (DPTF-DPMF|CAP_Owner id token-type)
        (DPTF-DPMF|UEV_CanChangeOwnerON id token-type)
        (DALOS.DALOS|UEV_EnforceAccountExists new-owner)
    )
    (defcap DPTF-DPMF|X_TOGGLE_BURN-ROLE (id:string account:string toggle:bool token-type:bool)
        @event
        (if toggle
            (DPTF-DPMF|UEV_CanAddSpecialRoleON id token-type)
            true
        )
        (DPTF-DPMF|CAP_Owner id token-type)
        (DPTF-DPMF|UEV_AccountBurnState id account (not toggle) token-type)
    )
    (defcap DPTF-DPMF|TOGGLE_PAUSE (id:string pause:bool token-type:bool)
        @event
        (compose-capability (DPTF-DPMF|X_TOGGLE_PAUSE id pause token-type))
        (compose-capability (P|DINIC))
    )
    (defcap DPTF-DPMF|X_TOGGLE_PAUSE (id:string pause:bool token-type:bool)
        (if pause
            (DPTF-DPMF|UEV_CanPauseON id token-type)
            true
        )
        (DPTF-DPMF|CAP_Owner id token-type)
        (DPTF-DPMF|UEV_PauseState id (not pause) token-type)
    )
    (defcap DPTF-DPMF|TOGGLE_TRANSFER-ROLE (patron:string id:string account:string toggle:bool token-type:bool)
        @event
        (compose-capability (DPTF-DPMF|X_TOGGLE_TRANSFER-ROLE id account toggle token-type))
        (compose-capability (DPTF-DPMF|UPDATE_ROLE-TRANSFER-AMOUNT))
        (compose-capability (P|DINIC))
        (compose-capability (BASIS|X_WRITE-ROLES id account 4 token-type))
    )
    (defcap DPTF-DPMF|X_TOGGLE_TRANSFER-ROLE (id:string account:string toggle:bool token-type:bool)
        (enforce (!= account DALOS.OUROBOROS|SC_NAME) (format "{} Account is immune to transfer roles" [DALOS.OUROBOROS|SC_NAME]))
        (enforce (!= account DALOS.DALOS|SC_NAME) (format "{} Account is immune to transfer roles" [DALOS.DALOS|SC_NAME]))
        (if toggle
            (DPTF-DPMF|UEV_CanAddSpecialRoleON id token-type)
            true
        )
        (DPTF-DPMF|CAP_Owner id token-type)
        (DPTF-DPMF|UEV_AccountTransferState id account (not toggle) token-type)
    )
    (defcap DPTF-DPMF|WIPE (patron:string id:string account-to-be-wiped:string token-type:bool)
        @event
        (compose-capability (DPTF-DPMF|X_WIPE id account-to-be-wiped token-type))
        (compose-capability (IGNIS|MATRON_SOFT id account-to-be-wiped))
        (compose-capability (P|DIN))
    )
    (defcap DPTF-DPMF|X_WIPE (id:string account-to-be-wiped:string token-type:bool)
        (DPTF-DPMF|UEV_CanWipeON id token-type)
        (DPTF-DPMF|UEV_AccountFreezeState id account-to-be-wiped true token-type)
        (if token-type
            (compose-capability (DPTF|DEBIT))
            (compose-capability (DPMF|DEBIT))
        )
        (compose-capability (DPTF-DPMF|UPDATE_SUPPLY))
    )
    ;;
    (defcap DPTF|MINT (id:string client:string amount:decimal origin:bool)
        (DALOS.DALOS|CAP_EnforceAccountOwnership client)
        (if origin
            (compose-capability (DPTF|MINT-ORIGIN id client amount))
            (compose-capability (DPTF|MINT-STANDARD id client amount))
        )
    )
    (defcap DPTF|MINT-ORIGIN (id:string client:string amount:decimal)
        @event
        (compose-capability (DPTF|X_MINT-ORIGIN id amount))
        (compose-capability (IGNIS|MATRON_SOFT id client))
        (compose-capability (P|DIN))
    )
    (defcap DPTF|X_MINT-ORIGIN (id:string amount:decimal)
        (DPTF-DPMF|CAP_Owner id true)
        (DPTF|UEV_Virgin id)
        (compose-capability (DPTF|MINT_GENERAL id amount))
    )
    (defcap DPTF|MINT-STANDARD (id:string client:string amount:decimal)
        @event
        (compose-capability (DPTF|X_MINT-STANDARD id client amount))
        (compose-capability (IGNIS|MATRON_SOFT id client))
        (compose-capability (P|DIN))
    )
    (defcap DPTF|X_MINT-STANDARD (id:string client:string amount:decimal )
        (DPTF|UEV_AccountMintState id client true)
        (compose-capability (DPTF|MINT_GENERAL id amount))
    )
    (defcap DPTF|MINT_GENERAL (id:string amount:decimal)
        (DPTF-DPMF|UEV_Amount id amount true)
        (compose-capability (DPTF|CREDIT))
        (compose-capability (DPTF-DPMF|UPDATE_SUPPLY))
    )
    (defcap DPTF|SET_FEE (patron:string id:string fee:decimal)
        @event
        (compose-capability (DPTF|X_SET_FEE id fee))
        (compose-capability (P|DINIC))
    )
    (defcap DPTF|X_SET_FEE (id:string fee:decimal)
        (UTILS.DALOS|UEV_Fee fee)
        (DPTF-DPMF|CAP_Owner id true)
        (DPTF|UEV_FeeLockState id false)
    )
    (defcap DPTF|SET_FEE-TARGET (patron:string id:string target:string)
        @event
        (compose-capability (DPTF|X_SET_FEE-TARGET id target))
        (compose-capability (P|DINIC))
    )
    (defcap DPTF|X_SET_FEE-TARGET (id:string target:string)
        (DALOS.DALOS|UEV_EnforceAccountExists target)
        (DPTF-DPMF|CAP_Owner id true)
        (DPTF|UEV_FeeLockState id false) 
    )
    (defcap DPTF|SET_MIN-MOVE (patron:string id:string min-move-value:decimal)
        @event
        (compose-capability (DPTF|X_SET_MIN-MOVE id min-move-value))
        (compose-capability (P|DINIC))
    )
    (defcap DPTF|X_SET_MIN-MOVE (id:string min-move-value:decimal)
        (let
            (
                (decimals:integer (DPTF-DPMF|UR_Decimals id true))
            )
            (enforce
                (= (floor min-move-value decimals) min-move-value)
                (format "Min tr amount {} does not conform with the {} DPTF dec. no." [min-move-value id])
            )
            (enforce (or (= min-move-value -1.0) (> min-move-value 0.0)) "Min-Move Value does not compute")
            (DPTF-DPMF|CAP_Owner id true)
            (DPTF|UEV_FeeLockState id false)
        )
    )
    (defcap DPTF|TOGGLE_FEE (patron:string id:string toggle:bool)
        @event
        (compose-capability (DPTF|X_TOGGLE_FEE id toggle))
        (compose-capability (P|DINIC))
    )
    (defcap DPTF|X_TOGGLE_FEE (id:string toggle:bool)
        (let
            (
                (fee-promile:decimal (DPTF|UR_FeePromile id))
            )
            (enforce (or (= fee-promile -1.0) (and (>= fee-promile 0.0) (<= fee-promile 1000.0))) "Please Set up Fee Promile before Turning Fee Collection on !")
            (DALOS.DALOS|UEV_EnforceAccountExists (DPTF|UR_FeeTarget id))
            (DPTF-DPMF|CAP_Owner id true)
            (DPTF|UEV_FeeLockState id false)
            (DPTF|UEV_FeeToggleState id (not toggle))
        )
    )
    (defcap DPTF|X_TOGGLE_FEE-EXEMPTION-ROLE (id:string account:string toggle:bool)
        @event
        (DPTF-DPMF|CAP_Owner id true)
        (DALOS.DALOS|UEV_EnforceAccountType account true)
        (if toggle
            (DPTF-DPMF|UEV_CanAddSpecialRoleON id true)
            true
        )
        (DPTF|UEV_AccountFeeExemptionState id account (not toggle))
    )
    (defcap DPTF|TOGGLE_FEE-LOCK (patron:string id:string toggle:bool)
        @event
        (compose-capability (DPTF|X_TOGGLE_FEE-LOCK id toggle))
        (compose-capability (P|DINIC))
        (compose-capability (DPTF|INCREMENT-LOCKS))
    )
    (defcap DPTF|X_TOGGLE_FEE-LOCK (id:string toggle:bool)
        (DPTF-DPMF|CAP_Owner id true)
        (DPTF|UEV_FeeLockState id (not toggle))
    )
    (defcap DPTF|X_TOGGLE_MINT-ROLE (id:string account:string toggle:bool)
        @event
        (DPTF-DPMF|CAP_Owner id true)
        (if toggle
            (DPTF-DPMF|UEV_CanAddSpecialRoleON id true)
            true
        )
        (DPTF|UEV_AccountMintState id account (not toggle))
    )
    ;;
    (defcap DPMF|ADD-QUANTITY (id:string client:string amount:decimal)
        @event
        (DALOS.DALOS|CAP_EnforceAccountOwnership client)
        (compose-capability (DPMF|X_ADD-QUANTITY id client amount))
        (compose-capability (IGNIS|MATRON_SOFT id client))
        (compose-capability (P|DIN))
    )
    (defcap DPMF|X_ADD-QUANTITY (id:string client:string amount:decimal)
        (DPTF-DPMF|UEV_Amount id amount false)
        (DPMF|UEV_AccountAddQuantityState id client true)
        (compose-capability (DPMF|CREDIT))
        (compose-capability (DPTF-DPMF|UPDATE_SUPPLY))
    )
    (defcap DPMF|CREATE (id:string client:string)
        @event
        (DALOS.DALOS|CAP_EnforceAccountOwnership client)
        (compose-capability (DPMF|X_CREATE id client))
        (compose-capability (IGNIS|MATRON_SOFT id client))
        (compose-capability (P|DIN))
    )
    (defcap DPMF|X_CREATE (id:string client:string)
        (DPMF|UEV_AccountCreateState id client true)
        (compose-capability (DPMF|INCREMENT_NONCE))
    )
    (defcap DPMF|MINT (id:string client:string amount:decimal)
        @event
        (DALOS.DALOS|CAP_EnforceAccountOwnership client)
        (compose-capability (DPMF|X_MINT id client amount))
        (compose-capability (IGNIS|MATRON_SOFT id client))
        (compose-capability (P|DIN))
    )
    (defcap DPMF|X_MINT (id:string client:string amount:decimal)
        (compose-capability (DPMF|X_CREATE id client))
        (compose-capability (DPMF|X_ADD-QUANTITY id client amount))
    )
    (defcap DPMF|X_MOVE_CREATE-ROLE (id:string receiver:string)
        @event
        (DALOS.DALOS|UEV_SenderWithReceiver (DPMF|UR_CreateRoleAccount id) receiver)
        (DPTF-DPMF|CAP_Owner id false)
        (DPMF|UEV_CanTransferNFTCreateRoleON id)
        (DPMF|UEV_AccountCreateState id (DPMF|UR_CreateRoleAccount id) true)
        (DPMF|UEV_AccountCreateState id receiver false)
    )
    (defcap DPMF|X_TOGGLE_ADD-QUANTITY-ROLE (id:string account:string toggle:bool)
        (DPTF-DPMF|CAP_Owner id false)
        (DPMF|UEV_AccountAddQuantityState id account (not toggle))
        (if toggle
            (DPTF-DPMF|UEV_CanAddSpecialRoleON id false)
            true
        )
    )
    (defcap DPMF|TRANSFER (id:string sender:string receiver:string transfer-amount:decimal method:bool)
        @event
        (compose-capability (DPMF|X_TRANSFER id sender receiver transfer-amount method))
        (compose-capability (IGNIS|MATRON_STRONG id sender receiver))
        (compose-capability (P|DIN))
        (compose-capability (DALOS|EXECUTOR))
    )
    (defcap DPMF|X_TRANSFER (id:string sender:string receiver:string transfer-amount:decimal method:bool)
        (DALOS.DALOS|CAP_EnforceAccountOwnership sender)
        (if (and method (DALOS.DALOS|UR_AccountType receiver))
            (DALOS.DALOS|CAP_EnforceAccountOwnership receiver)
            true
        )
        (DPTF-DPMF|UEV_Amount id transfer-amount false)
        (DALOS.DALOS|UEV_EnforceTransferability sender receiver method)
        (DPTF-DPMF|UEV_PauseState id false false)
        (DPTF-DPMF|UEV_AccountFreezeState id sender false false)
        (DPTF-DPMF|UEV_AccountFreezeState id receiver false false)
        (if
            (and 
                (> (DPTF-DPMF|UR_TransferRoleAmount id false) 0) 
                (not (or (= sender DALOS.OUROBOROS|SC_NAME)(= sender DALOS.DALOS|SC_NAME)))
            )
            (let
                (
                    (s:bool (DPTF-DPMF|UR_AccountRoleTransfer id sender false))
                    (r:bool (DPTF-DPMF|UR_AccountRoleTransfer id receiver false))
                )
                (enforce-one
                    (format "Neither the sender {} nor the receiver {} have an active transfer role" [sender receiver])
                    [
                        (enforce (= s true) (format "Transfer-Role doesnt check for sender {}" [sender]))
                        (enforce (= r true) (format "Transfer-Role doesnt check for sender {}" [sender]))
                    ]
                )
            )
            (format "No transfer restrictions exist when transfering {} from {} to {}" [id sender receiver])
        )
        (compose-capability (DPMF|DEBIT))
        (compose-capability (DPMF|CREDIT))
        (compose-capability (P|DALOS|UPDATE_ELITE))
    )
    ;;
    (defcap IGNIS|MATRON_SOFT (id:string client:string)
        (if (DALOS.IGNIS|URC_ZeroGAS id client)
            true
            (compose-capability (P|IC))
        )
    )
    (defcap IGNIS|MATRON_STRONG (id:string client:string target:string)
        (if (DALOS.IGNIS|URC_ZeroGAZ id client target)
            true
            (compose-capability (P|IC))
        )
    )
    ;;[UR]
    (defun DPTF|P-KEYS:[string] ()
        (keys DPTF|PropertiesTable)
    )
    (defun DPMF|P-KEYS:[string] ()
        (keys DPMF|PropertiesTable)
    )
    (defun DPTF|KEYS:[string] ()
        (keys DPTF|BalanceTable)
    )
    (defun DPMF|KEYS:[string] ()
        (keys DPMF|BalanceTable)
    )
    (defun BASIS|UR_Branding:object{DALOS.BrandingSchema} (id:string token-type:bool pending:bool)
        (DPTF-DPMF|UEV_id id token-type)
        (if token-type
            (if pending
                (with-read DPTF|PropertiesTable id
                    { "branding-pending" := b }
                    b
                )
                (with-read DPTF|PropertiesTable id
                    { "branding" := b }
                    b
                )
            )
            (if pending
                (with-read DPMF|PropertiesTable id
                    { "branding-pending" := b }
                    b
                )
                (with-read DPMF|PropertiesTable id
                    { "branding" := b }
                    b
                )
            )
        )
    )
    (defun BASIS|URB_Logo:string (id:string token-type:bool pending:bool)
        (at "logo" (BASIS|UR_Branding id token-type pending))
    )
    (defun BASIS|URB_Description:string (id:string token-type:bool pending:bool)
        (at "description" (BASIS|UR_Branding id token-type pending))
    )
    (defun BASIS|URB_Website:string (id:string token-type:bool pending:bool)
        (at "website" (BASIS|UR_Branding id token-type pending))
    )
    (defun BASIS|URB_Social:[object{SocialSchema}] (id:string token-type:bool pending:bool)
        (at "social" (BASIS|UR_Branding id token-type pending))
    )
    (defun BASIS|URB_Flag:integer (id:string token-type:bool pending:bool)
        (at "flag" (BASIS|UR_Branding id token-type pending))
    )
    (defun BASIS|URB_Genesis:time (id:string token-type:bool pending:bool)
        (at "genesis" (BASIS|UR_Branding id token-type pending))
    )
    (defun BASIS|URB_PremiumUntil:time (id:string token-type:bool pending:bool)
        (at "premium-until" (BASIS|UR_Branding id token-type pending))
    )
    (defun DPTF-DPMF|UR_Roles:[string] (id:string rp:integer token-type:bool)
        (if token-type
            (if (= rp 1)
                (with-default-read DPTF|RoleTable id
                    { "r-burn" : [UTILS.BAR]}
                    { "r-burn" := rb }
                    rb
                )
                (if (= rp 2)
                    (with-default-read DPTF|RoleTable id
                        { "r-mint" : [UTILS.BAR]}
                        { "r-mint" := rm }
                        rm
                    )
                    (if (= rp 3)
                        (with-default-read DPTF|RoleTable id
                            { "r-fee-exemption" : [UTILS.BAR]}
                            { "r-fee-exemption" := rfe }
                            rfe
                        )
                        (if (= rp 4)
                            (with-default-read DPTF|RoleTable id
                                { "r-transfer" : [UTILS.BAR]}
                                { "r-transfer" := rt }
                                rt
                            )
                            (with-default-read DPTF|RoleTable id
                                { "a-frozen" : [UTILS.BAR]}
                                { "a-frozen" := af }
                                af
                            )
                        )
                    )
                )
            )
            (if (= rp 1)
                (with-default-read DPMF|RoleTable id
                    { "r-burn" : [UTILS.BAR]}
                    { "r-burn" := rb }
                    rb
                )
                (if (= rp 2)
                    (with-default-read DPMF|RoleTable id
                        { "r-nft-create" : [UTILS.BAR]}
                        { "r-nft-create" := rnc }
                        rnc
                    )
                    (if (= rp 3)
                        (with-default-read DPMF|RoleTable id
                            { "r-nft-add-quantity" : [UTILS.BAR]}
                            { "r-nft-add-quantity" := rnaq }
                            rnaq
                        )
                        (if (= rp 4)
                            (with-default-read DPMF|RoleTable id
                                { "r-transfer" : [UTILS.BAR]}
                                { "r-transfer" := rt }
                                rt
                            )
                            (with-default-read DPMF|RoleTable id
                                { "a-frozen" : [UTILS.BAR]}
                                { "a-frozen" := af }
                                af
                            )
                        )
                    )
                )
            )
        )
    )
    (defun DPTF-DPMF|UR_AccountFrozenState:bool (id:string account:string token-type:bool)
        (if token-type
            (if (DALOS|UC_IzCoreDPTF id)
                (if (= id (DALOS.DALOS|UR_OuroborosID))
                    (DALOS.DALOS|UR_TrueFungible_AccountFreezeState account true)
                    (DALOS.DALOS|UR_TrueFungible_AccountFreezeState account false)
                )
                (with-default-read DPTF|BalanceTable (concat [id UTILS.BAR account])
                    { "frozen" : false}
                    { "frozen" := fr }
                    fr
                )
            )
            (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR account])
                { "frozen" : false}
                { "frozen" := fr }
                fr
            )
        )
    )
    (defun DPTF-DPMF|UR_AccountRoleBurn:bool (id:string account:string token-type:bool)
        (if token-type
            (if (DALOS|UC_IzCoreDPTF id)
                (if (= id (DALOS.DALOS|UR_OuroborosID))
                    (DALOS.DALOS|UR_TrueFungible_AccountRoleBurn account true)
                    (DALOS.DALOS|UR_TrueFungible_AccountRoleBurn account false)
                )
                (with-default-read DPTF|BalanceTable (concat [id UTILS.BAR account])
                    { "role-burn" : false}
                    { "role-burn" := rb }
                    rb
                )
            )
            (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR account])
                { "role-nft-burn" : false}
                { "role-nft-burn" := rb }
                rb
            )
        )
    )
    (defun DPTF-DPMF|UR_AccountRoleTransfer:bool (id:string account:string token-type:bool)
        (if token-type
            (if (DALOS|UC_IzCoreDPTF id)
                (if (= id (DALOS.DALOS|UR_OuroborosID))
                    (DALOS.DALOS|UR_TrueFungible_AccountRoleTransfer account true)
                    (DALOS.DALOS|UR_TrueFungible_AccountRoleTransfer account false)
                )
                (with-default-read DPTF|BalanceTable (concat [id UTILS.BAR account])
                    { "role-transfer" : false}
                    { "role-transfer" := rt }
                    rt
                )
            )
            (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR account])
                { "role-transfer" : false }
                { "role-transfer" := rt }
                rt
            )
        )
    )
    (defun DPTF-DPMF|UR_AccountSupply:decimal (id:string account:string token-type:bool)
        (if token-type
            (if (DALOS|UC_IzCoreDPTF id)
                (if (= id (DALOS.DALOS|UR_OuroborosID))
                    (DALOS.DALOS|UR_TrueFungible_AccountSupply account true)
                    (DALOS.DALOS|UR_TrueFungible_AccountSupply account false)
                )
                (with-default-read DPTF|BalanceTable (concat [id UTILS.BAR account])
                    { "balance" : 0.0 }
                    { "balance" := b}
                    b
                )
            )
            (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR  account])
                { "unit" : [DPMF|NEUTRAL] }
                { "unit" := read-unit}
                (let 
                    (
                        (result 
                            (fold
                                (lambda 
                                    (acc:decimal item:object{DPMF|Schema})
                                    (+ acc (at "balance" item))
                                )
                                0.0
                                read-unit
                            )
                        )
                    )
                    result
                )
            )
        )
    )
    (defun DPTF-DPMF|UR_CanAddSpecialRole:bool (id:string token-type:bool)
        (if token-type
            (at "can-add-special-role" (read DPTF|PropertiesTable id ["can-add-special-role"]))
            (at "can-add-special-role" (read DPMF|PropertiesTable id ["can-add-special-role"]))
        )
    )
    (defun DPTF-DPMF|UR_CanChangeOwner:bool (id:string token-type:bool)
        (if token-type
            (at "can-change-owner" (read DPTF|PropertiesTable id ["can-change-owner"]))
            (at "can-change-owner" (read DPMF|PropertiesTable id ["can-change-owner"]))
        )
    )
    (defun DPTF-DPMF|UR_CanFreeze:bool (id:string token-type:bool)
        (if token-type
            (at "can-freeze" (read DPTF|PropertiesTable id ["can-freeze"]))
            (at "can-freeze" (read DPMF|PropertiesTable id ["can-freeze"]))
        )
    )
    (defun DPTF-DPMF|UR_CanPause:bool (id:string token-type:bool)
        (if token-type
            (at "can-pause" (read DPTF|PropertiesTable id ["can-pause"]))
            (at "can-pause" (read DPMF|PropertiesTable id ["can-pause"]))
        )
    )
    (defun DPTF-DPMF|UR_CanUpgrade:bool (id:string token-type:bool)
        (if token-type
            (at "can-upgrade" (read DPTF|PropertiesTable id ["can-upgrade"]))
            (at "can-upgrade" (read DPMF|PropertiesTable id ["can-upgrade"]))
        )   
    )
    (defun DPTF-DPMF|UR_CanWipe:bool (id:string token-type:bool)
        (if token-type
            (at "can-wipe" (read DPTF|PropertiesTable id ["can-wipe"]))
            (at "can-wipe" (read DPMF|PropertiesTable id ["can-wipe"]))
        )
    )
    (defun DPTF-DPMF|UR_Decimals:integer (id:string token-type:bool)
        (if token-type
            (at "decimals" (read DPTF|PropertiesTable id ["decimals"]))
            (at "decimals" (read DPMF|PropertiesTable id ["decimals"]))
        )
    )
    (defun DPTF-DPMF|UR_Konto:string (id:string token-type:bool)
        (if token-type
            (at "owner-konto" (read DPTF|PropertiesTable id ["owner-konto"]))
            (at "owner-konto" (read DPMF|PropertiesTable id ["owner-konto"]))
        )
    )
    (defun DPTF-DPMF|UR_Name:string (id:string token-type:bool)
        (if token-type
            (at "name" (read DPTF|PropertiesTable id ["name"]))
            (at "name" (read DPMF|PropertiesTable id ["name"]))
        )
    )
    (defun DPTF-DPMF|UR_Paused:bool (id:string token-type:bool)
        (if token-type
            (at "is-paused" (read DPTF|PropertiesTable id ["is-paused"]))
            (at "is-paused" (read DPMF|PropertiesTable id ["is-paused"]))
        )
    )
    (defun DPTF-DPMF|UR_Supply:decimal (id:string token-type:bool)
        (if token-type
            (at "supply" (read DPTF|PropertiesTable id ["supply"]))
            (at "supply" (read DPMF|PropertiesTable id ["supply"]))
        )
    )
    (defun DPTF-DPMF|UR_Ticker:string (id:string token-type:bool)
        (if token-type
            (at "ticker" (read DPTF|PropertiesTable id ["ticker"]))
            (at "ticker" (read DPMF|PropertiesTable id ["ticker"]))   
        )
    )
    (defun DPTF-DPMF|UR_TransferRoleAmount:integer (id:string token-type:bool)
        (if token-type
            (at "role-transfer-amount" (read DPTF|PropertiesTable id ["role-transfer-amount"]))
            (at "role-transfer-amount" (read DPMF|PropertiesTable id ["role-transfer-amount"]))
        )
    )
    (defun DPTF-DPMF|UR_Vesting:string (id:string token-type:bool)
        (if token-type
            (at "vesting" (read DPTF|PropertiesTable id ["vesting"]))
            (at "vesting" (read DPMF|PropertiesTable id ["vesting"]))
        )
    )
    ;;
    (defun DPTF|UR_AccountRoleFeeExemption:bool (id:string account:string)
        (if (DALOS|UC_IzCoreDPTF id)
            (if (= id (DALOS.DALOS|UR_OuroborosID))
                (DALOS.DALOS|UR_TrueFungible_AccountRoleFeeExemption account true)
                (DALOS.DALOS|UR_TrueFungible_AccountRoleFeeExemption account false)
            )
            (with-default-read DPTF|BalanceTable (concat [id UTILS.BAR account])
                { "role-fee-exemption" : false}
                { "role-fee-exemption" := rfe }
                rfe
            )
        )
        
    )
    (defun DPTF|UR_AccountRoleMint:bool (id:string account:string)
        (if (DALOS|UC_IzCoreDPTF id)
            (if (= id (DALOS.DALOS|UR_OuroborosID))
                (DALOS.DALOS|UR_TrueFungible_AccountRoleMint account true)
                (DALOS.DALOS|UR_TrueFungible_AccountRoleMint account false)
            )
            (with-default-read DPTF|BalanceTable (concat [id UTILS.BAR account])
                { "role-mint" : false}
                { "role-mint" := rm }
                rm
            )
        )
    )
    (defun DPTF|UR_FeeLock:bool (id:string)
        (at "fee-lock" (read DPTF|PropertiesTable id ["fee-lock"]))
    )
    (defun DPTF|UR_FeePromile:decimal (id:string)
        (at "fee-promile" (read DPTF|PropertiesTable id ["fee-promile"]))
    )
    (defun DPTF|UR_FeeTarget:string (id:string)
        (at "fee-target" (read DPTF|PropertiesTable id ["fee-target"]))
    )
    (defun DPTF|UR_FeeToggle:bool (id:string)
        (at "fee-toggle" (read DPTF|PropertiesTable id ["fee-toggle"]))
    )
    (defun DPTF|UR_FeeUnlocks:integer (id:string)
        (at "fee-unlocks" (read DPTF|PropertiesTable id ["fee-unlocks"]))
    )
    (defun DPTF|UR_MinMove:decimal (id:string)
        (at "min-move" (read DPTF|PropertiesTable id ["min-move"]))
    )
    (defun DPTF|UR_OriginAmount:decimal (id:string)
        (at "origin-mint-amount" (read DPTF|PropertiesTable id ["origin-mint-amount"]))
    )
    (defun DPTF|UR_OriginMint:bool (id:string)
        (at "origin-mint" (read DPTF|PropertiesTable id ["origin-mint"]))
    )
    (defun DPTF|UR_PrimaryFeeVolume:decimal (id:string)
        (at "primary-fee-volume" (read DPTF|PropertiesTable id ["primary-fee-volume"]))
    )
    (defun DPTF|UR_RewardBearingToken:[string] (id:string)
        (at "reward-bearing-token" (read DPTF|PropertiesTable id ["reward-bearing-token"]))
    )
    (defun DPTF|UR_RewardToken:[string] (id:string)
        (at "reward-token" (read DPTF|PropertiesTable id ["reward-token"]))
    )
    (defun DPTF|UR_SecondaryFeeVolume:decimal (id:string)
        (at "secondary-fee-volume" (read DPTF|PropertiesTable id ["secondary-fee-volume"]))
    )
    ;;
    (defun DPMF|UR_AccountBalances:[decimal] (id:string account:string)
        (DPTF-DPMF|UEV_id id false)
        (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR  account])
            { "unit" : [DPMF|NEUTRAL] }
            { "unit" := read-unit}
            (let 
                (
                    (result 
                        (fold
                            (lambda 
                                (acc:[decimal] item:object{DPMF|Schema})
                                (if (!= (at "balance" item) 0.0)
                                        (UTILS.LIST|UC_AppendLast acc (at "balance" item))
                                        acc
                                )
                            )
                            []
                            read-unit
                        )
                    )
                )
                result
            )
        )
    )
    (defun DPMF|UR_AccountBatchMetaData (id:string nonce:integer account:string)
        (DPTF-DPMF|UEV_id id false)
        (with-default-read DPMF|BalanceTable (concat [id BAR  account])
            { "unit" : [DPMF|NEUTRAL] }
            { "unit" := read-unit}
            (let 
                (
                    (result-meta-data
                        (fold
                            (lambda 
                                (acc item:object{DPMF|Schema})
                                (let
                                    (
                                        (nonce-val:integer (at "nonce" item))
                                        (meta-data-val (at "meta-data" item))
                                    )
                                    (if (= nonce-val nonce)
                                        meta-data-val
                                        acc
                                    )
                                )
                            )
                            []
                            read-unit
                        )
                    )
                )
                result-meta-data
            )
        )
    )
    (defun DPMF|UR_AccountBatchSupply:decimal (id:string nonce:integer account:string)
        (DPTF-DPMF|UEV_id id false)
        (with-default-read DPMF|BalanceTable (concat [id BAR  account])
            { "unit" : [DPMF|NEUTRAL] }
            { "unit" := read-unit}
            (let 
                (
                    (result-balance:decimal
                        (fold
                            (lambda 
                                (acc:decimal item:object{DPMF|Schema})
                                (let
                                    (
                                        (nonce-val:integer (at "nonce" item))
                                        (balance-val:decimal (at "balance" item))
                                    )
                                    (if (= nonce-val nonce)
                                        balance-val
                                        acc
                                    )
                                )
                            )
                            0.0
                            read-unit
                        )
                    )
                )
                result-balance
            )
        )
    )
    (defun DPMF|UR_AccountNonces:[integer] (id:string account:string)
        (DPTF-DPMF|UEV_id id false)
        (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR  account])
            { "unit" : [DPMF|NEUTRAL] }
            { "unit" := read-unit}
            (let 
                (
                    (result 
                        (fold
                            (lambda 
                                (acc:[integer] item:object{DPMF|Schema})
                                (if (!= (at "nonce" item) 0)
                                        (UTILS.LIST|UC_AppendLast acc (at "nonce" item))
                                        acc
                                )
                            )
                            []
                            read-unit
                        )
                    )
                )
                result
            )
        )
    )
    (defun DPMF|UR_AccountRoleCreate:bool (id:string account:string)
        (DPTF-DPMF|UEV_id id false)
        (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR account])
            { "role-nft-create" : false}
            { "role-nft-create" := rnc }
            rnc
        )
    )
    (defun DPMF|UR_AccountRoleNFTAQ:bool (id:string account:string)
        (DPTF-DPMF|UEV_id id false)
        (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR account])
            { "role-nft-add-quantity" : false}
            { "role-nft-add-quantity" := rnaq }
            rnaq
        )
    )
    (defun DPMF|UR_AccountUnit:[object] (id:string account:string)
        (DPTF-DPMF|UEV_id id false)
        (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR account])
            { "unit" : DPMF|NEGATIVE}
            { "unit" := u }
            u
        )
    )
    (defun DPMF|UR_CanTransferNFTCreateRole:bool (id:string)
        (at "can-transfer-nft-create-role" (read DPMF|PropertiesTable id ["can-transfer-nft-create-role"]))
    )
    (defun DPMF|UR_CreateRoleAccount:string (id:string)
        (at "create-role-account" (read DPMF|PropertiesTable id ["create-role-account"]))
    )
    (defun DPMF|UR_NoncesUsed:integer (id:string)
        (at "nonces-used" (read DPMF|PropertiesTable id ["nonces-used"]))
    )
    (defun DPMF|UR_RewardBearingToken:string (id:string)
        (at "reward-bearing-token" (read DPMF|PropertiesTable id ["reward-bearing-token"]))
    )
    ;;[URC]& [UC]
    (defun ATS|UC_IzRT:bool (reward-token:string)
        (DPTF-DPMF|UEV_id reward-token true)
        (if (= (DPTF|UR_RewardToken reward-token) [UTILS.BAR])
            false
            true
        )
    )
    (defun ATS|UC_IzRTg:bool (atspair:string reward-token:string)
        (DPTF-DPMF|UEV_id reward-token true)
        (if (= (DPTF|UR_RewardToken reward-token) [UTILS.BAR])
            false
            (if (= (contains atspair (DPTF|UR_RewardToken reward-token)) true)
                true
                false
            )
        )
    )
    (defun ATS|UC_IzRBT:bool (reward-bearing-token:string cold-or-hot:bool)
        (DPTF-DPMF|UEV_id reward-bearing-token cold-or-hot)
        (if (= cold-or-hot true)
            (if (= (DPTF|UR_RewardBearingToken reward-bearing-token) [UTILS.BAR])
                false
                true
            )
            (if (= (DPMF|UR_RewardBearingToken reward-bearing-token) UTILS.BAR)
                false
                true
            )
        )
    )
    (defun ATS|UC_IzRBTg:bool (atspair:string reward-bearing-token:string cold-or-hot:bool)
        (DPTF-DPMF|UEV_id reward-bearing-token cold-or-hot)
        (if (= cold-or-hot true)
            (if (= (DPTF|UR_RewardBearingToken reward-bearing-token) [UTILS.BAR])
                false
                (if (= (contains atspair (DPTF|UR_RewardBearingToken reward-bearing-token)) true)
                    true
                    false
                )
            )
            (if (= (DPMF|UR_RewardBearingToken reward-bearing-token) UTILS.BAR)
                false
                (if (= (DPMF|UR_RewardBearingToken reward-bearing-token) atspair)
                    true
                    false
                )
            )
        )
    )
    (defun DALOS|UC_IzCoreDPTF:bool (id:string)
        (DPTF-DPMF|UEV_id id true)
        (let*
            (
                (ouro-id:string (DALOS.DALOS|UR_OuroborosID))
                (ignis-id:string (DALOS.DALOS|UR_IgnisID))
                (iz-ouro-defined:bool (not (= ouro-id UTILS.BAR)))
                (iz-ignis-defined:bool (not (= ignis-id UTILS.BAR)))
            )
            (if (not iz-ouro-defined)
                (if (not iz-ignis-defined)
                        false
                        (= id ignis-id)
                    )
                (if (= id ouro-id)
                    true
                    (if (not iz-ignis-defined)
                        false
                        (= id ignis-id)
                    )
                )
            )
        )
    )
    (defun DALOS|URC_EliteAurynzSupply (account:string)
        (if (!= (DALOS.DALOS|UR_EliteAurynID) UTILS.BAR)
            (let
                (
                    (ea-supply:decimal (DPTF-DPMF|UR_AccountSupply (DALOS.DALOS|UR_EliteAurynID) account true))
                    (vea:string (DPTF-DPMF|UR_Vesting (DALOS.DALOS|UR_EliteAurynID) true))
                )
                (if (!= vea UTILS.BAR)
                    (+ ea-supply (DPTF-DPMF|UR_AccountSupply vea account false))
                    ea-supply
                )
            )
            true
        )
    )
    (defun DPTF-DPMF|URC_AccountExist:bool (id:string account:string token-type:bool)
        (if token-type
            (with-default-read DPTF|BalanceTable (concat [id UTILS.BAR account])
                { "balance" : -1.0 }
                { "balance" := b}
                (if (= b -1.0)
                    false
                    true
                )
            )
            (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR account])
                { "unit" : [DPMF|NEGATIVE] }
                { "unit" := u}
                (if (= u [DPMF|NEGATIVE])
                    false
                    true
                )
            )
        )
    )
    (defun DPTF|UC_Fee:[decimal] (id:string amount:decimal)
        (let
            (
                (fee-toggle:bool (DPTF|UR_FeeToggle id))
            )
            (if (= fee-toggle false)
                [0.0 0.0 amount]
                (let*
                    (
                        (precision:integer (DPTF-DPMF|UR_Decimals id true))
                        (fee-promile:decimal (DPTF|UR_FeePromile id))
                        (fee-unlocks:integer (DPTF|UR_FeeUnlocks id))
                        (volumetric-fee:decimal (DPTF|UC_VolumetricTax id amount))
                        (primary-fee-value:decimal 
                            (if (= fee-promile -1.0)
                                volumetric-fee
                                (floor (* (/ fee-promile 1000.0) amount) precision)
                            ) 
                        )
                        (secondary-fee-value:decimal 
                            (if (= fee-unlocks 0)
                                0.0
                                (* (dec fee-unlocks) volumetric-fee)
                            )
                        )
                        (remainder:decimal (- amount (+ primary-fee-value secondary-fee-value)))
                    )
                    [primary-fee-value secondary-fee-value remainder]
                )
            )
        )
    )
    (defun DPTF|UC_TransferFeeAndMinException:bool (id:string sender:string receiver:string)
        (let*
            (
                (gas-id:string (DALOS.DALOS|UR_IgnisID))
                (sender-fee-exemption:bool (DPTF|UR_AccountRoleFeeExemption id sender))
                (receiver-fee-exemption:bool (DPTF|UR_AccountRoleFeeExemption id receiver))
                (token-owner:string (DPTF-DPMF|UR_Konto id true))
                (sender-t1:bool (or (= sender OUROBOROS|SC_NAME) (= sender DALOS|SC_NAME)))
                (sender-t2:bool (or (= sender token-owner)(= sender-fee-exemption true)))
                (iz-sender-exception:bool (or sender-t1 sender-t2))
                (receiver-t1:bool (or (= receiver OUROBOROS|SC_NAME) (= receiver DALOS|SC_NAME)))
                (receiver-t2:bool (or (= receiver token-owner)(= receiver-fee-exemption true)))
                (iz-receiver-exception:bool (or receiver-t1 receiver-t2))
                (are-members-exception (or iz-sender-exception iz-receiver-exception))
                (is-id-gas:bool (= id gas-id))
                (iz-exception:bool (or is-id-gas are-members-exception ))
            )
            iz-exception
        )
    )
    (defun DPTF|UC_UnlockPrice:[decimal] (id:string)
        (UTILS.DPTF|UC_UnlockPrice (DPTF|UR_FeeUnlocks id))
    )
    (defun DPTF|UC_VolumetricTax (id:string amount:decimal)
        (DPTF-DPMF|UEV_Amount id amount true)
        (UTILS.DPTF|UC_VolumetricTax (DPTF-DPMF|UR_Decimals id true) amount)
    )
    ;;[UCC]
    (defun DPMF|UCC_Compose:object{DPMF|Schema} (nonce:integer balance:decimal meta-data:[object])
        {"nonce" : nonce, "balance": balance, "meta-data" : meta-data}
    )
    (defun DPMF|UCC_Pair_Nonce-Balance:[object{DPMF|Nonce-Balance}] (nonce-lst:[integer] balance-lst:[decimal])
        (let
            (
                (nonce-length:integer (length nonce-lst))
                (balance-length:integer (length balance-lst))
            )
            (enforce (= nonce-length balance-length) "Nonce and Balance Lists are not of equal length")
            (zip (lambda (x:integer y:decimal) { "nonce": x, "balance": y }) nonce-lst balance-lst)
        )
    )
    ;;[C]
    (defun DPTF-DPMF|C_ChangeOwnership (patron:string id:string new-owner:string token-type:bool)
        (enforce-one
            "Ownership Change of DPTF|DPMF Account not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (DPTF-DPMF|OWNERSHIP-CHANGE id new-owner token-type)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (DPTF-DPMF|UR_Konto id token-type) DALOS.GAS_BIGGEST)
                true
            )
            (DPTF-DPMF|X_ChangeOwnership id new-owner token-type)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun DPTF-DPMF|C_DeployAccount (id:string account:string token-type:bool)
        (enforce-one
            "Deployment of DPTF|DPMF Account not allowed"
            [
                (enforce-guard (create-capability-guard (SECURE)))
                (enforce-guard (C_ReadPolicy "ATS|Sum"))
                (enforce-guard (C_ReadPolicy "ATSI|Caller"))
                (enforce-guard (C_ReadPolicy "ATSM|Caller"))
                (enforce-guard (C_ReadPolicy "VST|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (DALOS.DALOS|UEV_EnforceAccountExists account)
        (DPTF-DPMF|UEV_id id token-type)
        (if token-type
            (with-default-read DPTF|BalanceTable (concat [id UTILS.BAR account])
                { "balance"                             : 0.0
                , "role-burn"                           : false
                , "role-mint"                           : false
                , "role-transfer"                       : false
                , "role-fee-exemption"                  : false
                , "frozen"                              : false}
                { "balance"                             := b
                , "role-burn"                           := rb
                , "role-mint"                           := rm
                , "role-transfer"                       := rt
                , "role-fee-exemption"                  := rfe
                , "frozen"                              := f}
                (write DPTF|BalanceTable (concat [id UTILS.BAR account])
                    { "balance"                         : b
                    , "role-burn"                       : rb
                    , "role-mint"                       : rm
                    , "role-transfer"                   : rt
                    , "role-fee-exemption"              : rfe
                    , "frozen"                          : f}
                )
            )
            (let*
                (
                    (create-role-account:string (DPMF|UR_CreateRoleAccount id))
                    (role-nft-create-boolean:bool (if (= create-role-account account) true false))
                )
                (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR account])
                    { "unit" : [DPMF|NEUTRAL]
                    , "role-nft-add-quantity"           : false
                    , "role-nft-burn"                   : false
                    , "role-nft-create"                 : role-nft-create-boolean
                    , "role-transfer"                   : false
                    , "frozen"                          : false}
                    { "unit"                            := u
                    , "role-nft-add-quantity"           := rnaq
                    , "role-nft-burn"                   := rb
                    , "role-nft-create"                 := rnc
                    , "role-transfer"                   := rt
                    , "frozen"                          := f }
                    (write DPMF|BalanceTable (concat [id UTILS.BAR account])
                        { "unit"                        : u
                        , "role-nft-add-quantity"       : rnaq
                        , "role-nft-burn"               : rb
                        , "role-nft-create"             : rnc
                        , "role-transfer"               : rt
                        , "frozen"                      : f}
                    )
                )
            )
        )
    )
    (defun DPTF-DPMF|C_ToggleFreezeAccount (patron:string id:string account:string toggle:bool token-type:bool)
        (enforce-one
            "Free Toggle of DPTF|DPMF Account not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (DPTF-DPMF|FROZEN-ACCOUNT patron id account toggle token-type)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron account DALOS.GAS_BIG)
                true
            )
            (DPTF-DPMF|X_ToggleFreezeAccount id account toggle token-type)
            (DPTF-DPMF|XP_WriteRoles id account 5 toggle token-type)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun DPTF-DPMF|C_TogglePause (patron:string id:string toggle:bool token-type:bool)
        (enforce-one
            "Pause Toggle of DPTF|DPMF Account not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (DPTF-DPMF|TOGGLE_PAUSE patron id toggle token-type)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron patron DALOS.GAS_MEDIUM)
                true
            )
            (DPTF-DPMF|X_TogglePause id toggle token-type)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun DPTF-DPMF|C_ToggleTransferRole (patron:string id:string account:string toggle:bool token-type:bool)
        (enforce-one
            "Toggle Transfer Role not allowed"
            [
                (enforce-guard (C_ReadPolicy "VST|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (DPTF-DPMF|TOGGLE_TRANSFER-ROLE patron id account toggle token-type)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron account DALOS.GAS_SMALL)
                true
            )
            (DPTF-DPMF|X_ToggleTransferRole id account toggle token-type)
            (DPTF-DPMF|X_UpdateRoleTransferAmount id toggle token-type)
            (DPTF-DPMF|XP_WriteRoles id account 4 toggle token-type)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun DPTF-DPMF|C_Wipe (patron:string id:string atbw:string token-type:bool)
        (enforce-one
            "Wipe of DPTF|DPMF Account not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (DPTF-DPMF|WIPE patron id atbw token-type)
            (if (not (DALOS.IGNIS|URC_ZeroGAS id atbw))
                (DALOS.IGNIS|X_Collect patron atbw DALOS.GAS_BIGGEST)
                true
            )
            (DPTF-DPMF|X_Wipe id atbw token-type)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    ;;
    (defun DPTF|C_Burn (patron:string id:string account:string amount:decimal)
        (enforce-one
            "DPTF Burn not allowed"
            [
                (enforce-guard (C_ReadPolicy "ATSC|Caller"))
                (enforce-guard (C_ReadPolicy "ATSH|Caller"))
                (enforce-guard (C_ReadPolicy "LIQUID|Summoner"))
                (enforce-guard (C_ReadPolicy "OUROBOROS|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
            ]
        )
        (with-capability (DPTF-DPMF|BURN id account amount true)
            (if (not (DALOS.IGNIS|URC_ZeroGAS id account))
                (DALOS.IGNIS|X_Collect patron account DALOS.GAS_SMALL)
                true
            )
            (DPTF|XP_Burn id account amount)
            (DALOS.DALOS|X_IncrementNonce account)
        )
    )
    (defun DPTF|C_Control (patron:string id:string cco:bool cu:bool casr:bool cf:bool cw:bool cp:bool)
        (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
        (with-capability (DPTF-DPMF|CONTROL patron id true)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (DPTF-DPMF|UR_Konto id true) DALOS.GAS_SMALL)
                true
            )
            (DPTF|X_Control patron id cco cu casr cf cw cp)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun DPTF|C_Issue:[string] (patron:string account:string name:[string] ticker:[string] decimals:[integer] can-change-owner:[bool] can-upgrade:[bool] can-add-special-role:[bool] can-freeze:[bool] can-wipe:[bool] can-pause:[bool])
        (enforce-one
            "DPTF Issue not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "DEPLOYER|Summoner"))
                
            ]
        )
        (with-capability (DPTF|ISSUE account name ticker decimals can-change-owner can-upgrade can-add-special-role can-freeze can-wipe can-pause)
            (let*
                (
                    (l1:integer (length name))
                    (tf-cost:decimal (DALOS.DALOS|UR_UsagePrice "dptf"))
                    (gas-costs:decimal (* (dec l1) DALOS.GAS_ISSUE))
                    (kda-costs:decimal (* (dec l1) tf-cost))
                )
                (with-capability (DPTF-DPMF|ISSUE)
                    (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                        (DALOS.IGNIS|X_Collect patron account gas-costs)
                        true
                    )
                    (if (not (DALOS.IGNIS|URC_IsNativeGasZero))
                        (DALOS.DALOS|C_TransferRawDalosFuel patron kda-costs)
                        true
                    )
                    (fold
                        (lambda
                            (acc:[string] index:integer)
                            (let
                                (
                                    (id:string
                                        (DPTF|X_Issue 
                                            account 
                                            (at index name)
                                            (at index ticker)
                                            (at index decimals)
                                            (at index can-change-owner)
                                            (at index can-upgrade)
                                            (at index can-add-special-role)
                                            (at index can-freeze)
                                            (at index can-wipe) 
                                            (at index can-pause)
                                        )
                                    )
                                )
                                (DALOS.DALOS|X_IncrementNonce patron)
                                (UTILS.LIST|UC_AppendLast acc id)
                            )
                        )
                        []
                        (enumerate 0 (- (length name) 1))
                    )
                )
            )
        )
    )
    (defun DPTF|C_Mint (patron:string id:string account:string amount:decimal origin:bool)
        (enforce-one
            "DPTF Issue not permitted"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "ATSH|Caller"))
                (enforce-guard (C_ReadPolicy "ATSM|Caller"))
                (enforce-guard (C_ReadPolicy "LIQUID|Summoner"))
                (enforce-guard (C_ReadPolicy "OUROBOROS|Summoner"))
            ]
        )
        (with-capability (DPTF|MINT id account amount origin)
            (if (not (DALOS.IGNIS|URC_ZeroGAS id account))
                (if origin
                    (DALOS.IGNIS|X_Collect patron account DALOS.GAS_BIGGEST)
                    (DALOS.IGNIS|X_Collect patron account DALOS.GAS_SMALL)
                )
                true
            )
            (DPTF|X_Mint id account amount origin)
            (DALOS.DALOS|X_IncrementNonce account)
        )
    )
    (defun DPTF|C_SetFee (patron:string id:string fee:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
        (with-capability (DPTF|SET_FEE patron id fee)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (DPTF-DPMF|UR_Konto id true) DALOS.GAS_SMALL)
                true
            )
            (DPTF|X_SetFee id fee)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun DPTF|C_SetFeeTarget (patron:string id:string target:string)
        (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
        (with-capability (DPTF|SET_FEE-TARGET patron id target)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (DPTF-DPMF|UR_Konto id true) DALOS.GAS_SMALL)
                true
            )
            (DPTF|X_SetFeeTarget id target)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun DPTF|C_SetMinMove (patron:string id:string min-move-value:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
        (with-capability (DPTF|SET_MIN-MOVE patron id min-move-value)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (DPTF-DPMF|UR_Konto id true) DALOS.GAS_SMALL)
                true
            )
            (DPTF|X_SetMinMove id min-move-value)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun DPTF|C_ToggleFee (patron:string id:string toggle:bool)
        (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
        (with-capability (DPTF|TOGGLE_FEE patron id toggle)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (DPTF-DPMF|UR_Konto id true) DALOS.GAS_SMALL)
                true
            )
            (DPTF|X_ToggleFee id toggle)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun DPTF|C_ToggleFeeLock (patron:string id:string toggle:bool)
        (enforce-one
            "Toggling Fee Lock not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "DEPLOYER|Summoner"))
            ]
        )
        (let
            (
                (ZG:bool (DALOS.IGNIS|URC_IsVirtualGasZero))
                (NZG:bool (IGNIS|URC_IsNativeGasZero))
                (token-owner:string (DPTF-DPMF|UR_Konto id true))
            )
            (with-capability (DPTF|TOGGLE_FEE-LOCK patron id toggle)
                (if (not ZG)
                    (DALOS.IGNIS|X_Collect patron token-owner DALOS.GAS_SMALL)
                    true
                )
                (let*
                    (
                        (toggle-costs:[decimal] (DPTF|X_ToggleFeeLock id toggle))
                        (gas-costs:decimal (at 0 toggle-costs))
                        (kda-costs:decimal (at 1 toggle-costs))
                    )
                    (if (> gas-costs 0.0)
                        (with-capability (COMPOSE)
                            (if (not ZG)
                                (DALOS.IGNIS|X_Collect patron token-owner gas-costs)
                                true
                            )
                            (if (not NZG)
                                (DALOS.DALOS|C_TransferRawDalosFuel patron kda-costs)
                                true
                            )
                            (DPTF|X_IncrementFeeUnlocks id)
                        )
                        true
                    )
                )
                (DALOS.DALOS|X_IncrementNonce patron)
            )
        )
    )
    ;;
    (defun DPMF|C_AddQuantity (patron:string id:string nonce:integer account:string amount:decimal)
        (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
        (with-capability (DPMF|ADD-QUANTITY id account amount)
            (if (not (DALOS.IGNIS|URC_ZeroGAS id account))
                (DALOS.IGNIS|X_Collect patron account DALOS.GAS_SMALL)
                true
            )
            (DPMF|X_AddQuantity id nonce account amount)
            (DALOS.DALOS|X_IncrementNonce account)
        )
    )
    (defun DPMF|C_Burn (patron:string id:string nonce:integer account:string amount:decimal)
        (enforce-one
            "DPMF Burn not permitted"
            [
                (enforce-guard (C_ReadPolicy "ATSH|Caller"))
                (enforce-guard (C_ReadPolicy "VST|Sum"))
                (enforce-guard (C_ReadPolicy "OUROBOROS|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (DPTF-DPMF|BURN id account amount false)
            (if (not (DALOS.IGNIS|URC_ZeroGAS id account))
                (DALOS.IGNIS|X_Collect patron account DALOS.GAS_SMALL)
                true
            )
            (DPMF|X_Burn id nonce account amount)
            (DALOS.DALOS|X_IncrementNonce account)
        )
    )
    (defun DPMF|C_Control (patron:string id:string cco:bool cu:bool casr:bool cf:bool cw:bool cp:bool ctncr:bool)
        (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
        (with-capability (DPTF-DPMF|CONTROL id false)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (DPTF-DPMF|UR_Konto id false) DALOS.GAS_SMALL)
                true
            )
            (DPMF|X_Control id cco cu casr cf cw cp ctncr)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    (defun DPMF|C_Create:integer (patron:string id:string account:string meta-data:[object])
        (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
        (with-capability (DPMF|CREATE id account)
            (if (not (DALOS.IGNIS|URC_ZeroGAS id account))
                (DALOS.IGNIS|X_Collect patron account DALOS.GAS_SMALL)
                true
            )
            (DALOS.DALOS|X_IncrementNonce account)
            (DPMF|X_Create id account meta-data)
        )
    )
    (defun DPMF|C_Issue:[string] (patron:string account:string name:[string] ticker:[string] decimals:[integer] can-change-owner:[bool] can-upgrade:[bool] can-add-special-role:[bool] can-freeze:[bool] can-wipe:[bool] can-pause:[bool] can-transfer-nft-create-role:[bool])
        (enforce-one
            "DPTF Multi Transfer not permitted"
            [
                (enforce-guard (C_ReadPolicy "VST|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (DPMF|ISSUE account name ticker decimals can-change-owner can-upgrade can-add-special-role can-freeze can-wipe can-pause can-transfer-nft-create-role)
            (let*
                (
                    (l1:integer (length name))
                    (tf-cost:decimal (DALOS.DALOS|UR_UsagePrice "dpmf"))
                    (gas-costs:decimal (* (dec l1) DALOS.GAS_ISSUE))
                    (kda-costs:decimal (* (dec l1) tf-cost))
                )
                (with-capability (DPTF-DPMF|ISSUE)
                    (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                        (DALOS.IGNIS|X_Collect patron account gas-costs)
                        true
                    )
                    (if (not (DALOS.IGNIS|URC_IsNativeGasZero))
                        (DALOS.DALOS|C_TransferRawDalosFuel patron kda-costs)
                        true
                    )
                    (fold
                        (lambda
                            (acc:[string] index:integer)
                            (let
                                (
                                    (id:string
                                        (DPMF|X_Issue
                                            account 
                                            (at index name)
                                            (at index ticker)
                                            (at index decimals)
                                            (at index can-change-owner)
                                            (at index can-upgrade)
                                            (at index can-add-special-role)
                                            (at index can-freeze)
                                            (at index can-wipe) 
                                            (at index can-pause)
                                            (at index can-transfer-nft-create-role)
                                        )
                                    )
                                )
                                (DALOS.DALOS|X_IncrementNonce patron)
                                (UTILS.LIST|UC_AppendLast acc id)
                            )
                        )
                        []
                        (enumerate 0 (- (length name) 1))
                    )
                )
            )
        ) 
    )
    (defun DPMF|C_Mint:integer (patron:string id:string account:string amount:decimal meta-data:[object])
        (enforce-one
            "DPMF Mint not permitted"
            [
                (enforce-guard (C_ReadPolicy "ATSH|Caller"))
                (enforce-guard (C_ReadPolicy "VST|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (DPMF|MINT id account amount)
            (if (not (DALOS.IGNIS|URC_ZeroGAS id account))
                (DALOS.IGNIS|X_Collect patron account DALOS.GAS_SMALL)
                true
            )
            (DALOS.DALOS|X_IncrementNonce account)
            (DPMF|X_Mint id account amount meta-data)
        )
    )
    (defun DPMF|C_Transfer (patron:string id:string nonce:integer sender:string receiver:string transfer-amount:decimal method:bool)
        (enforce-one
            "DPMF Transfer not permitted"
            [
                (enforce-guard (C_ReadPolicy "ATSH|Caller"))
                (enforce-guard (C_ReadPolicy "VST|Summoner"))
                (enforce-guard (C_ReadPolicy "OUROBOROS|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (DPMF|TRANSFER id sender receiver transfer-amount method)
            (DPMF|XK_Transfer patron id nonce sender receiver transfer-amount method)
        )
    )
    ;;[X]
    (defun DPTF-DPMF|X_UpdateBranding (id:string token-type:bool pending:bool branding:object{DALOS.BrandingSchema})
        (enforce-guard (C_ReadPolicy "BRD|Update"))
        (if token-type
            (if pending
                (update DPTF|PropertiesTable id
                    {"branding-pending" : branding}
                )
                (update DPTF|PropertiesTable id
                    {"branding" : branding}
                )
            )
            (if pending
                (update DPMF|PropertiesTable id
                    {"branding-pending" : branding}
                )
                (update DPMF|PropertiesTable id
                    {"branding" : branding}
                )
            )
        )
    )
    (defun DPTF-DPMF|X_ChangeOwnership (id:string new-owner:string token-type:bool)
        (require-capability (DPTF-DPMF|X_OWNERSHIP-CHANGE id new-owner token-type))
        (if token-type
            (update DPTF|PropertiesTable id
                {"owner-konto"                      : new-owner}
            )
            (update DPMF|PropertiesTable id
                {"owner-konto"                      : new-owner}
            )
        )
    )
    (defun DPTF-DPMF|X_ToggleFreezeAccount (id:string account:string toggle:bool token-type:bool)
        (require-capability (DPTF-DPMF|X_FROZEN-ACCOUNT id account toggle token-type))
        (if token-type
            (if (DALOS|UC_IzCoreDPTF id)
                (with-capability (P|DALOS|UP_DATA)
                    (DALOS.DALOS|XO_UpdateFreeze account (= id (DALOS.DALOS|UR_OuroborosID)) toggle)
                )
                (update DPTF|BalanceTable (concat [id UTILS.BAR account])
                    { "frozen" : toggle}
                )
            )
            (update DPMF|BalanceTable (concat [id UTILS.BAR account])
                { "frozen" : toggle}
            )
        )
    )
    (defun DPTF-DPMF|X_TogglePause (id:string toggle:bool token-type:bool)
        (require-capability (DPTF-DPMF|X_TOGGLE_PAUSE id toggle token-type))
        (if token-type
            (update DPTF|PropertiesTable id
                { "is-paused" : toggle}
            )
            (update DPMF|PropertiesTable id
                { "is-paused" : toggle}
            )
        )
    )
    (defun DPTF-DPMF|X_ToggleTransferRole (id:string account:string toggle:bool token-type:bool)
        (require-capability (DPTF-DPMF|X_TOGGLE_TRANSFER-ROLE id account toggle token-type))
        (if token-type
            (if (DALOS|UC_IzCoreDPTF id)
                (with-capability (P|DALOS|UP_DATA)
                    (DALOS.DALOS|XO_UpdateTransferRole account (= id (DALOS.DALOS|UR_OuroborosID)) toggle)
                )
                (update DPTF|BalanceTable (concat [id UTILS.BAR account])
                    {"role-transfer" : toggle}
                )
            )
            (update DPMF|BalanceTable (concat [id UTILS.BAR account])
                {"role-transfer" : toggle}
            )
        )
    )
    (defun DPTF-DPMF|X_UpdateElite (id:string sender:string receiver:string)
        (let
            (
                (ea-id:string (DALOS.DALOS|UR_EliteAurynID))
            )
            (if (!= ea-id UTILS.BAR)
                (if (= id ea-id)
                    (with-capability (P|DALOS|UPDATE_ELITE)
                        (DALOS.DALOS|X_UpdateElite sender (DALOS|URC_EliteAurynzSupply sender))
                        (DALOS.DALOS|X_UpdateElite receiver (DALOS|URC_EliteAurynzSupply receiver))
                    )
                    (let
                        (
                            (v-ea-id:string (DPTF-DPMF|UR_Vesting ea-id true))
                        )
                        (if (and (!= v-ea-id UTILS.BAR)(= id v-ea-id))
                            (with-capability (P|DALOS|UPDATE_ELITE)
                                (DALOS.DALOS|X_UpdateElite sender (DALOS|URC_EliteAurynzSupply sender))
                                (DALOS.DALOS|X_UpdateElite receiver (DALOS|URC_EliteAurynzSupply receiver))
                            )
                            true
                        )
                    )
                )
                true
            )
        )
    )
    (defun DPTF-DPMF|X_UpdateRoleTransferAmount (id:string direction:bool token-type:bool)
        (require-capability (DPTF-DPMF|UPDATE_ROLE-TRANSFER-AMOUNT))
        (if token-type
            (if (= direction true)
                (with-read DPTF|PropertiesTable id
                    { "role-transfer-amount" := rta }
                    (update DPTF|PropertiesTable id
                        {"role-transfer-amount" : (+ rta 1)}
                    )
                )
                (with-read DPTF|PropertiesTable id
                    { "role-transfer-amount" := rta }
                    (update DPTF|PropertiesTable id
                        {"role-transfer-amount" : (- rta 1)}
                    )
                )
            )
            (if (= direction true)
                (with-read DPMF|PropertiesTable id
                    { "role-transfer-amount" := rta }
                    (update DPMF|PropertiesTable id
                        {"role-transfer-amount" : (+ rta 1)}
                    )
                )
                (with-read DPMF|PropertiesTable id
                    { "role-transfer-amount" := rta }
                    (update DPMF|PropertiesTable id
                        {"role-transfer-amount" : (- rta 1)}
                    )
                )
            )
        )
    )
    (defun DPTF-DPMF|X_UpdateSupply (id:string amount:decimal direction:bool token-type:bool)
        (require-capability (DPTF-DPMF|UPDATE_SUPPLY))
        (DPTF-DPMF|UEV_Amount id amount token-type)
        (if token-type
            (if (= direction true)
                (with-read DPTF|PropertiesTable id
                    { "supply" := s }
                    (enforce (>= (+ s amount) 0.0) "DPTF Token Supply cannot be updated to negative values!")
                    (update DPTF|PropertiesTable id { "supply" : (+ s amount)})
                )
                (with-read DPTF|PropertiesTable id
                    { "supply" := s }
                    (enforce (>= (- s amount) 0.0) "DPTF Token Supply cannot be updated to negative values!")
                    (update DPTF|PropertiesTable id { "supply" : (- s amount)})
                )
            )
            (if (= direction true)
                (with-read DPMF|PropertiesTable id
                    { "supply" := s }
                    (enforce (>= (+ s amount) 0.0) "DPMF Token Supply cannot be updated to negative values!")
                    (update DPMF|PropertiesTable id { "supply" : (+ s amount)})
                )
                (with-read DPMF|PropertiesTable id
                    { "supply" := s }
                    (enforce (>= (- s amount) 0.0) "DPMF Token Supply cannot be updated to negative values!")
                    (update DPMF|PropertiesTable id { "supply" : (- s amount)})
                )
            )
        )
    )
    (defun DPTF-DPMF|X_Wipe (id:string account-to-be-wiped:string token-type:bool)
        (require-capability (DPTF-DPMF|X_WIPE id account-to-be-wiped token-type))
        (if token-type
            (let
                (
                    (amount-to-be-wiped:decimal (DPTF-DPMF|UR_AccountSupply id account-to-be-wiped token-type))
                )
                (DPTF|X_DebitAdmin id account-to-be-wiped amount-to-be-wiped)
                (DPTF-DPMF|X_UpdateSupply id amount-to-be-wiped false token-type)
            )
            (let*
                (
                    (nonce-lst:[integer] (DPMF|UR_AccountNonces id account-to-be-wiped))
                    (balance-lst:[decimal] (DPMF|UR_AccountBalances id account-to-be-wiped))
                    (balance-sum:decimal (fold (+) 0.0 balance-lst))
                )
                (DPMF|X_DebitMultiple id nonce-lst account-to-be-wiped balance-lst)
                (DPTF-DPMF|X_UpdateSupply id balance-sum false token-type)
            )
        )
    )
    (defun DPTF-DPMF|XO_ToggleBurnRole (id:string account:string toggle:bool token-type:bool)
        (enforce-guard (C_ReadPolicy "ATSI|TgBrRl"))
        (with-capability (DPTF-DPMF|X_TOGGLE_BURN-ROLE id account toggle token-type)
            (DPTF-DPMF|XP_ToggleBurnRole id account toggle token-type)
        )
    )
    (defun DPTF-DPMF|XP_ToggleBurnRole (id:string account:string toggle:bool token-type:bool)
        (require-capability (DPTF-DPMF|X_TOGGLE_BURN-ROLE id account toggle token-type))
        (if token-type
            (if (DALOS|UC_IzCoreDPTF id)
                (with-capability (P|DALOS|UP_DATA)
                    (DALOS.DALOS|XO_UpdateBurnRole account (= id (DALOS.DALOS|UR_OuroborosID)) toggle)
                )
                (update DPTF|BalanceTable (concat [id UTILS.BAR account])
                    {"role-burn" : toggle}
                )
            )
            (update DPMF|BalanceTable (concat [id UTILS.BAR account])
                {"role-nft-burn" : toggle}
            )
        )
    )
    (defun DPTF-DPMF|XO_UpdateRewardBearingToken (id:string atspair:string token-type:bool)
        (enforce-one
            "Invalid Permissions to update Reward Bearing Token"
            [
                (enforce-guard (C_ReadPolicy "ATS|Sum"))
                (enforce-guard (C_ReadPolicy "ATSI|Caller"))
                (enforce-guard (C_ReadPolicy "ATSM|Caller"))
            ]
        )
        (with-capability (ATS|UPDATE_RBT)
            (ATS|UEV_UpdateRewardBearingToken id token-type)
            (DPTF-DPMF|XP_UpdateRewardBearingToken id atspair token-type)
        )
    )
    (defun DPTF-DPMF|XP_UpdateRewardBearingToken (id:string atspair:string token-type:bool)
        (require-capability (ATS|UPDATE_RBT))
        (if token-type
            (with-read DPTF|PropertiesTable id
                {"reward-bearing-token" := rbt}
                (if (= (at 0 rbt) UTILS.BAR)
                    (update DPTF|PropertiesTable id
                        {"reward-bearing-token" : [atspair]}
                    )
                    (update DPTF|PropertiesTable id
                        {"reward-bearing-token" : (UTILS.LIST|UC_AppendLast rbt atspair)}
                    )
                )
            )
            (update DPMF|PropertiesTable id
                {"reward-bearing-token" : atspair}
            )
        )
    )
    (defun DPTF-DPMF|XO_UpdateVesting (dptf:string dpmf:string)
        (enforce-guard (C_ReadPolicy "VST|UpVes"))
        (with-capability (VST|UPDATE)
            (DPTF-DPMF|XP_UpdateVesting dptf dpmf)
        )
    )
    (defun DPTF-DPMF|XP_UpdateVesting (dptf:string dpmf:string)        
        (require-capability (VST|UPDATE))
        (update DPTF|PropertiesTable dptf
            {"vesting" : dpmf}
        )
        (update DPMF|PropertiesTable dpmf
            {"vesting" : dptf}
        )
    )
    ;;
    (defun DPTF|X_Control (id:string can-change-owner:bool can-upgrade:bool can-add-special-role:bool can-freeze:bool can-wipe:bool can-pause:bool)
        (require-capability (DPTF-DPMF|X_CONTROL id true))
        (update DPTF|PropertiesTable id
            {"can-change-owner"                 : can-change-owner
            ,"can-upgrade"                      : can-upgrade
            ,"can-add-special-role"             : can-add-special-role
            ,"can-freeze"                       : can-freeze
            ,"can-wipe"                         : can-wipe
            ,"can-pause"                        : can-pause}
        )
    )
    (defun DPTF|X_Issue:string (account:string name:string ticker:string decimals:integer can-change-owner:bool can-upgrade:bool can-add-special-role:bool can-freeze:bool can-wipe:bool can-pause:bool)
        (UTILS.DALOS|UEV_Decimals decimals)
        (UTILS.DALOS|UEV_TokenName name)
        (UTILS.DALOS|UEV_TickerName ticker)
        (require-capability (DPTF-DPMF|ISSUE))
        (insert DPTF|PropertiesTable (DALOS|UC_Makeid ticker)
                {"branding"             : BRANDING|DEFAULT
                ,"branding-pending"     : BRANDING|DEFAULT
                ,"owner-konto"          : account
                ,"name"                 : name
                ,"ticker"               : ticker
                ,"decimals"             : decimals
                ,"can-change-owner"     : can-change-owner
                ,"can-upgrade"          : can-upgrade
                ,"can-add-special-role" : can-add-special-role
                ,"can-freeze"           : can-freeze
                ,"can-wipe"             : can-wipe
                ,"can-pause"            : can-pause
                ,"is-paused"            : false
                ,"supply"               : 0.0
                ,"origin-mint"          : false
                ,"origin-mint-amount"   : 0.0
                ,"role-transfer-amount" : 0
                ,"fee-toggle"           : false
                ,"min-move"             : -1.0
                ,"fee-promile"          : 0.0
                ,"fee-target"           : DALOS.OUROBOROS|SC_NAME
                ,"fee-lock"             : false
                ,"fee-unlocks"          : 0
                ,"primary-fee-volume"   : 0.0
                ,"secondary-fee-volume" : 0.0
                ,"reward-token"         : [UTILS.BAR]
                ,"reward-bearing-token" : [UTILS.BAR]
                ,"vesting"              : UTILS.BAR
            }
        )
        (with-capability (SECURE)
            (DPTF-DPMF|C_DeployAccount (DALOS|UC_Makeid ticker) account true)    
        )
        (DALOS.DALOS|UC_Makeid ticker)
    )
    (defun DPTF|X_Mint (id:string account:string amount:decimal origin:bool)
        (if origin
            (require-capability (DPTF|X_MINT-ORIGIN id amount ))
            (require-capability (DPTF|X_MINT-STANDARD id account amount))
        )
        (DPTF|XO_Credit id account amount)
        (DPTF-DPMF|X_UpdateSupply id amount true true)
        (if origin
            (update DPTF|PropertiesTable id
                { "origin-mint" : false
                , "origin-mint-amount" : amount}
            )
            true
        )
    )
    (defun DPTF|X_SetFee (id:string fee:decimal)
        (require-capability (DPTF|X_SET_FEE id fee))
        (update DPTF|PropertiesTable id
            { "fee-promile" : fee}
        )
    )
    (defun DPTF|X_SetFeeTarget (id:string target:string)
        (require-capability (DPTF|X_SET_FEE-TARGET id target))
        (update DPTF|PropertiesTable id
            { "fee-target" : target}
        )
    )
    (defun DPTF|X_SetMinMove (id:string min-move-value:decimal)
        (require-capability (DPTF|X_SET_MIN-MOVE id min-move-value))
        (update DPTF|PropertiesTable id
            { "min-move" : min-move-value}
        )
    )
    (defun DPTF|X_ToggleFee (id:string toggle:bool)
        (require-capability (DPTF|X_TOGGLE_FEE id toggle))
        (update DPTF|PropertiesTable id
            { "fee-toggle" : toggle}
        )
    )
    (defun DPTF|X_ToggleFeeLock:[decimal] (id:string toggle:bool)
        (require-capability (DPTF|X_TOGGLE_FEE-LOCK id toggle))
        (update DPTF|PropertiesTable id
            { "fee-lock" : toggle}
        )
        (if (= toggle true)
            [0.0 0.0]
            (UTILS.DPTF|UC_UnlockPrice (DPTF|UR_FeeUnlocks id))
        )
    )
    (defun DPTF|X_IncrementFeeUnlocks (id:string)
        (require-capability (DPTF|INCREMENT-LOCKS))
        (with-read DPTF|PropertiesTable id
            { "fee-unlocks" := fu }
            (enforce (< fu 7) (format "Cannot increment Fee Unlocks for Token {}" [id]))
            (update DPTF|PropertiesTable id
                {"fee-unlocks" : (+ fu 1)}
            )
        )
    )
    (defun DPTF|XO_Burn (id:string account:string amount:decimal)
        (enforce-guard (C_ReadPolicy "TFT|BrTF"))
        (with-capability (DPTF-DPMF|X_BURN id account amount true)
            (DPTF|XP_Burn id account amount)
        )
    )
    (defun DPTF|XP_Burn (id:string account:string amount:decimal)
        (require-capability (DPTF-DPMF|X_BURN id account amount true))
        (DPTF|XO_DebitStandard id account amount)
        (DPTF-DPMF|X_UpdateSupply id amount false true)
    )
    (defun DPTF|XO_Credit (id:string account:string amount:decimal)
        (enforce-one
            "Credit Not permitted"
            [
                (enforce-guard (create-capability-guard (DPTF|CREDIT)))
                (enforce-guard (C_ReadPolicy "TFT|CrTF"))
            ]
        )
        (with-capability (DPTF|CREDIT_PUR)
            (DPTF|XP_Credit id account amount)
        )
    )
    (defun DPTF|XP_Credit (id:string account:string amount:decimal )
        (require-capability (DPTF|CREDIT_PUR))
        (if (DALOS|UC_IzCoreDPTF id)
            (let*
                (
                    (snake-or-gas:bool (if (= id (DALOS.DALOS|UR_OuroborosID)) true false))
                    (read-balance:decimal (DALOS.DALOS|UR_TrueFungible_AccountSupply account snake-or-gas))
                )
                (enforce (>= read-balance 0.0) "Impossible operation, negative Primordial TrueFungible amounts detected")
                (enforce (> amount 0.0) "Crediting amount must be greater than zero, even for Primordial TrueFungibles")
                (with-capability (P|DALOS|UP_BALANCE)
                    (DALOS.DALOS|XO_UpdateBalance account snake-or-gas (+ read-balance amount))
                )
            )
            (let
                (
                    (dptf-account-exist:bool (DPTF-DPMF|URC_AccountExist id account true))
                )
                (enforce (> amount 0.0) "Crediting amount must be greater than zero")
                (if (= dptf-account-exist false)
                    (insert DPTF|BalanceTable (concat [id UTILS.BAR account])
                        { "balance"                         : amount
                        , "role-burn"                       : false
                        , "role-mint"                       : false
                        , "role-transfer"                   : false
                        , "role-fee-exemption"              : false
                        , "frozen"                          : false
                        }
                    )
                    (with-read DPTF|BalanceTable (concat [id UTILS.BAR account])
                        { "balance"                         := b
                        , "role-burn"                       := rb
                        , "role-mint"                       := rm
                        , "role-transfer"                   := rt
                        , "role-fee-exemption"              := rfe
                        , "frozen"                          := f
                        }
                        (write DPTF|BalanceTable (concat [id UTILS.BAR account])
                            { "balance"                     : (+ b amount)
                            , "role-burn"                   : rb
                            , "role-mint"                   : rm
                            , "role-transfer"               : rt
                            , "role-fee-exemption"          : rfe
                            , "frozen"                      : f
                            }   
                        )
                    )
                )
            )
        )
    )
    (defun DPTF|X_DebitAdmin (id:string account:string amount:decimal)
        (require-capability (DPTF|DEBIT))
        (with-capability (DPTF|DEBIT_PUR)
            (DPTF-DPMF|CAP_Owner id true)
            (DPTF|XP_Debit id account amount)
        )
    )
    (defun DPTF|XO_DebitStandard (id:string account:string amount:decimal)
        (enforce-one
            "Standard Debit Not permitted"
            [
                (enforce-guard (create-capability-guard (DPTF|DEBIT)))
                (enforce-guard (C_ReadPolicy "TFT|DbTF"))
            ]
        )
        (with-capability (DPTF|DEBIT_PUR)
            (DALOS.DALOS|CAP_EnforceAccountOwnership account)
            (DPTF|XP_Debit id account amount)
        )
    )
    (defun DPTF|XP_Debit (id:string account:string amount:decimal)
        (require-capability (DPTF|DEBIT_PUR))
        (if (DALOS|UC_IzCoreDPTF id)
            (let*
                (
                    (snake-or-gas:bool (if (= id (DALOS.DALOS|UR_OuroborosID)) true false))
                    (read-balance:decimal (DALOS.DALOS|UR_TrueFungible_AccountSupply account snake-or-gas))
                )
                (enforce (>= read-balance 0.0) "Impossible operation, negative Primordial TrueFungible amounts detected")
                (enforce (<= amount read-balance) "Insufficient Funds for debiting")
                (with-capability (P|DALOS|UP_BALANCE)
                    (DALOS.DALOS|XO_UpdateBalance account snake-or-gas (- read-balance amount))
                )

            )
            (with-read DPTF|BalanceTable (concat [id UTILS.BAR account])
                { "balance" := balance }
                (enforce (<= amount balance) "Insufficient Funds for debiting")
                (update DPTF|BalanceTable (concat [id UTILS.BAR account])
                    {"balance" : (- balance amount)}    
                )
            )
        )
    )
    (defun DPTF|XO_ToggleFeeExemptionRole (id:string account:string toggle:bool)
        (enforce-guard (C_ReadPolicy "ATSI|TgFeRl"))
        (with-capability (DPTF|X_TOGGLE_FEE-EXEMPTION-ROLE id account toggle)
            (DPTF|XP_ToggleFeeExemptionRole id account toggle)
        )
    )
    (defun DPTF|XP_ToggleFeeExemptionRole (id:string account:string toggle:bool)
        (require-capability (DPTF|X_TOGGLE_FEE-EXEMPTION-ROLE id account toggle))
        (if (DALOS|UC_IzCoreDPTF id)
            (with-capability (P|DALOS|UP_DATA)
                (DALOS.DALOS|XO_UpdateFeeExemptionRole account (= id (DALOS.DALOS|UR_OuroborosID)) toggle)
            )
            (update DPTF|BalanceTable (concat [id UTILS.BAR account])
                {"role-fee-exemption" : toggle}
            )
        )
    )
    (defun DPTF|XO_ToggleMintRole (id:string account:string toggle:bool)
        (enforce-guard (C_ReadPolicy "ATSI|TgMnRl"))
        (with-capability (DPTF|X_TOGGLE_MINT-ROLE id account toggle)
            (DPTF|XP_ToggleMintRole id account toggle)
        )
    )
    (defun DPTF|XP_ToggleMintRole (id:string account:string toggle:bool)
        (require-capability (DPTF|X_TOGGLE_MINT-ROLE id account toggle))
        (if (DALOS|UC_IzCoreDPTF id)
            (with-capability (P|DALOS|UP_DATA)
                (DALOS.DALOS|XO_UpdateMintRole account (= id (DALOS.DALOS|UR_OuroborosID)) toggle)
            )
            (update DPTF|BalanceTable (concat [id UTILS.BAR account])
                {"role-mint" : toggle}
            )
        )
    )
    (defun DPTF|XO_UpdateFeeVolume (id:string amount:decimal primary:bool)
        (enforce-guard (C_ReadPolicy "TFT|UpFees"))
        (with-capability (DPTF|UPDATE_FEES_PUR)
            (DPTF-DPMF|UEV_Amount id amount true)
            (DPTF|XP_UpdateFeeVolume id amount primary)
        )
    )
    (defun DPTF|XP_UpdateFeeVolume (id:string amount:decimal primary:bool)
        (require-capability (DPTF|UPDATE_FEES_PUR))
        (if primary
            (with-read DPTF|PropertiesTable id
                { "primary-fee-volume" := pfv }
                (update DPTF|PropertiesTable id
                    {"primary-fee-volume" : (+ pfv amount)}
                )
            )
            (with-read DPTF|PropertiesTable id
                { "secondary-fee-volume" := sfv }
                (update DPTF|PropertiesTable id
                    {"secondary-fee-volume" : (+ sfv amount)}
                )
            )
        )
    )
    (defun DPTF|XO_UpdateRewardToken (atspair:string id:string direction:bool)
        (enforce-one
            "Invalid Permissions to update Reward Token"
            [
                (enforce-guard (C_ReadPolicy "ATS|Sum"))
                (enforce-guard (C_ReadPolicy "ATSI|Caller"))
                (enforce-guard (C_ReadPolicy "ATSM|Caller"))
            ]
        )
        (with-capability (DPTF|UPDATE_RT)
            (DPTF|XP_UpdateRewardToken atspair id direction)
        )
    )
    (defun DPTF|XP_UpdateRewardToken (atspair:string id:string direction:bool)
        (require-capability (DPTF|UPDATE_RT))
        (with-read DPTF|PropertiesTable id
            {"reward-token" := rt}
            (if (= direction true)
                (if (= (at 0 rt) UTILS.BAR)
                    (update DPTF|PropertiesTable id
                        {"reward-token" : [atspair]}
                    )
                    (update DPTF|PropertiesTable id
                        {"reward-token" : (UTILS.LIST|UC_AppendLast rt atspair)}
                    )
                )
                (update DPTF|PropertiesTable id
                    {"reward-token" : (UTILS.LIST|UC_RemoveItem rt atspair)}
                )
            )
        )
    )
    (defcap BASIS|X_WRITE-ROLES (id:string account:string rp:integer token-type:bool)
        (UTILS.UTILS|UEV_PositionalVariable rp 5 "Invalid Role Position")
        (DALOS.DALOS|UEV_EnforceAccountExists account)
        (DPTF-DPMF|UEV_id id token-type)
    )
    (defun BASIS|UC_NewRoleList (current-lst:[string] account:string direction:bool)
        (if direction
            (if 
                (= current-lst [UTILS.BAR])
                [account]
                (UTILS.LIST|UC_AppendLast current-lst account)
            )
            (if
                (= current-lst [UTILS.BAR])
                [UTILS.BAR]
                (UTILS.LIST|UC_RemoveItem current-lst account)
            )
        )
    )
    (defun DPTF-DPMF|XO_WriteRoles (id:string account:string rp:integer d:bool token-type:bool)
        (enforce-guard (C_ReadPolicy "ATSI|WR"))
        (with-capability (BASIS|X_WRITE-ROLES id account rp token-type)
            (DPTF-DPMF|XP_WriteRoles id account rp d token-type)
        )
    )
    (defun DPTF-DPMF|XP_WriteRoles (id:string account:string rp:integer d:bool token-type:bool)
        (require-capability (BASIS|X_WRITE-ROLES id account rp token-type))
        (if token-type
            (with-default-read DPTF|RoleTable id
                { "r-burn"          : [UTILS.BAR]
                , "r-mint"          : [UTILS.BAR]
                , "r-fee-exemption" : [UTILS.BAR]
                , "r-transfer"      : [UTILS.BAR]
                , "a-frozen"        : [UTILS.BAR]}
                { "r-burn"          := rb
                , "r-mint"          := rm
                , "r-fee-exemption" := rfe
                , "r-transfer"      := rt
                , "a-frozen"        := af}
                (if (= rp 1)
                    (write DPTF|RoleTable id
                        {"r-burn"           : (BASIS|UC_NewRoleList rb account d)
                        , "r-mint"          : rm
                        , "r-fee-exemption" : rfe
                        , "r-transfer"      : rt
                        , "a-frozen"        : af}
                    )
                    (if (= rp 2)
                        (write DPTF|RoleTable id
                            {"r-burn"           : rb
                            , "r-mint"          : (BASIS|UC_NewRoleList rm account d)
                            , "r-fee-exemption" : rfe
                            , "r-transfer"      : rt
                            , "a-frozen"        : af}
                        )
                        (if (= rp 3)
                            (write DPTF|RoleTable id
                                {"r-burn"           : rb
                                , "r-mint"          : rm
                                , "r-fee-exemption" : (BASIS|UC_NewRoleList rfe account d)
                                , "r-transfer"      : rt
                                , "a-frozen"        : af}
                            )
                            (if (= rp 4)
                                (write DPTF|RoleTable id
                                    {"r-burn"           : rb
                                    , "r-mint"          : rm
                                    , "r-fee-exemption" : rfe
                                    , "r-transfer"      : (BASIS|UC_NewRoleList rt account d)
                                    , "a-frozen"        : af}
                                )
                                (write DPTF|RoleTable id
                                    {"r-burn"           : rb
                                    , "r-mint"          : rm
                                    , "r-fee-exemption" : rfe
                                    , "r-transfer"      : rt
                                    , "a-frozen"        : (BASIS|UC_NewRoleList af account d)}
                                )
                            )
                        )
                    )
                )
            )
            (with-default-read DPMF|RoleTable id
                { "r-nft-burn"          : [UTILS.BAR]
                , "r-nft-create"        : [UTILS.BAR]
                , "r-nft-add-quantity"  : [UTILS.BAR]
                , "r-transfer"          : [UTILS.BAR]
                , "a-frozen"            : [UTILS.BAR]}
                { "r-nft-burn"          := rb
                , "r-nft-create"        := rnc
                , "r-nft-add-quantity"  := rnaq
                , "r-transfer"          := rt
                , "a-frozen"            := af}
                (if (= rp 1)
                    (write DPMF|RoleTable id
                        { "r-nft-burn"          : (BASIS|UC_NewRoleList rb account d)
                        , "r-nft-create"        : rnc
                        , "r-nft-add-quantity"  : rnaq
                        , "r-transfer"          : rt
                        , "a-frozen"            : af}
                    )
                    (if (= rp 2)
                        (write DPMF|RoleTable id
                            { "r-nft-burn"          : rb
                            , "r-nft-create"        : (BASIS|UC_NewRoleList rnc account d)
                            , "r-nft-add-quantity"  : rnaq
                            , "r-transfer"          : rt
                            , "a-frozen"            : af}
                        )
                        (if (= rp 3)
                            (write DPMF|RoleTable id
                                { "r-nft-burn"          : rb
                                , "r-nft-create"        : rnc
                                , "r-nft-add-quantity"  : (BASIS|UC_NewRoleList rnaq account d)
                                , "r-transfer"          : rt
                                , "a-frozen"            : af}
                            )
                            (if (= rp 4)
                                (write DPMF|RoleTable id
                                    { "r-nft-burn"          : rb
                                    , "r-nft-create"        : rnc
                                    , "r-nft-add-quantity"  : rnaq
                                    , "r-transfer"          : (BASIS|UC_NewRoleList rt account d)
                                    , "a-frozen"            : af}
                                )
                                (write DPMF|RoleTable id
                                    { "r-nft-burn"          : rb
                                    , "r-nft-create"        : rnc
                                    , "r-nft-add-quantity"  : rnaq
                                    , "r-transfer"          : rt
                                    , "a-frozen"            : (BASIS|UC_NewRoleList af account d)}
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    ;;
    (defun DPMF|X_AddQuantity (id:string nonce:integer account:string amount:decimal)
        (require-capability (DPMF|X_ADD-QUANTITY id account amount))
        (with-read DPMF|BalanceTable (concat [id UTILS.BAR account])
            { "unit" := unit }
            (let*
                (
                    (current-nonce-balance:decimal (DPMF|UR_AccountBatchSupply id nonce account))
                    (current-nonce-meta-data (DPMF|UR_AccountBatchMetaData id nonce account))
                    (updated-balance:decimal (+ current-nonce-balance amount))
                    (meta-fungible-to-be-replaced:object{DPMF|Schema} (DPMF|UCC_Compose nonce current-nonce-balance current-nonce-meta-data))
                    (updated-meta-fungible:object{DPMF|Schema} (DPMF|UCC_Compose nonce updated-balance current-nonce-meta-data))
                    (processed-unit:[object{DPMF|Schema}] (UTILS.LIST|UC_ReplaceItem unit meta-fungible-to-be-replaced updated-meta-fungible))
                )
                (update DPMF|BalanceTable (concat [id UTILS.BAR account])
                    {"unit" : processed-unit}    
                )
            )
        )
        (DPTF-DPMF|X_UpdateSupply id amount true false)
    )
    (defun DPMF|X_Burn (id:string nonce:integer account:string amount:decimal)
        (require-capability (DPTF-DPMF|X_BURN id account amount false))
        (DPMF|X_DebitStandard id nonce account amount)
        (DPTF-DPMF|X_UpdateSupply id amount false false)
    )
    (defun DPMF|X_Control
        (
            patron:string
            id:string
            can-change-owner:bool 
            can-upgrade:bool 
            can-add-special-role:bool 
            can-freeze:bool 
            can-wipe:bool 
            can-pause:bool
            can-transfer-nft-create-role:bool
        )
        (require-capability (DPTF-DPMF|X_CONTROL id false))
        (update DPMF|PropertiesTable id
            {"can-change-owner"                 : can-change-owner
            ,"can-upgrade"                      : can-upgrade
            ,"can-add-special-role"             : can-add-special-role
            ,"can-freeze"                       : can-freeze
            ,"can-wipe"                         : can-wipe
            ,"can-pause"                        : can-pause
            ,"can-transfer-nft-create-role"     : can-transfer-nft-create-role}
        )
    )
    (defun DPMF|X_Credit (id:string nonce:integer meta-data:[object] account:string amount:decimal)
        (require-capability (DPMF|CREDIT))
        (let*
            (
                (create-role-account:string (DPMF|UR_CreateRoleAccount id))
                (role-nft-create-boolean:bool (if (= create-role-account account) true false))
            )
            (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR account])
                { "unit" : [DPMF|NEGATIVE]
                , "role-nft-add-quantity" : false
                , "role-nft-burn" : false
                , "role-nft-create" : role-nft-create-boolean
                , "role-transfer" : false
                , "frozen" : false}
                { "unit" := unit
                , "role-nft-add-quantity" := rnaq
                , "role-nft-burn" := rb
                , "role-nft-create" := rnc
                , "role-transfer" := rt
                , "frozen" := f}
                (let*
                    (
                        (next-unit:[object] (if (= unit [DPMF|NEGATIVE]) [DPMF|NEUTRAL] unit))
                        (is-new:bool (if (= unit [DPMF|NEGATIVE]) true false))
                        (current-nonce-balance:decimal (DPMF|UR_AccountBatchSupply id nonce account))
                        (credited-balance:decimal (+ current-nonce-balance amount))
                        (present-meta-fungible:object{DPMF|Schema} (DPMF|UCC_Compose nonce current-nonce-balance meta-data))
                        (credited-meta-fungible:object{DPMF|Schema} (DPMF|UCC_Compose nonce credited-balance meta-data))
                        (processed-unit-with-replace:[object{DPMF|Schema}] (UTILS.LIST|UC_ReplaceItem next-unit present-meta-fungible credited-meta-fungible))
                        (processed-unit-with-append:[object{DPMF|Schema}] (UTILS.LIST|UC_AppendLast next-unit credited-meta-fungible))
                    )
                    (if (= current-nonce-balance 0.0)
                        (write DPMF|BalanceTable (concat [id UTILS.BAR account])
                            { "unit"                        : processed-unit-with-append
                            , "role-nft-add-quantity"       : (if is-new false rnaq)
                            , "role-nft-burn"               : (if is-new false rb)
                            , "role-nft-create"             : (if is-new role-nft-create-boolean rnc)
                            , "role-transfer"               : (if is-new false rt)
                            , "frozen"                      : (if is-new false f)}
                        )
                        (write DPMF|BalanceTable (concat [id UTILS.BAR account])
                            { "unit"                        : processed-unit-with-replace
                            , "role-nft-add-quantity"       : (if is-new false rnaq)
                            , "role-nft-burn"               : (if is-new false rb)
                            , "role-nft-create"             : (if is-new role-nft-create-boolean rnc)
                            , "role-transfer"               : (if is-new false rt)
                            , "frozen"                      : (if is-new false f)}
                        )
                    )
                )
            )
        )
    )
    (defun DPMF|X_Create:integer (id:string account:string meta-data:[object])
        (require-capability (DPMF|X_CREATE id account))
        (let*
            (
                (new-nonce:integer (+ (DPMF|UR_NoncesUsed id) 1))
                (create-role-account:string (DPMF|UR_CreateRoleAccount id))
                (role-nft-create-boolean:bool (if (= create-role-account account) true false))
            )
            (with-default-read DPMF|BalanceTable (concat [id UTILS.BAR account])
                { "unit" : [DPMF|NEUTRAL]
                , "role-nft-add-quantity" : false
                , "role-nft-burn" : false
                , "role-nft-create" : role-nft-create-boolean
                , "role-transfer" : false
                , "frozen" : false}
                { "unit" := u
                , "role-nft-add-quantity" := rnaq
                , "role-nft-burn" := rb
                , "role-nft-create" := rnc
                , "role-transfer" := rt
                , "frozen" := f}
                (let*
                    (
                        (new-nonce:integer (+ (DPMF|UR_NoncesUsed id) 1))
                        (meta-fungible:object{DPMF|Schema} (DPMF|UCC_Compose new-nonce 0.0 meta-data))
                        (appended-meta-fungible:[object{DPMF|Schema}] (UTILS.LIST|UC_AppendLast u meta-fungible))
                    )
                    (write DPMF|BalanceTable (concat [id UTILS.BAR account])
                        { "unit"                        : appended-meta-fungible
                        , "role-nft-add-quantity"       : rnaq
                        , "role-nft-burn"               : rb
                        , "role-nft-create"             : rnc
                        , "role-transfer"               : rt
                        , "frozen"                      : f}
                    )
                    (DPMF|X_IncrementNonce id)
                    new-nonce
                )
            )
        )
    )
    (defun DPMF|X_DebitAdmin (id:string nonce:integer account:string amount:decimal)
        (require-capability (DPMF|DEBIT))
        (with-capability (DPMF|DEBIT_PUR)
            (DPTF-DPMF|CAP_Owner id false)
            (DPMF|X_DebitPure id nonce account amount)
        )
    )
    (defun DPMF|X_DebitStandard (id:string nonce:integer account:string amount:decimal)
        (require-capability (DPMF|DEBIT))
        (with-capability (DPMF|DEBIT_PUR)
            (DALOS.DALOS|CAP_EnforceAccountOwnership account)
            (DPMF|X_DebitPure id nonce account amount)
        )
    )
    (defun DPMF|X_DebitPure (id:string nonce:integer account:string amount:decimal)
        (require-capability (DPMF|DEBIT_PUR))
        (with-read DPMF|BalanceTable (concat [id UTILS.BAR account])
            {"unit"                                 := unit  
            ,"role-nft-add-quantity"                := rnaq
            ,"role-nft-burn"                        := rnb
            ,"role-nft-create"                      := rnc
            ,"role-transfer"                        := rt
            ,"frozen"                               := f}
            (let*
                (
                    (current-nonce-balance:decimal (DPMF|UR_AccountBatchSupply id nonce account))
                    (current-nonce-meta-data (DPMF|UR_AccountBatchMetaData id nonce account))
                    (debited-balance:decimal (- current-nonce-balance amount))
                    (meta-fungible-to-be-replaced:object{DPMF|Schema} (DPMF|UCC_Compose nonce current-nonce-balance current-nonce-meta-data))
                    (debited-meta-fungible:object{DPMF|Schema} (DPMF|UCC_Compose nonce debited-balance current-nonce-meta-data))
                    (processed-unit-with-remove:[object{DPMF|Schema}] (UTILS.LIST|UC_RemoveItem unit meta-fungible-to-be-replaced))
                    (processed-unit-with-replace:[object{DPMF|Schema}] (UTILS.LIST|UC_ReplaceItem unit meta-fungible-to-be-replaced debited-meta-fungible))
                )
                (enforce (>= debited-balance 0.0) "Insufficient Funds for debiting")
                (if (= debited-balance 0.0)
                    (update DPMF|BalanceTable (concat [id UTILS.BAR account])
                        {"unit"                     : processed-unit-with-remove
                        ,"role-nft-add-quantity"    : rnaq
                        ,"role-nft-burn"            : rnb
                        ,"role-nft-create"          : rnc
                        ,"role-transfer"            : rt
                        ,"frozen"                   : f}
                    )
                    (update DPMF|BalanceTable (concat [id UTILS.BAR account])
                        {"unit"                     : processed-unit-with-replace
                        ,"role-nft-add-quantity"    : rnaq
                        ,"role-nft-burn"            : rnb
                        ,"role-nft-create"          : rnc
                        ,"role-transfer"            : rt
                        ,"frozen"                   : f}
                    )
                )
            )
        )
    )
    (defun DPMF|X_DebitMultiple (id:string nonce-lst:[integer] account:string balance-lst:[decimal])
        (let
            (
                (nonce-balance-obj-lst:[object{DPMF|Nonce-Balance}] (DPMF|UCC_Pair_Nonce-Balance nonce-lst balance-lst))
            )
            (map (lambda (x:object{DPMF|Nonce-Balance}) (DPMF|X_DebitPaired id account x)) nonce-balance-obj-lst)
        )
    )
    (defun DPMF|X_DebitPaired (id:string account:string nonce-balance-obj:object{DPMF|Nonce-Balance})
        (let
            (
                (nonce:integer (at "nonce" nonce-balance-obj))
                (balance:decimal (at "balance" nonce-balance-obj))
            )
            (DPMF|X_DebitAdmin id nonce account balance)
        )
    )
    (defun DPMF|X_Issue:string
        (
            account:string 
            name:string 
            ticker:string 
            decimals:integer 
            can-change-owner:bool 
            can-upgrade:bool 
            can-add-special-role:bool 
            can-freeze:bool 
            can-wipe:bool 
            can-pause:bool
            can-transfer-nft-create-role:bool
        )
        (UTILS.DALOS|UEV_Decimals decimals)
        (UTILS.DALOS|UEV_TokenName name)
        (UTILS.DALOS|UEV_TickerName ticker)
        (require-capability (DPTF-DPMF|ISSUE))
        (insert DPMF|PropertiesTable (DALOS|UC_Makeid ticker)
            {"branding"             : BRANDING|DEFAULT
            ,"branding-pending"     : BRANDING|DEFAULT
            ,"owner-konto"          : account
            ,"name"                 : name
            ,"ticker"               : ticker
            ,"decimals"             : decimals
            ,"can-change-owner"     : can-change-owner
            ,"can-upgrade"          : can-upgrade
            ,"can-add-special-role" : can-add-special-role
            ,"can-freeze"           : can-freeze
            ,"can-wipe"             : can-wipe
            ,"can-pause"            : can-pause
            ,"is-paused"            : false
            ,"can-transfer-nft-create-role" : can-transfer-nft-create-role
            ,"supply"               : 0.0
            ,"create-role-account"  : account
            ,"role-transfer-amount" : 0
            ,"nonces-used"          : 0
            ,"reward-bearing-token" : UTILS.BAR
            ,"vesting"              : UTILS.BAR}
        )
        (with-capability (SECURE)
            (DPTF-DPMF|C_DeployAccount (DALOS|UC_Makeid ticker) account false)    
        )
        (DALOS|UC_Makeid ticker)
    )
    (defun DPMF|X_IncrementNonce (id:string)
        (require-capability (DPMF|INCREMENT_NONCE))
        (with-read DPMF|PropertiesTable id
            { "nonces-used" := nu }
            (update DPMF|PropertiesTable id { "nonces-used" : (+ nu 1)})
        )
    )
    (defun DPMF|X_Mint:integer (id:string account:string amount:decimal meta-data:[object])
        (require-capability (DPMF|X_MINT id account amount))
        (let
            (
                (new-nonce:integer (+ (DPMF|UR_NoncesUsed id) 1))
            )
            (DPMF|X_Create id account meta-data)
            (DPMF|X_AddQuantity id new-nonce account amount)
            new-nonce
        )
    )
    (defun DPMF|X_Transfer (id:string nonce:integer sender:string receiver:string transfer-amount:decimal method:bool)
        (require-capability (DPMF|X_TRANSFER id sender receiver transfer-amount method))
        (let
            (
                (current-nonce-meta-data (DPMF|UR_AccountBatchMetaData id nonce sender))
                (ea-id:string (DALOS.DALOS|UR_EliteAurynID))
            )
            (DPMF|X_DebitStandard id nonce sender transfer-amount)
            (DPMF|X_Credit id nonce current-nonce-meta-data receiver transfer-amount)
            (DPTF-DPMF|X_UpdateElite id sender receiver)
        )
    )
    (defun DPMF|XK_Transfer (patron:string id:string nonce:integer sender:string receiver:string transfer-amount:decimal method:bool)
        (require-capability (DALOS|EXECUTOR))
        (if (not (DALOS.IGNIS|URC_ZeroGAZ id sender receiver))
            (DALOS.IGNIS|X_Collect patron sender DALOS.GAS_SMALLEST)
            true
        )
        (DPMF|X_Transfer id nonce sender receiver transfer-amount method)
        (DALOS.DALOS|X_IncrementNonce sender)
    )
    (defun DPMF|XO_MoveCreateRole (id:string receiver:string)
        (enforce-guard (C_ReadPolicy "ATSI|MCRl"))
        (with-capability (DPMF|X_MOVE_CREATE-ROLE id receiver)
            (DPMF|XP_MoveCreateRole id receiver)
        )
    )
    (defun DPMF|XP_MoveCreateRole (id:string receiver:string)
        (require-capability (DPMF|X_MOVE_CREATE-ROLE id receiver))
        (update DPMF|BalanceTable (concat [id UTILS.BAR (DPMF|UR_CreateRoleAccount id)])
            {"role-nft-create" : false}
        )
        (update DPMF|BalanceTable (concat [id UTILS.BAR receiver])
            {"role-nft-create" : true}
        )
        (update DPMF|PropertiesTable id
            {"create-role-account" : receiver}
        )
    )
    (defun DPMF|XO_ToggleAddQuantityRole (id:string account:string toggle:bool)
        (enforce-guard (C_ReadPolicy "ATSI|TgAqRl"))
        (with-capability (DPMF|X_TOGGLE_ADD-QUANTITY-ROLE id account toggle)
            (DPMF|XP_ToggleAddQuantityRole id account toggle)
        )
    )
    (defun DPMF|XP_ToggleAddQuantityRole (id:string account:string toggle:bool)
        (require-capability (DPMF|X_TOGGLE_ADD-QUANTITY-ROLE id account toggle))
        (update DPMF|BalanceTable (concat [id UTILS.BAR account])
            {"role-nft-add-quantity" : toggle}
        )
    )
)


