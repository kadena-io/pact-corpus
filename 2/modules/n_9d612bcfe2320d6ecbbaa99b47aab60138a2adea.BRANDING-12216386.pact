(module BRANDING GOVERNANCE
    (use n_9d612bcfe2320d6ecbbaa99b47aab60138a2adea.BASIS)

    (defcap GOVERNANCE ()
        (compose-capability (BRANDING-ADMIN))
    )
    (defcap BRANDING-ADMIN ()
        (enforce-guard G-MD_BRANDING)
    )
    (defconst G-MD_BRANDING   (keyset-ref-guard DALOS.DALOS|DEMIURGOI))

    (defcap COMPOSE ()
        true
    )
    (defcap SECURE ()
        true
    )
    (defcap P|DIN ()
        true
    )
    (defcap P|IC ()
        true
    )
    (defcap P|BU ()
        true
    )
    (defcap P|DINIC ()
        (compose-capability (P|DIN))
        (compose-capability (P|IC))
    )

    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (BRANDING-ADMIN)
            (write BRD|PoliciesTable policy-name
                {"policy" : policy-guard}
            )
        )
    )
    (defun BRD|C_ReadPolicy:guard (policy-name:string)
        (at "policy" (read BRD|PoliciesTable policy-name ["policy"]))
    )
    (defun DefinePolicies ()
        (DALOS.A_AddPolicy 
            "BRD|IncrementDalosNonce"
            (create-capability-guard (P|DIN))
        )
        (DALOS.A_AddPolicy 
            "BRD|GasCollection"
            (create-capability-guard (P|IC))
        )
        (BASIS.A_AddPolicy
            "BRD|Update"
            (create-capability-guard (P|BU))
        )
    )

    ;;
    (defschema BRD|PolicySchema
        policy:guard
    )
    (deftable BRD|PoliciesTable:{BRD|PolicySchema})

    ;;[CAP]
    (defcap BRD|UPGRADE_BRANDING (id:string token-type:bool)
        (BASIS.DPTF-DPMF|UEV_id id token-type)
        (BASIS.DPTF-DPMF|CAP_Owner id token-type)
        (let
            (
                (current-flag:integer (BASIS|URB_Flag id token-type false))
            )
            (enforce (>= current-flag 1) "Golden Flag cannot be upgraded")
            (let*
                (
                    (premium:time (BASIS|URB_PremiumUntil id token-type false))
                    (current:time (at "block-time" (chain-data)))
                    (remaining:decimal (diff-time premium current))
                )
                (enforce (< remaining 1296000.0) "Blue Flag has more than 15 days remainig!")
            )
        )
    )
    (defcap BRD|BRANDING (id:string token-type:bool)
        (compose-capability (BRD|X_BRANDING id token-type))
        (compose-capability (P|DINIC))
    )
    (defcap BRD|X_BRANDING (id:string token-type:bool)
        (BASIS.DPTF-DPMF|UEV_id id token-type)
        (BASIS.DPTF-DPMF|CAP_Owner id token-type)
    )
    ;;[URC]& [UC]
    (defun BRD|UC_BrandingLogo:object{DALOS.BrandingSchema} (input:object{DALOS.BrandingSchema} logo:string)
        {"logo"             : logo
        ,"description"      : (at "description" input)
        ,"website"          : (at "website" input)
        ,"social"           : (at "social" input)
        ,"flag"             : (at "flag" input)
        ,"genesis"          : (at "genesis" input)
        ,"premium-until"    : (at "premium-until" input)}
    )
    (defun BRD|UC_BrandingDescription:object{DALOS.BrandingSchema} (input:object{DALOS.BrandingSchema} description:string)
        {"logo"             : (at "logo" input)
        ,"description"      : description
        ,"website"          : (at "website" input)
        ,"social"           : (at "social" input)
        ,"flag"             : (at "flag" input)
        ,"genesis"          : (at "genesis" input)
        ,"premium-until"    : (at "premium-until" input)}
    )
    (defun BRD|UC_BrandingWebsite:object{DALOS.BrandingSchema} (input:object{DALOS.BrandingSchema} website:string)
        {"logo"             : (at "logo" input)
        ,"description"      : (at "description" input)
        ,"website"          : website
        ,"social"           : (at "social" input)
        ,"flag"             : (at "flag" input)
        ,"genesis"          : (at "genesis" input)
        ,"premium-until"    : (at "premium-until" input)}
    )
    (defun BRD|UC_BrandingSocial:object{DALOS.BrandingSchema} (input:object{DALOS.BrandingSchema} social:[object{DALOS.SocialSchema}])
        {"logo"             : (at "logo" input)
        ,"description"      : (at "description" input)
        ,"website"          : (at "website" input)
        ,"social"           : social
        ,"flag"             : (at "flag" input)
        ,"genesis"          : (at "genesis" input)
        ,"premium-until"    : (at "premium-until" input)}
    )
    (defun BRD|UC_BrandingFlag:object{DALOS.BrandingSchema} (input:object{DALOS.BrandingSchema} flag:integer)
        (enforce (contains flag (enumerate 0 4)) "Invalid Flag Integer")
        {"logo"             : (at "logo" input)
        ,"description"      : (at "description" input)
        ,"website"          : (at "website" input)
        ,"social"           : (at "social" input)
        ,"flag"             : flag
        ,"genesis"          : (at "genesis" input)
        ,"premium-until"    : (at "premium-until" input)}
    )
    (defun BRD|UC_BrandingPremium:object{DALOS.BrandingSchema} (input:object{DALOS.BrandingSchema} premium:time)
        {"logo"             : (at "logo" input)
        ,"description"      : (at "description" input)
        ,"website"          : (at "website" input)
        ,"social"           : (at "social" input)
        ,"flag"             : (at "flag" input)
        ,"genesis"          : (at "genesis" input)
        ,"premium-until"    : premium}
    )
    (defun BRD|UC_MaxBluePayment (account:string)
        (let
            (
                (mt:integer (DALOS.DALOS|UR_Elite-Tier-Major account))
            )
            (if (<= mt 2)
                1
                (if (and (> mt 2)(< mt 5))
                    2
                    3
                )
            )
        )
    )
    ;;[A]
    (defun BRD|A_SetBrandingFlag (id:string token-type:bool flag:integer)
        (enforce-one
            "Setting Branding Flag not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (BRANDING-ADMIN)
            (BASIS.DPTF-DPMF|UEV_id id token-type)
            (enforce (contains flag (enumerate 0 4)) "Invalid Integer Flag")
            (let*
                (
                    (existing-branding:object{DALOS.BrandingSchema} (BASIS.BASIS|UR_Branding id token-type false))
                    (modified-branding:object{DALOS.BrandingSchema} (BRD|UC_BrandingFlag existing-branding flag))
                )
                (with-capability (P|BU)
                    (BASIS.DPTF-DPMF|X_UpdateBranding id token-type false modified-branding)
                )
            )
        )
    )
    (defun BRD|A_SetBrandingLive (id:string token-type:bool)
        (enforce-one
            "Setting Branding Live not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (BRANDING-ADMIN)
            (let*
                (
                    (pending:object{DALOS.BrandingSchema} (BASIS.BASIS|UR_Branding id token-type true))
                    (flag:integer (BASIS.BASIS|URB_Flag id token-type false))
                    (updated-flag:integer
                        (if (<= flag 1)
                            flag
                            2
                        )
                    )
                    (updated-branding:object{DALOS.BrandingSchema} (BRD|UC_BrandingFlag pending updated-flag))
                    (np1:object{DALOS.BrandingSchema} (BRD|UC_BrandingLogo pending UTILS.BAR))
                    (np2:object{DALOS.BrandingSchema} (BRD|UC_BrandingDescription np1 UTILS.BAR))
                    (np3:object{DALOS.BrandingSchema} (BRD|UC_BrandingWebsite np2 UTILS.BAR))
                    (np4:object{DALOS.BrandingSchema} (BRD|UC_BrandingSocial np3 [DALOS.SOCIAL|EMPTY]))
                )
                ;;to make functions in basis
                (with-capability (P|BU)
                    (BASIS.DPTF-DPMF|X_UpdateBranding id token-type false updated-branding)
                    (BASIS.DPTF-DPMF|X_UpdateBranding id token-type true np4)
                )
            )
        )
    )
    ;;[C]
    (defun BRD|C_UpgradeBranding (patron:string id:string token-type:bool months:integer)
        (enforce-one
            "Upgrading Branding not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (BRD|UPGRADE_BRANDING id token-type)
            (let*
                (
                    (owner:string (BASIS.DPTF-DPMF|UR_Konto id token-type))
                    (mp:integer (BRD|UC_MaxBluePayment owner))
                    (blue:decimal (DALOS.DALOS|UR_UsagePrice "blue"))
                    (current-branding:object{DALOS.BrandingSchema} (BASIS.BASIS|UR_Branding id token-type false))
                    (premium:time (BASIS|URB_PremiumUntil id token-type false))
                )
                (enforce (<= months mp) "Invalid Months Integer")
                (let*
                    (
                        (mdec:decimal (dec months))
                        (days:decimal (* 30.0 mdec))
                        (seconds:decimal (* 86400.0 days))
                        (payment:decimal (* mdec blue))
                        (premium-until:time (add-time premium seconds))
                        (ub1:object{DALOS.BrandingSchema} (BRD|UC_BrandingFlag current-branding 1))
                        (ub2:object{DALOS.BrandingSchema} (BRD|UC_BrandingPremium ub1 premium-until))
                    )
                    (DALOS.DALOS|C_TransferRawDalosFuel patron payment)
                    (with-capability (P|BU)
                        (BASIS.DPTF-DPMF|X_UpdateBranding id token-type false ub2)
                    )
                )
            )
        )
    )
    (defun DPTF-DPMF|C_UpdateBranding (patron:string id:string token-type:bool logo:string description:string website:string social:[object{DALOS.SocialSchema}])
        (enforce-one
            "Upgdating Branding not allowed"
            [
                (enforce-guard (C_ReadPolicy "TALOS|T|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|M|Summoner"))
            ]
        )
        (with-capability (BRD|BRANDING id token-type)
            (if (not (DALOS.IGNIS|URC_IsVirtualGasZero))
                (DALOS.IGNIS|X_Collect patron (DPTF-DPMF|UR_Konto id token-type) DALOS.GAS_BRANDING)
                true
            )
            (DPTF-DPMF|X_UpdatePendingBranding id token-type logo description website social)
            (DALOS.DALOS|X_IncrementNonce patron)
        )
    )
    ;;[X]
    (defun DPTF-DPMF|X_UpdatePendingBranding (id:string token-type:bool logo:string description:string website:string social:[object{DALOS.SocialSchema}])
        (require-capability (BRD|X_BRANDING id token-type))
        (let*
            (
                (pending:object{DALOS.BrandingSchema} (BASIS.BASIS|UR_Branding id token-type true))
                (p1:object{DALOS.BrandingSchema} (BRD|UC_BrandingLogo pending logo))
                (p2:object{DALOS.BrandingSchema} (BRD|UC_BrandingDescription p1 description))
                (p3:object{DALOS.BrandingSchema} (BRD|UC_BrandingWebsite p2 website))
                (p4:object{DALOS.BrandingSchema} (BRD|UC_BrandingSocial p3 social))
            )
            (with-capability (P|BU)
                (BASIS.DPTF-DPMF|X_UpdateBranding id token-type true p4)
            )
        )
    )
)


