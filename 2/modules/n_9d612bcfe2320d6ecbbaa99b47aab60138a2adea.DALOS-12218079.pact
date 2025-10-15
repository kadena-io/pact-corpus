(module DALOS GOVERNANCE
    (use coin)

    ;;Module Governance
    (defcap GOVERNANCE ()
        (compose-capability (DALOS-ADMIN))
    )
    (defcap DALOS-ADMIN ()
        (enforce-one
            "ATS Admin not satisfed"
            [
                (enforce-guard G-MD_DALOS)
                (enforce-guard G-SC_DALOS)
            ]
        )
    )
    
    (defconst G-MD_DALOS   (keyset-ref-guard DALOS|DEMIURGOI))
    (defconst G-SC_DALOS   (keyset-ref-guard DALOS|SC_KEY))

    (defconst DALOS|DEMIURGOI
        (+ UTILS.NS_USE ".dh_master-keyset")
    )
    (defconst DALOS|SC_KEY
        (+ UTILS.NS_USE ".dh_sc_dalos-keyset")
    )
    (defconst DALOS|SC_KDA-NAME (create-principal DALOS|GUARD))
    (defconst DALOS|SC_NAME         "Σ.W∇ЦwÏξБØnζΦψÕłěîбηжÛśTã∇țâĆã4ЬĚIŽȘØíÕlÛřбΩцμCšιÄиMkλ€УщшàфGřÞыÎäY8È₳BDÏÚmßOozBτòÊŸŹjПкцğ¥щóиś4h4ÑþююqςA9ÆúÛȚβжéÑψéУoЭπÄЩψďşõшżíZtZuψ4ѺËxЖψУÌбЧλüșěđΔjÈt0ΛŽZSÿΞЩŠ")
    (defconst LIQUID|SC_NAME        "Σ.śκν9₿ŻşYЙΣJΘÊO9jпF₿wŻ¥уPэõΣÑïoγΠθßÙzěŃ∇éÖиțșφΦτşэßιBιśiĘîéòюÚY$êFЬŤØ$868дyβT0ςъëÁwRγПŠτËMĚRПMaäЗэiЪiςΨoÞDŮěŠβLé4čØHπĂŃŻЫΣÀmăĐЗżłÄăiĞ₿йÎEσțłGΛЖΔŞx¥ÁiÙNğÅÌlγ¢ĎwдŃ")
    (defconst OUROBOROS|SC_NAME     "Σ.4M0èÞbøXśαiΠ€NùÇèφqλËãØÓNCнÌπпЬ4γTмыżěуàđЫъмéLUœa₿ĞЬŒѺrËQęíùÅЬ¥τ9£φď6pÙ8ìжôYиșîŻøbğůÞχEшäΞúзêŻöŃЮüŞöućЗßřьяÉżăŹCŸNÅìŸпĐżwüăŞãiÜą1ÃγänğhWg9ĚωG₳R0EùçGΨфχЗLπшhsMτξ")

    (defcap DALOS|NATIVE-AUTOMATIC ()
        @doc "Capability needed for auto management of the <kadena-konto> associated with the Ouroboros Smart DALOS Account"
        true
    )
    (defconst DALOS|GUARD (create-capability-guard (DALOS|NATIVE-AUTOMATIC)))
    ;;
    ;;
    ;;

    ;;Simple True Capabilities (3+1)
    (defcap KADENA ()
        true
    )
    (defcap DALOS|INCREMENT_NONCE ()
        true
    )
    (defcap IGNIS|COLLECTER ()
        true
    )
    (defcap IGNIS|INCREMENT ()
        true
    )
    (defcap DALOS|INCREMENT_NONCE||IGNIS|COLLECTER ()
        @doc "Composed Capability for ease of use"
        (compose-capability (DALOS|INCREMENT_NONCE))
        (compose-capability (IGNIS|COLLECTER))
    )

    (defcap DALOS|UP_BALANCE ()
        true
    )
    (defcap SECURE ()
        true
    )

    (defun A_AddPolicy (policy-name:string policy-guard:guard)
        (with-capability (DALOS-ADMIN)
            (write PoliciesTable policy-name
                {"policy" : policy-guard}
            )
        )
    )
    (defun C_ReadPolicy:guard (policy-name:string)
        (at "policy" (read PoliciesTable policy-name ["policy"]))
    )

    (defconst DALOS|CHR_AUX
        [ " " "!" "#" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/" ":" ";" "<" "=" ">" "?" "@" "[" "]" "^" "_" "`" "{" "|" "}" "~" "‰" ]
    )
    (defconst DALOS|CHR_DIGITS 
        ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]
    )
    (defconst DALOS|CHR_CURRENCIES 
        [ "Ѻ" "₿" "$" "¢" "€" "£" "¥" "₱" "₳" "∇" ]
    )
    (defconst DALOS|CHR_LATIN-B
        [ "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" ]
    )
    (defconst DALOS|CHR_LATIN-S
        [ "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" ]
    )
    (defconst DALOS|CHR_LATIN-EXT-B
        [ "Æ" "Œ" "Á" "Ă" "Â" "Ä" "À" "Ą" "Å" "Ã" "Ć" "Č" "Ç" "Ď" "Đ" "É" "Ě" "Ê" "Ë" "È" "Ę" "Ğ" "Í" "Î" "Ï" "Ì" "Ł" "Ń" "Ñ" "Ó" "Ô" "Ö" "Ò" "Ø" "Õ" "Ř" "Ś" "Š" "Ş" "Ș" "Þ" "Ť" "Ț" "Ú" "Û" "Ü" "Ù" "Ů" "Ý" "Ÿ" "Ź" "Ž" "Ż" ]
    )
    (defconst DALOS|CHR_LATIN-EXT-S
        [ "æ" "œ" "á" "ă" "â" "ä" "à" "ą" "å" "ã" "ć" "č" "ç" "ď" "đ" "é" "ě" "ê" "ë" "è" "ę" "ğ" "í" "î" "ï" "ì" "ł" "ń" "ñ" "ó" "ô" "ö" "ò" "ø" "õ" "ř" "ś" "š" "ş" "ș" "þ" "ť" "ț" "ú" "û" "ü" "ù" "ů" "ý" "ÿ" "ź" "ž" "ż" "ß" ]
    )
    (defconst DALOS|CHR_GREEK-B
        [ "Γ" "Δ" "Θ" "Λ" "Ξ" "Π" "Σ" "Φ" "Ψ" "Ω" ]
    )
    (defconst DALOS|CHR_GREEK-S
        [ "α" "β" "γ" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "π" "ρ" "σ" "ς" "τ" "φ" "χ" "ψ" "ω" ]
    )
    (defconst DALOS|CHR_CYRILLIC-B
        [ "Б" "Д" "Ж" "З" "И" "Й" "Л" "П" "У" "Ц" "Ч" "Ш" "Щ" "Ъ" "Ы" "Ь" "Э" "Ю" "Я" ]
    )
    (defconst DALOS|CHR_CYRILLIC-S
        [ "б" "в" "д" "ж" "з" "и" "й" "к" "л" "м" "н" "п" "т" "у" "ф" "ц" "ч" "ш" "щ" "ъ" "ы" "ь" "э" "ю" "я" ]
    )
    (defconst DALOS|CHARSET
        (+
            DALOS|CHR_DIGITS 
            (+
                DALOS|CHR_CURRENCIES
                (+
                    DALOS|CHR_LATIN-B
                    (+
                        DALOS|CHR_LATIN-S
                        (+
                            DALOS|CHR_LATIN-EXT-B
                            (+
                                DALOS|CHR_LATIN-EXT-S
                                (+
                                    DALOS|CHR_GREEK-B
                                    (+
                                        DALOS|CHR_GREEK-S
                                        (+
                                            DALOS|CHR_CYRILLIC-B
                                            DALOS|CHR_CYRILLIC-S
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
    (defconst DALOS|EXTENDED (+ DALOS|CHR_AUX DALOS|CHARSET))
    (defconst DALOS|INFO "DalosInformation")
    (defconst DALOS|VGD "VirtualGasData")
    ;;
    (defconst GAS_EXCEPTION [OUROBOROS|SC_NAME DALOS|SC_NAME LIQUID|SC_NAME])
    (defconst GAS_QUARTER 0.25)
    (defconst GAS_SMALLEST 1.00)
    (defconst GAS_SMALL 2.00)
    (defconst GAS_MEDIUM 3.00)
    (defconst GAS_BIG 4.00)
    (defconst GAS_BIGGEST 5.00)
    (defconst GAS_ISSUE 15.00)
    (defconst GAS_BRANDING 100.00)
    (defconst GAS_HUGE 500.00)

    (defschema DALOS|KadenaSchema
        dalos:[string]
    )
    (defschema PolicySchema
        policy:guard
    )
    (defschema DALOS|PropertiesSchema
        demiurgoi:[string]                  ;;Stores Demiurgoi DALOS Accounts
        unity-id:string                     ;;DALOS
        gas-source-id:string                ;;OUROBOROS
        gas-source-id-price:decimal         ;;OUROBOROS Price
        gas-id:string                       ;;IGNIS
        ats-gas-source-id:string            ;;AURYN
        elite-ats-gas-source-id:string      ;;ELITE-AURYN
        wrapped-kda-id:string               ;;DWK - Dalos Wrapped Kadena
        liquid-kda-id:string                ;;DLK - Dalos Liquid Kadena
    )
    (defschema DALOS|GasManagementSchema
        virtual-gas-tank:string             ;;IGNIS|SC_NAME = "GasTanker"
        virtual-gas-toggle:bool             ;;IGNIS collection toggle
        virtual-gas-spent:decimal           ;;IGNIS spent
        native-gas-toggle:bool              ;;KADENA collection toggle
        native-gas-spent:decimal            ;;KADENA spent
    )
    ;;DALOS Virtual Blockchain Prices
    (defschema DALOS|PricesSchema
        kda-price:decimal                   ;;Stores price for action
    )
    ;;DALOS Account Information
    (defschema DALOS|AccountSchema
        @doc "Schema that stores DALOS Account Information"
        ;;==================================;;
        public:string                       ;;Stores the Public Key associated with the Dalos Account string
        guard:guard                         ;;Guard of the DALOS Account
        kadena-konto:string                 ;;stores the underlying Kadena principal Account that was used to create the DALOS Account
                                            ;;this account is used for KDA payments. The guard for this account is not stored in this module
                                            ;;Rather the guard of the kadena account saved here, is stored in the table inside the coin module
        sovereign:string                    ;;Stores the Sovereign Account (Can only be a Standard DALOS Account)
                                            ;;The Sovereign Account, is the Account that has ownership of this Account;
                                            ;;For Normal DALOS Account, the Sovereign is itself (The Key of the Table Data)
                                            ;;For Smart DALOS Account, the Sovereign is another Normal DALOS Account
        governor:guard                      ;;Stores a guard that allows remote managing of the Smart Account via its own module
                                            ;;A Smart Account can only be managed by a single module this way
        ;;==================================;;
        smart-contract:bool                 ;;When true DALOS Account is a Smart DALOS Account, if <false> it is a Standard DALOS Account
        payable-as-smart-contract:bool      ;;when true, a Smart DALOS Account is payable by other Normal DALOS Accounts
        payable-by-smart-contract:bool      ;;when true, a Smart DALOS Account is payable by other Smart DALOS Accounts
        payable-by-method:bool              ;;when true, a Smart DALOS Account is payable only through special Functions
        ;;==================================;;
        nonce:integer                       ;;stores how many transactions the DALOS Account executed
        elite:object{DALOS|EliteSchema}
                                            ;;Primal DPTF Data
                                            ;;Ouroboros and Gas Info are stored at the DALOS Account Level
        ouroboros:object{DPTF|BalanceSchema}
        ignis:object{DPTF|BalanceSchema}
        ;;==================================;;
    )
    (defschema DPTF|BalanceSchema
        @doc "Schema that Stores Account Balances for DPTF Tokens (True Fungibles)\
            \ Key for the Table is a string composed of: <DPTF id> + UTILS.BAR + <account> \
            \ This ensure a single entry per DPTF id per account. \
            \ As an Exception OUROBOROS and IGNIS Account Data is Stored at the DALOS Account Level"
        balance:decimal
        ;;Special Roles
        role-burn:bool
        role-mint:bool
        role-transfer:bool
        role-fee-exemption:bool                         
        ;;States
        frozen:bool
    )
    (defconst DPTF|BLANK
        { "balance"                 : 0.0
        , "role-burn"               : false
        , "role-mint"               : false
        , "role-transfer"           : false
        , "role-fee-exemption"      : false
        , "frozen"                  : false }
    )
    (defschema DALOS|EliteSchema
        class:string
        name:string
        tier:string
        deb:decimal
    )
    (defconst DALOS|PLEB
        { "class" : "NOVICE"
        , "name"  : "Infidel"
        , "tier"  : "0.0"
        , "deb"   : 1.0 }
    )
    (defconst DALOS|VOID
        { "class" : "VOID"
        , "name"  : "Undead"
        , "tier"  : "0.0"
        , "deb"   : 0.0 }
    )
    ;;Branding
    (defschema BrandingSchema
        logo:string
        description:string
        website:string
        social:[object{SocialSchema}]
        flag:integer
        genesis:time
        premium-until:time
    )
    (defschema SocialSchema
        social-media-name:string
        social-media-link:string
    )
    (defconst SOCIAL|EMPTY
        {"social-media-name"    : UTILS.BAR
        ,"social-media-link"    : UTILS.BAR}
    )
    (defconst BRANDING|DEFAULT
        {"logo"                 : UTILS.BAR
        ,"description"          : UTILS.BAR
        ,"website"              : UTILS.BAR
        ,"social"               : [SOCIAL|EMPTY]
        ,"flag"                 : 3
        ,"genesis"              : (at "block-time" (chain-data))
        ,"premium-until"        : (at "block-time" (chain-data))}
    )

    (deftable DALOS|PropertiesTable:{DALOS|PropertiesSchema})
    (deftable DALOS|GasManagementTable:{DALOS|GasManagementSchema})
    (deftable DALOS|PricesTable:{DALOS|PricesSchema})
    (deftable DALOS|AccountTable:{DALOS|AccountSchema})
    (deftable PoliciesTable:{PolicySchema})
    (deftable DALOS|KadenaLedger:{DALOS|KadenaSchema})
    
    ;;GLYPH Submodule
    (defun GLYPH|UEV_DalosAccountCheck (account:string)
        @doc "Checks if a string is a valid DALOS Account, using no enforcements "
        (let*
            (
                (account-len:integer (length account))
                (t1:bool (= account-len 162))
                (ouroboros:string "Ѻ")
                (sigma:string "Σ")
                (first:string (take 1 account))
                (t2:bool (or (= first ouroboros)(= first sigma)))
                (t3:bool (and t1 t2))
                (second:string (drop 1 (take 2 account)))
                (point:string ".")
                (t4:bool (= second point))
                (t5:bool (GLYPH|UEV_EnforceMultiStringDalosCharset (drop 2 account)))
                (t6:bool (and t4 t5))
            )
            (and t3 t6)
        )
    )
    (defun GLYPH|UEV_DalosAccount (account:string)
        @doc "Enforces that a Dalos Account (Address) has the proper format"
        (let*
            (
                (account-len:integer (length account))
                (ouroboros:string "Ѻ")
                (sigma:string "Σ")
                (first:string (take 1 account))
                (second:string (drop 1 (take 2 account)))
                (point:string ".")
            )
            (enforce (= account-len 162) "Address|Account does not conform to the DALOS Standard for Addresses|Accounts")
            (enforce-one 
                "Address|Account format is invalid"
                [
                    (enforce (= first ouroboros) "Account|Address Identifier is invalid, while checking for a Standard Account|Address")
                    (enforce (= first sigma) "Account|Address Identifier is invalid, while checking for a Smart Account|Address")
                ]
            )
            (enforce (= second point) "Account|Address Format is invalid, second Character must be a <.>")
            (let
                (
                    (checkup:bool (GLYPH|UEV_EnforceMultiStringDalosCharset (drop 2 account)))
                )
                (enforce checkup "Characters do not conform to the DALOS|CHARSET")
            )
        )
    )
    (defun GLYPH|UEV_EnforceMultiStringDalosCharset:bool (multi-s:string)
        @doc "Enforce a multistring is part of the DALOS|CHARSET"
        (let
            (
                (str-lst:[string] (str-to-list multi-s))
            )
            (fold
                (lambda
                    (acc:bool idx:integer)
                    (let
                        (
                            (checkup:bool (contains (at idx str-lst) DALOS|CHARSET))
                        )
                        (or acc checkup)
                    )
                )
                false
                (enumerate 0 (- (length str-lst) 1))
            )
        )
    )
    ;;
    ;;
    ;;            DALOS             Submodule
    ;;
    ;;[CAP]
    (defun DALOS|CAP_EnforceAccountOwnership (account:string)
        (let*
            (
                (type:bool (DALOS|UR_AccountType account))
            )
            (if type
                (DALOS|CAP_X_EnforceSmartAccount account)
                (DALOS|CAP_X_EnforceStandardAccount account)
            )
        )
    )
    (defun DALOS|CAP_X_EnforceStandardAccount (account:string)
        (let
            (
                (account-guard:guard (DALOS|UR_AccountGuard account))
                (sovereign:string (DALOS|UR_AccountSovereign account))
                (governor:guard (DALOS|UR_AccountGovernor account))
            )
            (enforce (= sovereign account) "Incompatible Sovereign detected for a Standard DALOS Account")
            (enforce (= account-guard governor) "Incompatible Governer Guard detected for Standard DALOS Account")
            (enforce-guard account-guard)
        )
    )
    (defun DALOS|CAP_X_EnforceSmartAccount (account:string)
        (let*
            (
                (account-guard:guard (DALOS|UR_AccountGuard account))
                (sovereign:string (DALOS|UR_AccountSovereign account))
                (sovereign-guard:guard (DALOS|UR_AccountGuard sovereign))
                (governor:guard (DALOS|UR_AccountGovernor account))
            )
            (enforce (!= sovereign account) "Incompatible Sovereign detected for Smart DALOS Account")
            (enforce-one
                "Smart DALOS Account Permissions not satisfied !"
                [
                    (enforce-guard account-guard)
                    (enforce-guard sovereign-guard)
                    (enforce-guard governor)
                ]
            )
        )
    )
    ;;[CF]
    (defcap DALOS|CF|OWNER (account:string)
        (DALOS|CAP_EnforceAccountOwnership account)
    )
    ;;[UEV]
    (defun DALOS|UEV_Methodic (account:string method:bool)
        (if method
            (DALOS|CAP_EnforceAccountOwnership account)
            true
        )
    )
    (defun DALOS|UEV_EnforceAccountExists (dalos-account:string)
        (with-default-read DALOS|AccountTable dalos-account
            { "elite" : DALOS|VOID }
            { "elite" := e }
            (let
                (
                    (deb:decimal (at "deb" e))
                )
                (enforce 
                    (>= deb 1.0)
                    (format "The {} DALOS Account doesnt exist" [dalos-account])
                )
            )
        )
    )
    (defun DALOS|UEV_EnforceAccountType (account:string smart:bool)
        (let
            (
                (x:bool (DALOS|UR_AccountType account))
                (first:string (take 1 account))
                (ouroboros:string "Ѻ")
                (sigma:string "Σ")
            )
            (if smart 
                (enforce (and (= first sigma) (= x true)) (format "Operation requires a Smart DALOS Account; Account {} isnt" [account]))
                (enforce (and (= first ouroboros) (= x false)) (format "Operation requires a Standard DALOS Account; Account {} isnt" [account]))
            )
        )
    )
    (defun DALOS|UEV_EnforceTransferability (sender:string receiver:string method:bool)
        (let
            (
                (x:bool (DALOS|URC_Transferability sender receiver method))
            )
            (enforce (= x true) (format "Transferability between {} and {} with {} Method is not ensured" [sender receiver method]))
        )
    )
    (defun DALOS|UEV_SenderWithReceiver (sender:string receiver:string)
        (DALOS|UEV_EnforceAccountExists sender)
        (DALOS|UEV_EnforceAccountExists receiver)
        (enforce (!= sender receiver) "Sender and Receiver must be different")
    )
    (defun DALOS|UEV_TwentyFourPrecision (amount:decimal)
        (enforce
            (= (floor amount 24) amount)
            (format "The GAS Amount of {} is not a valid GAS Amount decimal wise" [amount])
        )
    )
    (defun IGNIS|UEV_VirtualState (state:bool)
        (let
            (
                (t:bool (DALOS|UR_VirtualToggle))
            )
            (enforce (= t state) "Invalid virtual gas collection state!")
            (if (not state)
                (IGNIS|UEV_VirtualOnCondition)
                true
            )
        )
    )
    (defun IGNIS|UEV_VirtualOnCondition ()
        (let
            (
                (ouro-id:string (DALOS|UR_OuroborosID))
                (gas-id:string (DALOS|UR_IgnisID))
            )
            (enforce (!= ouro-id UTILS.BAR) "OURO Id must be set for IGNIS Collection to turn ON!")
            (enforce (!= gas-id UTILS.BAR) "IGNIS Id must be set for IGNIS Collection to turn ON!")
            (enforce (!= gas-id ouro-id) "OURO and IGNIS id must be different for the IGNIS Collection to turn ON!")
        )
    )
    (defun IGNIS|UEV_NativeState (state:bool)
        (let
            (
                (t:bool (DALOS|UR_NativeToggle))
            )
            (enforce (= t state) "Invalid native gas collection state!")
        )
    )
    (defun IGNIS|UEV_Patron (patron:string)
        @doc "Capability that ensures a DALOS account can act as gas payer, enforcing all necesarry restrictions"
        (if (DALOS|UR_AccountType patron)
            (let
                (
                    (account-type:bool (DALOS|UR_AccountType patron))
                )
                (enforce (= patron DALOS|SC_NAME) "Only the DALOS Account can be a Smart Patron")
                (enforce (C_ReadPolicy "TALOS|AutomaticPatron"))
            )
            (DALOS|CAP_EnforceAccountOwnership patron)
        )
    )
    ;;[CAP]
    (defcap DALOS|DEPLOY_STANDARD (account:string guard:guard kadena:string)
        @event
        (let
            (
                (account-validation:bool (GLYPH|UEV_DalosAccount account))
                (first:string (take 1 account))
                (ouroboros:string "Ѻ")
            )
            (enforce account-validation (format "Account {} isn't a valid DALOS Account" [account]))
            (enforce (= first ouroboros) (format "Account {} doesn|t have the corrrect Format for a Standard DALOS Account" [account]))
            (enforce-guard guard)
            (compose-capability (KADENA))
            ;(UTILS.UTILS|UEV_EnforceReserved kadena guard)
        )
    )
    (defcap DALOS|DEPLOY_SMART (account:string guard:guard kadena:string sovereign:string)
        @event
        (let
            (
                (account-validation:bool (GLYPH|UEV_DalosAccount account))
                (first:string (take 1 account))
                (sigma:string "Σ")
            )
            (enforce account-validation (format "Account {} isn't a valid DALOS Account" [account]))
            (enforce (= first sigma) (format "Account {} doesn|t have the corrrect Format for a Smart DALOS Account" [account]))
            (DALOS|UEV_EnforceAccountType sovereign false)
            (enforce-guard guard)
            (compose-capability (KADENA))
            ;(UTILS.UTILS|UEV_EnforceReserved kadena guard)
        )
    )
    (defcap DALOS|ROTATE_ACCOUNT (account:string)
        @event
        (compose-capability (KADENA))
        (compose-capability (DALOS|CF|OWNER account))
        (compose-capability (DALOS|INCREMENT_NONCE||IGNIS|COLLECTER))
    )
    (defcap DALOS|ROTATE_SOVEREIGN (account:string new-sovereign:string)
        @event
        (compose-capability (DALOS|SOVEREIGN account new-sovereign))
        (compose-capability (DALOS|INCREMENT_NONCE||IGNIS|COLLECTER))
    )
    (defcap DALOS|SOVEREIGN (account:string new-sovereign:string)
        (DALOS|CAP_EnforceAccountOwnership account)
        (DALOS|UEV_EnforceAccountType account true)
        (DALOS|UEV_EnforceAccountType new-sovereign false)
        (DALOS|UEV_SenderWithReceiver (DALOS|UR_AccountSovereign account) new-sovereign)
    )
    (defcap DALOS|ROTATE_GOVERNOR (account:string)
        @event
        (compose-capability (DALOS|GOVERNOR account))
        (compose-capability (DALOS|INCREMENT_NONCE||IGNIS|COLLECTER))
    )
    (defcap DALOS|GOVERNOR (account:string)
        (DALOS|CAP_EnforceAccountOwnership account)
        (DALOS|UEV_EnforceAccountType account true)
    )
    (defcap DALOS|CONTROL_SMART-ACCOUNT (account:string pasc:bool pbsc:bool pbm:bool)
        @event
        (compose-capability (DALOS|X_CONTROL_SMART-ACCOUNT account pasc pbsc pbm))
        (compose-capability (DALOS|INCREMENT_NONCE||IGNIS|COLLECTER))
    )
    (defcap DALOS|X_CONTROL_SMART-ACCOUNT (account:string pasc:bool pbsc:bool pbm:bool)
        (compose-capability (DALOS|GOVERNOR account))
        (enforce (= (or (or pasc pbsc) pbm) true) "At least one Smart DALOS Account parameter must be true")
    )
    ;;
    (defcap IGNIS|COLLECT (patron:string active-account:string amount:decimal)
        (IGNIS|UEV_Patron patron)
        (let
            (
                (sender-type:bool (DALOS|UR_AccountType active-account))
            )
            (if sender-type
                (compose-capability (IGNIS|SMART_COLLECT patron active-account amount))
                (compose-capability (IGNIS|NORML_COLLECT patron amount))
            )
        )
    )
    (defcap IGNIS|NORML_COLLECT (patron:string amount:decimal)
        (let
            (
                (gas-id:string (DALOS|UR_IgnisID))
            )
            (if (!= gas-id UTILS.BAR)
                (compose-capability (IGNIS|X_NORML_COLLECT patron amount))
                true
            )
        )
    )
    (defcap IGNIS|X_NORML_COLLECT (patron:string amount:decimal)
        (compose-capability (IGNIS|TRANSFER patron (DALOS|UR_Tanker) amount))
        (compose-capability (IGNIS|INCREMENT))
    )
    (defcap IGNIS|SMART_COLLECT (patron:string active-account:string amount:decimal)
        (let
            (
                (gas-id:string (DALOS|UR_IgnisID))
            )
            (if (!= gas-id UTILS.BAR)
                (let*
                    (
                        (gas-pot:string (DALOS|UR_Tanker))
                        (quarter:decimal (* amount GAS_QUARTER))
                        (rest:decimal (- amount quarter))
                    )
                    (compose-capability (IGNIS|TRANSFER patron gas-pot rest))
                    (compose-capability (IGNIS|TRANSFER patron active-account quarter))
                    (compose-capability (IGNIS|INCREMENT))
                )
                true
            )
        )
    )
    (defcap IGNIS|TRANSFER (sender:string receiver:string ta:decimal)
        (enforce (!= sender receiver) "Sender and Receiver must be different")
        (DALOS|UEV_TwentyFourPrecision ta)
        (enforce (> ta 0.0) "Cannot debit|credit 0.0 or negative GAS amounts")
        (compose-capability (IGNIS|DEBIT sender))
        (compose-capability (IGNIS|CREDIT receiver))
    )
    (defcap IGNIS|DEBIT (sender:string)
        (DALOS|UEV_EnforceAccountExists sender)
        (DALOS|UEV_EnforceAccountType sender false)
        (compose-capability (DALOS|UP_BALANCE))
    )
    (defcap IGNIS|CREDIT (receiver:string)
        (DALOS|UEV_EnforceAccountExists receiver)
        (compose-capability (DALOS|UP_BALANCE))
    )
    (defcap IGNIS|TOGGLE (native:bool toggle:bool)
        (compose-capability (DALOS-ADMIN))
        (if native
            (IGNIS|UEV_NativeState (not toggle))
            (IGNIS|UEV_VirtualState (not toggle))
        )
    )
    ;;[UR]
    (defun DALOS|UR_DemiurgoiID:[string] ()
        (at "demiurgoi" (read DALOS|PropertiesTable DALOS|INFO ["demiurgoi"]))
    )
    (defun DALOS|UR_UnityID:string ()
        (at "unity-id" (read DALOS|PropertiesTable DALOS|INFO ["unity-id"]))
    )
    (defun DALOS|UR_OuroborosID:string ()
        (at "gas-source-id" (read DALOS|PropertiesTable DALOS|INFO ["gas-source-id"]))
    )
    (defun DALOS|UR_OuroborosPrice:decimal ()
        (at "gas-source-id-price" (read DALOS|PropertiesTable DALOS|INFO ["gas-source-id-price"]))
    )
    (defun DALOS|UR_IgnisID:string ()
        (with-default-read DALOS|PropertiesTable DALOS|INFO 
            { "gas-id" :  UTILS.BAR }
            { "gas-id" := gas-id}
            gas-id
        )
    )
    (defun DALOS|UR_AurynID:string ()
        (at "ats-gas-source-id" (read DALOS|PropertiesTable DALOS|INFO ["ats-gas-source-id"]))
    )
    (defun DALOS|UR_EliteAurynID:string ()
        (at "elite-ats-gas-source-id" (read DALOS|PropertiesTable DALOS|INFO ["elite-ats-gas-source-id"]))
    )
    (defun DALOS|UR_WrappedKadenaID:string ()
        (at "wrapped-kda-id" (read DALOS|PropertiesTable DALOS|INFO ["wrapped-kda-id"]))
    )
    (defun DALOS|UR_LiquidKadenaID:string ()
        (at "liquid-kda-id" (read DALOS|PropertiesTable DALOS|INFO ["liquid-kda-id"]))
    )
    ;;
    (defun DALOS|UR_Tanker:string ()
        (at "virtual-gas-tank" (read DALOS|GasManagementTable DALOS|VGD ["virtual-gas-tank"]))
    )
    (defun DALOS|UR_VirtualToggle:bool ()
        (with-default-read DALOS|GasManagementTable DALOS|VGD
            {"virtual-gas-toggle" : false}
            {"virtual-gas-toggle" := tg}
            tg
        )
    )
    (defun DALOS|UR_VirtualSpent:decimal ()
        (at "virtual-gas-spent" (read DALOS|GasManagementTable DALOS|VGD ["virtual-gas-spent"]))
    )
    (defun DALOS|UR_NativeToggle:bool ()
        (with-default-read DALOS|GasManagementTable DALOS|VGD
            {"native-gas-toggle" : false}
            {"native-gas-toggle" := tg}
            tg
        )
    )
    (defun DALOS|UR_NativeSpent:decimal ()
        (at "native-gas-spent" (read DALOS|GasManagementTable DALOS|VGD ["native-gas-spent"]))
    )
    ;;
    (defun DALOS|UR_UsagePrice:decimal (action:string)
        (at "kda-price" (read DALOS|PricesTable action ["kda-price"]))
    )
    ;;
    (defun DALOS|UR_AccountPublicKey:string (account:string)
        (at "public" (read DALOS|AccountTable account ["public"]))
    )
    (defun DALOS|UR_AccountGuard:guard (account:string)
        (at "guard" (read DALOS|AccountTable account ["guard"]))
    )
    (defun DALOS|UR_AccountKadena:string (account:string)
        (at "kadena-konto" (read DALOS|AccountTable account ["kadena-konto"]))
    )
    (defun DALOS|UR_AccountSovereign:string (account:string)
        (at "sovereign" (read DALOS|AccountTable account ["sovereign"]))
    )
    (defun DALOS|UR_AccountGovernor:guard (account:string)
        (at "governor" (read DALOS|AccountTable account ["governor"]))
    )
    ;;
    (defun DALOS|UR_AccountProperties:[bool] (account:string)
        (with-default-read DALOS|AccountTable account
            { "smart-contract" : false, "payable-as-smart-contract" : false, "payable-by-smart-contract" : false, "payable-by-method" : false}
            { "smart-contract" := sc, "payable-as-smart-contract" := pasc, "payable-by-smart-contract" := pbsc, "payable-by-method" := pbm }
            [sc pasc pbsc pbm]
        )
    )
    (defun DALOS|UR_AccountType:bool (account:string)
        (at 0 (DALOS|UR_AccountProperties account))
    )
    (defun DALOS|UR_AccountPayableAs:bool (account:string)
        (at 1 (DALOS|UR_AccountProperties account))
    )
    (defun DALOS|UR_AccountPayableBy:bool (account:string)
        (at 2 (DALOS|UR_AccountProperties account))
    )
    (defun DALOS|UR_AccountPayableByMethod:bool (account:string)
        (at 3 (DALOS|UR_AccountProperties account))
    )
    ;;
    (defun DALOS|UR_AccountNonce:integer (account:string)
        (with-default-read DALOS|AccountTable account
            { "nonce" : 0 }
            { "nonce" := n }
            n
        )
    )
    (defun DALOS|UR_Elite (account:string)
        (with-default-read DALOS|AccountTable account
            { "elite" : DALOS|PLEB }
            { "elite" := e}
            e
        )
    )
    (defun DALOS|UR_Elite-Class (account:string)
        (at "class" (DALOS|UR_Elite account))
    )
    (defun DALOS|UR_Elite-Name (account:string)
        (at "name" (DALOS|UR_Elite account))
    )
    (defun DALOS|UR_Elite-Tier (account:string)
        (at "tier" (DALOS|UR_Elite account))
    )
    (defun DALOS|UR_Elite-Tier-Major:integer (account:string)
        (str-to-int (take 1 (DALOS|UR_Elite-Tier account)))
    )
    (defun DALOS|UR_Elite-Tier-Minor:integer (account:string)
        (str-to-int (take -1 (DALOS|UR_Elite-Tier account)))
    )
    (defun DALOS|UR_Elite-DEB (account:string)
        (at "deb" (DALOS|UR_Elite account))
    )
    ;;
    (defun DALOS|UR_TrueFungible:object{DPTF|BalanceSchema} (account:string snake-or-gas:bool)
        (if snake-or-gas
            (with-default-read DALOS|AccountTable account
                { "ouroboros" : DPTF|BLANK }
                { "ouroboros" := o}
                o
            )
            (with-default-read DALOS|AccountTable account
                { "ignis" : DPTF|BLANK }
                { "ignis" := i}
                i
            )
        )
    )
    (defun DALOS|UR_TrueFungible_AccountSupply:decimal (account:string snake-or-gas:bool)
        (at "balance" (DALOS|UR_TrueFungible account snake-or-gas))
    )
    (defun DALOS|UR_TrueFungible_AccountRoleBurn:bool (account:string snake-or-gas:bool)
        (at "role-burn" (DALOS|UR_TrueFungible account snake-or-gas))
    )
    (defun DALOS|UR_TrueFungible_AccountRoleMint:bool (account:string snake-or-gas:bool)
        (at "role-mint" (DALOS|UR_TrueFungible account snake-or-gas))
    )
    (defun DALOS|UR_TrueFungible_AccountRoleTransfer:bool (account:string snake-or-gas:bool)
        (at "role-transfer" (DALOS|UR_TrueFungible account snake-or-gas))
    )
    (defun DALOS|UR_TrueFungible_AccountRoleFeeExemption:bool (account:string snake-or-gas:bool)
        (at "role-fee-exemption" (DALOS|UR_TrueFungible account snake-or-gas))
    )
    (defun DALOS|UR_TrueFungible_AccountFreezeState:bool (account:string snake-or-gas:bool)
        (at "frozen" (DALOS|UR_TrueFungible account snake-or-gas))
    )
    ;;
    (defun DALOS|UR_KadenaLedger:[string] (kadena:string)
        (with-default-read DALOS|KadenaLedger kadena
            { "dalos"    : [UTILS.BAR] }
            { "dalos"    := d }
            d
        )
    )
    ;;[UC] & [URC]
    (defun DALOS|UC_Makeid:string (ticker:string)
        (UTILS.DALOS|UEV_TickerName ticker)
        (UTILS.DALOS|UCC_Makeid ticker)
    )
    (defun DALOS|URC_Transferability:bool (sender:string receiver:string method:bool)
        (DALOS|UEV_SenderWithReceiver sender receiver)
        (let
            (
                (s-sc:bool (DALOS|UR_AccountType sender))
                (r-sc:bool (DALOS|UR_AccountType receiver))
                (r-pasc:bool (DALOS|UR_AccountPayableAs receiver))
                (r-pbsc:bool (DALOS|UR_AccountPayableBy receiver))
                (r-mt:bool (DALOS|UR_AccountPayableByMethod receiver))
            )
            (if (= s-sc false)
                (if (= r-sc false)              ;;sender is normal
                    true                        ;;receiver is normal (Normal => Normal | Case 1)
                    (if (= method true)         ;;receiver is smart  (Normal => Smart | Case 3)
                        r-mt
                        r-pasc
                    )
                )
                (if (= r-sc false)              ;;sender is smart
                    true                        ;;receiver is normal (Smart => Normal | Case 4)
                    (if (= method true)         ;;receiver is false (Smart => Smart | Case 2)
                        r-mt
                        r-pbsc
                    )
                )
            )
        )
    )
    (defun IGNIS|URC_Exception (account:string)
        (contains account GAS_EXCEPTION)
    )
    (defun IGNIS|URC_ZeroGAZ:bool (id:string sender:string receiver:string)
        (let*
            (
                (t1:bool (IGNIS|URC_ZeroGAS id sender))
                (t2:bool (IGNIS|URC_Exception receiver))
            )
            (or t1 t2)
        )
    )
    (defun IGNIS|URC_ZeroGAS:bool (id:string sender:string)
        (let*
            (
                (t1:bool (IGNIS|URC_IsVirtualGasZeroAbsolutely id))
                (t2:bool (IGNIS|URC_Exception sender))
            )
            (or t1 t2)
        )
    )
    (defun IGNIS|URC_IsVirtualGasZeroAbsolutely:bool (id:string)
        (let*
            (
                (t1:bool (IGNIS|URC_IsVirtualGasZero))
                (gas-id:string (DALOS|UR_IgnisID))
                (t2:bool (if (or (= gas-id UTILS.BAR)(= id gas-id)) true false))
            )
            (or t1 t2)
        )
    )
    (defun IGNIS|URC_IsVirtualGasZero:bool ()
        (if (DALOS|UR_VirtualToggle)
            false
            true
        )
    )
    (defun IGNIS|URC_IsNativeGasZero:bool ()
        (if (DALOS|UR_NativeToggle)
            false
            true
        )
    )
    ;;[A]
    (defun IGNIS|A_Toggle (native:bool toggle:bool)
        (with-capability (IGNIS|TOGGLE native toggle)
            (IGNIS|X_Toggle native toggle)
        )
    )
    (defun IGNIS|A_SetSourcePrice (price:decimal)
        (with-capability (DALOS-ADMIN)
            (IGNIS|X_UpdateSourcePrice price)
        )
    )
    (defun DALOS|A_UpdateUsagePrice (action:string new-price:decimal)
        (with-capability (DALOS-ADMIN)
            (write DALOS|PricesTable action
                {"kda-price"     : (floor new-price UTILS.KDA_PRECISION)}
            )
        )
    )
    (defun DALOS|A_UpdatePublicKey (account:string new-public:string)
        (with-capability (DALOS-ADMIN)
            (write DALOS|AccountTable account
                {"public"     : new-public}
            )
        )
    )
    ;;[C]
    (defun DALOS|C_TransferDalosFuel (sender:string receiver:string amount:decimal)
        (install-capability (coin.TRANSFER sender receiver amount))
        (coin.transfer sender receiver amount)
    )
    (defun DALOS|C_TransferRawDalosFuel (sender:string amount:decimal)
        (let*
            (
                (kadena-split:[decimal] (UTILS.IGNIS|UC_KadenaSplit amount))
                (am0:decimal (at 0 kadena-split))
                (am1:decimal (at 1 kadena-split))
                (am2:decimal (at 2 kadena-split))
                (kda-sender:string (DALOS|UR_AccountKadena sender))

                (demiurgoi:[string] (DALOS|UR_DemiurgoiID))
                (kda-cto:string (DALOS|UR_AccountKadena (at 0 demiurgoi)))
                (kda-hov:string (DALOS|UR_AccountKadena (at 1 demiurgoi)))
                (kda-ouroboros:string (DALOS|UR_AccountKadena OUROBOROS|SC_NAME))
                (kda-dalos:string (DALOS|UR_AccountKadena DALOS|SC_NAME))
            )
            (DALOS|C_TransferDalosFuel kda-sender kda-cto am0)          ;; 5% to KDA-CTO
            (DALOS|C_TransferDalosFuel kda-sender kda-hov am0)          ;; 5% to KDA-HOV
            (DALOS|C_TransferDalosFuel kda-sender kda-ouroboros am1)    ;;15% to KDA-Ouroboros (to be used for Liquid Kadena Protocol Fueling)
            (DALOS|C_TransferDalosFuel kda-sender kda-dalos am2)        ;;75% to KDA-Dalos (to be used for DALOS Gas Station)
        )
    )
    (defun DALOS|A_DeployStandardAccount (account:string guard:guard kadena:string public:string)
        (with-capability (DALOS-ADMIN)
            (DALOS|C_DeployStandardAccount account guard kadena public)
        )
    )
    (defun DALOS|C_DeployStandardAccount (account:string guard:guard kadena:string public:string)
        (enforce-one
            "Standard Deployment not permitted"
            [
                (enforce-guard (create-capability-guard (DALOS-ADMIN)))
                (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
            ]
        )
        (with-capability (DALOS|DEPLOY_STANDARD account guard kadena)
            (insert DALOS|AccountTable account
                { "public"                      : public
                , "guard"                       : guard
                , "kadena-konto"                : kadena
                , "sovereign"                   : account
                , "governor"                    : guard

                , "smart-contract"              : false
                , "payable-as-smart-contract"   : false
                , "payable-by-smart-contract"   : false
                , "payable-by-method"           : false
                
                , "nonce"                       : 0
                , "elite"                       : DALOS|PLEB
                , "ouroboros"                   : DPTF|BLANK
                , "ignis"                       : DPTF|BLANK
                }  
            )
            (DALOS|X_UpdateKadenaLedger kadena account true)
            (if (not (IGNIS|URC_IsNativeGasZero))
                (DALOS|C_TransferRawDalosFuel account (DALOS|UR_UsagePrice "standard"))
                true
            )
        )
    )
    (defun DALOS|A_DeploySmartAccount (account:string guard:guard kadena:string sovereign:string public:string)
        (with-capability (DALOS-ADMIN)
            (DALOS|C_DeploySmartAccount account guard kadena sovereign public)
        )
    )
    (defun DALOS|C_DeploySmartAccount (account:string guard:guard kadena:string sovereign:string public:string)
        (enforce-one
            "Smart Deployment not permitted"
            [
                (enforce-guard (create-capability-guard (DALOS-ADMIN)))
                (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
            ]
        )
        (with-capability (DALOS|DEPLOY_SMART account guard kadena sovereign)
            (insert DALOS|AccountTable account
                { "public"                      : public
                , "guard"                       : guard
                , "kadena-konto"                : kadena
                , "sovereign"                   : sovereign
                , "governor"                    : guard

                , "smart-contract"              : true
                , "payable-as-smart-contract"   : false
                , "payable-by-smart-contract"   : false
                , "payable-by-method"           : true
                
                , "nonce"                       : 0
                , "elite"                       : DALOS|PLEB
                , "ouroboros"                   : DPTF|BLANK
                , "ignis"                       : DPTF|BLANK
                }  
            )
            (DALOS|X_UpdateKadenaLedger kadena account true)
            (if (not (IGNIS|URC_IsNativeGasZero))
                (DALOS|C_TransferRawDalosFuel account (DALOS|UR_UsagePrice "smart"))
                true
            )
        )
    )
    (defun DALOS|C_RotateGuard (patron:string account:string new-guard:guard safe:bool)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (let
            (
                (ZG:bool (IGNIS|URC_IsVirtualGasZero))
            )
            (with-capability (DALOS|ROTATE_ACCOUNT account)
                (if (= ZG false)
                    (IGNIS|X_Collect patron account GAS_SMALL)
                    true
                )
                (DALOS|X_RotateGuard account new-guard safe)
                (DALOS|X_IncrementNonce patron)
            )
        )
    )
    (defun DALOS|C_RotateKadena (patron:string account:string kadena:string)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (let
            (
                (ZG:bool (IGNIS|URC_IsVirtualGasZero))
                (current-kadena:string (DALOS|UR_AccountKadena account))
            )
            (with-capability (DALOS|ROTATE_ACCOUNT account)
                (if (= ZG false)
                    (IGNIS|X_Collect patron account GAS_SMALL)
                    true
                )
                (DALOS|X_RotateKadena account kadena)
                (DALOS|X_UpdateKadenaLedger current-kadena account false)
                (DALOS|X_UpdateKadenaLedger kadena account true)
                (DALOS|X_IncrementNonce patron)
            )
        )
    )
    (defun DALOS|C_RotateSovereign (patron:string account:string new-sovereign:string)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (let
            (
                (ZG:bool (IGNIS|URC_IsVirtualGasZero))
            )
            (with-capability (DALOS|ROTATE_SOVEREIGN account new-sovereign)
                (if (= ZG false)
                    (IGNIS|X_Collect patron account GAS_SMALL)
                    true
                )
                (DALOS|X_RotateSovereign account new-sovereign)
                (DALOS|X_IncrementNonce patron)
            )
        )
    )
    (defun DALOS|C_RotateGovernor (patron:string account:string governor:guard)
        (enforce-one
            "Rotate Governor not permitted"
            [
                (enforce-guard (C_ReadPolicy "ATS|Sum"))
                (enforce-guard (C_ReadPolicy "VST|Summoner"))
                (enforce-guard (C_ReadPolicy "LIQUID|Summoner"))
                (enforce-guard (C_ReadPolicy "OUROBOROS|Summoner"))
                (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
            ]
        )
        (let
            (
                (ZG:bool (IGNIS|URC_IsVirtualGasZero))
            )
            (with-capability (DALOS|ROTATE_GOVERNOR account)
                (if (= ZG false)
                    (IGNIS|X_Collect patron account GAS_SMALL)
                    true
                )
                (DALOS|X_RotateGovernor account governor)
                (DALOS|X_IncrementNonce patron)
            )
        )
    )
    (defun DALOS|C_ControlSmartAccount (patron:string account:string payable-as-smart-contract:bool payable-by-smart-contract:bool payable-by-method:bool)
        (enforce-guard (C_ReadPolicy "TALOS|Summoner"))
        (let
            (
                (ZG:bool (IGNIS|URC_IsVirtualGasZero))
            )
            (with-capability (DALOS|CONTROL_SMART-ACCOUNT patron account payable-as-smart-contract payable-by-smart-contract)
                (if (= ZG false)
                    (IGNIS|X_Collect patron account GAS_SMALL)
                    true
                )
                (DALOS|X_UpdateSmartAccountParameters account payable-as-smart-contract payable-by-smart-contract payable-by-method)
                (DALOS|X_IncrementNonce patron)
            )
        )
    )
    ;;[X]
    (defun DALOS|X_RotateGuard (account:string new-guard:guard safe:bool)
        (require-capability (DALOS|CF|OWNER account))
        (if safe
            (enforce-guard new-guard)
            true
        )
        (update DALOS|AccountTable account
            {"guard"                        : new-guard}
        )
    )
    (defun DALOS|X_RotateKadena (account:string kadena:string)
        (require-capability (DALOS|CF|OWNER account))
        (update DALOS|AccountTable account
            {"kadena-konto"                  : kadena}
        )
    )
    (defun DALOS|X_RotateSovereign (account:string new-sovereign:string)
        (require-capability (DALOS|SOVEREIGN account new-sovereign))
        (update DALOS|AccountTable account
            {"sovereign"                        : new-sovereign}
        )
    )
    (defun DALOS|X_RotateGovernor (account:string governor:guard)
        (require-capability (DALOS|GOVERNOR account))
        (update DALOS|AccountTable account
            {"governor"                        : governor}
        )
    )
    (defun DALOS|X_UpdateSmartAccountParameters (account:string pasc:bool pbsc:bool pbm:bool)
        (require-capability (DALOS|X_CONTROL_SMART-ACCOUNT account pasc pbsc pbm))
        (update DALOS|AccountTable account
            {"payable-as-smart-contract"    : pasc
            ,"payable-by-smart-contract"    : pbsc
            ,"payable-by-method"            : pbm}
        )
    )

    (defun DALOS|X_UpdateElite (account:string amount:decimal)
        (enforce-one
            "Update Elite Account not permitted"
            [
                (enforce-guard (C_ReadPolicy "BASIS|UpdateElite"))
                (enforce-guard (C_ReadPolicy "TFT|UpdElite"))
            ]
        )
        (if (= (DALOS|UR_AccountType account) false)
            (update DALOS|AccountTable account
                { "elite" : (UTILS.ATS|UCC_Elite amount)}
            )
            true
        )
    )
    (defun DALOS|X_IncrementNonce (client:string)
        (enforce-one
            "Nonce increase not permitted"
            [
                (enforce-guard (create-capability-guard (DALOS|INCREMENT_NONCE)))
                (enforce-guard (C_ReadPolicy "BASIS|IncrementDalosNonce"))
                (enforce-guard (C_ReadPolicy "ATS|PlusDalosNonce"))
                (enforce-guard (C_ReadPolicy "ATSI|PlusDalosNonce"))
                (enforce-guard (C_ReadPolicy "TFT|PlusDalosNonce"))
                (enforce-guard (C_ReadPolicy "ATSM|PlusDalosNonce"))
                (enforce-guard (C_ReadPolicy "VST|PlusDalosNonce"))
                (enforce-guard (C_ReadPolicy "BRD|IncrementDalosNonce"))
            ]
        )
        (with-read DALOS|AccountTable client
            { "nonce"                       := n }
            (update DALOS|AccountTable client { "nonce" : (+ n 1)})
        )
    )
    (defun IGNIS|X_Collect (patron:string active-account:string amount:decimal)
        (enforce-one
            "Gas Collection not permitted"
            [
                (enforce-guard (create-capability-guard (IGNIS|COLLECTER)))
                (enforce-guard (C_ReadPolicy "BASIS|GasCollection"))
                (enforce-guard (C_ReadPolicy "ATS|GasCol"))
                (enforce-guard (C_ReadPolicy "ATSI|GasCol"))
                (enforce-guard (C_ReadPolicy "TFT|GasCol"))
                (enforce-guard (C_ReadPolicy "ATSM|GasCol"))
                (enforce-guard (C_ReadPolicy "VST|GasCol"))
                (enforce-guard (C_ReadPolicy "BRD|GasCollection"))
            ]
        )
        (with-capability (IGNIS|COLLECT patron active-account amount)
            (IGNIS|XP_Collect patron active-account amount)
        )
    )
    (defun IGNIS|XP_Collect (patron:string active-account:string amount:decimal)
        (require-capability (IGNIS|COLLECT patron active-account amount))
        (let
            (
                (sender-type:bool (DALOS|UR_AccountType active-account))
                (account-type:bool (DALOS|UR_AccountType patron))
            )
            (if (not account-type)
                (if (= sender-type false)
                    (IGNIS|X_CollectStandard patron amount)
                    (IGNIS|X_CollectSmart patron active-account amount)
                )
                true
            )
            
        )
    )
    (defun IGNIS|X_CollectStandard (patron:string amount:decimal)
        (require-capability (IGNIS|NORML_COLLECT patron amount))
        (IGNIS|X_Transfer patron (DALOS|UR_Tanker) amount)
        (IGNIS|X_Increment false amount)
    )
    (defun IGNIS|X_CollectSmart (patron:string active-account:string amount:decimal)
        @doc "Collects GAS when the <active-account> is a Smart DALOS Account"
        (require-capability (IGNIS|SMART_COLLECT patron active-account amount))
        (let*
            (
                (gas-pot:string (DALOS|UR_Tanker))
                (quarter:decimal (* amount GAS_QUARTER))
                (rest:decimal (- amount quarter))                
            )
            (IGNIS|X_Transfer patron gas-pot rest)              ;;Three Quarters goes to defined Gas-Tanker Account
            (IGNIS|X_Transfer patron active-account quarter)    ;;One Quarter goes to <Active-Account> because its a Smart DALOS Account
            (IGNIS|X_Increment false amount)                    ;;Increment the amount of Virtual Gas Spent
        )
    )
    (defun IGNIS|X_Increment (native:bool increment:decimal)
        (require-capability (IGNIS|INCREMENT))
        (let
            (
                (current-gas-spent:decimal (DALOS|UR_VirtualSpent))
                (current-ngas-spent:decimal (DALOS|UR_NativeSpent))
            )
            (if (= native true)
                (update DALOS|GasManagementTable DALOS|VGD
                    {"native-gas-spent" : (+ current-ngas-spent increment)}
                )
                (update DALOS|GasManagementTable DALOS|VGD
                    {"virtual-gas-spent" : (+ current-gas-spent increment)}
                )
            )
        )
    )
    (defun IGNIS|X_Transfer (sender:string receiver:string ta:decimal)
        (require-capability (IGNIS|TRANSFER sender receiver ta))
        (IGNIS|X_Debit sender ta)
        (IGNIS|X_Credit receiver ta)

    )
    (defun IGNIS|X_Debit (sender:string ta:decimal)
        (require-capability (IGNIS|DEBIT sender))
        (let
            (
                (read-gas:decimal (DALOS|UR_TrueFungible_AccountSupply sender false))
            )
            (enforce (<= ta read-gas) "Insufficient GAS for GAS-Debiting")
            (DALOS|XO_UpdateBalance sender false (- read-gas ta))
        )
    )
    (defun IGNIS|X_Credit (receiver:string ta:decimal)
        (require-capability (IGNIS|CREDIT receiver))
        (let
            (
                (read-gas:decimal (DALOS|UR_TrueFungible_AccountSupply receiver false))
            )
            (enforce (>= read-gas 0.0) "Impossible operation, negative GAS amounts detected")
            (DALOS|XO_UpdateBalance receiver false (+ read-gas ta))
        )
    )
    (defun IGNIS|X_UpdateSourcePrice (price:decimal)
        (enforce-one
            "Burn Not permitted"
            [
                (enforce-guard (create-capability-guard (DALOS-ADMIN)))
                (enforce-guard (C_ReadPolicy "SWAPER|UpdateOuroborosPrice"))
            ]
        )
        (update DALOS|GasManagementTable DALOS|VGD
            {"gas-source-price" : price}
        )
    )
    (defun IGNIS|X_Toggle (native:bool toggle:bool)
        (require-capability (DALOS-ADMIN))
        (if (= native true)
            (update DALOS|GasManagementTable DALOS|VGD
                {"native-gas-toggle" : toggle}
            )
            (update DALOS|GasManagementTable DALOS|VGD
                {"virtual-gas-toggle" : toggle}
            )
        )
    )
    (defun DALOS|XO_UpdateBalance (account:string snake-or-gas:bool new-balance:decimal)
        (enforce-one
            "Burn Not permitted"
            [
                (enforce-guard (create-capability-guard (DALOS|UP_BALANCE)))
                (enforce-guard (C_ReadPolicy "BASIS|UpdatePrimordialBalance"))
            ]
        )
        (let*
            (
                (read-burn:bool (DALOS|UR_TrueFungible_AccountRoleBurn account snake-or-gas))
                (read-mint:bool (DALOS|UR_TrueFungible_AccountRoleMint account snake-or-gas))
                (read-transfer:bool (DALOS|UR_TrueFungible_AccountRoleTransfer account snake-or-gas))
                (read-fee-exemption:bool (DALOS|UR_TrueFungible_AccountRoleFeeExemption account snake-or-gas))
                (read-frozen:bool (DALOS|UR_TrueFungible_AccountFreezeState account snake-or-gas))
                (new-obj:object{DPTF|BalanceSchema} 
                    {"balance"              : new-balance
                    ,"role-burn"            : read-burn
                    ,"role-mint"            : read-mint
                    ,"role-transfer"        : read-transfer
                    ,"role-fee-exemption"   : read-fee-exemption
                    ,"frozen"               : read-frozen}
                )
            )
            (if snake-or-gas
                (update DALOS|AccountTable account
                    {"ouroboros" : new-obj}
                )
                (update DALOS|AccountTable account
                    {"ignis" : new-obj}
                )
            )
        )
    )
    (defun DALOS|XO_UpdateBurnRole (account:string snake-or-gas:bool new-burn:bool)
        (enforce-guard (C_ReadPolicy "BASIS|UpdatePrimordialData"))
        (let*
            (
                (read-balance:decimal (DALOS|UR_TrueFungible_AccountSupply account snake-or-gas))
                (read-mint:bool (DALOS|UR_TrueFungible_AccountRoleMint account snake-or-gas))
                (read-transfer:bool (DALOS|UR_TrueFungible_AccountRoleTransfer account snake-or-gas))
                (read-fee-exemption:bool (DALOS|UR_TrueFungible_AccountRoleFeeExemption account snake-or-gas))
                (read-frozen:bool (DALOS|UR_TrueFungible_AccountFreezeState account snake-or-gas))
                (new-obj:object{DPTF|BalanceSchema} 
                    {"balance"              : read-balance
                    ,"role-burn"            : new-burn
                    ,"role-mint"            : read-mint
                    ,"role-transfer"        : read-transfer
                    ,"role-fee-exemption"   : read-fee-exemption
                    ,"frozen"               : read-frozen}
                )
            )
            (if snake-or-gas
                (update DALOS|AccountTable account
                    {"ouroboros" : new-obj}
                )
                (update DALOS|AccountTable account
                    {"ignis" : new-obj}
                )
            )
        )
    )
    (defun DALOS|XO_UpdateMintRole (account:string snake-or-gas:bool new-mint:bool)
        (enforce-guard (C_ReadPolicy "BASIS|UpdatePrimordialData"))
        (let*
            (
                (read-balance:decimal (DALOS|UR_TrueFungible_AccountSupply account snake-or-gas))
                (read-burn:bool (DALOS|UR_TrueFungible_AccountRoleBurn account snake-or-gas))
                (read-transfer:bool (DALOS|UR_TrueFungible_AccountRoleTransfer account snake-or-gas))
                (read-fee-exemption:bool (DALOS|UR_TrueFungible_AccountRoleFeeExemption account snake-or-gas))
                (read-frozen:bool (DALOS|UR_TrueFungible_AccountFreezeState account snake-or-gas))
                (new-obj:object{DPTF|BalanceSchema} 
                    {"balance"              : read-balance
                    ,"role-burn"            : read-burn
                    ,"role-mint"            : new-mint
                    ,"role-transfer"        : read-transfer
                    ,"role-fee-exemption"   : read-fee-exemption
                    ,"frozen"               : read-frozen}
                )
            )
            (if snake-or-gas
                (update DALOS|AccountTable account
                    {"ouroboros" : new-obj}
                )
                (update DALOS|AccountTable account
                    {"ignis" : new-obj}
                )
            )
        )
    )
    (defun DALOS|XO_UpdateTransferRole (account:string snake-or-gas:bool new-transfer:bool)
        (enforce-guard (C_ReadPolicy "BASIS|UpdatePrimordialData"))
        (let*
            (
                (read-balance:decimal (DALOS|UR_TrueFungible_AccountSupply account snake-or-gas))
                (read-burn:bool (DALOS|UR_TrueFungible_AccountRoleBurn account snake-or-gas))
                (read-mint:bool (DALOS|UR_TrueFungible_AccountRoleMint account snake-or-gas))
                (read-fee-exemption:bool (DALOS|UR_TrueFungible_AccountRoleFeeExemption account snake-or-gas))
                (read-frozen:bool (DALOS|UR_TrueFungible_AccountFreezeState account snake-or-gas))
                (new-obj:object{DPTF|BalanceSchema} 
                    {"balance"              : read-balance
                    ,"role-burn"            : read-burn
                    ,"role-mint"            : read-mint
                    ,"role-transfer"        : new-transfer
                    ,"role-fee-exemption"   : read-fee-exemption
                    ,"frozen"               : read-frozen}
                )
            )
            (if snake-or-gas
                (update DALOS|AccountTable account
                    {"ouroboros" : new-obj}
                )
                (update DALOS|AccountTable account
                    {"ignis" : new-obj}
                )
            )
        )
    )
    (defun DALOS|XO_UpdateFeeExemptionRole (account:string snake-or-gas:bool new-fee-exemption:bool)
        (enforce-guard (C_ReadPolicy "BASIS|UpdatePrimordialData"))
        (let*
            (
                (read-balance:decimal (DALOS|UR_TrueFungible_AccountSupply account snake-or-gas))
                (read-burn:bool (DALOS|UR_TrueFungible_AccountRoleBurn account snake-or-gas))
                (read-mint:bool (DALOS|UR_TrueFungible_AccountRoleMint account snake-or-gas))
                (read-transfer:bool (DALOS|UR_TrueFungible_AccountRoleTransfer account snake-or-gas))
                (read-frozen:bool (DALOS|UR_TrueFungible_AccountFreezeState account snake-or-gas))
                (new-obj:object{DPTF|BalanceSchema} 
                    {"balance"              : read-balance
                    ,"role-burn"            : read-burn
                    ,"role-mint"            : read-mint
                    ,"role-transfer"        : read-transfer
                    ,"role-fee-exemption"   : new-fee-exemption
                    ,"frozen"               : read-frozen}
                )
            )
            (if snake-or-gas
                (update DALOS|AccountTable account
                    {"ouroboros" : new-obj}
                )
                (update DALOS|AccountTable account
                    {"ignis" : new-obj}
                )
            )
        )
    )
    (defun DALOS|XO_UpdateFreeze (account:string snake-or-gas:bool new-freeze:bool)
        (enforce-guard (C_ReadPolicy "BASIS|UpdatePrimordialData"))
        (let*
            (
                (read-balance:decimal (DALOS|UR_TrueFungible_AccountSupply account snake-or-gas))
                (read-burn:bool (DALOS|UR_TrueFungible_AccountRoleBurn account snake-or-gas))
                (read-mint:bool (DALOS|UR_TrueFungible_AccountRoleMint account snake-or-gas))
                (read-transfer:bool (DALOS|UR_TrueFungible_AccountRoleTransfer account snake-or-gas))
                (read-fee-exemption:bool (DALOS|UR_TrueFungible_AccountRoleFeeExemption account snake-or-gas))
                (new-obj:object{DPTF|BalanceSchema} 
                    {"balance"              : read-balance
                    ,"role-burn"            : read-burn
                    ,"role-mint"            : read-mint
                    ,"role-transfer"        : read-transfer
                    ,"role-fee-exemption"   : read-fee-exemption
                    ,"frozen"               : new-freeze}
                )
            )
            (if snake-or-gas
                (update DALOS|AccountTable account
                    {"ouroboros" : new-obj}
                )
                (update DALOS|AccountTable account
                    {"ignis" : new-obj}
                )
            )
        )
    )
    (defun DALOS|X_UpdateKadenaLedger (kadena:string dalos:string direction:bool)
        (require-capability (KADENA))
        (with-default-read DALOS|KadenaLedger kadena
            { "dalos"    : [UTILS.BAR] }
            { "dalos"    := d }
            (let*
                (
                    (add-lst:[string]
                        (if (= d [UTILS.BAR])
                            [dalos]
                            (if (contains dalos d)
                                d
                                (UTILS.LIST|UC_AppendLast d dalos)
                            )
                        )
                    )
                    (data-len:integer (length d))
                    (first:string (at 0 d))
                    (rmv-lst:[string]
                        (if (and (= data-len 1)(!= first UTILS.BAR))
                            [UTILS.BAR]
                            (UTILS.LIST|UC_RemoveItem d dalos)
                        )
                    )
                )
                (if direction
                    (write DALOS|KadenaLedger kadena
                        { "dalos" : add-lst}
                    )
                    (write DALOS|KadenaLedger kadena
                        { "dalos" : rmv-lst}
                    )
                )
                
            )
        )
    )
)
