(module finux-registry GOVERNANCE
    ;; -----------------------------------------------------------------------------------------------------
    ;;  capabilities
    (defcap GOVERNANCE ()
        (enforce-keyset "free.finux-registry-module-admin")
    )

    ;; -----------------------------------------------------------------------------------------------------
    ;;  functionality
    (defun get-module-hash (chain-id:integer)
        (if (= chain-id 12)
            "z49MDJPQsOaVigY68QtdQbRAY1X1f54nh_tOuSzXYyM"
            "_SbwbGL4VPflEwv6q4LihJw85Sn1XKMbo1NkSyxlRAY"
        )
    )
)
