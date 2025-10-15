(module merkle-tree-hook GOVERNANCE

    (implements hook-iface)

    (use hyperlane-message)

    (defcap GOVERNANCE () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.upgrade-admin"))

    (defcap ONLY_ADMIN () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.bridge-admin"))

    (defcap INTERNAL () true)

    (defcap INSERTED_INTO_TREE
        (
            id:string
            index:integer
        )
        @event true
    )
    
    (defconst TREE_DEPTH 32)
    (defconst MAX_LEAVES (- (^ 2 TREE_DEPTH) 1))

    (defschema tree-schema
        branches:[string]
        count:integer
    )

    (deftable tree-state:{tree-schema})

    (defconst EMPTY:object{tree-schema} { "branches": (make-list TREE_DEPTH "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"), "count": 0 })

    (defconst ZEROES
        [
            "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
            "rTIotnb3081ChKVEPxfxlis25JGzCkCyQFhJ5Ze6X7U"
            "tMEZUZV8b49kLEr2HNayRkD-xtx_xgfuggapnpJBDTA"
            "Id25o1aBXD-sECa23sXfMSSvuttIXJulo-M5igS3uoU"
            "5Ydpsyob6vHqJzdaRAlaDR-2ZM4t01jn_L-3jCahk0Q"
            "DrAev8ntJ1AM1N_JeSctHwkTzJ9mVA1-gAWBEQnhzy0"
            "iHwivYdQ00AWrDxmtf8QLazdc_awFOcQtR6AIq-aGWg"
            "_9cBV-SAY_wzyXoFD39kAjO_ZGzJjZUkxrkrzzq1b4M"
            "mGfMX38Za5O64eJ-YyB0JEXSkPImOCdJi1T-xTn3Vq8"
            "zvrU5QjAmLmn4dj-sZlV-wK6lnVYUHhxCWnTRA9QVOA"
            "-dw-f-AW4FDv8mAzTxil1P45HYIJIxn1lk8uLrfBw6U"
            "-LE6SeKC9gnDF6gz-42XbRFRfFcdEiGiZdJa93js-JI"
            "NJDGzutFCuzcguKCkwMdEMfXO_heV78EGpc2CqLF2Zw"
            "wd-C2cS4dBPq4u8Ej5S001VM6nPZKw96-W4CccaR4rs"
            "XGet18bK8wIlat7ferEU2grP6HDUSaOkifeB1lnovsw"
            "2nvOn06GGLa9L0EyznmM3Hpg5-FGCnKZ48Y0KleWJtI"
            "JzPlD1JuwvoZoisx6O1Q8jzR_flMkVTtOnYJovH_mB8"
            "4dO1yAeygeRoPMbWMVz5W5rehkHe_LMjcvHBJuOY73o"
            "Wi3OCop_aLt0Vg-PcYN8LC67y_f_-0KuGJbxP3x0eaA"
            "tGootvVVQPiURPY94DeOPRIb4J4GzJ3tHCDmWHbTaqA"
            "xl6WRWRHhrYg4t0q1kjd_L9KflsaOk7P5_ZGZ6Pwt-I"
            "9EGFiO01okWM_-s5uT0m8Y0qsTvc5q7ljnuZNZ7C39k"
            "WpwW3ADW7xi3kzpvjcZcy1VmcTh3b33qEBBw3IeW43c"
            "TfhPQK4MginQ1gaeXI85p8KZZ3oJ02f8ewXjvDgO5lI"
            "zccllfdMexBD0OH_urc0ZIyDjfsFJ9lxtgK8IWyWGe8"
            "Cr9ayXSh7Vf0BQqlEN2cdPUIJ3s515c7st_Mxe6wYY0"
            "uM10BG_zN_CnvyyOA-EPZCwYhnmNcYBqseiI2eXuh9A"
            "g4xWVcshxsuDMTtaYxF13_SWN3LM6RCBiLNKyHyBxB4"
            "Zi7k3S3XsrxweWGx5kbEBHZp3LZYTw2Ndw2vXX596y4"
            "OIqyDiVz0XGogQjnnYIOmPJsC4Sqiy9KpJaNu4GOoyI"
            "kyN8ULp17khfTCKt8vdBQAvfjWqcx99-yuV2IhZl1zU"
            "hEiBi7SuRWKEnpSeF6wW4L4WaI4Va1zxXgmMYnwAVqk"
        ]
    )

    (defun initialize ()
        (with-capability (ONLY_ADMIN)
            (insert tree-state "default"
                { 
                    "branches": (make-list TREE_DEPTH "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"), 
                    "count": 0 
                }
            )
        )
    )

    (defun post-dispatch:bool (id:string message:object{hyperlane-message})
        (require-capability (mailbox.ONLY_MAILBOX))
        (with-capability (INTERNAL)
            (insert-node id)
            true
        )
    )

    (defun insert-node (node:string)
        (require-capability (INTERNAL))
        (with-read tree-state "default"
            {
                "count" := count,
                "branches" := branches
            }
            (enforce (< count MAX_LEAVES) "tree is full")

            (let*
                (
                    (insertstep
                        (lambda (acc i)
                            (let*
                                (
                                    (currentnode (at 0 acc))
                                    (size (at 1 acc))
                                    (branches (at 2 acc))
                                    (done (at 3 acc))
                                )

                                (if done
                                    ; if we are done just return the accumulator
                                    acc
                                    ; otherwise insert
                                    (if (compare-size size)
                                        ; calculate new branches
                                        [currentnode size (+ (+ (take i branches) [currentnode]) (drop (+ i 1) branches)) true]
                                        ; otherwise iterate
                                        [(hash-keccak256 [(at i branches) currentnode]) (/ size 2) branches false]
                                    )
                                )
                            )
                        )
                    )

                    (newbranches (at 2 (fold insertstep [node (+ 1 count) branches false] (enumerate 0 TREE_DEPTH))))
                )

                (update tree-state "default"
                    { 
                        "count": (+ 1 count), 
                        "branches": newbranches 
                    }
                )

                (emit-event (INSERTED_INTO_TREE node (- (at "count" (read tree-state "default")) 1)))
            )
        )
    )

    (defun root ()
        (with-read tree-state "default"
            {
                "count" := index,
                "branches" := branches
            }
            (let*
                (
                    (calcstep
                        (lambda (acc branch)
                            (let*
                                (
                                    (current (at 0 acc))
                                    (i (at 1 acc))
                                )
                                (if (compare-ith-bit index i)
                                    [(hash-keccak256 [branch current]) (+ i 1)]
                                    [(hash-keccak256 [current (at i ZEROES)]) (+ i 1)]
                                )
                            )
                        )
                    )
                )
                (at 0 (fold calcstep ["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" 0] branches))
            )
        )
    )

    (defun branch-root (branches:[string] item:string index:integer)
        (let*
            (
                (calcstep
                    (lambda (acc branch)
                        (let*
                            (
                                (current (at 0 acc))
                                (i (at 1 acc))
                            )
                            (if (compare-ith-bit index i)
                                [(hash-keccak256 [branch current]) (+ i 1)]
                                [(hash-keccak256 [current branch]) (+ i 1)]
                            )
                        )
                    )
                )
            )
            (at 0 (fold calcstep [item 0] branches))
        )
    )

    (defun hash-new-node (branch:string node:string)
        (hash-keccak256
            [
                (base64-encode branch)
                (base64-encode node)
            ]
        )
    )

    (defun compare-size (size:integer)
        (= (& size 1) 1)
    )

    (defun compare-ith-bit (index:integer i:integer)
        (= (& (shift index (- i)) 1) 1)
    )

    (defun tree ()
    {
        "branch" : (at "branches" (read tree-state "default")),
        "count" : (count)
    }
    )

    (defun count ()
        (at "count" (read tree-state "default"))
    )

    (defun latest-checkpoint ()
        { 
            "root" : (root),
            "count" : (- (count) 1)
        }
    )
)


