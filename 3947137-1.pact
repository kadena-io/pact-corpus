(module tokensale GOVERNANCE

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  ;define projects schema
  (defschema projects
    projectId:string
    title:string
    token:module{fungible-v2}
    hardCap:decimal
    softCap:decimal
    raised:decimal
    ratio:decimal
    startDate:time
    endDate:time
    status:integer
  )

  ;define pools schema
  (defschema pools
    poolId:string
    projectId:string
    openDate:time
    closeDate:time
    poolType:string
  )

  ;define funds schema
  (defschema funds
    projectId:string
    poolId: string
    fundOwner:string
    status:integer
    amount:decimal
    timestamp:time
  )

  ;define airdrop schema
  (defschema airdrops
    projectId:string
    account:string
    claimed:bool
    amount:decimal
    claim-start:time
    claim-end:time
  )

  ;define registrations schema
  (defschema registrations
    poolId:string
    account:string
    allocation:decimal
  )

  (deftable projects-table:{projects})
  (deftable pools-table:{pools})
  (deftable funds-table:{funds})
  (deftable airdrops-table:{airdrops})
  (deftable registrations-table:{registrations})


  ; --------------------------------------------------------------------------
  ; Constants

  ; Project statusses
  (defconst CREATED 0)
  (defconst CANCELLED 1)
  (defconst SUCCEEDED 2)
  (defconst FAILED 3)

  ; Pool types
  (defconst WHITELIST "WHITELIST")
  (defconst TIERED "TIERED")

  (defconst KDL_VAULT_ACCOUNT "kdlaunch-tokensale-vault")


  ; --------------------------------------------------------------------------
  ; Utils

  (defun vault-guard:guard () (create-module-guard "vault-guard"))

  (defun get-fund-key:string (poolId:string account:string) (format "{}-{}" [poolId account]))

  (defun get-airdrop-key:string (projectId:string account:string) (format "{}-{}" [projectId account]))

  (defun get-registration-key:string (poolId:string account:string) (format "{}-{}" [poolId account]))

  (defun curr-time:time ()
    @doc "Returns current chain's block-time in time type"
    (at 'block-time (chain-data)))


  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()

    @doc " Give the admin full access to call and upgrade the module. "

    (enforce-keyset 'kdlaunch-admin)
  )

  (defcap ACCT_GUARD (account)
    (enforce-guard (at 'guard (coin.details account)))
  )

  (defcap ROLLBACK (projectId from:string)
    (with-read projects-table projectId{
      "startDate":=startDate,
      "endDate":=endDate,
      "status":= status
      }
      (let ((from-guard (at 'guard (coin.details from))
      ))
      (enforce-one "refund guard failure or unable to refund because of project state" [
        (enforce (enforce-refund endDate startDate status from-guard)
          "Campaign is not open or guards don't match")
        (enforce (= status CANCELLED) "Project has cancelled")
        (enforce (= status FAILED) "Project has failed")
        ])
      )
    )
  )

  (defun enforce-refund:bool (endDate:time startDate:time status:integer issuer-guard:guard)
      (enforce (!= status CANCELLED) "PROJECT HAS BEEN CANCELLED")
      (enforce (!= status FAILED) "PROJECT HAS FAILED")
      (enforce (!= status SUCCEEDED) "PROJECT HAS ALREADY SUCCEEDED")
      (enforce (< (curr-time) endDate) "PROJECT HAS ENDED")
      (enforce (>= (curr-time) startDate) "PROJECT HAS NOT STARTED")
      (enforce-guard issuer-guard)
    )

  (defcap CANCEL:bool (projectId)
    (with-read projects-table projectId{
      "status":=status
      }
      (enforce (= status CANCELLED) "NOT CANCELLED")
    )
  )

  (defcap POOL_OPEN:bool (poolId projectId)
    (with-read pools-table poolId{
      "openDate":=openDate,
      "closeDate":=closeDate
      }

      (with-read projects-table projectId{
        "status":=status
        }

        (enforce (!= status CANCELLED) "PROJECT HAS BEEN CANCELLED")
        (enforce (< (curr-time) closeDate) "POOL HAS CLOSED")
        (enforce (>= (curr-time) openDate) "POOL IS NOT YET OPEN")
      )
    )
  )

  (defcap SUCCESS:bool (projectId)
    (with-read projects-table projectId{
      "softCap":=softCap,
      "hardCap":=hardCap,
      "raised":=raised,
      "endDate":=endDate,
      "status":=status
      }

      (enforce (or (>= (curr-time) endDate) (>= raised hardCap)) "PROJECT HAS NOT ENDED OR HARDCAP NOT MET")
      (enforce (>= raised softCap) "CAMPAIGN HAS NOT RAISED ENOUGH")
      (enforce (!= status CANCELLED) "CAMPAIGN HAS BEEN CANCELLED")
    )
  )

  (defcap FAIL:bool (projectId)
    (with-read projects-table projectId{
      "softCap":=softCap,
      "endDate":=endDate,
      "raised":=raised,
      "status":=status
      }
      (enforce (!= status CANCELLED) "PROJECT HAS BEEN CANCELLED")
      (enforce (>= (curr-time) endDate) "PROJECT HAS NOT ENDED")
      (enforce (< raised softCap) "PROJECT HAS SUCCEEDED"))
  )

  (defcap ALLOW_TOKEN_DISTRIBUTION:bool (projectId)
    (with-read projects-table projectId{
      "status":=status
      }
      (enforce (= status SUCCEEDED) "PROJECT HAS NOT BEEN MARKED SUCCESS, DISTRIBUTION NOT YET ALLOWED")
    )
  )

  (defcap ALLOW_AIRDROP_DISTRIBUTION:bool (projectId account)
    (with-read airdrops-table (get-airdrop-key projectId account) {
      "claim-start":= claim-start,
      "claim-end":= claim-end,
      "claimed":= claimed
      }

      (enforce (= claimed false) "ALREADY CLAIMED")
      (enforce (< (curr-time) claim-end) "AIRDROP CLAIMING HAS ENDED")
      (enforce (>= (curr-time) claim-start) "AIRDROP CLAIMING NOT OPEN YET")
    )
  )

  (defcap REFUND () true)
  (defcap RAISE () true)

  ; --------------------------------------------------------------------------
  ; Sale configuration functions

  (defun create-project (
    projectId:string
    title:string
    token:module{fungible-v2}
    hardCap:decimal
    softCap:decimal
    ratio:decimal
    startDate:time
    endDate:time)
    "Adds a project to projects table"
    (enforce (< (curr-time) startDate) "Start Date shouldn't be in the past")
    (enforce (< startDate endDate) "Start Date should be before end date")
    (enforce (< 0.0 hardCap) "Hard cap is not a positive number")
    (enforce (< 0.0 softCap) "Soft cap is not a positive number")
    (enforce (< 0.0 ratio) "Ratio is not a positive number")

    (with-capability (GOVERNANCE)
        (insert projects-table projectId {
            "projectId":projectId,
            "title":title,
            "hardCap":hardCap,
            "softCap":softCap,
            "ratio":ratio,
            "token":token,
            "raised": 0.0,
            "startDate":startDate,
            "endDate":endDate,
            "status": CREATED
            }))
  )

  (defun create-pool (
    poolId:string
    projectId:string
    openDate:time
    closeDate:time
    poolType:string
    )
    "Adds a pool to the pools table"
    (enforce (< (curr-time) openDate) "Open Date shouldn't be in the past")
    (enforce (< openDate closeDate) "Start Date should be before close date")

    (with-capability (GOVERNANCE)
        (insert pools-table poolId {
            "poolId":poolId,
            "projectId":projectId,
            "openDate":openDate,
            "closeDate":closeDate,
            "poolType":poolType
            })
    )
  )

  (defun deposit-tokens (projectId owner amount)
    (with-read projects-table projectId {
      "token":= customToken:module{fungible-v2}
      }
      (customToken::transfer-create owner KDL_VAULT_ACCOUNT (vault-guard) amount)
    )
  )

  (defun withdraw-tokens (projectId projectOwner projectOwnerGuard tokenAmount)
    (with-capability (GOVERNANCE)
      (with-read projects-table projectId {
        "token":= customToken:module{fungible-v2}
        }

        (customToken::transfer-create KDL_VAULT_ACCOUNT projectOwner projectOwnerGuard tokenAmount)
      )
    )
  )

  (defun cancel-project (projectId)
    (with-capability (GOVERNANCE)
      (update projects-table projectId {
          "status": CANCELLED
       }))
  )

  (defun succeed-project (projectId projectOwner projectOwnerGuard)
    (with-capability (GOVERNANCE)
      (with-capability (SUCCESS projectId)
        (update projects-table projectId {
            "status": SUCCEEDED
        })
        (with-read projects-table projectId {
          "raised":= raised
          }
          (coin.transfer-create KDL_VAULT_ACCOUNT projectOwner projectOwnerGuard raised)
        )
      )
    )
  )

  (defun fail-project (projectId)
    (with-capability (GOVERNANCE)
      (with-capability (FAIL projectId)
        (update projects-table projectId {
            "status": FAILED
        })))
  )

  ; --------------------------------------------------------------------------
  ; Tokensale functions

  (defun create-fund (projectId poolId funder amount)
    (require-capability (RAISE))
      (with-default-read funds-table (get-fund-key poolId funder)
        {
          "amount": 0.0
        }
        {
          "amount":= fundedAmount
        }
          (write funds-table (get-fund-key poolId funder) {
            "projectId":projectId,
            "poolId":poolId,
            "fundOwner":funder,
            "amount":(+ amount fundedAmount),
            "timestamp":(curr-time),
            "status":CREATED
            }))
  )

  (defun cancel-fund (projectId poolId funder)
    (require-capability (ROLLBACK projectId funder))
    (update funds-table (get-fund-key poolId funder) {
      "status":CANCELLED,
      "amount": 0.0
      }))

  (defun fetch-fundings:list (projectId:string)
    (with-capability (GOVERNANCE)
      (select funds-table ['fundOwner 'amount 'poolId] (and? (where 'status (= CREATED)) (where 'projectId (= projectId))))
  ))

  (defun fetch-funded-amount (poolId:string fundOwner:string)
    (with-default-read funds-table (get-fund-key poolId fundOwner)
    {
      "amount": 0.0
    }
    {
      "amount":= fundedAmount
    }
      fundedAmount
    )
  )

  (defun fetch-pools:list (projectId:string)
    (select pools-table (where 'projectId (= projectId))))

  (defun raise-project (projectId amount)
    (require-capability (RAISE))
    (with-read projects-table projectId {
      "raised":= raised
      }
      (update projects-table projectId {
        "raised": (+ raised amount)
        }))
  )

  (defun refund-project (projectId amount)
    (require-capability (REFUND))
    (with-read projects-table projectId {
      "raised":= raised
      }
      (update projects-table projectId {
        "raised": (- raised amount)
        }
      )
    )
  )

  (defun get-remaining-allocation (poolId account)
    (with-default-read registrations-table (get-registration-key poolId account)
      { "allocation": 0.0 }
      { "allocation":= allocation }

      (let*
        (
          (investedAmount (fetch-funded-amount poolId account))
          (remainingAllocation (- allocation investedAmount))
        )

        remainingAllocation
      )
    )
  )

  (defun fund-project (projectId poolId from amount)
    (with-capability (ACCT_GUARD from)
      (with-capability (POOL_OPEN poolId projectId)
        (with-capability (RAISE)
          (with-read projects-table projectId {
            "raised":= raised,
            "hardCap":= hardCap
            }

            (let*
              (
                (remainingAllocation (get-remaining-allocation poolId from))
                (remainingProjectCap (- hardCap raised))
                (allocatedAmount (if (< amount remainingAllocation) amount remainingAllocation))
                (fundAmount (if (< allocatedAmount remainingProjectCap) allocatedAmount remainingProjectCap))
              )

              (enforce (< raised hardCap) "HARDCAP REACHED")
              (enforce (> fundAmount 0.0) "Allocation depleted")

              (coin.transfer-create from KDL_VAULT_ACCOUNT (vault-guard) fundAmount)
              (create-fund projectId poolId from fundAmount)
              (raise-project projectId fundAmount)
            )
          )))))

  (defun rollback-fund-project (projectId poolId funder)
    (with-capability (ACCT_GUARD funder)
      (with-capability (REFUND)
        (with-capability (ROLLBACK projectId funder)
          (with-read funds-table (get-fund-key poolId funder) {
            "amount":= amount,
            "status":= status
            }
              (enforce (= status CREATED) "NO ACTIVE FUNDS")

              (cancel-fund projectId poolId funder)
              (coin.transfer KDL_VAULT_ACCOUNT funder amount)
              (refund-project projectId amount)
            )))))

  (defun claim-tokens (projectId poolId funder)
    (with-capability (SUCCESS projectId)
    (with-capability (ALLOW_TOKEN_DISTRIBUTION projectId)
    (with-read funds-table (get-fund-key poolId funder) {
      "amount":= fundAmount,
      "status":= status
      }
        (enforce (= status CREATED) "NO ACTIVE FUNDS")

        (with-read projects-table projectId {
          "token":= customToken:module{fungible-v2},
          "ratio":= ratio
        }

          (let ((funder-guard (at 'guard (coin.details funder))))
            (let ((tokenAmount (* ratio fundAmount)))
              (customToken::transfer-create KDL_VAULT_ACCOUNT funder funder-guard tokenAmount)
              (update funds-table (get-fund-key poolId funder) {
                "status":SUCCEEDED
                })
          ))))))
  )

  (defun claim-airdrop (projectId account)
    (with-capability (ALLOW_AIRDROP_DISTRIBUTION projectId account)
      (with-read airdrops-table (get-airdrop-key projectId account) {
        "claim-start":= claim-start,
        "claim-end":= claim-end,
        "claimed":= claimed,
        "amount":= amount
        }

        (with-read projects-table projectId {
          "token":= customToken:module{fungible-v2}
        }
          (update airdrops-table (get-airdrop-key projectId account) {
            "claimed": true,
            "amount": 0.0
          })
          (customToken::transfer-create KDL_VAULT_ACCOUNT account (at 'guard (coin.details account)) amount)
        )
    ))
  )

  ; --------------------------------------------------------------------------
  ; Front-end functions

  (defun read-project-fundstate (projectId)
    (with-read projects-table projectId {
      "raised":= raised,
      "hardCap":= hardCap,
      "softCap":= softCap,
      "status":= status
      }
      { "raised": raised, "hardCap": hardCap, "softCap": softCap, "status": status }
    )
  )

  (defun read-projects:list ()
    "Read all projects in projects table"
    (select projects-table
      ['projectId 'title 'hardCap 'softCap 'token 'raised 'startDate 'endDate 'status]
      (constantly true)
    )
  )

  (defun create-kda-account (account guard)
    (coin.create-account account guard)
  )

  (defun fetch-claimable-tokens:list (projectId:string fundOwner:string)
    (select funds-table ['fundOwner 'amount 'poolId]  (and? (and? (where 'status (= CREATED)) (where 'projectId (= projectId)))
                                                      (where 'fundOwner (= fundOwner)))
    )
  )

  (defun fetch-airdrop (projectId:string account:string)
    (with-default-read airdrops-table (get-airdrop-key projectId account)
      {
        "amount": 0.0
      }
      {
        "amount":= airdropAmount
      }
      airdropAmount
    )
  )
)

; --------------------------------------------------------------------------
; Table creation


