(module timpi-kda-to-cosmos GOVERNANCE
    
    @doc "Contract to collect keplr addresses for KDLaunch timpi IDO investors"


    (use coin)


;;------------------------------------------------------
;;Constants
;;------------------------------------------------------

(defconst RATIO 80)



;;------------------------------------------------------
;;Schemas and tables
;;------------------------------------------------------

    (defschema timpi-allocations
        @doc "Table to store kda addresses and Timpi allocations"
        
        kda-address:string
        allocation:decimal
        submitted:bool)

    (defschema keplr-addresses
        @doc "Table to store keplr addresses"
        
        keplr-address:string
        kda-address:string
        allocation:decimal)

    (deftable timpi-allocations-table:{timpi-allocations})
    (deftable keplr-addresses-table:{keplr-addresses})

;;------------------------------------------------------------------------------
;;Capabilities
;; -----------------------------------------------------------------------------

    (defcap GOVERNANCE ()
        @doc "Governance capability"
        (enforce-keyset  "free.timpi-admin" ))

    (defcap ACCT_GUARD (account)
        @doc "Capability to enforce the function is being called by the account owner"
        (enforce-keyset (at 'guard (coin.details account))))

    (defcap INTERNAL ()
        @doc "Capability to ensure Interal usage of functions"
        true)


;; -----------------------------------------------------------------------------      
;; Functions
;; -----------------------------------------------------------------------------


    (defun import-kda-allocations-file (ido-data-array:[object])
        @doc "Import allocations to each address"
        (with-capability (GOVERNANCE)
            (with-capability (INTERNAL)
                (map (import-kda-allocation) ido-data-array)
            )    
        )
        true
    )

    (defun import-kda-allocation (ido-data:object)
        @doc "Import allocation to address"
        (require-capability (INTERNAL))
        (insert timpi-allocations-table (at "kda-address" ido-data)
            {
            "kda-address":(at "kda-address" ido-data)
            ,"allocation":(*(- (at "allocation" ido-data) 0.0001)RATIO)
            ,"submitted":false
            }
        )
    )


    (defun write-keplr-address (keplr-address:string kda-address:string)
        @doc "Write keplr address to table"
        (with-capability (ACCT_GUARD kda-address)
            (enforce (!= (length keplr-address) 0)"Keplr address cannot be empty")
            (with-read timpi-allocations-table kda-address {"allocation" := allocation, "submitted" := submitted}
                (enforce (not submitted) "Allocation already submitted")
                (insert keplr-addresses-table keplr-address
                    {
                    "keplr-address":keplr-address
                    ,"kda-address":kda-address
                    ,"allocation":allocation
                    
                    }
                )
            )
            (update timpi-allocations-table kda-address {"submitted":true})
        )true
    )


    (defun get-allocations ()
        @doc "Get all allocations"
        (with-capability (GOVERNANCE)
            (select timpi-allocations-table  (constantly true))
        )
    )

    (defun get-keplr-allocations ()
    (with-capability (GOVERNANCE)
        (select keplr-addresses-table  (constantly true))
    )
)

    (defun get-allocation-with-kda (kda-address:string)
        @doc "Get allocation for kadena address"
        (select timpi-allocations-table ["allocation"]  (where "kda-address" (= kda-address))))

        
        (defun get-allocation-with-keplr-address (keplr-address:string)
        @doc "Get allocation for keplr address"
        (select keplr-addresses-table ["allocation" "kda-address"]  (where "keplr-address" (= keplr-address))))
      
        







)
        
        

