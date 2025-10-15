(module bonder-utils GOVERNANCE

  (use n_7d195c48a309af310bc16ced11d156aff8105579.bonder)

  ;; Capabilities
  (defcap GOVERNANCE ()
    (enforce-guard
     (keyset-ref-guard "n_7d195c48a309af310bc16ced11d156aff8105579.bonder-admin")))

  (defun get-account-active-bonds-ids:[string]
    (account:string)
    @doc "Returns current bonds an account is currently participating"
    (map (at 'bond-id) (get-account-active-bonds account))
    )

  (defun get-account-bonds:[object{lockup}]
    (account:string)
    @doc "Retrieves all bonds associated with a specified account."
    (select lockups (where 'account (= account))))

  (defun get-account-active-bonds:[object{lockup}]
    (account:string)
    @doc "Retrieves all active bonds associated with a specified account."
    (filter (where 'has-claimed (= false)) (get-account-bonds account))
    )

  (defun read-all-lockups-bond:[object{lockup}]
    (bond-id:string)
    @doc "Reads all lockups associated with a specified bond ID."
    (filter (where 'bond-id (= bond-id)) (read-all-lockups)))

  (defun read-all-lockups:[object{lockup}]
    ()
    @doc "Reads all lockups."
    (select lockups (constantly true)))

  (defun read-all-bonds:[object{bond-sale}] ()
         @doc "Get all bonds info"
         (select bond-sales (constantly true)))


  )

