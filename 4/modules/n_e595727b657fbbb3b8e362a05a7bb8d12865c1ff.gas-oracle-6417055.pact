(module gas-oracle GOVERNANCE
  (implements gas-oracle-iface)
  
  (use gas-oracle-iface)
  ;; Tables
  (deftable gas-data-table:{remote-gas-data})
  
  ;; Capabilities
  (defcap GOVERNANCE () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.upgrade-admin"))

  (defcap ONLY_ORACLE_ADMIN () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.gas-oracle-admin"))

  ;; Events
  (defcap REMOTE_GAS_DATA_SET
    (
      domain:integer
      token-exchange-rate:decimal
      gas-price:decimal
    )
    @doc "Emitted when an entry in `remoteGasData` is set."
    @event true
  )

  (defun set-remote-gas-data-configs:bool (configs:[object{remote-gas-data-input}])
    (map (set-remote-gas-data) configs)
    true
  )

  (defun set-remote-gas-data:bool (config:object{remote-gas-data-input})
    (with-capability (ONLY_ORACLE_ADMIN)
      (bind config
        {
          "domain" := domain,
          "token-exchange-rate" := token-exchange-rate,
          "gas-price" := gas-price
        }
        (write gas-data-table (int-to-str 10 domain)
          {
            "token-exchange-rate": token-exchange-rate,
            "gas-price": gas-price
          }
        )
        (emit-event (REMOTE_GAS_DATA_SET domain token-exchange-rate gas-price))
        true
      )
    )
  )
  
  (defun get-exchange-rate-and-gas-price:object{remote-gas-data} (domain:integer)
    (read gas-data-table (int-to-str 10 domain))
  )
)
  

