(module igp GOVERNANCE
  (implements igp-iface)
 
  (use igp-iface)
 
  ;; Tables
  (deftable contract-state:{igp-state})

  (deftable gas-amount-table:{remote-gas-amount})

  ;; Capabilities
  (defcap GOVERNANCE () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.upgrade-admin"))

  (defcap ONLY_ADMIN () (enforce-guard "n_e595727b657fbbb3b8e362a05a7bb8d12865c1ff.bridge-admin"))

  (defcap IGP () true)

  ;; Events
  (defcap GAS_PAYMENT
    (
      message-id:string
      destination-domain:integer
      gas-amount:integer
      kda-amount:integer  
    )
    @doc "Emitted when gas payment is transferred to treasury"
    @event true
  )

  ;; Treasury 
  (defconst IGP_ACCOUNT (create-principal (create-igp-guard)))

  (defun get-igp-account ()
      IGP_ACCOUNT
  )
  
  (defun create-igp-guard:guard ()
    (create-capability-guard (IGP))
  )

  (defun initialize ()
   (with-capability (ONLY_ADMIN)
      (insert contract-state "default"
         {
            "treasury": IGP_ACCOUNT
         }
      )
    )
    (coin.create-account IGP_ACCOUNT (create-igp-guard))
  )

  (defun set-remote-gas-amount (config:object{remote-gas-amount-input})
    (with-capability (ONLY_ADMIN)
      (bind config
        {
          "domain" := domain,
          "gas-amount" := gas-amount
        }
        (write gas-amount-table (int-to-str 10 domain)
          {
            "gas-amount": gas-amount
          }
        )
      )
      true
    )
  )

  (defun withdraw-kda (address:string amount:decimal)
    (with-capability (ONLY_ADMIN)
      (with-capability (IGP)
        (coin.transfer IGP_ACCOUNT address amount)
      )
    )
  )

  ;; An example: we transfer from Kadena to Ethereum
  ;; Gas amount required = 300_000 units
  ;; Gas price = 17 gwei (early morning - low fees)
  ;; Remote tx price = 300_000 * 17_000_000_000 = 5.1e15 wei (7.92 USD)
  
  ;; ETH price = 1555 USD, Kadena price = 0.41 USD =>
  ;; => Token exchange rate = 1555/0.41 = 3.792e3
  ;; Kadena tx price = 5.1e15 * 3.792e3 = 19.33e18 KDA (7.92 USD)

  ;; Another example: we transfer from Kadena to MockChain
  ;; Gas amount required = 2800 units
  ;; Gas price = 0.00051
  ;; Remote tx price = 2.8e3 * 5.1e-4 = 1.428 (0.002856 USD)

  ;; MockChain price = 0.002 USD, Kadena price = 0.52 USD => 
  ;; => Token exchange rate = 0.002 / 0.52 = 3.84e-3
  ;; Kadenx tx price = 1.428 * 3.84e-3 = 0.00548352 (0.002851 USD)
  

  (defun quote-gas-payment:decimal (domain:integer)
    (with-read gas-amount-table (int-to-str 10 domain)
      {
        "gas-amount" := gas-amount
      }
      (bind (gas-oracle.get-exchange-rate-and-gas-price domain)
        {
          "token-exchange-rate" := token-exchange-rate,
          "gas-price" := gas-price
        }
        (* (* gas-amount gas-price) token-exchange-rate)
      )
    )    
  )

  (defun pay-for-gas:bool (id:string domain:integer gas-amount:decimal)
    (let
      (
        (kda-amount:decimal (quote-gas-payment domain))
      )
      (with-read contract-state "default"
        {
          "treasury" := treasury:string
        }
        (coin.transfer (at "sender" (chain-data)) treasury kda-amount)
        (emit-event (GAS_PAYMENT id domain (round (* gas-amount (dec (^ 10 (coin.precision))))) (round (* kda-amount (dec (^ 10 (coin.precision)))))))
      )
      true
    )
  )

)


