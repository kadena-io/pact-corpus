(module ktg GOVERNANCE
(implements kaddex.swap-callable-v1)
(use kaddex.exchange [get-pair-key get-pair-by-key reserve-for add-liquidity swap remove-liquidity FEE])
(use free.util-math)
(use free.util-time)
(use free.util-lists)

(defcap GOVERNANCE ()
  (enforce-keyset "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.ktgguard")
)

(defcap OPS ()
 (enforce-guard "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.od")
)

(defcap AG (account:string)
  @doc "Proves you own this account by checking the guard"
  (enforce-guard (at 'guard (coin.details account)))
)

(defcap CREATE-CONTRACT (logo:string token-name:string contract-name:string dex:string hash:string supply:decimal sale:bool mint:bool account:string)
        @event true
)

; Internal Capabilities for handling transfers
(defcap TREASURY () true)
(defconst TREASURY-GUARD (create-capability-guard (TREASURY)))
(defconst TREASURY-ACCOUNT (create-principal TREASURY-GUARD))

(defcap SWAPS () true)
(defconst SWAP-GUARD (create-capability-guard (SWAPS)))
(defconst SWAP-ACCOUNT (create-principal SWAP-GUARD))

(defcap OD ()
(enforce-guard "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.od")
(compose-capability (SWAPS))
(compose-capability (TREASURY))
)

(defschema contract-sch
 @doc "The key value is the module + contract name"
  account:string ;; Who deployed it
  contract:string ;; Store the contract in b64
  hash:string ;; Store the hash of the contract
  version:integer ;; Increment for version control
  updated:time ;; Tracks the last update time
  logo:string ;; Provided by the contract owner
  dex:string ;; Stores Primary Dex based on supply-chain
  supply:decimal ;; Total supply of the token
  sale:bool ;; Does the token utilize sales
  mint:bool ;; Does the token utilize minting
)

(defschema pricing-schema
  cost:decimal
  updated:time
)

(deftable contract:{contract-sch})
(deftable pricing:{pricing-schema})

; Contract Functions

(defun add-contract (
    account:string
    contract-name:string
    ct:string
    h:string
    fung:string
    logo:string
    dex:string
    token-symbol:string
    supply:decimal
    sale:bool
    mint:bool
    )
    @doc "Adds a contract to the contract table"
    (enforce (or (= fung HERON) (= fung COIN)) "Invalid Token Selected")
    (with-capability (AG account)
    (if  (= COIN fung) (kda-purchase account) (burn-heron account))
        (insert contract contract-name {'account: account, 'contract:ct, 'hash:h, 
               'version: 0, 'logo:logo, 'dex:dex, 'updated:(curr-time), 'supply:supply, 'sale:sale, 'mint:mint})
        )
        (emit-event (CREATE-CONTRACT logo token-symbol contract-name dex h supply sale mint account))
)

;; We will re-implement this down the road
;; (defun update-contract (account:string contract-name:string ct:string h:string)
;;   @doc "Updates an existing contract"
;;     (with-capability (AG account)
;;         (with-default-read contract contract-name
;;         {'account:"", 'version:-1, 'logo: "", 'dex: "", 'supply: 0.0, 'sale: "", 'mint: ""}
;;         {'account:=acc, 'version:=v, 'logo:=l, 'dex:=d, 'supply:=s, 'sale:=sa, 'mint:=mint}
;;         (enforce (and (!= account "") (= account acc)) "Not your contract")
;;         (write contract contract-name {'account: (if (= v -1) account acc), 'contract:ct, 'hash:h, 
;;         'logo:l, 'dex:d, 'version: (++ v), 'updated:(curr-time), 'supply:s, 'sale:sa, 'mint:mint})
;;         )
;;     )
;;   )

; Constants

(defconst BURNWALLET "c:KI4cQnLt-DLK31X7mKay1c3DNPr-adIel5op8CkEOYo")

(defconst DEX-KEY (get-pair-key coin heron))
(defconst TXFEE:decimal 0.5)
(defconst COIN:string "coin")
(defconst HERON:string "heron")

; Transfer Functions

; Split the burn functions for simplification
(defun burn-heron:string (account:string)
  @doc "Burns Heron or KDA for Generating a Token"
  (require-capability (AG account))
  (let*
    (
      (cost:decimal (get-cost "heron"))
      (t-cost:decimal (round (* cost TXFEE) 12))
      (b-amount:decimal (- cost t-cost))
    )
    (enforce (> cost 0.0) "Cost must be greater than 0")
    
    ;; Directly burns 50% of the Heron
    (heron.transfer account BURNWALLET b-amount)
    (heron.transfer account TREASURY-ACCOUNT t-cost)

    (format "{} {} was Burned" [b-amount "Heron"]))
)

(defun kda-purchase:string (account:string)
 (require-capability (AG account))
  (let
    (
      (c:decimal (get-cost "coin"))
    )
    (enforce (>= c 15.0) "Invalid Amounts")

    ;; Transfer the required KDA amount to the swap account
    (coin.transfer-create account SWAP-ACCOUNT SWAP-GUARD c)
    
    (format "{} KDA was Purchased" [c])
  )
)

; Swap Functions

(defun swap-and-burn-kda ()
  @doc "Swaps KDA to Heron and burns the equivalent amount"
  (with-capability (OD)
    (let*
      (
        (c:decimal (coin.get-balance SWAP-ACCOUNT))
        (fee:decimal (floor (* c TXFEE) 12)) 
        (cost:decimal (- c fee)) 
        (hc:decimal (kda-to-heron cost))
        (BWG:guard (at 'guard (heron.details BURNWALLET)))
      )
      (enforce (>= cost 10.0) "Invalid Amounts")
      (enforce (> hc 10000.0) "No Heron in the swap")

      ; Transfer the fee to the treasury account
      (install-capability (coin.TRANSFER SWAP-ACCOUNT TREASURY-ACCOUNT fee))
      (coin.transfer SWAP-ACCOUNT TREASURY-ACCOUNT fee)

      ;; Perform the swap
      (swap ktg BURNWALLET BWG heron hc coin)

      (format "{} Heron was Burned worth approximately {} KDA" [hc cost])
    )
  )
)

(defun swap-call:bool (token-in:module{fungible-v2} token-out:module{fungible-v2} amount-out:decimal sender:string recipient:string recipient-guard:guard)
  (require-capability (OD))
  (install-capability (coin.TRANSFER SWAP-ACCOUNT sender (kda-balance)))
  (coin.transfer SWAP-ACCOUNT sender (kda-balance))
  true
)

; Utility Functions

(defun div:decimal (num:decimal den:decimal)
  @doc "Divide and limit numbers of decimal (gas optimisation)"
(floor (/ num den) 24))

(defun get-pair ()
 (get-pair-by-key DEX-KEY))

(defun dex-reserves:[decimal] ()
  @doc "Returns the Reserve of the DEXs to an array [$HERON $KDA]"
(let ((pair (get-pair)))
    [(reserve-for pair heron), (reserve-for pair coin)]))

(defun dex-account:string ()
  @doc "Returns the DEX's pair account"
(at 'account (get-pair)))

(defun kda-balance:decimal ()
  @doc "KDA balance of the swap account"
(coin.get-balance SWAP-ACCOUNT))

(defun heron-balance:decimal ()
  @doc "Heron balance of the swap account"
(heron.get-balance SWAP-ACCOUNT))

(defun heron-treasury:decimal ()
(heron.get-balance TREASURY-ACCOUNT))

(defun kda-to-heron:decimal (x:decimal)
  @doc "Compute the amount of heron we can have from a given amount of KDA"
  (let ((res (dex-reserves)))
    (floor (- (at 0 res) (div (prod res)
                              (+ (* x (- 1.0 FEE)) (at 1 res))))
            (heron.precision)))
)

(defun get-cost (token:string)
  @doc "Returns the price of the token"
  (enforce (or (= token "heron") (= token "coin")) "Invalid Token")
  (at 'cost (read pricing token))
)

(defun get-contract (cname:string)
  @doc "Returns the contract details"
   (read contract cname))

(defun get-code (cname:string)
  @doc "Returns the B64 contract value"
  (at 'contract (read contract cname)))

(defun get-keys ()
  (keys contract)
)

(defun curr-time:time ()
    @doc "Returns current chain's block-time"
    (at 'block-time (chain-data)))

; Internal Functions

(defun tx-coin:decimal (receiver:string guard:guard amount:decimal)
    @doc "Transfers kda to the receiver"
    (with-capability (OD)
        (coin.transfer-create TREASURY-ACCOUNT receiver guard amount)
    )
)

(defun tx-heron:decimal (receiver:string guard:guard amount:decimal)
    @doc "Transfers heron to the receiver"
    (with-capability (OD)
        (heron.transfer-create TREASURY-ACCOUNT receiver guard amount)
    )
)

(defun update-price (token:string cost:decimal)
    @doc "Updates the price of the token"
    (with-capability (OPS)
    (enforce (or (= token HERON) (= token COIN)) "Invalid Token")
    (enforce (> cost 0.0) "Cost must be greater than 0")
    (write pricing token {'cost: cost, 'updated: (curr-time)})
    )
)

(defun init ()
  (with-capability (GOVERNANCE)
  (heron.create-account SWAP-ACCOUNT SWAP-GUARD)
  (coin.create-account SWAP-ACCOUNT SWAP-GUARD)
  (coin.create-account TREASURY-ACCOUNT TREASURY-GUARD)
  (heron.create-account TREASURY-ACCOUNT TREASURY-GUARD)
    (insert pricing "heron" {'cost: 60000.0, 'updated: (curr-time)})
    (insert pricing "coin" {'cost: 25.0, 'updated: (curr-time)}))
)

)


