(module donate GOVERNANCE

(defcap GOVERNANCE ()
(enforce-guard (keyset-ref-guard "n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.dgov")))

(defcap DONATION-TRACKER (account:string f:string amount:decimal)
    @doc "Event to track donations"
    @event
    true)

(defcap DONATE () true)
(defconst DONATE-GUARD (create-capability-guard (DONATE)))
(defconst DONATE-ACCOUNT (create-principal DONATE-GUARD))

(defcap SEND-TX ()
(compose-capability (GOVERNANCE))
(compose-capability (DONATE)))

; Constants

(defconst COIN:string "coin")
(defconst HERON:string "heron")

; Tables

(defschema donation-schema
    amount:decimal)

(deftable donation-table:{donation-schema}) 

; Functions

(defun track-donations (account:string fun:string amount:decimal)
    @doc "Tracks donations"
    (enforce (> amount 0.0) "Invalid amount")
    (enforce (or (= fun COIN) (= fun HERON)) "Invalid token")
    (with-default-read donation-table fun {'amount:0.0} {'amount:=amt}
        (write donation-table fun {'amount: (+ amt amount)}))
    (if (= fun COIN)
    (coin.transfer-create account DONATE-ACCOUNT DONATE-GUARD amount)
    (heron.transfer-create account DONATE-ACCOUNT DONATE-GUARD amount))
    (emit-event (DONATION-TRACKER account fun amount)))

(defun get-donations:decimal (f:string)
    @doc "Returns the total donations"
    (read donation-table f))
 
(defun get-balance:decimal (f:module{fungible-v2})
    @doc "Returns the balance of the donation account per given token"
    (f::get-balance DONATE-ACCOUNT))

(defun send-tx (receiver:string f:module{fungible-v2} amount:decimal)
    @doc "Sends a transaction to the receiver"
    (with-capability (SEND-TX)
    (install-capability (f::TRANSFER DONATE-ACCOUNT receiver amount))
    (f::transfer-create DONATE-ACCOUNT receiver amount))
    (format "Sent {} {} to {}" [amount f receiver]))

(defun init ()
    (coin.create-account DONATE-ACCOUNT DONATE-GUARD)
    (n_e309f0fa7cf3a13f93a8da5325cdad32790d2070.heron.create-account DONATE-ACCOUNT DONATE-GUARD)
    (insert donation-table COIN {'amount:0.0})
    (insert donation-table HERON {'amount:0.0})
    )

)


