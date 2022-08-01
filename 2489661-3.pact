(module backalley-dev-45eb3723-d586-40c3-af24-bd6c00dec93a aasi (defschema transactions-schema created:time kind:string account:string amount:decimal) (deftable transactions:{transactions-schema}) (defcap aasi () (enforce false "aatl")) (defconst aasp:string "Fixed") (defconst aass:string "0.0.1") (defconst aasr:string "45eb3723-d586-40c3-af24-bd6c00dec93a") (defconst aasC:string "ba-b-45eb3723-d586-40c3-af24-bd6c00dec93a") (defconst aasx:string "ba-f") (defconst aaso:decimal 0.0) (defconst aasq:string "k:2ad0abae3e25f1d73b68599cb003c6f3873f8c46bdd641be51bf32eafe4abffc") (defconst aasy:decimal 1.000000000000) (defconst aasv:decimal 0.000100000000) (defconst aasu:decimal 0.001000000000) (defconst aasB:decimal 0.000000100000) (defconst aasA:decimal 0.001000000000) (defconst aasw:time (time "2021-12-22T20:55:00Z")) (defconst aast:time (time "2021-12-22T21:05:00Z")) (defconst aasD:time (time "2021-12-22T21:05:00Z")) (defconst aasz:decimal 0.005000000000) (defconst aasE:integer 15724800) (defun aarF:guard () (create-module-guard "qqqqqq")) (defcap aasg () @event true) (defun aart () (emit-event (aasg)) (free.ghi-v3.create-account aasC (aarF)) (coin.create-account aasC (aarF)) ) (defcap aasf (account:string tokens-for-sale:decimal) @event true) (defun aars (account:string amount:decimal) (let ( (aarss (aarQ)) ) (enforce (< (aarB) aasw) "aatk") (enforce (> amount 0.0) "aatj") (enforce (<= (+ aarss amount) aasy) "aati") (with-capability (aasf account amount) (free.ghi-v3.transfer account aasC amount) (insert transactions (aaru account amount) { 'created: (aarB) , 'kind: "gggg" , 'account: account , 'amount: amount }) ) ) ) (defcap aash (account:string amount:decimal) @event true) (defun aarw (account:string amount:decimal) (emit-event (aash account amount)) (let ( (aarsed (aarA)) (aarw-amount (aasc account)) (total-aarw-amount (aarS)) ) (enforce aarsed "aath") (enforce (> amount 0.0) "aatj") (enforce (aarv) "aatf") (enforce (<= (+ total-aarw-amount amount) aasu) "aate" ) (enforce (>= (+ aarw-amount amount) aasB) "aatd" ) (enforce (<= (+ aarw-amount amount) aasA) "aatc" ) (coin.transfer account aasC amount) (insert transactions (aaru account amount) { 'created: (aarB) , 'kind: "ffff" , 'account: account , 'amount: amount }) ) ) (defcap aasl (from:string to:string amount:decimal) @managed (aarO from amount)) (defun aarO:bool (account:string amount:decimal) (let ( (achieved-aarq-raise (aasd)) (redeemable-amount (aasb account)) ) (enforce (aarz) "aatb") (enforce achieved-aarq-raise "aata") (enforce (>= (aarB) aasD) "aasZ") (enforce (> redeemable-amount 0.0) "aasY") (enforce (> amount 0.0) "aatj") (enforce (<= amount redeemable-amount) "aasW") ) ) (defun aarG:string (from-acct:string to-acct:string amount:decimal) (with-capability (aasl from-acct to-acct amount) (let ( (redeemable-amount (aasb from-acct)) (to-guard (at 'guard (coin.details from-acct))) ) (enforce-keyset to-guard) (install-capability (free.ghi-v3.TRANSFER aasC to-acct amount)) (free.ghi-v3.transfer-create aasC to-acct to-guard amount) (insert transactions (aaru from-acct amount) { 'created: (aarB) , 'kind: "aaaa" , 'account: from-acct , 'amount: amount }) (format "Redeemed {} tokens" [amount]) ) ) ) (defcap aask (from:string to:string amount:decimal) @managed (aarM amount)) (defun aarM:bool (amount:decimal) (let ( (balance (aarS)) (achieved-aarq-raise (aasd)) (aarM (aarY)) (redeemed-aarss (aarP)) ) (enforce (aarz) "aatb") (enforce achieved-aarq-raise "aata") (enforce (> balance 0.0) "aasY") (enforce (> aarM 0.0) "aasS") (enforce (> amount 0.0) "aatj") (enforce (<= amount aarM) "aasW") ) ) (defun aarE:string (to-acct:string amount:decimal) (let ( (to-guard (at 'guard (coin.details aasq))) ) (with-capability (aask aasq to-acct amount) (enforce-guard to-guard) (let ( (payout (* amount (- 1.0 aaso))) ) (install-capability (coin.TRANSFER aasC to-acct payout)) (coin.transfer-create aasC to-acct to-guard payout) (insert transactions (aaru aasq payout) { 'created: (aarB) , 'kind: "bbbb" , 'account: aasq , 'amount: payout }) (format "Redeemed {} tokens" [payout]) ) ) ) ) (defcap aasj (account:string) @event true) (defun aarC:string () (let* ( (aarw-amount (aarS)) (achieved-aarq-raise (aasd)) (fee (* aarw-amount aaso)) (redeemed (aarJ)) ) (enforce (aarz) "aatb") (enforce achieved-aarq-raise "aata") (enforce (not redeemed) "aasN") (enforce (> fee 0.0) "aasY") (with-capability (aasj aasx) (install-capability (coin.TRANSFER aasC aasx fee)) (coin.transfer aasC aasx fee) (insert transactions (aaru aasx fee) { 'created: (aarB) , 'kind: "cccc" , 'account: aasx , 'amount: fee }) (format "Redeemed {} tokens" [fee]) ) ) ) (defcap aasn (account:string) @event (aarX)) (defun aarX:bool () (let ( (withdrawn-tokens (aasa)) (unsold-tokens (aarN)) ) (enforce (aarz) "aatb") (enforce (not withdrawn-tokens) "aasK") (enforce (> unsold-tokens 0.0) "aasJ") ) ) (defun aarI:string () (with-capability (aasn aasq) (let ( (unsold-tokens (aarN)) ) (install-capability (free.ghi-v3.TRANSFER aasC aasq unsold-tokens)) (free.ghi-v3.transfer aasC aasq unsold-tokens) (insert transactions (aaru aasq unsold-tokens) { 'created: (aarB) , 'kind: "dddd" , 'account: aasq , 'amount: unsold-tokens }) (format "Withdrew {} tokens" [unsold-tokens]) ) ) ) (defcap aasm (account:string) @event (aarT account)) (defun aarT:bool (account:string) (let ( (achieved-aarq-raise (aasd)) (withdrawn (aarU account)) (aarw-amount (aasc account)) ) (enforce (aarz) "aatb") (enforce (not achieved-aarq-raise) "aasH") (enforce (not withdrawn) "aasK") (enforce (> aarw-amount 0.0) "aasF") ) ) (defun aarH:string (account:string) (with-capability (aasm account) (let ( (amount (aasc account)) ) (install-capability (coin.TRANSFER aasC account amount)) (coin.transfer aasC account amount) (insert transactions (aaru account amount) { 'created: (aarB) , 'kind: "eeee" , 'account: account , 'amount: amount }) (format "Withdrew {} tokens" [amount]) ) ) ) (defun aarL:[object{transactions-schema}] () (select transactions (constantly true)) ) (defun aarS () (fold (+) 0.0 (map (at 'amount) (select transactions (where 'kind (= "ffff"))))) ) (defun aasc:decimal (account:string) (fold (+) 0.0 (map (at 'amount) (filter (where 'kind (= "ffff")) (select transactions (where 'account (= account)))) )) ) (defun aarR:decimal () (fold (+) 0.0 (map (at 'amount) (select transactions (where 'kind (= "bbbb"))))) ) (defun aarW:decimal () (fold (+) 0.0 (map (at 'amount) (select transactions (where 'kind (= "aaaa"))))) ) (defun aase:decimal (account:string) (fold (+) 0.0 (map (at 'amount) (filter (where 'kind (= "aaaa")) (select transactions (where 'account (= account)))) )) ) (defun aarQ:decimal () (fold (+) 0.0 (map (at 'amount) (select transactions (where 'kind (= "gggg"))))) ) (defun aarK:[string] () (distinct (map (at 'account) (select transactions (where 'kind (= "ffff")))) ) ) (defun aarJ:bool () (< 0 (length (select transactions (where 'kind (= "cccc"))))) ) (defun aarV:bool (account:string) (< 0 (length (filter (where 'kind (= "aaaa")) (select transactions (where 'account (= account)))))) ) (defun aarP:bool () (< 0 (length (select transactions (where 'kind (= "bbbb"))))) ) (defun aasa:bool () (< 0 (length (select transactions (where 'kind (= "dddd"))))) ) (defun aarU:bool (account:string) (< 0 (length (filter (where 'kind (= "eeee")) (select transactions (where 'account (= account)))))) ) (defun aarZ:decimal () (fold (+) 0.0 (map (at 'amount) (select transactions (where 'kind (= "eeee"))))) ) (defun aarY:decimal () (- (aarS) (aarR)) ) (defun aarA:bool () (= (aarQ) aasy) ) (defun aasd:bool () (>= (aarS) aasv)) (defun aarD:bool () (>= (aarB) aasw)) (defun aarz:bool () (>= (aarB) aast)) (defun aarv:bool () (and (aarD) (not (aarz)))) (defun aarN:decimal () (if (aasd) (- aasy (* (aary) (aarS))) (- aasy (aarQ)) ) ) (defun dadada:bool () (>= (aarB) aasD)) (defun aasb:decimal (account:string) (let* ( (aaaa (aase account)) (bbbb (aasc account)) (cccc (* bbbb (aary))) (dddd (diff-time (aarB) aasD)) (ffff (floor (+ 1 (/ dddd aasE)))) (gggg (aarr 0.0 (aarq 1.0 (* ffff aasz)))) (hhhh (* cccc gggg)) ) (- hhhh aaaa) ) ) (defun aary:decimal () (/ aasy aasu) ) (defun aarB:time () (at 'block-time (chain-data))) (defun aarr:decimal (a:decimal b:decimal) (if (>= a b) a b) ) (defun aarq:decimal (a:decimal b:decimal) (if (<= a b) a b) ) (defun aaru:string (account:string amount:decimal) (let ((h (hash { 'account: account, 'amount: amount, 'salt: (aarB) }))) (format "{}-{}" [account h]) ) ) (defun aarx:object () { "a1": aasp , "a2": aass , "a3": aasr , "a4": aasC , "a5": aaso , "b1": aasq , "b2": aasy , "b3": aasv , "b4": aasu , "b5": aasB , "b6": aasA , "b7": aasw , "b8": aast , "b9": aasD , "b10": aasz , "b11": aasE , "c1": (aarS) , "c2": (aarR) , "c3": (aarW) , "c4": (aarQ) , "c5": (length (aarK)) , "c6": (aarJ) , "c7": (aarP) , "c8": (aasa) , "c9": (aarZ) , "c10": (aarA) , "c11": (aasd) , "c12": (aarD) , "c13": (aarz) , "c14": (dadada), "c15": (aarv) , "c16": (aarN) , "c17": (aary) , "c18": (* (aary) (aarS)) } ) (defun opopo (fffff:string) { "aaaaa": (aasb fffff), "bbbbb": (aarU fffff), "ccccc": (aarV fffff), "ddddd": (aase fffff), "eeeee": (aasc fffff)}) ) 
