(module backalley-dev-046b13b4-6d3c-4f43-af7d-12f6cd30d395 aash (defschema ppppp-schema created:time kind:string account:string amount:decimal) (deftable xxxxx:{ppppp-schema}) (defcap aash () (enforce false "aatl")) (defconst aasp:string "Fixed") (defconst aass:string "0.0.1") (defconst aasr:string "046b13b4-6d3c-4f43-af7d-12f6cd30d395") (defconst aasC:string "ba-b-046b13b4-6d3c-4f43-af7d-12f6cd30d395") (defconst aasx:string "ba-f") (defconst aaso:decimal 0.0) (defconst aasq:string "k:2ad0abae3e25f1d73b68599cb003c6f3873f8c46bdd641be51bf32eafe4abffc") (defconst aasy:decimal 1.000000000000) (defconst aasv:decimal 0.000100000000) (defconst aasu:decimal 0.001000000000) (defconst aasB:decimal 0.000000100000) (defconst aasA:decimal 0.001000000000) (defconst aasw:time (time "2021-12-14T00:00:00Z")) (defconst aast:time (time "2021-12-15T00:00:00Z")) (defconst aasD:time (time "2021-12-15T00:00:00Z")) (defconst aasz:decimal 0.050000000000) (defconst aasE:integer 86400) (defun aarD:guard () (create-module-guard "qqqqq")) (defcap aasf () @event true) (defun aarr () (emit-event (aasf)) (free.ghi-v3.create-account aasC (aarD)) (coin.create-account aasC (aarD)) ) (defcap aase (account:string tokens-for-sale:decimal) @event true) (defun aarq (account:string amount:decimal) (let ( (aaaaa (aarO)) ) (enforce (< (aarz) aasw) "aatk") (enforce (> amount 0.0) "aatj") (enforce (<= (+ aaaaa amount) aasy) "aati") (with-capability (aase account amount) (free.ghi-v3.transfer account aasC amount) (insert xxxxx (aars account amount) { 'created: (aarz) , 'kind: "gggg" , 'account: account , 'amount: amount }) ) ) ) (defcap aasg (account:string amount:decimal) @event true) (defun aaru (account:string amount:decimal) (emit-event (aasg account amount)) (let ( (aaaa (aary)) (bbbb (aasb account)) (cccc (aarQ)) ) (enforce aaaa "aath") (enforce (> amount 0.0) "aatj") (enforce (aart) "aatf") (enforce (<= (+ cccc amount) aasu) "aate" ) (enforce (>= (+ bbbb amount) aasB) "aatd" ) (enforce (<= (+ bbbb amount) aasA) "aatc" ) (coin.transfer account aasC amount) (insert xxxxx (aars account amount) { 'created: (aarz) , 'kind: "ffff" , 'account: account , 'amount: amount }) ) ) (defcap aask (from:string to:string amount:decimal) @event (aarM from amount)) (defun aarM:bool (account:string amount:decimal) (let ( (aaaa (aasc)) (bbbb (aasa account)) ) (enforce (aarx) "aatb") (enforce aaaa "aata") (enforce (>= (aarz) aasD) "aasZ") (enforce (> bbbb 0.0) "aasY") (enforce (> amount 0.0) "aatj") (enforce (<= amount bbbb) "aasW") ) ) (defun aarE:string (from:string to:string amount:decimal) (with-capability (aask from to amount) (let ( (aaaa (aasa from)) (to-g (at 'guard (coin.details from))) ) (enforce-guard to-g) (install-capability (free.ghi-v3.TRANSFER aasC to amount)) (free.ghi-v3.transfer-create aasC to to-g amount) (insert xxxxx (aars from amount) { 'created: (aarz) , 'kind: "aaaa" , 'account: from , 'amount: amount }) ) ) ) (defcap aasj (from:string to:string to-g:guard amount:decimal) @event (aarK amount)) (defun aarK:bool (amount:decimal) (let ( (aaaa (aarQ)) (bbbb (aasc)) (cccc (aarX)) (dddd (aarN)) ) (enforce (aarx) "aatb") (enforce bbbb "aata") (enforce (> aaaa 0.0) "aasY") (enforce (> cccc 0.0) "aasS") (enforce (> amount 0.0) "aatj") (enforce (<= amount cccc) "aasW") ) ) (defun aarC:string (to-account:string amount:decimal) (let ( (guard (at 'guard (free.ghi-v3.details aasq))) ) (enforce-guard guard) (with-capability (aasj aasq to-account guard amount) (let ((aarp-payout (* (aarX) (- 1.0 aaso)))) (install-capability (coin.TRANSFER aasC to-account amount)) (coin.transfer-create aasC to-account guard amount) (insert xxxxx (aars aasq amount) { 'created: (aarz) , 'kind: "bbbb" , 'account: aasq , 'amount: amount }) ) ) ) ) (defcap aasi (account:string) @event true) (defun aarA:string () (let* ( (aaaa (aarQ)) (bbbb (aasc)) (dddd (* aaaa aaso)) (cccc (aarH)) ) (enforce (aarx) "aatb") (enforce bbbb "aata") (enforce (not cccc) "aasN") (enforce (> aaaa 0.0) "aasY") (with-capability (aasi aasx) (install-capability (coin.TRANSFER aasC aasx dddd)) (coin.transfer aasC aasx dddd) (insert xxxxx (aars aasx dddd) { 'created: (aarz) , 'kind: "cccc" , 'account: aasx , 'amount: dddd }) ) ) ) (defcap aasm (account:string) @event (aarW)) (defun aarW:bool () (let ( (aaaa (aarZ)) (bbbb (aarL)) ) (enforce (aarx) "aatb") (enforce (not aaaa) "aasK") (enforce (> bbbb 0.0) "aasJ") ) ) (defun aarG:string () (let ( (guard (at 'guard (free.ghi-v3.details aasq))) ) (enforce-guard guard) (with-capability (aasm aasq) (let ( (aaaa (aarL)) ) (install-capability (free.ghi-v3.TRANSFER aasC aasq aaaa)) (free.ghi-v3.transfer-create aasC aasq guard aaaa) (insert xxxxx (aars aasq aaaa) { 'created: (aarz) , 'kind: "dddd" , 'account: aasq , 'amount: aaaa }) ) ) ) ) (defcap aasl (account:string) @event (aarS account)) (defun aarS:bool (account:string) (let ( (cccc (aasc)) (bbbb (aarT account)) (aaaa (aasb account)) ) (enforce (aarx) "aatb") (enforce (not cccc) "aasH") (enforce (not bbbb) "aasK") (enforce (> aaaa 0.0) "aasF") ) ) (defun aarF:string (account:string) (with-capability (aasl account) (let ( (amount (aasb account)) ) (install-capability (coin.TRANSFER aasC account amount)) (coin.transfer aasC account amount) (insert xxxxx (aars account amount) { 'created: (aarz) , 'kind: "eeee" , 'account: account , 'amount: amount }) ) ) ) (defcap aasn () @event true) (defun aarR:string () (with-capability (aasn) (map (aarF) (aarI)) ) ) (defun aarJ:[object{ppppp-schema}] () (select xxxxx (constantly true)) ) (defun aarQ () (fold (+) 0.0 (map (at 'amount) (select xxxxx (where 'kind (= "ffff"))))) ) (defun aasb:decimal (account:string) (fold (+) 0.0 (map (at 'amount) (filter (where 'kind (= "ffff")) (select xxxxx (where 'account (= account)))) )) ) (defun aarP:decimal () (fold (+) 0.0 (map (at 'amount) (select xxxxx (where 'kind (= "bbbb"))))) ) (defun aarV:decimal () (fold (+) 0.0 (map (at 'amount) (select xxxxx (where 'kind (= "aaaa"))))) ) (defun aasd:decimal (account:string) (fold (+) 0.0 (map (at 'amount) (filter (where 'kind (= "aaaa")) (select xxxxx (where 'account (= account)))) )) ) (defun aarO:decimal () (fold (+) 0.0 (map (at 'amount) (select xxxxx (where 'kind (= "gggg"))))) ) (defun aarI:[string] () (distinct (map (at 'account) (select xxxxx (where 'kind (= "ffff")))) ) ) (defun aarH:bool () (< 0 (length (select xxxxx (where 'kind (= "cccc"))))) ) (defun aarU:bool (account:string) (< 0 (length (filter (where 'kind (= "aaaa")) (select xxxxx (where 'account (= account)))))) ) (defun aarN:bool () (< 0 (length (select xxxxx (where 'kind (= "bbbb"))))) ) (defun aarZ:bool () (< 0 (length (select xxxxx (where 'kind (= "dddd"))))) ) (defun aarT:bool (account:string) (< 0 (length (filter (where 'kind (= "eeee")) (select xxxxx (where 'account (= account)))))) ) (defun aarY:decimal () (fold (+) 0.0 (map (at 'amount) (select xxxxx (where 'kind (= "eeee"))))) ) (defun aarX:decimal () (- (aarQ) (aarP)) ) (defun aary:bool () (= (aarO) aasy) ) (defun aasc:bool () (>= (aarQ) aasv)) (defun aarB:bool () (>= (aarz) aasw)) (defun aarx:bool () (>= (aarz) aast)) (defun aart:bool () (and (aarB) (not (aarx)))) (defun aarL:decimal () (if (aasc) (- aasy (* (aarw) (aarQ))) aasy ) ) (defun aasa:decimal (account:string) (let* ( (gggg (aasd account)) (ffff (aasb account)) (eeee (* ffff (aarw))) (dddd (diff-time (aarz) aasD)) (cccc (floor (+ 1 (/ dddd aasE)))) (bbbb (aarp 0.0 (aaro 1.0 (* cccc aasz)))) (aaa (* eeee bbbb)) ) (- aaa gggg) ) ) (defun aarw:decimal () (/ aasy aasu) ) (defun aarz:time () (at 'block-time (chain-data))) (defun aarp:decimal (a:decimal b:decimal) (if (>= a b) a b) ) (defun aaro:decimal (a:decimal b:decimal) (if (<= a b) a b) ) (defun aars:string (account:string amount:decimal) (let ((h (hash { 'account: account, 'amount: amount, 'salt: (aarz) }))) (format "{}-{}" [account h]) ) ) (defun aarv:object () { "a1": aasp , "a2": aass , "a3": aasr , "a4": aasC , "a5": aaso , "b1": aasq , "b2": aasy , "b3": aasv , "b4": aasu , "b5": aasB , "b6": aasA , "b7": aasw , "b8": aast , "b9": aasD , "b10": aasz , "b11": aasE , "c1": (aarQ) , "c2": (aarP) , "c3": (aarV) , "c4": (aarO) , "c5": (length (aarI)) , "c6": (aarH) , "c7": (aarN) , "c8": (aarZ) , "c9": (aarY) , "c10": (aary) , "c11": (aasc) , "c12": (aarB) , "c13": (aarx) , "c14": (aart) , "c15": (aarL) , "c16": (aarw) , "c17": (* (aarw) (aarQ)) } ) ) 
