(module backalley-dev-eb17c4ad-d684-441f-b039-f9b9e7ec8e9d aasl (defschema xxxxx-schema created:time kind:string account:string amount:decimal) (deftable xxxxx:{xxxxx-schema}) (defcap aasl () (enforce false "aatl")) (defconst aast:string "Fixed") (defconst aasw:string "0.0.1") (defconst aasv:string "eb17c4ad-d684-441f-b039-f9b9e7ec8e9d") (defconst aasG:string "ba-b-eb17c4ad-d684-441f-b039-f9b9e7ec8e9d") (defconst aasB:string "ba-f") (defconst aass:decimal 0.0) (defconst aasu:string "k:35d235e21bedc3b93ec0f5060543d6be69d4970a8d0c048b576f522ebf22f0fd") (defconst aasC:decimal 0.500000000000) (defconst aasz:decimal 0.000500000000) (defconst aasy:decimal 0.001000000000) (defconst aasF:decimal 0.000000010000) (defconst aasE:decimal 0.001000000000) (defconst aasA:time (time "2021-12-10T18:35:00Z")) (defconst aasx:time (time "2021-12-10T19:35:00Z")) (defconst aasH:time (time "2021-12-10T19:40:00Z")) (defconst aasD:decimal 0.100000000000) (defconst aasI:integer 86400) (defun aarI:guard () (create-module-guard "ppppp")) (defcap aasj () @event true) (defun aarw () (emit-event (aasj)) (free.ghi-v3.create-account aasG (aarI)) (coin.create-account aasG (aarI)) ) (defcap aasi (account:string tokens-for-sale:decimal) @event true) (defun aarv (account:string amount:decimal) (let ( (aarvs (aarU)) ) (enforce (< (aarE) aasA) "aatk") (enforce (> amount 0.0) "aatj") (enforce (<= (+ aarvs amount) aasC) "aati") (with-capability (aasi account amount) (free.ghi-v3.transfer account aasG amount) (insert xxxxx (aarx account amount) { 'created: (aarE) , 'kind: "gggg" , 'account: account , 'amount: amount }) ) ) ) (defcap aask (account:string amount:decimal) @event true) (defun aarz (account:string amount:decimal) (emit-event (aask account amount)) (let ( (aaaa (aarD)) (bbbb (aasf account)) (cccc (aarW)) ) (enforce aaaa "aath") (enforce (> amount 0.0) "aatj") (enforce (aary) "aatf") (enforce (<= (+ cccc amount) aasy) "aate" ) (enforce (>= (+ bbbb amount) aasF) "aatd" ) (enforce (<= (+ bbbb amount) aasE) "aatc" ) (coin.transfer account aasG amount) (insert xxxxx (aarx account amount) { 'created: (aarE) , 'kind: "ffff" , 'account: account , 'amount: amount }) ) ) (defcap aaso (from-account:string to-account:string to-g:guard) @event (aarS from-account)) (defun aarS:bool (account:string) (let ( (aaaa (aasg)) (bbbb (aarQ account)) ) (enforce (aarC) "aatb") (enforce aaaa "aata") (enforce (>= (aarE) aasH) "aasZ") (enforce (> bbbb 0.0) "aasY") ) ) (defun aarJ:string (from-account:string to-account:string to-g:guard) (enforce-guard (at 'guard (coin.details from-account))) (with-capability (aaso from-account to-account to-g) (let ( (aaaa (aarQ from-account)) ) (install-capability (free.ghi-v3.TRANSFER aasG to-account aaaa)) (free.ghi-v3.transfer-create aasG to-account to-g aaaa) (insert xxxxx (aarx from-account aaaa) { 'created: (aarE) , 'kind: "aaaa" , 'account: from-account , 'amount: aaaa }) ) ) ) (defcap aasn (from-account:string to-account:string to-g:guard) @event (aarP)) (defun aarP:bool () (let ( (aaaa (aarW)) (bbbb (aasg)) (cccc (aarT)) ) (enforce (aarC) "aatb") (enforce bbbb "aata") (enforce (not cccc) "aasV") (enforce (> aaaa 0.0) "aasY") ) ) (defun aarH:string (to-account:string to-g:guard) (let ( (guard (at 'guard (free.ghi-v3.details aasu))) ) (enforce-guard guard) (with-capability (aasn aasu to-account to-g) (let ( (aaaa (* (aarW) (- 1.0 aass))) ) (install-capability (coin.TRANSFER aasG to-account aaaa)) (coin.transfer-create aasG to-account to-g aaaa) (insert xxxxx (aarx aasu aaaa) { 'created: (aarE) , 'kind: "bbbb" , 'account: aasu , 'amount: aaaa }) ) ) ) ) (defcap aasm (account:string) @event true) (defun aarF:string () (let* ( (aaaa (aarW)) (bbbb (aasg)) (cccc (* aaaa aass)) (dddd (aarM)) ) (enforce (aarC) "aatb") (enforce bbbb "aata") (enforce (not dddd) "aasV") (enforce (> aaaa 0.0) "aasY") (with-capability (aasm aasB) (install-capability (coin.TRANSFER aasG aasB cccc)) (coin.transfer aasG aasB cccc) (insert xxxxx (aarx aasB cccc) { 'created: (aarE) , 'kind: "cccc" , 'account: aasB , 'amount: cccc }) ) ) ) (defcap aasq (account:string) @event (aasc)) (defun aasc:bool () (let ( (aaaa (aase)) (bbbb (aarR)) ) (enforce (aarC) "aatb") (enforce (not aaaa) "aasO") (enforce (> bbbb 0.0) "aasN") ) ) (defun aarL:string () (let ( (guard (at 'guard (free.ghi-v3.details aasu))) ) (enforce-guard guard) (with-capability (aasq aasu) (let ( (aaaa (aarR)) ) (install-capability (free.ghi-v3.TRANSFER aasG aasu aaaa)) (free.ghi-v3.transfer-create aasG aasu guard aaaa) (insert xxxxx (aarx aasu aaaa) { 'created: (aarE) , 'kind: "dddd" , 'account: aasu , 'amount: aaaa }) ) ) ) ) (defcap aasp (account:string) @event (aarY account)) (defun aarY:bool (account:string) (let ( (aaaa (aasg)) (bbbb (aarZ account)) (cccc (aasf account)) ) (enforce (aarC) "aatb") (enforce (not aaaa) "aasL") (enforce (not bbbb) "aasO") (enforce (> cccc 0.0) "aasJ") ) ) (defun aarK:string (account:string) (with-capability (aasp account) (let ( (amount (aasf account)) ) (install-capability (coin.TRANSFER aasG account amount)) (coin.transfer aasG account amount) (insert xxxxx (aarx account amount) { 'created: (aarE) , 'kind: "eeee" , 'account: account , 'amount: amount }) ) ) ) (defcap aasr () @event true) (defun aarX:string () (with-capability (aasr) (map (aarK) (aarN)) ) ) (defun aarO:[object{xxxxx-schema}] () (select xxxxx (constantly true)) ) (defun aarW () (fold (+) 0.0 (map (at 'amount) (select xxxxx (where 'kind (= "ffff"))))) ) (defun aasf:decimal (account:string) (fold (+) 0.0 (map (at 'amount) (filter (where 'kind (= "ffff")) (select xxxxx (where 'account (= account)))) )) ) (defun aarV:decimal () (fold (+) 0.0 (map (at 'amount) (select xxxxx (where 'kind (= "bbbb"))))) ) (defun aasb:decimal () (fold (+) 0.0 (map (at 'amount) (select xxxxx (where 'kind (= "aaaa"))))) ) (defun aash:decimal (account:string) (fold (+) 0.0 (map (at 'amount) (filter (where 'kind (= "aaaa")) (select xxxxx (where 'account (= account)))) )) ) (defun aarU:decimal () (fold (+) 0.0 (map (at 'amount) (select xxxxx (where 'kind (= "gggg"))))) ) (defun aarN:[string] () (distinct (map (at 'account) (select xxxxx (where 'kind (= "ffff")))) ) ) (defun aarM:bool () (< 0 (length (select xxxxx (where 'kind (= "cccc"))))) ) (defun aasa:bool (account:string) (< 0 (length (filter (where 'kind (= "aaaa")) (select xxxxx (where 'account (= account)))))) ) (defun aarT:bool () (< 0 (length (select xxxxx (where 'kind (= "bbbb"))))) ) (defun aase:bool () (< 0 (length (select xxxxx (where 'kind (= "dddd"))))) ) (defun aarZ:bool (account:string) (< 0 (length (filter (where 'kind (= "eeee")) (select xxxxx (where 'account (= account)))))) ) (defun aasd:decimal () (fold (+) 0.0 (map (at 'amount) (select xxxxx (where 'kind (= "eeee"))))) ) (defun aarD:bool () (= (aarU) aasC) ) (defun aasg:bool () (>= (aarW) aasz)) (defun aarG:bool () (>= (aarE) aasA)) (defun aarC:bool () (>= (aarE) aasx)) (defun aary:bool () (and (aarG) (not (aarC)))) (defun aarR:decimal () (if (aasg) (- aasC (* (aarB) (aarW))) aasC ) ) (defun aarQ:decimal (account:string) (let* ( (aaaa (aash account)) (bbbb (aasf account)) (cccc (* bbbb (aarB))) (dddd (diff-time (aarE) aasH)) (ffff (floor (+ 1 (/ dddd aasI)))) (gggg (aaru 0.0 (aart 1.0 (* ffff aasD)))) (hhhh (* cccc gggg)) ) (- hhhh aaaa) ) ) (defun aarB:decimal () (/ aasC aasy) ) (defun aarE:time () (at 'block-time (chain-data))) (defun aaru:decimal (a:decimal b:decimal) (if (>= a b) a b) ) (defun aart:decimal (a:decimal b:decimal) (if (<= a b) a b) ) (defun aarx:string (account:string amount:decimal) (let ((h (hash { 'account: account, 'amount: amount, 'salt: (aarE) }))) (format "{}-{}" [account h]) ) ) (defun aarA:object () { "a1": aast , "a2": aasw , "a3": aasv , "a4": aasG , "a5": aass , "b1": aasu , "b2": aasC , "b3": aasz , "b4": aasy , "b5": aasF , "b6": aasE , "b7": aasA , "b8": aasx , "b9": aasH , "b10": aasD , "b11": aasI , "c1": (aarW) , "c2": (aarV) , "c3": (aasb) , "c4": (aarU) , "c5": (length (aarN)) , "c6": (aarM) , "c7": (aarT) , "c8": (aase) , "c9": (aasd) , "c10": (aarD) , "c11": (aasg) , "c12": (aarG) , "c13": (aarC) , "c14": (aary) , "c15": (aarR) , "c16": (aarB) , "c17": (* (aarB) (aarW)) } ) ) 
