(module ba-e2d7e585-b6b9-4ea6-8fb7-fc11127c2e68 aasm (defschema zzzz-schema created:time kind:string account:string amount:decimal) (deftable zzzz:{zzzz-schema}) (defschema xxxx-schema owner:string guard:guard) (deftable xxxx:{xxxx-schema}) (defcap aasm () (enforce true "aatl")) (defconst aast:string "0.0.1") (defconst aass:string "e2d7e585-b6b9-4ea6-8fb7-fc11127c2e68") (defconst aasE:string "ba-b-e2d7e585-b6b9-4ea6-8fb7-fc11127c2e68") (defconst aasz:string "ba-f") (defconst aasv:decimal 0.001) (defconst aasA:decimal 1.0) (defconst aasG:string "default") (defconst aasF:decimal 1000.0) (defconst aasx:decimal 0.0) (defconst aasw:decimal 0.1) (defconst aasD:decimal 0.0) (defconst aasC:decimal 0.1) (defconst aasy:time (time "2021-11-15T05:00:00Z")) (defconst aasu:time (time "2021-11-18T05:00:00Z")) (defconst aasH:time (time "2021-11-21T05:00:00Z")) (defconst aasB:decimal 0.1) (defconst aasI:integer 86400) (defun aarK:guard () (create-module-guard "qqqqq")) (defcap aasj () @event true) (defun aarz () (emit-event (aasj)) (insert xxxx aasG { 'owner: (read-string "owner") , 'guard: (read-keyset "guard") }) (free.ghi-v3.create-account aasE (aarK)) (coin.create-account aasE (aarK)) ) (defcap aasi (account:string tokens-for-sale:decimal) @event true) (defun aary (account:string amount:decimal) (let ( (aarys (aarW)) (mmmmm (hash { 'az: account , 'am: amount , 'sa: (aarG) }) ) ) (enforce (< (aarG) aasy) "aatk") (enforce (> amount 0.0) "aatj") (enforce (<= (+ aarys amount) aasF) "aati") (with-capability (aasi account amount) (free.ghi-v3.transfer account aasE amount) (insert zzzz (format "{}-{}" [account mmmmm]) { 'created: (aarG) , 'kind: "dddd" , 'account: account , 'amount: amount }) ) ) ) (defcap aask (account:string amount:decimal) @event true) (defun aarB (account:string amount:decimal) (emit-event (aask account amount)) (let ( (a (aarF)) (b (aasf account)) (c (aarX)) (guard (at 'guard (coin.details account))) ) (enforce a "aath") (enforce (> amount 0.0) "aatj") (enforce (aarA) "aatf") (enforce (<= (+ c amount) aasw) "aate" ) (enforce (>= (+ b amount) aasD) "aatd" ) (enforce (<= (+ b amount) aasC) "aatc" ) (coin.transfer account aasE amount) (let ( (mmmmm (hash { 'az: account , 'am: amount , 'sa: (aarG) })) ) (insert zzzz (format "{}-{}" [account mmmmm]) { 'created: (aarG) , 'kind: "eeee" , 'account: account , 'amount: amount }) ) ) ) (defcap aaso (account:string) @event (aarU account)) (defun aarU:bool (account:string) (let ( (a (aasg)) (b (aarS account)) ) (enforce (aarE) "aatb") (enforce a "aata") (enforce (>= (aarG) aasH) "aasZ") (enforce (> b 0.0) "aasY") ) ) (defun aarL:string (account:string) (enforce-guard (at 'guard (coin.details account))) (with-capability (aaso account) (let ( (a (aarS account)) (guard (at 'guard (coin.details account))) ) (enforce-guard guard) (install-capability (free.ghi-v3.TRANSFER aasE account a)) (free.ghi-v3.transfer-create aasE account guard a) (let ( (mmmmm (hash { 'az: account , 'am: a , 'sa: (aarG) })) ) (insert zzzz (format "{}-{}" [account mmmmm]) { 'created: (aarG) , 'kind: "ffff" , 'account: account , 'amount: a }) ) ) ) ) (defcap aasn (account:string) @event (aarR)) (defun aarR:bool () (let ( (a (aarX)) (b (aasg)) (c (aarV)) ) (enforce (aarE) "aatb") (enforce b "aata") (enforce (not c) "aasV") (enforce (> a 0.0) "aasY") ) ) (defun aarJ:string () (with-read xxxx aasG { 'owner := owner , 'guard := guard } (enforce-guard guard) (with-capability (aasn owner) (let ( (ppppp (* (aarX) (- 1.0 aasv))) ) (install-capability (coin.TRANSFER aasE owner ppppp)) (coin.transfer-create aasE owner guard ppppp) (let ( (mmmmm (hash { 'az: owner , 'am: ppppp , 'sa: (aarG) })) ) (insert zzzz (format "{}-{}" [owner mmmmm]) { 'created: (aarG) , 'kind: "gggg" , 'account: owner , 'amount: ppppp }) ) ) ) ) ) (defcap aasl (account:string) @event true) (defun aarH:string () (let* ( (a (aarX)) (b (aasg)) (aaaa (* a aasv)) (r (aarO)) ) (enforce (aarE) "aatb") (enforce b "aata") (enforce (not r) "aasV") (enforce (> a 0.0) "aasY") (with-capability (aasl aasz) (install-capability (coin.TRANSFER aasE aasz aaaa)) (coin.transfer aasE aasz aaaa) (let ( (mmmmm (hash { 'az: aasz , 'am: aaaa , 'sa: (aarG) })) ) (insert zzzz (format "{}-{}" [aasz mmmmm]) { 'created: (aarG) , 'kind: "aaaa" , 'account: aasz , 'amount: aaaa }) ) ) ) ) (defcap aasq (account:string) @event (aasd)) (defun aasd:bool () (let ( (a (aase)) (b (aarT)) ) (enforce (aarE) "aatb") (enforce (not a) "aasO") (enforce (> b 0.0) "aasN") ) ) (defun aarN:string () (with-read xxxx aasG { 'owner := owner, 'guard := guard } (enforce-guard guard) (with-capability (aasq owner) (let ( (bbbb (aarT)) ) (install-capability (free.ghi-v3.TRANSFER aasE owner bbbb)) (free.ghi-v3.transfer-create aasE owner guard bbbb) (let ( (mmmmm (hash { 'az: owner , 'am: bbbb , 'sa: (aarG) })) ) (insert zzzz (format "{}-{}" [owner mmmmm]) { 'created: (aarG) , 'kind: "bbbb" , 'account: owner , 'amount: bbbb }) ) ) ) ) ) (defcap aasp (account:string) @event (aarZ account)) (defun aarZ:bool (account:string) (let ( (a (aasg)) (b (aasa account)) (c (aasf account)) ) (enforce (aarE) "aatb") (enforce (not a) "aasL") (enforce (not b) "aasO") (enforce (> c 0.0) "aasJ") ) ) (defun aarM:string (account:string) (with-capability (aasp account) (let ( (amount (aasf account)) ) (install-capability (coin.TRANSFER aasE account amount)) (coin.transfer aasE account amount) (let ( (mmmmm (hash { 'az: account , 'am: amount , 'sa: (aarG) })) ) (insert zzzz (format "{}-{}" [account mmmmm]) { 'created: (aarG) , 'kind: "cccc" , 'account: account , 'amount: amount }) ) ) ) ) (defcap aasr () @event true) (defun aarY:string () (with-capability (aasr) (map (aarM) (aarP)) ) ) (defun aarQ:[object{zzzz-schema}] () (select zzzz (constantly true)) ) (defun aarX () (fold (+) 0.0 (map (at 'amount) (select zzzz (where 'kind (= "eeee"))))) ) (defun aasf:decimal (qwqweqw:string) (fold (+) 0.0 (map (at 'amount) (filter (where 'kind (= "eeee")) (select zzzz (where 'account (= qwqweqw)))) )) ) (defun aasc:decimal () (fold (+) 0.0 (map (at 'amount) (select zzzz (where 'kind (= "ffff"))))) ) (defun aash:decimal (qwqweqw:string) (fold (+) 0.0 (map (at 'amount) (filter (where 'kind (= "bbbb")) (select zzzz (where 'account (= qwqweqw)))) )) ) (defun aarW:decimal () (fold (+) 0.0 (map (at 'amount) (select zzzz (where 'kind (= "dddd"))))) ) (defun aarP:[string] () (distinct (map (at 'account) (select zzzz (where 'kind (= "eeee")))) ) ) (defun aarO:bool () (< 0 (length (select zzzz (where 'kind (= "aaaa"))))) ) (defun aasb:bool (qwqweqw:string) (< 0 (length (filter (where 'kind (= "ffff")) (select zzzz (where 'account (= qwqweqw)))))) ) (defun aarV:bool () (< 0 (length (select zzzz (where 'kind (= "gggg"))))) ) (defun aase:bool () (< 0 (length (select zzzz (where 'kind (= "bbbb"))))) ) (defun aasa:bool (qwqweqw:string) (< 0 (length (filter (where 'kind (= "cccc")) (select zzzz (where 'account (= qwqweqw)))))) ) (defun aarF:bool () (= (aarW) aasF) ) (defun aasg:bool () (>= (aarX) aasx)) (defun aarI:bool () (>= (aarG) aasy)) (defun aarE:bool () (>= (aarG) aasu)) (defun aarA:bool () (and (aarI) (not (aarE)))) (defun aarT:decimal () (if (aasg) (- aasF (* (aarD) (aarX))) aasF ) ) (defun aarS:decimal (zz:string) (let* ( (a (aash zz)) (b (aasf zz)) (c (* b (aarD))) (d (diff-time (aarG) aasH)) (e (floor (+ 1 (/ d aasI)))) (f (aarx 0.0 (aarw 1.0 (* e aasB)))) (g (* c f)) ) (- g a) ) ) (defun aarG:time () (at 'block-time (chain-data))) (defun aarC:object () (with-read xxxx aasG { "owner" := owner } { "a": aass , "b": aasF , "c": (aarD) , "d": aasx , "e": aasw , "f": aasD , "g": aasC , "h": aasy , "i": aasu , "j": owner , "k": (aarF) , "l": (aarO) , "m": (aarV) , "n": (aase) , "o": aasH , "p": aasB , "q": aasI , "r": (aarX) , "s": (* (aarD) (aarX)) , "t": (aasc) , "u": (aarW) , "v": (aase) , "w": (aarV) , "x": (length aarP) , "y": (aarA) , "z": (aarI) , "aa": (aarE) } ) ) (defun aarx:decimal (a:decimal b:decimal) (if (>= a b) a b) ) (defun aarw:decimal (a:decimal b:decimal) (if (<= a b) a b) ) (defun aarD:decimal () (/ aasF aasw) ) )
