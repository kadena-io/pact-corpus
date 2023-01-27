(module utxo1 'admin-k

  (defschema out
    q:decimal
    g:guard)

  (defconst GUARD (acct-guard))
  (defun acct-guard () (create-module-guard "acct"))

  (defun mk-acct (salt:integer)
    (take 64 (int-to-str 16 (+ salt (str-to-int 64 (tx-hash))))))

  (defun tx
    ( salt:integer
      token:module{fungible-v2}
      ins:[string]
      outs:[object{out}]
    )
    (let*
      ( (acct (mk-acct salt))
        (total-in
          (fold (+) 0.0 (map (drain token acct) ins)))
        (r (fold (fund token acct)
            { 'total: 0
            , 'accts: []
            , 'salt: (+ salt 1)
            }
            outs))
        (total-out (at 'total r))
      )
      (enforce (= total-in total-out)
        (format "error, {} != {}" [total-in, total-out]))
      (at 'accts r))
  )

  (defun fund (token:module{fungible-v2} acct:string state:object out:object{out})
    (bind out { 'g:=g, 'q:=q }
      (bind state { 'total:=total, 'salt:=salt, 'accts:=accts }
        (let ((a (mk-acct salt)))
          (tfr-out token acct a g q)
          {'total: (+ total q), 'salt: (+ salt 1), 'accts: (+ accts [a])}
        )))
  )

  (defun drain:decimal
    ( token:module{fungible-v2}
      acct:string
      in:string
    )
    (let ((b (token::get-balance in)))
      (token::transfer-create in acct GUARD b)
      b))

  (defun tfr-out (token:module{fungible-v2} f:string t:string g:guard a:decimal)
    (install-capability (token::TRANSFER f t a))
    (token::transfer-create f t g a))
)

;
;   (defun tx1
;     ( token:module{fungible-v2}
;       state:object
;       out:object{out}
;     )
;     (let*
;       ( (salt (at 'salt state))
;         (a (take 64 (int-to-str 16 (+ salt (str-to-int 64 (tx-hash))))))
;         (ins (at 'ins state))
;         (r (fold
;           (tx2 token a (at 'g out))
;           { 'ins: (drop 1 ins)
;           , 'outs: (at 'outs state)
;           , 'q: (at 'q out)
;           }
;           ins))
;       )
;       { 'ins: (at 'ins r)
;       , 'outs: (drop 1 (at 'outs r))
;       , 'q: (at 'q r)
;       , 'salt: (+ 1 salt)
;       }))
;
;   (defun tx2
;     ( token:module{fungible-v2}
;       a:string
;       g:guard
;       state:object
;       in:string
;     )
;     (let*
;       ( (q (at 'q state))
;         (b (token::get-balance in))
;       )
;       (if (>= b q) ;; more in than out
;         (let ((r (- b q))) ;; remainder in
;           (token::transfer-create in a g q) ;; tfr q, leaving r
;           { 'q: r
;           , 'ins: (at 'ins state) ;; remain at current in
;           , 'outs: (at 'outs state) ;;
;           }
;         )
;         (let ((r (- q b)))
;           (token::transfer-create in a g b)
;         )
;       )
;     )
;   )
; )
  ;
  ; (defschema utxo
  ;   g:guard
  ;   m:module{fungible-v1})
  ;
  ; (deftable utxos:{utxo})
  ;
  ; (defun read-all () (keys utxos))
  ; (defun get-balance (a:string)
  ;   (with-read utxos a { 'm: m }
  ;     (m::get-balance a)))
  ;
  ; (defcap HOLD (a:string m:module{fungible-v2} q:decimal)
  ;   @managed
  ;   true)
  ;
  ; (defun hold
  ;   ( salt:integer
  ;     m:module{fungible-v1}
  ;     from:string
  ;     q:decimal
  ;     g:guard
  ;   )
  ;   (let
  ;     ((a (take 64 (int-to-str 16 (+ salt (str-to-int 64 (tx-hash)))))))
  ;     (insert utxos a {'g: g, 'm: m})
  ;     (m::transfer-create from a (create-module-guard a))
  ;     a))
  ;
  ; (defun release
  ;   ( as:[string]
  ;     m:module{fungible-v1}
  ;     q:decimal
  ;     to:string
  ;     g:guard
  ;   )
  ;   (fold (release1 m to g) q as))
  ;
  ; (defun str= (a b)
  ;   (= (format "{}" [a]) (format "{}" [b])))
  ;
  ; (defun release1
  ;   ( m:module{fungible-v1}
  ;     to:string
  ;     g:guard
  ;     q:decimal
  ;     a:string
  ;   )
  ;   (enforce (> q 0.0) (format "invalid qty {}" [q]))
  ;   (let ((b (m::get-balance a)))
  ;     (enforce (> b 0.0) (format "spent output {}" [a]))
  ;     (with-read utxos a { 'm: um, 'g: ug }
  ;       (enforce (str= um m)
  ;         (format "bad module for {}, {}, expected {}" [a,um,m]))
  ;       (enforce-guard ug)
  ;       (if (>= q b)
  ;         (let ((r (- q b)))
  ;           (install-capability (m::TRANSFER a to b))
  ;           (m::transfer-create a to g b)
  ;           r)
  ;         (let ((r (- b q)))
  ;           (install-capability (m::TRANSFER a to q))
  ;           (m::transfer-create a to g q)
  ;           (install-capability (m::TRANSFER a ))
  ;
  ;       (if (>= ))
  ;           (r (- b q))
  ;
  ;     )
  ;     (let* ((b (coin.get-balance a))
  ;          (r (- b q)))
  ;   (with-read
  ;       es a { 'q := aq, 'g := ag }
  ;       (enforce-guard ag)

