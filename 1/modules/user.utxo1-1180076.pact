(module utxo1 'admin-k

  (defschema out
    q:decimal
    g:guard)

  (defconst GUARD (acct-guard))
  (defun acct-guard () (create-module-guard "acct"))

  (defun mk-acct (salt:integer)
    (take 64 (int-to-str 16 (str-to-int 64 (hash (+ salt (* salt salt)))))))

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

