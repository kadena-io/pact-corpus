(module keys-3-pred GOVERNANCE
    (defcap GOVERNANCE() (enforce-guard (keyset-ref-guard 'pred-guard)))
    
    (defun keys-3 :bool(count:integer matched:integer)
        (enforce (= count 5) "Only 5 keys can be added")
        (>= matched (+ (/ count 2) 1)))    
)

