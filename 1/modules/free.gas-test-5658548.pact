(module gas-test GOVERNANCE

    (defcap GOVERNANCE ()
        (enforce-guard (keyset-ref-guard "free.hyper-admin-local-test-staging")))

 (defschema project-schema
    id:string
    nfts:[integer]
 )

 (deftable projects:{project-schema})



 (defun create-project (name:string)
    (insert projects name {"id": name, "nfts":[]})   
 )

    (defun get-project-nfts (name:string)
        (take 10 (at "nfts" (read projects name)))
    )


    (defun allocate-random-nfts (name:string nfts:[integer])
        (update projects name {"nfts": nfts})
    )


    (defun random-list-generator (seed:integer size:integer)
        (let* (
            (empty_arr (enumerate 0 size))
            (randomizer (lambda (seed)  (let* ((seed2 (* 11795372955171141389 (+ seed 6971258582664805397)))(m1 (* 1946526487930394057 (xor (shift seed2 -16) seed2)))) (& (shift (+ (* 214013 (xor (shift m1 -64) m1)) 2531011) -64) 327670))))
            (random (fold (lambda (result curr) (+ result [(randomizer (fold (*) 1 (take -1 result)))])) [(randomizer (str-to-int 64 (hash seed)))] empty_arr))) 
            (map (at "i") (sort ["rand"] (map (lambda (i) {"i": i, "rand": (at i random)}) empty_arr)))
        )
    )

    ;  (defun get-distribution (seed:integer size:integer iterations:integer)

    ;      (let* (
    ;          (results (map 
    ;              (lambda (iteration) (let* ((try (random-list-generator (* seed iteration) size)))
    ;                    (map (lambda (index) {"i": index, "res": (at index try)})  (enumerate 0 size))
    ;            )) (enumerate 1 iterations)))
    ;          )
    ;          (map (lambda (index) 
    ;              (map (lambda (bigarray) (at "res" (at index bigarray))) results)
    ;          ) (enumerate 0 size))
    ;      )
        

        
    ;  )
)


