(module kitty-kad-kitties-helper-functions 'kitty-kad
  "Kitty Kad Kitties NFTs game helper functions"
  (defconst GENE_BITS 30)
  (defconst MUTATION_ODDS 5) ; 1 in X chance for the gene to mutate

  ;;; Breeding Functions ;;;
  (defun create-new-kitty-data (parent-1-data:object parent-2-data:object id-for-new-kitty:string owner:string guard:guard)
        @doc "Makes data for a new kitty based on parent data"
        ; Make sure kitty can be bred
        (enforce (ready-to-breed (at "next-breed-time" parent-1-data)) "Parent 1 should be ready to breed")
        (enforce (ready-to-breed (at "next-breed-time" parent-2-data)) "Parent 2 should be ready to breed")
        ; Combine genes for new kitty
        (let (
                (generation (max-int (at "generation" parent-1-data) (at "generation" parent-2-data)))
                (genes (make-new-genes (at "genes" parent-1-data) (at "genes" parent-2-data)))
                (items (make-new-genes (at "items" parent-1-data) (at "items" parent-2-data)))
            )
            ; Return new kitty data to be used by another contract
            (identity {
                "id": id-for-new-kitty,
                "parent-1-id": (at "id" parent-1-data), 
                "parent-2-id": (at "id" parent-2-data), 
                "generation": generation,
                "birthday": (at "block-time" (chain-data)),
                "next-breed-time": (next-breed-time genes generation),
                "genes": genes, 
                "items": items,
                "owner": owner,
                "guard": guard
            })
        )
    )

    ;;; Breeding validation functions

    (defun ready-to-breed (next-breed-time:time)
        @doc "True if a cat is read to breed"
        (<= next-breed-time (at "block-time" (chain-data)))
    )

    (defun next-breed-time (genes:list generation:integer)
        @doc "Returns next breed time for a kitty, to be completed"
        (add-time (at "block-time" (chain-data)) (days 7))
    )

    ;;; Making new genes functions

    (defun make-new-genes (parent-1-genes:list parent-2-genes:list)
        @doc "Given list of gene/item pairs, return new ones"
        (let ( 
            (parent-1-pass-on (genes-to-pass-on parent-1-genes))
            (parent-2-pass-on (genes-to-pass-on parent-2-genes))
            ) 
            (randomly-combine-genes parent-1-pass-on parent-2-pass-on)
        )
    )

    (defun genes-to-pass-on (genes:list)
        @doc "Given a list of gene/item pairs, choose one from each pair to pass on"
        (map (choose-one-from-int-pair-and-mutate) genes)
    )

    (defun choose-one-from-int-pair-and-mutate (pair:list)
        @doc "Given an int pair, choose one and mutate it ranodmly"
        (mutate-gene (choose-one-from-int-pair pair))
    )

    (defun mutate-gene (gene:integer)
        @doc "Returns what a randomly mutated gene looks like"
        (
            let (
                (bits-to-shift ( random-number-small-range gene (* GENE_BITS MUTATION_ODDS) ))
                )
                (if (
                    >
                    ;  Use the gene as the seed for a random number
                    bits-to-shift
                    (- GENE_BITS 1)
                )
                gene
                (xor gene (shift 1 bits-to-shift))
            )       
        )
    )

    (defun random-number-small-range(seed:string range:integer)
        @doc "Returns a small number within a range"
        (
            mod (random-number-small seed) range
        )
    )

    (defun random-number-small (seed:integer)
        @doc "Gets a small-ish random number based on the seed"
        ; Use a random string, convert a character from it to an integer
        (str-to-int 64 (base64-encode (take 1 (random-string seed))) )
    )

    (defun random-string (seed:integer)
        (hash 
            ; Add the seed to a string of the current time and amount of kitties
            (concat [
                (int-to-str 10 seed) 
                (hash (at "block-time" (chain-data))) 
                ;;;; TO DO --- FUNCTION TO GET KITTIES COUNT
                ;  (int-to-str 10 (get-count KITTIES_CREATED_COUNT_KEY)) 
            ]) 
        )
    )


    (defun randomly-combine-genes (genes-1:list genes-2:list)
        @doc "Randomly combine the genes and return the combined ones"
        (enforce (= (length genes-1) (length genes-2)) "Must have equal amount of genes to combine")
        (map 
            (randomly-combine-genes-at-index genes-1 genes-2)
            (enumerate 0 (- (length genes-1) 1) )
        )
    )

    (defun randomly-combine-genes-at-index(genes-1:list genes-2:list index:integer)
        @doc "Given two lists and an index, randomy combine the ones at the index"
        (choose-one-from-int-pair [(at index genes-1) (at index genes-2)])
    )

    (defun choose-one-from-int-pair (pair:list)
        @doc "Chooses one element from int pair randomly"
        ( at 
            ; Use the sum of the elements in the tuple as a seed
            ; Then we can use this as an index to select from the pair
            (random-bit (+ (at 0 pair) (at 1 pair)))
            pair
        )
    )

    (defun random-bit (seed:integer)
        @doc "Returns a pseudo random bit"
        (& (random-number-small seed) 1)
    )

    (defun max-int (int-1:integer int-2:integer)
        @doc "Returnins maximum of two ints"
        (if (> int-1 int-2) int-1 int-2)
    )
)

