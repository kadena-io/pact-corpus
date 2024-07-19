(module kitty-kad-kitties-helper-functions 'kitty-kad
  "Kitty Kad Kitties NFTs game helper functions"

  (defconst GENE_BITS 30)
  (defconst MUTATION_ODDS 5) ;Probability out of 100 of mutating
  (defconst RANDOM_SEED_STR_KEY "random_seed_str_key")

    (defschema random-seed-str-schema
        @doc "Basic schema used for storing a random seed str during execution"
        value:string
    )
    (deftable random-seed-str:{random-seed-str-schema})

  ;;; Breeding Functions ;;;
  (defun create-new-kitty-data (parent-1-data:object parent-2-data:object new-id:string owner:string)
        @doc "Makes data for a new kitty based on parent data"
        (write random-seed-str RANDOM_SEED_STR_KEY {"value": new-id})
        ; Make sure kitty can be bred
        (enforce (ready-to-breed (at "next-breed-time" parent-1-data)) "Parent 1 should be ready to breed")
        (enforce (ready-to-breed (at "next-breed-time" parent-2-data)) "Parent 2 should be ready to breed")
        ; Combine genes for new kitty
        (let (
                (gene-pairs (make-new-genes (at "gene-pairs" parent-1-data) (at "gene-pairs" parent-2-data) ))
                (item-pairs (make-new-genes (at "item-pairs" parent-1-data) (at "item-pairs" parent-2-data)))
            )
            ; Return new kitty data to be used by another contract
            (identity {
                "id": new-id,
                "parent-1-id": (at "id" parent-1-data), 
                "parent-2-id": (at "id" parent-2-data), 
                "generation": 1,
                "birthday": (at "block-time" (chain-data)),
                "next-breed-time": (next-breed-time gene-pairs 1),
                "gene-pairs": gene-pairs, 
                "item-pairs": item-pairs,
                "name": new-id,
                "owner": owner
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
        (let* 
            (
                (generation-offset (* 24 generation))
                (kitty-specific-offset ( random-number-small-range (at 0 (at 0 genes)) 24))
                (minimum-offset 1) ; At least 1 hour must pass between breeds
                (total-offset (+ (+ generation-offset kitty-specific-offset) minimum-offset))
            )
            (add-time (at "block-time" (chain-data)) (hours total-offset))
        )
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
        @doc "Depending on probabilities, mutates the gene"
        (if 
            ; Do not mutate the gene if odds aren't high enough
            ( < MUTATION_ODDS ( random-number-small-range gene 100))
            (* gene 1)
            (let* 
                (
                    ; Find a random bit to flip within the valid range
                    (bit-to-flip (random-number-small-range gene GENE_BITS))
                )
                ; Get the bit to flip by moving 1 by the index of the bit
                ; Then xor with the gene to flip the bit
                (xor gene (shift 1 bit-to-flip))
            )
        )
    )

    (defun random-number-small-range(seed:integer range:integer)
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
                (at "value" (read random-seed-str RANDOM_SEED_STR_KEY))
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
        (let* 
            (
                (gene-1 (at index genes-1))
                (gene-2 (at index genes-2))
                (pseudo-random-hash (hash [gene-1 gene-2 index]))
                (seed (str-to-int 64 pseudo-random-hash))
            )
            (if (= 0 (& seed 1))
                [gene-1 gene-2]
                [gene-2 gene-1]
            )
        )
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

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )
)
;  (create-table random-seed-str)
