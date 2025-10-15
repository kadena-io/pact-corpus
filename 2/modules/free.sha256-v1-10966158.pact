(module sha256-v1 GOV
  ; sha256 implementation for Pact.
  ;
  ; This module implements the full sha256 specifications.
  ; The public function (digest ) is supposed from 3rd party codes.
  ;   - This function accepts a list of padded '512 bits blocks' (according to FIPS.180-4 § 5.1.1)
  ;
  ; The function (pad-int) can be used to create a list of SHA256 blocks from an integer up to 959 bits

  (defcap GOV ()
    (enforce false "Not upgradable module"))

  (defschema register
    a:integer
    b:integer
    c:integer
    d:integer
    e:integer
    f:integer
    g:integer
    h:integer
  )

  ; Initial Hash values according to FIPS.180-4 § 5.3.2
  (defconst INITIAL-HASH {'a:(str-to-int 16 "6a09e667"),
                          'b:(str-to-int 16 "bb67ae85"),
                          'c:(str-to-int 16 "3c6ef372"),
                          'd:(str-to-int 16 "a54ff53a"),
                          'e:(str-to-int 16 "510e527f"),
                          'f:(str-to-int 16 "9b05688c"),
                          'g:(str-to-int 16 "1f83d9ab"),
                          'h:(str-to-int 16 "5be0cd19")})

  ; SHA-256 constants according to FIPS.180-4 § 4.2.2
  (defconst SHA256-K (map (str-to-int 16) ["428a2f98" "71374491" "b5c0fbcf" "e9b5dba5" "3956c25b" "59f111f1" "923f82a4" "ab1c5ed5"
                                           "d807aa98" "12835b01" "243185be" "550c7dc3" "72be5d74" "80deb1fe" "9bdc06a7" "c19bf174"
                                           "e49b69c1" "efbe4786" "0fc19dc6" "240ca1cc" "2de92c6f" "4a7484aa" "5cb0a9dc" "76f988da"
                                           "983e5152" "a831c66d" "b00327c8" "bf597fc7" "c6e00bf3" "d5a79147" "06ca6351" "14292967"
                                           "27b70a85" "2e1b2138" "4d2c6dfc" "53380d13" "650a7354" "766a0abb" "81c2c92e" "92722c85"
                                           "a2bfe8a1" "a81a664b" "c24b8b70" "c76c51a3" "d192e819" "d6990624" "f40e3585" "106aa070"
                                           "19a4c116" "1e376c08" "2748774c" "34b0bcb5" "391c0cb3" "4ed8aa4a" "5b9cca4f" "682e6ff3"
                                           "748f82ee" "78a5636f" "84c87814" "8cc70208" "90befffa" "a4506ceb" "bef9a3f7" "c67178f2"]))

  ; Some util functions
  (defconst BASE-32 (^ 2 32))
  (defconst MASK-32 (mask 32))

  (defun mask (nbits:integer)
    @doc "Compute a msk to select N lowest biys"
    (- (shift 1 nbits) 1))

  (defun mod+:integer (x:integer y:integer)
    @doc "Sum 2 integers modulo 2^32"
    (mod (+ x y) BASE-32))

  (defun sum-4:integer (x:integer y:integer z:integer zz:integer)
    @doc "Sum 4 integers"
    (+ (+ x y) (+ z zz)))

  (defun modsum-4:integer (x:integer y:integer z:integer zz:integer)
    @doc "Sum 4 integers modulo 2^32"
    (mod (+ (+ x y) (+ z zz)) BASE-32))

  (defun take-word:integer (x:integer position:integer)
    @doc "Take a 32 bits word at a given position inside an integer"
    (& MASK-32 (shift x position)))

  ; Rotation functions
  (defun rotr-2:integer (x:integer)
    (| (shift x -2) (shift x 30)))

  (defun rotr-6:integer (x:integer)
    (| (shift x -6) (shift x 26)))

  (defun rotr-7:integer (x:integer)
    (| (shift x -7) (shift x 25)))

  (defun rotr-11:integer (x:integer)
    (| (shift x -11) (shift x 21)))

  (defun rotr-13:integer (x:integer)
    (| (shift x -13) (shift x 19)))

  (defun rotr-17:integer (x:integer)
    (| (shift x -17) (shift x 15)))

  (defun rotr-18:integer (x:integer)
    (| (shift x -18) (shift x 14)))

  (defun rotr-19:integer (x:integer)
    (| (shift x -19) (shift x 13)))

  (defun rotr-22:integer (x:integer)
    (| (shift x -22) (shift x 10)))

  (defun rotr-25:integer (x:integer)
    (| (shift x -25) (shift x 7)))


  ; SHA-256 logical functions according to FIPS.180-4 § 4.1.2
  (defun Ch:integer (x:integer y:integer z:integer)
    (xor (& x y) (& (~ x) z)))

  (defun Maj:integer (x:integer y:integer z:integer)
    (xor (xor (& x y) (& x z)) (& y z)))

  (defun Sigma-0:integer (x:integer)
    (xor (xor (rotr-2 x) (rotr-13 x)) (rotr-22 x)))

  (defun Sigma-1:integer (x:integer)
    (xor (xor (rotr-6 x) (rotr-11 x)) (rotr-25 x)))

  (defun sigma-0:integer (x:integer)
    (xor (xor (rotr-7 x) (rotr-18 x)) (shift x -3)))

  (defun sigma-1:integer (x:integer)
    (xor (xor (rotr-17 x) (rotr-19 x)) (shift x -10)))

  (defun block-to-words:[integer] (x:integer)
    @doc "Compute Wi for a '512 bits block' according to FIPS.180-4 § 6.2.2 (step 1)"
    (let* (
      (w0  (take-word x -480))
      (w1  (take-word x -448))
      (w2  (take-word x -416))
      (w3  (take-word x -384))
      (w4  (take-word x -352))
      (w5  (take-word x -320))
      (w6  (take-word x -288))
      (w7  (take-word x -256))
      (w8  (take-word x -224))
      (w9  (take-word x -192))
      (w10 (take-word x -160))
      (w11 (take-word x -128))
      (w12 (take-word x  -96))
      (w13 (take-word x  -64))
      (w14 (take-word x  -32))
      (w15 (take-word x    0))
      (w16 (modsum-4 (sigma-1 w14) w9  (sigma-0 w1)  w0))
      (w17 (modsum-4 (sigma-1 w15) w10 (sigma-0 w2)  w1))
      (w18 (modsum-4 (sigma-1 w16) w11 (sigma-0 w3)  w2))
      (w19 (modsum-4 (sigma-1 w17) w12 (sigma-0 w4)  w3))
      (w20 (modsum-4 (sigma-1 w18) w13 (sigma-0 w5)  w4))
      (w21 (modsum-4 (sigma-1 w19) w14 (sigma-0 w6)  w5))
      (w22 (modsum-4 (sigma-1 w20) w15 (sigma-0 w7)  w6))
      (w23 (modsum-4 (sigma-1 w21) w16 (sigma-0 w8)  w7))
      (w24 (modsum-4 (sigma-1 w22) w17 (sigma-0 w9)  w8))
      (w25 (modsum-4 (sigma-1 w23) w18 (sigma-0 w10) w9))
      (w26 (modsum-4 (sigma-1 w24) w19 (sigma-0 w11) w10))
      (w27 (modsum-4 (sigma-1 w25) w20 (sigma-0 w12) w11))
      (w28 (modsum-4 (sigma-1 w26) w21 (sigma-0 w13) w12))
      (w29 (modsum-4 (sigma-1 w27) w22 (sigma-0 w14) w13))
      (w30 (modsum-4 (sigma-1 w28) w23 (sigma-0 w15) w14))
      (w31 (modsum-4 (sigma-1 w29) w24 (sigma-0 w16) w15))
      (w32 (modsum-4 (sigma-1 w30) w25 (sigma-0 w17) w16))
      (w33 (modsum-4 (sigma-1 w31) w26 (sigma-0 w18) w17))
      (w34 (modsum-4 (sigma-1 w32) w27 (sigma-0 w19) w18))
      (w35 (modsum-4 (sigma-1 w33) w28 (sigma-0 w20) w19))
      (w36 (modsum-4 (sigma-1 w34) w29 (sigma-0 w21) w20))
      (w37 (modsum-4 (sigma-1 w35) w30 (sigma-0 w22) w21))
      (w38 (modsum-4 (sigma-1 w36) w31 (sigma-0 w23) w22))
      (w39 (modsum-4 (sigma-1 w37) w32 (sigma-0 w24) w23))
      (w40 (modsum-4 (sigma-1 w38) w33 (sigma-0 w25) w24))
      (w41 (modsum-4 (sigma-1 w39) w34 (sigma-0 w26) w25))
      (w42 (modsum-4 (sigma-1 w40) w35 (sigma-0 w27) w26))
      (w43 (modsum-4 (sigma-1 w41) w36 (sigma-0 w28) w27))
      (w44 (modsum-4 (sigma-1 w42) w37 (sigma-0 w29) w28))
      (w45 (modsum-4 (sigma-1 w43) w38 (sigma-0 w30) w29))
      (w46 (modsum-4 (sigma-1 w44) w39 (sigma-0 w31) w30))
      (w47 (modsum-4 (sigma-1 w45) w40 (sigma-0 w32) w31))
      (w48 (modsum-4 (sigma-1 w46) w41 (sigma-0 w33) w32))
      (w49 (modsum-4 (sigma-1 w47) w42 (sigma-0 w34) w33))
      (w50 (modsum-4 (sigma-1 w48) w43 (sigma-0 w35) w34))
      (w51 (modsum-4 (sigma-1 w49) w44 (sigma-0 w36) w35))
      (w52 (modsum-4 (sigma-1 w50) w45 (sigma-0 w37) w36))
      (w53 (modsum-4 (sigma-1 w51) w46 (sigma-0 w38) w37))
      (w54 (modsum-4 (sigma-1 w52) w47 (sigma-0 w39) w38))
      (w55 (modsum-4 (sigma-1 w53) w48 (sigma-0 w40) w39))
      (w56 (modsum-4 (sigma-1 w54) w49 (sigma-0 w41) w40))
      (w57 (modsum-4 (sigma-1 w55) w50 (sigma-0 w42) w41))
      (w58 (modsum-4 (sigma-1 w56) w51 (sigma-0 w43) w42))
      (w59 (modsum-4 (sigma-1 w57) w52 (sigma-0 w44) w43))
      (w60 (modsum-4 (sigma-1 w58) w53 (sigma-0 w45) w44))
      (w61 (modsum-4 (sigma-1 w59) w54 (sigma-0 w46) w45))
      (w62 (modsum-4 (sigma-1 w60) w55 (sigma-0 w47) w46))
      (w63 (modsum-4 (sigma-1 w61) w56 (sigma-0 w48) w47))

    )
    [w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15
     w16 w17 w18 w19 w20 w21 w22 w23 w24 w25 w26 w27 w28 w29 w30 w31
     w32 w33 w34 w35 w36 w37 w38 w39 w40 w41 w42 w43 w44 w45 w46 w47
     w48 w49 w50 w51 w52 w53 w54 w55 w56 w57 w58 w59 w60 w61 w62 w63]
  ))

  (defun do-round:object{register} (register-in:object{register} input:integer)
    @doc "Run a SHA256 round according to FIPS.180-4 § 6.2.2 (step 3)"
    (bind register-in {'a:=a, 'b:=b, 'c:=c, 'd:=d, 'e:=e, 'f:=f, 'g:=g, 'h:=h}
      (let ((t1 (sum-4 h (Sigma-1 e) (Ch e f g) input))
            (t2 (+ (Sigma-0 a) (Maj a b c))))
        {'h:g, 'g:f, 'f:e, 'e:(mod+ d t1), 'd:c, 'c:b, 'b:a, 'a:(mod+ t1 t2)}))
  )

  (defun sum-reg:object{register} (x:object{register} y:object{register})
    @doc "Sum 2 SHA256 resgisters according to FIPS.180-4 § 6.2.2 (Step 4)"
    (bind x {'a:=xa, 'b:=xb, 'c:=xc, 'd:=xd, 'e:=xe, 'f:=xf, 'g:=xg, 'h:=xh}
      (bind y {'a:=ya, 'b:=yb, 'c:=yc, 'd:=yd, 'e:=ye, 'f:=yf, 'g:=yg, 'h:=yh}
        {'a:(mod+ xa ya), 'b:(mod+ xb yb), 'c:(mod+ xc yc), 'd:(mod+ xd yd),
         'e:(mod+ xe ye), 'f:(mod+ xf yf), 'g:(mod+ xg yg), 'h:(mod+ xh yh)}))
  )

  (defun do-block:object{register} (register-in:object{register} input:integer)
    @doc "Compute a single 512 bits blocks according to FIPS.180-4 § 6.2.2"
    (sum-reg register-in
             (fold (do-round) register-in (zip (+) SHA256-K (block-to-words input))))
  )

  (defun reg-to-int:integer (x:object{register})
    @doc "Last step to compute 256 bits Digest according to FIPS.180-4 § 6.2.2 (Postambule)"
    (bind x {'a:=xa, 'b:=xb, 'c:=xc, 'd:=xd, 'e:=xe, 'f:=xf, 'g:=xg, 'h:=xh}
      (fold (|) 0 (zip (shift) [xh xg xf xe xd xc xb xa]
                               (enumerate 0 256 32))))
  )

  (defun digest:integer (input:[integer])
    @doc "Compute the hash from a list of '512 bits blocks' according to FIPS.180-4 § 6.2.2"
    (reg-to-int (fold (do-block) INITIAL-HASH input))
  )

  ; Padding functions
  (defun pack-left(nbits:integer x:integer)
    @doc "Pack an integer by shifting it to the left and append a tail bit"
    (shift (| (shift x 1) 1) (- nbits 1)))

  (defun pad-int:[integer] (nbits:integer msg:integer)
    @doc "Pad an integer up to 959 bits according to FIPS.180-4 § 5.1.1"
    (cond
      ; Case 1: Fit in a single block
      ((< nbits 448) [(| (pack-left (- 512 nbits) msg) nbits)])
      ; Case 2: The msg itself + tail bit fit in a single-block but not the length
      ((< nbits 512) [(pack-left (- 512 nbits) msg) nbits])
      ; Case 3: The msg + tail bit + length fit in 2 blocks
      ((< nbits 960) [(shift msg (- 512 nbits)) (| (pack-left (- 1024 nbits) (& (mask (- nbits 512)) msg)) nbits)])
      ; Others cases: Not supported
      []
    )
  )

  (defun digest-btc-header:integer (input:string)
    @doc "Wrapper function to digest a BTC header (Hex string)"
    (digest (pad-int 256 (digest (pad-int 640 (str-to-int 16 input)))))
  )

)

