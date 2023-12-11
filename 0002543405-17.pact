(module rsa GOV
  @doc "Module to verify a RSA signature: \
      \  Supported paddings: Only pkcs#1 v1.5 \
      \  Supported hash algorithms: Only Blake2b \
      \  Supported key lengths: 1024, 2048, 3072, 4096 \
      \  Supported exponents: Any : eg 3,5,17,257, 65537, ...."

  (defconst VERSION:string "0.1")

  (defcap GOV()
    (enforce-keyset "free.rsa-lib"))

  ; OID = 1.3.6.1.4.1.1722.12.2.1.32
  ; Note:
  ;  - According to the RFC 7693 OID should be 1.3.6.1.4.1.1722.12.2.1.8
  ;  - But according to the RFC Draft OID should be 1.3.6.1.4.1.1722.12.2.1.32
  ;
  ; PyCryptoDome uses the second one. => For now, stay compatible with cryptodome
  (defconst BLAKE2B-OID:string "060b2b060104018d3a0c020120")

  ; Padding according to RFC 8017 ( PKCS #1)
  (defconst DIGEST-INFO:string (concat [ "3033" ;Sequence of size 51
                                            "300f" ;Digest Info: Sequence of size 15
                                              BLAKE2B-OID ;Size 13
                                              "0500" ; Null
                                            "0420" ; (+ Hash) String of size 32
                                        ]))


  (defun --make-padding:integer (key-size:integer)
    "Private function to pre-generate at deployment time the PKCS#1-v1.5 signature padding"
    (let* ((key-size-nibble (/ key-size 4))
           (inner-length (fold (-) key-size-nibble [6 (length DIGEST-INFO) 64]))
           (inner (concat (make-list inner-length "f")))
           (header (concat ["0001" inner "00" DIGEST-INFO])))
      (hex-to-big-int header))
  )


  (defconst PADDINGS  { "1024":(--make-padding 1024),
                        "2048":(--make-padding 2048),
                        "3072":(--make-padding 3072),
                        "4096":(--make-padding 4096)})

  ; (str-to-int) is limited to 512 characters. For very big integers, we have to
  ; cut the strings into two parts to parse the integer; We have exactly the same issue
  ; with base64 encoded ints and hex encoded ints
  (defun hex-to-big-int:integer (x:string)
    "Convert an hex string to a big integer. Can accept string length up to 1024 chars"
    (if (> (length x) 512)
      (| ( shift (str-to-int 16 (drop -512 x)) 2048) (str-to-int 16 (take -512 x)))
      (str-to-int 16 x))
  )

  (defun b64-to-big-int:integer (x:string)
    "Convert an base64 string to a big integer. Can accept string length up to 1024 chars"
    (if (> (length x) 512)
        (let* ((x-len (length x))
               (left-shift (* 6 (- x-len 512)))
               (b64-offset (at (mod x-len 4) [0 0 4 2 ])))
          (| (shift (str-to-int 64 (take 512 x)) (- left-shift b64-offset)) (str-to-int 64 (drop 512 x))))
      (str-to-int 64 x))
  )

  (defun str-to-big-int:integer (x:string)
    "Convert a string (hex or base64) to a big integer"
    (if (= (take 2 x) "0x")
        (hex-to-big-int (drop 2 x))
        (b64-to-big-int x)))

  (defun encode-pkcs1-v15 (hash-b64:string key-size:integer)
    (let ((padding (at (int-to-str 10 key-size) PADDINGS)))
      (| (shift padding 256) (str-to-int 64 hash-b64)))
  )

  (defun rsa-decrypt (pub-key:integer pub-exponent:integer c-msg:integer)
    ;(mod (^ c-msg pub-exponent) pub-key)) ; => This is the naive implementation. That's works but takes millions of gas because of high memory usage.

    ; Here is a smarter approach: based on https://en.wikipedia.org/wiki/Modular_exponentiation (Right to left binary method)
    ; pub exponent in transformed to it's binary representation, and one bit is processed at each iteration.
    ; fold transmits between  each iteration a list with 2 elements: [result, base] => Please refer to the algorithm on Wikipedia.
    (at 0 (fold (lambda (x ex) (let* ((result (at 0 x))
                                      (base (at 1 x))
                                      (new-base (mod (* base base) pub-key)))
                                  (if (= "1" ex)
                                      [(mod (* result base) pub-key), new-base]
                                      [result, new-base])))
                [1, (mod c-msg pub-key)]
                (reverse (str-to-list (int-to-str 2 pub-exponent)))))
 )

  (defun verify-pkcs1-v15 (pub-key:string  key-size:integer pub-exponent:string signature:string msg)
    @doc "Main function to verify a signature. Returns true or false \
          \  - pub-key is the RSA N. Can be encoded in Base64URL or in hexa (must start with 0x) \
          \  - keys-size can be 512, 1024, 2048 or 3072 \
          \  - pub-exponent is RSA e. Can be encoded in Base64URL or in hexa (must start with 0x) \
          \  - signature is the signature to verify.  Can be encoded in Base64URL or in hexa (must start with 0x) \
          \  - msg is the original message. Can be a string or any Pact type. But before signature, the sender should \
          \      have encoded the message following the 'Pact rules':  https://github.com/kadena-io/pact/blob/master/src/Pact/Types/Codec.hs "
    (enforce (contains key-size [1024, 2048, 3072, 4096]) "Key size is not supported")
    (let ((encoded-hash (encode-pkcs1-v15 (hash msg) key-size))
          (rec-hash (rsa-decrypt (str-to-big-int pub-key)
                                 (str-to-big-int pub-exponent)
                                 (str-to-big-int signature))))
      (= encoded-hash rec-hash))
  )
)

