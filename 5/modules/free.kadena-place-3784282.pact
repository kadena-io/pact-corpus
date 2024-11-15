(module kadena-place GOV
  
  ; ----------------------
  ; capabilities

  (defcap GOV ()
    "module governance"
    (enforce-keyset "free.kadena-place-admin")
  )

  (defcap POOL-GUARD ()
    "pool guard capability"
    true
  )

  (defcap INTERNAL (phrase:string)
    "internal capability"
    true
  )

  ; ----------------------
  ; schemas and tables

  (defschema color-schema
    color:string
  )

  (deftable color-table:{color-schema})

  (defschema place-schema
    price:decimal
  )

  (deftable place-table:{place-schema})
  
  (defschema pixel-schema
    price:decimal
    owner:string
    last-claim-place-price:decimal
  )

  (deftable pixel-table:{pixel-schema})

  (defschema account-schema
    joined:time
    min-tip:decimal
    tip-count:integer
    lifetime-pixels-bought:integer
    e2ee-messaging-account:string
  )

  (deftable account-table:{account-schema})

  (defschema tip-schema
    time:time
    message-id:string
    from:string
    to:string
    amount:decimal
  )

  (deftable tip-table:{tip-schema})

  ; ----------------------
  ; constants

  (defconst NUM-OF-PIXELS:integer 1000000)

  (defconst INIT-PRICE:decimal 0.01)

  (defconst ADMIN:string "k:414b6583c0899eccedb14a50d0e0152d0be13f6b2d1ddd080ea1d6393f2115ce")

  (defconst POOL:string "kadena-place-rewards-pool")

  ; ----------------------
  ; functions

  (defun enforce-pool-guard ()
    "enforce authorized pool access"
    (require-capability (POOL-GUARD))
  )

  (defun initialize ()
    "init module"
    (coin.create-account POOL (create-user-guard (enforce-pool-guard)))
    (insert place-table "data" {"price":(* INIT-PRICE NUM-OF-PIXELS)})
  )

  (defun validate-color:bool (color:string)
    "hexcolors #000000 to #ffffff"
    (enforce (= (length color) 7) "Invalid color length")
    (enforce (= (at 0 (str-to-list color)) "#") "Color must start with #")
    (enforce (= (length (filter (= "#") (str-to-list color))) 1) "Invalid color too many #s")
    (enforce (fold (and) true (map (lambda (x) (contains x ["#","0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"])) (str-to-list color))) "Invalid color characters")
    true
  )

  (defun validate-pixel-id:bool (pixel-id:integer)
    "pixel-ids only 0 to 999999"
    (enforce (<= pixel-id (- NUM-OF-PIXELS 1)) "Invalid pixel-id")
    (enforce (>= pixel-id 0) "Invalid pixel-id")
    true
  )

  (defun create-account (account:string public-key:string private-key-hash:string xor-private-key-password-hash:string)
    "create a kadena place account"
    (insert account-table account {"joined":(at "block-time" (chain-data)),"min-tip":0.0,"tip-count":0,"lifetime-pixels-bought":0,"e2ee-messaging-account":(free.e2ee-messaging.account-id account private-key-hash)})
    (free.e2ee-messaging.create-account account public-key private-key-hash xor-private-key-password-hash (at "guard" (coin.details account)))
  )

  (defun buy-many (account:string pixels:list)
    "buy many pixels in one tx"
    (update account-table account {"lifetime-pixels-bought":(+ (length pixels) (at "lifetime-pixels-bought" (read account-table account)))})
    (let*
      (
        (debts:list
          (map
            (lambda
              (pixel)
              (let
                (
                  (pixel-id-str:string (at "pixel-id-str" pixel))
                  (color:string (at "color" pixel))
                  (request-price-hundredths-str:string (at "request-price-hundredths-str" pixel))
                  (is-new-pixel:bool (at "is-new-pixel" pixel))
                  (place-price (at "price" (read place-table "data")))
                )
                (validate-color color)
                (validate-pixel-id (str-to-int 10 pixel-id-str))
                (with-capability (INTERNAL "buy")
                  (if is-new-pixel
                    (buy-new pixel-id-str account color place-price (/ (str-to-int 10 request-price-hundredths-str) 100.0))
                    (buy-used pixel-id-str account color place-price (/ (str-to-int 10 request-price-hundredths-str) 100.0))
                  )
                )
              )
            )
            pixels
          )
        )
        (user-debt:decimal
          (fold (+) 0
            (map
              (lambda
                (pixel-total-debt)
                (at "request-price" pixel-total-debt)
              )
              debts
            )
          )
        )
        (creator-allocation:decimal
          (fold (+) 0
            (map
              (lambda
                (pixel-total-debt)
                (at "creator-rewards" pixel-total-debt)
              )
              debts
            )
          )
        )
        (allocations:list
          (let
            (
              (owners:list
                (distinct
                  (+
                    (filter
                      (lambda
                        (address)
                        (not (= address ""))
                      )
                      (map
                        (lambda
                          (pixel-total-debt)
                          (at "pixel-owner" pixel-total-debt)
                        )
                        debts
                      )
                    )
                    [ADMIN]
                  )
                )
              )
            )
            (zip
              (lambda
                (address allocation)
                {"owner":address,"allocation":allocation}
              )
              owners
              (map
                (lambda
                  (address)
                  (fold
                    (+)
                    0
                    (map
                      (lambda
                        (pixel-total-debt)
                        (if (= (at "pixel-owner" pixel-total-debt) address)
                          (at "pixel-owner-rewards" pixel-total-debt)
                          0
                        )
                      )
                      (+ debts [{"pixel-owner":ADMIN,"pixel-owner-rewards":creator-allocation}])
                    )
                  )
                )
                owners
              )
            )
          )
        )
      )
      (coin.transfer account POOL user-debt)
      (with-capability (POOL-GUARD)
        (map
          (lambda
            (address-and-allocation)
            (let
              (
                (address (at "owner" address-and-allocation))
                (allocation (at "allocation" address-and-allocation))
              )
              (install-capability (coin.TRANSFER POOL address allocation))
              (coin.transfer POOL address allocation)
            )
          )
          allocations
        )
      )
    )
  )

  (defun buy-new:object (pixel-id-str:string account:string color:string place-price:decimal request-price:decimal)
    "buy pixel new INTERNAL"
    (require-capability (INTERNAL "buy"))
    (enforce (= request-price INIT-PRICE) "Requested price does not match pixel price")
    (let
      (
        (place-price-new (+ INIT-PRICE place-price))
        (pixel-price-new (* 2 INIT-PRICE))
      )
      (let
        (
          (creator-rewards (+ (* 0.26 INIT-PRICE) (/ (- place-price (* INIT-PRICE NUM-OF-PIXELS)) NUM-OF-PIXELS)))
        )
        (update place-table "data" {"price":place-price-new})
        (insert pixel-table pixel-id-str {"price":pixel-price-new,"owner":account,"last-claim-place-price":place-price})
        (insert color-table pixel-id-str {"color":color})
        {"creator-rewards":creator-rewards,"pixel-owner-rewards":0.0,"pixel-owner":"","request-price":request-price}
      )
    )
  )

  (defun buy-used:object (pixel-id-str:string account:string color:string place-price:decimal request-price:decimal)
    "buy pixel used INTERNAL"
    (require-capability (INTERNAL "buy"))
    (let*
      (
        (pixel (read pixel-table pixel-id-str))
        (pixel-price (at "price" pixel))
        (pixel-owner (at "owner" pixel))
        (pixel-last-claim-place-price (at "last-claim-place-price" pixel))
        (place-price-new (+ pixel-price place-price))
        (pixel-price-new (* 2 pixel-price))
      )
      (enforce (= request-price pixel-price) "Requested price does not match pixel price")
      (let
        (
          (creator-rewards (* 0.01 pixel-price))
          (pixel-owner-rewards (+ (* 0.25 pixel-price) (/ (- place-price pixel-last-claim-place-price) NUM-OF-PIXELS)))
        )
        (update place-table "data" {"price":place-price-new})
        (update pixel-table pixel-id-str {"owner":account,"last-claim-place-price":place-price,"price":pixel-price-new})
        (update color-table pixel-id-str {"color":color})
        {"creator-rewards":creator-rewards,"pixel-owner-rewards":pixel-owner-rewards,"pixel-owner":pixel-owner,"request-price":request-price}
      )
    )
  )

  (defun claim-rewards (account:string pixel-ids:list)
    "send pixel rewards to account"
    (enforce-guard (at "guard" (coin.details account)))
    (let*
      (
        (place-price (at "price" (read place-table "data")))
        (total-reward
          (fold
            (lambda
              (
                acc
                pixel-id-str
              )
              (let*
                (
                  (pixel (read pixel-table pixel-id-str))
                  (pixel-owner (at "owner" pixel))
                  (pixel-last-claim-place-price (at "last-claim-place-price" pixel))
                  (reward (/ (- place-price pixel-last-claim-place-price) NUM-OF-PIXELS))
                )
                (update pixel-table pixel-id-str {"last-claim-place-price":place-price})
                (enforce (= account pixel-owner) "You do not own pixel")
                (+ acc reward)
              )
            )
            0
            pixel-ids
          )
        )
      )
      (with-capability (POOL-GUARD)
        (install-capability (coin.TRANSFER POOL account total-reward))
        (coin.transfer POOL account total-reward)
      )
    )
  )

  (defun change-pixel-colors (pixels:list)
    "change your pixel's color"
    (map
      (lambda
        (pixel-pair)
        (let
          (
            (pixel-id-str:string (at "pixel-id-str" pixel-pair))
            (color:string (at "color" pixel-pair))
          )
          (validate-pixel-id (str-to-int 10 pixel-id-str))
          (validate-color color)
          (let*
            (
              (pixel (read pixel-table pixel-id-str))
              (pixel-owner (at "owner" pixel))
              (pixel-owner-guard (at "guard" (coin.details pixel-owner)))
            )
            (enforce-guard pixel-owner-guard)
            (update color-table pixel-id-str {"color":color})
          )
        )
      )
      pixels
    )
  )

  (defun change-min-tip (account:string min-tip:decimal)
    "change your min-tip value"
    (enforce-guard (at "guard" (coin.details account)))
    (update account-table account {"min-tip":min-tip})
  )

  (defun change-e2ee-messaging-account (account:string new-e2ee-messaging-account)
    "change tip messaging account"
    (enforce-guard (at "guard" (coin.details account)))
    (update account-table account {"e2ee-messaging-account":new-e2ee-messaging-account})
  )

  (defun give-tip (from-account:string to-account:string amount:decimal content)
    "tip an account"
    (coin.transfer from-account to-account amount)
    (let*
      (
        (from-account-row (read account-table from-account))
        (to-account-row (read account-table to-account))
        (from-e2ee-messaging-account (at "e2ee-messaging-account") from-account-row)
        (to-e2ee-messaging-account (at "e2ee-messaging-account") to-account-row)
        (tip-count (at "tip-count" to-account-row))
        (min-tip (at "min-tip" to-account-row))
        (tip-hash (tip-id to-account tip-count))
        (conversation-id (free.e2ee-messaging.conversation-id from-e2ee-messaging-account to-e2ee-messaging-account))
        (message-num (try 0 (at "messages-count" (at 0 (free.e2ee-messaging.get-conversations [conversation-id])))))
      )
      (enforce (>= amount min-tip) "Amount is less than account's min-tip")
      (insert tip-table tip-hash {"time":(at "block-time" (chain-data)),"message-id":(free.e2ee-messaging.message-id conversation-id message-num),"from":from-account,"to":to-account,"amount":amount})
      (update account-table to-account {"tip-count":(+ tip-count 1)})
      (free.e2ee-messaging.create-message from-e2ee-messaging-account to-e2ee-messaging-account content)
    )
  )

  (defun tip-id:string (to-account:string tip-num:integer)
    (hash (+ (hash to-account) (hash tip-num)))
  )

  (defun get-tips:object (tip-hashes:list)
    "LOCAL"
    (map
      (lambda
        (tip-hash)
        (read tip-table tip-hash)
      )
      tip-hashes
    )
  )

  (defun get-account:object (account:string)
    (read account-table account)
  )

  (defun get-pixels-data:[object] (pixel-id-strs:[string])
    "get pixel data"
    (map
      (lambda
        (pixel-id-str)
        (let*
          (
            (pixel (read pixel-table pixel-id-str))
          )
          (+ (+ {"price-hundredths-str":(int-to-str 10 (floor (* (at "price" pixel) 100)))} (drop ["price"] pixel)) (read color-table pixel-id-str))
        )
      )
      pixel-id-strs
    )
  )

  (defun get-all-account-pixel-ids:list (account:string)
    "get all pixel ids that belong to account LOCAL"
    (filter
      (lambda
        (pixel-id-str)
        (= account (at "owner" (read pixel-table pixel-id-str)))
      )
      (keys pixel-table)
    )
  )

  (defun get-all-accounts:object ()
    "get all accounts LOCAL"
    (fold
      (lambda
        (
          acc
          account
        )
        (+ acc [(+ {"account":account} (read account-table account))])
      )
      []
      (keys account-table)
    )
  )

  (defun get-all-tips:object ()
    "get all tips LOCAL"
    (fold
      (lambda
        (
          acc
          tip-hash
        )
        (+ acc [(+ {"hash":tip-hash} (read tip-table tip-hash))])
      )
      []
      (keys tip-table)
    )
  )

  (defun get-place:object ()
    "get colors LOCAL"
    (fold
      (lambda
        (
          acc
          pixel-id-str
        )
        (+ acc [(+ [pixel-id-str] [(at "color" (read color-table pixel-id-str))])])
      )
      []
      (keys color-table)
    )
  )

  (defun get-place-price:decimal ()
    (with-read place-table "data" {"price":=price}
      price
    )
  )
)

;(create-table color-table)
;(create-table place-table)
;(create-table pixel-table)
;(create-table account-table)
;(create-table tip-table)
;(initialize)
