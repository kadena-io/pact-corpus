(module kadena-place-paint GOV ; ---- CHANGE ON NEW

  ; ---------------------- Simplifying v0
  
  ; ----------------------
  ; capabilities

  (defcap GOV ()
    "module governance"
    (enforce-keyset "free.kadena-place-admin") ; ---- CHANGE ON NEW
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
    account-created:time
    lifetime-pixels-bought:integer
  )

  (deftable account-table:{account-schema})

  ; ----------------------
  ; constants

  (defconst NUM-OF-PIXELS:integer 1000000)

  (defconst INIT-PRICE:decimal 0.01)

  (defconst CREATOR:string "k:414b6583c0899eccedb14a50d0e0152d0be13f6b2d1ddd080ea1d6393f2115ce") ; ---- CHANGE ON NEW

  (defconst POOL:string "kadena-place paint pool") ; ---- CHANGE ON NEW

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

  (defun validate-pixel-id:bool (pixel-id-str:string)
    "pixel-ids only 0 to 999999"
    (enforce (= (length pixel-id-str) 6) "Invalid pixel-id")
    (let
      (
        (pixel-id:integer (str-to-int 10 pixel-id-str))
      )
      (enforce (<= pixel-id (- NUM-OF-PIXELS 1)) "Invalid pixel-id")
      (enforce (>= pixel-id 0) "Invalid pixel-id")
    )
    true
  )

  (defun create-account (account:string)
    "create a kadena place account"
    (enforce-guard (at "guard" (coin.details account)))
    (insert account-table account {"account-created":(at "block-time" (chain-data)),"lifetime-pixels-bought":0})
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
                (validate-pixel-id pixel-id-str)
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
                    [CREATOR]
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
                      (+ debts [{"pixel-owner":CREATOR,"pixel-owner-rewards":creator-allocation}])
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
      total-reward
    )
  )

  (defun get-account:object (account:string)
    (read account-table account)
  )

  (defun get-pixel-data:object (pixel-id-str:string)
    "get pixel data"
    (let*
      (
        (pixel (read pixel-table pixel-id-str))
      )
      (+ (+ {"price-hundredths-str":(int-to-str 10 (floor (* (at "price" pixel) 100)))} (drop ["price"] pixel)) (read color-table pixel-id-str))
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
)


