(module mainnetmodulemakagyn0 GOV
  
  ; ----------------------
  ; capabilities

  (defcap GOV ()
    "module governance"
    (enforce-keyset "free.mainnetkeysetmakagyn")
  )

  (defcap POOL-GUARD ()
    "pool guard capability"
    true
  )

  (defcap INTERNAL (from-function:string)
    "internal capability"
    true
  )

  ; ----------------------
  ; schemas and tables

  (defschema place-schema
    price:decimal
  )

  (deftable place-table:{place-schema})
  
  (defschema pixel-schema
    color:string
    price:decimal
    owner:string
    last-claim-place-price:decimal
  )

  (deftable pixel-table:{pixel-schema})

  ; ----------------------
  ; constants

  (defconst NUM-OF-PIXELS:integer 1000000)

  (defconst INIT-PRICE:decimal 0.01)

  (defconst CREATOR:string "k:46670f2369cdb834a8fc68ad2b1c873bd215a3d04d1e78771859d691fc54aa90")

  (defconst POOL:string "mainnetmodulemakagyn0 test pool")

  ; ----------------------
  ; functions

  (defun enforce-pool-guard ()
    "enforce authorized pool access"
    (require-capability (POOL-GUARD))
  )

  (defun create-pool-account ()
    "make coin account for pool"
    "will fail if called again"
    (coin.create-account POOL (create-user-guard (enforce-pool-guard)))
  )

  (defun place-table-init ()
    "to init place"
    "will fail if called again"
    (insert place-table "data" {"price":(* INIT-PRICE NUM-OF-PIXELS)})
  )

  (defun validate-color:bool (color:string)
    (enforce (= (length color) 7) "Invalid color length")
    (enforce (= (at 0 (str-to-list color)) "#") "Invalid color 1st character")
    (enforce (= (length (filter (= "#") (str-to-list color))) 1) "Invalid color too many #s")
    (enforce (fold (and) true (map (lambda (x) (contains x ["#","0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"])) (str-to-list color))) "Invalid color characters")
    true
  )

  (defun buy-one (pixel-id:integer account:string color:string request-price:decimal)
    "buy a pixel"
    
    ; ---- VALID COLOR
    (validate-color color)

    ; ---- VALID PIXEL
    (enforce (<= pixel-id (- NUM-OF-PIXELS 1)) "Invalid pixel-id")
    (enforce (>= pixel-id 0) "Invalid pixel-id")

    ; ---- BUY
    (let
      (
        (pixel-id-str (int-to-str 10 pixel-id))
        (place-price (at "price" (read place-table "data")))
      )
      (with-capability (INTERNAL "buy")
        (if (contains pixel-id-str (keys pixel-table))
          (buy-used pixel-id-str account color place-price request-price)
          (buy-new pixel-id-str account color place-price)
        )
      )
    )
  )

  (defun buy-many (pixel-ids:list)
    "buy many pixels in one tx"
    (map
      (lambda
        (pixel)
        (let
          (
            (pixel-id:integer (at "pixel-id" pixel))
            (account:string (at "account" pixel))
            (color:string (at "color" pixel))
            (request-price:decimal (at "request-price" pixel))
          )
          (buy-one pixel-id account color request-price)
        )
      )
    )
  )

  (defun buy-new (pixel-id-str:string account:string color:string place-price:decimal)
    "buy pixel new"
    "can only be called by buy-one"
    (require-capability (INTERNAL "buy"))
    (let
      (
        (place-price-new (+ INIT-PRICE place-price))
        (pixel-price-new (* 2 INIT-PRICE))
      )
      (install-capability (coin.TRANSFER account CREATOR (* 0.26 INIT-PRICE)))
      (coin.transfer account CREATOR (* 0.26 INIT-PRICE))
      (install-capability (coin.TRANSFER account POOL (* 0.74 INIT-PRICE)))
      (coin.transfer account POOL (* 0.74 INIT-PRICE))
      (install-capability (coin.TRANSFER POOL CREATOR (/ (- place-price (* INIT-PRICE NUM-OF-PIXELS)) NUM-OF-PIXELS)))
      (with-capability (POOL-GUARD)
        (coin.transfer POOL CREATOR (/ (- place-price (* INIT-PRICE NUM-OF-PIXELS)) NUM-OF-PIXELS))
      )
      (update place-table "data" {"price":place-price-new})
      (insert pixel-table pixel-id-str {"color":color,"price":pixel-price-new,"owner":account,"last-claim-place-price":place-price-new})
    )
  )

  (defun buy-used (pixel-id-str:string account:string color:string place-price:decimal request-price:decimal)
    "buy pixel used"
    "can only be called by buy-one"
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
      (enforce (= request-price pixel-price) "Price changed")
      (coin.transfer account pixel-owner (* 0.25 pixel-price))
      (coin.transfer account CREATOR (* 0.01 pixel-price))
      (coin.transfer account POOL (* 0.74 pixel-price))
      (with-capability (POOL-GUARD)
        (coin.transfer POOL pixel-owner (/ (- place-price pixel-last-claim-place-price) NUM-OF-PIXELS))
      )
      (update place-table "data" {"price":place-price-new})
      (update pixel-table pixel-id-str {"color":color,"owner":account,"last-claim-place-price":place-price-new,"price":pixel-price-new})
    )
  )

  (defun claim-rewards (pixel-id:integer)
    "send all pixel rewards to owner"
    (let*
      (
        (pixel-id-str (int-to-str 10 pixel-id))
        (pixel (read pixel-table pixel-id-str))
        (pixel-owner (at "owner" pixel))
        (pixel-last-claim-place-price (at "last-claim-place-price" pixel))
        (place-price (at "price" (read place-table "data")))
        (reward (- place-price pixel-last-claim-place-price))
        (pixel-owner-guard (at "guard" (coin.details pixel-owner)))
      )
      (enforce-guard pixel-owner-guard)
      (enforce (not (= 0 reward)) "No reward available")
      (with-capability (POOL-GUARD)
        (coin.transfer POOL pixel-owner reward)
      )
      (update pixel-table pixel-id-str {"last-claim-place-price":place-price})
    )
  )

  (defun get-all-pixels:list ()
    "get all place pixel data"
    "call locally"
    (map
      (lambda
        (pixel-id-str)
        (let
          (
            (pixel (read pixel-table pixel-id-str))
          )
          {"id":pixel-id-str,"color":(at "color" pixel),"owner":(at "owner" pixel),"price":(at "price" pixel)}
        )
      )
      (keys pixel-table)
    )
  )
)

