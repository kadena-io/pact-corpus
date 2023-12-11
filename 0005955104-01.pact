(module mainnetmodulemakagyn2 GOV ; ---- CHANGE ON NEW
  
  ; ----------------------
  ; capabilities

  (defcap GOV ()
    "module governance"
    (enforce-keyset "free.mainnetkeysetmakagyn") ; ---- CHANGE ON NEW
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
  )

  (deftable account-table:{account-schema})

  (defschema tip-schema
    time:time
    message:string
    from:string
    to:string
    amount:decimal
  )

  (deftable tip-table:{tip-schema})

  ; ----------------------
  ; constants

  (defconst NUM-OF-PIXELS:integer 1000000)

  (defconst INIT-PRICE:decimal 0.01)

  (defconst CREATOR:string "k:46670f2369cdb834a8fc68ad2b1c873bd215a3d04d1e78771859d691fc54aa90") ; ---- CHANGE ON NEW

  (defconst POOL:string "kadena-place mainnet pool mainnetmodulemakagyn2") ; ---- CHANGE ON NEW

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

  (defun create-account (account:string)
    "create a kadena place account"
    (enforce-guard (at "guard" (coin.details account)))
    (insert account-table account {"joined":(at "block-time" (chain-data)),"min-tip":0.0,"tip-count":0,"lifetime-pixels-bought":0})
  )

  (defun buy-one (pixel-id-str:string account:string color:string request-price-hundredths-str:string is-new-pixel:bool)
    "buy a pixel"
    (validate-color color)
    (validate-pixel-id (str-to-int 10 pixel-id-str))
    (update account-table account {"lifetime-pixels-bought":(+ 1 (at "lifetime-pixels-bought" (read account-table account)))})
    (let
      (
        (place-price (at "price" (read place-table "data")))
      )
      (with-capability (INTERNAL "buy")
        (if is-new-pixel
          (buy-new pixel-id-str account color place-price (/ (str-to-int 10 request-price-hundredths-str) 100.0) true)
          (buy-used pixel-id-str account color place-price (/ (str-to-int 10 request-price-hundredths-str) 100.0) true)
        )
      )
    )
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
                    (buy-new pixel-id-str account color place-price (/ (str-to-int 10 request-price-hundredths-str) 100.0) false)
                    (buy-used pixel-id-str account color place-price (/ (str-to-int 10 request-price-hundredths-str) 100.0) false)
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
        (owner-allocations:list
          (let
            (
              (owners:list
                (distinct
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
                      debts
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
            (owner-and-allocation)
            (let
              (
                (owner (at "owner" owner-and-allocation))
                (allocation (at "allocation" owner-and-allocation))
              )
              (install-capability (coin.TRANSFER POOL owner allocation))
              (coin.transfer POOL owner allocation)
            )
          )
          (+ owner-allocations [{"owner":CREATOR,"allocation":creator-allocation}])
        )
      )
    )
  )

  (defun buy-new:object (pixel-id-str:string account:string color:string place-price:decimal request-price:decimal pay:bool)
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
        (if pay
          (with-capability (POOL-GUARD)
            (coin.transfer account POOL INIT-PRICE)
            (install-capability (coin.TRANSFER POOL CREATOR creator-rewards))
            (coin.transfer POOL CREATOR creator-rewards)
          )
          true
        )
        (update place-table "data" {"price":place-price-new})
        (insert pixel-table pixel-id-str {"price":pixel-price-new,"owner":account,"last-claim-place-price":place-price})
        (insert color-table pixel-id-str {"color":color})
        {"creator-rewards":creator-rewards,"pixel-owner-rewards":0.0,"pixel-owner":"","request-price":request-price}
      )
    )
  )

  (defun buy-used:object (pixel-id-str:string account:string color:string place-price:decimal request-price:decimal pay:bool)
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
        (if pay
          (with-capability (POOL-GUARD)
            (coin.transfer account POOL pixel-price) ; ---- requires (coin.TRANSFER account POOL pixel-price) Capability
            (install-capability (coin.TRANSFER POOL CREATOR creator-rewards))
            (coin.transfer POOL CREATOR creator-rewards)
            (install-capability (coin.TRANSFER POOL pixel-owner pixel-owner-rewards))
            (coin.transfer POOL pixel-owner pixel-owner-rewards)
          )
          true
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

  (defun give-tip (from-account:string to-account:string amount:decimal message:string)
    "tip an account"
    (coin.transfer from-account to-account amount)
    (let*
      (
        (to-account-row (read account-table to-account))
        (tip-count (at "tip-count" to-account-row))
        (min-tip (at "min-tip" to-account-row))
        (tip-hash (hash (+ (hash to-account) (hash (int-to-str 10 tip-count)))))
      )
      (enforce (>= amount min-tip) "Account's min-tip is higher than amount")
      (insert tip-table tip-hash {"time":(at "block-time" (chain-data)),"message":message,"from":from-account,"to":to-account,"amount":amount})
      (update account-table to-account {"tip-count":(+ tip-count 1)})
    )
  )

  (defun get-tips:object (tip-hashes:list)
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
)


