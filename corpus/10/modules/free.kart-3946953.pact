(module kart GOV
  
  (defcap GOV () (enforce-keyset "free.kart-keyset"))
  (defschema pixel rgb:integer)
  (defschema artist-cooldown end-time:time)
  (deftable pixels:{pixel})
  (deftable artist-cooldowns:{artist-cooldown})
  
  (defconst DIMENSION_LENGTH 1000)
  (defconst COORDINATE_LENGTH (log 10 DIMENSION_LENGTH))
  (defconst MAX_RGB_VALUE 16777216)
  (defconst POSITIVE_CORRDINATE_ERR "Coordinates must be positive")
  (defconst INVALID_RGB_ERR "Invalid RGB value: ")
  (use util.guards)

  (defun validate-account-id (accountId:string)
    (enforce
      (is-charset CHARSET_LATIN1 accountId)
      (format "Account ID does not conform to the required charset: {}" [accountId])
    )
    (enforce 
      (and? (> 257) (< 2) (length accountId))
      (format "Account ID must be between 3 and 256 characters long: {}" [accountId])
    )
  )

  (defun validate-coordinate (name:string value:integer)
    (enforce
      (< value DIMENSION_LENGTH)
      (format "{} value '{}' exceeds maximum of {}" [name value DIMENSION_LENGTH])
    )
  )

  (defun get-artist-cooldown (artist:string)
    (with-default-read artist-cooldowns artist { 'end-time: (chain-time) } { 'end-time := end-time }
      (diff-time end-time (chain-time))
    )
  )

  (defun assign-pixel (rgb: string x:integer y:integer)
    (assign-pixel-as (at 'sender (chain-data)) rgb x y)
  )

  (defun assign-pixel-as (artist:string rgb:string x:integer y:integer)
    (enforce (>= x 0) POSITIVE_CORRDINATE_ERR)
    (enforce (>= y 0) POSITIVE_CORRDINATE_ERR)
    (validate-coordinate 'X x)
    (validate-coordinate 'Y y)
    (validate-account-id artist)
    (let ((cooldown (get-artist-cooldown artist)))
      (enforce-one
        (format "You need to wait {} more seconds before making a new assigninment" [cooldown])
        [(enforce (<= cooldown 0.0) "") (enforce-guard "free.rog-keyset")]
      )
      (let ((nRGB (str-to-int 16 rgb)))
        (enforce (and? (> MAX_RGB_VALUE) (<= 0) nRGB) (format "{}{}" [INVALID_RGB_ERR rgb]))
        (insert pixels (format "{}_{}" [x y]) { 'rgb: nRGB })
        (write artist-cooldowns artist { 'end-time: (add-time (chain-time) (minutes 1)) })
      )
    )
  )

  (defun get-section (fromX:integer fromY:integer toX:integer toY:integer)
    (map 
      (lambda (pos) (with-default-read pixels pos { "rgb": 0 } { "rgb" := rgb } rgb))
      (fold (+) []
        (map (lambda (x) (map (lambda (y) (format "{}_{}" [x, y]))
          (enumerate fromY toY))) (enumerate fromX toX)
        )
      )
    )
  )

  (defun get-raw ()
    (map (at 'rgb)
      (select pixels ['rgb] (where 'rgb (constantly true)))
    )
  )

  (defun get-canvas ()
    (fold-db pixels 
      (constantly true)
      (lambda (key obj) (+ {'key: key} (take ['rgb] obj)))
    )
  )
)


