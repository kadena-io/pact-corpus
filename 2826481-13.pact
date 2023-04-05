(module kadcar-factory GOVERNANCE
(bless "--Qc1E2rZDTbwUT_g9T9t9r6XPXCS6so9RmLQNdgoF8")
(use kip.token-manifest)



;;;;;;;;;;;;;; SCHEMAS ;;;;;;;;;;;;;;

  (defschema mutable-state-schema
    @doc "regular nft stats indexed by IDs"
    components:[object:{component}]
  )

  (defschema vehicle-information
    @doc "vehicle identification certificate"
    vin:string
    make:string
    model:string
    )
  (defschema immutable-state-schema
      @doc "immutable-state data such as id and creation information"
      vehicle-information:object{vehicle-information}
      mint-time:time
  )

(defschema component
        name:string
        stats:[object:{keyval}]
)

(defschema keyval
      @doc "Open data model to allow for any typed stat"
      key:string
      val
)

(defschema view-references
        @doc "stores references to NFT 3d files"
        genesis-3d-asset:object{mf-uri}
        final-3d-asset:object{mf-uri}
)
(defschema view-references-schema
        @doc "stores references to NFT 3d files"
        art-asset:object{mf-uri}
)

;;;;;;;;;;;;;; Capabilities ;;;;;;;;;;;;;;

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.kc-policy-admin")))


;;;;;;;;;;;;;;;;;;;;;;;; MARM WRAPPERS ;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-k2s-bulk ()

  (let*
    (
      (specs:[object] (read-msg "specs"))

    )
    (map (create-k2s) specs)
  )
)

(defun create-k2s (vehicleSpec:object)

  (let*
    (
      (cleaned-vehicle-spec (at 'vehicle_spec vehicleSpec ))
      (vehicle-information:object{vehicle-information} (at 'vehicle-information cleaned-vehicle-spec))

      (make:string (at 'make vehicle-information))
      (model:string (at 'model vehicle-information))
      (vin:string (at 'vin vehicle-information))
      (token-id (get-kadcar-token-id vin make model))

    )

    (free.universal-ledger.create-token token-id 0
    (get-k2-manifest cleaned-vehicle-spec vin make model) free.kadcars-nft-policy)
  )
)
;;vin:string make:string model:string
;;read 3d view references from msg
;;read in top level img from MSG
(defun create-k2 ()

  (let*
    (
      (vehicleSpec:object (read-msg "vehicle_spec"))
      (webp-ipfs (at 'webp-ipfs vehicleSpec ))
      (vehicle-information:object{vehicle-information} (at 'vehicle-information vehicleSpec))
      (make:string (at 'make vehicle-information))
      (model:string (at 'model vehicle-information))
      (vin:string (at 'vin vehicle-information))
      (token-id (get-kadcar-token-id vin make model))
      (final-token-id (free.universal-ledger.create-token token-id 0
      (get-k2-manifest vehicleSpec vin make model) free.kadcars-nft-policy))
    )

    (format "created token {}" [final-token-id])
  )

)

;;;;;;;;;;;;;; main entry to retrieve k2 manifest ;;;;;;;;;;;;;;

(defun get-k2-manifest(vehicleSpec:object vin:string make:string model:string)

(let*
  (
    (vehicle-information:object{vehicle-information} (at 'vehicle-information vehicleSpec))
    (webp-ipfs (at 'webp-ipfs vehicleSpec ))

    (mut:object{mutable-state-schema} (at 'mutable-state vehicleSpec))
    (immut:object{immutable-state-schema} (get-immutable-state vin make model))
    (view-ref:object{view-references-schema} (get-view-ref (at 'view-refs vehicleSpec)))

    (mutable (create-datum (uri "pact:schema" "mutable-state-data") mut))
    (immutable (create-datum (uri "pact:schema" "immutable-state-data") immut))
    (view (create-datum (uri "pact:schema" "view-references") view-ref))
  )
  (create-manifest (uri "ipfs" webp-ipfs) [mutable immutable view])
)
)



  (defun mint-k2 (token-id:string account:string account-guard:guard)
    (free.universal-ledger.mint token-id account account-guard 1.0)
  )

  (defun mint-bulk:bool
    (
      account:string
      guard:guard
    )
      (let*
          (
            (ids:[string](read-msg "token-list"))
            (id-len (length ids))
          )
          (enforce (< id-len 10) "minting too many")
          (map (mint-wrapper account guard) ids)
          ids
        )
  )

  (defun mint-wrapper:bool
    (
      account:string
      guard:guard
      id:string
    )
      (mint-k2 id account guard)

  )







  ;;;;;;;;;;;;;; top-level state getters ;;;;;;;;;;;;;;


  (defun get-mutable-state-from-msg:object{mutable-state-schema} ()
      (let*
          (
            (components (read-msg "components"))

            )
             components
        )
  )

  (defun get-view-ref:object{view-references-schema} (obj:object)
      (let*
          (
            (data (at 'data obj))
            (scheme (at 'scheme obj))

            )

             {'art-asset:(uri scheme data)}
        )
  )

  (defun get-immutable-state:object{immutable-state-schema} (
      vin:string make:string model:string
    )
      {
        "vehicle-information":
        {
          'vin:vin,
          'make: make,
          'model:model
        }
        ,'mint-time: (at "block-time" (chain-data))
      }

  )
  ;;;;;;;;;;;;;; Component builders ;;;;;;;;;;;;;;



  (defun get-kadcar-token-id (vin:string make:string model:string)
    (format "{}#{}:{}" [make model vin])
  )

  (defun time-stamp:string (any-data)
    (hash (+ (hash (at 'block-time (chain-data)))
    (hash any-data)))
  )
;;;;;;todelete


  (defun build-k2-manifest:object{manifest} (vin:string)

    (let*
      (
        (vehicleSpec:object{vehicle-information} (read-msg "vehicle_spec"))
        (make:string (at 'make vehicleSpec))
        (model:string (at 'model vehicleSpec))
        (token-id (get-kadcar-token-id vin make model))
        (final-manifest (get-k2-manifest))
      )

      final-manifest
    )

  )

  ;;;TESTING ONLY
  (defun create-test-manifests (number:integer)
    (let*
        (
          (int-list (enumerate 0 number 1))
          (int-string-list (map (int-to-str 10) int-list))
          (int-string-list-2 (map (+ "dsdsd") int-string-list))
          )
        (map (create-test-manifest) int-string-list-2)
      )
  )

  (defun create-test-manifest(vin:string)
    (let*
      (
        (vehicleSpec:object (at 'vehicle_spec (get-test-vehicle-spec vin)))
        (webp-ipfs (at 'webp-ipfs vehicleSpec ))
        (vehicle-information:object{vehicle-information} (at 'vehicle-information vehicleSpec))
        (make:string (at 'make vehicle-information))
        (model:string (at 'model vehicle-information))
        (token-id (get-kadcar-token-id vin make model))
        (final-token-id (free.universal-ledger.create-token token-id 0
          (get-k2-manifest vehicleSpec) free.kadcars-nft-policy))

      )
        final-token-id
    )
  )

  (defun get-test-vehicle-spec (vin:string)

    {
        'vehicle_spec: {
        'vehicle-information : {
            'vin: vin
            ,'make: "Kadcars"
            ,'model: "K2"
          }
        ,'webp-ipfs:"ipfs://bafybeiajzwhyu6yo3ayhanybsn6qfqgibc5ywozregz5zp32b7zow5zqme"
        ,'mutable-state : {
          "components": [
              {
                  "name": "body",
                  "stats": [
                      {
                          "key": "body-type",
                          "val": ""
                      },
                      {
                          "key": "body-material",
                          "val": {
                              "type": "",
                              "id": ""
                          }
                      },
                      {
                          "key": "trim-material",
                          "val": {
                              "type": "",
                              "id": ""
                          }
                      },
                      {
                          "key": "headlights",
                          "val": {}
                      },
                      {
                          "key": "length",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "ground-to-roof",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "ground-clearance",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "wheel-base",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "weight",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "center-width",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "hood-width",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "grill-width",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "rear-width",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      }
                  ]
              },
              {
                  "name": "wheel",
                  "stats": [
                      {
                          "key": "wheel-type",
                          "val": "offroad"
                      },
                      {
                          "key": "rim-type",
                          "val": ""
                      },
                      {
                          "key": "rim-material",
                          "val": {
                              "type": "",
                              "id": ""
                          }
                      },
                      {
                          "key": "width",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "diameter",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "rim-to-edge",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      },
                      {
                          "key": "weight",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      }
                  ]
              },
              {
                  "name": "engine",
                  "stats": [
                      {
                          "key": "weight",
                          "val": {
                              "value": 0.0,
                              "unit": ""
                          }
                      }
                  ]
              }
          ]
        }
        ,'view-refs:{
           "data":"ipfs://bafybeialeqtxuzejtxoxcplmrysxwzo4ywrxhnajscafpk4ere2wh4vuci",
           "scheme":"ipfs://"
          }
      }
    }
  )

  (defun get-test-datum ()
    (let* (
        (vehicleSpec:object (at 'vehicle_spec (get-test-vehicle-spec "test")))
        (mut:object{mutable-state-schema} (at 'mutable-state vehicleSpec))
        (mutable (create-datum (uri "pact:schema" "test-state-data") mut))
      )
      mutable
    )
  )
)
