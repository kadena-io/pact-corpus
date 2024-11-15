(module kadcars-factory GOVERNANCE

(use kip.token-manifest)


;;;  token id
;;;  MAKE : MODEL : VIN_NUMBER
;;;
;;;


;;;;;;;;;;;;;;  CONTST ;;;;;;;;;;;;;;

(defconst URL "https://bafybeictqzkgpzfwwdno3upe3lrm5z5euo2oe6xohmup2ckoy3qk7xi2zi.ipfs.infura-ipfs.io/?filename=b-1.png")

;;;;;;;;;;;;;; SCHEMAS ;;;;;;;;;;;;;;
(defcap TEST_CAP ()
    @doc "private cap for test"
    true)

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

(defun create-k2ss (vehicleSpec:object)

    vehicleSpec
)

(defun create-k2s (vehicleSpec:object)

  (let*
    (
      (vehicle-spec (at 'vehicle_spec vehicleSpec ))
      (vehicle-information:object{vehicle-information} (at 'vehicle-information vehicle-spec))

      (make:string (at 'make vehicle-information))
      (model:string (at 'model vehicle-information))
      (vin:string (at 'vin vehicle-information))
      (token-id (get-kadcar-token-id vin make model))
      (final-token-id (free.universal-ledger.create-token token-id 0
      (get-k2-manifest vin vehicle-spec) free.kadcars-nft-policy))
    )

    (format "created token {}" [final-token-id])
  )

)

;;vin:string make:string model:string
;;read 3d view references from msg
;;read in top level img from MSG
(defun create-k2 ()

  (let*
    (
      (vehicleSpec:object (read-msg "vehicle_spec"))
      (vehicle-information:object{vehicle-information} (at 'vehicle-information vehicleSpec))
      (make:string (at 'make vehicle-information))
      (model:string (at 'model vehicle-information))
      (vin:string (at 'vin vehicle-information))
      (token-id (get-kadcar-token-id vin make model))
      (final-token-id (free.universal-ledger.create-token token-id 0
      (get-k2-manifest vin vehicleSpec) free.kadcars-nft-policy))
    )

    (format "created token {}" [final-token-id])
  )

)

;;;;;;;;;;;;;; main entry to retrieve k2 manifest ;;;;;;;;;;;;;;

(defun get-k2-manifest(vin-number:string vehicleSpec:object)

(let*
  (
    (vehicle-information:object{vehicle-information} (at 'vehicle-information vehicleSpec))

    (mut:object{mutable-state-schema} (at 'mutable-state vehicleSpec))
    (immut:object{immutable-state-schema} (get-immutable-state vehicle-information))
    (view-ref:object{view-references-schema} (get-view-ref (at 'view-refs vehicleSpec)))

    (mutable (create-datum (uri "pact:schema" "mutable-state-data") mut))
    (immutable (create-datum (uri "pact:schema" "immutable-state-data") immut))
    (view (create-datum (uri "pact:schema" "view-references") view-ref))
  )
  (create-manifest (uri "ipfs" URL) [mutable immutable view])
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
          )
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
      vehicle-info:object{vehicle-information}
    )
      {
        "vehicle-information":
        {
          'vin:(at 'vin vehicle-info),
          'make: (at 'make vehicle-info),
          'model:(at 'model vehicle-info)
        }
        ,'mint-time: (at "block-time" (chain-data))
      }

  )

  (defun get-view-refs:object{view-references} ()
    {
      'genesis-3d-asset: (uri "ipfs" "https://bafybeiblo3baxrxskpsxbgjp2nb7em2img666jo6ssyv4zjbw7ae7lu6q4.ipfs.infura-ipfs.io/?filename=new_embedded.gltf"),
      'final-3d-asset: (uri "pact:schema" "https://bafybeiblo3baxrxskpsxbgjp2nb7em2img666jo6ssyv4zjbw7ae7lu6q4.ipfs.infura-ipfs.io/?filename=new_embedded.gltf")
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
        (final-manifest (get-k2-manifest vin))
      )

      final-manifest
    )

  )

)

