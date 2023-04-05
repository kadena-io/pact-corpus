(module quality-ledger GOVERNANCE

  ; ------------------ Schemas --------------------------------
  (defschema product
    hash:string
    creation-date:time
    )

  (defschema lot
    product-id:string
    hash:string
    creation-date:time
    )

  ; ------------------ Tables ------------------------------------

  (deftable products-table:{product})
  (deftable lots-table:{lot})


  ; ------------------ Capabilities -----------------------------

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard "free.ql-admin-keyset" )))

  (defcap OPS ()
    (enforce-guard
      (keyset-ref-guard "free.ql-user-keyset" )))


   ; ------------------ Utility Functions -----------------------------

    (defun curr-time:time ()
        @doc "Returns current chain's block-time in time type"
        (at 'block-time (chain-data)))

   ; ------------------ Functions -----------------------------

   (defun insert-product:string (id:string hash:string)
      @doc "Insert new product into products-table"
      (with-capability (OPS)
        (insert products-table id{
          "hash":hash,
          "creation-date":(curr-time)
          })
        (format "Product {} has been inserted with hash {}" [id hash])
      )
   )

   (defun insert-lot:string (id:string product-id:string hash:string)
      @doc "Insert new product lot into lots-table"
      (with-capability (OPS)
        (with-read products-table product-id
          {
            'hash:=product-hash
          }
          (insert lots-table id{
            "product-id":product-id,
            "hash":hash,
            "creation-date":(curr-time)
            })
          (format "Lot {} of product {} has been inserted with hash {}" [id product-id hash])
        )
      )
   )

   (defun read-product (product-id:string)
    @doc "Get product info giving a specific product-id"
    (read products-table product-id)
   )

   (defun read-lot (lot-id:string)
    @doc "Get lot info giving a specific lot-id"
    (read lots-table lot-id)
   )

)

; ------------------ Defining tables ---------------------------------


