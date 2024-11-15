(module marketplace GOVERNANCE

	; TODO: create a separate keyset for marketplace admin
  (defconst ADMIN_KEYSET "n_ebe54249b2e9d68f5060961f3c419f8288d18dc2.marketplace-admin-keyset")

  (defcap GOVERNANCE ()
    (enforce-keyset "n_ebe54249b2e9d68f5060961f3c419f8288d18dc2.admin-keyset"))

  (defschema marketplace
    id:string
    username:string
    title:string
    description:string
    image:string
    price:decimal
    budget:decimal
    created:time
    content:string
    deleted:bool
    type:integer
    guard:guard
  )

  (defschema purchases
    url:string
    price:decimal
    created:time
    keyset:keyset
  )

  (deftable marketplace-table:{marketplace})

  (deftable purchases-table:{purchases})

  (defcap ALLOW_READ_CONTENT (contentId:string userId:string)
    "Only admins, owners and users who have purchased the content can read it."
    (with-read marketplace-table contentId
      { "guard" := ownerGuard }
      (with-default-read purchases-table (concat [contentId ":" userId])
        { "keyset": ownerGuard } ; TODO: what is the correct default value?
        { "keyset" := buyerKeyset }
        (enforce-one "Access denied"
          [(enforce-keyset ADMIN_KEYSET)
           (enforce-keyset ownerGuard)
           (enforce-keyset buyerKeyset)]
        )
      )
    )
  )

  (defcap ALLOW_UPDATE_CONTENT (contentId:string)
    "Only admins and owners can update content."
    (with-read marketplace-table contentId
      { "guard" := ownerGuard }
        (enforce-one "Access denied"
          [(enforce-keyset ADMIN_KEYSET)
           (enforce-keyset ownerGuard)]
        )
    )
  )

  (defun read-content (contentId:string userId:string)
    "Read data for a content id if owner or admin"
    (with-capability (ALLOW_READ_CONTENT contentId userId)
      (select marketplace-table (and? (where 'id (= contentId)) (where 'deleted (= false))))
    )
  )

  (defun content-keys ()
    "Get all content keys"
    (enforce-keyset ADMIN_KEYSET)
    (keys marketplace)
  )

  (defun create-content (
    id:string
    username:string
    title:string
    description:string
    image:string
    price:decimal
    budget:decimal
    content:string
    type:integer
    guard:guard
    )
    "Create new content record with a given keyset."
    (enforce-keyset ADMIN_KEYSET)
    ;TODO: enforce non-null values etc.
    ;(enforce (!= username "") "Username cannot be empty")
    (let ((curr-time:time (at 'block-time (chain-data))))
      (insert marketplace-table id {
        "id": id,
        "username": username,
        "title": title,
        "description": description,
        "image": image,
        "price": price,
        "budget": budget,
        "created": curr-time,
        "content": content,
        "type": type,
        "deleted": false,
        "guard": guard
      })
    )
  )

  (defun update-content (
    id:string
    username:string
    title:string
    description:string
    image:string
    price:decimal
    budget:decimal
    content:string
    type:integer
    guard:guard
    )
    "Update existing content record."
    (with-capability (ALLOW_UPDATE_CONTENT id)
      ;TODO: enforce non-null values etc.
      (update marketplace-table id {
        "id": id,
        "username": username,
        "title": title,
        "description": description,
        "image": image,
        "price": price,
        "budget": budget,
        "content": content,
        "type": type,
        "guard": guard
      })
    )
  )

  (defun delete-content (id:string)
    "Mark existing content record as deleted."
    (with-capability (ALLOW_UPDATE_CONTENT id)
      (update marketplace-table id { "deleted": true })
    )
  )

  (defun fetch-all ()
    (enforce-keyset ADMIN_KEYSET)
    (let*
      ((qry (lambda (k obj) (= false (at "deleted" obj)) ))
        (f (lambda (k obj) [
          k,
          (at 'username obj),
          (at 'title obj),
          (at 'description obj),
          (at 'image obj),
          (at 'price obj),
          (at 'budget obj),
          (at 'type obj),
          (at 'created obj),
          (at 'guard obj)
        ]))
      )
      (fold-db marketplace-table (qry) (f))
    )
  )

  (defun fetch-purchases (id:string)
    (enforce-keyset ADMIN_KEYSET)
    (let*
      ((qry (lambda (k obj) (contains (concat [id ":"]) k) ))
        (f (lambda (k obj) [
          k,
          (at 'price obj)
        ]))
      )
      (fold-db purchases-table (qry) (f))
    )
  )

  (defun fetch-all-purchases ()
    (enforce-keyset ADMIN_KEYSET)
    (let*
      ((qry (lambda (k obj) true ))
        (f (lambda (k obj) [
          k,
          (at 'price obj)
        ]))
      )
      (fold-db purchases-table (qry) (f))
    )
  )

  ; anyone can check if a purchase exists
  (defun read-purchase (id)
    "Read data for purchase"
    (read purchases-table id ['keyset]))

  (defun create-purchase (id url price keyset)
    "Create new purchase record with a given keyset."
    (enforce-keyset ADMIN_KEYSET)
      (let ((curr-time:time (at 'block-time (chain-data))))
      (insert purchases-table id {
        "url": url,
        "price": price,
        "created": curr-time,
        "keyset": keyset
      })
    )
  )
)

;; Read the `upgrade` key from transaction data

