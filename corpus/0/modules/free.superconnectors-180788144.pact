(module superconnectors GOVERNANCE

  @doc "Superconnectors are the most important people in the world. They are the people who know everyone and can \
  \ connect you to anyone. They are the people who can get things done. They are the glue that holds the world \
  \ together. They are the most valuable people in your network. They are the people you want to be."

  @model
    [ (defproperty valid-id (connection:string)
        (= (length connection) 36)
      )
      (defproperty valid-event-type (type:string)
        (contains type VALID_TYPES)
      )
    ]

  ; --------------------------------------------------------------------------
  ; Schemas and Tables

  (defschema connection-event-schema
    @doc "A connection event is a record of a connection between two people. It is a record of a meeting, a phone \
    \ call, an email, a text message, ... anything that connects two people."

    @model
      [ (invariant (valid-id connection-id))
        (invariant (valid-event-type type))
      ]
    id: string
    connection-id:string
    type:string
    metadata:object
  )
  (deftable connection-events:{connection-event-schema})

  ; --------------------------------------------------------------------------
  ; Capabilities

  (defcap GOVERNANCE ()
    @doc "Only the Admin can upgrade the contract"
    (enforce-keyset ADMIN_KEYSET)
  )

  (defcap OPS ()
    @doc "Capability for running operation functions"
    (enforce-keyset OPERATIONS_KEYSET)
  )

  ; --------------------------------------------------------------------------
  ; Constants

  (defconst ADMIN_KEYSET (read-keyset "varias-admin-keyset")
    "Admin keyset for the contract")
  (defconst OPERATIONS_KEYSET (read-keyset "varias-operations-keyset")
    "Operations keyset for the contract")
  (defconst VALID_TYPES:[string] ["planned-meeting" "updated-meeting" "blocked-meeting"]
    "List of valid event types")

  ; --------------------------------------------------------------------------
  ; Utilities

  (defun validate-id (id:string type:string)
    @doc "Validate an id"
    (enforce (is-charset CHARSET_LATIN1 id)
      (format "{} id must be a valid latin1 string" [type])
    )
    (enforce (= (length id) 36)
      (format "{} id must be 36 characters long" [type])
    )
  )

  (defun validate-connection-id (connection-id:string)
    @doc "Validate a connection id"
    (validate-id connection-id "Connection")
  )

  (defun validate-connection-event-id (connection-event-id:string)
    @doc "Validate a connection event id"
    (validate-id connection-event-id "Connection Event")
  )

  (defun validate-type (type)
    @doc "Validate a type"
    (enforce (is-charset CHARSET_LATIN1 type)
      "Type must be a valid latin1 string"
    )
    (enforce (contains type VALID_TYPES ) (format "Type must be one of {}" [VALID_TYPES]))
  )

  ; --------------------------------------------------------------------------
  ; Superconnectors Contract

  (defun add-connection-event:object{connection-event-schema}
    (connection-event:object{connection-event-schema})
    @doc "Create a connection event"
    (with-capability (OPS)
      (bind connection-event
        {
          "id":= id,
          "connection-id":= connection-id,
          "type":= type,
          "metadata":= metadata
        }
        (validate-connection-event-id id)
        (validate-connection-id connection-id)
        (validate-type type)
        (insert connection-events id
          {
            "id": id,
            "connection-id": connection-id,
            "type": type,
            "metadata": metadata
          }
        )
        { "id": id,
          "connection-id": connection-id,
          "type": type,
          "metadata": metadata
        }
      )
    )
  )

  (defun find-connection-event:object{connection-event-schema}
    (connection-event-id:string)
    @doc "Find a connection event"
    (with-capability (OPS)
      (validate-connection-event-id connection-event-id)
      (with-read connection-events connection-event-id
        { "connection-id":= connection-id,
          "type":= type,
           "metadata":= metadata
        }
        { "id": connection-event-id,
          "connection-id": connection-id,
          "type": type,
          "metadata": metadata
        }
      )
    )
  )

  (defun find-connection-events-by-connection-id:[object{connection-event-schema}]
    (connection-id:string)
    @doc "Find connection events by connection id"
    (with-capability (OPS)
      (validate-connection-id connection-id)
      (select connection-events
        (where "connection-id" (= connection-id))
      )
    )
  )
)

