(module rymedi-covid GOVERNANCE

  (use coin)

  (defcap GOVERNANCE ()
    ;going to be goverened by rymedi account
    (enforce-guard (at 'guard (details "rymedi"))))

  (defschema records-schema
    age-group:string
    sex:string
    last-mod-time:time
    result:string
    init-by:string
    end-by:string
    uuid:string
    serial:string
    img-hash:string)

  (defschema doctors-schema
    uuid:string
    name:string
    residency-hospital:string
    account:string
    last-mod-time:time
    num-tests:integer
    guard:guard
    inited:list
    ended:list
    approved:bool)

  (deftable records-table:{records-schema})
  (deftable doctors-table:{doctors-schema})

  (defcap ADMIN ()
    "makes sure only rymedi account can approve new doctors"
    (enforce-guard (at 'guard (coin.details "rymedi")))
  )

  (defcap DOCTOR (uuid:string)
    "make sure doctor is registered and approved"
    ;make sure keypair matches account name (uuid === account name)
    (enforce-guard (at 'guard (coin.details uuid)))
    ;will fail if doctor not in the table
    (with-read doctors-table uuid {"approved" := approved}
      (enforce approved "you are not an approved doctor")
    )
  )

  (defconst POSITIVE:string 'positive)
  (defconst NEGATIVE:string 'negative)
  (defconst INITIATED:string 'initiated)

  (defun init-doctor (uuid:string name:string hospital:string account:string)
    @doc "ADMIN ONLY: create a doctor"
    (with-capability (ADMIN)
      (coin.create-account uuid (read-keyset "ks"))
      (insert doctors-table uuid {
        "uuid": uuid,
        "name": name,
        "residency-hospital": hospital,
        "last-mod-time": (at "block-time" (chain-data)),
        "num-tests": 0,
        "guard": (read-keyset "doc-ks"),
        "inited": [],
        "ended": [],
        "approved": true
      })
    )
  )

  (defun unapprove-doctor (uuid:string)
    @doc "ADMIN ONLY: blacklist a doctor"
    (with-capability (ADMIN)
      (update doctors-table uuid {
        "approved": false
      })
    )
  )

  (defun init-record (age:string sex:string serial:string uuid:string)
    @doc "DOCTOR ONLY: initialize and adiminister a covid-19 test"
    (with-capability (DOCTOR)
      (insert records-table serial {
        "age-group": age,
        "sex": sex,
        "last-mod-time": (at "block-time" (chain-data)),
        "result": INITIATED,
        "init-by": uuid,
        "end-by": INITIATED,
        "serial": serial,
        "img-hash": INITIATED
      })
    )
  )

  (defun end-record (serial:string uuid:string img-hash:string result:bool)
    @doc "DOCTOR ONLY: record result of a covid-19 test"
    (with-capability (DOCTOR)
      (update records-table serial {
        "last-mod-time": (at "block-time" (chain-data)),
        "end-by": uuid,
        "result": (if result POSITIVE NEGATIVE),
        "img-hash": img-hash
      })
    )
  )

  (defun get-doctor-uuids ()
    @doc "return all doctor uuids"
    (keys doctors-table)
  )

  (defun get-test-serials ()
    @doc "return all test serial numbers"
    (keys records-table)
  )

  (defun get-test (serial:string)
    @doc "get data for a test by serial number"
    (read records-table serial)
  )

  (defun get-doctor (uuid:string)
    @doc "get data for a test by serial number"
    (read doctors-table uuid)
  )

)


