(module rymedi-data-exchange GOVERNANCE

  (use coin)

  (defcap GOVERNANCE ()
    ;going to be goverened by rymedi account
    (enforce-guard (at 'guard (details "rymedi"))))

  (defschema records-schema
    ;data coming in [[hashed(data), blockheight, timestamp], []]
    priv-data:list
    ;data in [[hashed(data), blockheight, timestamp], []]
    pub-plus-data:list
    ;updates to last mod time
    last-mod-time:time
    ;set when initing a node "poster data"
    pub-data:string
    ;list of accounts that can write to this ruuid
    writers:list)


  (deftable record-table:{records-schema})

  (defcap ADMIN ()
    "makes sure only rymedi account can perform certain actions"
    ;;tbd how we implement this, if we want multiple accounts etc
    ;for now, only rymedi account can perform admin actions
    (enforce-guard (at 'guard (coin.details "rymedi")))
  )

  (defcap WRITE (account:string ruuid:string)
    "make sure account can write to a particular ruuid record"
    ;make sure keypair matches account name
    (enforce-guard (at 'guard (coin.details account)))
    ;make sure account can write to specified ruuid
    (with-read record-table ruuid {"writers" := writers}
      (enforce (contains account writers) "you cannot write to this record")
    )
  )


  (defun init-record (ruuid:string pub-data:string approved:list)
    @doc "ADMIN ONLY: create a new entry assosciated to a ruuid"
    (with-capability (ADMIN)
      (insert record-table ruuid {
        ;empty for now, updated by approved writers
        "priv-data": [],
        ;empty for now, will wait for admin to set
        "pub-plus-data": [],
        ;getting block time
        "last-mod-time": (at "block-time" (chain-data)),
        ;"shape" of data -> {"music": "80s disco", "theme": "studio 54", "ratio": "3:1"}
        "pub-data": pub-data,
        ;prepulate list of accounts that can write to this record if had
        "writers": approved
      })
    )
  )

  (defun init-write-record (ruuid:string pub-data:string approved:list priv-data:string)
    @doc "ADMIN ONLY: create a new entry assosciated to a ruuid with initial priv-data"
    (with-capability (ADMIN)
      (insert record-table ruuid {
        ;init the record with an initial piece of data
        "priv-data": [[priv-data, (at "block-height" (chain-data)), (at "block-time" (chain-data))]],
        ;empty for now, will wait for admin to set
        "pub-plus-data": [],
        ;getting block time
        "last-mod-time": (at "block-time" (chain-data)),
        ;"shape" of data -> {"music": "80s disco", "theme": "studio 54", "ratio": "3:1"}
        "pub-data": pub-data,
        ;prepulate list of accounts that can write to this record if had
        "writers": approved
      })
    )
  )

  (defun add-writers (ruuid:string new-writers:list)
    @doc "ADMIN ONLY: add accounts that can write to a particular record"
    (with-capability (ADMIN)
      (with-read record-table ruuid {"writers" := prev-writers}
        (update record-table ruuid
          {"writers": (+ prev-writers new-writers),
          "last-mod-time": (at "block-time" (chain-data))})
      )
    )
  )

  (defun modify-pub-data (ruuid:string new-pub-data:string)
    @doc "ADMIN ONLY: modify public data field"
    (with-capability (ADMIN)
      (update record-table ruuid
        {"pub-data": new-pub-data,
        "last-mod-time": (at "block-time" (chain-data))
        })
    )
  )

  (defun write-pub-plus (ruuid:string pub-plus-data:string)
    @doc "ADMIN ONLY: add public plus data to record. New will append not overwrite"
    (with-capability (ADMIN)
      (with-read record-table ruuid {"pub-plus-data" := prev-pub-plus-data}
        (update record-table ruuid {
          "pub-plus-data": (+ prev-pub-plus-data [[pub-plus-data, (at "block-height" (chain-data)), (at "block-time" (chain-data))]]),
          "last-mod-time": (at "block-time" (chain-data))
        })
      )
    )
  )

  (defun write-record (ruuid:string priv-data:string account:string)
    @doc "let registered writer update a record"
    (with-capability (WRITE account ruuid)
      (with-read record-table ruuid {"priv-data" := prev-priv-data}
        (update record-table ruuid {
          "priv-data": (+ prev-priv-data [[priv-data, (at "block-height" (chain-data)), (at "block-time" (chain-data))]]),
          "last-mod-time": (at "block-time" (chain-data))
        })
      )
    )
  )

  (defun read-record-data (ruuid:string)
    @doc "return the priv-data (list of hashed data) "
    (at "priv-data" (read record-table ruuid))
  )

  (defun read-record-meta (ruuid:string)
    @doc "return the pub-data for a ruuid"
    (at "pub-data" (read record-table ruuid))
  )

  (defun read-record-pub-plus (ruuid:string)
    @doc "return the pub-data for a ruuid"
    (at "pub-plus-data" (read record-table ruuid))
  )

  (defun get-ruuids ()
    @doc "return all ruuids currently inserted in the table"
    (keys record-table))

)


