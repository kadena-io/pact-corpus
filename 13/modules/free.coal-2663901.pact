(module coal COALGOV

  (defconst ADMIN_ADDRESS "k:5a7fd75c8fefbe3d116504004c0e417cc2328a823eff89dd6d3168e5e02b95b0")
  (defconst TOTAL_COALS 6969)
  (defconst MINT_CHAIN_ID "13")
  (defconst DAWN_OF_TIME (time "1970-01-01T00:00:00Z"))
  (defconst COALS_MINTED_COUNT_KEY 'coals-minted)

  (defconst COALS_ARE_PAUSED_VALUE 1)
  (defconst COALS_ARE_RUNNING_VALUE 0)
  (defconst COALS_RUNNING_STATE_KEY 'coals-running-state)


  (defcap COALGOV () (enforce-keyset "free.coal-maker-keyset"))

  (defun get-mint-price () 13.69)
  (defun get-grace-period () (* 7.0 (* 24.0 (* 60.0 60.0))))
  (defun get-project-royalty () 0.02)
  (defun get-minter-royalty () 0.02)
  (defun get-nofrontrunning-period () (* 7.0 (* 24.0 (* 60.0 60.0))))


  (defcap NFT_SUCCESSFULLY_STOLEN (id:string)
    @event true
    )

  (defcap NFT_MINTED (id:string)
    @event true
    )

  (defcap NFT_SOLD (id:string price:decimal)
    @event true
    )

  (defcap NFT_PRICE_CHANGED (id:string old-price:decimal new-price:decimal)
    @event true
    )

  (defcap NFT_STRENGHENED (id:string old-strength:decimal new-strength:decimal)
    @event true
    )

  (defcap COALCAP ()
    true
  )

  (defschema nofrontrun-schm
    nfr-guard       :guard
    nfr-time        :time
    )

  (defschema coal-schm
    id              :integer
    mint-date       :time
    minter          :string
    owner           :string
    curhash         :string
    attrib          :string
    on-sale         :bool
    last-sale-price :decimal
    sale-timeout    :decimal
    sale-date       :time
    price           :decimal
    coal-guard      :guard
    minter-guard    :guard
    )

  (defschema coal-int-settings-kv-schm
     teh-val        :integer
    )


  (deftable coal-tbl:{coal-schm})
  (deftable coal-int-settings-kv-tbl:{coal-int-settings-kv-schm})
  (deftable nofrontrun-tbl:{nofrontrun-schm})


  (defun grace-period:decimal (id:string)
    (with-read coal-tbl id {'mint-date := mint-date}
      (diff-time (at 'block-time (chain-data)) mint-date)
    )
  )

  (defun get-owner (id:string)
    (with-read coal-tbl id {'owner:=owner} {'owner:owner})
  )

  (defun get-nft-details (id:string)
    (read coal-tbl id)
  )

  (defun load-from (from:integer)
    (select coal-tbl (where 'id (lambda (x) (and (< x (+ from 20)) (>= x from)))))
  )

  (defun load-on-sale-from (from:integer)
    (let (
      (f (lambda (y x) x))
      (mwhere (lambda (y x)
        (and
          (and (>= (at 'id x) from) (> (+ from 20) (at 'id x)))
          (= true (at 'on-sale x))
          )
          )
      ))
      (fold-db coal-tbl (mwhere) (f))
    )
  )

  (defun load-on-sale ()
    (select coal-tbl (where 'on-sale (= true)))
  )

  (defun get-cur-hash (id:string)
    (at 'curhash (read coal-tbl id))
  )

  (defun all-owned-by (owner:string)
    (select coal-tbl (where 'owner (= owner)))
  )

  (defun read-nfr (mhash:string)
    (read nofrontrun-tbl mhash)
  )


  (defun get-strength:integer (xhash:string)
    (- 256 (length (int-to-str 2 (str-to-int 64 xhash))))
  )

  (defun enforce-contract-running ()
    (with-read coal-int-settings-kv-tbl COALS_RUNNING_STATE_KEY {'teh-val:=running-state}
      (enforce (= running-state COALS_ARE_RUNNING_VALUE) "contract is not running")
    )
  )


  (defun mint-coal ()
    (enforce-contract-running)
    (enforce (= (at 'chain-id (chain-data)) MINT_CHAIN_ID) (+ "minting only available on chain " MINT_CHAIN_ID))
    (enforce (> (get-mint-price) 0.0) "mint price should not be zero")
    (let ((minter (at 'sender (chain-data))))
      (with-read coal-int-settings-kv-tbl COALS_MINTED_COUNT_KEY {'teh-val := coals-minted}
        (enforce (<= coals-minted TOTAL_COALS) "all coals are minted")
        (coin.transfer minter ADMIN_ADDRESS (get-mint-price))
        (let (
          (teh-id (int-to-str 10 coals-minted))
          (curhash (hash (+ (hash (chain-data)) (+ (hash coals-minted) minter))))
          )
          (insert coal-tbl teh-id
          {
            'id               :   coals-minted,
            'mint-date        :   (at 'block-time (chain-data)),
            'minter           :   minter,
            'owner            :   minter,
            'curhash          :   curhash,
            'attrib           :   (hash (+ "attributes:" curhash)),
            'on-sale          :   false,
            'last-sale-price  :   (get-mint-price),
            'sale-timeout     :   -1.0,
            'sale-date        :   (at 'block-time (chain-data)),
            'price            :   (get-mint-price),
            'coal-guard       :   (at 'guard (coin.details minter)),
            'minter-guard     :   (at 'guard (coin.details minter))
            })
          (write coal-int-settings-kv-tbl COALS_MINTED_COUNT_KEY {'teh-val : (+ 1 coals-minted)})
          (emit-event (NFT_MINTED teh-id))
          (read coal-tbl teh-id)
        )
      )
    )
  )

  (defun buy-coal (id:string)
    (enforce-contract-running)
    (let ((buyer (at 'sender (chain-data))))
      (with-read coal-tbl id
        {
          'on-sale      :=     on-sale,
          'sale-timeout :=     sale-timeout,
          'sale-date    :=     sale-date,
          'owner        :=     owner,
          'price        :=     price,
          'minter       :=     minter
        }
        (enforce on-sale "nft not on sale")
        (enforce (not (= buyer owner)) "can't buy from self")
        (if
          (> sale-timeout 0.0)
          (enforce (< (diff-time (at 'block-time (chain-data)) sale-date) sale-timeout) "nft sale timeout expired")
          true
          )
        (let (
          (proj-royal:decimal (get-project-royalty))
          (minter-royal:decimal (get-minter-royalty))
          )
          (coin.transfer buyer ADMIN_ADDRESS (* price proj-royal))
          (coin.transfer buyer owner (* price (- 1.0 (+ proj-royal minter-royal))))

          (if (= buyer minter)
            true
            (let ((q false))
              (coin.transfer buyer minter (* price minter-royal))
              )
              )

          (update coal-tbl id {
              'owner            : (at 'sender (chain-data)),
              'on-sale          : false,
              'last-sale-price  : price,
              'coal-guard       : (at 'guard (coin.details buyer))})
          (emit-event (NFT_SOLD id price))
          (read coal-tbl id)
          )
        )
      )
    )


  (defun set-sale-state (id:string price:decimal state:bool timeout:decimal)
    (enforce-contract-running)
    (enforce (> price 0.0001) "yo wtf price?")
    (with-read coal-tbl id {'coal-guard:=coal-guard,'price:=old-price}
      (enforce-guard coal-guard)
      (if state
        (update coal-tbl id {'price:price,'on-sale:true,'sale-timeout:timeout,'sale-date:(at 'block-time (chain-data))})
        (update coal-tbl id {'price:price,'on-sale:false,'sale-timeout:-1.0})
        )
      (emit-event (NFT_PRICE_CHANGED id old-price price))

      )
    )


  (defun calc-nofrontrun-hash (id:string curhash:string proposed-nonce:string)
    (hash (+ id (+ ":" (+ curhash (+ ":" proposed-nonce)))))
    )

  (defun steal-nft (id:string proposed-nonce:string)
    (enforce-contract-running)
    (let ((thief (at 'sender (chain-data))))
      (with-read coal-tbl id {'curhash := curhash}
        (with-read nofrontrun-tbl (calc-nofrontrun-hash id curhash proposed-nonce) {'nfr-guard := nfr-guard}
          (enforce-guard nfr-guard)
          (let ((curgrace (grace-period id)))
            (enforce (>= curgrace (get-grace-period)) "nft in grace period")
            (let (
              (proposed-hash (hash (+ curhash proposed-nonce)))
              )
              (enforce (< (get-strength curhash) (get-strength proposed-hash)) "proposed nonce does not increase the strength")
              (update coal-tbl id {'owner:thief,'coal-guard:(at 'guard (coin.details thief)),'curhash:proposed-hash,'on-sale:false})
              (emit-event (NFT_SUCCESSFULLY_STOLEN id))
              (read coal-tbl id)
              )
            )
          )
        )
      )
    )

  (defun strengthen-nft (id:string proposed-nonce:string)
    (enforce-contract-running)
    (with-read coal-tbl id {'curhash := curhash,'coal-guard:=coal-guard}
      (enforce-guard coal-guard)
      (with-read nofrontrun-tbl (calc-nofrontrun-hash id curhash proposed-nonce) {'nfr-guard := nfr-guard}
        (enforce-guard nfr-guard)
          (let (
            (proposed-hash (hash (+ curhash proposed-nonce)))
            )
            (enforce (< (get-strength curhash) (get-strength proposed-hash)) "proposed nonce does not increase the strength")
            (update coal-tbl id {'curhash:proposed-hash,'on-sale:false})
            (read coal-tbl id)
            (emit-event (NFT_STRENGHENED id (get-strength curhash) (get-strength proposed-hash)))
            )
          )
      )
  )

  (defun verify-nfr (mhash:string)
    (with-read nofrontrun-tbl mhash {'nfr-guard:=nfr-guard}
      (enforce-guard nfr-guard)
    )
  )

  (defun enforce-me (ks:keyset)
    (enforce-keyset ks)
  )

  (defun no-front-running-me (mhash:string)
    (enforce-contract-running)
    (with-default-read nofrontrun-tbl mhash
      {'nfr-guard:"",'nfr-time:DAWN_OF_TIME}
      {'nfr-guard:=nfr-guard,'nfr-time:=nfr-time}
      (if (< (diff-time nfr-time DAWN_OF_TIME) 1.0)
          (insert nofrontrun-tbl mhash
            {
              'nfr-guard  : (create-user-guard (enforce-me (read-keyset 'nfr-ks))),
              'nfr-time   : (at 'block-time (chain-data))
              })
          (let ((q false))
            (enforce
              (> (diff-time (at 'block-time (chain-data)) nfr-time) (get-nofrontrunning-period))
              (+ "someone has the lock on this NFR until" (format-time "%a, %_d %b %Y %H:%M:%S %Z" (add-time nfr-time (get-nofrontrunning-period))))
              )
            (write nofrontrun-tbl mhash
              {
                'nfr-guard  : (create-user-guard (enforce-me (read-keyset 'nfr-ks))),
                'nfr-time   : (at 'block-time (chain-data))
                })
          )
        )
      )
    )

    ; (defun transfer (id:string reci:string)
    ;   (enforce-contract-running)
    ;   (with-read coal-tbl id {'owner:=owner,'coal-guard:=coal-guard}
    ;     (enforce-guard coal-guard)
    ;
    ;     (update coal-tbl id {'owner:reci,'coal-guard:(at 'guard (coin.details reci))})
    ;   )
    ; )


)


