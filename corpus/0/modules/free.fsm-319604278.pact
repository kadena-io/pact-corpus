(module fsm GOVERNANCE
    (use free.fsyc-pre-sale)
    (use free.util-lists [chain])
    (defconst CURRENT_ID_COUNT "current-id-count")
    (defconst BANK_KDA_ACCT "fsm-mining-bank")
    (defconst MAX_CLAIM_AMOUNT 10) ; max nfts that can be claimed in a single transaction
    (defconst TOTAL_KDA_MINED "total-kda-mined")
    (defconst MINED_TABLE_INDEX "mined-table-index") ; will be used for the key of each row in the mined-table after full mint out
                                                    ; increasing by 1 every time the list gets larger than 504
                                                    ; 1 update every 20 minutes, each list holds 5 days of updates
    (defconst MAX_SUPPLY:integer 9999)
    (defconst PRICE_KEY "price-key")
    (defconst STAKED "staked")
    (defconst COLLECTION_INDEX "collection-index")
    (defconst MINED_COLLECTION_INDEX "mined-collection-index")
    (defconst CLAIM_COUNT "claimed-index")
    (defconst MINED_INDEX "mined-index")
    (defconst MINING_ACCOUNT "fsm-mining")
    (defconst FSM_BANK:string "k:ae418d602096fc967702f4d05d69f37898fd79bafbcd29278222724f869a21fe")
    (defconst RILEY_ACCOUNT "k:ae418d602096fc967702f4d05d69f37898fd79bafbcd29278222724f869a21fe") ;TODO change for rileys address

    (defconst REINVESTMENT_FEE "reinvestment-fee") ; for purchasing new miners
    (defconst COMPANY_FEE "company-fee")  ; for company costs
    (defconst ELECTRIC_FEE "electric-fee") ; for paying the electric bill
    (defconst STAKE_TYPE:list [1,2])
    (defconst DEFAULT_URI "ipfs://bafybeiaia5wzyvql7tsrufjh4auub63j5llkeel7zk4i3ndowkui3abzeq")

    (defschema counts-schema
        @doc "Keeps track of how many things there are."
        count:integer
    )

    (defschema price-schema
        @doc "Stores the price of each type of NFT or upgrade"
        price:decimal
    )

    (defschema claim
    	@doc "Stores information about each account that has claimed a reserved token"
    	account:string
    	amount-fsyc:integer ;counts up from zero
    )

    (defschema mined-presale-schema
    	@doc "Stores information about each presale NFT that has been minted"
    	account:string
    	amount-fsyc:integer
      mint-status:integer ; 0 means it's yet to be minted | 1 means it has been minted
    )

    (defschema fsyc-collection-schema
    	@doc "Stores the fsyc collection"
    	uri:string
    	attributes:[object]
      mint-status:integer ; 0 means it's yet to be minted | 1 means it has been minted
    )

    (defschema kda-mined-schema
      @doc "stores the datetime and amount of total KDA mined, keys are 1->infinity"
     	kda-mined:decimal
    )

    (defschema mined-schema
        @doc "key is the current id count"
        kda-mined-list:[decimal]
    )

    (defschema admin-withdraw-schema
        mined-index:integer ;indicates where in the list the admin withdrew
        key-index:integer ;indicates which key in the table the admin withdrew
        kda-mined:decimal ;excess kda of non staked nfts
    )

    (defschema mined-sum-schema
        @doc "Contains the sum of the list whenever a new NFT gets minted"
        sum:decimal
        electric-rate:decimal
    )

    (defschema account-schema
        @doc "Contains the total amount reinvested and already claimed"
        ; key is the k:address
        address:string
        reinvested:decimal
        claimed:list
    )

    (defschema ledger
      @doc "Store core information about each nft"
    	id:string
    	name: string
    	uri:string
    	owner:string
    	attributes:[object]
    	mined-index:integer
    	fsm-count:integer ; MINED_TABLE_INDEX matches this until mint-out
    	percent-reinvested:decimal
    	staked:bool
    	stake-type:integer
    )

    (defschema staking-schema
        @doc "Store core information about account staked tokens"
        tokens:list
        number:integer
    )

    (defschema claim-schema
        claim-count:integer
        account:string
        claim:list
    )

    (deftable mined-presale-table:{mined-presale-schema})
    (deftable nft-collections-table:{fsyc-collection-schema})
    (deftable admin-withdraw-table:{admin-withdraw-schema})
    (deftable accounts-table:{account-schema})
    (deftable mined-sum-table:{mined-sum-schema})
    (deftable price-table:{price-schema})
    (deftable claim-fsm-ledger:{claim})
    (deftable counts-table:{counts-schema})
    (deftable fsm-nfts:{ledger})
    (deftable account-staking-table:{staking-schema})
    (deftable kda-mined-table:{kda-mined-schema})
    (deftable mined-table:{mined-schema})
    (deftable claim-table:{claim-schema})

    (defun initialize ()
        @doc "Initializes values upon deploy of the contract"
        (with-capability (GOVERNANCE)
            (insert price-table PRICE_KEY { "price": 0.1})
            (insert counts-table CURRENT_ID_COUNT { "count": 1 })
            (insert counts-table STAKED { "count": 0 })
            (insert counts-table COLLECTION_INDEX { "count": 0 })
            (insert counts-table CLAIM_COUNT { "count": 0 })
            (insert counts-table MINED_COLLECTION_INDEX { "count": 0 })
            (insert counts-table MINED_INDEX { "count": 0 }) ;size of the list
            (insert price-table TOTAL_KDA_MINED { "price": 0.0})
            (insert mined-table "0" { "kda-mined-list": [] })
            (insert counts-table MINED_TABLE_INDEX { "count": 1 }) ;number of keys in the table
            (insert price-table REINVESTMENT_FEE { "price" : 0.1 })
            (insert price-table COMPANY_FEE { "price" : 0.1 })
            (insert price-table ELECTRIC_FEE { "price" : 0.59 })
            (insert admin-withdraw-table "Riley" { "mined-index" : 0, "key-index" : 1, "kda-mined": 0.0 })
        )
    )

    (defun get-earned-kda-for-ids (reinvested:bool ids:[string] type:integer)
        @doc "Gets the total earned kda for a list of NFTs"
        (fold (+) 0.0 (map (get-earned-kda-for-id reinvested type false) ids))
    )

    (defun get-earned-kda-nfts (owner:string ids:[string] type:integer)
        @doc "Gets the total earned and reinvested kda for a list of NFTs"
        (let*
            (
                (earned-kda (fold (+) 0.0 (map (get-earned-kda-nft type) ids)))
                (earned-reinvested-kda (get-reinvested-kda owner))
            )
            (+ earned-kda earned-reinvested-kda)
        )
    )

    (defun get-earned-kda-nft (type:integer id:string)
        @doc "Gets the total earned kda for an NFT"
        (get-earned-kda-for-id true type false id)
    )

    (defun private-get-earned-kda-for-ids (reinvested:bool ids:[string])
        @doc "gets the total earn kda for a list of NFTs, ensures only the owner can call this"
        ;used for calculating claimable kda
        (fold (+) 0.0 (map (private-get-earned-kda-for-id reinvested) ids))
    )

    (defun claim-kda (recipient:string reinvested:bool ids:list)
        @doc "Allows a user to withdraw x amount of KDA from the automated bank wallet"
        (with-capability (PRIVATE)
        (with-capability (BANK_DEBIT)
        (with-capability (ACCOUNT_GUARD recipient)
            (let*
                (
                    (single (private-get-earned-kda-for-id reinvested (at 0 ids)))
                    (num-nfts (length ids))
                    (calculated (* single num-nfts))
                    (reinvested-amount (get-reinvested-kda recipient))
                )
                (if (> (+ calculated reinvested-amount) 0.0)
                    [
                        (emit-event (CLAIM_FSM_REWARDS recipient reinvested ids))
                        (claim-work recipient ids (+ calculated reinvested-amount))
                    ]
                    (format "Total claim amount is {}. Claim amount must be above 0.0 KDA, thus no kda was sent" [(+ calculated reinvested-amount)])
                )
            )
        )))
    )

    (defun get-reinvested-kda (recipient:string)
        @doc "Gets the reinvested KDA for mining"
        (with-read accounts-table recipient
            {
                "reinvested" := reinvested
            }
            reinvested
        )
    )

    (defun claim-work (recipient:string ids:list calculated:decimal)
        @doc "does the work for the claim function"
        (require-capability (PRIVATE))
        (let
            (
                (previous-balance (coin.get-balance recipient))
            )
            (install-capability (coin.TRANSFER BANK_KDA_ACCT recipient calculated))
            (coin.transfer BANK_KDA_ACCT recipient calculated)
            (map (update-kda-mined-index) ids)
            (update-kda-claim recipient calculated ids)
            (update accounts-table recipient  { "reinvested" : 0.0 } )
            ; (emit-event (CLAIM recipient calculated))
            (format "Succesfully claimed {} kda from {} nfts. Old balance: {} KDA and New Balance: {} KDA" [calculated, (length ids), previous-balance, (coin.get-balance recipient)])
        )
    )

    (defun add-claim (account:string amount:decimal)
        @doc "Adds a claim to the claim table"
        (require-capability (PRIVATE))
        (insert claim-table (int-to-str 10 (get-count CLAIM_COUNT))
            { "account": account
            , "claim": (make-list 1 {"amount":amount, "tiempo": (at "block-time" (chain-data))})
            , "claim-count": (get-count CLAIM_COUNT) }
        )
        (increase-count CLAIM_COUNT)
    )

    (defun get-claimed-kda (account:string)
        @doc "Get KDA earned by a user."
        (select claim-table (where "account" (= account)))
    )

    (defun update-kda-mined-index (id:string)
        @doc "updates the kda-mined-index for one nft"
        (require-capability (PRIVATE))
        (update fsm-nfts id
            { "mined-index": (get-count MINED_INDEX)
            , "fsm-count": (- (get-count MINED_TABLE_INDEX) 1)}
        )
    )

    (defun insert-kda-mined (new-kda-mined:decimal)
        @doc "updates the total kda mined"
        (with-capability (PRIVATE)
        (with-capability (MINING_ADMIN)
            (let*
                (
                    (current-id-count (int-to-str 10 (- (get-count MINED_TABLE_INDEX) 1)))
                    (previous-kda-mined (get-price TOTAL_KDA_MINED))

                    (difference (- new-kda-mined previous-kda-mined))
                )
                (enforce (>= difference 0.0) "kda-mined cannot be negative compared to the previous value")

                    (with-default-read mined-table current-id-count ;update if the key exists
                        { 'kda-mined-list: []}
                        { 'kda-mined-list := kda-mined-list }
                        (write mined-table current-id-count
                            { "kda-mined-list": (+ kda-mined-list [difference]) }
                        )
                    )
                (update price-table TOTAL_KDA_MINED
                    {"price": new-kda-mined}
                )
                (increase-count MINED_INDEX)
                (format "total kda mined = {}, difference from last = {}" [new-kda-mined difference])

            )
        ))
    )

    (defun sum-previous-key ()
        (require-capability (PRIVATE))
        (with-read mined-table (int-to-str 10 (- (get-count MINED_TABLE_INDEX) 1))
            { "kda-mined-list" := kda-mined-list }
            (insert mined-sum-table (int-to-str 10 (- (get-count MINED_TABLE_INDEX) 1))
                { 'sum : (fold (+) 0.0 kda-mined-list)
                , 'electric-rate : (get-price ELECTRIC_FEE) }
            )
        )
    )

    (defun insert-new-mined-key ()
        (require-capability (PRIVATE))
        (insert mined-table  (int-to-str 10 (get-count MINED_TABLE_INDEX))
            { "kda-mined-list": [] }
        )
        (sum-previous-key)
    )

    (defun get-mined-table ()
        (select mined-table (or? (where "kda-mined-list" (= [])) (where "kda-mined-list" (!= []))))
    )

    (defun get-mined-sum()
        (select mined-sum-table (or? (where "sum" (= 0.0)) (where "sum" (!= 0.0))))
    )

    (defun insert-nft-collection-multiple (collections:list)
        (with-capability (MINING_ADMIN)
            (with-capability (PRIVATE)
                (map (insert-nft-collection) collections)
            )
        )
    )

    (defun insert-nft-collection (collection-id collection:object)
        (with-capability (MINING_ADMIN)
            (enforce (!= (at 'uri collection) "") "NFT uri can't be empty")
            (enforce (!= (at 'name collection) "") "NFT name can't be empty")
            (enforce (!= (at 'attributes collection) []) "NFT attributtes must be an array of object.")

            (insert nft-collections-table collection-id
                { "uri":  (at 'uri collection)
                ,"attributes": (at 'attributes collection)
                ,"mint-status": 1
                }
            )

            (update fsm-nfts collection-id
                { "uri":  (at 'uri collection)
                ,"attributes": (at 'attributes collection)
                ,"name": (at 'name collection)
                }
            )
        )
    )

    (defun get-all-nft-collection ()
    	@doc "Returns a list of all NFT collections"
    	(select nft-collections-table (where "uri" (!= "null")))
    )

    (defun get-nft-without-collection ()
        @doc "Get the NFTs without collection."
        (select fsm-nfts ['id] (where "uri" (= DEFAULT_URI)))
    )

    (defun nft-collection-count ()
    	@doc "Returns the total number of NFT collections"
    	(get-count COLLECTION_INDEX)
    )

    (defun get-mined-presale-function ()
    	@doc "Returns a list of all NFT transfered from presale"
    	(select mined-presale-table (where "mint-status" (!= "null")))
    )


    (defun admin-mint-fsm (account:string amount:integer)
    	@doc "Allows the admin to mint nfts for giveaway/free mints"
    	(with-capability (GOVERNANCE)
    	(with-capability (PRIVATE)
            (enforce-mint amount)
            (emit-event (BUY_FSM account amount))
            (map (mint account) (make-list amount 1))
        ))
    )

    (defun buy-fsm (account:string amount:integer)
    	@doc "Allows the user to buy an NFT that has not yet been minted"
    	(with-capability (PRIVATE)
            (coin.transfer account FSM_BANK (* amount (get-price PRICE_KEY)))
            (enforce-mint amount)
            (emit-event (BUY_FSM account amount))
            (map (mint account) (make-list amount 1))
        )
    )

    (defun set-count (key:string count:integer)
        (with-capability (GOVERNANCE)
            (update counts-table key
                { "count": count })
        )
    )

    (defun claim-reserved-fsm (account:string amount:integer)
    	@doc "Allows the user to claim the amount of NFTs they have reserved in the pre-sale"
    	(with-capability (ACCOUNT_GUARD account)
    	(with-capability (PRIVATE)
        (enforce (<= amount MAX_CLAIM_AMOUNT) (format "Only {} can be claimed in one Transaction" [MAX_CLAIM_AMOUNT]))
    		(update-claim-fsm account amount)
    	 	(map (mint account) (make-list amount 1))
        ))
    )

    (defun update-claim-fsm (account:string amount:integer)
    	@doc "inserts or updates a key in the claim-fsm-ledger"
    	(require-capability (PRIVATE))
        (let
            (
        		(num-tokens-reserved (free.fsyc-pre-sale.get-fsyc-reserved account))
        		(amount-already-claimed (get-claimed-amount account))
                (exists (try false (let ((ok true)) (with-read claim-fsm-ledger account { 'account := temp-account } "") ok)))
            )
        	(enforce (<= (+ amount amount-already-claimed) num-tokens-reserved)
                (format "Error: You only have {} tokens reserved. You have already claimed {} NFTs and you're trying to claim {} additional NFTs" [num-tokens-reserved amount-already-claimed amount]))
            (if (not exists)
                [(emit-event (TRANSFER_PRESALE account amount))
                 (insert claim-fsm-ledger account
                    { "account": account
                    , "amount-fsyc": amount })
                ]
                [(update claim-fsm-ledger account
                    { "amount-fsyc": (+ amount-already-claimed amount) }
                )]
            )
        )
    )

    (defun mint (account:string amount:integer)
    	(require-capability (PRIVATE))
    	(let
    	    (
    		    (id (int-to-str 10 (get-count CURRENT_ID_COUNT)))
                (exists (try false (let ((ok true)) (with-read accounts-table account {'claimed := temp1}"") ok)))
    	    )

            (if (= false exists)
                (insert accounts-table account
                    { "claimed":  []
                    , "reinvested": 0.0
                    , "address": account } )
                "")
        	(insert fsm-nfts id
        		{ "id": id
                , "name": ""
        		, "uri": DEFAULT_URI
        		, "owner": account
        		, "attributes": []
        		, "fsm-count": (get-count MINED_TABLE_INDEX)
                , "mined-index": (get-count MINED_INDEX)
                , "percent-reinvested": 0.0
                , "staked":false
                , "stake-type": 0
            }
          )
          (insert-new-mined-key)
          (increase-count CURRENT_ID_COUNT)
          (increase-count MINED_TABLE_INDEX)
          (increase-count MINED_COLLECTION_INDEX)
          (update counts-table MINED_INDEX { "count": 0 } )
          (format "Minted id {}" [id])

        )
    )

    (defun get-nft-collection-uri:object (collection-id:string)
        (with-default-read nft-collections-table collection-id
            { 'uri: 0, 'attributes: []  }
            { 'uri:= uri, 'attributes:= attributes }
            {"uri": uri, "attributes":attributes}
        )
    )

    (defun get-tokens-owned (account:string)
        @doc "Get the NFTs owned by a particular owner."
        (select fsm-nfts (where "owner" (= account)))
    )

    (defun stake-ids (account:string ids:[string] percent-reinvested:decimal stake-type:integer)
		@doc "Allows a user to stake their NFTs to start earning rewards"
		(with-capability (PRIVATE)
        (with-capability (BANK_DEBIT)
        (let*
            (
                (previously-staked-tokens (with-default-read account-staking-table account
                    { "tokens" : [] }
            		{ "tokens" := tokens }
            		tokens ))
                (token-length (+ (length ids) (length previously-staked-tokens)))
                (new-token-list (chain [previously-staked-tokens ids]))
            )
            (if (> (length previously-staked-tokens) 0)
                    [(claim-kda account true previously-staked-tokens)
                        (map (lambda (id) (update fsm-nfts id
                            { "mined-index": (get-count MINED_INDEX)
                            , "fsm-count" : (- (get-count MINED_TABLE_INDEX) 1)})) previously-staked-tokens)]"")

            (enforce-reinvested-percent percent-reinvested)
            (enforce (= true (contains stake-type STAKE_TYPE)) "Staking type not allowed")
            (write account-staking-table account { "tokens": new-token-list, "number": token-length })
            (map (claim-unclaimed-rewards) ids)
			(map (stake-id account stake-type percent-reinvested) ids)
	    )
        (format "Staked ids {}" [ids])))
    )

    (defun stake-id (account:string stake-type:integer percent-reinvested:decimal id:string)
    	@doc "Updates the staked status of one NFT" ;30.5% reinvested is entered as 30.5
    	(require-capability (PRIVATE))
        (with-capability (OWNER id)
            (if (not (is-staked id))
                [(emit-event (STAKE_FSM account stake-type percent-reinvested id))
                (update fsm-nfts id
                    { "mined-index": (get-count MINED_INDEX)
                    , "fsm-count" : (- (get-count MINED_TABLE_INDEX) 1)
                    , "percent-reinvested": (round percent-reinvested 4)
                    , "staked": true
                    , "stake-type": stake-type
                    }
                )
                (increase-count STAKED)]
                (format "Item {} is already staked" [id])
            )
        )
    )

    (defun claim-unclaimed-rewards (id:string)
        (require-capability (PRIVATE))
        (with-default-read admin-withdraw-table "Riley"
            {  'mined-index: 0, 'key-index: 0, 'kda-mined: 0}
            {  'mined-index:=mined-index, 'key-index:=key-index, 'kda-mined:=kda-mined}
            (update admin-withdraw-table "Riley" {
            'mined-index: mined-index,
            'key-index: key-index,
            'kda-mined: (+ (get-earned-kda-for-id false 1 true id) kda-mined)
        }))
    )

    (defun enforce-reinvested-percent (percent-reinvested:decimal)
        @doc "Makes sure reinvested percentage is between 1% - 100%"
        (enforce (>= percent-reinvested 1.0) "Reinvested percentage must be greater than 1.0")
        (enforce (<= percent-reinvested 100.0) "Reinvested percentage must be less than or equal to 100.0")
    )

    (defun unstake-ids (account:string ids:[string])
        @doc "Updates the ids' staked status to false and claims KDA at the same time"
        (with-capability (PRIVATE)
            (let*
                (
                    (previously-staked-tokens (with-default-read account-staking-table account
                        { "tokens" : [] }
                        { "tokens" := tokens }
                        tokens ))
                    (new-list (remove-items previously-staked-tokens ids))
                    (token-length (length new-list))
                )
                (claim-kda account true ids)
                (write account-staking-table account { "tokens": new-list, "number": token-length })
                (map (unstake-id account) ids)
            )
        )
    )

    (defun unstake-id (account:string id:string)
        (require-capability (PRIVATE))
        (with-capability (OWNER id)
            (if (and (= (nft-stake-type id) 1) (is-staked id))
                [
                    (emit-event (UNSTAKE_FSM account id))
                    (update fsm-nfts id
                        { "staked": false
                        , "percent-reinvested": 0.0
                        , "stake-type": 0
                        }
                    )
                    (decrease-count STAKED)
                ]
                "You can not unstake this NFT, NFT currently mining."
            )
        )
    )

    (defun nft-stake-type (id:string)
        (with-read fsm-nfts id
            { 'stake-type := stake-type }
            stake-type
        )
    )

    (defun remove-items:list (in:list items-to-remove:list)
        "Remove all occurrences of items in items-to-remove from in"
        (filter (lambda (item) (not (contains item items-to-remove))) in)
    )

    (defun get-account-earned-kda (account:string)
        @doc "Gets account earned KDA"
        (let*
            (
                (previously-staked-tokens (with-default-read account-staking-table account
                    { "tokens" : [] }
                    { "tokens" := tokens }
                    tokens ))
                (num-nfts (length previously-staked-tokens))
                (single (if (> num-nfts 0) (get-earned-kda-nft 1 (at 0 previously-staked-tokens)) 0))
                (calculated (* single num-nfts))
            )
            calculated
        )
    )

    (defun get-account-staked-info (account:string)
        @doc "Gets account staking information"
        (read account-staking-table account)
    )

    (defun get-average-percent-to-reinvest (ids:[string])
        @doc "returns the average percent-to-reinvest for all IDs that an account owns"
        (/ (fold (+) 0.0 (map (get-percent-to-reinvest) ids)) (length ids))
    )

    (defun get-percent-to-reinvest (id:string)
        (with-read fsm-nfts id
            { 'percent-reinvested := percent-reinvested }
            percent-reinvested
        )
    )

    (defun update-kda-mined-admin (kda-mined:decimal index:string )
        @doc "updates the total kda mined"
        (with-capability (GOVERNANCE)
            (update kda-mined-table index
                { "kda-mined": kda-mined
                , "datetime": (at "block-time" (chain-data))}
            )
        )
    )

    (defun get-attributes (id:string)
        (format "placeholder {}" [id])
    )

    (defun get-claimed-amount (account:string)
    	@doc "returns the total number of fsyc presale tokens already claimed for a single account"
    	(with-default-read claim-fsm-ledger account
            { "amount-fsyc" : 0 }
    		{ "amount-fsyc" := amount }
    		amount
    	)
    )

    (defun enforce-mint (amount:integer)
    	@doc "Ensures the conditions to mint are met before allowing a user to mint an NFT"
    	(let (
    	        (total-fsm-reserved (free.fsyc-pre-sale.get-total-fsyc-reserved))
    		    (current-supply (- (get-count CURRENT_ID_COUNT) 1))
    	     )
    		 (enforce (<= (+ current-supply amount) (- MAX_SUPPLY total-fsm-reserved )) (format "This transaction will exceed the maximum supply, there are currently {} minted" [current-supply]))
    	)
    )

    (defun get-remaining-claims-for-account (account:string)
        (let
            (
                (reserved (free.fsyc-pre-sale.get-fsyc-reserved account))
                (already-claimed (get-claimed-amount account))
            )
            (- reserved already-claimed)
        )
    )

    (defun select-accounts-over-threshold (account:string)
        (select accounts-table (and? (where "address" (= account)) (where "reinvested" (> (get-price PRICE_KEY)))))
    )

    (defun get-max-supply ()
        @doc "Returns the maximum supply of all the NFTs"
        MAX_SUPPLY
    )

    (defun get-all-nfts ()
    	@doc "Returns a list of all NFT objects"
    	(select fsm-nfts (where "owner" (!= "null")))
    )

    (defun is-staked (id:string)
        (with-default-read fsm-nfts id
            {'staked: false}
            {'staked:=staked}
            staked
        )
    )

    (defun get-nfts-details (id:string)
        @doc "Returns token info"
        (read fsm-nfts id)
    )

    (defun get-count:integer (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counts-table key ['count]))
    )

    (defun increase-count (key:string)
        @doc "Increase the count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )

    (defun update-percentage-fee (key:string percentage:decimal)
        @doc "Updates the price of a key"
        (enforce (> percentage 0.0) "Percentage must be greater than 0.0")
        (with-capability (GOVERNANCE)
            (update price-table key {"price": percentage})
        )
    )

    (defun decrease-count (key:string)
        @doc "Decrease the count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update counts-table key {"count": (- (get-count key) 1)})
    )

    (defun get-price (price-key:string)
        @doc "Gets the price for a key"
        (at "price" (read price-table price-key ["price"]))
    )

    (defun set-price(key:string value:decimal)
        @doc "Sets the price for a key to store in the price-table"
        (with-capability (MINING_ADMIN)
            (update price-table key
                {"price": value}
        ))
    )

    (defun update-kda-claim (account:string amount:decimal ids:[string])
        @doc "Adds a claim amount to the airdrop table for an already existing account"
        (require-capability (PRIVATE))
        (with-read accounts-table account
            { 'claimed := claim-temp
            , 'reinvested := previous-reinvested }
            (let
                (
                    (reinvested (get-average-percent-to-reinvest ids))
                )
                (add-claim account amount)
            )
        )
    )

    (defun get-claim-details (account:string)
        @doc "Returns all claim events and current reinvested amount"
        (read accounts-table account)
    )

    (defun get-accrued-reinvestements (id:string type:integer account:string)
        @doc "Front-end only: Returns the accrued and unclaimed reinvestments"
        ;(require-capability (PRIVATE))
        (with-read accounts-table account
            { 'reinvested := accrued }
            (let
                (
                    (currently-unclaimed (get-earned-kda-for-id true type false id))
                )
                (+ currently-unclaimed accrued)
            )
        )
    )

    (defun middle-calc (fsm-count:integer divide:bool)
        (let*
            (
                (current-id-count (- (get-count MINED_TABLE_INDEX) 1))
                (ses
                    (if (>= (- current-id-count fsm-count) 2)
                        (map (get-mined-sum-for-count divide) (enumerate (+ fsm-count 1) (- current-id-count 1)))
                        [{"s":0.0, "es": 0.0}]) )
                (s (map (at-s) ses))
                (es (map (at-es) ses))
            )
            {"s": (fold (+) 0.0 s), "es": (fold (+) 0.0 es) }
        )
    )

    (defun at-s (x:object) (at 's x))

    (defun at-es (x:object) (at 'es x))

    (defun get-earned-kda-for-id (reinvested:bool type:integer turning-on:bool id:string)
        @doc "Get the KDA earned for a single NFT, including fees"
        (with-read fsm-nfts id
            { 'mined-index := mined-index
            , 'fsm-count := fsm-count
            , 'percent-reinvested := percent-reinvested
            , 'stake-type := stake-type
            , 'staked := staked }
                    (let*
                        (
                            (current-id-count (- (get-count MINED_TABLE_INDEX) 1))
                            (first-electric-rate
                                (with-read mined-sum-table (int-to-str 10 (- fsm-count 1))
                                    { 'electric-rate := electric-rate }
                                    electric-rate ))
                            (last-electric-rate
                                (with-read mined-sum-table (int-to-str 10 (- current-id-count 1))
                                    { 'electric-rate := electric-rate }
                                    electric-rate ))
                            (first-calc (get-earned-kda-for-count true mined-index fsm-count))
                            (middle-object
                                    (if (>= (- current-id-count fsm-count) 2)
                                        (middle-calc fsm-count true)
                                        {"s":0.0, "es": 0.0} )
                            )
                            (last-calc
                                    (if (!= current-id-count fsm-count)
                                        (get-earned-kda-for-count true 0 current-id-count)
                                    0.0 )
                            )
                            (first-electric-sum (* first-calc first-electric-rate))
                            (last-electric-sum (* last-calc last-electric-rate))
                            (total-earned (+ (+ first-calc (at 's middle-object)) last-calc))
                            (fee-total (* (+ (get-price REINVESTMENT_FEE) (get-price COMPANY_FEE)) total-earned))
                            (total-earned-minus-fees (- total-earned fee-total) )
                            (total-minus-electric (- total-earned-minus-fees (+ (+ first-electric-sum last-electric-sum) (at 'es middle-object))))
                        )
                        (if (= reinvested true)
                            (* total-minus-electric (/ percent-reinvested 100))
                            (* total-minus-electric (- 1 (/ percent-reinvested 100)))
                        )

                        (if (and (= true staked) (= stake-type type))
                            total-minus-electric
                            (if turning-on total-earned 0.0)
                        )
                        ; (format "current-id-count {}, first-electric-rate {}, last-electric-rate {}, first-calc {}, middle-object {}, last-calc {}, first-electric-sum {}, last-electric-sum {}, total-earned {}, fee-total {}, total-earned-minus-fees {}, total-minus-electric {}, reinvested-true {}, reinvested-false {}"
                        ; [current-id-count first-electric-rate last-electric-rate first-calc middle-object last-calc first-electric-sum last-electric-sum total-earned fee-total total-earned-minus-fees total-minus-electric (* total-minus-electric (/ percent-reinvested 100)) (* total-minus-electric (- 1 (/ percent-reinvested 100)))])
                    )
        )
    )

    (defun admin-withdraw (address:string)
        @doc "Allows the admin to withdraw all accrued fees from the automated bank"
        (with-capability (ACCOUNT_GUARD RILEY_ACCOUNT)
        (with-capability (BANK_DEBIT)
            (with-read admin-withdraw-table "Riley"
                { 'mined-index := mined-index
                , 'key-index := fsm-count }
                (let*
                    (
                        (current-id-count (- (get-count MINED_TABLE_INDEX) 1))
                        (first-electric-rate
                            (with-read mined-sum-table (int-to-str 10 (- fsm-count 1))
                                { 'electric-rate := electric-rate }
                                electric-rate ))
                        (last-electric-rate
                            (with-read mined-sum-table (int-to-str 10 (- current-id-count 1))
                                { 'electric-rate := electric-rate }
                                electric-rate ))
                        (first-calc (get-earned-kda-for-count false mined-index fsm-count))
                        (middle-object
                                (if (>= (- current-id-count fsm-count) 2)
                                    (middle-calc fsm-count false)
                                    {"s":0.0, "es": 0.0} ))
                        (last-calc
                                (if (!= current-id-count fsm-count)
                                    (get-earned-kda-for-count false 0 current-id-count)
                                    0.0 ))
                        (calc-sum (+ (+ first-calc (at 's middle-object)) last-calc ))
                        (electric-sum (+ (+ (* first-calc first-electric-rate) (* last-calc last-electric-rate)) (at 'es middle-object)))
                        (other-fee-sum (* calc-sum (+ (get-price REINVESTMENT_FEE) (get-price COMPANY_FEE))))
                        (sum (+ electric-sum other-fee-sum))
                    )
        ; (format "current-id-count {}, first-electric-rate {}, last-electric-rate {}, first-calc {}, middle-object {}, last-calc {}, calc-sum {}, electric-sum {}, other-fee-sum {}, sum {} "
        ;     [current-id-count first-electric-rate last-electric-rate first-calc middle-object last-calc calc-sum electric-sum other-fee-sum sum])
                    (install-capability (coin.TRANSFER BANK_KDA_ACCT address sum))
                    (enforce (> sum 0.0) "No KDA to be claimed at this time" )
                    (update admin-withdraw-table "Riley"
                        { 'mined-index : (get-count MINED_INDEX)
                        , 'key-index : (- (get-count MINED_TABLE_INDEX) 1) }
                    )
                    (coin.transfer BANK_KDA_ACCT address sum)
                    (format "{} KDA transferred to {}" [sum address])
                )
            )
        ))
    )

    (defun get-admin-withdraw ()
        (with-capability (GOVERNANCE)
            (read admin-withdraw-table "Riley")
        )
    )

    (defun get-kda-mined-for-index (index:integer)
        @doc "Gets the object containing amount of kda mined and date/time for an index"
        (read mined-table (int-to-str 10 index))
    )

    (defun get-mined-sum-for-count (divide:bool fsm-count:integer)
        (with-read mined-sum-table (int-to-str 10 fsm-count)
            { "sum" := sum
            , "electric-rate" := electric-rate }
            (let*
                (
                    (divisor (if (= true divide)
                                fsm-count
                                1))
                    (sum (round (/ sum divisor) 5))
                    (electric-sum (* sum electric-rate))
                )
                {"s":sum, "es": electric-sum}
            )
        )
    )

    (defun get-earned-kda-for-count (divide:bool mined-index:integer fsm-count:integer)
        (with-read mined-table (int-to-str 10 fsm-count)
            { 'kda-mined-list := kda-mined-list }
            (let*
                (
                    (divisor (if (= true divide)
                                fsm-count
                                1))
                    (result (round (/ (fold (+) 0.0 (drop mined-index kda-mined-list)) divisor) 5))
                )
                result
            )
        )
    )

    (defun get-max-claim-amount ()
      MAX_CLAIM_AMOUNT
    )

    (defun private-get-earned-kda-for-id (reinvested:bool id:string)
        (with-capability (OWNER id)
            (with-read fsm-nfts id
                { 'stake-type := stake-type}
                (get-earned-kda-for-id reinvested stake-type false id)
            )
        )
    )

    (defun create-simple-user-guard (funder:string amount:decimal)
        (with-capability (GOVERNANCE)
        (coin.transfer-create funder BANK_KDA_ACCT
            (create-BANK_DEBIT-guard) amount)
        )
    )

    (defun require-BANK_DEBIT ()
        (require-capability (BANK_DEBIT))
    )

    (defun create-BANK_DEBIT-guard ()
        (create-user-guard (require-BANK_DEBIT))
    )

    (defcap GOVERNANCE ()
        (enforce-guard (keyset-ref-guard "free.fsm-admin"))
    )

    (defcap MINING_ADMIN ()
        (enforce-guard (keyset-ref-guard "free.fsm-mining-admin"))
    )

    (defcap PRIVATE ()
        true
    )

    (defcap OWNER (id:string)
        @doc "Enforces that an account owns a particular fsm NFT"
        (let
            (
                (nft-owner (at "owner" (read fsm-nfts id ["owner"])))
            )
            (compose-capability (ACCOUNT_GUARD nft-owner))
        )
    )

    (defcap ACCOUNT_GUARD (account:string)
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard
        (at "guard" (coin.details account))
        )
    )

    (defcap BANK_DEBIT ()
        true
    )

    (defcap BUY_FSM:bool (account:string amount:integer)
        @doc "For event emission of minted NFTs"
        @event true
    )

    (defcap CLAIM_FSM_REWARDS:bool (recipient:string reinvested:bool ids:list)
        @doc "For event emission for claiming rewards"
        @event true
    )

    (defcap STAKE_FSM:bool (account:string stake-type:integer percent-reinvested:decimal id:string)
        @doc "For event emission for staking NFTs"
        @event true
    )

    (defcap UNSTAKE_FSM:bool (account:string id:string)
        @doc "For event emission for unstaking NFTs"
        @event true
    )

    (defcap TRANSFER_PRESALE:bool (account:string amount:integer)
        @doc "For event emission for transfer of presale NFTs to the main smart contract."
        @event true
    )
)
