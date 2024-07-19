(module inu-crew 'inu-boss

  @doc " Grab yours algo-gen Inu Gangster cause something serious Ahusharmota is about to happen  \
       \ #fucktheilluminat https://www.inucrewnft.com \
       \ Kadena ecosystem fam - you came early. "

    (defconst GANGSTERS_CREATED_COUNT "gangsters-count")
    (defconst GANGSTERS_MINT_COUNT "gangsters-mint-count")
    (defconst GANGSTERS_MINTED_COUNT "gangsters-minted-count")
    (defconst GANGSTERS_URI "gangsters-uri")
    (defconst MINT_CHAIN_ID "mint-chain-id")
    (defconst PRICE "price")
    (defconst INU_BOSS (read-keyset 'inu-boss))
    (defconst INU_BOSS_WALLET "k:460d776c75c4a665acd070d3d3f1fc96a26c96837af8d5ce3436b75fe487975a")

    (defcap PRIVATE ()
        @doc "can only be called from a private context"
        true
    )

    (defcap ACCOUNT_GUARD (account:string)
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defcap IS_OWNER (account:string id:string)
        @doc "Enforces that an account owns a inu crew gangster"
        (let
            (
                (nft-owner (at "owner" (read nfts id ["owner"])))
            )
            (enforce (= nft-owner account) "Account is not owner of this Inu Gangster")
            (compose-capability (ACCOUNT_GUARD account))
        )
    )

    (defcap IS_BOSS()
        @doc "Only allows Inu Boss to call these"
        (enforce-keyset INU_BOSS)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD INU_BOSS_WALLET))
    )

    (defun init ()
        @doc "Inu Crew is in the BUILDING!"
        (insert counts GANGSTERS_CREATED_COUNT {"count": 0})
        (insert counts GANGSTERS_MINT_COUNT {"count": 0})
        (insert counts GANGSTERS_MINTED_COUNT {"count": 0})
        (insert values MINT_CHAIN_ID {"value": "1"})
        (insert values GANGSTERS_URI {"value": "https://firebasestorage.googleapis.com/v0/b/inucrewnft.appspot.com/o/"})
        (insert price PRICE {"price": 77.7})
    )

    ;;;;;; SCHEMAS AND TABLES ;;;;;;

    (defschema inu-gangster-schema
        @doc "Stores core information about each Inu Gangster"
        id:string
        generation:integer
        birthday:time
        owner:string
        name:string
        strength:integer
        wisdom:integer
        charisma:integer
        endurance:integer
    )

    (defschema mint-schema
        @doc "Gen 0s for minting"
        name:string
        strength:integer
        wisdom:integer
        charisma:integer
        endurance:integer
        minted:bool
    )

    (defschema counts-schema
        @doc "Schema used for counters"
        count:integer
    )

    (defschema values-schema
        @doc "Schema used for storing values"
        value:string
    )

    (defschema price-schema
        @doc "Schema used for price"
        price:decimal
    )

    (deftable nfts:{inu-gangster-schema})
    (deftable mint:{mint-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})
    (deftable price:{price-schema})

    ;;;;;; INU CREW STATE MODIFYING FUNCTIONS - REQUIRE CAPABILITIES ;;;;;;

    (defun create-multiple-gen-0-for-mint (attributes-list:list)
        @doc "Take a list of attributes, create gen 0 Inu gangsters for mint"
        (with-capability (IS_BOSS)
            (map
                (create-gen-0-for-mint)
                attributes-list
            )
        )
    )

    (defun create-gen-0-for-mint (attributes:list)
        (require-capability (IS_BOSS))
        (let
            ((id (int-to-str 10 (get-count GANGSTERS_MINT_COUNT))))
            (insert mint id { "name": (at 0 attributes), "strength": (at 1 attributes), "wisdom": (at 2 attributes), "charisma": (at 3 attributes), "endurance": (at 4 attributes), "minted": false })
        )
        (increase-count GANGSTERS_MINT_COUNT)
    )

    (defun mint-for-giveaway (owner:string amount:integer)
        @doc "Mints inu gangster for giveaway campaign"
        (with-capability (IS_BOSS)
            (map
                (mint-inu-gangster owner)
                (make-list amount 1)
            )
        )
    )

    (defun mint-inu-gangsters (owner:string amount:integer)
        @doc "Mints a gen 0 inu gangster"
        (enforce (>= amount 1) "Must mint at least one gangster")
        (enforce (<= amount 5) "Maximum 5 Inu Gangsters NFTs for per account")
        (let (
                (gangsters-owned (gangsters-owned-by owner))
            )
            (enforce (>= 5 (+ amount (length gangsters-owned))) "Maximum 5 NFTs per account")
        )
        (let (
                (gangsters-minted (get-count GANGSTERS_MINTED_COUNT))
                (gangsters-created-for-mint (get-count GANGSTERS_MINT_COUNT))
            )
            (enforce (<= (+ gangsters-minted amount) gangsters-created-for-mint) "Tried to mint more gangsters then available! Please reduce the amount.")
        )
        (if
            (!= owner INU_BOSS_WALLET)
            (coin.transfer owner INU_BOSS_WALLET (* (get-price) amount))
            "Pay to the Inu Crew Boss"
        )
        (with-capability (ACCOUNT_GUARD owner)
            (with-capability (PRIVATE)
                (map
                    (mint-inu-gangster owner)
                    (make-list amount 1)
                )
            )
        )
    )

    (defun mint-inu-gangster (owner:string number:integer)
        @doc "Mints a gen 0 gangster"
        (enforce (= number 1) "Number enforced to be 1 to avoid confusion but allow mapping to work")
        (require-capability (PRIVATE))
        (require-capability (ACCOUNT_GUARD owner))
        (let (
                (id (id-for-new-gangster))
                (data (get-latest-minting-data))
            )
            (mint-gangster id {
                "id": id,
                "generation": 0,
                "birthday": (at "block-time" (chain-data)),
                "name": (at "name" data),
                "strength": (at "strength" data),
                "wisdom": (at "wisdom" data),
                "charisma": (at "charisma" data),
                "endurance": (at "endurance" data),
                "owner": owner
            })
        )
        (increase-count GANGSTERS_MINTED_COUNT)
    )

    (defun mint-gangster (id:string data:object)
        @doc "Mints a new Inu Gangster"
        (require-capability (PRIVATE))
        (let ((mint-chain-id (get-value MINT_CHAIN_ID)))
            (enforce (= (curr-chain-id) mint-chain-id) "Can only mint on specific chain")
        )
        (let ((id (id-for-new-gangster)))
            (insert nfts id data)
        )
        (increase-count GANGSTERS_CREATED_COUNT)
    )

    (defun increase-count(key:string)
        @doc "Increases count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update counts key
            {"count": (+ 1 (get-count key))}
        )
    )

    (defun set-value(key:string value:string)
        @doc "Sets the value for a key to store in a table"
        (with-capability (IS_BOSS)
            (update values key
                {"value": value}
            )
        )
    )

    (defun set-price(price-value:decimal)
        @doc "Set the price"
        (with-capability (IS_BOSS)
            (update price PRICE {"price": price-value})
        )
    )

    (defun transfer:string
        ( id:string
          sender:string
          receiver:string
          amount:decimal
        )
        @doc " Transfer to an account, failing if the account to account does not exist. "
        (enforce (= 1.0 amount) "Only 1 inu gangster can be transferred")
        (enforce-account-exists receiver)
        (enforce (= sender INU_BOSS_WALLET) "Can only send from admin account for now")
        (with-capability (IS_BOSS)
            (with-capability (IS_OWNER sender id)
                (update nfts id {"owner": receiver})
            )
        )
    )

    (defun giveaway:string
        ( id:string
          receiver:string
        )
        @doc " Transfer to an account, failing if the account to account does not exist. "
        (enforce-account-exists receiver)
        (with-capability (IS_BOSS)
            (update nfts id {"owner": receiver})
        )
    )


    (defun inu-club-fight (owner:string kdaAmount:integer gangster:string)
        @doc "Inu Club Fight"
        (enforce (>= kdaAmount 5) "Minumun 5 KDA for play")
        (enforce (<= kdaAmount 60) "Maximum 60 KDA for paly")
        (let (
                (gangsters-owner (is-gangsters-owner gangster))
            )
            (enforce (= owner gangsters-owner) "You are not the owner of this NFT")
        )

        (if
            (!= owner INU_BOSS_WALLET)
            (coin.transfer owner INU_BOSS_WALLET kdaAmount)
            "Deposit KDA to play in the Inu Club"
        )
        (with-capability (ACCOUNT_GUARD owner)
            (with-capability (PRIVATE)
                (fight gangster)
            )
        )
    )

    (defun fight (gangsterId:string)
        @doc "Inu Club Fight"
        (with-capability (PRIVATE)
            (let* ((gangster-opponent "1:56")
                   (gangster-opponent-hp (calc-gangster-hp gangster-opponent))
                   (gangster-hp (calc-gangster-hp gangsterId)))
              (> gangster-hp gangster-opponent-hp))
        )
    )

    ;;;;;; INU CREW NON STATE MODIFYING HELPER FUNCTIONS ;;;;;;

    (defun get-price()
        (at "price" (read price PRICE ["price"]))
    )

    (defun get-count (key:string)
        @doc "Gets count for key"
        (at "count" (read counts key ['count]))
    )

    (defun get-value (key:string)
        @doc "Gets value for a key"
        (at "value" (read values key ['value]))
    )

    (defun get-latest-minting-data ()
        (require-capability (PRIVATE))
        (let
            (
                (minted-count (get-count GANGSTERS_MINTED_COUNT))
                (created-for-mint-count (get-count GANGSTERS_MINT_COUNT))
            )
            (enforce (< 0 created-for-mint-count) "No gangsters have been put up for minting")
            (enforce
                (< minted-count created-for-mint-count)
                 "All gangsters put up for minting have already been minted, please check later"
            )
            (let
                (
                    (data (read mint (int-to-str 10 minted-count) ['name 'strength 'wisdom 'charisma 'endurance 'minted]))
                )
                (enforce (= (at "minted" data) false) "All gangsters have been minted. Viva la Kadena.")
                data
            )
        )
    )

    (defun enforce-account-exists (account:string)
        @doc "Enforces that an account exists in the coin table"
        (let ((coin-account (at "account" (coin.details account))))
            (enforce (= coin-account account) "account was not found")
        )
    )

    (defun gangsters-owned-by (owner:string)
        @doc "All gangsters owned by someone"
        (select nfts ["id"] (where "owner" (= owner)))
    )
    (defun is-gangsters-owner (id:string)
        @doc "All gangsters owned by someone"
        (select nfts ["owner"] (where "id" (= id)))
    )

    (defun get-gangster-attributes-for-ids (ids:list)
        @doc "Return fields for a list of ids"
        (with-capability (PRIVATE)
            (map
                (get-gangster-fields-for-id ["name" "strength" "wisdom" "charisma" "endurance"])
                ids
            )
        )
    )

    (defun get-gangster-fields-for-id (fields:list id:string )
        @doc "Return the fields for a given id"
        (require-capability (PRIVATE))
        (+ {"id": id} (read nfts id fields))
    )

    (defun get-gangster-attributes-for-fight (ids:string)
        @doc "Return fields for a list of ids"
        (with-capability (PRIVATE)
            (map
                (get-gangster-attributes-for-fight-by-id ["name" "strength" "wisdom" "charisma" "endurance"])
                ids
            )
        )
    )

    (defun get-gangster-attributes-for-fight-by-id (fields:list id:string )
        @doc "Return the fields for a given id"
        (require-capability (PRIVATE))
        (+ {} (read nfts id fields))
    )

    (defun uri:string (id:string)
        @doc
        " Give URI for ID. If not supported, return \"\" (empty string)."
        (+ (get-value GANGSTERS_URI) (+ id ".png?alt=media"))
    )

    (defun id-for-new-gangster ()
        @doc "Returns an id for a new gangster"
        (require-capability (PRIVATE))
        (+ (+ (curr-chain-id) ":") (int-to-str 10 (get-count GANGSTERS_CREATED_COUNT)))
    )

    (defun all-gangsters ()
        @doc "Returns all the ids"
        (keys nfts)
    )

    (defun calc-gangster-hp (gangsterId:string)
      @doc "Return gangster HP"
      (require-capability (PRIVATE))
      (with-capability (PRIVATE)
        (let* ((gangster-stats (get-gangster-attributes-for-fight gangsterId))
               (sum (fold gangster-stats 0 (lambda (accum value) (+ accum value)))))
          sum
        )
      )
    )

    ;;;;;; GENERIC HELPER FUNCTIONS ;;;;;;

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )
)

