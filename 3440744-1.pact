(module kitty-kad-kitties 'kitty-kad
  "Kitty Kad Kitties NFTs game"

    (defconst KITTIES_CREATED_COUNT_KEY "kitties-count-key")
    (defconst KITTIES_ADOPTED_COUNT_KEY "kitties-adopted-count-key")
    (defconst KITTIES_CREATED_TO_ADOPT_COUNT_KEY "kitties-created-to-adopt-count-key")
    (defconst KITTIES_URI_KEY "kitties-uri-key")
    (defconst MINT_CHAIN_ID_KEY "mint-chain-id-key")
    (defconst CURR_WL_ROLE_KEY "curr-wl-role-key")
    (defconst WL_PREMIUM_ROLE "wl-premium-role")
    (defconst WL_SECONDARY_ROLE "wl-secondary-role")
    (defconst WL_ALL_ROLE "wl-all-role")
    (defconst PRICE_KEY "price-key")
    (defconst BITS_PER_GENE 30)
    (defconst MAX_GENE_SIZE 1073741823)
    (defconst AMOUNT_OF_GENES 15)
    (defconst AMOUNT_OF_ITEMS 15)
    (defconst ADMIN_KEYSET (read-keyset 'kitty-kad))
    (defconst ADMIN_ADDRESS "k:f7278eeaa55a4b52c281fa694035f82a43a6711eb547fc1ab900be1ccf9fb409")

    (defcap PRIVATE () 
        @doc "can only be called from a private context"
        true
    ) 

    (defcap ACCOUNT_GUARD(account:string) ; Used for admin functions
        @doc "Verifies account meets format and belongs to caller"
        (enforce (= "k:" (take 2 account)) "For security, only support k: accounts")
        (enforce-guard   
            (at "guard" (coin.details account))
        )
    )

    (defcap OWNER (account:string id:string)
        @doc "Enforces that an account owns a kitty kad"
        (let 
            (
                (nft-owner (at "owner" (read nfts id ["owner"])))
            )
            (enforce (= nft-owner account) "Account is not owner of the NFT")
            (compose-capability (ACCOUNT_GUARD account))
        )
    )

    (defcap ADMIN() ; Used for admin functions
        @doc "Only allows admin to call these"
        (enforce-keyset  ADMIN_KEYSET)
        (compose-capability (PRIVATE))
        (compose-capability (ACCOUNT_GUARD ADMIN_ADDRESS))
    )

    (defun initialize ()
        @doc "Initialize the contract the first time its loaded "
        (insert counts KITTIES_CREATED_COUNT_KEY {"count": 0})
        (insert counts KITTIES_ADOPTED_COUNT_KEY {"count": 0})
        (insert counts KITTIES_CREATED_TO_ADOPT_COUNT_KEY {"count": 0})
        (insert values MINT_CHAIN_ID_KEY {"value": "1"})
        (insert values KITTIES_URI_KEY {"value": "kittykad.com/"})
        (insert price PRICE_KEY {"price": 1.0})
        (insert values CURR_WL_ROLE_KEY {"value": WL_PREMIUM_ROLE})
    )

    ;;;; SCHEMAS AND TABLES ;;;;;

    (defschema nft-main-schema
        @doc "Stores core information about each nft"
        id:string
        parent-1-id:string
        parent-2-id:string
        generation:integer
        birthday:time
        next-breed-time:time
        gene-pairs:list
        item-pairs:list
        name:string
        owner:string
    )

    (defschema gen-0-adopt-schema
        @doc "Gen 0s for adoption traits"
        gene-pairs:list
        item-pairs:list
        adopted:bool
    )

    (defschema counts-schema
        @doc "Basic schema used for counting things"
        count:integer
    )

    (defschema values-schema
        @doc "Basic schema used for storing basic values"
        value:string
    )

    (defschema wl-schema
        @doc "Basic schema used for WL members, keys are account ids"
        role:string
    )

    (defschema price-schema
        @doc "Prices schema"
        price:decimal
    )

    (deftable nfts:{nft-main-schema})
    (deftable gen-0-adopt:{gen-0-adopt-schema})
    (deftable counts:{counts-schema})
    (deftable values:{values-schema})
    (deftable wl:{wl-schema})
    (deftable price:{price-schema})


    ;;; STATE MODIFYING FUNCTIONS, REQUIRE CAPABILITIES ;;;
    (defun create-multiple-gen-0-for-adoption (gene-item-pairs-list:list)
        @doc "Take a list of gene-item pairs, create gen0s for adoptions "
        (with-capability (ADMIN)
            (map 
                (create-gen-0-for-adoption)
                gene-item-pairs-list
            )
        )
    )

    (defun create-gen-0-for-adoption (gene-item-pairs:list)
        (require-capability (ADMIN))
        (let 
            ((gene-pairs (at 0 gene-item-pairs)) (item-pairs (at 1 gene-item-pairs)))
            (enforce-genes-and-items gene-pairs item-pairs)
            (let 
                ((id (int-to-str 10(get-count KITTIES_CREATED_TO_ADOPT_COUNT_KEY))))
                (insert gen-0-adopt id
                    {"gene-pairs":gene-pairs, "item-pairs":item-pairs, "adopted": false}
                )
            )
            (increase-count KITTIES_CREATED_TO_ADOPT_COUNT_KEY)
        )
    )

    (defun adopt-gen-0s-bulk (owner:string amount:integer )
        @doc "Mints a gen 0 kitty"
        (enforce (>= amount 1) "Must mint at least one kitty")
        (enforce-adopt-wl-role owner)
        (let (
                (kitties-adopted (get-count KITTIES_ADOPTED_COUNT_KEY))
                (kitties-created-to-adopt (get-count KITTIES_CREATED_TO_ADOPT_COUNT_KEY))
            )
            (enforce (<= (+ kitties-adopted amount) kitties-created-to-adopt) "Tried to adopt more kitties then available! Please reduce the amount")
        )
        (if 
            (!= owner ADMIN_ADDRESS)
            (coin.transfer owner ADMIN_ADDRESS (* (get-price) amount))
            "Admin account"
        )
        (with-capability (ACCOUNT_GUARD owner)
            (with-capability (PRIVATE)
                (map
                    (adopt-gen-0 owner) 
                    (make-list amount 1)
                )
            )
        )
    )

    (defun adopt-gen-0 (owner:string number:integer)
        @doc "Mints a gen 0 kitty"
        (enforce (= number 1) "Number enforced to be 1 to avoid confusion but allow mapping to work")
        (require-capability (PRIVATE))
        (require-capability (ACCOUNT_GUARD owner))
        (let (
                (id (id-for-new-kitty))
                (data (get-latest-gen-0-to-adopt-data) )
            )
            (mint-kitty id {
                "id": id,
                "parent-1-id": "", 
                "parent-2-id": "", 
                "generation": 0,
                "birthday": (at "block-time" (chain-data)),
                "next-breed-time": (at "block-time" (chain-data)),
                "gene-pairs": (at "gene-pairs" data), 
                "item-pairs": (at "item-pairs" data), 
                "name": "",
                "owner": owner
            })
        )
        (increase-count KITTIES_ADOPTED_COUNT_KEY)
    )

    (defun mint-kitty (id:string data:object)
        @doc "Mints a new kitty"
        (require-capability (PRIVATE))
        (let ((mint-chain-id (get-value MINT_CHAIN_ID_KEY)))
            (enforce (= (curr-chain-id) mint-chain-id) "Can only mint on specific chain")
        )
        (enforce-genes-and-items (at "gene-pairs" data) (at "item-pairs" data))
        (let ((id (id-for-new-kitty)))
            (insert nfts id data)
        )
        (increase-count KITTIES_CREATED_COUNT_KEY)
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
        (with-capability (ADMIN)
            (update values key 
                {"value": value} 
            )
        )
    )

    (defun add-to-wl-for-role (role:string accounts:list )
        @doc "Adds wl users with a role"
        (enforce
            (
                or?
                (= WL_PREMIUM_ROLE)
                (= WL_SECONDARY_ROLE)
                role
            )
            "Must specify a valid role for adding WL members"
        )
        (with-capability (ADMIN)
            (map (add-to-wl role) accounts)
        )
    )

    (defun add-to-wl (role:string account:string )
        @doc "Adds a user to a wl"
        (require-capability (ADMIN))
        (insert wl account {"role": role})
    )

    (defun set-price(price-value:decimal)
        @doc "Set the price"
        (with-capability (ADMIN)
            (update price PRICE_KEY {"price": price-value})
        )
    )

    (defun transfer:string
        ( id:string
          sender:string
          receiver:string
          amount:decimal
        )
        @doc " Transfer to an account, failing if the account to account does not exist. "
        (enforce (= 1.0 amount) "Only 1 kitty kad can be transferred")
        (enforce-account-exists receiver)
        (enforce (= sender ADMIN_ADDRESS) "Can only send from admin account for now")
        (with-capability (ADMIN)
            (with-capability (OWNER sender id)
                (update nfts id {"owner": receiver})
            )
        )
    )

    ;;;;;; KITTY KAD NON STATE MODIFYING HELPER FUNCTIONS ;;;;;;;;;

    (defun get-price()
        (at "price" (read price PRICE_KEY ["price"]))
    )

    (defun get-wl-role (account:string)
        @doc "Gets current wl role for  the user"
        (try
            ""
            (at "role" (read wl account ["role"]))
        )
    )

    (defun get-count (key:string)
        @doc "Gets count for key"
        (at "count" (read counts key ['count]))
    )

    (defun get-value (key:string)
        @doc "Gets value for a key"
        (at "value" (read values key ['value]))
    )

    (defun get-latest-gen-0-to-adopt-data ()
        (let 
            (
                (adopted-count (get-count KITTIES_ADOPTED_COUNT_KEY))
                (created-to-adopt-count (get-count KITTIES_CREATED_TO_ADOPT_COUNT_KEY))
            )
            (enforce (< 0 created-to-adopt-count) "No kitties have been put up for adoption")
            (enforce 
                (< adopted-count created-to-adopt-count)
                 "All kitties put up for adoption have already been adopted, please check later"
            )
            (let 
                (
                    (data (read gen-0-adopt (int-to-str 10 adopted-count ) ['gene-pairs 'item-pairs 'adopted]))
                )
                (enforce (= (at "adopted" data) false) "All kitties have been adopted, that's unexpected")
                data
            )
        )
    )

    (defun enforce-genes-and-items (gene-pairs:list item-pairs:list)
        @doc "Enforces that the genes and items lengths are correct"
        (enforce (= (length gene-pairs) AMOUNT_OF_GENES) "Genes length is incorrect")
        (enforce (= (length item-pairs) AMOUNT_OF_ITEMS) "Items length is incorrect")
        (enforce-bits gene-pairs)
        (enforce-bits item-pairs)
    )

    (defun enforce-adopt-wl-role (account:string)
        @doc "Enforce the account has a role that allows them to adopt kitties"
        (let 
            (
                (curr_wl_role (get-value CURR_WL_ROLE_KEY))
                (user_wl_role (get-wl-role account))
            )
            (if 
                (= curr_wl_role WL_PREMIUM_ROLE)
                (enforce (= user_wl_role WL_PREMIUM_ROLE) "Only premium WL members allowed")
                true
            )
            (if 
                (= curr_wl_role WL_SECONDARY_ROLE)
                (enforce 
                    (or?
                        (= WL_PREMIUM_ROLE)
                        (= WL_SECONDARY_ROLE)
                        user_wl_role
                    ) 
                    "Only secondary and premium WL members allowed"
                )
                true
            )
        )    
    )

    (defun enforce-account-exists (account:string)
        @doc "Enforces that an account exists in the coin table"
        (let ((coin-account (at "account" (coin.details account))))
            (enforce (= coin-account account) "account was not found")
        )
    )

    (defun enforce-bits (pairs:list)
        @doc "Enforces that all the bits in the list are of valid size"
        (map (enforce-gene-length) pairs)
    )

    (defun enforce-gene-length (pair:list)
        @doc "Enforces that all the bits in the list are of valid size"
        (enforce (< (at 0 pair) MAX_GENE_SIZE) "A gene is larger than expected")
        (enforce (< (at 1 pair) MAX_GENE_SIZE) "A gene is larger than expected")

    )

    (defun kitties-owned-by (owner:string)
        @doc "All kitties owned by someone"
        (select nfts ["id"] (where "owner" (= owner)))
    )

    (defun get-kitty-fields-for-ids (fields:list ids:list) 
        @doc "Return fields for a list of ids"
        (map 
            (get-kitty-fields-for-id fields)
            ids
        )
    )

    (defun get-kitty-fields-for-id (fields:list id:string )
        @doc "Return the fields for a given id"
        (+ {"id": id} (read nfts id fields))
    )

    (defun uri:string (id:string)
        @doc
        " Give URI for ID. If not supported, return \"\" (empty string)."
        (+ (get-value KITTIES_URI_KEY) id)
    )

    (defun id-for-new-kitty ()
        @doc "Returns an id for a new kitty to be minted"
        (+ (+ (curr-chain-id) ":") (int-to-str 10 (get-count KITTIES_CREATED_COUNT_KEY)))
    )

    (defun all-kitties ()
        @doc "Returns all the ids"
        (keys nfts)
    )

    ;;;;;; GENERIC HELPER FUNCTIONS ;;;;;;;;;;

    (defun curr-chain-id ()
        @doc "Current chain id"
        (at "chain-id" (chain-data))
    )
)


