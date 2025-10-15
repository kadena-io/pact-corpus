(module favorites GOVERNANCE
@doc "Module for managing user favorite tokens with limit per user"

  (defconst FAVORITES_LIMIT 20)

  (defcap GOVERNANCE ()
  @doc "Governance capability"
    (enforce-keyset "n_b9880e372b7ec49d21ef4afe82ece29c3a536bde.admin-keyset"))

  (defcap ACCOUNT-OWNER (account:string)
  @doc "Ensures only account owner can modify their favorites"
    (enforce-guard (at 'guard (coin.details account))))

  (defschema favorites-schema
  @doc "Favorites storage schema"
    tokens:[string])

  (deftable user-favorites:{favorites-schema})

  (defun add-to-favorites (account:string tokenId:string)
  @doc "Add token to favorites"
    (with-capability (ACCOUNT-OWNER account)
      (with-default-read user-favorites account
        {"tokens": []}
        {"tokens" := current-tokens}
        (enforce (< (length current-tokens) FAVORITES_LIMIT)
          (format "Favorites limited to {}" [FAVORITES_LIMIT]))
        (enforce (not (contains tokenId current-tokens))
          "Token already in favorites")
        (write user-favorites account
          {"tokens": (+ current-tokens [tokenId])}))))

  (defun remove-from-favorites (account:string tokenId:string)
  @doc "Remove token from favorites"
    (with-capability (ACCOUNT-OWNER account)
      (with-read user-favorites account {"tokens" := current-tokens}
        (write user-favorites account
          {"tokens": (filter (!= tokenId) current-tokens)}))))

  (defun get-user-favorites (account:string)
  @doc "Get user's favorite tokens"
    (read user-favorites account))

  (defun is-favorite:bool (account:string tokenId:string)
  @doc "Check if token is in user's favorites"
    (contains tokenId (at 'tokens (read user-favorites account))))
)


