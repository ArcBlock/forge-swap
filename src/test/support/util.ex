defmodule ForgeSwapWebTest.Util do
  @doc """
  Generates the request for Did auth.
  """

  import ExUnit.Assertions

  alias ForgeSwap.Schema.Swap

  @app_did "zNKipquLJtew2CowKBFEmsZBd99s9LmgyDKP"
  @app_pk "zEY3RfiTmbQLtEP393c2FJ2YrQpqmdZLRS33wRg5TrVSe"

  def gen_signed_request(w, extra) do
    user_info = AbtDid.Signer.gen_and_sign(w.address, w.sk, extra)

    %{
      userPk: Multibase.encode!(w.pk, :base58_btc),
      userInfo: user_info
    }
  end

  def get_auth_body(auth_info) do
    auth_info
    |> String.split(".")
    |> Enum.at(1)
    |> Base.url_decode64!(padding: false)
    |> Jason.decode!()
  end

  def assert_common_auth_info(pk, auth_body) do
    assert pk === @app_pk

    assert auth_body["appInfo"] == %{
             "description" =>
               "A blockchain-based ticket-selling application that allows event hosts and participants to have full control of every ticket.",
             "name" => "Event Chain"
           }

    assert auth_body["chainInfo"]["host"] == "http://localhost:8410/api/"
    assert auth_body["iss"] == "did:abt:#{@app_did}"
    assert not is_nil(auth_body["exp"])
    assert not is_nil(auth_body["iat"])
    assert not is_nil(auth_body["nbf"])
  end

  def create_swap(params) do
    config = ArcConfig.read_config(:forge_swap)
    user_did = params["userDid"]
    asset_owner = params["assetOwner"] || "default"

    offer_chain = params["offerChain"] || "application"
    offer_assets = params["offerAssets"] || []
    offer_token = params["offerToken"] || 0
    offer_chain_config = config["chains"][offer_chain]
    offer_locktime = params["offerLocktime"] || offer_chain_config["offer_locktime"]

    demand_chain = params["demandChain"] || "asset"
    demand_assets = params["demandAssets"] || []
    demand_token = params["demandToken"] || 0
    demand_chain_config = config["chains"][demand_chain]
    demand_locktime = params["demandLocktime"] || demand_chain_config["demand_locktime"]

    Swap.insert_changeset(%{
      user_did: user_did,
      asset_owner: asset_owner,
      status: "not_started",
      offer_assets: offer_assets,
      offer_token: offer_token,
      offer_chain: offer_chain,
      offer_locktime: offer_locktime,
      demand_assets: demand_assets,
      demand_token: demand_token,
      demand_chain: demand_chain,
      demand_locktime: demand_locktime
    })
  end
end
