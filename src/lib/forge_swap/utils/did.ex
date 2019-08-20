defmodule ForgeSwap.Utils.Did do
  @moduledoc """
  Contains the utilities to do the DID authentication.
  """

  alias ForgeSwap.Utils.Util

  def parse_user_info(user_info) do
    result =
      user_info
      |> String.split(".")
      |> Enum.at(1)
      |> Base.url_decode64!(padding: false)
      |> Jason.decode!()

    result
    |> Map.put("iss", did_to_address(result["iss"]))
    |> Map.put("iat", Util.str_to_int(result["iat"]))
    |> Map.put("nbf", Util.str_to_int(result["nbf"]))
    |> Map.put("exp", Util.str_to_int(result["exp"]))
  end

  def did_to_address("did:abt:" <> address), do: address
  def did_to_address(address), do: address

  def require_user_did(swap) do
    %{
      type: "did",
      didType: "account",
      meta: %{
        description: "Please proof you won DID #{swap.user_did} before start.",
        target: "#{swap.user_did}"
      }
    }
  end

  def require_user_set_up(swap) do
    config = ArcConfig.read_config(:forge_swap)
    chain_config = config["chains"][swap.demand_chain]
    owner = config["asset_owners"][swap.asset_owner]

    %{
      type: "swap",
      offerAssets: swap.offer_assets,
      offerToken: Decimal.to_integer(swap.offer_token),
      demandAssets: swap.demand_assets,
      demandToken: Decimal.to_integer(swap.demand_token),
      demandLocktime: swap.demand_locktime,
      receiver: owner.address,
      demandChain: chain_config["chain_id"],
      meta: %{
        description: "Please set up an atomic swap on the ABT asset chain."
      }
    }
  end

  def prepare_extra_response(claims, url, offer_chain) do
    offer_chain
    |> get_general_response()
    |> Map.put(:requestedClaims, claims)
    |> Map.put(:url, url)
  end

  def get_general_response(offer_chain) do
    config = ArcConfig.read_config(:forge_swap)
    app_config = config["application"]
    chain_config = config["chains"][offer_chain]

    %{
      appInfo: %{
        name: app_config["name"],
        description: app_config["description"]
      },
      chainInfo: %{
        host: "#{chain_config["host"]}:#{chain_config["port"]}/api/"
      }
    }
  end

  def gen_and_sign_response!(extra) do
    config = ArcConfig.read_config(:forge_swap)
    sk = Util.str_to_bin(config["application"]["sk"])
    pk = config["application"]["pk"] |> Util.str_to_bin() |> Multibase.encode!(:base58_btc)
    did = config["application"]["did"]

    %{
      appPk: pk,
      authInfo: AbtDid.Signer.gen_and_sign(did, sk, extra)
    }
  end
end
