defmodule ForgeSwap.Utils.Did do
  @moduledoc """
  Contains the utilities to do the DID authentication.
  """

  alias ForgeSwap.Utils.Util
  alias ForgeSwap.Utils.Config, as: ConfigUtil

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

  def sign_response!(extra, asset_owner) do
    case ConfigUtil.read_config()["asset_owners"][asset_owner] do
      nil ->
        raise "Could not find asset owner #{asset_owner} in config files."

      owner ->
        address = owner["did"]
        pk = Base.decode16!(owner["pk"], case: :mixed)
        sk = Base.decode16!(owner["sk"], case: :mixed)

        %{
          appPk: Multibase.encode!(pk, :base58_btc),
          authInfo: AbtDid.Signer.gen_and_sign(address, sk, extra)
        }
    end
  end
end
