defmodule ForgeSwap.Utils.Config do
  alias ForgeSwap.Utils.Chain, as: ChainUtil

  def enrich_chain_config() do
    config = read_config()

    chains =
      config["chains"]
      |> Enum.map(&do_enrich_chain_config(&1))
      |> Enum.into(%{}, fn {k, v} -> {k, v} end)

    config = Map.put(config, "chains", chains)
    :ets.insert(:forge_swap, {:config, config})
  end

  defp do_enrich_chain_config({chain_name, chain_config}) do
    info = ChainUtil.get_chain_info(chain_name)
    # state = ChainUtil.get_forge_state(chain_name)

    if "fg:t:setup_swap" not in info["supportedTxs"] or
         "fg:t:retrieve_swap" not in info["supportedTxs"] or
         "fg:t:revoke_swap" not in info["supportedTxs"] do
      raise "The chain #{chain_name} does not support atomic swap."
    else
      config =
        chain_config
        |> Map.put("chain_id", info["network"])

      # |> Map.put("token", state["token"])

      {chain_name, config}
    end
  end

  def read_config() do
    if :ets.whereis(:forge_swap) == :undefined do
      :ets.new(:forge_swap, [:named_table])
    end

    case :ets.lookup(:forge_swap, :config) do
      [{:config, config}] ->
        config

      _ ->
        config = do_read_config()
        :ets.insert(:forge_swap, {:config, config})
        config
    end
  end

  defp do_read_config() do
    default =
      :forge_swap |> Application.app_dir() |> Path.join("priv/config/default.toml") |> read_toml()

    change =
      case System.get_env("FORGESWAP_CONFIG") do
        nil -> %{}
        path -> read_toml(path)
      end

    default |> Map.merge(change) |> parse()
  end

  # Traverse the config map and replace values by system environment variables.
  defp parse(config) when is_map(config) do
    config
    |> Map.to_list()
    |> Enum.into(%{}, fn
      {"asset_owners", asset_owners} -> {"asset_owners", parse_wallets(asset_owners)}
      {key, val} -> {key, parse(val)}
    end)
  end

  defp parse("SYSTEM:" <> env) do
    case System.get_env(env) do
      val when val in [nil, ""] ->
        raise "Could not read system environment variable #{inspect(env)}."

      val ->
        val
    end
  end

  defp parse(val), do: val

  defp parse_wallets(owners) do
    owners
    |> Map.to_list()
    |> Enum.into(%{}, fn {moniker, owner} ->
      {moniker,
       ForgeAbi.WalletInfo.new(
         address: owner["address"],
         pk: Base.decode16!(owner["pk"], case: :mixed),
         sk: Base.decode16!(owner["sk"], case: :mixed)
       )}
    end)
  end

  defp read_toml(path) do
    path
    |> File.read!()
    |> Toml.decode!()
  end
end
