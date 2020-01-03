defmodule ForgeSwap.Utils.Config do
  alias ForgeSwap.{Repo, PostgresRepo}
  alias ForgeSwap.Utils.Util
  alias ForgeSwap.Utils.Chain, as: ChainUtil

  # Add chain id to the chain config.
  def enrich_chain_config(chains) do
    env = Application.get_env(:forge_swap, :env)

    chains
    |> Enum.map(&do_enrich_chain_config(env, &1))
    |> Enum.into(%{}, fn {k, v} -> {k, v} end)
  end

  defp do_enrich_chain_config(:test, {chain_name, chain_config}) do
    chain_config =
      case chain_name do
        "asset" -> Map.put(chain_config, "chain_id", "asset_chain")
        "application" -> Map.put(chain_config, "chain_id", "app_chain")
        _ -> chain_config
      end

    {chain_name, chain_config}
  end

  defp do_enrich_chain_config(_, {chain_name, chain_config}) do
    info = ChainUtil.get_chain_info(chain_name)

    if "fg:t:setup_swap" not in info["supportedTxs"] or
         "fg:t:retrieve_swap" not in info["supportedTxs"] or
         "fg:t:revoke_swap" not in info["supportedTxs"] do
      raise "The chain #{chain_name} does not support atomic swap."
    else
      config =
        chain_config
        |> Map.put("chain_id", info["network"])

      {chain_name, config}
    end
  end

  def parse_wallets(owners) do
    owners
    |> Map.to_list()
    |> Enum.into(%{}, fn {moniker, owner} ->
      {moniker,
       ForgeAbi.WalletInfo.new(
         address: owner["address"],
         pk: Util.str_to_bin(owner["pk"]),
         sk: Util.str_to_bin(owner["sk"])
       )}
    end)
  end

  def apply_endpoint_config(config, endpoint_module) do
    schema = config["schema"]
    host = config["host"]
    port = config["port"]
    path = config["path"]
    endpoint = Application.get_env(:forge_swap, endpoint_module)
    endpoint = Keyword.put(endpoint, :url, host: host, port: port, path: path)

    endpoint =
      case schema do
        "https" ->
          Keyword.update(endpoint, :https, [port: port], fn v -> Keyword.put(v, :port, port) end)

        "http" ->
          Keyword.update(endpoint, :http, [port: port], fn v -> Keyword.put(v, :port, port) end)
      end

    Application.put_env(:forge_swap, endpoint_module, endpoint)
  end

  def apply_repo_config() do
    config = ArcConfig.read_config(:forge_swap)
    repo = get_repo(config)
    db_config = config["database"]

    repo_config =
      :forge_swap
      |> Application.get_env(repo)
      |> update_keyword(:username, db_config["username"])
      |> update_keyword(:password, db_config["password"])
      |> update_keyword(:database, db_config["database"])
      |> update_keyword(:hostname, db_config["hostname"])

    Application.put_env(:forge_swap, repo, repo_config)
    repo
  end

  defp get_repo(config) do
    case config["database"]["type"] do
      "postgres" ->
        Repo.set_module(PostgresRepo)
        PostgresRepo

      _ ->
        raise "Not supported database type: #{config["database"]["type"]}"
    end
  end

  defp update_keyword(list, _key, nil), do: list
  defp update_keyword(list, _key, ""), do: list
  defp update_keyword(list, key, value), do: Keyword.put(list, key, value)
end
