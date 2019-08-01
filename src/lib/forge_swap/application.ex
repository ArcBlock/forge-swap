defmodule ForgeSwap.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  alias ForgeSwap.{Repo, PostgresRepo}
  alias ForgeSwap.Swapper.{Setupper, Retriever, Revoker}
  alias ForgeSwap.Utils.Config, as: ConfigUtil

  def start(_type, _args) do
    config = ConfigUtil.read_config()
    ConfigUtil.enrich_chain_config()
    update_endpoint_config(config)
    repo = update_repo_config(config)

    children =
      [repo, ForgeSwapWeb.Endpoint] ++
        get_swapper(Application.get_env(:forge_swap, :env))

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: ForgeSwap.Supervisor]
    result = Supervisor.start_link(children, opts)
    prepare_wallet(Application.get_env(:forge_swap, :env))
    result
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    ForgeSwapWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp get_swapper(:test), do: []
  defp get_swapper(_), do: [Setupper, Retriever, Revoker]

  defp update_endpoint_config(config) do
    schema = config["service"]["schema"]
    host = config["service"]["host"]
    port = config["service"]["port"]
    endpoint = Application.get_env(:forge_swap, ForgeSwapWeb.Endpoint)
    endpoint = Keyword.put(endpoint, :url, host: host, port: port)

    endpoint =
      case schema do
        "https" ->
          Keyword.update(endpoint, :https, [port: port], fn v -> Keyword.put(v, :port, port) end)

        "http" ->
          Keyword.update(endpoint, :http, [port: port], fn v -> Keyword.put(v, :port, port) end)
      end

    Application.put_env(:forge_swap, ForgeSwapWeb.Endpoint, endpoint)
  end

  defp update_repo_config(config) do
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

  defp prepare_wallet(:dev) do
    ForgeSdk.connect("tcp://127.0.0.1:10020", name: "asset_chain", default: true)
    ForgeSdk.connect("tcp://127.0.0.1:10120", name: "app_chain", default: false)
    wallet = ConfigUtil.read_config()["asset_owners"]["default"]
    itx = apply(ForgeAbi.DeclareTx, :new, [[moniker: "owner"]])
    ForgeSdk.declare(itx, wallet: wallet, conn: "asset_chain", send: :commit)
  end

  defp prepare_wallet(_), do: :ok
end
