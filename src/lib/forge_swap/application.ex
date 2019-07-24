defmodule ForgeSwap.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  alias ForgeSwap.{Repo, PostgresRepo}
  alias ForgeSwap.Utils.Config, as: ConfigUtil

  def start(_type, _args) do
    config = ConfigUtil.read_config()
    ConfigUtil.enrich_chain_config()
    update_endpoint_config(config)
    repo = get_repo(config)

    # List all child processes to be supervised
    children = [
      # Start the Ecto repository
      repo,
      ForgeSwap.Swapper,
      # Start the endpoint when the application starts
      ForgeSwapWeb.Endpoint
      # Starts a worker by calling: ForgeSwap.Worker.start_link(arg)
      # {ForgeSwap.Worker, arg},
    ]

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

  defp get_repo(config) do
    case config["database"]["type"] do
      "postgres" ->
        Repo.set_module(PostgresRepo)
        PostgresRepo

      _ ->
        raise "Not supported database type: #{config["database"]["type"]}"
    end
  end

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

  defp prepare_wallet(:dev) do
    ForgeSdk.connect("tcp://127.0.0.1:10020", name: "asset_chain", default: true)
    ForgeSdk.connect("tcp://127.0.0.1:10120", name: "app_chain", default: false)
    wallet = ConfigUtil.read_config()["asset_owners"]["default"]
    itx = apply(ForgeAbi.DeclareTx, :new, [[moniker: "owner"]])
    ForgeSdk.declare(itx, wallet: wallet, chain_name: "asset_chain") |> IO.inspect()
    ForgeSdk.declare(itx, wallet: wallet, chain_name: "app_chain") |> IO.inspect()
  end

  defp prepare_wallet(_), do: :ok
end
