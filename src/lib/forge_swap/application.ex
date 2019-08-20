defmodule ForgeSwap.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  alias ForgeSwap.Swapper.{Setupper, Retriever, Revoker}
  alias ForgeSwap.Utils.Config, as: ConfigUtil

  def start(_type, _args) do
    ArcConfig.read_config(:forge_swap)
    ArcConfig.update_config(:forge_swap, ["asset_owners"], &ConfigUtil.parse_wallets/1)
    ArcConfig.update_config(:forge_swap, ["chains"], &ConfigUtil.enrich_chain_config/1)
    ConfigUtil.apply_endpoint_config()
    repo = ConfigUtil.apply_repo_config()

    children =
      [repo, ForgeSwapWeb.Endpoint] ++
        get_swapper(Application.get_env(:forge_swap, :env))

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

  defp prepare_wallet() do
  end

  defp prepare_wallet(:dev) do
    ForgeSdk.connect("tcp://127.0.0.1:10020", name: "asset_chain", default: true)
    ForgeSdk.connect("tcp://127.0.0.1:10120", name: "app_chain", default: false)
    wallet = ArcConfig.read_config(:forge_swap)["asset_owners"]["default"]
    itx = apply(ForgeAbi.DeclareTx, :new, [[moniker: "owner"]])
    ForgeSdk.declare(itx, wallet: wallet, conn: "asset_chain", send: :commit)
  end

  defp prepare_wallet(_), do: :ok
end
