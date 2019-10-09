defmodule ForgeSwap.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  alias ForgeSwapWeb.Endpoint, as: SwapEndpoint
  alias ForgeSwapManageWeb.Endpoint, as: ManageEndpoint
  alias ForgeSwap.Swapper.{Setupper, Retriever, Revoker}
  alias ForgeSwap.Utils.Config, as: ConfigUtil
  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Tx, as: TxUtil

  def start(_type, _args) do
    ArcConfig.read_config(:forge_swap)
    ArcConfig.update_config(:forge_swap, ["asset_owners"], &ConfigUtil.parse_wallets/1)
    ArcConfig.update_config(:forge_swap, ["chains"], &ConfigUtil.enrich_chain_config/1)
    ConfigUtil.apply_endpoint_config(ArcConfig.read_config(:forge_swap)["service"], SwapEndpoint)
    ConfigUtil.apply_endpoint_config(ArcConfig.read_config(:forge_swap)["manage"], ManageEndpoint)

    repo = ConfigUtil.apply_repo_config()

    children =
      [repo, SwapEndpoint, ManageEndpoint] ++
        get_swapper(Application.get_env(:forge_swap, :env))

    opts = [strategy: :one_for_one, name: ForgeSwap.Supervisor]
    result = Supervisor.start_link(children, opts)
    prepare_wallet(Application.get_env(:forge_swap, :env))
    result
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    SwapEndpoint.config_change(changed, removed)
    :ok
  end

  defp get_swapper(:test), do: []
  defp get_swapper(_), do: [Setupper, Retriever, Revoker]

  defp prepare_wallet(:test), do: :ok

  defp prepare_wallet(_) do
    :forge_swap
    |> ArcConfig.read_config()
    |> Map.get("asset_owners")
    |> Enum.each(fn {moniker, wallet} -> prepare_wallet(moniker, wallet) end)
  end

  defp prepare_wallet(moniker, wallet) do
    :forge_swap
    |> ArcConfig.read_config()
    |> Map.get("chains")
    |> Enum.each(&prepare_wallet(&1, moniker, wallet))
  end

  defp prepare_wallet({chain_name, _}, moniker, wallet) do
    if ChainUtil.get_account_state(wallet.address, chain_name) == nil do
      TxUtil.declare_wallet(moniker, wallet, chain_name)
    end
  end
end
