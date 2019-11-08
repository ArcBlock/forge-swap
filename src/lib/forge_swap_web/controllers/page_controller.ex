defmodule ForgeSwapWeb.PageController do
  use ForgeSwapWeb, :controller

  alias ForgeSwap.Utils.Chain, as: ChainUtil

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def config(conn, _) do
    config = ArcConfig.read_config(:forge_swap)

    payload = %{
      serviceInfo: config["service"],
      appInfo: config["hyjal"]["application"] |> Map.delete("sk") |> Map.delete("pk"),
      offerChainInfo: config["chains"]["application"],
      demandChainInfo: config["chains"]["asset"],
      offerChainToken: ChainUtil.get_forge_state("application")["token"],
      demandChainToken: ChainUtil.get_forge_state("asset")["token"],
    }

    json(conn, payload)
  end
end
