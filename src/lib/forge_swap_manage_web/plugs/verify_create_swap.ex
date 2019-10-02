defmodule ForgeSwapManageWeb.Plugs.VerifyCreateSwap do
  @moduledoc false

  import Plug.Conn
  import Phoenix.Controller

  def init(_) do
  end

  def call(%Plug.Conn{body_params: body_params} = conn, _) do
    config = ArcConfig.read_config(:forge_swap)
    offer_chain = body_params["offerChain"] || "application"
    demand_chain = body_params["demandChain"] || "asset"
    offer_chain_config = config["chains"][offer_chain]
    demand_chain_config = config["chains"][demand_chain]

    cond do
      offer_chain_config == nil ->
        conn |> json(%{error: "Does not support chain #{offer_chain}"}) |> halt()

      demand_chain_config == nil ->
        conn |> json(%{error: "Does not support chain #{demand_chain}"}) |> halt()

      true ->
        conn
    end
  end
end
