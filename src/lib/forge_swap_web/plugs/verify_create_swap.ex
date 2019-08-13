defmodule ForgeSwapWeb.Plugs.VerifyCreateSwap do
  @moduledoc false

  import Plug.Conn
  import Phoenix.Controller

  alias ForgeSwap.Utils.Config, as: ConfigUtil

  def init(_) do
  end

  def call(%Plug.Conn{body_params: body_params} = conn, _) do
    config = ConfigUtil.read_config()
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
