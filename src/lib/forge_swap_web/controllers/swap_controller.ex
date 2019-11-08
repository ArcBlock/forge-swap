defmodule ForgeSwapWeb.SwapController do
  use ForgeSwapWeb, :controller

  alias ForgeSwapWeb.Plugs.ReadSwap

  plug(ReadSwap)

  @doc """
  Shows the status of a swap. The displayed live view page will get updated 
  automatically when the swap status is changed.
  """
  def show(conn, _) do
    swap = conn.assigns.swap
    live_render(conn, ForgeSwapWeb.SwapLive, session: %{id: swap.id, status: swap.status})
  end

  def get(conn, _) do
    result =
      conn.assigns.swap
      |> Map.from_struct()
      |> Map.delete(:__meta__)
      |> Map.update!(:offer_token, &Decimal.to_integer/1)
      |> Map.update!(:demand_token, &Decimal.to_integer/1)

    json(conn, result)
  end
end
