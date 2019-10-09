defmodule ForgeSwapWeb.SwapController do
  use ForgeSwapWeb, :controller

  alias ForgeSwapWeb.Plugs.ReadSwap

  plug(ReadSwap when action == :show)

  @doc """
  Shows the status of a swap. The displayed live view page will get updated 
  automatically when the swap status is changed.
  """
  def show(conn, _) do
    swap = conn.assigns.swap
    live_render(conn, ForgeSwapWeb.SwapLive, session: %{id: swap.id, status: swap.status})
  end
end
