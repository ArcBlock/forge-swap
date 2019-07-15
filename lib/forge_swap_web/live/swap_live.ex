defmodule ForgeSwapWeb.SwapLive do
  use Phoenix.LiveView

  alias ForgeSwap.Schema.Swap
  alias ForgeSwap.Utils.Util

  def render(assigns) do
    ForgeSwapWeb.SwapView.render("swap_status.html", assigns)
  end

  def mount(session, socket) do
    if connected?(socket) do
      :timer.send_interval(1000, self(), :tick)
    end

    swap = Swap.get(session.id)

    {:ok,
     assign(socket,
       id: swap.id,
       status: swap.status,
       qr_code: Util.gen_start_swap_qr_code(swap.id)
     )}
  end

  def handle_info(:tick, socket) do
    swap = Swap.get(socket.assigns.id)
    {:noreply, assign(socket, :status, swap.status)}
  end
end
