defmodule ForgeSwapWeb.SwapLive do
  use Phoenix.LiveView

  alias ForgeSwap.Schema.Swap
  alias ForgeSwap.Utils.Util

  def render(assigns) do
    ForgeSwapWeb.SwapView.render("swap_status.html", assigns)
  end

  def mount(%{id: id}, socket) do
    if connected?(socket) do
      :timer.send_interval(1000, self(), :tick)
    end

    swap = Swap.get(id)

    assigns =
      swap.status
      |> determine_display()
      |> Map.put(:id, swap.id)
      |> Map.put(:qr_code, Util.gen_qr_code(swap))
      |> Map.to_list()

    {:ok, assign(socket, assigns)}
  end

  def handle_info(:tick, socket) do
    swap = Swap.get(socket.assigns.id)

    assigns =
      swap.status
      |> determine_display()
      |> Map.put(:qr_code, Util.gen_qr_code(swap))
      |> Map.to_list()

    {:noreply, assign(socket, assigns)}
  end

  defp determine_display(status) do
    display = %{
      display_not_started: "none",
      display_user_deposited: "none",
      display_both_deposited: "none",
      display_user_retrieved: "none"
    }

    key = String.to_atom("display_" <> status)
    Map.put(display, key, "block")
  end
end
