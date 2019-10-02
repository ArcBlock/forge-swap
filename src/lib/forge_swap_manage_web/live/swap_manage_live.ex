defmodule ForgeSwapManageWeb.SwapManageLive do
  use Phoenix.LiveView

  alias ForgeSwap.Schema.Swap

  def render(assigns) do
    IO.inspect(assigns, label: "@@@")

    ForgeSwapManageWeb.SwapManageView.render("index.html", assigns)
  end

  def mount(%{swaps: swaps}, socket) do
    if connected?(socket) do
      :timer.send_interval(1000, self(), :tick)
    end

    {:ok, assign(socket, :swaps, swaps)}
  end

  def handle_info(:tick, socket) do
    swaps = Swap.get_by_status("not_started") |> IO.inspect(label: "##")
    {:noreply, assign(socket, :swaps, swaps)}
  end
end
