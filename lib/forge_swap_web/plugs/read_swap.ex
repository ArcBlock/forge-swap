defmodule ForgeSwapWeb.Plugs.ReadSwap do
  @moduledoc false

  import Plug.Conn
  import Phoenix.Controller

  alias ForgeSwap.Schema.Swap

  def init(_) do
  end

  def call(%{params: %{"id" => id}} = conn, _) do
    case Swap.get(id) do
      nil -> conn |> json(%{error: "Could not find swap, Id: #{id}"}) |> halt()
      swap -> assign(conn, :swap, swap)
    end
  end

  def call(conn, _) do
    conn
    |> json(%{error: "Request must have swap Id."})
    |> halt()
  end
end
