defmodule ForgeSwapWeb.Plugs.VerifyUser do
  @moduledoc false

  import Plug.Conn
  import Phoenix.Controller

  def init(_) do
  end

  def call(conn, _) do
    %{user: user, swap: swap} = conn.assigns

    case user.address === swap.user_did do
      true -> conn
      _ -> conn |> json(%{error: "Invalid user."}) |> halt()
    end
  end
end
