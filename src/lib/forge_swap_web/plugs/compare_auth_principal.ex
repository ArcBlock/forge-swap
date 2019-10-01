defmodule ForgeSwapWeb.Plugs.CompareAuthPrincipal do
  @moduledoc false

  import Plug.Conn
  import Phoenix.Controller

  def init(_) do
  end

  def call(conn, _) do
    %{auth_principal: auth_principal, swap: swap} = conn.assigns

    case auth_principal.address === swap.user_did do
      true -> conn
      _ -> conn |> json(%{error: "Invalid authentication principal."}) |> halt()
    end
  end
end
