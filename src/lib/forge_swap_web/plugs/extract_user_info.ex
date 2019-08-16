defmodule ForgeSwapWeb.Plugs.ExtractUserInfo do
  @moduledoc false

  import Plug.Conn
  import Phoenix.Controller

  alias ForgeSwap.Utils.Util
  alias ForgeSwap.Utils.Did, as: DidUtil

  use Plug.Builder

  def init(_) do
  end

  def call(%Plug.Conn{body_params: %{"userPk" => user_pk, "userInfo" => user_info}} = conn, _) do
    pk_bin = Util.str_to_bin(user_pk)
    body = DidUtil.parse_user_info(user_info)
    now = DateTime.utc_now() |> DateTime.to_unix()

    conn
    |> assign(:user, %{address: body["iss"], pk: pk_bin})
    |> assign(:claims, Map.get(body, "requestedClaims", []))
    |> assign(:params, Map.get(body, "params", %{}))
    |> check_iat(body, now)
    |> check_nbf(body, now)
    |> check_exp(body, now)
  end

  def call(conn, _) do
    conn
    |> json(%{error: "Request must have userPk and userInfo."})
    |> halt()
  end

  defp check_iat(%{halted: true} = conn, _, _), do: conn

  defp check_iat(conn, %{"iat" => iat}, _) when is_nil(iat),
    do: conn |> json(%{error: "Must specify 'iat' in user info."}) |> halt()

  defp check_iat(conn, %{"iat" => iat}, now) do
    if iat > now - 300 do
      assign(conn, :iat, iat)
    else
      conn |> json(%{error: "The request must be issued within 5 minutes."}) |> halt()
    end
  end

  defp check_nbf(%{halted: true} = conn, _, _), do: conn

  defp check_nbf(conn, %{"nbf" => nbf}, _) when is_nil(nbf),
    do: conn |> json(%{error: "Must specify 'nbf' in user info."}) |> halt()

  defp check_nbf(conn, %{"nbf" => nbf}, now) do
    if now >= nbf do
      assign(conn, :nbf, nbf)
    else
      conn
      |> json(%{error: "The request is not valid before #{inspect(DateTime.from_unix!(nbf))}"})
      |> halt()
    end
  end

  defp check_exp(%{halted: true} = conn, _, _), do: conn

  defp check_exp(conn, %{"exp" => exp}, _) when is_nil(exp),
    do: conn |> json(%{error: "Must specify 'exp' in user info."}) |> halt()

  defp check_exp(conn, %{"exp" => exp}, now) do
    if now <= exp do
      assign(conn, :exp, exp)
    else
      conn
      |> json(%{error: "The request is expired at #{inspect(DateTime.from_unix!(exp))}"})
      |> halt()
    end
  end
end
