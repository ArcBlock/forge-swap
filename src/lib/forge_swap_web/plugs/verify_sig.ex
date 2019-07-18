defmodule ForgeSwapWeb.Plugs.VerifySig do
  @moduledoc false

  import Plug.Conn
  import Phoenix.Controller

  alias ForgeSwap.Utils.Util

  def init(_) do
  end

  def call(%Plug.Conn{body_params: %{"userPk" => user_pk, "userInfo" => user_info}} = conn, _) do
    pk_bin = Util.str_to_bin(user_pk)

    case AbtDid.Signer.verify(user_info, pk_bin) do
      true ->
        conn

      _ ->
        conn
        |> json(%{error: "The signature of the user info does not match the public key."})
        |> halt()
    end
  end

  def call(conn, _) do
    conn
    |> json(%{error: "Request must have userPk and userInfo."})
    |> halt()
  end
end
