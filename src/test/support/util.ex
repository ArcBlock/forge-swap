defmodule ForgeSwapWebTest.Util do
  @doc """
  Generates the request for Did auth.
  """
  def gen_signed_request(w, extra) do
    user_info = AbtDid.Signer.gen_and_sign(w.address, w.sk, extra)

    %{
      userPk: Multibase.encode!(w.pk, :base58_btc),
      userInfo: user_info
    }
  end

  def get_auth_body(auth_info) do
    auth_info
    |> String.split(".")
    |> Enum.at(1)
    |> Base.url_decode64!(padding: false)
    |> Jason.decode!()
  end
end
