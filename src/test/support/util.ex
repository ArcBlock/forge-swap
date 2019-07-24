defmodule ForgeSwapWebTest.Util do
  @doc """
  Generates the request for Did auth.
  """

  import ExUnit.Assertions

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

  def assert_common_auth_info(pk, auth_body, owner) do
    assert pk === Multibase.encode!(owner.pk, :base58_btc)

    assert auth_body["appInfo"] == %{
             "description" =>
               "A blockchain-based ticket-selling application that allows event hosts and participants to have full control of every ticket.",
             "name" => "Event Chain"
           }

    assert auth_body["chanInfo"] == %{"host" => "http://127.0.0.1:8410/api"}
    assert auth_body["iss"] == "did:abt:#{owner.address}"
    assert not is_nil(auth_body["exp"])
    assert not is_nil(auth_body["iat"])
    assert not is_nil(auth_body["nbf"])
  end
end
