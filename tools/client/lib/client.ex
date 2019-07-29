defmodule Client do
  @moduledoc """
  Documentation for Client.
  """

  @ed25519 %Mcrypto.Signer.Ed25519{}
  @secp256k1 %Mcrypto.Signer.Secp256k1{}
  @localhost "localhost:4000"

  @doc """
  options:

  `:cross`: `true` means create the wallet on both workshop chain and remote forge chain.

  `:host`: The host of workshop.
  """
  def create_wallet(opts) do
    cross = Keyword.get(opts, :cross, false)
    host = Keyword.get(opts, :host, @localhost)

    request_body =
      case cross do
        false -> ""
        true -> Jason.encode!(%{cross_chain: true})
      end

    %HTTPoison.Response{body: body} =
      HTTPoison.post!(host <> "/api/wallet/recover", request_body, [
        {"content-type", "application/json"}
      ])

    w = Jason.decode!(body)

    ForgeAbi.WalletInfo.new(
      address: w["address"],
      pk: str_to_bin(w["pk"]),
      sk: str_to_bin(w["sk"])
    )
  end

  def wallet_state(address, host \\ @localhost) do
    %HTTPoison.Response{body: response} = HTTPoison.get!(host <> "/api/wallet/#{address}")

    Jason.decode!(response)
  end

  def request(w, tx_id, host \\ @localhost)

  def request(w, :cert, host) do
    url = host <> "/api/cert/issue?userDid=#{w.address}"
    do_request(w, url)
  end

  def request(w, tx_id, host) do
    pk = Multibase.encode!(w.pk, :base58_btc)
    url = host <> "/api/workflow/#{tx_id}?userDid=#{w.address}&userPk=#{pk}"

    do_request(w, url)
  end

  defp do_request(w, url) when is_binary(url) do
    %HTTPoison.Response{body: response} = HTTPoison.get!(url)
    response = Jason.decode!(response)
    handle_response(w, response)
  end

  defp handle_response(_, %{"error" => error}) do
    error
  end

  defp handle_response(w, %{"authInfo" => auth_info}) do
    info_body = get_body(auth_info) |> IO.inspect(label: "@@@")
    url = info_body["url"]
    claims = handle_claims(info_body["requestedClaims"], w)
    req = prepare_request(w, %{requestedClaims: claims})

    %HTTPoison.Response{body: body} =
      HTTPoison.post!(url, req, [{"Content-type", "application/json"}],
        timeout: 80000,
        recv_timeout: 80000
      )

    response =
      body
      |> Jason.decode!()

    handle_response(w, response)
  end

  defp handle_response(_w, param) do
    param
  end

  defp handle_claims(claims, wallet) do
    Enum.map(claims, &handle_claim(&1, wallet))
  end

  defp handle_claim(%{"type" => "signature", "tx" => tx_str} = claim, wallet) do
    display_claim(claim)
    tx_data = Multibase.decode!(tx_str)
    tx = ForgeAbi.Transaction.decode(tx_data)
    itx = ForgeAbi.decode_any(tx.itx)
    IO.inspect(tx)
    IO.inspect(itx)

    answer =
      IO.gets("Sign this transaction?\n") |> String.trim_trailing("\n") |> String.downcase()

    sig =
      case answer do
        "yes" -> ForgeSdk.Wallet.Util.sign!(wallet, tx_data)
        "y" -> ForgeSdk.Wallet.Util.sign!(wallet, tx_data)
        _ -> ""
      end

    Map.put(claim, "sig", sig |> Multibase.encode!(:base58_btc))
  end

  defp handle_claim(%{"type" => "signature"} = claim, wallet) do
    display_claim(claim)
    data = claim["data"] |> Multibase.decode!()
    tx = claim["origin"] |> Multibase.decode!() |> ForgeAbi.Transaction.decode()
    IO.inspect(ForgeSdk.display(tx))

    answer =
      IO.gets("Sign this transaction?\n") |> String.trim_trailing("\n") |> String.downcase()

    sig =
      case answer do
        "yes" -> sign(data, wallet)
        "y" -> sign(data, wallet)
        _ -> ""
      end

    Map.put(claim, "sig", sig)
  end

  defp handle_claim(%{"type" => "did"} = claim, _) do
    display_claim(claim)
    answer = IO.gets("") |> String.trim_trailing("\n")
    Map.put(claim, "did", answer)
  end

  defp handle_claim(%{"type" => "token"} = claim, _) do
    display_claim(claim)
    answer = IO.gets("") |> String.trim_trailing("\n")
    Map.put(claim, "value", answer)
  end

  defp handle_claim(%{"type" => "deposit"} = claim, _) do
    display_claim(claim)
    answer = IO.gets("") |> String.trim_trailing("\n")
    Map.put(claim, "deposit", answer)
  end

  defp display_claim(%{"meta" => %{"description" => des}}) do
    IO.puts(des <> "\n")
  end

  defp sign(data, wallet) do
    wallet.address
    |> AbtDid.get_did_type()
    |> Map.get(:key_type)
    |> sign(wallet.sk, data)
    |> Multibase.encode!(:base58_btc)
  end

  defp sign(:ed25519, sk, data), do: Mcrypto.sign!(@ed25519, data, sk)
  defp sign(:secp256k1, sk, data), do: Mcrypto.sign!(@secp256k1, data, sk)

  defp get_body(jwt) do
    jwt
    |> String.split(".")
    |> Enum.at(1)
    |> Base.url_decode64!(padding: false)
    |> Jason.decode!()
  end

  defp prepare_request(w, extra) do
    user_info = AbtDid.Signer.gen_and_sign(w.address, w.sk, extra)

    %{
      userPk: Multibase.encode!(w.pk, :base58_btc),
      userInfo: user_info
    }
    |> Jason.encode!()
  end

  defp str_to_bin(str) do
    case Base.decode16(str, case: :mixed) do
      {:ok, bin} -> bin
      _ -> Multibase.decode!(str)
    end
  end
end
