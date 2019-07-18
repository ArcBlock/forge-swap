defmodule ForgeSwap.Utils.Util do
  require Logger

  alias ForgeSwap.Utils.Config, as: ConfigUtil

  def str_to_bin(str) do
    case Base.decode16(str, case: :mixed) do
      {:ok, bin} ->
        bin

      _ ->
        case Multibase.decode(str) do
          {:ok, bin} -> bin
          _ -> Base.url_decode64!(str, padding: false)
        end
    end
  end

  def str_to_int(nil), do: nil
  def str_to_int(""), do: nil

  def str_to_int(str) do
    str |> String.trim() |> String.to_integer()
  end

  def gql_call(url, query) do
    Logger.debug(fn -> "Making GraphQL call, url: #{inspect(url)}, query: #{inspect(query)}" end)

    %{body: body} = HTTPoison.post!(url, query, [{"Content-type", "application/graphql"}])
    %{"data" => data} = Jason.decode!(body)
    data
  end

  def gen_qr_code(url) when is_binary(url), do: url |> EQRCode.encode() |> EQRCode.svg()

  def gen_qr_code(id, status) do
    config = ConfigUtil.read_config()
    host = config["service"]["host"]
    port = config["service"]["port"]
    gen_qr_code(status, id, host, port)
  end

  # Generates the QR code by scanning which a user can start the swap process.
  defp gen_qr_code("not_started", swap_id, host, port),
    do: gen_qr_code("#{host}:#{port}/swap/#{swap_id}/start")

  # Generates the QR code by scanning which a user can continue to retrieve the swap set up by application.
  defp gen_qr_code("both_set_up", swap_id, host, port),
    do: gen_qr_code("#{host}:#{port}/swap/#{swap_id}/retrieve")

  defp gen_qr_code(_, _, _, _), do: ""
end
