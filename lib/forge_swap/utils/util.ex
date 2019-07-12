defmodule ForgeSwap.Utils.Util do
  require Logger

  alias ForgeSwap.Utils.Config, as: ConfigUtil

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

  @doc """
  Generates the QR code by scanning which a user can start the swap process.
  """
  def gen_start_swap_qr_code(swap_id) do
    config = ConfigUtil.read_config()
    host = config["swap"]["host"]
    port = config["swap"]["port"]
    "http://#{host}:#{port}/swap/#{swap_id}/start"
  end

  @doc """
  Generates the QR cdoe by scanning which a user can submit the address of swap set up by her.
  """
  def gen_submit_swap_qr_code(swap_id) do
    config = ConfigUtil.read_config()
    host = config["swap"]["host"]
    port = config["swap"]["port"]
    "http://#{host}:#{port}/swap/#{swap_id}/submit"
  end

  @doc """
  Generates the QR code by scanning which a user can continue to retrieve the swap set up by application.
  """
  def gen_continue_swap_qr_code(swap_id) do
    config = ConfigUtil.read_config()
    host = config["swap"]["host"]
    port = config["swap"]["port"]
    "http://#{host}:#{port}/swap/#{swap_id}/continue"
  end
end
