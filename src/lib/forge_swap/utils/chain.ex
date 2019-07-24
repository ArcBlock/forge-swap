defmodule ForgeSwap.Utils.Chain do
  require Logger

  alias ForgeAbi.Transaction
  alias ForgeSwap.Utils.Config, as: ConfigUtil

  @query_get_chain_info """
  {
    getChainInfo {
      info {
        network
        blockHeight
        supportedTxs
      }
    }
  }
  """

  @query_get_forge_state """
  {
    {
      getForgeState {
        state {
          token {
            decimal
            description
            name
            symbol
            unit
          }
        }
      }
    }
  }
  """

  defp query_get_swap_state(address),
    do: """
    {
      getSwapState(address: "#{address}") {
        state {
          address
          assets
          hash
          hashkey
          hashlock
          locktime
          receiver
          sender
          value
        }
      }
    }
    """

  defp query_get_tx(hash),
    do: """
    {
      getTx(hash: "#{hash}") {
        info {
          code
        }
      }
    }
    """

  defp mutation_send_tx(tx),
    do: """
    mutation MyMutation {
      sendTx(commit: true, tx: "#{tx}") {
        hash
      }
    }
    """

  @doc """
  Convert time to locktime. Parameter `chain_name` is what you configured in 
  the `chains` section if the config file. `time` is the time(hours) you 
  want to lock a swap, e.g. 24 hours.

  The output is a block height that the expected time to reach it from the current block 
  height is the input `time`.
  """
  @spec time_to_locktime(number(), String.t()) :: Integer.t()
  def time_to_locktime(time, chain_name) do
    config = ConfigUtil.read_config()

    case config["chains"][chain_name] do
      nil ->
        raise "Not able to find configuration for chain: #{inspect(chain_name)}. Please make sure you configured it in the 'Chains' section."

      %{"block_gap" => block_gap} ->
        block_numbers = trunc(time * 60 * 60 / block_gap) + 1
        current_height = get_chain_info(chain_name)["blockHeight"] |> String.to_integer()
        current_height + block_numbers

      _ ->
        raise "The configuration must contain the block_time which specifies the average time in seconds to generate a block."
    end
  end

  @doc """
  The `chain_name` is what you configured in the `chains` section if the config file.
  """
  def get_chain_info(chain_name) do
    %{"getChainInfo" => %{"info" => info}} = do_query(@query_get_chain_info, chain_name)
    info
  end

  def get_forge_state(chain_name) do
    %{"getForgeState" => %{"state" => state}} = do_query(@query_get_forge_state, chain_name)
    state
  end

  def get_swap_state(address, chain_name) do
    query = query_get_swap_state(address)
    %{"getSwapState" => %{"state" => state}} = do_query(query, chain_name)
    state
  end

  def get_tx(hash, chain_name) do
    query = query_get_tx(hash)
    %{"getTx" => %{"info" => info}} = do_query(query, chain_name)
    info
  end

  def send_tx(tx, chain_name) do
    %{"sendTx" => %{"hash" => hash}} =
      tx
      |> Transaction.encode()
      |> Base.url_encode64(padding: false)
      |> mutation_send_tx()
      |> do_query(chain_name)

    hash
  end

  defp do_query(query, chain_name) do
    config = ConfigUtil.read_config()

    case config["chains"][chain_name] do
      nil ->
        raise "Not able to find configuration for chain: #{inspect(chain_name)}. Please make sure you configured it in the 'Chains' section."

      %{"host" => host, "port" => port} ->
        url = "#{host}:#{port}/api"
        gql_call(url, query)

      _ ->
        raise "The configuration must contain the host and port to connect to forge web."
    end
  end

  defp gql_call(url, query) do
    Logger.debug(fn -> "Making GraphQL call, url: #{inspect(url)}, query: #{inspect(query)}" end)

    %{body: body} = HTTPoison.post!(url, query, [{"Content-type", "application/graphql"}])
    %{"data" => data} = Jason.decode!(body)
    data
  end
end
