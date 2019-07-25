defmodule ForgeSwap.Swapper.Retriever do
  @moduledoc """

  # This is the list of swaps that app should try to retrieve
  [
    # If hashkey is empty, then it means the user has not yet retrieve the swap.
    {swap, ""},
    # If the hashkey is not empty, then gen server should try to retrieve the swap.
    {swap, hashkey}
  ]
  """

  use GenServer

  require Logger

  alias ForgeSwap.Repo
  alias ForgeSwap.Schema.Swap
  alias ForgeSwap.Swapper.Revoker
  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Tx, as: TxUtil

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def retrieve_swap(swap) do
    GenServer.cast(__MODULE__, {:retrieve_swap, swap})
  end

  ####################### Call backs ############################

  def init(:ok) do
    {:ok, []}
  end

  def handle_cast({:retrieve_swap, swap}, gen_server_state) do
    gen_server_state = gen_server_state ++ [{swap, "", ""}]
    send(__MODULE__, :tick)
    {:noreply, gen_server_state}
  end

  def handle_info(:tick, gen_server_state) do
    gen_server_state =
      gen_server_state
      |> Enum.map(&do_retrieve_swap/1)
      |> Enum.reject(&is_nil/1)

    if gen_server_state != [] do
      Process.send_after(__MODULE__, :tick, 3000)
    end

    {:noreply, gen_server_state}
  end

  # The hashkey is still empty, then try to get the hashkey first.
  defp do_retrieve_swap({swap, "", ""}) do
    offer_state = ChainUtil.get_swap_state(swap.offer_swap, swap.offer_chain)

    case offer_state do
      # Hashkey is not empty, user has retrieved the swap.
      %{"hashkey" => hashkey} when hashkey != "" -> user_retrieved(swap, hashkey)
      # The value and assets have been gone, but hashkey is still empty, this means user has revoked swap.
      %{"value" => 0, "assets" => [], "hashkey" => ""} -> user_revoked(swap)
      _ -> {swap, "", ""}
    end
  end

  defp do_retrieve_swap({swap, hashkey, ""}) do
    retrieve_hash = TxUtil.retrieve_swap(swap, hashkey)

    Logger.info(fn ->
      "Server sent RetrieveSwapTx, Swap Id: #{swap.id}, Hash: #{inspect(retrieve_hash)}"
    end)

    {swap, hashkey, retrieve_hash}
  end

  defp do_retrieve_swap({swap, hashkey, retrieve_hash}) do
    case ChainUtil.get_tx(retrieve_hash, swap.demand_chain) do
      # Successfully retrived swap
      %{"code" => "OK"} ->
        both_retrieved(swap, retrieve_hash)

      # Retrieve swap tx failed, reset the hash and try again
      %{"code" => _} ->
        Logger.info(fn ->
          "Server RetrieveSwapTx failed, will retry. Swap Id: #{swap.id}, Hash: #{
            inspect(retrieve_hash)
          }"
        end)

        {swap, hashkey, ""}

      # Tx is not on chain yet, keep waiting
      nil ->
        {swap, hashkey, retrieve_hash}
    end
  end

  defp user_retrieved(swap, hashkey) do
    Logger.info(fn ->
      "User retrieved the swap, Swap Id: #{swap.id}, Swap address: #{swap.offer_swap}"
    end)

    retrieve_hash = TxUtil.retrieve_swap(swap, hashkey)

    Logger.info(fn ->
      "Server sent RetrieveSwapTx, Swap Id: #{swap.id}, Hash: #{inspect(retrieve_hash)}"
    end)

    change = Swap.update_changeset(swap, %{status: "user_retrieved"})
    apply(Repo, :update!, [change])

    swap = Swap.get(swap.id)
    {swap, hashkey, retrieve_hash}
  end

  defp both_retrieved(swap, retrieve_hash) do
    Logger.info(fn ->
      "Server RetrieveSwapTx succeeded, Swap Id: #{swap.id}, Hash: #{inspect(retrieve_hash)}"
    end)

    change =
      Swap.update_changeset(swap, %{status: "both_retrieved", retrieve_hash: retrieve_hash})

    apply(Repo, :update!, [change])
    nil
  end

  defp user_revoked(swap) do
    Revoker.revoke_swap(swap)
    nil
  end
end
