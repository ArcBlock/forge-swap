defmodule ForgeSwap.Swapper.Setupper do
  @moduledoc """
  This module is responsible for setting up swap for user on the offer chain.
  The state of this gen server is list looks like this:
    [ 
        # If the offer hash is empty, then it means there has not been a successfully set up swap tx yet.
        {swap, hashlock, ”“},
        # If the offer hash is not empty, then the gen server will try to validate the tx.
        {swap, hashlock, offer_hash},
        {swap, hashlock, offer_hash},
    ]
  """
  use GenServer

  require Logger

  alias ForgeSwap.Repo
  alias ForgeSwap.Schema.Swap
  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Tx, as: TxUtil
  alias ForgeSwap.Swapper.{Retriever, Revoker}

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def set_up_swap(swap, hashlock) do
    GenServer.cast(__MODULE__, {:set_up_swap, swap, hashlock})
  end

  ####################### Call backs ############################

  def init(:ok) do
    swaps =
      "user_set_up"
      |> Swap.get_by_status()
      |> Enum.map(fn swap ->
        demand_state = ChainUtil.get_swap_state(swap.demand_swap, swap.demand_chain)
        {swap, demand_state["hashlock"], swap.set_up_hash || ""}
      end)

    if swaps != [] do
      send(__MODULE__, :tick)
    end

    set = Enum.reduce(swaps, MapSet.new(), fn {swap, _, _}, acc -> MapSet.put(acc, swap.id) end)

    {:ok, {swaps, set}}
  end

  def handle_cast({:set_up_swap, swap, hashlock}, {swaps, set}) do
    case MapSet.member?(set, swap.id) do
      true ->
        {swaps, set}

      false ->
        swaps = swaps ++ [{swap, hashlock, ""}]
        set = MapSet.put(set, swap.id)
        send(__MODULE__, :tick)
        {:noreply, {swaps, set}}
    end
  end

  def handle_info(:tick, {swaps, set}) do
    {rest, to_delete} =
      swaps
      |> Enum.map(&do_set_up_swap/1)
      |> Enum.split_with(fn
        {_, :delete, _} -> false
        _ -> true
      end)

    set = Enum.reduce(to_delete, set, fn {swap, _, _}, acc -> MapSet.delete(acc, swap.id) end)

    if rest != [] do
      gap = ArcConfig.read_config(:forge_swap)["service"]["swapper_tick_gap"]
      Process.send_after(__MODULE__, :tick, trunc(gap * 1000))
    end

    {:noreply, {rest, set}}
  end

  defp do_set_up_swap({swap, hashlock, ""}) do
    case TxUtil.set_up_swap(swap, hashlock) do
      nil ->
        Logger.warn("Swap Id: #{swap.id}, Server failed to sent SetupSwapTx.")
        {swap, hashlock, ""}

      offer_hash ->
        Logger.info(fn ->
          "Swap Id: #{swap.id}, Server sent SetupSwapTx, Hash: #{inspect(offer_hash)}"
        end)

        change = Swap.update_changeset(swap, %{set_up_hash: offer_hash})
        apply(Repo, :update!, [change])
        swap = Swap.get(swap.id)

        {swap, hashlock, offer_hash}
    end
  end

  defp do_set_up_swap({swap, hashlock, offer_hash}) do
    case ChainUtil.get_tx(offer_hash, swap.offer_chain) do
      %{"code" => "OK"} ->
        both_set_up(swap, hashlock, offer_hash)

      %{"code" => _} ->
        Logger.warn(
          "Swap Id: #{swap.id}, Server SetupSwapTx failed, will retry. Hash: #{
            inspect(offer_hash)
          }"
        )

        {swap, hashlock, ""}

      nil ->
        {swap, hashlock, offer_hash}
    end
  end

  # Updates the swap in DB.
  # Sets the status = 'both_set_up', offer_swap = swap_state.address
  # If successfully updates the DB, return nil, othewise returns the original input.
  defp both_set_up(swap, hashlock, offer_hash) do
    offer_address = ForgeSdk.Util.to_swap_address(offer_hash)

    Logger.info(fn ->
      "Swap Id: #{swap.id}, Server set up a swap, Swap address: #{offer_address}"
    end)

    change = Swap.update_changeset(swap, %{status: "both_set_up", offer_swap: offer_address})

    apply(Repo, :update!, [change])
    swap = Swap.get(swap.id)

    Retriever.retrieve_swap(swap)
    Revoker.revoke_swap(swap)
    {swap, :delete, ""}
  rescue
    e ->
      Logger.warn(
        "Swap Id: #{swap.id}, Failed to set swap status to both_set_up, will retry. Hash: #{
          inspect(offer_hash)
        }. Error: #{inspect(e)}."
      )

      {swap, hashlock, offer_hash}
  end
end
