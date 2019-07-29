defmodule ForgeSwap.Swapper.Setupper do
  @moduledoc """
  This module is responsible for setting up swap for user on the offer chain.
  The state of this gen server is list looks like this:
    [ 
        # If the offer hash is empty, then it means there has not been a successfully set up swap tx yet.
        {swap, demand_state, ”“},
        # If the offer hash is not empty, then the gen server will try to validate the tx.
        {swap, demand_state, offer_hash},
        {swap, demand_state, offer_hash},
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

  def set_up_swap(swap, demand_state) do
    GenServer.cast(__MODULE__, {:set_up_swap, swap, demand_state})
  end

  ####################### Call backs ############################

  def init(:ok) do
    {:ok, []}
  end

  def handle_cast({:set_up_swap, swap, demand_state}, gen_server_state) do
    gen_server_state = gen_server_state ++ [{swap, demand_state, ""}]
    send(__MODULE__, :tick)
    {:noreply, gen_server_state}
  end

  def handle_info(:tick, gen_server_state) do
    gen_server_state =
      gen_server_state
      |> Enum.map(&do_set_up_swap/1)
      |> Enum.reject(&is_nil/1)

    if gen_server_state != [] do
      Process.send_after(__MODULE__, :tick, 3000)
    end

    {:noreply, gen_server_state}
  end

  defp do_set_up_swap({swap, demand_state, ""}) do
    offer_hash = TxUtil.set_up_swap(swap, demand_state["hashlock"])

    Logger.info(fn ->
      "Server sent SetupSwapTx, Swap Id: #{swap.id}, Hash: #{inspect(offer_hash)}"
    end)

    {swap, demand_state, offer_hash}
  end

  defp do_set_up_swap({swap, demand_state, offer_hash}) do
    case ChainUtil.get_tx(offer_hash, swap.offer_chain) do
      %{"code" => "OK"} ->
        both_set_up(swap, demand_state, offer_hash)

      %{"code" => _} ->
        Logger.info(fn ->
          "Server SetupSwapTx failed, will retry. Swap Id: #{swap.id}, Hash: #{
            inspect(offer_hash)
          }"
        end)

        {swap, demand_state, ""}

      nil ->
        {swap, demand_state, offer_hash}
    end
  end

  # Updates the swap in DB.
  # Sets the status = 'both_set_up', offer_swap = swap_state.address
  # If successfully updates the DB, return nil, othewise returns the original input.
  defp both_set_up(swap, demand_state, offer_hash) do
    offer_address = ForgeSdk.Util.to_swap_address(offer_hash)

    Logger.info(fn ->
      "Server set up a swap, Swap Id: #{swap.id}, Swap address: #{offer_address}"
    end)

    change = Swap.update_changeset(swap, %{status: "both_set_up", offer_swap: offer_address})

    apply(Repo, :update!, [change])
    swap = Swap.get(swap.id)

    Retriever.retrieve_swap(swap)
    Revoker.revoke_swap(swap)
    nil
  rescue
    e ->
      Logger.warn(
        "Failed to set swap status to both_set_up, will retry. Swap Id: #{swap.id}, Hash: #{
          inspect(offer_hash)
        }. Error: #{inspect(e)}."
      )

      {swap, demand_state, offer_hash}
  end
end
