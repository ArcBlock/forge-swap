defmodule ForgeSwap.Swapper do
  @moduledoc """
  This module is responsible for setting up swap for user on the offer chain.
  """
  use GenServer

  require Logger

  alias ForgeSwap.Repo
  alias ForgeSwap.Schema.Swap
  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Config, as: ConfigUtil
  alias ForgeSwap.Utils.Tx, as: TxUtil

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def set_up_swap(swap, swap_state) do
    GenServer.cast(__MODULE__, {:set_up_swap, swap, swap_state})
  end

  # Call backs

  def init(:ok) do
    {:ok, []}
  end

  def handle_cast({:set_up_swap, swap, swap_state}, state) do
    state = state ++ [{swap, swap_state, ""}]
    send(__MODULE__, :tick)
    {:noreply, state}
  end

  def handle_info(:tick, state) do
    state =
      state
      |> Enum.map(&do_set_up_swap/1)
      |> Enum.reject(&is_nil/1)

    case state do
      [] -> state
      _ -> Process.send_after(self(), :tick, 3000)
    end

    {:noreply, state}
  end

  defp do_set_up_swap({swap, swap_state, ""}) do
    hash = send_set_up_swap_tx(swap, swap_state)
    Logger.info("Set up swap for user, hash: #{inspect(hash)}")
    {swap, swap_state, hash}
  end

  defp do_set_up_swap({swap, swap_state, hash}) do
    case ChainUtil.get_tx(hash, swap.offer_chain) do
      %{"code" => "OK"} -> both_set_up(swap, swap_state, hash)
      %{"code" => _} -> {swap, swap_state, ""}
      nil -> {swap, swap_state, hash}
    end
  end

  defp send_set_up_swap_tx(swap, state) do
    config = ConfigUtil.read_config()
    owner = config["asset_owners"][swap.asset_owner]
    tx = TxUtil.set_up_swap(owner, swap.user_did, swap, state["hashlock"])
    ChainUtil.send_tx(tx, swap.offer_chain)
  end

  # Updates the swap in DB.
  # Sets the status = 'both_set_up', offer_swap = swap_state.address
  # If successfully updates the DB, return nil, othewise returns the original input.
  defp both_set_up(swap, swap_state, hash) do
    address = ForgeSdk.Util.to_swap_address(hash)
    swap_state = ChainUtil.get_swap_state(address, swap.offer_chain)

    change =
      Swap.update_changeset(swap, %{status: "both_set_up", offer_swap: swap_state["address"]})

    Logger.debug(fn -> "Updating swap status to both_set_up, swap id: #{swap.id}" end)
    apply(Repo, :update!, [change])
    nil
  rescue
    e ->
      Logger.warn(
        "Failed to set swap status to both_set_up, will retry. Swap Id: #{swap.id}, Hash: #{hash}. Error: #{
          inspect(e)
        }."
      )

      {swap, swap_state, hash}
  end
end
