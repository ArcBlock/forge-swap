defmodule ForgeSwap.Swapper.Revoker do
  @moduledoc """
  This module is responsible for revoking the swap on behave of application.
  The gen server state is like:
  [
    # The boolean value in the middle indicates whether or not if the revoker should 
    # try to revoke the swap. False means revoker should not revoke the swap.
    {swap, false, ""},
    # If the second value is set to TRUE and the third value is empty, then revoker should
    # try to send a transaction.
    {swap, true, ""},
    # If the hash is not empty, then revoker needs to check if the transaction succeeded or not.
    {swap, true, revoke_hash}
  ]
  """

  use GenServer
  require Logger

  alias ForgeSwap.Repo
  alias ForgeSwap.Schema.Swap
  alias ForgeSwap.Swapper.Retriever
  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Tx, as: TxUtil
  alias ForgeSwap.Utils.Config, as: ConfigUtil

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def delete(swap) do
    GenServer.cast(__MODULE__, {:delete, swap})
  end

  def revoke_swap(swap) do
    GenServer.cast(__MODULE__, {:revoke_swap, swap})
  end

  ####################### Call backs ############################
  def init(:ok) do
    swaps =
      ["both_set_up", "user_revoked"]
      |> Swap.get_by_status()
      |> Enum.map(fn
        %{status: "both_set_up"} = swap -> {swap, false, ""}
        %{status: "user_revoked"} = swap -> {swap, true, swap.revoke_hash || ""}
      end)

    {:ok, swaps}
  end

  def handle_cast({:delete, swap}, gen_server_state) do
    gen_server_state = Enum.reject(gen_server_state, fn {s, _, _} -> s.id == swap.id end)
    {:noreply, gen_server_state}
  end

  def handle_cast({:revoke_swap, swap}, gen_server_state) do
    gen_server_state = gen_server_state ++ [{swap, false, ""}]
    send(__MODULE__, :tick)
    {:noreply, gen_server_state}
  end

  def handle_info(:tick, gen_server_state) do
    gen_server_state =
      gen_server_state
      |> Enum.map(&do_revoke_swap/1)
      |> Enum.reject(&is_nil/1)

    if gen_server_state != [] do
      gap = ConfigUtil.read_config()["service"]["swapper_tick_gap"]
      Process.send_after(__MODULE__, :tick, trunc(gap * 1000))
    end

    {:noreply, gen_server_state}
  end

  defp do_revoke_swap({swap, false, ""}) do
    demand_state = ChainUtil.get_swap_state(swap.demand_swap, swap.demand_chain)

    case demand_state do
      # The value and assets have been gone, but hashkey is still empty, this means user has revoked swap.
      %{"value" => "0", "assets" => [], "hashkey" => ""} -> user_revoked(swap)
      _ -> {swap, false, ""}
    end
  end

  defp do_revoke_swap({swap, true, ""}) do
    revoke_hash = TxUtil.revoke_swap(swap)

    Logger.info(fn ->
      "Swap Id: #{swap.id}, Server sent RevokeSwapTx, Hash: #{inspect(revoke_hash)}"
    end)

    change = Swap.update_changeset(swap, %{revoke_hash: revoke_hash})
    apply(Repo, :update!, [change])
    swap = Swap.get(swap.id)

    {swap, true, revoke_hash}
  end

  defp do_revoke_swap({swap, true, revoke_hash}) do
    case ChainUtil.get_tx(revoke_hash, swap.offer_chain) do
      # Successfully revoked swap
      %{"code" => "OK"} ->
        both_revoked(swap, revoke_hash)

      # Revoke swap tx failed, reset the hash and try again
      %{"code" => _} ->
        Logger.warn(
          "Swap Id: #{swap.id}, Server RevokeSwapTx failed, will retry. Hash: #{
            inspect(revoke_hash)
          }"
        )

        {swap, true, ""}

      # Tx is not on chain yet, keep waiting
      nil ->
        {swap, true, revoke_hash}
    end
  end

  defp user_revoked(swap) do
    Logger.info(fn ->
      "Swap Id: #{swap.id}, User revoked the swap, Swap address: #{swap.demand_swap}"
    end)

    revoke_hash = TxUtil.revoke_swap(swap)

    Logger.info(fn ->
      "Swap Id: #{swap.id}, Server sent RevokeSwapTx, Hash: #{inspect(revoke_hash)}"
    end)

    change = Swap.update_changeset(swap, %{status: "user_revoked", revoke_hash: revoke_hash})
    apply(Repo, :update!, [change])
    swap = Swap.get(swap.id)

    # Since user has revoked the swap set up by her, then there
    # is no for us to check if we can retrieve that swap.
    Retriever.delete(swap)

    {swap, true, revoke_hash}
  end

  defp both_revoked(swap, revoke_hash) do
    Logger.info(fn ->
      "Swap Id: #{swap.id}, Server RevokeSwapTx succeeded, Hash: #{inspect(revoke_hash)}"
    end)

    change = Swap.update_changeset(swap, %{status: "both_revoked", revoke_hash: revoke_hash})

    apply(Repo, :update!, [change])
    nil
  end
end
