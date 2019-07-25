defmodule ForgeSwap.Swapper.Revoker do
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

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def revoke_swap(swap) do
  end

  ####################### Call backs ############################
  def init(:ok) do
    {:ok, []}
  end
end
