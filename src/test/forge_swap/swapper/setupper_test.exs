defmodule ForgeSwap.Swapper.SetupperTest do
  use ExUnit.Case

  import Mock

  alias ForgeSwap.Schema.Swap
  alias ForgeSwap.PostgresRepo
  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Tx, as: TxUtil
  alias ForgeSwap.Swapper.Setupper

  setup do
    # Explicitly get a connection before each test
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(PostgresRepo)
    # Setting the shared mode must be done only after checkout
    Ecto.Adapters.SQL.Sandbox.mode(PostgresRepo, {:shared, self()})
  end

  test "Start setupper, tx not sent yet." do
    # For this swap, our mocked scenario is that the swapper has not yet sent
    # SetUpSwap Tx for it, so swapper will send the tx.
    swap = %{
      user_did: "user",
      asset_owner: "default",
      status: "user_set_up",
      set_up_hash: nil,
      retrieve_hash: nil,
      revoke_hash: nil,
      offer_swap: "offer_swap",
      offer_chain: "application",
      offer_locktime: 8888,
      demand_chain: "asset",
      demand_swap: "demand_swap",
      demand_locktime: 8888
    }

    swap |> Swap.insert_changeset() |> PostgresRepo.insert!()
    {:ok, agent} = Agent.start_link(fn -> nil end)

    with_mocks([
      {Swap, [:passthrough], []},
      {ChainUtil, [],
       get_swap_state: fn "demand_swap", _ -> %{"hashlock" => "hashlock"} end,
       get_tx: fn "set_up_hash", _ -> %{"code" => "OK"} end},
      {TxUtil, [],
       set_up_swap: fn s, "hashlock" ->
         Agent.update(agent, fn _ -> s end)
         "set_up_hash"
       end},
      {ForgeSdk.Util, [:passthrough], to_swap_address: fn "set_up_hash" -> "swap_addr" end}
    ]) do
      Setupper.start_link([])
      Process.sleep(600)

      ss = Agent.get(agent, fn s -> s end)
      assert_called(ChainUtil.get_swap_state("demand_swap", "asset"))
      assert_called(TxUtil.set_up_swap(ss, "hashlock"))
      assert_called(Swap.update_changeset(ss, %{set_up_hash: "set_up_hash"}))
      assert_called(ChainUtil.get_tx("set_up_hash", swap.offer_chain))
      ss = %{ss | set_up_hash: "set_up_hash"}
      assert_called(Swap.update_changeset(ss, %{status: "both_set_up", offer_swap: "swap_addr"}))
    end
  end

  test "Start setupper, tx already sent, but not verified yet." do
    # For this swap, our mocked scenario is that the swapper has sent the 
    # SetupSwapTx, but has not yet verify it. When swapper get the tx, it wil
    # find the tx failed. The swapper will resend a tx which will succeed.
    swap = %{
      user_did: "user",
      asset_owner: "default",
      status: "user_set_up",
      set_up_hash: "set_up_hash",
      retrieve_hash: nil,
      revoke_hash: nil,
      offer_swap: "offer_swap",
      offer_chain: "application",
      offer_locktime: 8888,
      demand_chain: "asset",
      demand_swap: "demand_swap",
      demand_locktime: 8888
    }

    swap |> Swap.insert_changeset() |> PostgresRepo.insert!()
    {:ok, agent} = Agent.start_link(fn -> nil end)

    with_mocks([
      {Swap, [:passthrough], []},
      {ChainUtil, [],
       get_swap_state: fn "demand_swap", _ -> %{"hashlock" => "hashlock"} end,
       get_tx: fn
         "set_up_hash", _ -> %{"code" => "INVALID"}
         "set_up_again_hash", _ -> %{"code" => "OK"}
       end},
      {TxUtil, [],
       set_up_swap: fn s, "hashlock" ->
         Agent.update(agent, fn _ -> s end)
         "set_up_again_hash"
       end},
      {ForgeSdk.Util, [:passthrough], to_swap_address: fn "set_up_again_hash" -> "swap_addr" end}
    ]) do
      Setupper.start_link([])
      Process.sleep(600)

      ss = Agent.get(agent, fn s -> s end)
      assert_called(ChainUtil.get_tx("set_up_hash", swap.offer_chain))
      assert_called(TxUtil.set_up_swap(ss, "hashlock"))
      assert_called(Swap.update_changeset(ss, %{set_up_hash: "set_up_again_hash"}))
      assert_called(ChainUtil.get_tx("set_up_again_hash", swap.offer_chain))
      ss = %{ss | set_up_hash: "set_up_again_hash"}
      assert_called(Swap.update_changeset(ss, %{status: "both_set_up", offer_swap: "swap_addr"}))
    end
  end
end
