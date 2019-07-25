defmodule ForgeSwap.Utils.Tx do
  alias ForgeAbi.{RetrieveSwapTx, SetupSwapTx, Transaction}
  alias ForgeAbi.Util.BigInt
  alias ForgeSdk.Wallet.Util, as: WalletUtil

  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Config, as: ConfigUtil

  def set_up_swap(swap, hashlock) do
    setup_swap =
      SetupSwapTx.new(
        assets: swap.offer_assets,
        value: to_token(swap.offer_token),
        hashlock: Base.decode16!(hashlock),
        locktime: ChainUtil.time_to_locktime(swap.offer_locktime, swap.offer_chain),
        receiver: swap.user_did
      )

    itx =
      Google.Protobuf.Any.new(
        type_url: "fg:t:setup_swap",
        value: SetupSwapTx.encode(setup_swap)
      )

    config = ConfigUtil.read_config()
    owner = config["asset_owners"][swap.asset_owner]
    chain_id = config["chains"][swap.offer_chain]["chain_id"]

    owner
    |> gen_tx(chain_id, itx)
    |> ChainUtil.send_tx(swap.offer_chain)
  end

  def retrieve_swap(swap, hashkey) do
    retrieve_swap =
      RetrieveSwapTx.new(
        address: swap.demand_swap,
        hashkey: Base.decode16!(hashkey)
      )

    itx =
      Google.Protobuf.Any.new(
        type_url: "fg:t:retrieve_swap",
        value: RetrieveSwapTx.encode(retrieve_swap)
      )

    config = ConfigUtil.read_config()
    owner = config["asset_owners"][swap.asset_owner]
    chain_id = config["chains"][swap.demand_chain]["chain_id"]

    owner
    |> gen_tx(chain_id, itx)
    |> ChainUtil.send_tx(swap.demand_chain)
  end

  defp to_token(offer_token) do
    offer_token
    |> Decimal.to_integer()
    |> BigInt.biguint()
  end

  defp gen_tx(sender, chain_id, itx) do
    <<nonce::64>> = :crypto.strong_rand_bytes(8)

    tx =
      Transaction.new(
        chain_id: chain_id,
        from: sender.address,
        itx: itx,
        nonce: nonce,
        pk: sender.pk
      )

    sig = WalletUtil.sign!(sender, Transaction.encode(tx))
    %{tx | signature: sig}
  end
end
