defmodule ForgeSwap.Utils.Tx do
  alias ForgeAbi.{SetupSwapTx, Transaction}
  alias ForgeAbi.Util.BigInt
  alias ForgeSdk.Wallet.Util, as: WalletUtil

  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Config, as: ConfigUtil

  def set_up_swap(sender, receiver, swap, hashlock) do
    setup_swap =
      SetupSwapTx.new(
        assets: swap.offer_assets,
        value: to_token(swap.offer_token),
        hashlock: hashlock,
        locktime: ChainUtil.time_to_locktime(swap.offer_locktime, swap.offer_chain),
        receiver: receiver
      )

    itx =
      Google.Protobuf.Any.new(
        type_url: "fg:t:setup_swap",
        value: SetupSwapTx.encode(setup_swap)
      )

    chain_id = ConfigUtil.read_config()["chains"][swap.offer_chain]["chain_id"]
    gen_tx(sender, chain_id, itx)
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
