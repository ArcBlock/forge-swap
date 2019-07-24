defmodule ForgeSwapWeb.RetrieveSwapController do
  @moduledoc """
  Handles the workflow in which user manually continues to retrieve swap set up by app.
  There 2 steps in this workflow.
    Step 1, user calls :retrieve by scanning a QR code, server requires proof of wonership for a user address.
    Step 2, user returns the user did by calling :start_re_user, server requires a swap address.
    Step 3, user returns the swap address by calling :start_re_swap, server sets up a swap and returns the address.
  """
  use ForgeSwapWeb, :controller

  # alias ForgeSwap.Repo
  # alias ForgeSwap.Schema.Swap
  # alias ForgeSwap.Utils.Chain, as: ChainUtil
  # alias ForgeSwap.Utils.Config, as: ConfigUtil
  alias ForgeSwap.Utils.Did, as: DidUtil
  # alias ForgeSwap.Utils.Tx, as: TxUtil

  alias ForgeSwapWeb.Plugs.{ExtractUserInfo, ReadSwap, VerifySig, VerifyUser}

  plug(VerifySig when action in [:retrieve_re_user])
  plug(ExtractUserInfo when action in [:retrieve_re_user])
  plug(ReadSwap when action in [:retrieve, :retrieve_re_user])
  plug(VerifyUser when action in [:retrieve_re_user])

  def retrieve(conn, _params) do
    swap = conn.assigns.swap
    claims = [DidUtil.require_user_did(swap)]
    callback = Routes.retrieve_swap_url(conn, :retrieve_re_user, swap.id)
    extra = DidUtil.prepare_extra_response(claims, callback, swap.offer_chain)
    response = DidUtil.gen_and_sign_response!(extra, swap.asset_owner)
    json(conn, response)
  end

  def retrieve_re_user(conn, _params) do
    swap = conn.assigns.swap
    json(conn, %{response: %{swapAddress: swap.offer_swap}})
  end
end
