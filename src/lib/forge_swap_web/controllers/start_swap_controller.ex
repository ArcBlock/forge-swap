defmodule ForgeSwapWeb.StartSwapController do
  @moduledoc """
  Handles the workflow in which user starts the swap process.
  There 3 steps in this workflow.
    Step 1, user calls :start by scanning a QR code, server requires proof of wonership for a user address.
    Step 2, user returns the user did by calling :start_re_user, server requires a swap address.
    Step 3, user returns the swap address by calling :start_re_swap, server sets up a swap and returns the address.
  """
  use ForgeSwapWeb, :controller

  require Logger

  alias ForgeSwap.{Repo, Schema.Swap, Swapper.Setupper}
  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Config, as: ConfigUtil
  alias ForgeSwap.Utils.Did, as: DidUtil

  alias ForgeSwapWeb.Plugs.{ExtractUserInfo, ReadSwap, VerifySig, VerifyUser}

  plug(VerifySig when action in [:start_re_user, :start_re_swap])
  plug(ExtractUserInfo when action in [:start_re_user, :start_re_swap])
  plug(ReadSwap when action in [:start, :start_re_user, :start_re_swap])
  plug(VerifyUser when action in [:start_re_user, :start_re_swap])

  @doc """
  The endpoint of the QR code by scanning which a user can start the swap process.
  This endpoint requires user to proof of ownership of a certain DID.
  """
  def start(conn, _) do
    swap = conn.assigns.swap
    claims = [DidUtil.require_user_did(swap)]
    callback = Routes.start_swap_url(conn, :start_re_user, swap.id)
    extra = DidUtil.prepare_extra_response(claims, callback, swap.offer_chain)
    response = DidUtil.gen_and_sign_response!(extra)
    json(conn, response)
  end

  @doc """
  Handles user returning user address during start swap workflow.
  """
  def start_re_user(conn, _params) do
    swap = conn.assigns.swap

    case swap.status do
      "not_started" -> do_start_re_user(conn, swap)
      _ -> json(conn, %{error: "Cannot start the swap again as it has already started."})
    end
  end

  defp do_start_re_user(conn, swap) do
    claims = [DidUtil.require_user_set_up(swap)]
    callback = Routes.start_swap_url(conn, :start_re_swap, swap.id)
    extra = DidUtil.prepare_extra_response(claims, callback, swap.offer_chain)
    response = DidUtil.gen_and_sign_response!(extra)
    IO.inspect(response)
    json(conn, response)
  end

  @doc """
  Handles user returning swap address during start swap workflow.
  """
  def start_re_swap(conn, _params) do
    swap = conn.assigns.swap

    claim =
      Enum.find(conn.assigns.claims, fn
        %{"type" => "swap", "address" => address} when address != "" -> true
        _ -> false
      end)

    case claim do
      nil -> json(conn, %{error: "Invalid request, could not find swap state address."})
      _ -> do_start_re_swap(conn, swap, claim)
    end
  end

  defp do_start_re_swap(conn, swap, %{"address" => demand_address}) do
    demand_state = ChainUtil.get_swap_state(demand_address, swap.demand_chain)

    cond do
      demand_state == nil ->
        json(conn, %{error: "Could not find the demanded swap state #{demand_address}"})

      (error = verify_swap(swap, demand_state)) != :ok ->
        json(conn, %{
          error: "Invalid demanded swap state, address: #{demand_address}, error: #{error}"
        })

      true ->
        # Change the swap status to user_set_up in db.
        swap = user_set_up(swap, demand_state)
        # Asynchronously set up a swap for user 
        Setupper.set_up_swap(swap, demand_state["hashlock"])
        callback = Routes.retrieve_swap_url(conn, :retrieve_re_user, swap.id)
        json(conn, %{response: %{callback: callback}})
    end
  end

  defp verify_swap(swap, demand_state) do
    config = ConfigUtil.read_config()
    asset_owner = config["asset_owners"][swap.asset_owner]
    expected_locktime = ChainUtil.time_to_locktime(swap.demand_locktime, swap.demand_chain)
    actual_token = String.to_integer(demand_state["value"])
    expected_token = Decimal.to_integer(swap.demand_token)

    cond do
      # if it is not set up by user
      demand_state["sender"] !== swap.user_did ->
        "Unexpected swap sender."

      # if it is not set up for app
      demand_state["receiver"] !== asset_owner.address ->
        "Unexpected swap receiver."

      # if set up assets are not exactly same as demand assets
      demand_state["assets"] -- swap.demand_assets !== [] ->
        "Unexpected swap assets"

      swap.demand_assets -- demand_state["assets"] !== [] ->
        "Unexpected swap assets"

      # if set up token is not exactly same as demand token
      actual_token !== expected_token ->
        "Unexpected swap token"

      # if set up locktime is earlier than expected locktime
      demand_state["locktime"] < expected_locktime ->
        "Unexpected swap locktime, required locktime: #{expected_locktime}, actual locktime: #{
          demand_state["locktime"]
        }"

      true ->
        :ok
    end
  end

  defp user_set_up(swap, state) do
    Logger.info(fn ->
      "User set up a swap, Swap Id: #{swap.id}, Swap address: #{state["address"]}"
    end)

    change = Swap.update_changeset(swap, %{status: "user_set_up", demand_swap: state["address"]})
    apply(Repo, :update!, [change])
    Swap.get(swap.id)
  end
end
