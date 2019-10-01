defmodule ForgeSwapWeb.PaymentController do
  use ForgeSwapWeb, :controller
  use Hyjal, router: ForgeSwapWeb.Router

  require Logger

  alias ForgeSwapWeb.Plugs.{CompareAuthPrincipal, ReadSwap}
  alias ForgeSwap.{Repo, Schema.Swap, Swapper.Setupper}
  alias ForgeSwap.Utils.Util
  alias ForgeSwap.Utils.Chain, as: ChainUtil

  alias Hyjal.Plugs.VerifyAuthPrincipal
  alias Hyjal.Claims.{AuthPrincipal, SetUpSwap}

  plug(VerifyAuthPrincipal when action != :start)
  plug(ReadSwap)
  plug(CompareAuthPrincipal when action != :start)

  @impl AuthFlow
  def start(conn, _params) do
    swap = conn.assigns.swap

    claim = %AuthPrincipal{
      description: "Please set the authentication principal to the specified DID.",
      target: swap.user_did
    }

    reply(conn, [claim], __MODULE__, :auth_principal, [swap.id])
  end

  @impl AuthFlow
  def auth_principal(conn, _params) do
    swap = conn.assigns.swap

    case swap.status do
      "not_started" -> require_set_up_swap(conn, swap)
      _ -> reply(conn, :error, "Cannot start the swap again as it has already started.")
    end
  end

  defp require_set_up_swap(conn, swap) do
    config = ArcConfig.read_config(:forge_swap)
    chain_config = config["chains"][swap.demand_chain]
    owner = config["asset_owners"][swap.asset_owner]

    claim = %SetUpSwap{
      description: "Please set up an atomic swap on the ABT asset chain.",
      offer_assets: swap.offer_assets,
      offer_token: Decimal.to_integer(swap.offer_token),
      demand_assets: swap.demand_assets,
      demand_token: Decimal.to_integer(swap.demand_token),
      demand_locktime: swap.demand_locktime,
      receiver: owner.address,
      demand_chain: chain_config["chain_id"]
    }

    reply(conn, [claim], __MODULE__, :payment_return_swap, [swap.id])
  end

  def payment_return_swap(conn, _params) do
    swap = conn.assigns.swap
    claim = Util.find_claim(conn.assigns.claims, SetUpSwap, fn c -> c.address != "" end)

    cond do
      swap.status != "not_started" -> reply(conn, :error, "User has already set up a swap.")
      claim == nil -> reply(conn, :error, "Invalid request, could not find swap state address.")
      true -> do_payment_return_swap(conn, swap, claim)
    end
  end

  defp do_payment_return_swap(conn, swap, %{address: demand_address}) do
    demand_state = ChainUtil.get_swap_state(demand_address, swap.demand_chain)

    cond do
      demand_state == nil ->
        reply(conn, :error, "Could not find the demanded swap state #{demand_address}")

      (error = verify_swap(swap, demand_state)) != :ok ->
        reply(
          conn,
          :error,
          "Invalid demanded swap state, address: #{demand_address}, error: #{error}"
        )

      true ->
        # Change the swap status to user_set_up in db.
        swap = user_set_up(swap, demand_state)
        # Asynchronously set up a swap for user 
        Setupper.set_up_swap(swap, demand_state["hashlock"])
        callback = Routes.retrieve_swap_url(conn, :auth_principal, swap.id)
        json(conn, %{response: %{callback: callback}})
    end
  end

  defp verify_swap(swap, demand_state) do
    config = ArcConfig.read_config(:forge_swap)
    asset_owner = config["asset_owners"][swap.asset_owner]
    expected_locktime = ChainUtil.to_locktime(swap.demand_locktime, swap.demand_chain)
    actual_token = String.to_integer(demand_state["value"])
    expected_token = Decimal.to_integer(swap.demand_token)

    cond do
      # if it is not set up by user
      demand_state["sender"] !== swap.user_did ->
        Logger.info(fn ->
          "Swap Id: #{swap.id}, Unexpected swap sender. Expected: #{swap.user_did}, actual: #{
            demand_state["sender"]
          }"
        end)

        "Unexpected swap sender."

      # if it is not set up for app
      demand_state["receiver"] !== asset_owner.address ->
        Logger.info(fn ->
          "Swap Id: #{swap.id}, Unexpected swap receiver. Expected: #{asset_owner.address}, actual: #{
            demand_state["receiver"]
          }"
        end)

        "Unexpected swap receiver."

      # if set up assets are not exactly same as demand assets
      demand_state["assets"] -- swap.demand_assets !== [] ->
        Logger.info(fn ->
          "Swap Id: #{swap.id}, Unexpected swap assets. Expected: #{swap.demand_assets}, actual: #{
            demand_state["assets"]
          }"
        end)

        "Unexpected swap assets"

      swap.demand_assets -- demand_state["assets"] !== [] ->
        Logger.info(fn ->
          "Swap Id: #{swap.id}, Unexpected swap assets. Expected: #{swap.demand_assets}, actual: #{
            demand_state["assets"]
          }"
        end)

        "Unexpected swap assets"

      # if set up token is not exactly same as demand token
      actual_token !== expected_token ->
        Logger.info(fn ->
          "Swap Id: #{swap.id}, Unexpected swap token. Expected: #{expected_token}, actual: #{
            actual_token
          }"
        end)

        "Unexpected swap token"

      # if set up locktime is earlier than expected locktime
      demand_state["locktime"] < expected_locktime ->
        Logger.info(fn ->
          "Swap Id: #{swap.id}, Unexpected swap locktime. Expected: #{expected_locktime}, actual: #{
            demand_state["locktime"]
          }"
        end)

        "Unexpected swap locktime, required locktime: #{expected_locktime}, actual locktime: #{
          demand_state["locktime"]
        }"

      true ->
        :ok
    end
  end

  defp user_set_up(swap, state) do
    Logger.info(fn ->
      "Swap Id: #{swap.id}, User set up a swap, Swap address: #{state["address"]}"
    end)

    change = Swap.update_changeset(swap, %{status: "user_set_up", demand_swap: state["address"]})
    apply(Repo, :update!, [change])
    Swap.get(swap.id)
  end
end
