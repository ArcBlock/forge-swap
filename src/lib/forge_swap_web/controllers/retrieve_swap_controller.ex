defmodule ForgeSwapWeb.RetrieveSwapController do
  @moduledoc """
  Handles the workflow in which user manually continues to retrieve swap set up by app.
  There 2 steps in this workflow.
    Step 1, user calls :retrieve by scanning a QR code, server requires proof of wonership for a user address.
    Step 2, user returns the user did by calling :start_re_user, server requires a swap address.
    Step 3, user returns the swap address by calling :payment_return_swap, server sets up a swap and returns the address.
  """
  use ForgeSwapWeb, :controller
  use Hyjal, router: ForgeSwapWeb.Router, endpoint: ForgeSwapWeb.Endpoint

  alias ForgeSwap.Utils.Did, as: DidUtil
  alias ForgeSwapWeb.Plugs.{CompareAuthPrincipal, ReadSwap}

  alias Hyjal.Plugs.VerifyAuthPrincipal

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
    reply(conn, :ok, %{swapAddress: swap.offer_swap})
  end
end
