defmodule ForgeSwapWeb.SwapController do
  use ForgeSwapWeb, :controller

  alias ForgeSwap.Utils.Config, as: ConfigUtil
  alias ForgeSwap.Schema.Swap

  alias ForgeSwapWeb.Plugs.{ExtractUserInfo, ReadSwap, VerifySig, VerifyUser}

  # plug(VerifySig when action in [:show, :start, :submit])
  # plug(ExtractUserInfo when action in [:show, :start, :submit])
  plug(ReadSwap when action in [:show, :start, :submit])
  # plug(VerifyUser when action in [:show, :start, :submit])

  def index(conn, _params) do
    render(conn, "index.html")
  end

  @doc """
  Creates a swap for between the application and a wallet.
  """
  def create(conn, params) do
    config = ConfigUtil.read_config()
    user_did = params["userDid"]

    offer_assets = params["offerAssets"] || []
    offer_token = params["offerToken"] || 0
    offer_chain = params["offerChain"] || "application"
    offer_locktime = params["offerLocktime"] || config["swap"]["offer_locktime"]

    demand_assets = params["demandAssets"] || []
    demand_token = params["demandToken"] || 0
    demand_chain = params["demandChain"] || "asset"
    demand_locktime = params["demandLocktime"] || config["swap"]["demand_locktime"]

    do_create(conn, %{
      user_did: user_did,
      offer_assets: offer_assets,
      offer_token: offer_token,
      offer_chain: offer_chain,
      offer_locktime: offer_locktime,
      demand_assets: demand_assets,
      demand_token: demand_token,
      demand_chain: demand_chain,
      demand_locktime: demand_locktime
    })
  end

  defp do_create(conn, %{offer_assets: [], offer_token: 0}),
    do: json(conn, %{error: "Assets and token to offer cannot be both empty at the same time."})

  defp do_create(conn, %{demand_assets: [], demand_token: 0}),
    do: json(conn, %{error: "Assets and token to demand cannot be both empty at the same time."})

  defp do_create(conn, params) do
    change_set = Swap.insert_changeset(params)

    case apply(Repo, :insert, [change_set]) do
      {:ok, swap} -> json(conn, %{id: swap.id})
      {:error, change} -> json(conn, %{error: "#{inspect(change.errors)}"})
    end
  end

  @doc """
  Shows the status of a swap. The displayed live view page will get updated 
  automatically when the swap status is changed.
  """
  def show(conn, _) do
    swap = conn.assigns.swap
    live_render(conn, ForgeSwapWeb.SwapLive, session: %{id: swap.id, status: swap.status})
  end

  @doc """
  Starts the swapping by wallet.
  """
  def start(conn, params) do
  end

  @doc """
  Submits the swap set up by wallet to the application. The app will
  set up the swap as return.
  """
  def submit(conn, params) do
  end
end
