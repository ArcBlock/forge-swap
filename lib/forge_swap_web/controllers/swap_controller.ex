defmodule ForgeSwapWeb.SwapController do
  use ForgeSwapWeb, :controller

  alias ForgeSwap.Repo
  alias ForgeSwap.Schema.Swap
  alias ForgeSwap.Utils.Util
  alias ForgeSwap.Utils.Config, as: ConfigUtil
  alias ForgeSwap.Utils.Did, as: DidUtil

  alias ForgeSwapWeb.Plugs.{ExtractUserInfo, ReadSwap, VerifySig, VerifyUser}

  plug(VerifySig when action in [:start, :submit])
  plug(ExtractUserInfo when action in [:start, :submit])
  plug(ReadSwap when action in [:show, :start, :submit])
  plug(VerifyUser when action in [:start, :submit])

  def index(conn, _params) do
    render(conn, "index.html")
  end

  @doc """
  Creates a swap for between the application and a wallet.
  """
  def create(conn, params) do
    config = ConfigUtil.read_config()
    user_did = params["userDid"]
    asset_owner = params["assetOwner"] || "default"

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
      asset_owner: asset_owner,
      status: "not_started",
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
      {:ok, swap} -> json(conn, %{response: %{id: swap.id}})
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
  def start(conn, _params) do
    swap = conn.assigns.swap

    case swap.status do
      "not_started" -> do_start(conn, swap)
      _ -> json(conn, %{error: "Cannot start the swap again as it has already started."})
    end
  end

  defp do_start(conn, swap) do
    claims = require_user_set_up(swap)
    url = Util.get_submit_swap_callback(swap.id)
    extra = prepare_response(claims, url, swap.offer_chain)
    response = DidUtil.sign_response(extra)
    json(conn, response)
  end

  defp require_user_set_up(swap) do
    %{
      type: "did",
      did_type: "swap",
      meta: %{
        description: "Please set up an atomic swap on the ABT asset chain.",
        offer_assets: swap.offer_assets,
        offer_token: swap.offer_token,
        demand_assets: swap.demand_assets,
        demand_token: swap.demand_token,
        demand_locktime: swap.demand_locktime
      }
    }
  end

  @doc """
  Submits the swap set up by wallet to the application. The app will
  set up the swap as return.
  """
  def submit(_conn, _params) do
  end

  defp prepare_response(claims, url, offer_chain) do
    get_general_response(offer_chain)
    |> Map.put(:requestedClaims, claims)
    |> Map.put(:url, url)
  end

  defp get_general_response(offer_chain) do
    config = ConfigUtil.read_config()
    app_config = config["application"]
    chain_config = config["chains"][offer_chain]

    %{
      appInfo: %{
        name: app_config["name"],
        description: app_config["description"]
      },
      chanInfo: %{
        host: "#{chain_config["host"]}:#{chain_config["port"]}/api"
      }
    }
  end
end
