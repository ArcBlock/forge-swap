defmodule ForgeSwapWeb.SwapController do
  use ForgeSwapWeb, :controller

  alias ForgeSwap.Repo
  alias ForgeSwap.Schema.Swap
  alias ForgeSwap.Utils.Chain, as: ChainUtil
  alias ForgeSwap.Utils.Config, as: ConfigUtil
  alias ForgeSwap.Utils.Did, as: DidUtil
  alias ForgeSwap.Utils.Tx, as: TxUtil

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
    claims = [require_user_set_up(swap)]
    callback = Routes.swap_url(conn, :submit, swap.id)
    extra = prepare_response(claims, callback, swap.offer_chain)
    response = DidUtil.sign_response!(extra, swap.asset_owner)
    json(conn, response)
  end

  defp require_user_set_up(swap) do
    chain = ConfigUtil.read_config()["chains"][swap.demand_chain]
    chain_url = "#{chain["host"]}:#{chain["port"]}/api"

    %{
      type: "did",
      didType: "swap",
      meta: %{
        description: "Please set up an atomic swap on the ABT asset chain.",
        offerAssets: swap.offer_assets,
        offerToken: Decimal.to_integer(swap.offer_token),
        demandAssets: swap.demand_assets,
        demandToken: Decimal.to_integer(swap.demand_token),
        demandLocktime: swap.demand_locktime,
        demandChain: chain_url
      }
    }
  end

  @doc """
  Submits the swap set up by wallet to the application. The app will
  set up the swap as return.
  """
  def submit(conn, _params) do
    swap = conn.assigns.swap

    claim =
      Enum.find(conn.assigns.claims, fn
        %{"type" => "did", "didType" => "swap", "did" => did} when did != "" -> true
        _ -> false
      end)

    case claim do
      nil -> json(conn, %{error: "Invalid request, could not find swap state address."})
      _ -> do_submit(conn, swap, claim)
    end
  end

  defp do_submit(conn, swap, %{"did" => swap_address}) do
    state = ChainUtil.get_swap_sate(swap_address, swap.demand_chain)

    cond do
      state == nil ->
        json(conn, %{error: "Could not find the swap state #{swap_address}"})

      verify_swap(swap, state) == false ->
        json(conn, %{error: "Invalid swap state, address: #{swap_address}"})

      true ->
        swap = user_set_up(swap, state)
        hash = set_up_swap(swap, state)
        both_set_up(swap, hash)
        json(conn, %{response: %{hash: hash}})
    end
  end

  defp verify_swap(swap, state) do
    config = ConfigUtil.read_config()
    asset_owner = config["asset_owners"][swap.asset_owner]
    expected_locktime = ChainUtil.time_to_locktime(swap.demand_locktime, swap.demand_chain)
    actual_token = String.to_integer(state["value"])
    expected_token = Decimal.to_integer(swap.demand_token)

    cond do
      # if it is not set up by user
      state["sender"] !== swap.user_did -> false
      # if it is set up for app
      state["receiver"] !== asset_owner.address -> false
      # if set up assets are not exactly same as demand assets
      state["assets"] -- swap.demand_assets !== [] -> false
      swap.demand_assets -- state["assets"] !== [] -> false
      # if set up token is not exactly same as demand token
      actual_token !== expected_token -> false
      # if set up locktime is earlier than expected locktime
      state["locktime"] < expected_locktime -> false
      true -> true
    end
  end

  def set_up_swap(swap, state) do
    config = ConfigUtil.read_config()
    owner = config["asset_owners"][swap.asset_owner]
    tx = TxUtil.set_up_swap(owner, swap.user_did, swap, state["hashlock"])
    ChainUtil.send_tx(tx, swap.offer_chain)
  end

  defp user_set_up(swap, state) do
    change = Swap.update_changeset(swap, %{status: "user_set_up", demand_swap: state["address"]})
    apply(Repo, :update!, [change])
    Swap.get(swap.id)
  end

  defp both_set_up(swap, hash) do
    address = ForgeSdk.Util.to_swap_address(hash)
    swap_state = ChainUtil.get_swap_sate(address, swap.offer_chain)

    change =
      Swap.update_changeset(swap, %{status: "both_set_up", offer_swap: swap_state["address"]})

    apply(Repo, :update!, [change])
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
