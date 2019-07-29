defmodule ForgeSwapWeb.Router do
  use ForgeSwapWeb, :router

  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_flash)
    plug(Phoenix.LiveView.Flash)
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
  end

  pipeline :api do
    plug(:accepts, ["json"])
  end

  scope "/", ForgeSwapWeb do
    pipe_through(:browser)

    get("/", PageController, :index)
    get("/swap/:id", SwapController, :show)
  end

  scope "/api", ForgeSwapWeb do
    pipe_through(:api)

    post("/swap/", SwapController, :create)

    # The workflow to start a swap.
    # Step 1, The QR code endpoint to start the swap
    get("/swap/:id/start", StartSwapController, :start)
    # Step 2, The endpoint to let user return user addr
    post("/swap/:id/start/re/user", StartSwapController, :start_re_user)
    # Step 3, The endpoint to let user return swap addr
    post("/swap/:id/start/re/swap", StartSwapController, :start_re_swap)

    # The workflow to let user continue retrieve the swap.
    # Step 1, The QR code to scan.
    get("/swap/:id/retrieve", RetrieveSwapController, :retrieve)
    # Step 2, The endpoint to let user return user addr
    post("/swap/:id/retrieve/re/user", RetrieveSwapController, :retrieve_re_user)
  end

  # Other scopes may use custom stacks.
  # scope "/api", ForgeSwapWeb do
  #   pipe_through :api
  # end
end
