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
    get("/swap/:id/payment", PaymentController, :start)
    # Step 2, The endpoint to let user return user addr
    post("/swap/:id/payment/authprincipal", PaymentController, :auth_principal)
    # Step 3, The endpoint to let user return swap addr
    post("/swap/:id/payment/returnswap", PaymentController, :payment_return_swap)

    # The workflow to let user continue retrieve the swap.
    # Step 1, The QR code to scan.
    get("/swap/:id/retrieve", RetrieveSwapController, :start)
    # Step 2, The endpoint to let user return user addr
    post("/swap/:id/retrieve/authprincipal", RetrieveSwapController, :auth_principal)
  end

  # Other scopes may use custom stacks.
  # scope "/api", ForgeSwapWeb do
  #   pipe_through :api
  # end
end
