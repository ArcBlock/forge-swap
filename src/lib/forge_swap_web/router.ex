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
    get("/app/*path", WebappController, :index)
  end

  scope "/api", ForgeSwapWeb do
    pipe_through(:api)

    get("/swap/:id", SwapController, :get)

    # The workflow to start a swap.
    # Step 1, The QR code endpoint to start the swap
    get("/payment/:id/", PaymentController, :start)
    # Step 2, The endpoint to let user return user addr
    post("/payment/:id/authprincipal", PaymentController, :auth_principal)
    # Step 3, The endpoint to let user return swap addr
    post("/payment/:id/returnswap", PaymentController, :payment_return_swap)

    # The workflow to let user continue retrieve the swap.
    # Step 1, The QR code to scan.
    get("/retrievepayment/:id/", RetrieveSwapController, :start)
    # Step 2, The endpoint to let user return user addr
    post("/retrievepayment/:id/authprincipal", RetrieveSwapController, :auth_principal)
  end

  # Other scopes may use custom stacks.
  # scope "/api", ForgeSwapWeb do
  #   pipe_through :api
  # end
end
