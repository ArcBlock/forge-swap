defmodule ForgeSwapWeb.Router do
  use ForgeSwapWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug Phoenix.LiveView.Flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", ForgeSwapWeb do
    pipe_through :browser

    get "/", PageController, :index
    post "/pay", PageController, :pay
    get "/swap/:id", SwapController, :show
  end

  scope "/api", ForgeSwapWeb do
    pipe_through(:api)

    post("/swap/", SwapController, :create)
    post("/swap/:id/start", SwapController, :start)
  end

  # Other scopes may use custom stacks.
  # scope "/api", ForgeSwapWeb do
  #   pipe_through :api
  # end
end
