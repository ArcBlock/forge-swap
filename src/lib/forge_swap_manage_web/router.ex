defmodule ForgeSwapManageWeb.Router do
  use ForgeSwapManageWeb, :router

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

  scope "/", ForgeSwapManageWeb do
    pipe_through(:browser)

    get("/", SwapManageController, :index)
  end

  scope "/api", ForgeSwapManageWeb do
    pipe_through(:api)

    post("/swap/", SwapManageController, :create)
  end

  # Other scopes may use custom stacks.
  # scope "/api", ForgeSwapManageWeb do
  #   pipe_through :api
  # end
end
