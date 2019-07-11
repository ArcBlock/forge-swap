defmodule ForgeSwapWeb.PageController do
  use ForgeSwapWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
