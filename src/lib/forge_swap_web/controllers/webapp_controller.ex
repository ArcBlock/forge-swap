defmodule ForgeSwapWeb.WebappController do
  use ForgeSwapWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
