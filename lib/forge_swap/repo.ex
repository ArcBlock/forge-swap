defmodule ForgeSwap.Repo do
  use Ecto.Repo,
    otp_app: :forge_swap,
    adapter: Ecto.Adapters.Postgres
end
