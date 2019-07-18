use Mix.Config

# Configure your database
config :forge_swap, ForgeSwap.PostgresRepo,
  username: "postgres",
  password: "postgres",
  database: "forge_swap_test",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :forge_swap, ForgeSwapWeb.Endpoint, server: false

# Print only warnings and errors during test
config :logger, level: :warn
