use Mix.Config

config :forge_swap, :env, :integration

# Configure your database
config :forge_swap, ForgeSwap.PostgresRepo,
  username: "postgres",
  password: "postgres",
  database: "forge_swap_inte",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox

# Print only warnings and errors during test
config :logger, level: :info
