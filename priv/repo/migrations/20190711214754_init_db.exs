defmodule ForgeSwap.Repo.Migrations.InitDb do
  use Ecto.Migration

  def change do
    create table(:swap) do
      add :user_did, :string
      add :status, :string
      add :offer_assets, {:array, :string}
      add :offer_token, :decimal
      add :offer_chain, :string
      add :offer_swap, :string
      add :offer_locktime, :integer
      add :demand_assets, {:array, :string}
      add :demand_token, :decimal
      add :demand_chain, :string
      add :demand_swap, :string
      add :demand_locktime, :integer
    end
  end
end
