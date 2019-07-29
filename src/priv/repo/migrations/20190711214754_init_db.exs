defmodule ForgeSwap.Repo.Migrations.InitDb do
  use Ecto.Migration

  def change do
    create table(:swap) do
      add(:user_did, :string, null: false)
      add(:asset_owner, :string, null: false)
      add(:status, :string, null: false)
      add(:retrieve_hash, :string)
      add(:revoke_hash, :string)
      add(:offer_assets, {:array, :string})
      add(:offer_token, :decimal)
      add(:offer_chain, :string, null: false)
      add(:offer_locktime, :integer, null: false)
      add(:offer_swap, :string)
      add(:demand_assets, {:array, :string})
      add(:demand_token, :decimal)
      add(:demand_chain, :string, null: false)
      add(:demand_locktime, :integer, null: false)
      add(:demand_swap, :string)
    end
  end
end
