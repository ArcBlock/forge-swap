defmodule ForgeSwap.Schema.Swap do
  @moduledoc """
  Represents a swap between applciation and user
  """
  use Ecto.Schema

  import Ecto.Changeset
  import Ecto.Query

  alias ForgeSwap.{Repo, Schema.Swap}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema("swap") do
    field(:user_did, :string)
    field(:asset_owner, :string)
    field(:status, :string)
    field(:retrieve_hash, :string)
    field(:revoke_hash, :string)
    field(:offer_assets, {:array, :string})
    field(:offer_token, :decimal)
    field(:offer_chain, :string)
    field(:offer_locktime, :integer)
    field(:offer_swap, :string)
    field(:demand_assets, {:array, :string})
    field(:demand_token, :decimal)
    field(:demand_chain, :string)
    field(:demand_locktime, :integer)
    field(:demand_swap, :string)
  end

  def insert_changeset(params) do
    %ForgeSwap.Schema.Swap{}
    |> cast(params, [
      :user_did,
      :asset_owner,
      :status,
      :offer_assets,
      :offer_token,
      :offer_chain,
      :offer_locktime,
      :demand_assets,
      :demand_token,
      :demand_chain,
      :demand_locktime
    ])
    |> validate_required([
      :user_did,
      :asset_owner,
      :status,
      :offer_chain,
      :offer_locktime,
      :demand_chain,
      :demand_locktime
    ])
  end

  def update_changeset(data, params) do
    data
    |> cast(params, [:status, :offer_swap, :demand_swap, :retrieve_hash, :revoke_hash])
    |> validate_required([:status])
  end

  def get(id) do
    query = from(s in Swap, where: s.id == ^id)

    Repo
    |> apply(:one, [query])
  end

  # def get_by_status(status) do
  #   query = from(s in Swap, where: s.status == ^status)

  #   Repo
  #   |> apply(:all, [query])
  # end
end
