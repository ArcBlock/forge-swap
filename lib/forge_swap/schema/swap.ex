defmodule ForgeSwap.Schema.Swap do
  @moduledoc """
  Represents a swap between applciation and user
  """
  use Ecto.Schema

  import Ecto.Changeset
  import Ecto.Query

  alias ForgeSwap.{Repo, Schema.Swap}

  schema("swap") do
    field(:user_did, :string)
    field(:status, :string)
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
    params = Map.put(params, :status, "not_started")

    %ForgeSwap.Schema.Swap{}
    |> cast(params, [
      :user_did,
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
      :status,
      :offer_chain,
      :offer_locktime,
      :demand_chain,
      :demand_locktime
    ])
  end

  def update_changeset(data, params) do
    data
    |> cast(params, [:status, :offer_swap, :deman_swap])
    |> validate_required([:status])
  end

  def get(id) do
    query = from(s in Swap, where: s.id == ^id)

    Repo
    |> apply(:one, [query])
  end
end
