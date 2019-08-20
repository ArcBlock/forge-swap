defmodule ForgeAbi.DeclareTx do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          moniker: String.t(),
          issuer: String.t(),
          data: Google.Protobuf.Any.t() | nil
        }
  defstruct [:moniker, :issuer, :data]

  field :moniker, 1, type: :string
  field :issuer, 2, type: :string
  field :data, 15, type: Google.Protobuf.Any
end
