defmodule ForgeAbi.RevokeSwapTx do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          address: String.t(),
          data: Google.Protobuf.Any.t() | nil
        }
  defstruct [:address, :data]

  field :address, 1, type: :string
  field :data, 15, type: Google.Protobuf.Any
end
