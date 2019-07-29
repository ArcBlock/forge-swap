defmodule ForgeAbi.RetrieveSwapTx do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          address: String.t(),
          hashkey: binary,
          data: Google.Protobuf.Any.t() | nil
        }
  defstruct [:address, :hashkey, :data]

  field :address, 1, type: :string
  field :hashkey, 2, type: :bytes
  field :data, 15, type: Google.Protobuf.Any
end
