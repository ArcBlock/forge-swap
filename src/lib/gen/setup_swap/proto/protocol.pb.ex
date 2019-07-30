defmodule ForgeAbi.SetupSwapTx do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          value: ForgeAbi.BigUint.t() | nil,
          assets: [String.t()],
          receiver: String.t(),
          hashlock: binary,
          locktime: non_neg_integer,
          data: Google.Protobuf.Any.t() | nil
        }
  defstruct [:value, :assets, :receiver, :hashlock, :locktime, :data]

  field(:value, 1, type: ForgeAbi.BigUint)
  field(:assets, 2, repeated: true, type: :string)
  field(:receiver, 3, type: :string)
  field(:hashlock, 4, type: :bytes)
  field(:locktime, 5, type: :uint32)
  field(:data, 15, type: Google.Protobuf.Any)
end
