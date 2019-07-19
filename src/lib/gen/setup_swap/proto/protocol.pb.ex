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

  field :value, 1, type: ForgeAbi.BigUint
  field :assets, 2, repeated: true, type: :string
  field :receiver, 3, type: :string
  field :hashlock, 4, type: :bytes
  field :locktime, 5, type: :uint32
  field :data, 15, type: Google.Protobuf.Any
end

defmodule ForgeAbi.SwapState do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          hash: String.t(),
          address: String.t(),
          hashkey: binary,
          sender: String.t(),
          receiver: String.t(),
          value: ForgeAbi.BigUint.t() | nil,
          assets: [String.t()],
          locktime: non_neg_integer,
          hashlock: binary,
          context: ForgeAbi.StateContext.t() | nil
        }
  defstruct [
    :hash,
    :address,
    :hashkey,
    :sender,
    :receiver,
    :value,
    :assets,
    :locktime,
    :hashlock,
    :context
  ]

  field :hash, 1, type: :string
  field :address, 2, type: :string
  field :hashkey, 3, type: :bytes
  field :sender, 4, type: :string
  field :receiver, 5, type: :string
  field :value, 6, type: ForgeAbi.BigUint
  field :assets, 7, repeated: true, type: :string
  field :locktime, 8, type: :uint32
  field :hashlock, 9, type: :bytes
  field :context, 10, type: ForgeAbi.StateContext
end
