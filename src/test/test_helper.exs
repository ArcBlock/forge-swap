ExUnit.start()
Ecto.Adapters.SQL.Sandbox.mode(ForgeSwap.Repo.get_module(), :manual)

if Application.get_env(:forge_swap, :env) == :integration do
  ForgeSdk.connect("tcp://127.0.0.1:10020", name: "asset_chain", default: true)
  ForgeSdk.connect("tcp://127.0.0.1:10120", name: "app_chain", default: false)

  user = %{
    address: "z1SWvGbnCiJf5MpWnV8qoYQWrYnK2BdtpaQ",
    pk:
      <<248, 202, 174, 37, 243, 223, 249, 206, 99, 222, 173, 12, 97, 245, 149, 210, 123, 29, 16,
        101, 60, 167, 206, 217, 141, 96, 75, 48, 20, 159, 173, 66>>,
    sk:
      <<130, 122, 74, 55, 242, 33, 15, 235, 96, 163, 197, 239, 122, 63, 172, 44, 67, 33, 94, 158,
        32, 98, 206, 163, 30, 151, 42, 71, 169, 214, 132, 178, 248, 202, 174, 37, 243, 223, 249,
        206, 99, 222, 173, 12, 97, 245, 149, 210, 123, 29, 16, 101, 60, 167, 206, 217, 141, 96,
        75, 48, 20, 159, 173, 66>>
  }

  owner = %{
    address: "z1ewYeWM7cLamiB6qy6mDHnzw1U5wEZCoj7",
    pk:
      <<117, 203, 141, 163, 125, 31, 190, 56, 26, 215, 25, 10, 206, 28, 135, 228, 209, 49, 42,
        104, 155, 38, 5, 244, 194, 122, 44, 158, 28, 230, 60, 197>>,
    sk:
      <<180, 193, 254, 213, 9, 13, 214, 69, 24, 194, 14, 175, 95, 22, 54, 203, 76, 42, 104, 69,
        106, 148, 81, 97, 25, 38, 53, 239, 184, 60, 103, 82, 117, 203, 141, 163, 125, 31, 190, 56,
        26, 215, 25, 10, 206, 28, 135, 228, 209, 49, 42, 104, 155, 38, 5, 244, 194, 122, 44, 158,
        28, 230, 60, 197>>
  }

  itx = apply(ForgeAbi.DeclareTx, :new, [[moniker: "user"]])
  ForgeSdk.declare(itx, wallet: user, conn: "app_chain", send: :commit)

  itx = apply(ForgeAbi.DeclareTx, :new, [[moniker: "owner"]])
  ForgeSdk.declare(itx, wallet: owner, conn: "asset_chain", send: :commit)
end
