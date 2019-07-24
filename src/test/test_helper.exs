ExUnit.start()
Ecto.Adapters.SQL.Sandbox.mode(ForgeSwap.Repo.get_module(), :manual)

ForgeSdk.connect("tcp://127.0.0.1:10020", name: "asset_chain", default: true)
ForgeSdk.connect("tcp://127.0.0.1:10120", name: "app_chain", default: false)
