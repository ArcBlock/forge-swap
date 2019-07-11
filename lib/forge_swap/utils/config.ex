defmodule ForgeSwap.Utils.Config do
  def read_config() do
    if :ets.whereis(:forge_swap) == :undefined do
      :ets.new(:forge_swap, [:named_table])
    end

    case :ets.lookup(:forge_swap, :config) do
      [{:config, config}] ->
        config

      _ ->
        config = do_read_config()
        :ets.insert(:forge_swap, {:config, config})
        config
    end
  end

  def read_toml(path) do
    path
    |> File.read!()
    |> Toml.decode!()
  end

  defp do_read_config() do
    default =
      :forge_swap |> Application.app_dir() |> Path.join("priv/config/default.toml") |> read_toml()

    change =
      case System.get_env("FORGESWAP_CONFIG") do
        nil -> %{}
        path -> read_toml(path)
      end

    Map.merge(default, change)
  end
end
