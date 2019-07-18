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

    default |> Map.merge(change) |> parse()
  end

  # Traverse the config map and replace values by system environment variables.
  defp parse(config) when is_map(config) do
    config
    |> Map.to_list()
    |> Enum.into(%{}, fn {key, val} -> {key, parse(val)} end)
  end

  defp parse("SYSTEM:" <> env) do
    case System.get_env(env) do
      val when val in [nil, ""] ->
        raise "Could not read system environment variable #{inspect(env)}."

      val ->
        val
    end
  end

  defp parse(val), do: val
end
