defmodule ForgeSwap.Repo do
  @moduledoc """
  Dispatch the Repo
  """

  require Logger

  def set_module(module) do
    if :ets.whereis(:forge_swap) == :undefined do
      :ets.new(:forge_swap, [:named_table])
    end

    :code.ensure_loaded(module)
    :ets.insert(:forge_swap, {:repo, module})
  end

  def get_module() do
    [{:repo, module}] = :ets.lookup(:forge_swap, :repo)
    module
  end

  def unquote(:"$handle_undefined_function")(func, args) do
    case :ets.lookup(:forge_swap, :repo) do
      [{:repo, module}] -> apply(module, func, args)
      _ -> nil
    end
  rescue
    e ->
      Logger.warn(
        "Repo failure: Function name: #{inspect(func)}. Arguments: #{inspect(args)}. Error: #{
          inspect(e)
        } "
      )
  end
end
