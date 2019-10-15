defmodule ForgeSwap.Release do
  @app :forge_swap

  def migrate do
    Application.load(@app)
    repo = ForgeSwap.Utils.Config.apply_repo_config()
    {:ok, _, _} = Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :up, all: true))
  end

  def rollback(repo, version) do
    {:ok, _, _} = Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :down, to: version))
  end
end
