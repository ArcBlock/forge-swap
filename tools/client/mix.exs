defmodule Client.MixProject do
  use Mix.Project

  @top "../../"
  @version @top |> Path.join("version") |> File.read!() |> String.trim()
  @elixir_version @top |> Path.join(".elixir_version") |> File.read!() |> String.trim()

  def project do
    [
      app: :client,
      version: @version,
      elixir: @elixir_version,
      deps_path: Path.join(@top, "deps"),
      build_path: Path.join(@top, "_build"),
      lockfile: Path.join(@top, "src/mix.lock"),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Client.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
      {:httpoison, "~> 1.4"},
      {:abt_did_elixir, "~> 0.3"},
      {:multibase, ">= 0.0.0"}
    ]
  end
end
