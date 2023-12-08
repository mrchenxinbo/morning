defmodule MsyncProto.MixProject do
  use Mix.Project

  def project do
    [
      app: :msync_proto,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      erlc_options: erlc_options(),
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:lager, :logger],
      applications: [:lager]
    ]
  end

  defp erlc_options do
    # Use our own includes + includes from all dependencies
    includes = ["include", "../message_store/include"] ++ deps_include(["gpb"])
    result = [:debug_info, {:d, :OTP23}] ++
      Enum.map(includes, fn (path) -> {:i, path} end)
    defines = for {:d, value} <- result, do: {:d, value}
    result ++ [{:d, :ALL_DEFS, defines}]
  end

  defp deps_include(deps) do
    base = "../../deps"
    Enum.map(deps, fn dep -> base<>"/#{dep}/include" end)
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:gpb, "~> 4.19"},
      {:fast_xml, github: "processone/fast_xml", ref: "4e8d0ba4ea08d642acf63b25c65f89a6f632ea3b", override: true},
      {:lager, github: "erlang-lager/lager", ref: "459a3b2cdd9eadd29e5a7ce5c43932f5ccd6eb88", override: true}
    ]
  end
end
