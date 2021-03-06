defmodule PrometheusPush.Mixfile do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :prometheus_push,
      version: @version,
      elixir: "~> 1.3",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      name: "Prometheus Push",
      deps: deps(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [coveralls: :test, "coveralls.html": :test],
      docs: [
        main: Prometheus,
        source_ref: "v#{@version}",
        source_url: "https://github.com/deadtrickster/prometheus-push",
        extras: []
      ]
    ]
  end

  def application do
    [applications: [:logger, :prometheus_ex], extra_applications: [:inets, :ssl]]
  end

  defp description do
    """
    Prometheus Pushgateway client
    """
  end

  defp package do
    [
      maintainers: ["Ilya Khaprov"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/deadtrickster/prometheus-push",
        "Prometheus.erl" => "https://hex.pm/packages/prometheus",
        "Prometheus.ex" => "https://hex.pm/packages/prometheus_ex",
        "Inets HTTPD Exporter" => "https://hex.pm/packages/prometheus_httpd",
        "Ecto Instrumenter" => "https://hex.pm/packages/prometheus_ecto",
        "Phoenix Instrumenter" => "https://hex.pm/packages/prometheus_phoenix",
        "Plugs Instrumenter/Exporter" => "https://hex.pm/packages/prometheus_plugs",
        "Process info Collector" => "https://hex.pm/packages/prometheus_process_collector"
      }
    ]
  end

  defp deps do
    [
      {:prometheus_ex, "~> 3.0"},
      {:credo, "~> 1.1.0", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.15", only: :dev},
      {:earmark, ">= 0.0.0", only: :dev}
    ]
  end
end
