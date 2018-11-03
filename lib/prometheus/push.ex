defmodule Prometheus.Push do

  @moduledoc """
    Prometheus.Push works with the [prometheus.ex](https://github.com/deadtrickster/prometheus.ex/) library to push metrics to a [Pushgateway](https://github.com/prometheus/pushgateway).
    The Pushgateway is used for short running jobs that may not give Prometheus enough time to scrape metrics.

    To use this module [create a metric](https://hexdocs.pm/prometheus_ex/Prometheus.Metric.html#content), then `push` when appropriate.
  """

  @doc """
  Pushes metrics to the Pushgateway

  ## Examples

      iex> Prometheus.Push.push(%{job: "qwe",
                       grouping_key: [{"abra", "kadabra"}]})

  """
  def push(options) do
    :prometheus_push.push(options)
  end

  def add(options) do
    :prometheus_push.add(options)
  end

  def delete(options) do
    :prometheus_push.delete(options)
  end
end
