defmodule Prometheus.Push do

  @moduledoc """

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
