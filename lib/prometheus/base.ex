defmodule Prometheus.Base do
  @moduledoc false

  @callback push(Map.t()) ::
              :ok
              | {:error, {:missing_option, String.t()}}
              | {:error, {:unknown_registry, String.t()}}
              | {:error, {:to_string_failed, String.t()}}
  @callback push(String.t()) ::
              :ok
              | {:error, {:missing_option, String.t()}}
              | {:error, {:unknown_registry, String.t()}}
              | {:error, {:to_string_failed, String.t()}}

  @callback add(Map.t()) ::
              :ok
              | {:error, {:missing_option, String.t()}}
              | {:error, {:unknown_registry, String.t()}}
              | {:error, {:to_string_failed, String.t()}}
  @callback add(String.t()) ::
              :ok
              | {:error, {:missing_option, String.t()}}
              | {:error, {:unknown_registry, String.t()}}
              | {:error, {:to_string_failed, String.t()}}

  @callback delete(Map.t()) ::
              :ok
              | {:error, {:missing_option, String.t()}}
              | {:error, {:unknown_registry, String.t()}}
              | {:error, {:to_string_failed, String.t()}}
  @callback delete(String.t()) ::
              :ok
              | {:error, {:missing_option, String.t()}}
              | {:error, {:unknown_registry, String.t()}}
              | {:error, {:to_string_failed, String.t()}}
end
