#!/usr/bin/env sh

rebar3 do lint, xref, dialyzer, eunit && MIX_ENV=test mix credo --strict
