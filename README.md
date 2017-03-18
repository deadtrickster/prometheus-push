

# prometheus_push #

Copyright (c) 2017 Ilya Khaprov <<i.khaprov@gmail.com>>.

__Version:__ 0.0.1

[![Hex.pm][Hex badge]][Hex link]
[![Hex.pm Downloads][Hex downloads badge]][Hex link]
[![Build Status][Travis badge]][Travis link]
[![Coverage Status][Coveralls badge]][Coveralls link]

## Example

Erlang

```erlang

prometheus_counter:new([{name, qwe},
                        {labels, []},
                        {help, "qwe qwe"}]).
prometheus_counter:inc(qwe).

prometheus_counter:new([{name, foo},
                        {labels, []},
                        {help, "foo foo"}]).
prometheus_counter:inc(foo, 10).
prometheus_push:push(#{job => "qwe",
                       grouping_key => [{"abra", "kadabra"}]}).

```

Elixir

```elixir


use Prometheus.Metric

Counter.new([name: :qwe,
             labels: [],
             help: "qwe qwe"])
Counter.inc(:qwe)

Counter.new([name: :foo,
             labels: [],
             help: "foo foo"])
Counter.inc(:foo, 10)

Prometheus.Push.push(%{job: "qwe",
                       grouping_key: [{"abra", "kadabra"}]})

```

## Contributing

Section order:

- Types
- Macros
- Callbacks
- Public API
- Deprecations
- Private Parts

Install the `git` pre-commit hook:

```bash

./bin/pre-commit.sh install

```

The pre-commit check can be skipped by passing `--no-verify` to `git commit`.

## License

MIT

[Hex badge]: https://img.shields.io/hexpm/v/prometheus_push.svg?maxAge=2592000?style=plastic
[Hex link]: https://hex.pm/packages/prometheus_push
[Hex downloads badge]: https://img.shields.io/hexpm/dt/prometheus_push.svg?maxAge=2592000
[Travis badge]: https://travis-ci.org/deadtrickster/prometheus_push.svg?branch=version-3
[Travis link]: https://travis-ci.org/deadtrickster/prometheus_push
[Coveralls badge]: https://coveralls.io/repos/github/deadtrickster/prometheus_push/badge.svg?branch=master
[Coveralls link]: https://coveralls.io/github/deadtrickster/prometheus_push?branch=master


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/deadtrickster/prometheus_push/blob/master/doc/prometheus_push.md" class="module">prometheus_push</a></td></tr></table>

