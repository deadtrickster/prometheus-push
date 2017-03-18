@copyright 2017 Ilya Khaprov <<i.khaprov@gmail.com>>.
@title Prometheus.io Pushgateway client.
@version 0.0.1

@doc

[![Hex.pm][Hex badge]][Hex link]
[![Hex.pm Downloads][Hex downloads badge]][Hex link]
[![Build Status][Travis badge]][Travis link]
[![Coverage Status][Coveralls badge]][Coveralls link]

- IRC: #erlang on Freenode;
- [Slack](https://elixir-slackin.herokuapp.com/): #prometheus channel - [Browser](https://elixir-lang.slack.com/messages/prometheus) or App(slack://elixir-lang.slack.com/messages/prometheus).

## Example

Erlang

<pre lang="erlang">
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
</pre>

Elixir

<pre lang="elixir">

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
</pre>

## Integrations
- [Ecto Instrumenter](https://hex.pm/packages/prometheus_ecto)
- [Erlang client](https://github.com/deadtrickster/prometheus.erl)
- [Elixir client](https://github.com/deadtrickster/prometheus.ex)
- [Elixir plugs Instrumenters and Exporter](https://hex.pm/packages/prometheus_plugs)
- [Extatus - App to report metrics to Prometheus from Elixir GenServers](https://github.com/gmtprime/extatus)
- [Fuse plugin](https://github.com/jlouis/fuse#fuse_stats_prometheus)
- [Inets HTTPD Exporter](https://github.com/deadtrickster/prometheus_httpd)
- [OS process info Collector](https://hex.pm/packages/prometheus_process_collector) (linux-only)
- [Phoenix Instrumenter](https://hex.pm/packages/prometheus_phoenix)
- [RabbitMQ Exporter](https://github.com/deadtrickster/prometheus_rabbitmq_exporter).

## Dashboards

- [Beam Dashboards](https://github.com/deadtrickster/beam-dashboards).

## Blogs

- [Monitoring Elixir apps in 2016: Prometheus and Grafana](https://aldusleaf.org/monitoring-elixir-apps-in-2016-prometheus-and-grafana/)
- [A Simple Erlang Application, with Prometheus](http://markbucciarelli.com/2016-11-23_a_simple_erlang_application_with_prometheus.html).

## Contributing

Section order:

- Types
- Macros
- Callbacks
- Public API
- Deprecations
- Private Parts

Install the `git' pre-commit hook:

<pre lang="bash">
./bin/pre-commit.sh install
</pre>

The pre-commit check can be skipped by passing `--no-verify' to `git commit'.

## License

MIT

<!-- Named Links -->

[Hex badge]: https://img.shields.io/hexpm/v/prometheus_push.svg?maxAge=2592000?style=plastic
[Hex link]: https://hex.pm/packages/prometheus_push
[Hex downloads badge]: https://img.shields.io/hexpm/dt/prometheus_push.svg?maxAge=2592000
[Travis badge]: https://travis-ci.org/deadtrickster/prometheus_push.svg?branch=version-3
[Travis link]: https://travis-ci.org/deadtrickster/prometheus_push
[Coveralls badge]: https://coveralls.io/repos/github/deadtrickster/prometheus_push/badge.svg?branch=master
[Coveralls link]: https://coveralls.io/github/deadtrickster/prometheus_push?branch=master
