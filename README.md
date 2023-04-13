# `eventlog-live` Infrastructure

This repository contains a [Docker Compose][docs:docker-compose] configuration
that defines infrastructure for real-time monitoring of Haskell programs via
[`ghc-eventlog-socket`][ghc-eventlog-socket] and
[`eventlog-live`][eventlog-live]. The infrastructure includes an
[InfluxDB][docs:influxdb] database that gets populated with metrics from the
eventlog, and a [Grafana][docs:grafana] instance that offers interactive
visualizations the eventlog metrics.

![](./assets/img/dashboard.png)

## Get Started



[docs:docker-compose]: https://docs.docker.com/compose/
[docs:influxdb]: https://www.influxdata.com/
[docs:grafana]: https://grafana.com/

[eventlog-live]: https://github.com/mpickering/eventlog-live
[ghc-eventlog-socket]: https://github.com/bgamari/ghc-eventlog-socket