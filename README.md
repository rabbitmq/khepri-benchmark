# Khepri benchmark

This application is a micro-benchmarking tool to compare Khepri and Mnesia with
varying concurrency and number of Erlang nodes.

## How to run the benchmark

```sh
rebar3 escriptize
_build/default/bin/khepri-benchmark
```

## How does it work?

The following "backends" are compared:
* Khepri with the default safe settings in Ra
* Khepri with `fsync(2)` disabled in Ra (unsafe)
* Mnesia with the default settings and a disk-copy table

Note that the level of guaranty with Mnesia is closer to Khepri's unsafe
setting in case of power outage and no replication.

Each micro-benchmark (insertions, deletes, ...) is performed for each backend
with concurrency gradually increasing every 4 seconds, starting with 1 worker
(no concurrency), going up to 200 parallel workers. Concurrency is increased by
10 workers a time. There is a 1 second warmup phase before starting a 3-second
sampling phase.

The reported number of operations per second corresponds to the average during
those 3 seconds before more workers are spawned.

Here is an example:

```
T+0:  1 worker, warmup
T+1:  -> start sampling
T+4:  -> stop sampling, save average ops/s for 1 worker
      10 workers, warmup
T+5:  -> start sampling
T+8:  -> stop sampling, save average ops/s for 10 workers
      20 workers, warmup
T+9:  -> start sampling
T+12: -> stop sampling, save average ops/s for 20 workers
      30 workers, warmup
T+13: -> start sampling
...
```

The whole benchmark is executed in the following conditions:
1. in a single Erlang node
2. in a 3-node cluster (note that all three nodes run on the same host)

## Reports

The script will first display a report on stdout. Here is an example:

```
Starting additional nodes to create a 3-node cluster...

Benchmarking Inserts: Khepri (safe)... Khepri (unsafe)... Mnesia...
Benchmarking Deletes: Khepri (safe)... Khepri (unsafe)... Mnesia...
Benchmarking Inserts, 3-node cluster: Khepri (safe)... Khepri (unsafe)... Mnesia...
Benchmarking Deletes, 3-node cluster: Khepri (safe)... Khepri (unsafe)... Mnesia...


System information:
  OS: FreeBSD
  CPU: Intel(R) Core(TM) i7-9850H CPU @ 2.60GHz
  Number of cores: 12
  Memory: 32 GiB
  Erlang: 24.2.2

(...)

Inserts, 3-node cluster, no concurrency
      Khepri (safe): ■■ 567 ops/s
    Khepri (unsafe): ■■■■■■■■ 3862 ops/s
             Mnesia: ■■■■■■■■■■■ 5631 ops/s

Inserts, 3-node cluster, 50 concurrent workers
      Khepri (safe): ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 21871 ops/s
    Khepri (unsafe): ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 18011 ops/s
             Mnesia: ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 18014 ops/s

Inserts, 3-node cluster, 100 concurrent workers
      Khepri (safe): ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 25317 ops/s
    Khepri (unsafe): ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 18854 ops/s
             Mnesia: ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 19201 ops/s

Inserts, 3-node cluster, 200 concurrent workers
      Khepri (safe): ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 25851 ops/s
    Khepri (unsafe): ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 19442 ops/s
             Mnesia: ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 19537 ops/s

(...)
```

An HTML report will also be available in `public/index.html`.

## Automatic execution in CI

The benchmark is executed automatically when there is a change in Khepri's
`main` branch or in this benchmark tool. The HTML report will be published
automatically to the following page:

https://rabbitmq.github.io/khepri-benchmark/

The GitHub Actions host which runs this benchmark has 2 CPU cores only. Please
consider the published numbers with this constraint in mind.
