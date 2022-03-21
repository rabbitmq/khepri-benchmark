%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(khepri_benchmark).
-behaviour(cli).

-export([main/1,
         run/0,
         cli/0]).

main(Args) ->
    ScriptName = filename:basename(escript:script_name()),
    _Out = cli:run(Args, #{progname => ScriptName}),
    ok.

cli() ->
    #{handler => fun run/1,
      arguments =>
      [
       #{name => bench_inserts,
         long => "-bench-inserts",
         help => "Measure performance of insertions",
         type => boolean},
       #{name => bench_queries,
         long => "-bench-queries",
         help => "Measure performance of queries",
         type => boolean},
       #{name => bench_deletes,
         long => "-bench-deletes",
         help => "Measure performance of deletions",
         type => boolean},

       #{name => bench_khepri_safe,
         long => "-bench-khepri-safe",
         help =>
         "Measure insert/delete performance of Khepri "
         "(using safe/default settings)",
         type => boolean},
       #{name => bench_khepri_unsafe,
         long => "-bench-khepri-unsafe",
         help =>
         "Measure insert/delete performance of Khepri "
         "(using less safe settings)",
         type => boolean},
       #{name => bench_khepri_low_latency,
         long => "-bench-khepri-local",
         help =>
         "Measure query performance of Khepri (local queries)",
         type => boolean},
       #{name => bench_khepri_compromise,
         long => "-bench-khepri-compromise",
         help =>
         "Measure query performance of Khepri (leader+consistent queries)",
         type => boolean},
       #{name => bench_khepri_consistent,
         long => "-bench-khepri-consistent",
         help =>
         "Measure query performance of Khepri (consistent queries)",
         type => boolean},
       #{name => bench_mnesia,
         long => "-bench-mnesia",
         help => "Measure performance of Mnesia",
         type => boolean},

       #{name => bench_single_node,
         long => "-bench-single-node",
         help => "Measure performance with a single Erlang node",
         type => boolean},
       #{name => bench_cluster,
         long => "-bench-cluster",
         help => "Measure performance with an Erlang cluster",
         type => boolean},
       #{name => cluster_size,
         long => "-cluster-size",
         help => "Set the number of nodes to start for cluster benchmarks",
         type => {int, [{min, 2}]}},

       #{name => max_workers,
         long => "-max-workers",
         help => "Set the maximum number of concurrent workers",
         type => {int, [{min, 1}]}},
       #{name => sampling,
         long => "-sampling",
         help => "Set the sampling duration in seconds",
         type => {int, [{min, 1}]}},
       #{name => multiple_of,
         long => "-workers-bump-step",
         help => "Set the number of workers to add after each sampling phase",
         type => {int, [{min, 1}]}}
      ]
     }.

run() ->
    run(#{}).

run(Options) ->
    io:setopts([{encoding, unicode}]),
    logger:set_primary_config(level, error),
    SystemInfo = khepri_benchmark_system:info(),

    {ok, _} = application:ensure_all_started(khepri_benchmark),

    ThisNode = node(),
    SingleNode = [ThisNode],

    BenchSingleNode = maps:get(bench_single_node, Options, true),
    BenchCluster = maps:get(bench_cluster, Options, true),
    BenchInserts = maps:get(bench_inserts, Options, true),
    BenchQueries = maps:get(bench_queries, Options, true),
    BenchDeletes = maps:get(bench_deletes, Options, true),

    SingleNodeBenchmarks =
    if
        BenchSingleNode ->
            BMs4 = [],
            BMs5 = if
                       BenchDeletes ->
                           [{"Deletes",
                             SingleNode,
                             list_benchmarks(deletes, SingleNode, Options)}
                            | BMs4];
                       true ->
                           BMs4
                   end,
            BMs6 = if
                       BenchQueries ->
                           [{"Queries",
                             SingleNode,
                             list_benchmarks(queries, SingleNode, Options)}
                            | BMs5];
                       true ->
                           BMs5
                   end,
            BMs7 = if
                       BenchInserts ->
                           [{"Inserts",
                             SingleNode,
                             list_benchmarks(inserts, SingleNode, Options)}
                            | BMs6];
                       true ->
                           BMs6
                   end,
            BMs7;
        true ->
            []
    end,

    ClusterBenchmarks =
    if
        BenchCluster andalso ThisNode =/= nonode@nohost ->
            ClusterSize = maps:get(cluster_size, Options, 3),
            OtherNodes = khepri_benchmark_utils:setup_cluster(ClusterSize),
            Cluster = [ThisNode | OtherNodes],
            Label = khepri_benchmark_utils:cluster_label(ClusterSize),

            BMs0 = [],
            BMs1 = if
                      BenchDeletes ->
                          [{"Deletes, " ++ Label,
                            Cluster,
                            list_benchmarks(deletes, Cluster, Options)}
                           | BMs0];
                      true ->
                          BMs0
                  end,
            BMs2 = if
                      BenchQueries ->
                          [{"Queries, " ++ Label,
                            Cluster,
                            list_benchmarks(queries, Cluster, Options)}
                           | BMs1];
                      true ->
                          BMs1
                  end,
            BMs3 = if
                      BenchInserts ->
                          [{"Inserts, " ++ Label,
                            Cluster,
                            list_benchmarks(inserts, Cluster, Options)}
                           | BMs2];
                      true ->
                          BMs2
                  end,
            BMs3;
        true ->
            []
    end,

    Benchmarks = SingleNodeBenchmarks ++ ClusterBenchmarks,

    MaxWorkers = maps:get(max_workers, Options, 200),
    Sampling = maps:get(sampling, Options, 3),
    MultipleOf = maps:get(multiple_of, Options, 10),
    RunOptions = #{warmup => 1,
                   samples => Sampling,
                   report => extended},
    ConcurrencyOptions = #{min => 1,
                           max => MaxWorkers,
                           threshold => MaxWorkers,
                           multiple_of => MultipleOf},

    Results = run_benchmarks(Benchmarks, RunOptions, ConcurrencyOptions),

    khepri_benchmark_output:print_results(Options, SystemInfo, Results),
    khepri_benchmark_output:generate_html(Options, SystemInfo, Results),

    khepri_benchmark_utils:cleanup_cluster(),
    ok.

list_benchmarks(What, Nodes, Options)
  when What =:= inserts orelse What =:= deletes ->
    BenchKhepriSafe = maps:get(bench_khepri_safe, Options, true),
    BenchKhepriUnsafe = maps:get(bench_khepri_unsafe, Options, true),
    BenchMnesia = maps:get(bench_mnesia, Options, true),
    Benchmarks0 = [],
    Benchmarks1 = if
                      BenchKhepriSafe andalso What =:= inserts ->
                          Benchmarks0 ++
                          [khepri_benchmark_khepri:insert_benchmark(
                             Nodes, safe)];
                      BenchKhepriSafe andalso What =:= deletes ->
                          Benchmarks0 ++
                          [khepri_benchmark_khepri:delete_benchmark(
                             Nodes, safe)];
                      true ->
                          Benchmarks0
                  end,
    Benchmarks2 = if
                      BenchKhepriUnsafe andalso What =:= inserts ->
                          Benchmarks1 ++
                          [khepri_benchmark_khepri:insert_benchmark(
                             Nodes, unsafe)];
                      BenchKhepriUnsafe andalso What =:= deletes ->
                          Benchmarks1 ++
                          [khepri_benchmark_khepri:delete_benchmark(
                             Nodes, unsafe)];
                      true ->
                          Benchmarks1
                  end,
    Benchmarks3 = if
                      BenchMnesia andalso What =:= inserts ->
                          Benchmarks2 ++
                          [khepri_benchmark_mnesia:insert_benchmark(Nodes)];
                      BenchMnesia andalso What =:= deletes ->
                          Benchmarks2 ++
                          [khepri_benchmark_mnesia:delete_benchmark(Nodes)];
                      true ->
                          Benchmarks2
                  end,
    Benchmarks3;
list_benchmarks(queries, Nodes, Options) ->
    BenchKhepriLowLat = maps:get(bench_khepri_low_latency, Options, true),
    BenchKhepriCompromise = maps:get(bench_khepri_compromise, Options, true),
    BenchKhepriConsistent = maps:get(bench_khepri_consistent, Options, true),
    BenchMnesia = maps:get(bench_mnesia, Options, true),
    Benchmarks0 = [],
    Benchmarks1 = if
                      BenchKhepriLowLat ->
                          Benchmarks0 ++
                          [khepri_benchmark_khepri:query_benchmark(
                             Nodes, low_latency)];
                      true ->
                          Benchmarks0
                  end,
    Benchmarks2 = if
                      BenchKhepriCompromise ->
                          Benchmarks1 ++
                          [khepri_benchmark_khepri:query_benchmark(
                             Nodes, compromise)];
                      true ->
                          Benchmarks1
                  end,
    Benchmarks3 = if
                      BenchKhepriConsistent ->
                          Benchmarks2 ++
                          [khepri_benchmark_khepri:query_benchmark(
                             Nodes, consistency)];
                      true ->
                          Benchmarks2
                  end,
    Benchmarks4 = if
                      BenchMnesia ->
                          Benchmarks3 ++
                          [khepri_benchmark_mnesia:query_benchmark(Nodes)];
                      true ->
                          Benchmarks3
                  end,
    Benchmarks4.

run_benchmarks(Benchmarks, RunOptions, ConcurrencyOptions) ->
    run_benchmarks(Benchmarks, RunOptions, ConcurrencyOptions, []).

run_benchmarks(
  [{Name, Nodes, Benchmarks} | Rest],
  RunOptions, ConcurrencyOptions, Results) ->
    io:format("~nBenchmarking ~ts:", [Name]),
    Result = run_benchmarks1(
               Benchmarks, Nodes, RunOptions, ConcurrencyOptions, []),
    Results1 = [{Name, Result} | Results],
    run_benchmarks(Rest, RunOptions, ConcurrencyOptions, Results1);
run_benchmarks([], _RunOptions, _ConcurrencyOptions, Results) ->
    lists:reverse(Results).

run_benchmarks1(
  [#{name := Name} = Benchmark | Rest],
  Nodes, RunOptions, ConcurrencyOptions, Results) ->
    io:format(" ~ts...", [Name]),
    Parent = self(),
    Monitor = spawn_link(
                fun() ->
                        true = register(kb_resource_monitor, self()),
                        receive go -> ok end,

                        Monitoring = monitor_resources(Nodes),
                        unlink(Parent),
                        Parent ! {monitor, Monitoring}
                end),
    _Runner = spawn_link(
                fun() ->
                        Result = do_run_benchmark(
                                   Benchmark, RunOptions, ConcurrencyOptions),
                        unlink(Parent),
                        Parent ! {result, Result}
                end),
    {Result, Monitoring} = receive
                               {result, R} ->
                                   Monitor ! stop,
                                   receive
                                       {monitor, M} -> {R, M}
                                   end
                           end,
    Results1 = [{Name, Result, Monitoring} | Results],
    run_benchmarks1(Rest, Nodes, RunOptions, ConcurrencyOptions, Results1);
run_benchmarks1([], _Nodes, _RunOptions, _ConcurrencyOptions, Results) ->
    lists:reverse(Results).

do_run_benchmark(Benchmark, RunOptions, ConcurrencyOptions) ->
    {_Max, Result} = erlperf:run(Benchmark, RunOptions, ConcurrencyOptions),
    lists:reverse(Result).

monitor_resources(Nodes) ->
    Samples = [],
    monitor_resources(Nodes, 0, Samples).

monitor_resources(Nodes, T, Samples) ->
    Sample = [#{node => Node,
                memory => rpc:call(Node, erlang, memory, []),
                gc => rpc:call(Node, erlang, statistics, [garbage_collection])}
              || Node <- Nodes],
    Samples1 = [{T, Sample} | Samples],
    receive
        stop ->
            lists:reverse(Samples1)
    after 1000 ->
              monitor_resources(Nodes, T + 1, Samples1)
    end.
