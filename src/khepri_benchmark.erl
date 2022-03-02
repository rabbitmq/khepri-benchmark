%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(khepri_benchmark).

-export([main/1,
         run/0]).

main(_Args) ->
    run().

run() ->
    io:setopts([{encoding, unicode}]),
    logger:set_primary_config(level, error),
    SystemInfo = khepri_benchmark_system:info(),

    {ok, _} = application:ensure_all_started(khepri_benchmark),

    ThisNode = node(),
    SingleNode = [ThisNode],

    ClusterBenchmarks =
    case ThisNode of
        nonode@nohost ->
            [];
        _ ->
            ClusterSize = 3,
            OtherNodes = khepri_benchmark_utils:setup_cluster(ClusterSize),
            Cluster = [ThisNode | OtherNodes],

            Label = lists:flatten(
                      io_lib:format("~b-node cluster", [ClusterSize])),
            [{"Inserts, " ++ Label,
              [khepri_benchmark_khepri:insert_benchmark(Cluster, safe),
               khepri_benchmark_khepri:insert_benchmark(Cluster, unsafe),
               khepri_benchmark_mnesia:insert_benchmark(Cluster)]},
             {"Deletes, " ++ Label,
              [khepri_benchmark_khepri:delete_benchmark(Cluster, safe),
               khepri_benchmark_khepri:delete_benchmark(Cluster, unsafe),
               khepri_benchmark_mnesia:delete_benchmark(Cluster)]}]
    end,

    Benchmarks =
    [{"Inserts",
      [khepri_benchmark_khepri:insert_benchmark(SingleNode, safe),
       khepri_benchmark_khepri:insert_benchmark(SingleNode, unsafe),
       khepri_benchmark_mnesia:insert_benchmark(SingleNode)]},
     {"Deletes",
      [khepri_benchmark_khepri:delete_benchmark(SingleNode, safe),
       khepri_benchmark_khepri:delete_benchmark(SingleNode, unsafe),
       khepri_benchmark_mnesia:delete_benchmark(SingleNode)]}
     | ClusterBenchmarks],

    RunOptions = #{warmup => 1,
                   samples => 3,
                   report => extended},
    ConcurrencyOptions = #{min => 1,
                           max => 200,
                           threshold => 500,
                           multiple_of => 10},

    Results = run_benchmarks(Benchmarks, RunOptions, ConcurrencyOptions),

    khepri_benchmark_output:print_results(SystemInfo, Results),
    khepri_benchmark_output:generate_html(SystemInfo, Results),

    khepri_benchmark_utils:cleanup_cluster(),
    ok.

run_benchmarks(Benchmarks, RunOptions, ConcurrencyOptions) ->
    run_benchmarks(Benchmarks, RunOptions, ConcurrencyOptions, []).

run_benchmarks(
  [{Name, Benchmarks} | Rest], RunOptions, ConcurrencyOptions, Results) ->
    io:format("~nBenchmarking ~ts:", [Name]),
    Result = run_benchmarks1(Benchmarks, RunOptions, ConcurrencyOptions, []),
    Results1 = [{Name, Result} | Results],
    run_benchmarks(Rest, RunOptions, ConcurrencyOptions, Results1);
run_benchmarks([], _RunOptions, _ConcurrencyOptions, Results) ->
    lists:reverse(Results).

run_benchmarks1(
  [#{name := Name} = Benchmark | Rest],
  RunOptions, ConcurrencyOptions, Results) ->
    io:format(" ~ts...", [Name]),
    {_Max, Result} = erlperf:run(Benchmark, RunOptions, ConcurrencyOptions),
    Results1 = [{Name, lists:reverse(Result)} | Results],
    run_benchmarks1(Rest, RunOptions, ConcurrencyOptions, Results1);
run_benchmarks1([], _RunOptions, _ConcurrencyOptions, Results) ->
    lists:reverse(Results).
