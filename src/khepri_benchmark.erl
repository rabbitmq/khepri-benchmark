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

    NoConcurrency = #{
                      name => "no concurrency",
                      run_options => #{warmup => 1,
                                       samples => 5}
                     },
    Concurrency50 = #{
                      name => "50 concurrent workers",
                      run_options => #{warmup => 2,
                                       samples => 5,
                                       concurrency => 50}
                     },
    Concurrency500 = #{
                       name => "500 concurrent workers",
                       run_options => #{warmup => 5,
                                        samples => 5,
                                        concurrency => 500}
                      },
    Conditions = [NoConcurrency, Concurrency50, Concurrency500],

    Results = run_benchmarks(Benchmarks, Conditions),

    io:format("~p~n", [Results]),
    khepri_benchmark_output:print_results(Results),
    khepri_benchmark_output:generate_html(Conditions, Results),

    khepri_benchmark_utils:cleanup_cluster(),
    ok.

run_benchmarks(Benchmarks, Conditions) ->
    run_benchmarks(Benchmarks, Conditions, []).

run_benchmarks([{Name, Benchmarks} | Rest], Conditions, Results) ->
    io:format("~nBenchmarking: ~ts", [Name]),
    Result = run_with_conditions(Benchmarks, Conditions, []),
    Results1 = [{Name, Result} | Results],
    run_benchmarks(Rest, Conditions, Results1);
run_benchmarks([], _Conditions, Results) ->
    lists:reverse(Results).

run_with_conditions(
  Benchmarks,
  [#{name := Name, run_options := RunOptions} | Rest],
  Results) ->
    io:format("~n  ~ts:", [Name]),
    Result = run_with_run_options(Benchmarks, RunOptions, []),
    Results1 = [{Name, Result} | Results],
    run_with_conditions(Benchmarks, Rest, Results1);
run_with_conditions(_Benchmarks, [], Results) ->
    lists:reverse(Results).

run_with_run_options(
  [#{name := Name} = Benchmark | Rest],
  RunOptions, Results) ->
    io:format(" ~ts...", [Name]),
    Result = erlperf:run(Benchmark, RunOptions),
    Results1 = [{Name, Result} | Results],
    run_with_run_options(Rest, RunOptions, Results1);
run_with_run_options([], _RunOptions, Results) ->
    lists:reverse(Results).
