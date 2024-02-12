%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2022-2023 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(khepri_benchmark_output).

-export([print_results/3,
         generate_html/3]).

print_results(_Options, SystemInfo, Results) ->
    io:format("~n~n"),
    print_system_info(SystemInfo),
    print_results1(Results).

print_system_info(
  #{cpu_speed := CpuSpeed,
    num_cores := NumCores,
    available_memory := AvailableMemory,
    os := OS,
    erlang := Erlang,
    datetime := DateTime}) ->
    io:format(
      "~n"
      "\033[1mDate and time:\033[0m~n"
      "  ~ts~n"
      "~n"
      "\033[1mSystem information:\033[0m~n"
      "  OS: ~ts~n"
      "  CPU: ~ts~n"
      "  Number of cores: ~b~n"
      "  Memory: ~ts~n"
      "  Erlang: ~ts~n",
      [DateTime, OS, CpuSpeed, NumCores, AvailableMemory, Erlang]).

print_results1([{Category, CategoryResults} | Rest] = Results) ->
    Workers = tested_workers(Results),
    WantedConcurrency = lists:usort(
                          [Concurrency
                           || Concurrency <- [1, 50, 100, lists:max(Workers)],
                              lists:member(Concurrency, Workers)]),
    CategoryResults1 = [{Backend,
                         [{average(Scores), Concurrency}
                          || {Scores, Concurrency} <- BackendResults,
                             lists:member(Concurrency, WantedConcurrency)]}
                        || {Backend, BackendResults, _} <- CategoryResults],

    {MaxLabelLen, MaxValue} =
    lists:foldl(
      fun({Backend, BackendResults}, {MLL, MV}) ->
              Len = string:length(Backend),
              MLL1 = if
                         Len > MLL -> Len;
                         true      -> MLL
                     end,
              Score = lists:max(
                        [AvgScore
                         || {AvgScore, _Concurrency} <- BackendResults]),
              MV1 = if
                        Score > MV -> Score;
                        true       -> MV
                    end,
              {MLL1, MV1}
      end, {0, 0}, CategoryResults1),

    io:format("~n~n"),
    lists:foreach(
      fun(Concurrency) ->
              print_category(
                Category, CategoryResults1, Concurrency,
                MaxLabelLen, MaxValue)
      end, WantedConcurrency),
    print_results1(Rest);
print_results1([]) ->
    ok.

print_category(
  Category, CategoryResults,
  Concurrency, MaxLabelLen, MaxValue) ->
    ConcurrencyLabel = case Concurrency of
                           1 -> "no concurrency";
                           _ -> lists:flatten(
                                  io_lib:format("~b concurrent workers",
                                                [Concurrency]))
                       end,
    io:format("~n\033[1m~ts, ~ts\033[0m~n", [Category, ConcurrencyLabel]),
    print_diagram(CategoryResults, Concurrency, MaxLabelLen, MaxValue).

print_diagram(
  [{Backend, BackendResults} | Rest],
  Concurrency, MaxLabelLen, MaxValue) ->
    {AvgScore, Concurrency} = lists:keyfind(Concurrency, 2, BackendResults),
    io:format(
      "    ~*.. ts: ~s~*..■s\033[0m ~b ops/s~n",
      [MaxLabelLen, Backend, backend_color(Backend),
       ceil(50 * AvgScore / MaxValue), "", AvgScore]),
    print_diagram(Rest, Concurrency, MaxLabelLen, MaxValue);
print_diagram([], _Concurrency, _MaxLabelLen, _MaxValue) ->
    ok.

backend_color("Khepri (safe)") -> "\033[38;2;204;255;51m";
backend_color("Khepri (unsafe)") -> "\033[38;2;56;176;0m";
backend_color("Khepri (low_latency)") -> "\033[38;2;144;224;239m";
backend_color("Khepri (compromise)") -> "\033[38;2;0;180;216m";
backend_color("Khepri (consistency)") -> "\033[38;2;0;119;182m";
backend_color("Mnesia") -> "\033[38;2;188;71;73m";
backend_color(_) -> "".

generate_html(Options, SystemInfo, Results) ->
    BenchSingleNode = maps:get(bench_single_node, Options, true),
    BenchCluster = maps:get(bench_cluster, Options, true),
    BenchInserts = maps:get(bench_inserts, Options, true),
    BenchQueries = maps:get(bench_queries, Options, true),
    BenchDeletes = maps:get(bench_deletes, Options, true),

    Filename = case khepri_benchmark_utils:runs_from_escript() of
                   true ->
                       Dir = khepri_benchmark_utils:uncompressed_escript_dir(),
                       case BenchCluster of
                           true ->
                               ok;
                           false ->
                               _ = khepri_benchmark_utils:uncompress_escript()
                       end,
                       filename:join(
                         [Dir, "priv", "index.html"]);
                   false ->
                       filename:join(
                         [code:priv_dir(khepri_benchmark), "index.html"])
               end,
    {ok, Template} = file:read_file(Filename),

    #{cpu_speed := CpuSpeed,
      num_cores := NumCores,
      available_memory := AvailableMemory,
      os := OS,
      erlang := Erlang,
      datetime := DateTime} = SystemInfo,

    ClusterSize = maps:get(cluster_size, Options, 3),
    Label = khepri_benchmark_utils:cluster_label(ClusterSize),

    Workers = tested_workers(Results),

    Cats0 = [],
    Cats1 = if
                BenchSingleNode andalso BenchInserts ->
                    InsertKhepriSafe = collect_scores(
                                         Results, "Inserts",
                                         "Khepri (safe)"),
                    InsertKhepriUnsafe = collect_scores(
                                           Results, "Inserts",
                                           "Khepri (unsafe)"),
                    InsertMnesia = collect_scores(
                                     Results, "Inserts",
                                     "Mnesia"),
                    InsertKhepriSafeMon = collect_monitoring(
                                            Results, "Inserts",
                                            "Khepri (safe)"),
                    InsertKhepriUnsafeMon = collect_monitoring(
                                              Results, "Inserts",
                                              "Khepri (unsafe)"),
                    InsertMnesiaMon = collect_monitoring(
                                        Results, "Inserts",
                                        "Mnesia"),
                    Cats0 ++
                    [#{"category" => "insert",
                       "name" => "Inserts (unclustered)",
                       "backends" =>
                       [#{"backend" => "khepri_safe",
                          "name" => "Khepri (safe)",
                          "ops_samples" => jsx:encode(InsertKhepriSafe),
                          "monitoring_samples" =>
                          jsx:encode(InsertKhepriSafeMon)},
                        #{"backend" => "khepri_unsafe",
                          "name" => "Khepri (unsafe)",
                          "ops_samples" => jsx:encode(InsertKhepriUnsafe),
                          "monitoring_samples" =>
                          jsx:encode(InsertKhepriUnsafeMon)},
                        #{"backend" => "mnesia",
                          "name" => "Mnesia",
                          "ops_samples" => jsx:encode(InsertMnesia),
                          "monitoring_samples" =>
                          jsx:encode(InsertMnesiaMon)}]}];
                true ->
                    Cats0
            end,
    Cats2 = if
                BenchSingleNode andalso BenchQueries ->
                    QueryKhepriLowLat = collect_scores(
                                          Results, "Queries",
                                          "Khepri (low_latency)"),
                    QueryKhepriCompromise = collect_scores(
                                              Results, "Queries",
                                              "Khepri (compromise)"),
                    QueryKhepriConsistent = collect_scores(
                                              Results, "Queries",
                                              "Khepri (consistency)"),
                    QueryMnesia = collect_scores(
                                    Results, "Queries",
                                    "Mnesia"),
                    QueryKhepriLowLatMon = collect_monitoring(
                                             Results, "Queries",
                                             "Khepri (low_latency)"),
                    QueryKhepriCompromiseMon = collect_monitoring(
                                                 Results, "Queries",
                                                 "Khepri (compromise)"),
                    QueryKhepriConsistentMon = collect_monitoring(
                                                 Results, "Queries",
                                                 "Khepri (consistency)"),
                    QueryMnesiaMon = collect_monitoring(
                                       Results, "Queries",
                                       "Mnesia"),
                    Cats1 ++
                    [#{"category" => "query",
                       "name" => "Queries (unclustered)",
                       "backends" =>
                       [#{"backend" => "khepri_lowlat",
                          "name" => "Khepri (low_latency)",
                          "ops_samples" => jsx:encode(QueryKhepriLowLat),
                          "monitoring_samples" =>
                          jsx:encode(QueryKhepriLowLatMon)},
                        #{"backend" => "khepri_compromise",
                          "name" => "Khepri (compromise)",
                          "ops_samples" => jsx:encode(QueryKhepriCompromise),
                          "monitoring_samples" =>
                          jsx:encode(QueryKhepriCompromiseMon)},
                        #{"backend" => "khepri_consistency",
                          "name" => "Khepri (consistency)",
                          "ops_samples" => jsx:encode(QueryKhepriConsistent),
                          "monitoring_samples" =>
                          jsx:encode(QueryKhepriConsistentMon)},
                        #{"backend" => "mnesia",
                          "name" => "Mnesia",
                          "ops_samples" => jsx:encode(QueryMnesia),
                          "monitoring_samples" =>
                          jsx:encode(QueryMnesiaMon)}]}];
                true ->
                    Cats1
            end,
    Cats3 = if
                BenchSingleNode andalso BenchDeletes ->
                    DeleteKhepriSafe = collect_scores(
                                         Results, "Deletes",
                                         "Khepri (safe)"),
                    DeleteKhepriUnsafe = collect_scores(
                                           Results, "Deletes",
                                           "Khepri (unsafe)"),
                    DeleteMnesia = collect_scores(
                                     Results, "Deletes",
                                     "Mnesia"),
                    DeleteKhepriSafeMon = collect_monitoring(
                                            Results, "Deletes",
                                            "Khepri (safe)"),
                    DeleteKhepriUnsafeMon = collect_monitoring(
                                              Results, "Deletes",
                                              "Khepri (unsafe)"),
                    DeleteMnesiaMon = collect_monitoring(
                                        Results, "Deletes",
                                        "Mnesia"),
                    Cats2 ++
                    [#{"category" => "delete",
                       "name" => "Deletes (unclustered)",
                       "backends" =>
                       [#{"backend" => "khepri_safe",
                          "name" => "Khepri (safe)",
                          "ops_samples" => jsx:encode(DeleteKhepriSafe),
                          "monitoring_samples" =>
                          jsx:encode(DeleteKhepriSafeMon)},
                        #{"backend" => "khepri_unsafe",
                          "name" => "Khepri (unsafe)",
                          "ops_samples" => jsx:encode(DeleteKhepriUnsafe),
                          "monitoring_samples" =>
                          jsx:encode(DeleteKhepriUnsafeMon)},
                        #{"backend" => "mnesia",
                          "name" => "Mnesia",
                          "ops_samples" => jsx:encode(DeleteMnesia),
                          "monitoring_samples" =>
                          jsx:encode(DeleteMnesiaMon)}]}];
                true ->
                    Cats2
            end,
    Cats4 = if
                BenchCluster andalso BenchInserts ->
                    InsertClusteredKhepriSafe = collect_scores(
                                                  Results,
                                                  "Inserts, " ++ Label,
                                                  "Khepri (safe)"),
                    InsertClusteredKhepriUnsafe = collect_scores(
                                                    Results,
                                                    "Inserts, " ++ Label,
                                                    "Khepri (unsafe)"),
                    InsertClusteredMnesia = collect_scores(
                                              Results,
                                              "Inserts, " ++ Label,
                                              "Mnesia"),
                    InsertClusteredKhepriSafeMon = collect_monitoring(
                                                     Results,
                                                     "Inserts, " ++ Label,
                                                     "Khepri (safe)"),
                    InsertClusteredKhepriUnsafeMon = collect_monitoring(
                                                       Results,
                                                       "Inserts, " ++ Label,
                                                       "Khepri (unsafe)"),
                    InsertClusteredMnesiaMon = collect_monitoring(
                                                 Results,
                                                 "Inserts, " ++ Label,
                                                 "Mnesia"),
                    Cats3 ++
                    [#{"category" => "insert_clustered",
                       "name" => "Inserts, " ++ Label,
                       "backends" =>
                       [#{"backend" => "khepri_safe",
                          "name" => "Khepri (safe)",
                          "ops_samples" =>
                          jsx:encode(InsertClusteredKhepriSafe),
                          "monitoring_samples" =>
                          jsx:encode(InsertClusteredKhepriSafeMon)},
                        #{"backend" => "khepri_unsafe",
                          "name" => "Khepri (unsafe)",
                          "ops_samples" =>
                          jsx:encode(InsertClusteredKhepriUnsafe),
                          "monitoring_samples" =>
                          jsx:encode(InsertClusteredKhepriUnsafeMon)},
                        #{"backend" => "mnesia",
                          "name" => "Mnesia",
                          "ops_samples" =>
                          jsx:encode(InsertClusteredMnesia),
                          "monitoring_samples" =>
                          jsx:encode(InsertClusteredMnesiaMon)}]}];
                true ->
                    Cats3
            end,
    Cats5 = if
                BenchCluster andalso BenchQueries ->
                    QueryClusteredKhepriLowLat = collect_scores(
                                                   Results,
                                                   "Queries, " ++ Label,
                                                   "Khepri (low_latency)"),
                    QueryClusteredKhepriCompromise = collect_scores(
                                                       Results,
                                                       "Queries, " ++ Label,
                                                       "Khepri (compromise)"),
                    QueryClusteredKhepriConsistent = collect_scores(
                                                       Results,
                                                       "Queries, " ++ Label,
                                                       "Khepri (consistency)"),
                    QueryClusteredMnesia = collect_scores(
                                             Results,
                                             "Queries, " ++ Label,
                                             "Mnesia"),
                    QueryClusteredKhepriLowLatMon =
                    collect_monitoring(
                      Results,
                      "Queries, " ++ Label,
                      "Khepri (low_latency)"),
                    QueryClusteredKhepriCompromiseMon =
                    collect_monitoring(
                      Results, "Queries, " ++ Label,
                      "Khepri (compromise)"),
                    QueryClusteredKhepriConsistentMon =
                    collect_monitoring(
                      Results, "Queries, " ++ Label,
                      "Khepri (consistency)"),
                    QueryClusteredMnesiaMon = collect_monitoring(
                                                Results, "Queries, " ++ Label,
                                                "Mnesia"),
                    Cats4 ++
                    [#{"category" => "query_clustered",
                       "name" => "Queries, " ++ Label,
                       "backends" =>
                       [#{"backend" => "khepri_lowlat",
                          "name" => "Khepri (low_latency)",
                          "ops_samples" =>
                          jsx:encode(QueryClusteredKhepriLowLat),
                          "monitoring_samples" =>
                          jsx:encode(QueryClusteredKhepriLowLatMon)},
                        #{"backend" => "khepri_compromise",
                          "name" => "Khepri (compromise)",
                          "ops_samples" =>
                          jsx:encode(QueryClusteredKhepriCompromise),
                          "monitoring_samples" =>
                          jsx:encode(QueryClusteredKhepriCompromiseMon)},
                        #{"backend" => "khepri_consistency",
                          "name" => "Khepri (consistency)",
                          "ops_samples" =>
                          jsx:encode(QueryClusteredKhepriConsistent),
                          "monitoring_samples" =>
                          jsx:encode(QueryClusteredKhepriConsistentMon)},
                        #{"backend" => "mnesia",
                          "name" => "Mnesia",
                          "ops_samples" =>
                          jsx:encode(QueryClusteredMnesia),
                          "monitoring_samples" =>
                          jsx:encode(QueryClusteredMnesiaMon)}]}];
                true ->
                    Cats4
            end,
    Cats6 = if
                BenchCluster andalso BenchDeletes ->
                    DeleteClusteredKhepriSafe = collect_scores(
                                                  Results,
                                                  "Deletes, " ++ Label,
                                                  "Khepri (safe)"),
                    DeleteClusteredKhepriUnsafe = collect_scores(
                                                    Results,
                                                    "Deletes, " ++ Label,
                                                    "Khepri (unsafe)"),
                    DeleteClusteredMnesia = collect_scores(
                                              Results,
                                              "Deletes, " ++ Label,
                                              "Mnesia"),
                    DeleteClusteredKhepriSafeMon = collect_monitoring(
                                                     Results,
                                                     "Deletes, " ++ Label,
                                                     "Khepri (safe)"),
                    DeleteClusteredKhepriUnsafeMon = collect_monitoring(
                                                       Results,
                                                       "Deletes, " ++ Label,
                                                       "Khepri (unsafe)"),
                    DeleteClusteredMnesiaMon = collect_monitoring(
                                                 Results,
                                                 "Deletes, " ++ Label,
                                                 "Mnesia"),
                    Cats5 ++
                    [#{"category" => "delete_clustered",
                       "name" => "Deletes, " ++ Label,
                       "backends" =>
                       [#{"backend" => "khepri_safe",
                          "name" => "Khepri (safe)",
                          "ops_samples" =>
                          jsx:encode(DeleteClusteredKhepriSafe),
                          "monitoring_samples" =>
                          jsx:encode(DeleteClusteredKhepriSafeMon)},
                        #{"backend" => "khepri_unsafe",
                          "name" => "Khepri (unsafe)",
                          "ops_samples" =>
                          jsx:encode(DeleteClusteredKhepriUnsafe),
                          "monitoring_samples" =>
                          jsx:encode(DeleteClusteredKhepriUnsafeMon)},
                        #{"backend" => "mnesia",
                          "name" => "Mnesia",
                          "ops_samples" =>
                          jsx:encode(DeleteClusteredMnesia),
                          "monitoring_samples" =>
                          jsx:encode(DeleteClusteredMnesiaMon)}]}];
                true ->
                    Cats5
            end,

    Vars = #{"os" => OS,
             "cpu_speed" => CpuSpeed,
             "num_cores" => NumCores,
             "available_memory" => AvailableMemory,
             "erlang" => Erlang,
             "datetime" => DateTime,
             "cluster_size" => ClusterSize,
             "workers" => jsx:encode(Workers),
             "categories" => Cats6},
    Content = bbmustache:render(Template, Vars),

    TargetDir = "public",
    _ = file:del_dir_r(TargetDir),
    ok = file:make_dir(TargetDir),
    TargetFile = filename:join(TargetDir, "index.html"),
    ok = file:write_file(TargetFile, Content),
    Results.

collect_scores(Results, WantedCategory, WantedBackend) ->
    [average(Scores)
     || {Category, CategoryResults} <- Results,
        Category =:= WantedCategory,
        {Backend, BackendResults, _Monitoring} <- CategoryResults,
        Backend =:= WantedBackend,
        {Scores, _Concurrency} <- BackendResults].

tested_workers(Results) ->
    [Concurrency
     || {_Category, CategoryResults} <- [hd(Results)],
        {_Backend, BackendResults, _Monitoring} <- [hd(CategoryResults)],
        {_Scores, Concurrency} <- BackendResults].

average(Scores) ->
    lists:sum(Scores) div length(Scores).

collect_monitoring(Results, WantedCategory, WantedBackend) ->
    [#{timestamp => Timestamp,
       data =>
       [#{node => Node,
          gc => NumberOfGCs,
          total_mem => proplists:get_value(total, Memory)}
        || #{node := Node,
             gc := {NumberOfGCs, _, _},
             memory := Memory} <- Sample]}
     || {Category, CategoryResults} <- Results,
        Category =:= WantedCategory,
        {Backend, _BackendResults, Monitoring} <- CategoryResults,
        Backend =:= WantedBackend,
        {Timestamp, Sample} <- Monitoring].
