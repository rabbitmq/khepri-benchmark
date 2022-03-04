%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2022 VMware, Inc. or its affiliates.  All rights reserved.
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
    erlang := Erlang}) ->
    io:format(
      "~n"
      "\033[1mSystem information:\033[0m~n"
      "  OS: ~ts~n"
      "  CPU: ~ts~n"
      "  Number of cores: ~b~n"
      "  Memory: ~ts~n"
      "  Erlang: ~ts~n",
      [OS, CpuSpeed, NumCores, AvailableMemory, Erlang]).

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
                        || {Backend, BackendResults} <- CategoryResults],

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
  [{Backend, BackendResults} | Rest], Concurrency, MaxLabelLen, MaxValue) ->
    {AvgScore, Concurrency} = lists:keyfind(Concurrency, 2, BackendResults),
    io:format(
      "    ~*.. ts: ~s~*..■s\033[0m ~b ops/s~n",
      [MaxLabelLen, Backend, backend_color(Backend),
       ceil(50 * AvgScore / MaxValue), "", AvgScore]),
    print_diagram(Rest, Concurrency, MaxLabelLen, MaxValue);
print_diagram([], _Concurrency, _MaxLabelLen, _MaxValue) ->
    ok.

backend_color("Mnesia") -> "\033[38;2;188;71;73m";
backend_color("Khepri (safe)") -> "\033[38;2;204;255;51m";
backend_color("Khepri (unsafe)") -> "\033[38;2;56;176;0m";
backend_color(_) -> "".

generate_html(Options, SystemInfo, Results) ->
    Filename = case khepri_benchmark_utils:runs_from_escript() of
                   true ->
                       %% Escript already uncompressed to setup cluster.
                       filename:join(
                         [khepri_benchmark_utils:
                          uncompressed_escript_dir(),
                          "priv",
                          "index.html"]);
                   false ->
                       filename:join(
                         [code:priv_dir(khepri_benchmark),
                          "index.html"])
               end,
    {ok, Template} = file:read_file(Filename),

    Workers = tested_workers(Results),

    InsertKhepriSafe = collect_scores(Results, "Inserts", "Khepri (safe)"),
    InsertKhepriUnafe = collect_scores(Results, "Inserts", "Khepri (unsafe)"),
    InsertMnesia = collect_scores(Results, "Inserts", "Mnesia"),

    ClusterSize = maps:get(cluster_size, Options, 3),
    Label = khepri_benchmark_utils:cluster_label(ClusterSize),
    InsertClusteredKhepriSafe = collect_scores(
                                  Results, "Inserts, " ++ Label,
                                  "Khepri (safe)"),
    InsertClusteredKhepriUnafe = collect_scores(
                                   Results, "Inserts, " ++ Label,
                                   "Khepri (unsafe)"),
    InsertClusteredMnesia = collect_scores(
                              Results, "Inserts, " ++ Label, "Mnesia"),

    DeleteKhepriSafe = collect_scores(Results, "Deletes", "Khepri (safe)"),
    DeleteKhepriUnafe = collect_scores(Results, "Deletes", "Khepri (unsafe)"),
    DeleteMnesia = collect_scores(Results, "Deletes", "Mnesia"),

    DeleteClusteredKhepriSafe = collect_scores(
                                  Results, "Deletes, " ++ Label,
                                  "Khepri (safe)"),
    DeleteClusteredKhepriUnafe = collect_scores(
                                   Results, "Deletes, " ++ Label,
                                   "Khepri (unsafe)"),
    DeleteClusteredMnesia = collect_scores(
                              Results, "Deletes, " ++ Label, "Mnesia"),

    Content1 = replace_list(Template, "workers", Workers),

    Content2 = replace_list(
                 Content1, "insert_khepri_safe", InsertKhepriSafe),
    Content3 = replace_list(
                 Content2, "insert_khepri_unsafe", InsertKhepriUnafe),
    Content4 = replace_list(
                 Content3, "insert_mnesia", InsertMnesia),

    Content5 = replace_list(
                 Content4, "insert_clustered_khepri_safe",
                 InsertClusteredKhepriSafe),
    Content6 = replace_list(
                 Content5, "insert_clustered_khepri_unsafe",
                 InsertClusteredKhepriUnafe),
    Content7 = replace_list(
                 Content6, "insert_clustered_mnesia", InsertClusteredMnesia),

    Content8 = replace_list(
                 Content7, "delete_khepri_safe", DeleteKhepriSafe),
    Content9 = replace_list(
                 Content8, "delete_khepri_unsafe", DeleteKhepriUnafe),
    Content10 = replace_list(
                  Content9, "delete_mnesia", DeleteMnesia),

    Content11 = replace_list(
                  Content10, "delete_clustered_khepri_safe",
                  DeleteClusteredKhepriSafe),
    Content12 = replace_list(
                  Content11, "delete_clustered_khepri_unsafe",
                  DeleteClusteredKhepriUnafe),
    Content13 = replace_list(
                  Content12, "delete_clustered_mnesia", DeleteClusteredMnesia),

    #{cpu_speed := CpuSpeed,
      num_cores := NumCores,
      available_memory := AvailableMemory,
      os := OS,
      erlang := Erlang} = SystemInfo,

    Content14 = replace_var(Content13, "%OS%", OS),
    Content15 = replace_var(Content14, "%CPU%", CpuSpeed),
    Content16 = replace_var(Content15, "%CORES%", NumCores),
    Content17 = replace_var(Content16, "%MEMORY%", AvailableMemory),
    Content18 = replace_var(Content17, "%ERLANG%", Erlang),
    Content19 = replace_var(Content18, "%CLUSTER_SIZE%", ClusterSize),

    TargetDir = "public",
    _ = file:del_dir_r(TargetDir),
    ok = file:make_dir(TargetDir),
    TargetFile = filename:join(TargetDir, "index.html"),
    ok = file:write_file(TargetFile, Content19),
    Results.

collect_scores(Results, WantedCategory, WantedBackend) ->
    [average(Scores)
     || {Category, CategoryResults} <- Results,
        Category =:= WantedCategory,
        {Backend, BackendResults} <- CategoryResults,
        Backend =:= WantedBackend,
        {Scores, _Concurrency} <- BackendResults].

replace_list(Content, Var, List) ->
    List1 = string:join([integer_to_list(I) || I <- List], ", "),
    re:replace(
      Content,
      Var ++ " = \\[\\]",
      Var ++ " = [" ++ List1 ++ "]").

tested_workers(Results) ->
    [Concurrency
     || {_Category, CategoryResults} <- [hd(Results)],
        {_Backend, BackendResults} <- [hd(CategoryResults)],
        {_Scores, Concurrency} <- BackendResults].

average(Scores) ->
    lists:sum(Scores) div length(Scores).

replace_var(Content, Var, Value) when is_number(Value) ->
    Value1 = integer_to_list(Value),
    replace_var(Content, Var, Value1);
replace_var(Content, Var, Value) ->
    re:replace(Content, Var, Value, [global]).
