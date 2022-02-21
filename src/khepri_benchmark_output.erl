%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(khepri_benchmark_output).

-export([print_results/1,
         generate_html/2]).

print_results(Results) ->
    io:format("~n"),
    print_results1(Results).

print_results1([{Category, Results} | Rest]) ->
    {MaxLabelLen, MaxValue} =
    lists:foldl(
      fun({_Condition, InnerResults}, Acc) ->
              lists:foldl(
                fun({Backend, Result}, {MLL, MV}) ->
                        Len = string:length(Backend),
                        MLL1 = if
                                   Len > MLL -> Len;
                                   true      -> MLL
                               end,
                        MV1 = if
                                  Result > MV -> Result;
                                  true        -> MV
                              end,
                        {MLL1, MV1}
                end, Acc, InnerResults)
      end, {0, 0}, Results),

    io:format("~n~n"),
    print_category(Category, Results, MaxLabelLen, MaxValue),
    print_results1(Rest);
print_results1([]) ->
    ok.

print_category(
  Category, [{Condition, Results} | Rest], MaxLabelLen, MaxValue) ->
    io:format("~n\033[1m~ts, ~ts\033[0m~n", [Category, Condition]),
    print_diagram(Results, MaxLabelLen, MaxValue),
    print_category(Category, Rest, MaxLabelLen, MaxValue);
print_category(_Category, [], _MaxLabelLen, _MaxValue) ->
    ok.

print_diagram([{Backend, Result} | Rest], MaxLabelLen, MaxValue) ->
    io:format(
      "    ~*.. ts: ~s~*..■s\033[0m ~b ops/s~n",
      [MaxLabelLen, Backend, backend_color(Backend),
       ceil(50 * Result / MaxValue), "", Result]),
    print_diagram(Rest, MaxLabelLen, MaxValue);
print_diagram([], _MaxLabelLen, _MaxValue) ->
    ok.

backend_color("Mnesia") -> "\033[38;2;188;71;73m";
backend_color("Khepri (safe)") -> "\033[38;2;204;255;51m";
backend_color("Khepri (unsafe)") -> "\033[38;2;56;176;0m";
backend_color(_) -> "".

generate_html(Conditions, Results) ->
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

    Workers = [maps:get(concurrency, RunOptions, 1)
               || #{run_options := RunOptions} <- Conditions],

    InsertKhepriSafe = collect_scores(Results, "Inserts", "Khepri (safe)"),
    InsertKhepriUnafe = collect_scores(Results, "Inserts", "Khepri (unsafe)"),
    InsertMnesia = collect_scores(Results, "Inserts", "Mnesia"),

    %% FIXME: Don't hard-code the "3-node cluster" suffix.
    InsertClusteredKhepriSafe = collect_scores(
                                  Results, "Inserts, 3-node cluster",
                                  "Khepri (safe)"),
    InsertClusteredKhepriUnafe = collect_scores(
                                   Results, "Inserts, 3-node cluster",
                                   "Khepri (unsafe)"),
    InsertClusteredMnesia = collect_scores(
                              Results, "Inserts, 3-node cluster", "Mnesia"),

    DeleteKhepriSafe = collect_scores(Results, "Deletes", "Khepri (safe)"),
    DeleteKhepriUnafe = collect_scores(Results, "Deletes", "Khepri (unsafe)"),
    DeleteMnesia = collect_scores(Results, "Deletes", "Mnesia"),

    DeleteClusteredKhepriSafe = collect_scores(
                                  Results, "Deletes, 3-node cluster",
                                  "Khepri (safe)"),
    DeleteClusteredKhepriUnafe = collect_scores(
                                   Results, "Deletes, 3-node cluster",
                                   "Khepri (unsafe)"),
    DeleteClusteredMnesia = collect_scores(
                              Results, "Deletes, 3-node cluster", "Mnesia"),

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

    TargetDir = "public",
    _ = file:del_dir_r(TargetDir),
    ok = file:make_dir(TargetDir),
    TargetFile = filename:join(TargetDir, "index.html"),
    ok = file:write_file(TargetFile, Content13),
    Results.

collect_scores([{Category, Results} | _Rest], Category, Label) ->
    [proplists:get_value(Label, Result)
     || {_Condition, Result} <- Results];
collect_scores([_ | Rest], Category, Label) ->
    collect_scores(Rest, Category, Label);
collect_scores([], _, _) ->
    [].

replace_list(Content, Var, List) ->
    List1 = string:join([integer_to_list(I) || I <- List], ", "),
    re:replace(
      Content,
      Var ++ " = \\[\\]",
      Var ++ " = [" ++ List1 ++ "]").
