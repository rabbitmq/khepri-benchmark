%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2022-2024 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(khepri_benchmark_mnesia).

-include_lib("stdlib/include/assert.hrl").

-export([insert_benchmark/1,
         query_benchmark/1,
         delete_benchmark/1]).

-define(TABLE, my_table).

insert_benchmark([Node] = Nodes) when Node =:= node() ->
    #{
      name => "Mnesia",
      runner => fun(_) -> insert_in_mnesia() end,
      init => fun() ->
                      setup_mnesia(Nodes),
                      assert_mnesia_is_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      init_runner => "runner() -> ok.",
      done => fun(_) ->
                      assert_mnesia_is_not_empty(),
                      stop_mnesia(Nodes)
              end
     };
insert_benchmark(Nodes) ->
    #{
      name => "Mnesia",
      runner => fun(_) -> insert_in_mnesia(Nodes) end,
      init => fun() ->
                      setup_mnesia(Nodes),
                      assert_mnesia_is_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      init_runner => "runner() -> ok.",
      done => fun(_) ->
                      assert_mnesia_is_not_empty(),
                      stop_mnesia(Nodes)
              end
     }.

query_benchmark([Node] = Nodes) when Node =:= node() ->
    #{
      name => "Mnesia",
      runner => fun(_) -> query_in_mnesia() end,
      init => fun() ->
                      setup_mnesia(Nodes),
                      fill_mnesia(),
                      assert_mnesia_is_not_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      init_runner => "runner() -> ok.",
      done => fun(_) -> stop_mnesia(Nodes) end
     };
query_benchmark(Nodes) ->
    #{
      name => "Mnesia",
      runner => fun(_) -> query_in_mnesia(Nodes) end,
      init => fun() ->
                      setup_mnesia(Nodes),
                      fill_mnesia(),
                      assert_mnesia_is_not_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      init_runner => "runner() -> ok.",
      done => fun(_) -> stop_mnesia(Nodes) end
     }.

delete_benchmark([Node] = Nodes) when Node =:= node() ->
    #{
      name => "Mnesia",
      runner => fun(_) -> delete_in_mnesia() end,
      init => fun() ->
                      setup_mnesia(Nodes),
                      fill_mnesia(),
                      assert_mnesia_is_not_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      init_runner => "runner() -> ok.",
      done => fun(_) -> stop_mnesia(Nodes) end
     };
delete_benchmark(Nodes) ->
    #{
      name => "Mnesia",
      runner => fun(_) -> delete_in_mnesia(Nodes) end,
      init => fun() ->
                      setup_mnesia(Nodes),
                      fill_mnesia(),
                      assert_mnesia_is_not_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      init_runner => "runner() -> ok.",
      done => fun(_) -> stop_mnesia(Nodes) end
     }.

setup_mnesia(Nodes) ->
    remove_mnesia_dir(Nodes),
    ok = mnesia:create_schema(Nodes),
    _ = [rpc:call(Node, mnesia, start, []) || Node <- Nodes],
    {atomic, ok} = mnesia:create_table(
                     ?TABLE,
                     [{attributes, [key, value]},
                      {disc_copies, Nodes}]),
    ok.

assert_mnesia_is_empty() ->
    Size = mnesia:table_info(?TABLE, size),
    ?assertEqual(0, Size).

assert_mnesia_is_not_empty() ->
    Size = mnesia:table_info(?TABLE, size),
    ?assert(Size > 100).

fill_mnesia() ->
    lists:foreach(
      fun(I) ->
              Key = integer_to_binary(I),
              {atomic, ok} = mnesia:transaction(
                               fun() -> mnesia:write({?TABLE, Key, none}) end)
      end, lists:seq(1, khepri_benchmark_utils:max_keys())).

stop_mnesia(Nodes) ->
    mnesia:delete_table(?TABLE),
    _ = [rpc:call(Node, mnesia, stop, []) || Node <- Nodes],
    remove_mnesia_dir(Nodes).

remove_mnesia_dir(Nodes) ->
    _ = [file:del_dir_r(io_lib:format("Mnesia.~s", [Node])) || Node <- Nodes],
    ok.

insert_in_mnesia() ->
    Key = khepri_benchmark_utils:get_key(),
    Value = khepri_benchmark_utils:get_key(),
    {atomic, ok} = mnesia:transaction(
                     fun() -> mnesia:write({?TABLE, Key, Value}) end),
    ok.

insert_in_mnesia(Nodes) ->
    Key = khepri_benchmark_utils:get_key(),
    Value = khepri_benchmark_utils:get_key(),
    Node = khepri_benchmark_utils:pick_node(Nodes),
    {atomic, ok} = rpc:call(
                     Node, mnesia, transaction,
                     [fun() -> mnesia:write({?TABLE, Key, Value}) end]),
    ok.

query_in_mnesia() ->
    Key = khepri_benchmark_utils:get_key(),
    {atomic, _} = mnesia:transaction(
                    fun() -> mnesia:read({?TABLE, Key}) end),
    ok.

query_in_mnesia(Nodes) ->
    Key = khepri_benchmark_utils:get_key(),
    Node = khepri_benchmark_utils:pick_node(Nodes),
    {atomic, _} = rpc:call(
                    Node, mnesia, transaction,
                    [fun() -> mnesia:read({?TABLE, Key}) end]),
    ok.

delete_in_mnesia() ->
    Key = khepri_benchmark_utils:get_key(),
    {atomic, ok} = mnesia:transaction(
                     fun() -> mnesia:delete({?TABLE, Key}) end),
    ok.

delete_in_mnesia(Nodes) ->
    Key = khepri_benchmark_utils:get_key(),
    Node = khepri_benchmark_utils:pick_node(Nodes),
    {atomic, ok} = rpc:call(
                     Node, mnesia, transaction,
                     [fun() -> mnesia:delete({?TABLE, Key}) end]),
    ok.
