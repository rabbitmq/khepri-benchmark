%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2022-2023 Broadcom. All Rights Reserved. The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
%%

-module(khepri_benchmark_khepri).

-include_lib("stdlib/include/assert.hrl").
-include_lib("khepri/include/khepri.hrl").

-export([insert_benchmark/2,
         query_benchmark/2,
         delete_benchmark/2]).

-define(RA_SYSTEM, default).
-define(STORE_ID, khepri).

-define(TABLE, my_table).

insert_benchmark([Node] = Nodes, Profile) when Node =:= node() ->
    #{
      name => name(Profile),
      runner => fun(_) -> insert_in_khepri() end,
      init => fun() ->
                      setup_khepri(Nodes, Profile),
                      assert_khepri_is_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      done => fun(_) ->
                      assert_khepri_is_not_empty(),
                      stop_khepri(Nodes)
              end
     };
insert_benchmark(Nodes, Profile) ->
    #{
      name => name(Profile),
      runner => fun(_) -> insert_in_khepri(Nodes) end,
      init => fun() ->
                      setup_khepri(Nodes, Profile),
                      assert_khepri_is_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      done => fun(_) ->
                      assert_khepri_is_not_empty(),
                      stop_khepri(Nodes)
              end
     }.

query_benchmark([Node] = Nodes, Favor) when Node =:= node() ->
    #{
      name => name(Favor),
      runner => fun(_) -> query_in_khepri(Favor) end,
      init => fun() ->
                      setup_khepri(Nodes, safe),
                      fill_khepri(),
                      assert_khepri_is_not_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      done => fun(_) -> stop_khepri(Nodes) end
     };
query_benchmark(Nodes, Favor) ->
    #{
      name => name(Favor),
      runner => fun(_) -> query_in_khepri(Nodes, Favor) end,
      init => fun() ->
                      setup_khepri(Nodes, safe),
                      fill_khepri(),
                      assert_khepri_is_not_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      done => fun(_) -> stop_khepri(Nodes) end
     }.

delete_benchmark([Node] = Nodes, Profile) when Node =:= node() ->
    #{
      name => name(Profile),
      runner => fun(_) -> delete_in_khepri() end,
      init => fun() ->
                      setup_khepri(Nodes, Profile),
                      fill_khepri(),
                      assert_khepri_is_not_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      done => fun(_) -> stop_khepri(Nodes) end
     };
delete_benchmark(Nodes, Profile) ->
    #{
      name => name(Profile),
      runner => fun(_) -> delete_in_khepri(Nodes) end,
      init => fun() ->
                      setup_khepri(Nodes, Profile),
                      fill_khepri(),
                      assert_khepri_is_not_empty(),
                      khepri_benchmark_utils:bench_runner_init_done()
              end,
      done => fun(_) -> stop_khepri(Nodes) end
     }.

name(Profile) ->
    lists:flatten(io_lib:format("Khepri (~s)", [Profile])).

setup_khepri(Nodes, Profile) ->
    remove_khepri_dir(Nodes),

    _ = [ok = rpc:call(
                Node, application, set_env,
                [ra, wal_max_size_bytes, 64_000_000, [{persistent, true}]])
         || Node <- Nodes],

    case Profile of
        safe ->
            _ = [ok = rpc:call(
                        Node, application, unset_env,
                        [ra, wal_sync_method])
                 || Node <- Nodes];
        unsafe ->
            _ = [ok = rpc:call(
                        Node, application, set_env,
                        [ra, wal_sync_method, none, [{persistent, true}]])
                 || Node <- Nodes]
    end,

    _ = [{ok, ?STORE_ID} = rpc:call(Node, khepri, start, [])
         || Node <- Nodes],
    _ = [ok = rpc:call(Node, khepri_cluster, join, [node()])
         || Node <- Nodes, Node =/= node()],

    _ = [begin
             Members = [Member
                        || {_, Member} <-
                           rpc:call(Node,
                               khepri_cluster, members, [?STORE_ID])],
             ?assertEqual(lists:sort(Nodes), lists:sort(Members))
         end || Node <- Nodes],
    ok.

assert_khepri_is_empty() ->
    {ok, Count} = khepri:count([?KHEPRI_WILDCARD_STAR]),
    ?assertEqual(0, Count).

assert_khepri_is_not_empty() ->
    {ok, Count} = khepri:count([?KHEPRI_WILDCARD_STAR_STAR]),
    ?assert(Count > 100).

fill_khepri() ->
    lists:foreach(
      fun(I) ->
              Key = integer_to_binary(I),
              ok = khepri:put([?TABLE, Key], none)
      end, lists:seq(1, khepri_benchmark_utils:max_keys())).

stop_khepri(Nodes) ->
    _ = [begin
             ok = rpc:call(Node, khepri_cluster, stop, []),
             ok = rpc:call(Node, application, stop, [khepri]),
             ok = rpc:call(Node, application, stop, [ra]),
             ok
         end || Node <- Nodes],
    remove_khepri_dir(Nodes),
    ok.

remove_khepri_dir(Nodes) ->
    _ = [file:del_dir_r(io_lib:format("khepri#~s", [Node])) || Node <- Nodes],
    ok.

insert_in_khepri() ->
    Key = khepri_benchmark_utils:get_key(),
    Value = khepri_benchmark_utils:get_key(),
    ok = khepri:put([?TABLE, Key], Value),
    ok.

insert_in_khepri(Nodes) ->
    Key = khepri_benchmark_utils:get_key(),
    Value = khepri_benchmark_utils:get_key(),
    Node = khepri_benchmark_utils:pick_node(Nodes),
    ok = rpc:call(Node, khepri, put, [[?TABLE, Key], Value]),
    ok.

query_in_khepri(Favor) ->
    Key = khepri_benchmark_utils:get_key(),
    {ok, _} = khepri:get([?TABLE, Key], #{favor => Favor}),
    ok.

query_in_khepri(Nodes, Favor) ->
    Key = khepri_benchmark_utils:get_key(),
    Node = khepri_benchmark_utils:pick_node(Nodes),
    {ok, _} = rpc:call(Node, khepri, get, [[?TABLE, Key], #{favor => Favor}]),
    ok.

delete_in_khepri() ->
    Key = khepri_benchmark_utils:get_key(),
    ok = khepri:delete([?TABLE, Key]),
    ok.

delete_in_khepri(Nodes) ->
    Key = khepri_benchmark_utils:get_key(),
    Node = khepri_benchmark_utils:pick_node(Nodes),
    ok = rpc:call(Node, khepri, delete, [[?TABLE, Key]]),
    ok.
