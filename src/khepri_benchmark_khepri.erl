%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(khepri_benchmark_khepri).

-include_lib("stdlib/include/assert.hrl").
-include_lib("khepri/include/khepri.hrl").

-export([insert_benchmark/2,
         delete_benchmark/2]).

-define(RA_SYSTEM, default).
-define(RA_CLUSTER, khepri).

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

    _ = [begin
             {ok, _} = rpc:call(
                         Node, application, ensure_all_started,
                         [ra]),
             {ok, _} = rpc:call(Node, ra_system, start_default, [])
         end || Node <- Nodes],

    {ok, _} = khepri:start(),
    _ = [ok = khepri:add_member(?RA_SYSTEM, Node)
         || Node <- Nodes, Node =/= node()],

    _ = [begin
             Members = [Member
                        || {_, Member} <-
                           rpc:call(Node, khepri, members, [?RA_CLUSTER])],
             ?assertEqual(lists:sort(Nodes), lists:sort(Members))
         end || Node <- Nodes],
    ok.

assert_khepri_is_empty() ->
    {ok, Result} = khepri_machine:get(
                     ?RA_CLUSTER, [#if_path_matches{regex = any}]),
    Size = maps:size(Result),
    %% There is the root node.
    ?assertEqual(1, Size).

assert_khepri_is_not_empty() ->
    {ok, Result} = khepri_machine:get(
                     ?RA_CLUSTER, [#if_path_matches{regex = any}]),
    Size = maps:size(Result),
    ?assert(Size > 100).

fill_khepri() ->
    lists:foreach(
      fun(I) ->
              Key = integer_to_binary(I),
              {ok, _} = khepri_machine:put(?RA_CLUSTER, [?TABLE, Key], none)
      end, lists:seq(1, khepri_benchmark_utils:max_keys())).

stop_khepri(Nodes) ->
    _ = [begin
             rpc:call(Node, application, stop, [khepri]),
             rpc:call(Node, application, stop, [ra])
         end || Node <- Nodes],
    remove_khepri_dir(Nodes),
    ok.

remove_khepri_dir(Nodes) ->
    _ = [file:del_dir_r(io_lib:format("~s", [Node])) || Node <- Nodes],
    ok.

insert_in_khepri() ->
    Key = khepri_benchmark_utils:get_key(),
    Value = khepri_benchmark_utils:get_key(),
    {ok, _} = khepri_machine:put(
                ?RA_CLUSTER, [?TABLE, Key], #kpayload_data{data = Value}),
    ok.

insert_in_khepri(Nodes) ->
    Key = khepri_benchmark_utils:get_key(),
    Value = khepri_benchmark_utils:get_key(),
    Node = khepri_benchmark_utils:pick_node(Nodes),
    {ok, _} = rpc:call(
                Node, khepri_machine, put,
                [?RA_CLUSTER, [?TABLE, Key], #kpayload_data{data = Value}]),
    ok.

delete_in_khepri() ->
    Key = khepri_benchmark_utils:get_key(),
    {ok, _} = khepri_machine:delete(?RA_CLUSTER, [?TABLE, Key]),
    ok.

delete_in_khepri(Nodes) ->
    Key = khepri_benchmark_utils:get_key(),
    Node = khepri_benchmark_utils:pick_node(Nodes),
    {ok, _} = rpc:call(
                Node, khepri_machine, delete, [?RA_CLUSTER, [?TABLE, Key]]),
    ok.
