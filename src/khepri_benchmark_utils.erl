%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(khepri_benchmark_utils).

-include_lib("stdlib/include/assert.hrl").

-export([max_keys/0,
         get_key/0,
         pick_node/1,
         runs_from_escript/0,
         uncompressed_escript_dir/0,
         cluster_label/1,
         setup_cluster/1,
         cleanup_cluster/0]).

-define(MAX_KEYS, 10000).
-define(UNCOMPRESSED_ESCRIPT_DIR, "_uncompressed_escript").

max_keys() ->
    ?MAX_KEYS.

get_key() ->
    integer_to_binary(rand:uniform(?MAX_KEYS)).

pick_node(Cluster) ->
    I = rand:uniform(length(Cluster)),
    lists:nth(I, Cluster).

runs_from_escript() ->
    not filelib:is_dir(code:lib_dir(khepri)).

uncompressed_escript_dir() ->
    ?UNCOMPRESSED_ESCRIPT_DIR.

cluster_label(ClusterSize) ->
    lists:flatten(io_lib:format("~b-node cluster", [ClusterSize])).

setup_cluster(ClusterSize) ->
    io:format(
      "Starting additional nodes to create a ~b-node cluster...~n",
      [ClusterSize]),
    CodePath = case runs_from_escript() of
                   true ->
                       TargetDir = uncompressed_escript_dir(),
                       _ = file:del_dir_r(TargetDir),
                       ok = file:make_dir(TargetDir),
                       Escript = escript:script_name(),
                       {ok, Sections} = escript:extract(Escript, []),
                       Archive = proplists:get_value(archive, Sections),
                       {ok, Files} = zip:unzip(Archive, [{cwd, TargetDir}]),
                       [filename:dirname(File)
                        || File <- Files,
                           filename:extension(File) =:= ".app"];
                   false ->
                       code:get_path()
               end,
    [begin
         [Name0, Hostname] = string:split(atom_to_list(node()), "@"),
         Name = lists:flatten(io_lib:format("~s-peer~b", [Name0, I])),
         {ok, Node} = slave:start_link(Hostname, Name),
         rpc:call(Node, code, add_paths, [CodePath]),
         ?assert(is_list(rpc:call(Node, ?MODULE, module_info, []))),
         _ = rpc:call(Node, logger, set_primary_config, [level, error]),
         Node
     end || I <- lists:seq(2, ClusterSize)].

cleanup_cluster() ->
    _ = file:del_dir_r(uncompressed_escript_dir()),
    ok.
