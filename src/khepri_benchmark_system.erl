%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(khepri_benchmark_system).

-export([info/0]).

info() ->
    #{erlang => erlang(),
      num_cores => num_cores(),
      os => os(),
      available_memory => available_memory(),
      cpu_speed => cpu_speed()}.

erlang() ->
    OTPRel = erlang:system_info(otp_release),
    OTP_VERSION = filename:join(
                    [code:root_dir(), "releases", OTPRel, "OTP_VERSION"]),
    case file:read_file(OTP_VERSION) of
        {ok, Content} -> string:trim(unicode:characters_to_list(Content));
        _             -> undefined
    end.

num_cores() ->
    erlang:system_info(schedulers_online).

os() ->
    case os:type() of
        {_, darwin}  -> "Mac OSX";
        {_, freebsd} -> "FreeBSD";
        {_, linux}   -> "Linux";
        {_, nt}      -> "Windows";
        {win32, _}   -> "Windows";
        {_, Name}    -> atom_to_list(Name)
    end.

available_memory() ->
    available_memory(os()).

available_memory("Mac OSX" = OS) ->
    Output = os:cmd("sysctl -n hw.memsize"),
    parse_memory_for(OS, Output);
available_memory("FreeBSD" = OS) ->
    Output = os:cmd("sysctl -n hw.physmem"),
    parse_memory_for(OS, Output);
available_memory("Linux" = OS) ->
    Output = os:cmd("cat /proc/meminfo"),
    parse_memory_for(OS, Output);
available_memory("Windows" = OS) ->
    Output = os:cmd("wmic COMPUTERSYSTEM GET TOTALPHYSICALMEMORY"),
    parse_memory_for(OS, Output);
available_memory(_) ->
    undefined.

parse_memory_for("Mac OSX", Output) ->
    {Memory, _} = string:to_integer(Output),
    format_memory(Memory);
parse_memory_for("FreeBSD", Output) ->
    {Memory, _} = string:to_integer(Output),
    format_memory(Memory);
parse_memory_for("Linux", Output) ->
    {match, [MemoryString]} = re:run(
                                Output, "MemTotal:\\s*([0-9]+)\\s*kB",
                                [{capture, all_but_first, list}]),
    {Memory, _} = string:to_integer(MemoryString),
    format_memory(Memory * 1024);
parse_memory_for("Windows", Output) ->
    {match, [MemoryString]} = re:run(
                                Output, "(\\d+)",
                                [{capture, all_but_first, list}]),
    {Memory, _} = string:to_integer(MemoryString),
    format_memory(Memory).

format_memory(Memory) when Memory >= 1024 * 1024 * 1024 * 1024 ->
    lists:flatten(
      io_lib:format("~b TiB", [round(Memory / (1024 * 1024 * 1024 * 1024))]));
format_memory(Memory) when Memory >= 1024 * 1024 * 1024 ->
    lists:flatten(
      io_lib:format("~b GiB", [round(Memory / (1024 * 1024 * 1024))]));
format_memory(Memory) when Memory >= 1024 * 1024 ->
    lists:flatten(io_lib:format("~b MiB", [round(Memory / (1024 * 1024))]));
format_memory(Memory) when Memory >= 1024 ->
    lists:flatten(io_lib:format("~b kiB", [round(Memory / 1024)]));
format_memory(Memory) ->
    lists:flatten(io_lib:format("~b B", [Memory])).

cpu_speed() ->
    cpu_speed(os()).

cpu_speed("Mac OSX" = OS) ->
    Output = os:cmd("sysctl -n machdep.cpu.brand_string"),
    parse_cpu_for(OS, Output);
cpu_speed("FreeBSD" = OS) ->
    Output = os:cmd("sysctl -n hw.model"),
    parse_cpu_for(OS, Output);
cpu_speed("Linux" = OS) ->
    Output = os:cmd("cat /proc/cpuinfo"),
    parse_cpu_for(OS, Output);
cpu_speed("Windows" = OS) ->
    Output = os:cmd("wmic CPU GET NAME"),
    parse_cpu_for(OS, Output);
cpu_speed(_) ->
    undefined.

parse_cpu_for("Mac OSX", Output) ->
    string:trim(Output);
parse_cpu_for("FreeBSD", Output) ->
    string:trim(Output);
parse_cpu_for("Linux", Output) ->
    {match, [Model]} = re:run(
                         Output,
                         "model name.*:([\\w \\(\\)\\-\\@\\.]*)",
                         [{capture, all_but_first, list}, caseless]),
    string:trim(Model);
parse_cpu_for("Windows", Output) ->
    {match, [Model]} = re:run(
                         Output,
                         "Name(.*)",
                         [{capture, all_but_first, list}]),
    string:trim(Model).
