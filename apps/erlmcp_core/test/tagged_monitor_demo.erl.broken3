#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin

%% Demo script showing OTP 27/28 tagged monitors

main(_) ->
    io:format("~n=== OTP 27/28 Tagged Monitors Demo ===~n~n"),

    %% Test 1: Basic tagged monitor
    io:format("Test 1: Basic tagged monitor~n"),
    Pid1 = spawn(fun() -> receive die -> ok end end),
    Tag1 = {tool, calculator},
    Ref1 = erlang:monitor(process, Pid1, [{tag, Tag1}]),
    io:format("  Created monitor: ~p~n", [Ref1]),
    io:format("  Tag embedded: ~p~n", [element(1, Ref1)]),
    Pid1 ! die,
    receive
        {'DOWN', Ref1, process, Pid1, Reason1} ->
            io:format("  DOWN received, tag: ~p, reason: ~p~n", [element(1, Ref1), Reason1])
    after
        1000 -> io:format("  ERROR: Timeout~n")
    end,

    %% Test 2: Multiple tagged monitors
    io:format("~nTest 2: Multiple tagged monitors~n"),
    Pid2a = spawn(fun() -> receive die -> ok end end),
    Pid2b = spawn(fun() -> receive die -> ok end end),
    Tag2a = {session, session_a},
    Tag2b = {session, session_b},
    Ref2a = erlang:monitor(process, Pid2a, [{tag, Tag2a}]),
    Ref2b = erlang:monitor(process, Pid2b, [{tag, Tag2b}]),
    io:format("  Monitor A: ~p~n", [Ref2a]),
    io:format("  Monitor B: ~p~n", [Ref2b]),

    %% Kill in reverse order
    Pid2b ! die,
    receive
        {'DOWN', Ref2b, process, Pid2b, _} ->
            io:format("  Session B DOWN identified correctly: ~p~n", [element(1, Ref2b)])
    after
        1000 -> io:format("  ERROR: Timeout~n")
    end,
    Pid2a ! die,
    receive
        {'DOWN', Ref2a, process, Pid2a, _} ->
            io:format("  Session A DOWN identified correctly: ~p~n", [element(1, Ref2a)])
    after
        1000 -> io:format("  ERROR: Timeout~n")
    end,

    %% Test 3: No mapping needed
    io:format("~nTest 3: No Ref->Tag mapping needed~n"),
    io:format("  Before (OTP 26):~n"),
    io:format("    Ref = erlang:monitor(process, Pid)~n"),
    io:format("    Map = maps:put(Ref, Tag, Map)~n"),
    io:format("    %% Later: Tag = maps:get(Ref, Map)~n"),
    io:format("~n  After (OTP 27+):~n"),
    io:format("    Ref = erlang:monitor(process, Pid, [{tag, Tag}])~n"),
    io:format("    %% Later: Tag = element(1, Ref)~n"),
    io:format("    %% No separate map needed!~n"),

    %% Test 4: Performance
    io:format("~nTest 4: Performance comparison~n"),
    N = 10000,
    {TimeOld, _} = timer:tc(fun() ->
        [begin
             P = spawn(fun() -> receive die -> ok end end),
             R = erlang:monitor(process, P),
             P ! die,
             receive {'DOWN', R, process, P, _} -> ok end
         end || _ <- lists:seq(1, N)]
    end),
    {TimeNew, _} = timer:tc(fun() ->
        [begin
             P = spawn(fun() -> receive die -> ok end end),
             R = erlang:monitor(process, P, [{tag, {test, I}}]),
             P ! die,
             receive {'DOWN', R, process, P, _} -> ok end
         end || I <- lists:seq(1, N)]
    end),
    io:format("  Untagged (~p monitors): ~p us~n", [N, TimeOld]),
    io:format("  Tagged (~p monitors): ~p us~n", [N, TimeNew]),
    io:format("  Overhead: ~p%~n", [((TimeNew - TimeOld) / TimeOld) * 100]),

    io:format("~n=== Demo Complete ===~n"),
    ok.
