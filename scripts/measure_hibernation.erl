#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin

-mode(compile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hibernation Memory Measurement Script
%%
%% Measures memory reduction from process hibernation in erlmcp
%%
%% Usage:
%%   ./scripts/measure_hibernation.erl [session_backend|client|transport_tcp]
%%
%% Examples:
%%   ./scripts/measure_hibernation.erl session_backend
%%   ./scripts/measure_hibernation.erl client
%%   ./scripts/measure_hibernation.erl transport_tcp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main([Module]) ->
    code:add_patha("_build/default/lib/erlmcp_core/ebin"),
    code:add_patha("_build/default/lib/erlmcp_transports/ebin"),

    io:format("~n=== Hibernation Memory Measurement ===~n"),
    io:format("Module: ~p~n~n", [Module]),

    case Module of
        "session_backend" -> measure_session_backend();
        "client" -> measure_client();
        "transport_tcp" -> measure_transport_tcp();
        _ -> usage()
    end;

main(_) ->
    usage().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Session Backend Measurement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

measure_session_backend() ->
    io:format("Starting 100 session backend processes...~n"),
    Pids = [begin
        {ok, Pid} = erlmcp_session_backend:start_link(#{
            backend => erlmcp_session_ets,
            cleanup_interval => 60000
        }),
        Pid
    end || _ <- lists:seq(1, 100)],

    %% Measure memory before hibernation
    MemBefore = measure_memory(Pids, "before hibernation"),

    %% Trigger hibernation by sending requests
    io:format("~nTriggering hibernation (sending requests)...~n"),
    lists:foreach(fun(Pid) ->
        gen_server:call(Pid, list, 5000)
    end, Pids),

    %% Wait for hibernation to complete
    timer:sleep(500),

    %% Measure memory after hibernation
    MemAfter = measure_memory(Pids, "after hibernation"),

    %% Calculate savings
    print_savings(MemBefore, MemAfter),

    %% Cleanup
    lists:foreach(fun(Pid) -> gen_server:stop(Pid) end, Pids),

    io:format("~n✓ Session backend measurement complete~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client Measurement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

measure_client() ->
    io:format("Starting 100 client processes...~n"),
    Pids = [begin
        {ok, Pid} = erlmcp_client:start_link({stdio, []}, #{}),
        Pid
    end || _ <- lists:seq(1, 100)],

    %% Wait for hibernate_after timeout (30 seconds)
    io:format("~nWaiting 31 seconds for automatic hibernation...~n"),
    timer:sleep(31000),

    %% Measure memory after automatic hibernation
    MemAfter = measure_memory(Pids, "after automatic hibernation"),

    %% For comparison, spawn non-hibernated clients
    io:format("~nSpawning 100 non-hibernated clients for comparison...~n"),
    PidsNoHib = [begin
        %% Start without hibernate_after option
        {ok, Pid} = gen_server:start_link(erlmcp_client, [{stdio, []}, #{}], []),
        Pid
    end || _ <- lists:seq(1, 100)],

    MemBefore = measure_memory(PidsNoHib, "without hibernation"),

    %% Calculate savings
    print_savings(MemBefore, MemAfter),

    %% Cleanup
    lists:foreach(fun(Pid) -> gen_server:stop(Pid) end, Pids ++ PidsNoHib),

    io:format("~n✓ Client measurement complete~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Transport TCP Measurement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

measure_transport_tcp() ->
    io:format("Starting 50 TCP transport processes (server mode)...~n"),
    {ok, ListenSocket} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(ListenSocket),

    Pids = [begin
        {ok, Socket} = gen_tcp:accept(ListenSocket, 1000),
        {ok, Pid} = erlmcp_transport_tcp:start_link(#{
            mode => server,
            socket => Socket,
            owner => self()
        }),
        Pid
    end || _ <- lists:seq(1, 50)],

    %% Measure memory before hibernation
    MemBefore = measure_memory(Pids, "before hibernation"),

    %% Trigger hibernation by sending unknown messages
    io:format("~nTriggering hibernation (sending unknown messages)...~n"),
    lists:foreach(fun(Pid) ->
        Pid ! unknown_message
    end, Pids),

    %% Wait for hibernation to complete
    timer:sleep(500),

    %% Measure memory after hibernation
    MemAfter = measure_memory(Pids, "after hibernation"),

    %% Calculate savings
    print_savings(MemBefore, MemAfter),

    %% Cleanup
    lists:foreach(fun(Pid) -> gen_server:stop(Pid) end, Pids),
    gen_tcp:close(ListenSocket),

    io:format("~n✓ Transport TCP measurement complete~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Measure total memory for a list of processes
measure_memory(Pids, Label) ->
    TotalMemory = lists:sum([begin
        case erlang:process_info(Pid, memory) of
            {memory, Mem} -> Mem;
            undefined -> 0
        end
    end || Pid <- Pids]),

    AvgMemory = TotalMemory / length(Pids),

    io:format("~nMemory ~s:~n", [Label]),
    io:format("  Total: ~.2f MB (~p bytes)~n", [TotalMemory / 1024 / 1024, TotalMemory]),
    io:format("  Average: ~.2f KB per process~n", [AvgMemory / 1024]),

    TotalMemory.

%% Print memory savings statistics
print_savings(MemBefore, MemAfter) ->
    SavingsBytes = MemBefore - MemAfter,
    SavingsPercent = (SavingsBytes * 100) / MemBefore,

    io:format("~n=== Memory Savings ===~n"),
    io:format("  Before: ~.2f MB~n", [MemBefore / 1024 / 1024]),
    io:format("  After:  ~.2f MB~n", [MemAfter / 1024 / 1024]),
    io:format("  Saved:  ~.2f MB (~.1f%)~n", [SavingsBytes / 1024 / 1024, SavingsPercent]),

    case SavingsPercent > 80 of
        true ->
            io:format("  Status: ✓ EXCELLENT (>80% reduction)~n");
        false when SavingsPercent > 50 ->
            io:format("  Status: ✓ GOOD (>50% reduction)~n");
        false ->
            io:format("  Status: ⚠ SUBOPTIMAL (<50% reduction)~n")
    end.

%% Print usage information
usage() ->
    io:format("~nUsage: ~n", []),
    io:format("  ./scripts/measure_hibernation.erl <module>~n~n", []),
    io:format("Modules:~n", []),
    io:format("  session_backend  - Measure session backend hibernation~n", []),
    io:format("  client          - Measure client hibernation~n", []),
    io:format("  transport_tcp   - Measure TCP transport hibernation~n~n", []),
    halt(1).
