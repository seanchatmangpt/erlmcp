-module(otp28_process_monitoring_example).
-modledoc """
Example: OTP 28 Enhanced Process Monitoring

This example demonstrates enhanced process monitoring with metadata
using OTP 28.3.1 improvements.

Run with:
  erl -pa _build/default/lib/erlmcp_core/ebin -s otp28_process_monitoring_example run
""" .

-export([run/0, start_upgrade_manager/0,
         monitor_with_metadata/2, get_process_info/1]).

%%%===================================================================
%%% Example API
%%%===================================================================

%% @doc Run the complete example
run() ->
    io:format("~n=== OTP 28 Enhanced Process Monitoring Example ===~n~n"),

    %% Start upgrade manager
    io:format("1. Starting OTP upgrade manager...~n"),
    {ok, _Pid} = start_upgrade_manager(),
    io:format("   Upgrade manager started~n~n"),

    %% Create test processes
    io:format("2. Creating test processes...~n"),
    Pid1 = spawn(fun() -> test_worker(connection_handler) end),
    Pid2 = spawn(fun() -> test_worker(request_processor) end),
    Pid3 = spawn(fun() -> test_worker(cache_manager) end),
    io:format("   Created 3 test processes~n~n"),

    %% Monitor with metadata
    io:format("3. Monitoring processes with metadata...~n"),
    Ref1 = monitor_with_metadata(Pid1, #{purpose => connection_handling,
                                         client => <<"acme">>}),
    Ref2 = monitor_with_metadata(Pid2, #{purpose => request_processing,
                                         priority => high}),
    Ref3 = monitor_with_metadata(Pid3, #{purpose => cache_management,
                                         size => large}),
    io:format("   Monitoring: ~p, ~p, ~p~n~n", [Ref1, Ref2, Ref3]),

    %% Get optimized process info
    io:format("4. Getting optimized process info...~n"),
    Info1 = get_process_info(Pid1),
    io:format("   Process 1 memory: ~p bytes~n", [maps:get(memory, Info1, 0)]),
    io:format("   Process 1 reductions: ~p~n", [maps:get(reductions, Info1, 0)]),
    io:format("   Process 1 status: ~p~n~n", [maps:get(status, Info1, unknown)]),

    %% Demonstrate single-call performance
    io:format("5. Performance comparison...~n"),
    {TimeSingle, _} = timer:tc(fun() ->
        erlmcp_otp28_upgrade:get_process_info_optimized(Pid1,
            [memory, heap_size, total_heap_size, reductions, status])
    end),
    SingleMs = TimeSingle / 1000,

    {TimeMulti, _} = timer:tc(fun() ->
        process_info(Pid1, memory),
        process_info(Pid1, heap_size),
        process_info(Pid1, total_heap_size),
        process_info(Pid1, reductions),
        process_info(Pid1, status)
    end),
    MultiMs = TimeMulti / 1000,

    Speedup = MultiMs / SingleMs,
    io:format("   Single call (optimized): ~.3fms~n", [SingleMs]),
    io:format("   Multiple calls (legacy): ~.3fms~n", [MultiMs]),
    io:format("   Speedup: ~.1fx~n~n", [Speedup]),

    %% Trigger termination and monitor DOWN
    io:format("6. Triggering process termination...~n"),
    exit(Pid1, normal),
    receive
        {'DOWN', Ref1, process, Pid1, Reason} ->
            io:format("   Process 1 terminated: ~p~n", [Reason])
    after 1000 ->
        io:format("   Timeout waiting for DOWN message~n")
    end,
    io:format("~n"),

    %% Cleanup
    io:format("7. Cleaning up...~n"),
    exit(Pid2, kill),
    exit(Pid3, kill),
    io:format("   All processes stopped~n~n"),

    io:format("=== Example Complete ===~n~n"),
    ok.

%% @doc Start the OTP upgrade manager
start_upgrade_manager() ->
    erlmcp_otp28_upgrade:start_link().

%% @doc Monitor process with metadata
monitor_with_metadata(Pid, Metadata) ->
    erlmcp_otp28_upgrade:monitor_with_metadata(Pid, Metadata).

%% @doc Get optimized process information
get_process_info(Pid) ->
    erlmcp_otp28_upgrade:get_process_info_optimized(Pid).

%%%===================================================================
%%% Test Worker Process
%%%===================================================================

test_worker(Role) ->
    %% Set logger metadata
    erlmcp_otp28_upgrade:logger_metadata(#{
        role => Role,
        start_time => erlang:system_time(millisecond)
    }),

    %% Do some work
    logger:info("Worker started: ~p", [Role]),

    %% Simulate work
    timer:sleep(5000),

    logger:info("Worker stopping: ~p", [Role]),
    ok.
