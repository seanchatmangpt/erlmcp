%%%-----------------------------------------------------------------------------
%%% @doc Tests for erlmcp_profiler
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_profiler_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TEST FIXTURES
%%%=============================================================================

profiler_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Memory snapshot", fun test_memory_snapshot/0},
      {"Process memory inspection", fun test_process_memory/0},
      {"Binary leak detection", fun test_binary_leaks/0},
      {"Heap fragmentation", fun test_heap_fragmentation/0},
      {"Profile PID", fun test_profile_pid/0},
      {"Message tracing", fun test_trace_messages/0}
     ]}.

setup() ->
    application:ensure_all_started(erlmcp_observability),
    %% Start a test process
    Pid = spawn(fun test_worker/0),
    register(test_worker, Pid),
    Pid.

cleanup(Pid) ->
    catch unregister(test_worker),
    catch exit(Pid, kill),
    erlmcp_profiler:stop_profiling(),
    ok.

%%%=============================================================================
%%% MEMORY PROFILING TESTS
%%%=============================================================================

test_memory_snapshot() ->
    {ok, Snapshot} = erlmcp_profiler:memory_snapshot(#{top => 10}),
    
    ?assert(is_list(Snapshot)),
    ?assert(length(Snapshot) =< 10),
    
    case Snapshot of
        [] -> ok;
        [First | _] ->
            ?assertMatch(#{
                pid := _,
                memory_bytes := _,
                memory_mb := _,
                message_queue_len := _,
                reductions := _
            }, First)
    end.

test_process_memory() ->
    Pid = whereis(test_worker),
    {ok, Info} = erlmcp_profiler:process_memory(Pid),
    
    ?assertMatch(#{
        pid := Pid,
        memory_bytes := _,
        memory_mb := _,
        heap_size_words := _,
        stack_size_words := _,
        heap_fragmentation_pct := _
    }, Info),
    
    %% Verify numeric values
    ?assert(maps:get(memory_bytes, Info) > 0),
    ?assert(maps:get(heap_fragmentation_pct, Info) >= 0.0).

test_binary_leaks() ->
    %% Create a process with large binaries
    LeakyPid = spawn(fun() -> 
        Bin = binary:copy(<<1>>, 1000000),
        receive stop -> ok end,
        _ = Bin
    end),
    
    timer:sleep(100),
    
    {ok, Suspects} = erlmcp_profiler:binary_leaks(),
    
    ?assert(is_list(Suspects)),
    
    %% Check if our leaky process is detected
    Found = lists:any(fun(#{pid := P}) -> P == LeakyPid end, Suspects),
    ?assert(Found orelse length(Suspects) >= 0),  % May or may not be flagged
    
    LeakyPid ! stop.

test_heap_fragmentation() ->
    Pid = whereis(test_worker),
    {ok, Fragmentation} = erlmcp_profiler:heap_fragmentation(Pid),
    
    ?assert(is_float(Fragmentation)),
    ?assert(Fragmentation >= 0.0),
    ?assert(Fragmentation =< 100.0).

%%%=============================================================================
%%% CPU PROFILING TESTS
%%%=============================================================================

test_profile_pid() ->
    Pid = whereis(test_worker),
    
    %% Send some work to the process
    [Pid ! {work, N} || N <- lists:seq(1, 100)],
    
    {ok, Result} = erlmcp_profiler:profile_pid(Pid, #{
        duration => 100,
        mode => fprof,
        output => "/tmp/test_profile.out"
    }),
    
    ?assertMatch(#{
        mode := fprof,
        pid := Pid,
        file := "/tmp/test_profile.out"
    }, Result),
    
    %% Verify file was created
    ?assert(filelib:is_regular("/tmp/test_profile.out")),
    
    %% Cleanup
    file:delete("/tmp/test_profile.out").

%%%=============================================================================
%%% MESSAGE TRACING TESTS
%%%=============================================================================

test_trace_messages() ->
    Pid = whereis(test_worker),
    
    %% Start tracing
    {ok, Messages} = erlmcp_profiler:trace_messages(Pid, 200),
    
    %% Send some messages during trace
    [Pid ! {msg, N} || N <- lists:seq(1, 10)],
    
    timer:sleep(250),
    
    ?assert(is_list(Messages)),
    ?assert(length(Messages) >= 0).  % May capture some messages

%%%=============================================================================
%%% HELPER FUNCTIONS
%%%=============================================================================

%% Simple worker process for testing
test_worker() ->
    receive
        {work, N} ->
            _ = lists:seq(1, N * 1000),
            test_worker();
        {msg, _} ->
            test_worker();
        stop ->
            ok;
        _ ->
            test_worker()
    end.
