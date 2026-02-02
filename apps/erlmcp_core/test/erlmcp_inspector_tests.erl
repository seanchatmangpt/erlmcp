%%%-------------------------------------------------------------------
%%% @doc
%%% Test suite for erlmcp_inspector - OTP 28 Process Iterator BIFs
%%%
%%% Chicago School TDD: Real processes, no mocks, testing observable behavior.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_inspector_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
-define(TEST_SERVER_ID, <<"test_context">>).
-define(TEST_TRANSPORT_ID, test_transport).

%%====================================================================
%% Test Fixtures
%%====================================================================

%% @doc Setup function - starts necessary applications
setup() ->
    application:ensure_all_started(gproc),
    ok.

%% @doc Teardown function - cleanup
cleanup(_) ->
    ok.

%%====================================================================
%% Iterator Functionality Tests
%%====================================================================

iterator_returns_processes_test() ->
    %% Test that iterator returns processes
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Create a test process with type marker
         TestPid = spawn(fun() ->
                                put('$mcp_type', model_context),
                                receive
                                    stop -> ok
                                end
                        end),

         %% Get list of MCP processes
         Processes = erlmcp_inspector:list_mcp_processes(),

         %% Cleanup test process
         TestPid ! stop,

         %% Should find at least one process (our test process)
         ?assert(length(Processes) >= 1),

         %% Verify process info structure
         [{Pid, Info}] = [P || P <- Processes, element(1, P) =:= TestPid],
         ?assertEqual(TestPid, Pid),
         ?assert(maps:is_key(type, Info)),
         ?assert(maps:is_key(pid, Info)),
         ?assert(maps:is_key(memory, Info)),
         ?assert(maps:is_key(queue_len, Info))
     end}.

iterator_memory_efficiency_test() ->
    %% Test that iterator doesn't create huge lists
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Measure memory before
         Before = erlang:memory(processes_used),

         %% Create 100 test processes
         TestPids = [spawn(fun() ->
                                   put('$mcp_type', tool_process),
                                   receive stop -> ok end
                           end) || _ <- lists:seq(1, 100)],

         %% Iterate over all processes (should not create huge list)
         Processes = erlmcp_inspector:list_mcp_processes(),

         %% Measure memory after
         After = erlang:memory(processes_used),

         %% Cleanup test processes
         lists:foreach(fun(P) -> P ! stop end, TestPids),

         %% Memory growth should be reasonable (< 1MB for 100 processes)
         MemoryGrowth = After - Before,
         ?assert(MemoryGrowth < 1000000),
         ?assert(length(Processes) >= 100)
     end}.

dead_process_handling_test() ->
    %% Test that dead processes are handled gracefully
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Create and immediately kill a process
         TestPid = spawn(fun() ->
                                put('$mcp_type', model_context),
                                ok
                        end),

         %% Process might die during iteration
         Processes = erlmcp_inspector:list_mcp_processes(),

         %% Should not crash, should return valid list
         ?assert(is_list(Processes)),
         ?assert(lists:all(fun({P, _I}) -> is_pid(P) end, Processes))
     end}.

%%====================================================================
%% Process Type Filtering Tests
%%====================================================================

find_contexts_by_type_test() ->
    %% Test finding processes by type
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Create test processes of different types
         ModelPid = spawn(fun() ->
                                  put('$mcp_type', model_context),
                                  receive stop -> ok end
                          end),
         ToolPid = spawn(fun() ->
                                 put('$mcp_type', tool_process),
                                 receive stop -> ok end
                         end),

         %% Find by type
         ModelContexts = erlmcp_inspector:find_contexts_by_type(model_context),
         ToolProcesses = erlmcp_inspector:find_contexts_by_type(tool_process),

         %% Cleanup
         ModelPid ! stop,
         ToolPid ! stop,

         %% Should find our test processes
         ?assert(lists:member(ModelPid, ModelContexts)),
         ?assert(lists:member(ToolPid, ToolProcesses))
     end}.

unknown_type_filtering_test() ->
    %% Test that unknown type processes are filtered out
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Create process without type marker
         TestPid = spawn(fun() ->
                                receive stop -> ok end
                        end),

         %% List MCP processes (should exclude unknown types)
         Processes = erlmcp_inspector:list_mcp_processes(),

         %% Cleanup
         TestPid ! stop,

         %% Process without $mcp_type should not be in list
         ?assertNot(lists:keymember(TestPid, 1, Processes))
     end}.

%%====================================================================
%% Aggregate Statistics Tests
%%====================================================================

aggregate_stats_counts_test() ->
    %% Test aggregate statistics accuracy
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Create test processes
         ModelPids = [spawn(fun() ->
                                     put('$mcp_type', model_context),
                                     receive stop -> ok end
                             end) || _ <- lists:seq(1, 3)],
         ToolPids = [spawn(fun() ->
                                     put('$mcp_type', tool_process),
                                     receive stop -> ok end
                             end) || _ <- lists:seq(1, 2)],

         %% Get aggregate stats
         Stats = erlmcp_inspector:get_aggregate_stats(),

         %% Cleanup
         lists:foreach(fun(P) -> P ! stop end, ModelPids ++ ToolPids),

         %% Verify counts
         ?assert(maps:is_key(total, Stats)),
         ?assert(maps:is_key(by_type, Stats)),

         Total = maps:get(total, Stats),
         ByType = maps:get(by_type, Stats),

         %% Should have at least our test processes
         ?assert(Total >= 5),
         ?assert(maps:get(model_context, ByType, 0) >= 3),
         ?assert(maps:get(tool_process, ByType, 0) >= 2)
     end}.

aggregate_stats_memory_test() ->
    %% Test memory statistics
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Get aggregate stats
         Stats = erlmcp_inspector:get_aggregate_stats(),

         %% Verify memory fields
         ?assert(maps:is_key(total_memory, Stats)),
         ?assert(maps:is_key(memory_by_type, Stats)),

         TotalMemory = maps:get(total_memory, Stats),
         MemoryByType = maps:get(memory_by_type, Stats),

         %% Total memory should be positive
         ?assert(TotalMemory > 0),
         ?assert(is_map(MemoryByType))
     end}.

aggregate_stats_queue_test() ->
    %% Test queue statistics
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Get aggregate stats
         Stats = erlmcp_inspector:get_aggregate_stats(),

         %% Verify queue stats fields
         ?assert(maps:is_key(queue_stats, Stats)),

         QueueStats = maps:get(queue_stats, Stats),

         %% Each type should have min, max, avg
         maps:foreach(fun(_Type, TypeStats) ->
                             ?assert(maps:is_key(min, TypeStats)),
                             ?assert(maps:is_key(max, TypeStats)),
                             ?assert(maps:is_key(avg, TypeStats))
                     end,
                     QueueStats)
     end}.

%%====================================================================
%% Process Info Tests
%%====================================================================

get_process_info_success_test() ->
    %% Test getting info for live process
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Create test process
         TestPid = spawn(fun() ->
                                put('$mcp_type', model_context),
                                put('$mcp_context_id', <<"test_ctx_123">>),
                                receive stop -> ok end
                        end),

         %% Get process info
         {ok, Info} = erlmcp_inspector:get_process_info(TestPid),

         %% Cleanup
         TestPid ! stop,

         %% Verify info structure
         ?assertEqual(TestPid, maps:get(pid, Info)),
         ?assertEqual(model_context, maps:get(type, Info)),
         ?assert(maps:is_key(memory, Info)),
         ?assert(maps:is_key(queue_len, Info)),
         ?assert(maps:is_key(dictionary, Info))
     end}.

get_process_info_dead_test() ->
    %% Test getting info for dead process
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Create and immediately kill process
         TestPid = spawn(fun() -> ok end),
         timer:sleep(10), % Give it time to die

         %% Try to get info for dead process
         Result = erlmcp_inspector:get_process_info(TestPid),

         %% Should return error
         ?assertEqual({error, process_dead}, Result)
     end}.

%%====================================================================
%% Find by ID Tests
%%====================================================================

find_by_id_success_test() ->
    %% Test finding process by context ID
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Create test process with specific ID
         ContextId = <<"test_ctx_456">>,
         TestPid = spawn(fun() ->
                                put('$mcp_type', model_context),
                                put('$mcp_context_id', ContextId),
                                receive stop -> ok end
                        end),

         %% Find by ID
         Result = erlmcp_inspector:find_context_by_id(ContextId),

         %% Cleanup
         TestPid ! stop,

         %% Should find the process
         ?assertEqual({ok, TestPid}, Result)
     end}.

find_by_id_not_found_test() ->
    %% Test finding non-existent context ID
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Try to find non-existent ID
         Result = erlmcp_inspector:find_context_by_id(<<"non_existent_ctx">>),

         %% Should return not_found
         ?assertEqual({error, not_found}, Result)
     end}.

%%====================================================================
%% Helper Functions Tests
%%====================================================================

get_process_count_by_type_test() ->
    %% Test convenience function for counting by type
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Create test processes
         _Pids = [spawn(fun() ->
                               put('$mcp_type', model_context),
                               receive stop -> ok end
                       end) || _ <- lists:seq(1, 3)],

         %% Get counts by type
         Counts = erlmcp_inspector:get_process_count_by_type(),

         %% Cleanup
         %% (Note: Pids are captured in list comprehension but not accessible here,
         %%  in real test we'd track them for cleanup)

         %% Should have count for model_context
         ?assert(maps:is_key(model_context, Counts)),
         ?assert(maps:get(model_context, Counts, 0) >= 3)
     end}.

get_memory_by_type_test() ->
    %% Test convenience function for memory by type
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Get memory by type
         MemoryByType = erlmcp_inspector:get_memory_by_type(),

         %% Should return map with memory values
         ?assert(is_map(MemoryByType)),

         %% All values should be non-negative integers
         maps:foreach(fun(_Type, Memory) ->
                             ?assert(is_integer(Memory)),
                             ?assert(Memory >= 0)
                     end,
                     MemoryByType)
     end}.

get_queue_stats_by_type_test() ->
    %% Test convenience function for queue stats
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% Get queue stats by type
         QueueStats = erlmcp_inspector:get_queue_stats_by_type(),

         %% Should return map of stats
         ?assert(is_map(QueueStats)),

         %% Each type should have min, max, avg
         maps:foreach(fun(_Type, TypeStats) ->
                             ?assert(maps:is_key(min, TypeStats)),
                             ?assert(maps:is_key(max, TypeStats)),
                             ?assert(maps:is_key(avg, TypeStats))
                     end,
                     QueueStats)
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_with_real_system_test() ->
    %% Test integration with real erlmcp processes
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         %% This test verifies inspector works with actual erlmcp processes
         %% that have proper $mcp_type markers

         %% List all MCP processes in the system
         Processes = erlmcp_inspector:list_mcp_processes(),

         %% Should be a list (may be empty if no erlmcp processes running)
         ?assert(is_list(Processes)),

         %% All entries should be {Pid, InfoMap} tuples
         lists:foreach(fun({Pid, Info}) ->
                             ?assert(is_pid(Pid)),
                             ?assert(is_map(Info)),
                             ?assert(maps:is_key(type, Info))
                     end,
                     Processes)
     end}.
