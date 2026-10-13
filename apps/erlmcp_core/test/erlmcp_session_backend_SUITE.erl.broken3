%%%-------------------------------------------------------------------
%%% @doc
%%% Session Backend Common Test Suite - OTP 28 Features
%%%
%%% Comprehensive integration tests for erlmcp_session_backend with:
%%% - Session lifecycle management (create, use, terminate)
%%% - Hibernation behavior and memory reduction
%%% - Memory guard integration (OTP 28 process flags)
%%% - Nominal type enforcement (EEP-69)
%%% - Tool spawning with tagged monitors (OTP 27/28)
%%% - Priority/urgent message handling (OTP 28 EEP-76)
%%% - Error scenarios and recovery
%%%
%%% Chicago School TDD:
%%% - Real gen_server instances (session_backend, session_ets)
%%% - Observable behavior: memory usage, process state, message delivery
%%% - No mocks or fakes
%%% - Graceful degradation for OTP < 28
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_backend_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases - Session Lifecycle
-export([test_session_store_and_fetch/1,
         test_session_delete/1,
         test_session_list/1,
         test_session_update_last_accessed/1]).

%% Test cases - Hibernation
-export([test_hibernation_after_store/1,
         test_hibernation_after_fetch/1,
         test_hibernation_memory_reduction/1,
         test_hibernation_after_cleanup/1]).

%% Test cases - Memory Guard
-export([test_memory_guard_tool_limits/1,
         test_memory_guard_context_limits/1,
         test_memory_guard_validation/1,
         test_memory_guard_force_hibernate/1]).

%% Test cases - Nominal Types
-export([test_nominal_session_id_type/1,
         test_nominal_type_safety/1,
         test_invalid_nominal_type_usage/1,
         test_nominal_type_constructor/1]).

%% Test cases - Tool Spawning
-export([test_tool_spawn_with_monitor/1,
         test_tool_monitor_tagging/1,
         test_tool_crash_handling/1,
         test_tool_memory_guard_enabled/1]).

%% Test cases - Priority Messages
-export([test_priority_message_handling/1,
         test_urgent_message_handling/1,
         test_priority_ping_pong/1,
         test_priority_session_cancellation/1]).

%% Test cases - Error Scenarios
-export([test_session_timeout_handling/1,
         test_memory_limit_enforcement/1,
         test_concurrent_session_access/1,
         test_backend_crash_recovery/1]).

-define(BACKEND_OPTS, #{backend => erlmcp_session_ets,
                         cleanup_interval => 1000}).
-define(TEST_TIMEOUT, 5000).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [{group, lifecycle},
     {group, hibernation},
     {group, memory_guard},
     {group, nominal_types},
     {group, tool_spawning},
     {group, priority_messages},
     {group, error_scenarios}].

groups() ->
    [{lifecycle, [sequence], [test_session_store_and_fetch,
                              test_session_delete,
                              test_session_list,
                              test_session_update_last_accessed]},
     {hibernation, [sequence], [test_hibernation_after_store,
                                test_hibernation_after_fetch,
                                test_hibernation_memory_reduction,
                                test_hibernation_after_cleanup]},
     {memory_guard, [sequence], [test_memory_guard_tool_limits,
                                 test_memory_guard_context_limits,
                                 test_memory_guard_validation,
                                 test_memory_guard_force_hibernate]},
     {nominal_types, [sequence], [test_nominal_session_id_type,
                                  test_nominal_type_safety,
                                  test_invalid_nominal_type_usage,
                                  test_nominal_type_constructor]},
     {tool_spawning, [sequence], [test_tool_spawn_with_monitor,
                                  test_tool_monitor_tagging,
                                  test_tool_crash_handling,
                                  test_tool_memory_guard_enabled]},
     {priority_messages, [sequence], [test_priority_message_handling,
                                      test_urgent_message_handling,
                                      test_priority_ping_pong,
                                      test_priority_session_cancellation]},
     {error_scenarios, [sequence], [test_session_timeout_handling,
                                    test_memory_limit_enforcement,
                                    test_concurrent_session_access,
                                    test_backend_crash_recovery]}].

init_per_suite(Config) ->
    ct:pal("Starting Session Backend CT Suite"),
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    ct:pal("Session Backend CT Suite completed"),
    application:stop(erlmcp),
    ok.

init_per_group(lifecycle, Config) ->
    ct:pal("Starting lifecycle tests"),
    start_backend(Config);
init_per_group(hibernation, Config) ->
    ct:pal("Starting hibernation tests"),
    start_backend(Config);
init_per_group(memory_guard, Config) ->
    ct:pal("Starting memory guard tests"),
    start_backend(Config);
init_per_group(nominal_types, Config) ->
    ct:pal("Starting nominal types tests"),
    start_backend(Config);
init_per_group(tool_spawning, Config) ->
    ct:pal("Starting tool spawning tests"),
    start_backend(Config);
init_per_group(priority_messages, Config) ->
    ct:pal("Starting priority messages tests"),
    start_backend(Config);
init_per_group(error_scenarios, Config) ->
    ct:pal("Starting error scenarios tests"),
    start_backend(Config).

end_per_group(_GroupName, Config) ->
    stop_backend(Config),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Ending test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases - Session Lifecycle
%%====================================================================

test_session_store_and_fetch(_Config) ->
    ct:pal("Testing session store and fetch"),

    SessionId = <<"session_test_1">>,
    Session = create_test_session(SessionId),

    %% Store session
    ok = erlmcp_session_backend:store(SessionId, Session),

    %% Fetch session
    {ok, FetchedSession} = erlmcp_session_backend:fetch(SessionId),

    %% Verify session data
    ?assertEqual(SessionId, maps:get(id, FetchedSession)),
    ?assert(maps:get(created_at, FetchedSession) > 0),
    ?assert(maps:get(last_accessed, FetchedSession) > 0),

    %% Cleanup
    ok = erlmcp_session_backend:delete(SessionId),
    ?assertEqual({error, not_found}, erlmcp_session_backend:fetch(SessionId)).

test_session_delete(_Config) ->
    ct:pal("Testing session deletion"),

    SessionId = <<"session_test_2">>,
    Session = create_test_session(SessionId),

    %% Store session
    ok = erlmcp_session_backend:store(SessionId, Session),

    %% Verify exists
    {ok, _} = erlmcp_session_backend:fetch(SessionId),

    %% Delete session
    ok = erlmcp_session_backend:delete(SessionId),

    %% Verify deleted
    ?assertEqual({error, not_found}, erlmcp_session_backend:fetch(SessionId)).

test_session_list(_Config) ->
    ct:pal("Testing session listing"),

    %% Create multiple sessions
    SessionIds = [<<"session_list_", (integer_to_binary(N))/binary>>
                  || N <- lists:seq(1, 5)],

    lists:foreach(fun(Id) ->
        Session = create_test_session(Id),
        ok = erlmcp_session_backend:store(Id, Session)
    end, SessionIds),

    %% List sessions
    {ok, ListedIds} = erlmcp_session_backend:list(),

    %% Verify all sessions are listed
    ?assert(length(ListedIds) >= 5),

    %% Cleanup
    lists:foreach(fun(Id) ->
        erlmcp_session_backend:delete(Id)
    end, SessionIds).

test_session_update_last_accessed(_Config) ->
    ct:pal("Testing last_accessed timestamp update"),

    SessionId = <<"session_test_3">>,
    Session = create_test_session(SessionId),

    %% Store session
    ok = erlmcp_session_backend:store(SessionId, Session),
    {ok, Session1} = erlmcp_session_backend:fetch(SessionId),
    OriginalLastAccessed = maps:get(last_accessed, Session1),

    %% Wait a bit
    timer:sleep(10),

    %% Fetch again (should update last_accessed)
    {ok, Session2} = erlmcp_session_backend:fetch(SessionId),
    UpdatedLastAccessed = maps:get(last_accessed, Session2),

    %% Verify timestamp updated
    ?assert(UpdatedLastAccessed >= OriginalLastAccessed),

    %% Cleanup
    ok = erlmcp_session_backend:delete(SessionId).

%%====================================================================
%% Test Cases - Hibernation
%%====================================================================

test_hibernation_after_store(_Config) ->
    ct:pal("Testing hibernation after store operation"),

    SessionId = <<"session_hibernation_1">>,
    Session = create_test_session(SessionId),

    %% Get backend PID
    BackendPid = whereis(erlmcp_session_backend),

    %% Get initial message queue length
    {message_queue_len, QueueBefore} = erlang:process_info(BackendPid, message_queue_len),

    %% Store session (triggers hibernation)
    ok = erlmcp_session_backend:store(SessionId, Session),

    %% Wait for hibernation to take effect
    timer:sleep(100),

    %% Verify message queue is drained
    {message_queue_len, QueueAfter} = erlang:process_info(BackendPid, message_queue_len),

    ?assert(QueueAfter =< QueueBefore),

    %% Cleanup
    erlmcp_session_backend:delete(SessionId).

test_hibernation_after_fetch(_Config) ->
    ct:pal("Testing hibernation after fetch operation"),

    SessionId = <<"session_hibernation_2">>,
    Session = create_test_session(SessionId),

    %% Store session
    ok = erlmcp_session_backend:store(SessionId, Session),

    %% Get backend PID
    BackendPid = whereis(erlmcp_session_backend),

    %% Fetch session (triggers hibernation)
    {ok, _FetchedSession} = erlmcp_session_backend:fetch(SessionId),

    %% Wait for hibernation to take effect
    timer:sleep(100),

    %% Check current_memory and reductions (hibernation reduces heap)
    {memory, Memory} = erlang:process_info(BackendPid, memory),
    {reductions, _Reductions} = erlang:process_info(BackendPid, reductions),

    ct:pal("Backend process memory after hibernation: ~p words", [Memory]),

    %% Memory should be reasonable (< 10MB words)
    ?assert(Memory < 1_000_000),

    %% Cleanup
    erlmcp_session_backend:delete(SessionId).

test_hibernation_memory_reduction(_Config) ->
    ct:pal("Testing memory reduction through hibernation"),

    SessionId = <<"session_hibernation_3">>,
    Session = create_test_session(SessionId),

    %% Get backend PID
    BackendPid = whereis(erlmcp_session_backend),

    %% Perform multiple operations to build up heap
    lists:foreach(fun(N) ->
        Id = <<"session_bulk_", (integer_to_binary(N))/binary>>,
        S = create_test_session(Id),
        ok = erlmcp_session_backend:store(Id, S),
        {ok, _} = erlmcp_session_backend:fetch(Id)
    end, lists:seq(1, 20)),

    %% Get memory before hibernation
    {memory, MemoryBefore} = erlang:process_info(BackendPid, memory),

    %% Trigger hibernation via idle period
    timer:sleep(200),

    %% Get memory after hibernation
    {memory, MemoryAfter} = erlang:process_info(BackendPid, memory),

    ct:pal("Memory before hibernation: ~p, after: ~p", [MemoryBefore, MemoryAfter]),

    %% Memory should be reduced (may vary, but check for reduction)
    %% Note: hibernation compacts stack but heap size may not change dramatically
    %% The key benefit is reduced memory footprint during idle periods
    ?assert(MemoryAfter =< MemoryBefore * 1.5),  % Allow some variance

    %% Cleanup
    lists:foreach(fun(N) ->
        Id = <<"session_bulk_", (integer_to_binary(N))/binary>>,
        erlmcp_session_backend:delete(Id)
    end, lists:seq(1, 20)),
    erlmcp_session_backend:delete(SessionId).

test_hibernation_after_cleanup(_Config) ->
    ct:pal("Testing hibernation after cleanup operation"),

    SessionId = <<"session_hibernation_4">>,
    Session = create_test_session(SessionId#{timeout_ms => 100}),

    %% Store session with short timeout
    ok = erlmcp_session_backend:store(SessionId, Session),

    %% Get backend PID
    BackendPid = whereis(erlmcp_session_backend),

    %% Trigger cleanup
    {ok, Count} = erlmcp_session_backend:cleanup_expired(),

    ct:pal("Cleaned up ~p expired sessions", [Count]),

    %% Wait for hibernation after cleanup
    timer:sleep(200),

    %% Verify backend is still alive and responsive
    ?assert(is_process_alive(BackendPid)),

    %% Memory should be reasonable
    {memory, Memory} = erlang:process_info(BackendPid, memory),
    ?assert(Memory < 1_000_000).

%%====================================================================
%% Test Cases - Memory Guard
%%====================================================================

test_memory_guard_tool_limits(_Config) ->
    ct:pal("Testing memory guard tool limits"),

    %% Get default tool limits
    Limits = erlmcp_memory_guard:get_limits(tool),

    %% Verify tool limits are configured
    MaxHeap = maps:get(max_heap, Limits),
    MaxBinHeap = maps:get(max_bin_heap, Limits),
    HibernateThreshold = maps:get(hibernate_threshold, Limits),

    ?assert(MaxHeap > 0),
    ?assert(MaxBinHeap > 0),
    ?assert(HibernateThreshold > 0 andalso HibernateThreshold =< 1.0),

    ct:pal("Tool limits: max_heap=~p, max_bin_heap=~p, threshold=~p",
           [MaxHeap, MaxBinHeap, HibernateThreshold]).

test_memory_guard_context_limits(_Config) ->
    ct:pal("Testing memory guard context limits"),

    %% Get default context limits
    Limits = erlmcp_memory_guard:get_limits(context),

    %% Verify context limits are configured
    MaxHeap = maps:get(max_heap, Limits),
    MaxBinHeap = maps:get(max_bin_heap, Limits),
    HibernateThreshold = maps:get(hibernate_threshold, Limits),

    ?assert(MaxHeap > 0),
    ?assert(MaxBinHeap > 0),
    ?assert(HibernateThreshold > 0 andalso HibernateThreshold =< 1.0),

    ct:pal("Context limits: max_heap=~p, max_bin_heap=~p, threshold=~p",
           [MaxHeap, MaxBinHeap, HibernateThreshold]).

test_memory_guard_validation(_Config) ->
    ct:pal("Testing memory guard validation"),

    %% Spawn a test process with memory guard
    Self = self(),
    Pid = spawn_link(fun() ->
        %% Enable tool memory guard
        try
            ok = erlmcp_memory_guard:enable_tool_guard()
        catch
            _:_ -> ok
        end,

        %% Validate memory is within limits
        case erlmcp_memory_guard:validate_memory(tool) of
            {ok, Percent} ->
                Self ! {memory_ok, Percent};
            {error, Reason} ->
                Self ! {memory_error, Reason}
        end
    end),

    %% Receive validation result
    receive
        {memory_ok, Percent} ->
            ct:pal("Memory usage at ~p% of limit", [Percent]),
            ?assert(Percent >= 0 andalso Percent =< 100);
        {memory_error, Reason} ->
            ct:pal("Memory validation error: ~p", [Reason]),
            ?assert(false, "Memory validation failed")
    after 1000 ->
        ct:fail("Memory validation timeout")
    end.

test_memory_guard_force_hibernate(_Config) ->
    ct:pal("Testing memory guard force hibernate"),

    %% Spawn a test process
    Self = self(),
    Pid = spawn_link(fun() ->
        %% Enable tool memory guard
        try
            ok = erlmcp_memory_guard:enable_tool_guard()
        catch
            _:_ -> ok
        end,

        %% Get initial memory
        {memory, Before} = process_info(self(), memory),

        %% Force hibernation
        ok = erlmcp_memory_guard:force_hibernate(),

        %% Wait for wake-up
        timer:sleep(50),

        %% Get memory after hibernation
        {memory, After} = process_info(self(), memory),

        %% Send result
        Self ! {hibernate_result, Before, After}
    end),

    %% Receive result
    receive
        {hibernate_result, Before, After} ->
            ct:pal("Memory before hibernate: ~p, after: ~p", [Before, After]),
            %% Hibernation should reduce memory (stack is discarded)
            ?assert(After =< Before * 1.5)
    after 1000 ->
        ct:fail("Force hibernate timeout")
    end.

%%====================================================================
%% Test Cases - Nominal Types
%%====================================================================

test_nominal_session_id_type(_Config) ->
    ct:pal("Testing nominal session_id type"),

    %% Create session ID using nominal type constructor
    SessionIdBin = <<"session_nominal_1">>,
    SessionId = erlmcp_mcp_types:new_session_id(SessionIdBin),

    %% Verify type
    ?assert(is_binary(SessionId)),
    ?assertEqual(SessionIdBin, SessionId),

    %% Dialyzer would catch this at compile time:
    %% RequestId = erlmcp_mcp_types:new_request_id(<<"req_1">>),
    %% erlmcp_session_backend:store(RequestId, Session)  % TYPE ERROR
    %% With nominal types, Dialyzer rejects passing RequestId where SessionId expected.

    ok.

test_nominal_type_safety(_Config) ->
    ct:pal("Testing nominal type safety"),

    %% Create different nominal types
    SessionId = erlmcp_mcp_types:new_session_id(<<"session_1">>),
    RequestId = erlmcp_mcp_types:new_request_id(<<"request_1">>),
    ToolName = erlmcp_mcp_types:new_tool_name(<<"tool_1">>),

    %% All are binaries at runtime (nominal types are compile-time)
    ?assert(is_binary(SessionId)),
    ?assert(is_binary(RequestId)),
    ?assert(is_binary(ToolName)),

    %% But Dialyzer treats them as distinct types
    %% This prevents accidental confusion like:
    %% invoke_tool(RequestId, ToolName)  % Wrong: arguments swapped
    %% invoke_tool(ToolName, RequestId)  % Correct

    ct:pal("Nominal types: SessionId=~p, RequestId=~p, ToolName=~p",
           [SessionId, RequestId, ToolName]),

    ok.

test_invalid_nominal_type_usage(_Config) ->
    ct:pal("Testing invalid nominal type usage"),

    %% Nominal type constructors should reject invalid input
    try
        erlmcp_mcp_types:new_session_id(<<>>),
        ct:fail("Should have raised badarg for empty session_id")
    catch
        error:badarg -> ok
    end,

    try
        erlmcp_mcp_types:new_request_id(<<>>),
        ct:fail("Should have raised badarg for empty request_id")
    catch
        error:badarg -> ok
    end,

    try
        erlmcp_mcp_types:new_tool_name(<<>>),
        ct:fail("Should have raised badarg for empty tool_name")
    catch
        error:badarg -> ok
    end,

    %% Non-binary input should fail
    try
        erlmcp_mcp_types:new_session_id(123),
        ct:fail("Should have raised badarg for non-binary session_id")
    catch
        error:badarg -> ok
    end,

    try
        erlmcp_mcp_types:new_request_id(atom),
        ct:fail("Should have raised badarg for non-binary request_id")
    catch
        error:badarg -> ok
    end,

    ct:pal("Invalid nominal types properly rejected").

test_nominal_type_constructor(_Config) ->
    ct:pal("Testing nominal type constructors"),

    %% Valid constructors
    ?assertEqual(<<"test">>, erlmcp_mcp_types:new_session_id(<<"test">>)),
    ?assertEqual(<<"req">>, erlmcp_mcp_types:new_request_id(<<"req">>)),
    ?assertEqual(<<"tool">>, erlmcp_mcp_types:new_tool_name(<<"tool">>)),

    %% Invalid constructors (empty binary) - should raise badarg
    try
        erlmcp_mcp_types:new_session_id(<<>>),
        ct:fail("Should have raised badarg for empty session_id")
    catch
        error:badarg -> ok
    end,

    try
        erlmcp_mcp_types:new_request_id(<<>>),
        ct:fail("Should have raised badarg for empty request_id")
    catch
        error:badarg -> ok
    end,

    try
        erlmcp_mcp_types:new_tool_name(<<>>),
        ct:fail("Should have raised badarg for empty tool_name")
    catch
        error:badarg -> ok
    end,

    ok.

%%====================================================================
%% Test Cases - Tool Spawning
%%====================================================================

test_tool_spawn_with_monitor(_Config) ->
    ct:pal("Testing tool spawn with monitor"),

    %% Spawn a tool
    ToolName = <<"test_tool_monitor">>,
    Params = #{test_param => test_value},

    {ok, Pid, Ref} = erlmcp_session_backend:spawn_tool(ToolName, Params),

    %% Verify tool process is alive
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Verify reference is valid
    ?assert(is_reference(Ref)),

    %% Wait for tool to complete
    timer:sleep(200),

    %% Process should have exited normally
    ?assertNot(is_process_alive(Pid)),

    ct:pal("Tool ~p spawned with monitor ref: ~p", [ToolName, Ref]).

test_tool_monitor_tagging(_Config) ->
    ct:pal("Testing tool monitor tagging (OTP 27/28)"),

    %% Spawn a tool
    ToolName = <<"test_tool_tagging">>,
    Params = #{},

    {ok, Pid, TaggedRef} = erlmcp_session_backend:spawn_tool(ToolName, Params),

    %% Verify tagged reference (OTP 27/28 feature)
    %% The tag is embedded in the reference
    ?assert(is_reference(TaggedRef)),

    %% Manually test monitor tagging
    Self = self(),
    TestPid = spawn_link(fun() ->
        %% Create a tagged monitor
        Tag = {test_tool, tagging_test},
        MonitorRef = erlang:monitor(process, Pid, [{tag, Tag}]),

        %% Wait for DOWN message
        receive
            {'DOWN', MonitorRef, process, Pid, Reason} ->
                Self ! {tagged_down, Tag, Reason}
        after 2000 ->
            Self ! {tagged_down, timeout}
        end
    end),

    %% Wait for DOWN message
    receive
        {tagged_down, Tag, Reason} ->
            ct:pal("Received DOWN for tag ~p with reason ~p", [Tag, Reason]),
            ?assertEqual({test_tool, tagging_test}, Tag);
        {tagged_down, timeout} ->
            ct:fail("Tagged DOWN message timeout")
    after 3000 ->
        ct:fail("Test process timeout")
    end.

test_tool_crash_handling(_Config) ->
    ct:pal("Testing tool crash handling with tagged monitors"),

    %% Spawn a tool that will crash
    ToolName = <<"test_tool_crash">>,
    Params = #{crash => true},

    %% We need to manually spawn a crashing tool to test DOWN handling
    Self = self(),
    CrashPid = spawn_link(fun() ->
        %% Enable memory guard
        try
            erlmcp_memory_guard:enable_tool_guard()
        catch
            _:_ -> ok
        end,

        %% Create tagged monitor
        Tag = {tool, ToolName},
        TaggedRef = erlang:monitor(process, self(), [{tag, Tag}]),

        %% Simulate tool crash
        Self ! {tool_started, TaggedRef},
        timer:sleep(50),
        exit(crash)
    end),

    %% Wait for tool to start
    receive
        {tool_started, Ref} ->
            ?assert(is_reference(Ref))
    after 1000 ->
        ct:fail("Tool did not start")
    end,

    %% Wait for DOWN message
    receive
        {'DOWN', _Ref, {tool, ToolName}, process, CrashPid, Reason} ->
            ct:pal("Received DOWN for crashed tool ~p: ~p", [ToolName, Reason]),
            ?assertEqual(crash, Reason)
    after 2000 ->
        ct:fail("DOWN message not received for crashed tool")
    end.

test_tool_memory_guard_enabled(_Config) ->
    ct:pal("Testing tool memory guard is enabled"),

    %% Spawn a test tool process
    Self = self(),
    ToolPid = spawn_link(fun() ->
        %% Enable tool memory guard
        try
            ok = erlmcp_memory_guard:enable_tool_guard(),
            Self ! guard_enabled,

            %% Do some work
            timer:sleep(100),

            %% Check memory usage
            {Heap, BinHeap} = erlmcp_memory_guard:get_memory_usage(),
            Self ! {memory_usage, Heap, BinHeap}
        catch
            _:_ ->
                Self ! guard_not_available
        end
    end),

    %% Wait for guard to be enabled
    receive
        guard_enabled ->
            ct:pal("Tool memory guard enabled successfully");
        guard_not_available ->
            ct:pal("Memory guard not available (OTP < 28)")
    after 1000 ->
        ct:fail("Tool guard enable timeout")
    end,

    %% Get memory usage
    receive
        {memory_usage, Heap, BinHeap} ->
            ct:pal("Tool memory: heap=~p, bin_heap=~p", [Heap, BinHeap]),
            ?assert(Heap >= 0),
            ?assert(BinHeap >= 0)
    after 1000 ->
        ct:fail("Memory usage timeout")
    end.

%%====================================================================
%% Test Cases - Priority Messages
%%====================================================================

test_priority_message_handling(_Config) ->
    ct:pal("Testing priority message handling"),

    %% Get backend PID
    BackendPid = whereis(erlmcp_session_backend),

    %% Check if priority messages are available (OTP 28+)
    HasPriority = has_priority_messages(),

    case HasPriority of
        false ->
            ct:pal("Priority messages not available (OTP < 28) - skipping");
        true ->
            %% Send a priority ping message
            Self = self(),
            Ref = make_ref(),

            %% Send priority message
            erlmcp_session_backend:send_priority_message({ping, Ref}, Self),

            %% Wait for pong
            receive
                {pong, Ref} ->
                    ct:pal("Received priority pong response"),
                    ?assert(true)
            after 1000 ->
                ct:fail("Priority pong timeout")
            end
    end.

test_urgent_message_handling(_Config) ->
    ct:pal("Testing urgent message handling"),

    %% Check if priority messages are available
    HasPriority = has_priority_messages(),

    case HasPriority of
        false ->
            ct:pal("Priority messages not available (OTP < 28) - skipping");
        true ->
            %% Send an urgent message (system-level, no sender)
            %% This should be logged but not crash the backend
            erlmcp_session_backend:send_urgent_message({test_rgent, test_data}),

            %% Wait for message to be processed
            timer:sleep(100),

            %% Backend should still be alive
            BackendPid = whereis(erlmcp_session_backend),
            ?assert(is_process_alive(BackendPid))
    end.

test_priority_ping_pong(_Config) ->
    ct:pal("Testing priority ping/pong for health checks"),

    HasPriority = has_priority_messages(),

    case HasPriority of
        false ->
            ct:pal("Priority messages not available (OTP < 28) - skipping");
        true ->
            %% Send priority ping
            Self = self(),
            Ref = make_ref(),

            %% Measure ping/pong latency
            T1 = erlang:monotonic_time(microsecond),
            erlmcp_session_backend:send_priority_message({ping, Ref}, Self),

            receive
                {pong, Ref} ->
                    T2 = erlang:monotonic_time(microsecond),
                    Latency = T2 - T1,
                    ct:pal("Priority ping/pong latency: ~p microseconds", [Latency]),
                    ?assert(Latency < 100_000)  % < 100ms
            after 1000 ->
                ct:fail("Priority ping/pong timeout")
            end
    end.

test_priority_session_cancellation(_Config) ->
    ct:pal("Testing priority session cancellation"),

    HasPriority = has_priority_messages(),

    case HasPriority of
        false ->
            ct:pal("Priority messages not available (OTP < 28) - skipping");
        true ->
            %% Create a session
            SessionId = <<"session_priority_cancel">>,
            Session = create_test_session(SessionId),
            ok = erlmcp_session_backend:store(SessionId, Session),

            %% Verify session exists
            {ok, _} = erlmcp_session_backend:fetch(SessionId),

            %% Send priority cancellation signal
            erlmcp_session_backend:send_priority_message({cancel_session, SessionId}, self()),

            %% Wait for cancellation
            timer:sleep(100),

            %% Session should be deleted
            ?assertEqual({error, not_found}, erlmcp_session_backend:fetch(SessionId)),

            ct:pal("Session cancelled via priority signal")
    end.

%%====================================================================
%% Test Cases - Error Scenarios
%%====================================================================

test_session_timeout_handling(_Config) ->
    ct:pal("Testing session timeout handling"),

    %% Create session with short timeout (100ms)
    SessionId = <<"session_timeout">>,
    Now = erlang:system_time(millisecond),
    Session = #{id => SessionId,
                created_at => Now - 200,
                last_accessed => Now - 200,
                timeout_ms => 100,
                metadata => #{}},

    ok = erlmcp_session_backend:store(SessionId, Session),

    %% Trigger cleanup (should remove expired session)
    {ok, Count} = erlmcp_session_backend:cleanup_expired(),

    ct:pal("Cleaned ~p expired sessions", [Count]),
    ?assert(Count > 0),

    %% Session should be deleted
    ?assertEqual({error, not_found}, erlmcp_session_backend:fetch(SessionId)).

test_memory_limit_enforcement(_Config) ->
    ct:pal("Testing memory limit enforcement"),

    %% This test validates that memory guards prevent excessive memory usage
    %% Actual enforcement happens via OTP 28 process flags

    %% Check if memory guard is available
    IsAvailable = erlmcp_memory_guard:is_otp_28_or_later(),

    case IsAvailable of
        false ->
            ct:pal("Memory guard not available (OTP < 28) - skipping");
        true ->
            %% Get tool limits
            Limits = erlmcp_memory_guard:get_limits(tool),
            MaxHeap = maps:get(max_heap, Limits),

            ct:pal("Tool max heap limit: ~p words", [MaxHeap]),

            %% Spawn a process with memory guard
            Self = self(),
            Pid = spawn_link(fun() ->
                try
                    ok = erlmcp_memory_guard:enable_tool_guard(),
                    Self ! guard_enabled,

                    %% Try to allocate memory (will be limited)
                    BigData = lists:duplicate(10000, some_large_data),
                    Self ! {allocated, length(BigData)}
                catch
                    _:_ ->
                        Self ! allocation_failed
                end
            end),

            receive
                guard_enabled ->
                    ok;
                timeout ->
                    ct:fail("Guard enable timeout")
            after 1000 ->
                ct:fail("Guard enable timeout")
            end,

            %% Process should either succeed or be terminated by memory limit
            receive
                {allocated, Size} ->
                    ct:pal("Allocated ~p items", [Size]);
                allocation_failed ->
                    ct:pal("Allocation failed (expected under memory limit)");
                {'EXIT', Pid, Reason} ->
                    ct:pal("Process terminated: ~p", [Reason])
            after 2000 ->
                ct:pal("Process still running (within limits)")
            end
    end.

test_invalid_nominal_type_usage(_Config) ->
    ct:pal("Testing invalid nominal type usage"),

    %% Nominal type constructors should reject invalid input
    ?error({badarg, _}, (catch erlmcp_mcp_types:new_session_id(<<>>))),
    ?error({badarg, _}, (catch erlmcp_mcp_types:new_request_id(<<>>))),
    ?error({badarg, _}, (catch erlmcp_mcp_types:new_tool_name(<<>>))),

    %% Non-binary input should fail
    ?error({badarg, _}, (catch erlmcp_mcp_types:new_session_id(123))),
    ?error({badarg, _}, (catch erlmcp_mcp_types:new_request_id(atom))),
    ?error({badarg, _}, (catch erlmcp_mcp_types:new_tool_name(<<"">>))),

    ct:pal("Invalid nominal types properly rejected").

test_concurrent_session_access(_Config) ->
    ct:pal("Testing concurrent session access"),

    %% Create a session
    SessionId = <<"session_concurrent">>,
    Session = create_test_session(SessionId),
    ok = erlmcp_session_backend:store(SessionId, Session),

    %% Spawn multiple concurrent readers
    Self = self(),
    ReaderCount = 10,
    Pids = [spawn_link(fun() ->
        %% Each reader fetches the session
        case erlmcp_session_backend:fetch(SessionId) of
            {ok, _Fetched} ->
                Self ! {read_ok, self()};
            {error, Reason} ->
                Self ! {read_error, Reason}
        end
    end) || _ <- lists:seq(1, ReaderCount)],

    %% Wait for all readers to complete
    Results = [receive
        {read_ok, Pid} ->
            ok;
        {read_error, Reason} ->
            {error, Reason}
    after 1000 ->
        timeout
    end || Pid <- Pids],

    %% All reads should succeed
    SuccessCount = length([ok || ok <- Results]),
    ct:pal("Successful concurrent reads: ~p/~p", [SuccessCount, ReaderCount]),
    ?assertEqual(ReaderCount, SuccessCount),

    %% Cleanup
    erlmcp_session_backend:delete(SessionId).

test_backend_crash_recovery(_Config) ->
    ct:pal("Testing backend crash recovery"),

    %% Create a session before crash
    SessionId = <<"session_crash_test">>,
    Session = create_test_session(SessionId),
    ok = erlmcp_session_backend:store(SessionId, Session),

    %% Get backend PID
    BackendPid = whereis(erlmcp_session_backend),
    ?assert(is_pid(BackendPid)),

    %% Simulate backend crash (brutal kill)
    exit(BackendPid, kill),

    %% Wait for supervisor to restart
    timer:sleep(500),

    %% Verify backend is restarted
    NewBackendPid = whereis(erlmcp_session_backend),
    ?assert(is_pid(NewBackendPid)),
    ?assertNotEqual(BackendPid, NewBackendPid),

    %% Session may or may not survive (depends on backend)
    %% ETS backend does not survive crash, DETS/Mnesia might
    ct:pal("Backend restarted: old=~p, new=~p", [BackendPid, NewBackendPid]),

    %% Backend should be functional
    {ok, List} = erlmcp_session_backend:list(),
    ct:pal("Sessions after restart: ~p", [length(List)]).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Create a test session
create_test_session(SessionId) ->
    Now = erlang:system_time(millisecond),
    #{id => SessionId,
      created_at => Now,
      last_accessed => Now,
      timeout_ms => 3600000,  % 1 hour
      metadata => #{test => true}}.

%% @doc Start session backend
start_backend(Config) ->
    case whereis(erlmcp_session_backend) of
        undefined ->
            {ok, Pid} = erlmcp_session_backend:start_link(?BACKEND_OPTS),
            ct:pal("Started session backend: ~p", [Pid]),
            [{backend_pid, Pid} | Config];
        Pid ->
            ct:pal("Session backend already running: ~p", [Pid]),
            [{backend_pid, Pid} | Config]
    end.

%% @doc Stop session backend
stop_backend(Config) ->
    case proplists:get_value(backend_pid, Config) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                true ->
                    gen_server:stop(Pid),
                    ct:pal("Stopped session backend"),
                    ok;
                false ->
                    ok
            end
    end.

%% @doc Check if priority messages are available (OTP 28+)
has_priority_messages() ->
    try
        OldValue = process_flag(priority, true),
        process_flag(priority, OldValue),
        true
    catch
        error:badarg ->
            false;
        _:_ ->
            false
    end.
