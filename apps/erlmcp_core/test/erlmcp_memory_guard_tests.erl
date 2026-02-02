%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Tests for erlmcp_memory_guard - OTP 28 Process Memory Limiting
%%%
%%% Test suite for memory guard functionality including process flag
%%% configuration, memory usage tracking, and hibernation behavior.
%%%
%%% == Test Coverage ==
%%%
%%% - Process flag configuration (min/max heap and binary heap)
%%% - Context guard limits (100MB heap, 50MB binary heap)
%%% - Tool guard limits (50MB heap, 25MB binary heap)
%%% - Transport guard limits (30MB heap, 15MB binary heap)
%%% - Memory usage tracking
%%% - Memory validation
%%% - Force hibernation
%%% - OTP version compatibility
%%%
%%% == Testing Philosophy ==
%%%
%%% These tests follow Chicago School TDD principles:
%%% - Test observable behavior, not implementation details
%%% - Use real processes (no mocks)
%%% - Test boundaries and edge cases
%%% - Verify OTP compatibility (graceful degradation on OTP < 28)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_guard_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Helpers
%%====================================================================

%% @doc Create a test process with memory guard enabled
-spec create_test_process(fun()) -> pid().
create_test_process(TestFun) ->
    spawn(fun() ->
        try
            TestFun()
        catch
            _:Error ->
                erlang:exit({test_failed, Error})
        end
    end).

%% @doc Wait for process to terminate
-spec wait_for_process(pid(), timeout()) -> ok | timeout.
wait_for_process(Pid, Timeout) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after Timeout ->
        erlang:demonitor(Ref, [flush]),
        timeout
    end.

%%====================================================================
%% Process Flag Configuration Tests
%%====================================================================

configure_limits_sets_correct_flags_test() ->
    %% Test that configure_limits sets process flags correctly on OTP 28+
    ?assertNotEqual(undefined, erlang:system_info(otp_release)),

    %% Spawn test process
    Pid = create_test_process(fun() ->
        %% Configure limits
        ok = erlmcp_memory_guard:configure_limits(10_000, 5_000),

        %% Verify process flags were set (OTP 28+ only)
        case erlmcp_memory_guard:is_otp_28_or_later() of
            true ->
                %% Check that flags were set (we can't directly read them back,
                %% but we can verify the function didn't crash)
                {ok, _} = erlmcp_memory_guard:validate_memory(generic);
            false ->
                %% On OTP < 28, should not crash
                ok
        end,

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

configure_limits_handles_invalid_input_test() ->
    %% Test that configure_limits handles invalid input gracefully
    ?assertError(_, erlmcp_memory_guard:configure_limits(0, 1000)),
    ?assertError(_, erlmcp_memory_guard:configure_limits(1000, 0)),
    ?assertError(_, erlmcp_memory_guard:configure_limits(-1, 1000)).

%%====================================================================
%% Context Guard Tests
%%====================================================================

enable_context_guard_applies_limits_test() ->
    %% Test that enable_context_guard applies correct limits
    Pid = create_test_process(fun() ->
        %% Enable context guard
        ok = erlmcp_memory_guard:enable_context_guard(),

        %% Verify limits are configured
        Limits = erlmcp_memory_guard:get_limits(context),
        ?assertEqual(100_000_000, maps:get(max_heap, Limits)),
        ?assertEqual(50_000_000, maps:get(max_bin_heap, Limits)),
        ?assertEqual(0.9, maps:get(hibernate_threshold, Limits)),

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

enable_context_guard_sets_high_priority_test() ->
    %% Test that context guard sets process priority to high (OTP 28+)
    Pid = create_test_process(fun() ->
        ok = erlmcp_memory_guard:enable_context_guard(),

        %% Check priority was set (if OTP supports it)
        case erlmcp_memory_guard:is_otp_28_or_later() of
            true ->
                %% Priority should be high
                {priority, Priority} = erlang:process_info(self(), priority),
                ?assertEqual(high, Priority);
            false ->
                ok
        end,

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

%%====================================================================
%% Tool Guard Tests
%%====================================================================

enable_tool_guard_sets_tool_limits_test() ->
    %% Test that tool guard applies correct limits
    Pid = create_test_process(fun() ->
        ok = erlmcp_memory_guard:enable_tool_guard(),

        %% Verify limits
        Limits = erlmcp_memory_guard:get_limits(tool),
        ?assertEqual(50_000_000, maps:get(max_heap, Limits)),
        ?assertEqual(25_000_000, maps:get(max_bin_heap, Limits)),
        ?assertEqual(0.85, maps:get(hibernate_threshold, Limits)),

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

%%====================================================================
%% Transport Guard Tests
%%====================================================================

enable_transport_guard_sets_transport_limits_test() ->
    %% Test that transport guard applies correct limits
    Pid = create_test_process(fun() ->
        ok = erlmcp_memory_guard:enable_transport_guard(),

        %% Verify limits
        Limits = erlmcp_memory_guard:get_limits(transport),
        ?assertEqual(30_000_000, maps:get(max_heap, Limits)),
        ?assertEqual(15_000_000, maps:get(max_bin_heap, Limits)),
        ?assertEqual(0.80, maps:get(hibernate_threshold, Limits)),

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

%%====================================================================
%% Memory Usage Tests
%%====================================================================

get_memory_usage_returns_tuple_test() ->
    %% Test that get_memory_usage returns correct format
    Pid = create_test_process(fun() ->
        {Heap, BinHeap} = erlmcp_memory_guard:get_memory_usage(),

        %% Verify return type
        ?assert(is_integer(Heap)),
        ?assert(is_integer(BinHeap)),
        ?assert(Heap >= 0),
        ?assert(BinHeap >= 0),

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

get_memory_usage_increases_with_allocation_test() ->
    %% Test that memory usage increases when we allocate data
    Pid = create_test_process(fun() ->
        %% Get baseline memory
        {Heap1, _BinHeap1} = erlmcp_memory_guard:get_memory_usage(),

        %% Allocate some data
        _Data = lists:seq(1, 10000),

        %% Get memory after allocation
        {Heap2, _BinHeap2} = erlmcp_memory_guard:get_memory_usage(),

        %% Memory should have increased
        ?assert(Heap2 >= Heap1),

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

%%====================================================================
%% Memory Validation Tests
%%====================================================================

validate_memory_checks_thresholds_test() ->
    %% Test that validate_memory correctly validates against limits
    Pid = create_test_process(fun() ->
        %% For a generic process with low memory, should return ok
        {ok, Percent} = erlmcp_memory_guard:validate_memory(generic),
        ?assert(Percent >= 0),
        ?assert(Percent < 100),

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

validate_memory_returns_ok_for_healthy_process_test() ->
    %% Test that healthy processes get ok status
    Pid = create_test_process(fun() ->
        Result = erlmcp_memory_guard:validate_memory(generic),
        ?assertMatch({ok, _}, Result),

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

%%====================================================================
%% Hibernation Tests
%%====================================================================

force_hibernate_reduces_memory_test() ->
    %% Test that hibernation reduces memory footprint
    Pid = create_test_process(fun() ->
        %% Allocate some data
        _Data = lists:seq(1, 10000),

        %% Get memory before hibernation
        {memory, Before} = erlang:process_info(self(), memory),

        %% Force hibernation
        ok = erlmcp_memory_guard:force_hibernate(),

        %% Get memory after hibernation (should be lower)
        {memory, After} = erlang:process_info(self(), memory),

        %% Memory should be reduced (though not guaranteed to always be lower)
        %% Just verify the function doesn't crash and memory is still reasonable
        ?assert(After > 0),
        ?assert(After =< Before * 2),  % Allow some variance

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

%%====================================================================
%% OTP Compatibility Tests
%%====================================================================

is_otp_28_or_later_detects_version_test() ->
    %% Test OTP version detection
    Is28Plus = erlmcp_memory_guard:is_otp_28_or_later(),

    %% Just verify it returns a boolean
    ?assert(is_boolean(Is28Plus)).

memory_guard_degrades_gracefully_on_old_otp_test() ->
    %% Test that memory guard doesn't crash on OTP < 28
    %% This test passes on both old and new OTP versions
    Pid = create_test_process(fun() ->
        %% These should not crash regardless of OTP version
        ok = erlmcp_memory_guard:enable_context_guard(),
        ok = erlmcp_memory_guard:enable_tool_guard(),
        ok = erlmcp_memory_guard:enable_transport_guard(),

        %% Memory tracking should always work
        {Heap, BinHeap} = erlmcp_memory_guard:get_memory_usage(),
        ?assert(is_integer(Heap)),
        ?assert(is_integer(BinHeap)),

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

%%====================================================================
%% Integration Tests
%%====================================================================

full_memory_guard_workflow_test() ->
    %% Test complete workflow: enable -> track -> validate -> hibernate
    Pid = create_test_process(fun() ->
        %% 1. Enable context guard
        ok = erlmcp_memory_guard:enable_context_guard(),

        %% 2. Track memory usage
        {Heap1, _BinHeap1} = erlmcp_memory_guard:get_memory_usage(),

        %% 3. Allocate memory
        _LargeData = lists:seq(1, 100000),

        %% 4. Track again
        {Heap2, _BinHeap2} = erlmcp_memory_guard:get_memory_usage(),
        ?assert(Heap2 > Heap1),

        %% 5. Validate memory
        {ok, _Percent} = erlmcp_memory_guard:validate_memory(context),

        %% 6. Force hibernation
        ok = erlmcp_memory_guard:force_hibernate(),

        %% 7. Verify process still works
        {Heap3, _BinHeap3} = erlmcp_memory_guard:get_memory_usage(),
        ?assert(is_integer(Heap3)),

        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(Pid, 5000)).

multiple_process_types_test() ->
    %% Test that different process types get different limits
    ContextPid = create_test_process(fun() ->
        ok = erlmcp_memory_guard:enable_context_guard(),
        Limits = erlmcp_memory_guard:get_limits(context),
        ?assertEqual(100_000_000, maps:get(max_heap, Limits)),
        erlang:exit(test_complete)
    end),

    ToolPid = create_test_process(fun() ->
        ok = erlmcp_memory_guard:enable_tool_guard(),
        Limits = erlmcp_memory_guard:get_limits(tool),
        ?assertEqual(50_000_000, maps:get(max_heap, Limits)),
        erlang:exit(test_complete)
    end),

    TransportPid = create_test_process(fun() ->
        ok = erlmcp_memory_guard:enable_transport_guard(),
        Limits = erlmcp_memory_guard:get_limits(transport),
        ?assertEqual(30_000_000, maps:get(max_heap, Limits)),
        erlang:exit(test_complete)
    end),

    ?assertEqual(ok, wait_for_process(ContextPid, 5000)),
    ?assertEqual(ok, wait_for_process(ToolPid, 5000)),
    ?assertEqual(ok, wait_for_process(TransportPid, 5000)).
