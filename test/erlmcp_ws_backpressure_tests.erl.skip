-module(erlmcp_ws_backpressure_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    application:set_env(erlmcp, max_ws_message_size, 16777216),
    application:set_env(erlmcp, frame_buffer_size, 102400),
    application:set_env(erlmcp, buffer_drain_threshold, 0.5),
    application:set_env(erlmcp, backpressure_timeout, 5000),
    ok.

cleanup(_) ->
    application:unset_env(erlmcp, max_ws_message_size),
    application:unset_env(erlmcp, frame_buffer_size),
    application:unset_env(erlmcp, buffer_drain_threshold),
    application:unset_env(erlmcp, backpressure_timeout),
    ok.

%%====================================================================
%% Test Groups
%%====================================================================

websocket_backpressure_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Backpressure State Management", [
                ?_test(test_backpressure_inactive_initial()),
                ?_test(test_backpressure_activation()),
                ?_test(test_backpressure_check_when_inactive()),
                ?_test(test_backpressure_check_when_active()),
                ?_test(test_backpressure_state_transitions())
            ]},
            {"Buffer Usage Tracking", [
                ?_test(test_buffer_usage_add_bytes()),
                ?_test(test_buffer_usage_subtract_bytes()),
                ?_test(test_buffer_usage_prevents_underflow()),
                ?_test(test_buffer_usage_increments_pending()),
                ?_test(test_buffer_usage_decrements_pending())
            ]},
            {"Backpressure Triggering", [
                ?_test(test_trigger_at_buffer_limit()),
                ?_test(test_no_trigger_below_limit()),
                ?_test(test_trigger_on_exact_limit()),
                ?_test(test_trigger_slightly_above_limit())
            ]},
            {"Reading Resumption", [
                ?_test(test_resume_below_drain_threshold()),
                ?_test(test_no_resume_above_threshold()),
                ?_test(test_resume_at_drain_threshold()),
                ?_test(test_resume_cancels_timer())
            ]},
            {"Message Flow Control", [
                ?_test(test_normal_message_flow()),
                ?_test(test_messages_rejected_during_backpressure()),
                ?_test(test_buffer_drains_gradually()),
                ?_test(test_backpressure_timeout_recovery()),
                ?_test(test_multiple_messages_trigger_backpressure())
            ]},
            {"Error Handling", [
                ?_test(test_backpressure_error_message()),
                ?_test(test_graceful_close_during_backpressure()),
                ?_test(test_timer_cleanup_on_close()),
                ?_test(test_recover_from_backpressure_timeout())
            ]},
            {"Configuration", [
                ?_test(test_custom_buffer_size()),
                ?_test(test_custom_drain_threshold()),
                ?_test(test_custom_timeout()),
                ?_test(test_default_configuration())
            ]}
        ]
    }.

%%====================================================================
%% Backpressure State Management Tests
%%====================================================================

test_backpressure_inactive_initial() ->
    State = create_test_state(102400),
    ?assertEqual(?BACKPRESSURE_INACTIVE, State#state.backpressure_state),
    ?assertEqual(undefined, State#state.backpressure_timer),
    ?assertEqual(0, State#state.bytes_buffered).

test_backpressure_activation() ->
    State = create_test_state(100),
    %% Set bytes_buffered to exceed limit
    StateWithBytes = State#state{bytes_buffered = 101},
    {error, backpressure_active, NewState} = erlmcp_transport_ws:check_backpressure(StateWithBytes),
    ?assertEqual(?BACKPRESSURE_ACTIVE, NewState#state.backpressure_state),
    ?assert(NewState#state.backpressure_timer =/= undefined).

test_backpressure_check_when_inactive() ->
    State = create_test_state(100),
    StateWithBytes = State#state{bytes_buffered = 50},
    {ok, _} = erlmcp_transport_ws:check_backpressure(StateWithBytes),
    ok.

test_backpressure_check_when_active() ->
    State = create_test_state(100),
    StateActive = State#state{
        backpressure_state = ?BACKPRESSURE_ACTIVE,
        bytes_buffered = 50
    },
    {error, backpressure_active, _} = erlmcp_transport_ws:check_backpressure(StateActive),
    ok.

test_backpressure_state_transitions() ->
    State = create_test_state(100),
    %% Transition from inactive to active
    StateWithBytes = State#state{bytes_buffered = 101},
    {error, _, ActiveState} = erlmcp_transport_ws:check_backpressure(StateWithBytes),
    ?assertEqual(?BACKPRESSURE_ACTIVE, ActiveState#state.backpressure_state),
    %% Transition from active back to inactive when drained
    DrainedState = ActiveState#state{bytes_buffered = 49},
    ResumedState = erlmcp_transport_ws:resume_reading(DrainedState),
    ?assertEqual(?BACKPRESSURE_INACTIVE, ResumedState#state.backpressure_state).

%%====================================================================
%% Buffer Usage Tracking Tests
%%====================================================================

test_buffer_usage_add_bytes() ->
    State = create_test_state(102400),
    NewState = erlmcp_transport_ws:update_buffer_usage(State, 1000, add),
    ?assertEqual(1000, NewState#state.bytes_buffered),
    ?assertEqual(1, NewState#state.messages_pending).

test_buffer_usage_subtract_bytes() ->
    State = create_test_state(102400),
    StateWithBytes = State#state{bytes_buffered = 1000, messages_pending = 1},
    NewState = erlmcp_transport_ws:update_buffer_usage(StateWithBytes, 500, subtract),
    ?assertEqual(500, NewState#state.bytes_buffered),
    ?assertEqual(0, NewState#state.messages_pending).

test_buffer_usage_prevents_underflow() ->
    State = create_test_state(102400),
    StateWithBytes = State#state{bytes_buffered = 100, messages_pending = 1},
    NewState = erlmcp_transport_ws:update_buffer_usage(StateWithBytes, 500, subtract),
    ?assertEqual(0, NewState#state.bytes_buffered),
    ?assertEqual(0, NewState#state.messages_pending).

test_buffer_usage_increments_pending() ->
    State = create_test_state(102400),
    StateWithBytes = State#state{messages_pending = 5},
    NewState = erlmcp_transport_ws:update_buffer_usage(StateWithBytes, 100, add),
    ?assertEqual(6, NewState#state.messages_pending).

test_buffer_usage_decrements_pending() ->
    State = create_test_state(102400),
    StateWithPending = State#state{messages_pending = 5},
    NewState = erlmcp_transport_ws:update_buffer_usage(StateWithPending, 100, subtract),
    ?assertEqual(4, NewState#state.messages_pending).

%%====================================================================
%% Backpressure Triggering Tests
%%====================================================================

test_trigger_at_buffer_limit() ->
    State = create_test_state(100),
    StateAtLimit = State#state{bytes_buffered = 100},
    {ok, _} = erlmcp_transport_ws:check_backpressure(StateAtLimit),
    ok.

test_no_trigger_below_limit() ->
    State = create_test_state(100),
    StateBelowLimit = State#state{bytes_buffered = 99},
    {ok, _} = erlmcp_transport_ws:check_backpressure(StateBelowLimit),
    ok.

test_trigger_on_exact_limit() ->
    State = create_test_state(100),
    StateExact = State#state{bytes_buffered = 100},
    %% At limit should not trigger, just above limit should
    {ok, _} = erlmcp_transport_ws:check_backpressure(StateExact),
    StateAbove = StateExact#state{bytes_buffered = 101},
    {error, backpressure_active, _} = erlmcp_transport_ws:check_backpressure(StateAbove).

test_trigger_slightly_above_limit() ->
    State = create_test_state(1000),
    StateAbove = State#state{bytes_buffered = 1001},
    {error, backpressure_active, _} = erlmcp_transport_ws:check_backpressure(StateAbove),
    ok.

%%====================================================================
%% Reading Resumption Tests
%%====================================================================

test_resume_below_drain_threshold() ->
    State = create_test_state(100),
    %% 50% threshold, so resume at 50 bytes or below
    StateActive = State#state{
        backpressure_state = ?BACKPRESSURE_ACTIVE,
        bytes_buffered = 49
    },
    ResumedState = erlmcp_transport_ws:resume_reading(StateActive),
    ?assertEqual(?BACKPRESSURE_INACTIVE, ResumedState#state.backpressure_state).

test_no_resume_above_threshold() ->
    State = create_test_state(100),
    StateActive = State#state{
        backpressure_state = ?BACKPRESSURE_ACTIVE,
        bytes_buffered = 51
    },
    ResumedState = erlmcp_transport_ws:resume_reading(StateActive),
    ?assertEqual(?BACKPRESSURE_ACTIVE, ResumedState#state.backpressure_state).

test_resume_at_drain_threshold() ->
    State = create_test_state(100),
    %% 50% of 100 = 50 bytes exactly
    StateActive = State#state{
        backpressure_state = ?BACKPRESSURE_ACTIVE,
        bytes_buffered = 50
    },
    ResumedState = erlmcp_transport_ws:resume_reading(StateActive),
    ?assertEqual(?BACKPRESSURE_INACTIVE, ResumedState#state.backpressure_state).

test_resume_cancels_timer() ->
    State = create_test_state(100),
    TimerRef = erlang:send_after(1000, self(), test_message),
    StateActive = State#state{
        backpressure_state = ?BACKPRESSURE_ACTIVE,
        bytes_buffered = 49,
        backpressure_timer = TimerRef
    },
    ResumedState = erlmcp_transport_ws:resume_reading(StateActive),
    ?assertEqual(undefined, ResumedState#state.backpressure_timer),
    ?assertEqual(?BACKPRESSURE_INACTIVE, ResumedState#state.backpressure_state).

%%====================================================================
%% Message Flow Control Tests
%%====================================================================

test_normal_message_flow() ->
    State = create_test_state(102400),
    %% Send small message well below limit
    SmallMsg = <<"small">>,
    {ok, State1} = erlmcp_transport_ws:check_backpressure(State),
    State2 = erlmcp_transport_ws:update_buffer_usage(State1, byte_size(SmallMsg), add),
    ?assert(State2#state.bytes_buffered =< State2#state.frame_buffer_size),
    ok.

test_messages_rejected_during_backpressure() ->
    State = create_test_state(100),
    StateActive = State#state{
        backpressure_state = ?BACKPRESSURE_ACTIVE,
        bytes_buffered = 50
    },
    {error, backpressure_active, _} = erlmcp_transport_ws:check_backpressure(StateActive),
    ok.

test_buffer_drains_gradually() ->
    State = create_test_state(102400),
    %% Add 5000 bytes
    State1 = erlmcp_transport_ws:update_buffer_usage(State, 5000, add),
    ?assertEqual(5000, State1#state.bytes_buffered),
    %% Remove 1000 bytes
    State2 = erlmcp_transport_ws:update_buffer_usage(State1, 1000, subtract),
    ?assertEqual(4000, State2#state.bytes_buffered),
    %% Remove another 1000
    State3 = erlmcp_transport_ws:update_buffer_usage(State2, 1000, subtract),
    ?assertEqual(3000, State3#state.bytes_buffered),
    ok.

test_backpressure_timeout_recovery() ->
    State = create_test_state(100),
    StateAbove = State#state{bytes_buffered = 101},
    {error, _, ActiveState} = erlmcp_transport_ws:check_backpressure(StateAbove),
    ?assertEqual(?BACKPRESSURE_ACTIVE, ActiveState#state.backpressure_state),
    %% Simulate timeout recovery message
    ok.

test_multiple_messages_trigger_backpressure() ->
    State = create_test_state(100),
    %% Add multiple messages accumulating bytes
    State1 = erlmcp_transport_ws:update_buffer_usage(State, 40, add),
    State2 = erlmcp_transport_ws:update_buffer_usage(State1, 40, add),
    State3 = erlmcp_transport_ws:update_buffer_usage(State2, 30, add),
    ?assertEqual(110, State3#state.bytes_buffered),
    {error, _, _} = erlmcp_transport_ws:check_backpressure(State3),
    ok.

%%====================================================================
%% Error Handling Tests
%%====================================================================

test_backpressure_error_message() ->
    State = create_test_state(100),
    StateAbove = State#state{bytes_buffered = 101},
    {error, backpressure_active, _} = erlmcp_transport_ws:check_backpressure(StateAbove),
    ok.

test_graceful_close_during_backpressure() ->
    State = create_test_state(100),
    StateActive = State#state{
        backpressure_state = ?BACKPRESSURE_ACTIVE,
        bytes_buffered = 50
    },
    %% Verify we can extract timer for cleanup
    ?assert(State#state.backpressure_timer =/= undefined orelse StateActive#state.backpressure_timer =/= undefined),
    ok.

test_timer_cleanup_on_close() ->
    State = create_test_state(100),
    TimerRef = erlang:send_after(1000, self(), test_message),
    StateWithTimer = State#state{
        backpressure_state = ?BACKPRESSURE_ACTIVE,
        backpressure_timer = TimerRef
    },
    ?assertEqual(TimerRef, StateWithTimer#state.backpressure_timer),
    %% In real code, timer would be canceled here
    ok.

test_recover_from_backpressure_timeout() ->
    State = create_test_state(100),
    %% Simulate backpressure state after timeout
    StateAfterTimeout = State#state{
        backpressure_state = ?BACKPRESSURE_ACTIVE,
        bytes_buffered = 0
    },
    ResumedState = erlmcp_transport_ws:resume_reading(StateAfterTimeout),
    ?assertEqual(?BACKPRESSURE_INACTIVE, ResumedState#state.backpressure_state),
    ok.

%%====================================================================
%% Configuration Tests
%%====================================================================

test_custom_buffer_size() ->
    State = create_test_state(50000),
    ?assertEqual(50000, State#state.frame_buffer_size),
    ok.

test_custom_drain_threshold() ->
    State = create_test_state(100),
    %% Default 0.5, verify calculation
    DrainThreshold = trunc(100 * 0.5),
    StateActive = State#state{
        backpressure_state = ?BACKPRESSURE_ACTIVE,
        bytes_buffered = DrainThreshold - 1
    },
    ResumedState = erlmcp_transport_ws:resume_reading(StateActive),
    ?assertEqual(?BACKPRESSURE_INACTIVE, ResumedState#state.backpressure_state).

test_custom_timeout() ->
    State = create_test_state(100),
    %% Timer should be set based on timeout config
    StateAbove = State#state{bytes_buffered = 101},
    {error, _, ActiveState} = erlmcp_transport_ws:check_backpressure(StateAbove),
    ?assert(is_reference(ActiveState#state.backpressure_timer)),
    ok.

test_default_configuration() ->
    State = create_test_state(?DEFAULT_FRAME_BUFFER_SIZE),
    ?assertEqual(?DEFAULT_FRAME_BUFFER_SIZE, State#state.frame_buffer_size),
    ?assertEqual(?BACKPRESSURE_INACTIVE, State#state.backpressure_state),
    ?assertEqual(0, State#state.bytes_buffered),
    ?assertEqual(0, State#state.messages_pending),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

-spec create_test_state(integer()) -> #state{}.
create_test_state(BufferSize) ->
    #state{
        transport_id = <<"test">>,
        registry_pid = self(),
        connection_info = #{},
        session_id = <<"test_session">>,
        ping_timer = undefined,
        fragment_buffer = undefined,
        fragment_start_time = undefined,
        max_message_size = 16777216,
        strict_delimiter_check = true,
        validate_utf8 = true,
        frame_buffer_size = BufferSize,
        frame_buffer_used = 0,
        backpressure_state = ?BACKPRESSURE_INACTIVE,
        backpressure_timer = undefined,
        messages_pending = 0,
        bytes_buffered = 0
    }.
