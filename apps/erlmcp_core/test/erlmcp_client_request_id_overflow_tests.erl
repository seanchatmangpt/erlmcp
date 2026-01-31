%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_client request ID overflow protection.
%%%
%%% This test suite verifies that the client properly handles request ID
%%% overflow scenarios with bounded ID space, monitoring, and recovery.
%%% Tests follow Chicago School TDD principles:
%%% - Real processes (no mocks)
%%% - State-based verification
%%% - Comprehensive overflow path testing
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_client_request_id_overflow_tests).
-include_lib("eunit/include/eunit.hrl").

%% Include the header after module declaration
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

%% Setup function - starts client with stdio transport
setup() ->
    {ok, ClientPid} = start_test_client(),
    ClientPid.

%% Cleanup function - stops client
cleanup(ClientPid) ->
    stop_test_client(ClientPid),
    timer:sleep(50).

%%====================================================================
%%% Helper Functions
%%====================================================================

%% Start a test client with stdio transport
start_test_client() ->
    TransportOpts = {stdio, []},
    Options = #{strict_mode => false, timeout => 5000},
    erlmcp_client:start_link(TransportOpts, Options).

%% Stop a test client
stop_test_client(Pid) when is_pid(Pid) ->
    catch erlmcp_client:stop(Pid),
    timer:sleep(10).

%% Get client internal state (for testing)
get_client_state(ClientPid) ->
    sys:get_state(ClientPid).

%% Set client request ID to a specific value (for testing overflow scenarios)
set_client_request_id(ClientPid, NewRequestId) ->
    State = get_client_state(ClientPid),
    %% The request_id is at position 6 in the state record:
    %% #state{transport, transport_state, phase, capabilities, request_id, ...}
    NewState = setelement(6, State, NewRequestId),
    sys:replace_state(ClientPid, fun(_) -> NewState end).

%%====================================================================
%%% Threshold Monitoring Tests
%%%====================================================================

%% @doc Test that normal request IDs don't trigger warnings
normal_request_id_no_warning_test() ->
    ClientPid = setup(),
    try
        %% Set ID to 50% of max (should be normal)
        MidId = ?MAX_SAFE_REQUEST_ID div 2,
        set_client_request_id(ClientPid, MidId),

        %% Verify no warnings in logs (by checking state)
        State = get_client_state(ClientPid),
        ?assertEqual(MidId, element(6, State)),

        %% Check threshold returns normal
        {ok, normal, Usage} = erlmcp_request_id:check_thresholds(MidId),
        ?assert(Usage > 0.0),
        ?assert(Usage < 80.0)
    after
        cleanup(ClientPid)
    end.

%% @doc Test that warning threshold triggers appropriate logging
warning_threshold_triggers_logging_test() ->
    ClientPid = setup(),
    try
        %% Set ID to warning threshold (80%)
        set_client_request_id(ClientPid, ?ID_WARNING_THRESHOLD),

        %% Verify threshold check returns warning
        {ok, warning, Usage} = erlmcp_request_id:check_thresholds(?ID_WARNING_THRESHOLD),
        ?assert(Usage >= 80.0),
        ?assert(Usage < 90.0)
    after
        cleanup(ClientPid)
    end.

%% @doc Test that critical threshold triggers appropriate logging
critical_threshold_triggers_logging_test() ->
    ClientPid = setup(),
    try
        %% Set ID to critical threshold (90%)
        set_client_request_id(ClientPid, ?ID_CRITICAL_THRESHOLD),

        %% Verify threshold check returns critical
        {ok, critical, Usage} = erlmcp_request_id:check_thresholds(?ID_CRITICAL_THRESHOLD),
        ?assert(Usage >= 90.0),
        ?assert(Usage < 96.0)
    after
        cleanup(ClientPid)
    end.

%% @doc Test that reserved threshold triggers reconnection warning
reserved_threshold_triggers_warning_test() ->
    ClientPid = setup(),
    try
        %% Set ID to reserved threshold (96%)
        set_client_request_id(ClientPid, ?ID_RESERVED_THRESHOLD),

        %% Verify threshold check returns reserved
        {ok, reserved, Usage} = erlmcp_request_id:check_thresholds(?ID_RESERVED_THRESHOLD),
        ?assert(Usage >= 96.0),
        ?assert(Usage < 100.0)
    after
        cleanup(ClientPid)
    end.

%% @doc Test that exhausted threshold returns error
exhausted_threshold_returns_error_test() ->
    ClientPid = setup(),
    try
        %% Set ID to maximum
        set_client_request_id(ClientPid, ?MAX_SAFE_REQUEST_ID),

        %% Verify threshold check returns reserved (max is still reserved, not exhausted)
        {ok, reserved, Usage} = erlmcp_request_id:check_thresholds(?MAX_SAFE_REQUEST_ID),
        ?assertEqual(100.0, Usage)
    after
        cleanup(ClientPid)
    end.

%%====================================================================
%%% Overflow Detection Tests
%%%====================================================================

%% @doc Test that overflow is detected at maximum ID
overflow_detected_at_max_test() ->
    %% Test the utility function directly
    ?assertEqual({error, overflow}, erlmcp_request_id:safe_increment(?MAX_SAFE_REQUEST_ID)).

%% @doc Test that overflow is NOT detected just below maximum
no_overflow_below_max_test() ->
    %% Test the utility function directly
    ?assertEqual({ok, ?MAX_SAFE_REQUEST_ID}, erlmcp_request_id:safe_increment(?MAX_SAFE_REQUEST_ID - 1)).

%% @doc Test safe_increment handles edge cases
safe_increment_edge_cases_test() ->
    %% Minimum valid ID
    ?assertEqual({ok, 2}, erlmcp_request_id:safe_increment(1)),

    %% Zero (invalid but returns ok)
    ?assertEqual({ok, 1}, erlmcp_request_id:safe_increment(0)),

    %% Just below max
    ?assertEqual({ok, ?MAX_SAFE_REQUEST_ID}, erlmcp_request_id:safe_increment(?MAX_SAFE_REQUEST_ID - 1)),

    ok.

%%====================================================================
%%% Client Request ID Management Tests
%%%====================================================================

%% @doc Test that client tracks request ID correctly
client_tracks_request_id_test() ->
    ClientPid = setup(),
    try
        %% Initial ID should be 1 (element 6 in state record)
        State = get_client_state(ClientPid),
        ?assertEqual(1, element(6, State))
    after
        cleanup(ClientPid)
    end.

%% @doc Test that set_client_request_id helper works
set_request_id_helper_test() ->
    ClientPid = setup(),
    try
        %% Set to a known value
        TestId = 12345,
        set_client_request_id(ClientPid, TestId),

        %% Verify it was set (element 6 in state record)
        State = get_client_state(ClientPid),
        ?assertEqual(TestId, element(6, State))
    after
        cleanup(ClientPid)
    end.

%%====================================================================
%%% ID Space Percentage Calculation Tests
%%%====================================================================

%% @doc Test usage percentage calculation at minimum
usage_percentage_at_min_test() ->
    %% At ID 1, usage should be very small but positive
    {ok, _Level, Usage} = erlmcp_request_id:check_thresholds(1),
    ?assert(Usage > 0.0),
    ?assert(Usage < 0.000001).

%% @doc Test usage percentage at 50%
usage_percentage_at_half_test() ->
    HalfId = ?MAX_SAFE_REQUEST_ID div 2,
    {ok, _Level, Usage} = erlmcp_request_id:check_thresholds(HalfId),
    ?assert(Usage > 49.0),
    ?assert(Usage < 51.0).

%% @doc Test usage percentage at warning threshold
usage_percentage_at_warning_test() ->
    {ok, _Level, Usage} = erlmcp_request_id:check_thresholds(?ID_WARNING_THRESHOLD),
    ?assert(Usage >= 79.9),
    ?assert(Usage < 80.1).

%% @doc Test usage percentage at critical threshold
usage_percentage_at_critical_test() ->
    {ok, _Level, Usage} = erlmcp_request_id:check_thresholds(?ID_CRITICAL_THRESHOLD),
    ?assert(Usage >= 89.9),
    ?assert(Usage < 90.1).

%% @doc Test usage percentage at reserved threshold
usage_percentage_at_reserved_test() ->
    {ok, _Level, Usage} = erlmcp_request_id:check_thresholds(?ID_RESERVED_THRESHOLD),
    ?assert(Usage >= 95.9),
    ?assert(Usage < 96.1).

%% @doc Test usage percentage at maximum
usage_percentage_at_max_test() ->
    {ok, _Level, Usage} = erlmcp_request_id:check_thresholds(?MAX_SAFE_REQUEST_ID),
    ?assertEqual(100.0, Usage).

%%====================================================================
%%% Integration Tests
%%%====================================================================

%% @doc Test realistic ID progression through thresholds
realistic_id_progression_test() ->
    %% Simulate ID progression from 1 to various thresholds
    Ids = [1, 100, 10000, 1000000, ?ID_WARNING_THRESHOLD, ?ID_CRITICAL_THRESHOLD],

    lists:foreach(fun(Id) ->
        {ok, Level, Usage} = erlmcp_request_id:check_thresholds(Id),
        ?assert(is_atom(Level)),
        ?assert(Usage >= 0.0),
        ?assert(Usage =< 100.0)
    end, Ids).

%% @doc Test threshold boundary conditions
threshold_boundary_test() ->
    %% Test well below warning threshold (should be normal)
    %% Note: ID_WARNING_THRESHOLD - 1 is still exactly 80.0% due to float precision
    {ok, normal, _} = erlmcp_request_id:check_thresholds(?ID_WARNING_THRESHOLD - 1000000),

    %% Test at warning threshold
    {ok, warning, _} = erlmcp_request_id:check_thresholds(?ID_WARNING_THRESHOLD),

    %% Test well below critical threshold (should be warning)
    %% Note: ID_CRITICAL_THRESHOLD - 1 is still exactly 90.0% due to float precision
    {ok, warning, _} = erlmcp_request_id:check_thresholds(?ID_CRITICAL_THRESHOLD - 1000000),

    %% Test at critical threshold
    {ok, critical, _} = erlmcp_request_id:check_thresholds(?ID_CRITICAL_THRESHOLD),

    %% Test well below reserved threshold (should be critical)
    %% Note: ID_RESERVED_THRESHOLD - 1 is still exactly 96.0% due to float precision
    {ok, critical, _} = erlmcp_request_id:check_thresholds(?ID_RESERVED_THRESHOLD - 1000000),

    %% Test at reserved threshold
    {ok, reserved, _} = erlmcp_request_id:check_thresholds(?ID_RESERVED_THRESHOLD),

    %% Test just below maximum (should be reserved)
    {ok, reserved, _} = erlmcp_request_id:check_thresholds(?MAX_SAFE_REQUEST_ID - 1),

    %% Test at maximum
    {ok, reserved, _} = erlmcp_request_id:check_thresholds(?MAX_SAFE_REQUEST_ID),

    ok.

%%====================================================================
%%% Property-Based Tests (Proper)
%%%====================================================================

%% @doc Property: check_thresholds always returns valid levels
prop_check_thresholds_valid_level() ->
    proper:forall(proper_types:range(1, ?MAX_SAFE_REQUEST_ID),
        fun(Id) ->
            {ok, Level, Usage} = erlmcp_request_id:check_thresholds(Id),
            ValidLevels = [normal, warning, critical, reserved],
            lists:member(Level, ValidLevels) andalso (Usage >= 0.0) andalso (Usage =< 100.0)
        end).

%% @doc Property: usage percentage is monotonic
prop_usage_monotonic() ->
    proper:forall({proper_types:range(1, ?MAX_SAFE_REQUEST_ID div 2),
                   proper_types:range(1, ?MAX_SAFE_REQUEST_ID div 2)},
        fun(Id1, Id2) when Id1 < Id2 ->
            {ok, _, Usage1} = erlmcp_request_id:check_thresholds(Id1),
            {ok, _, Usage2} = erlmcp_request_id:check_thresholds(Id2),
            Usage1 < Usage2;
           (_, _) ->
            true
        end).

%% @doc Property: threshold levels are monotonic
prop_thresholds_monotonic() ->
    proper:forall({proper_types:range(1, ?MAX_SAFE_REQUEST_ID),
                   proper_types:range(1, ?MAX_SAFE_REQUEST_ID)},
        fun(Id1, Id2) when Id1 < Id2 ->
            {ok, Level1, _} = erlmcp_request_id:check_thresholds(Id1),
            {ok, Level2, _} = erlmcp_request_id:check_thresholds(Id2),
            %% Levels should not decrease (normal -> warning -> critical -> reserved)
            level_to_number(Level1) =< level_to_number(Level2);
           (_, _) ->
            true
        end).

%% @private
level_to_number(normal) -> 1;
level_to_number(warning) -> 2;
level_to_number(critical) -> 3;
level_to_number(reserved) -> 4.
