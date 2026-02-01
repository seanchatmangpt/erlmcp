%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_client_fsm
%%% Following Chicago School TDD - testing behavior, not implementation
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_client_fsm_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Test Descriptions
%%====================================================================

%% Test suite for client FSM lifecycle
client_fsm_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_initial_state/1,
      fun test_connect_transition/1,
      fun test_initialize_transition/1,
      fun test_disconnect_transition/1,
      fun test_error_state_transition/1,
      fun test_reconnect_logic/1,
      fun test_state_name_query/1,
      fun test_priority_health_check/1,
      fun test_priority_drain_signal/1,
      fun test_priority_cancel_signal/1]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Start client FSM for testing
    ClientId = make_ref(),
    {ok, Pid} = erlmcp_client_fsm:start_link(ClientId, #{auto_reconnect => false}),
    #{pid => Pid, client_id => ClientId}.

cleanup(#{pid := Pid}) ->
    case is_process_alive(Pid) of
        true ->
            erlmcp_client_fsm:stop(Pid);
        false ->
            ok
    end.

%%====================================================================
%% Test Cases
%%====================================================================

test_initial_state(#{pid := Pid}) ->
    %% Client FSM should start in pre_initialization state
    State = erlmcp_client_fsm:state_name(Pid),
    ?_assertEqual(pre_initialization, State).

test_connect_transition(#{pid := Pid}) ->
    %% Client should transition from pre_initialization to initializing
    ok = erlmcp_client_fsm:connect(Pid),
    State = erlmcp_client_fsm:state_name(Pid),
    ?_assertEqual(initializing, State).

test_initialize_transition(#{pid := Pid}) ->
    %% Client should transition from initializing to initialized after successful init
    ok = erlmcp_client_fsm:connect(Pid),
    InitParams =
        #{protocol_version => <<"2025-11-25">>, capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_client_fsm:initialize(Pid, InitParams),
    State = erlmcp_client_fsm:state_name(Pid),
    ?_assertEqual(initialized, State).

test_disconnect_transition(#{pid := Pid}) ->
    %% Client should transition from initialized to disconnected
    ok = erlmcp_client_fsm:connect(Pid),
    InitParams =
        #{protocol_version => <<"2025-11-25">>, capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_client_fsm:initialize(Pid, InitParams),
    ok = erlmcp_client_fsm:disconnect(Pid),
    State = erlmcp_client_fsm:state_name(Pid),
    ?_assertEqual(disconnected, State).

test_error_state_transition(#{pid := Pid}) ->
    %% Client should transition to error state on initialization failure
    ok = erlmcp_client_fsm:connect(Pid),
    %% Trigger error by providing invalid params
    BadParams = #{invalid => true},
    Result = erlmcp_client_fsm:initialize(Pid, BadParams),
    State = erlmcp_client_fsm:state_name(Pid),
    [?_assertMatch({error, _}, Result), ?_assertEqual(error, State)].

test_reconnect_logic(#{pid := Pid}) ->
    %% Client should support reconnect from disconnected state
    ok = erlmcp_client_fsm:connect(Pid),
    InitParams =
        #{protocol_version => <<"2025-11-25">>, capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_client_fsm:initialize(Pid, InitParams),
    ok = erlmcp_client_fsm:disconnect(Pid),

    %% Reconnect should transition to initializing
    ok = erlmcp_client_fsm:reconnect(Pid),
    State = erlmcp_client_fsm:state_name(Pid),
    ?_assertEqual(initializing, State).

test_state_name_query(#{pid := Pid}) ->
    %% State name query should work in any state
    State1 = erlmcp_client_fsm:state_name(Pid),
    ok = erlmcp_client_fsm:connect(Pid),
    State2 = erlmcp_client_fsm:state_name(Pid),
    [?_assertEqual(pre_initialization, State1), ?_assertEqual(initializing, State2)].

test_priority_health_check(#{pid := Pid}) ->
    %% Health check should work as priority message in any state
    Result = gen_statem:call(Pid, {health_check}),
    ?_assertEqual(ok, Result).

test_priority_drain_signal(#{pid := Pid}) ->
    %% Drain signal should transition to disconnected from any state
    ok = erlmcp_client_fsm:connect(Pid),
    ok = gen_statem:call(Pid, drain),
    State = erlmcp_client_fsm:state_name(Pid),
    ?_assertEqual(disconnected, State).

test_priority_cancel_signal(#{pid := Pid}) ->
    %% Cancel signal should transition to disconnected immediately
    ok = erlmcp_client_fsm:connect(Pid),
    ok = gen_statem:call(Pid, cancel),
    State = erlmcp_client_fsm:state_name(Pid),
    ?_assertEqual(disconnected, State).

%%====================================================================
%% Property-Based Test Stubs (for future PropEr integration)
%%====================================================================

%% TODO: Add PropEr model-based tests for state machine
%% - Valid state transitions
%% - Invalid state transitions should be rejected
%% - State invariants hold across all transitions
