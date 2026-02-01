%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_server_fsm
%%% Following Chicago School TDD - testing behavior, not implementation
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_server_fsm_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Descriptions
%%====================================================================

%% Test suite for server FSM lifecycle
server_fsm_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_initial_state/1,
      fun test_accept_connections_transition/1,
      fun test_drain_transition/1,
      fun test_shutdown_transition/1,
      fun test_connection_tracking/1,
      fun test_state_name_query/1,
      fun test_priority_health_check/1,
      fun test_priority_drain_signal/1,
      fun test_priority_cancel_signal/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Start server FSM for testing
    ServerId = make_ref(),
    Capabilities = #mcp_server_capabilities{},
    {ok, Pid} = erlmcp_server_fsm:start_link(ServerId, #{capabilities => Capabilities}),
    #{pid => Pid, server_id => ServerId}.

cleanup(#{pid := Pid}) ->
    case is_process_alive(Pid) of
        true -> erlmcp_server_fsm:stop(Pid);
        false -> ok
    end.

%%====================================================================
%% Test Cases
%%====================================================================

test_initial_state(#{pid := Pid}) ->
    %% Server FSM should start in initialization state
    State = erlmcp_server_fsm:state_name(Pid),
    ?_assertEqual(initialization, State).

test_accept_connections_transition(#{pid := Pid}) ->
    %% Server should transition from initialization to accepting
    ok = erlmcp_server_fsm:accept_connections(Pid),
    State = erlmcp_server_fsm:state_name(Pid),
    ?_assertEqual(accepting, State).

test_drain_transition(#{pid := Pid}) ->
    %% Server should transition from accepting to drain
    ok = erlmcp_server_fsm:accept_connections(Pid),
    ok = erlmcp_server_fsm:drain(Pid),
    State = erlmcp_server_fsm:state_name(Pid),
    ?_assertEqual(drain, State).

test_shutdown_transition(#{pid := Pid}) ->
    %% Server should transition to shutdown from any state
    ok = erlmcp_server_fsm:accept_connections(Pid),
    ok = erlmcp_server_fsm:shutdown(Pid),
    State = erlmcp_server_fsm:state_name(Pid),
    ?_assertEqual(shutdown, State).

test_connection_tracking(#{pid := Pid}) ->
    %% Server should track active connections
    ok = erlmcp_server_fsm:accept_connections(Pid),

    %% Simulate connection establishment
    ConnPid = spawn(fun() -> timer:sleep(1000) end),
    Pid ! {connection_established, ConnPid},

    %% Give time for message processing
    timer:sleep(100),

    %% Health check should show 1 active connection
    {ok, Health} = gen_statem:call(Pid, {health_check}),
    ActiveConns = maps:get(active_connections, Health),
    ?_assertEqual(1, ActiveConns).

test_state_name_query(#{pid := Pid}) ->
    %% State name query should work in any state
    State1 = erlmcp_server_fsm:state_name(Pid),
    ok = erlmcp_server_fsm:accept_connections(Pid),
    State2 = erlmcp_server_fsm:state_name(Pid),
    [
        ?_assertEqual(initialization, State1),
        ?_assertEqual(accepting, State2)
    ].

test_priority_health_check(#{pid := Pid}) ->
    %% Health check should work as priority message in any state
    Result = gen_statem:call(Pid, {health_check}),
    ?_assertMatch({ok, _}, Result).

test_priority_drain_signal(#{pid := Pid}) ->
    %% Drain signal should transition to drain from accepting state
    ok = erlmcp_server_fsm:accept_connections(Pid),
    ok = gen_statem:call(Pid, drain),
    State = erlmcp_server_fsm:state_name(Pid),
    ?_assertEqual(drain, State).

test_priority_cancel_signal(#{pid := Pid}) ->
    %% Cancel signal should transition to shutdown immediately
    ok = erlmcp_server_fsm:accept_connections(Pid),
    ok = gen_statem:call(Pid, cancel),
    State = erlmcp_server_fsm:state_name(Pid),
    ?_assertEqual(shutdown, State).

%%====================================================================
%% Property-Based Test Stubs (for future PropEr integration)
%%====================================================================

%% TODO: Add PropEr model-based tests for state machine
%% - Valid state transitions
%% - Connection lifecycle tracking
%% - Drain timeout behavior
%% - State invariants hold across all transitions
