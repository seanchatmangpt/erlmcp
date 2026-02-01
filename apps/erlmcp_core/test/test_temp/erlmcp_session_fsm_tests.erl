%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_session_fsm
%%% Following Chicago School TDD - testing behavior, not implementation
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_fsm_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Test Descriptions
%%====================================================================

%% Test suite for session FSM lifecycle
session_fsm_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_initial_state/1,
      fun test_negotiate_transition/1,
      fun test_suspend_resume_cycle/1,
      fun test_close_transition/1,
      fun test_connection_management/1,
      fun test_connection_death_handling/1,
      fun test_state_name_query/1,
      fun test_priority_health_check/1,
      fun test_priority_cancel_signal/1]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Start session FSM for testing
    SessionId = <<"test-session-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    {ok, Pid} = erlmcp_session_fsm:start_link(SessionId, #{timeout_ms => infinity}),
    #{pid => Pid, session_id => SessionId}.

cleanup(#{pid := Pid}) ->
    case is_process_alive(Pid) of
        true ->
            erlmcp_session_fsm:stop(Pid);
        false ->
            ok
    end.

%%====================================================================
%% Test Cases
%%====================================================================

test_initial_state(#{pid := Pid}) ->
    %% Session FSM should start in negotiation state
    State = erlmcp_session_fsm:state_name(Pid),
    ?_assertEqual(negotiation, State).

test_negotiate_transition(#{pid := Pid}) ->
    %% Session should transition from negotiation to active
    NegotiateParams =
        #{server_capabilities => #mcp_server_capabilities{},
          client_capabilities => #mcp_client_capabilities{},
          protocol_version => <<"2025-11-25">>},
    ok = erlmcp_session_fsm:negotiate(Pid, NegotiateParams),
    State = erlmcp_session_fsm:state_name(Pid),
    ?_assertEqual(active, State).

test_suspend_resume_cycle(#{pid := Pid}) ->
    %% Session should support suspend/resume cycle
    NegotiateParams =
        #{server_capabilities => #mcp_server_capabilities{},
          client_capabilities => #mcp_client_capabilities{},
          protocol_version => <<"2025-11-25">>},
    ok = erlmcp_session_fsm:negotiate(Pid, NegotiateParams),

    %% Suspend
    ok = erlmcp_session_fsm:suspend(Pid),
    State1 = erlmcp_session_fsm:state_name(Pid),

    %% Resume
    ok = erlmcp_session_fsm:resume(Pid),
    State2 = erlmcp_session_fsm:state_name(Pid),

    [?_assertEqual(suspended, State1), ?_assertEqual(active, State2)].

test_close_transition(#{pid := Pid}) ->
    %% Session should transition to closed from any state
    NegotiateParams =
        #{server_capabilities => #mcp_server_capabilities{},
          client_capabilities => #mcp_client_capabilities{},
          protocol_version => <<"2025-11-25">>},
    ok = erlmcp_session_fsm:negotiate(Pid, NegotiateParams),
    ok = erlmcp_session_fsm:close(Pid),
    State = erlmcp_session_fsm:state_name(Pid),
    ?_assertEqual(closed, State).

test_connection_management(#{pid := Pid}) ->
    %% Session should manage connections as supervisor
    NegotiateParams =
        #{server_capabilities => #mcp_server_capabilities{},
          client_capabilities => #mcp_client_capabilities{},
          protocol_version => <<"2025-11-25">>},
    ok = erlmcp_session_fsm:negotiate(Pid, NegotiateParams),

    %% Add connection
    ConnId = make_ref(),
    ConnPid = spawn(fun() -> timer:sleep(5000) end),
    ok = erlmcp_session_fsm:add_connection(Pid, {ConnId, ConnPid}),

    %% Get connections
    Connections = erlmcp_session_fsm:get_connections(Pid),
    HasConnection = maps:is_key(ConnId, Connections),

    %% Remove connection
    ok = erlmcp_session_fsm:remove_connection(Pid, ConnId),
    Connections2 = erlmcp_session_fsm:get_connections(Pid),
    NoConnection = not maps:is_key(ConnId, Connections2),

    %% Clean up
    exit(ConnPid, kill),

    [?_assert(HasConnection), ?_assert(NoConnection)].

test_connection_death_handling(#{pid := Pid}) ->
    %% Session should handle connection process death
    NegotiateParams =
        #{server_capabilities => #mcp_server_capabilities{},
          client_capabilities => #mcp_client_capabilities{},
          protocol_version => <<"2025-11-25">>},
    ok = erlmcp_session_fsm:negotiate(Pid, NegotiateParams),

    %% Add connection
    ConnId = make_ref(),
    ConnPid = spawn(fun() -> timer:sleep(100) end),
    ok = erlmcp_session_fsm:add_connection(Pid, {ConnId, ConnPid}),

    %% Kill connection
    exit(ConnPid, kill),
    timer:sleep(200),

    %% Session should have removed dead connection and transitioned to suspended
    Connections = erlmcp_session_fsm:get_connections(Pid),
    State = erlmcp_session_fsm:state_name(Pid),

    [?_assertEqual(0, maps:size(Connections)), ?_assertEqual(suspended, State)].

test_state_name_query(#{pid := Pid}) ->
    %% State name query should work in any state
    State1 = erlmcp_session_fsm:state_name(Pid),
    NegotiateParams =
        #{server_capabilities => #mcp_server_capabilities{},
          client_capabilities => #mcp_client_capabilities{},
          protocol_version => <<"2025-11-25">>},
    ok = erlmcp_session_fsm:negotiate(Pid, NegotiateParams),
    State2 = erlmcp_session_fsm:state_name(Pid),
    [?_assertEqual(negotiation, State1), ?_assertEqual(active, State2)].

test_priority_health_check(#{pid := Pid}) ->
    %% Health check should work as priority message in any state
    Result = gen_statem:call(Pid, {health_check}),
    ?_assertMatch({ok, _}, Result).

test_priority_cancel_signal(#{pid := Pid}) ->
    %% Cancel signal should transition to closed immediately
    NegotiateParams =
        #{server_capabilities => #mcp_server_capabilities{},
          client_capabilities => #mcp_client_capabilities{},
          protocol_version => <<"2025-11-25">>},
    ok = erlmcp_session_fsm:negotiate(Pid, NegotiateParams),
    ok = gen_statem:call(Pid, cancel),
    State = erlmcp_session_fsm:state_name(Pid),
    ?_assertEqual(closed, State).

%%====================================================================
%% Property-Based Test Stubs (for future PropEr integration)
%%====================================================================

%% TODO: Add PropEr model-based tests for state machine
%% - Valid state transitions
%% - Connection lifecycle as supervisor
%% - Timeout behavior
%% - State invariants hold across all transitions
