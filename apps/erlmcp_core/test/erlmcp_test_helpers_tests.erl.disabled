-module(erlmcp_test_helpers_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%%%====================================================================
%%% Test Suite for erlmcp_test_helpers
%%% Chicago School TDD: Test the test helpers themselves
%%%====================================================================

%%%====================================================================
%%% Test Generators
%%%====================================================================

test_helpers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Start and stop server", fun test_start_stop_server/0},
          {"With server wrapper", fun test_with_server/0},
          {"Server with custom capabilities", fun test_server_custom_caps/0},
          {"Stop with timeout", fun test_stop_with_timeout/0},
          {"Wait for death", fun test_wait_for_death/0}
         ]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc Test starting and stopping a server with defaults
test_start_stop_server() ->
    {ok, ServerPid} = erlmcp_test_helpers:start_test_server(),
    ?assert(is_pid(ServerPid)),
    ?assert(is_process_alive(ServerPid)),
    ok = erlmcp_test_helpers:stop_test_server(ServerPid),
    ?assertNot(is_process_alive(ServerPid)).

%% @doc Test with_server wrapper ensures cleanup
test_with_server() ->
    Result = erlmcp_test_helpers:with_test_server(fun(ServerPid) ->
        ?assert(is_pid(ServerPid)),
        ?assert(is_process_alive(ServerPid)),
        server_alive
    end),
    ?assertEqual(server_alive, Result).

%% @doc Test server with custom capabilities
test_server_custom_caps() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = false},
        prompts = #mcp_capability{enabled = true}
    },
    {ok, ServerPid} = erlmcp_test_helpers:start_test_server(
        <<"custom_caps_test">>, Caps
    ),
    ?assert(is_pid(ServerPid)),
    ok = erlmcp_test_helpers:stop_test_server(ServerPid).

%% @doc Test stop with custom timeout
test_stop_with_timeout() ->
    {ok, ServerPid} = erlmcp_test_helpers:start_test_server(),
    ?assert(is_process_alive(ServerPid)),
    %% Stop with 1000ms timeout
    ok = erlmcp_test_helpers:stop_test_server(ServerPid, 1000),
    ?assertNot(is_process_alive(ServerPid)).

%% @doc Test wait_for_death internal helper
test_wait_for_death() ->
    {ok, ServerPid} = erlmcp_test_helpers:start_test_server(),
    ?assert(is_process_alive(ServerPid)),
    %% Spawn a process that will kill the server
    spawn(fun() ->
        timer:sleep(100),
        erlmcp_server:stop(ServerPid)
    end),
    %% Wait for death with 5 second timeout
    Result = erlmcp_test_helpers:wait_for_death(ServerPid, 5000),
    ?assertEqual(true, Result),
    ?assertNot(is_process_alive(ServerPid)).
