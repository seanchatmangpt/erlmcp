%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for Component Health Check Module
%%%
%%% Chicago TDD - Real processes, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_component_health_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Descriptions
%%====================================================================

erlmcp_component_health_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Registry health check returns status", fun test_registry_health/0},
      {"Session manager health check returns status", fun test_session_manager_health/0},
      {"Client health check with invalid PID returns unhealthy", fun test_client_health_invalid_pid/0},
      {"Server health check with invalid PID returns unhealthy", fun test_server_health_invalid_pid/0}
     ]}.

%%====================================================================
%% Setup / Cleanup
%%====================================================================

setup() ->
    % Start necessary applications
    application:ensure_all_started(gproc),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_registry_health() ->
    % Test registry health check
    {Status, Metrics} = erlmcp_component_health:registry_health(),
    
    % Verify status is one of the valid health statuses
    ?assert(lists:member(Status, [healthy, degraded, unhealthy, unknown])),
    
    % Verify metrics is a map
    ?assert(is_map(Metrics)),
    
    % Verify expected metric keys
    ?assert(maps:is_key(status, Metrics)).

test_session_manager_health() ->
    % Test session manager health check
    {Status, Metrics} = erlmcp_component_health:session_manager_health(),
    
    % Verify status is one of the valid health statuses
    ?assert(lists:member(Status, [healthy, degraded, unhealthy, unknown])),
    
    % Verify metrics is a map
    ?assert(is_map(Metrics)),
    
    % Verify expected metric keys
    ?assert(maps:is_key(status, Metrics)).

test_client_health_invalid_pid() ->
    % Test client health with non-existent PID
    InvalidPid = list_to_pid("<0.999999.0>"),  % Likely dead PID
    {Status, Metrics} = erlmcp_component_health:client_health(InvalidPid),
    
    % Should return unhealthy for invalid PID
    ?assertEqual(unhealthy, Status),
    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(reason, Metrics)).

test_server_health_invalid_pid() ->
    % Test server health with non-existent PID
    InvalidPid = list_to_pid("<0.999999.0>"),  % Likely dead PID
    {Status, Metrics} = erlmcp_component_health:server_health(InvalidPid),
    
    % Should return unhealthy for invalid PID
    ?assertEqual(unhealthy, Status),
    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(reason, Metrics)).
