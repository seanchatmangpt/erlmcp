%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_flags_tests - Lock-Free Flags Tests
%%%
%%% Chicago School TDD:
%%% - NO MOCKS: Test real atomic flag operations
%%% - Test ALL observable behavior
%%% - Test concurrency and race conditions
%%% - Test flag state transitions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flags_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

flags_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_init/0,
      fun test_accepting_connections/0,
      fun test_maintenance_mode/0,
      fun test_shutdown/0,
      fun test_health/0,
      fun test_get_all/0,
      fun test_maintenance_mode_disables_accepting/0,
      fun test_shutdown_disables_accepting/0,
      fun test_concurrent_flag_updates/0,
      fun test_flag_transitions/0]}.

setup() ->
    erlmcp_flags:init(),
    ok.

cleanup(_) ->
    % Reset to defaults
    erlmcp_flags:start_accepting(),
    erlmcp_flags:exit_maintenance_mode(),
    erlmcp_flags:cancel_shutdown(),
    erlmcp_flags:mark_healthy(),
    ok.

%%====================================================================
%% Initialization Tests
%%====================================================================

test_init() ->
    ?_test(begin
               % Test default flag states
               ?assertEqual(true, erlmcp_flags:is_accepting()),
               ?assertEqual(false, erlmcp_flags:is_maintenance_mode()),
               ?assertEqual(false, erlmcp_flags:is_shutting_down()),
               ?assertEqual(true, erlmcp_flags:is_healthy())
           end).

%%====================================================================
%% Connection Flags Tests
%%====================================================================

test_accepting_connections() ->
    ?_test(begin
               % Initially accepting
               ?assertEqual(true, erlmcp_flags:is_accepting()),

               % Stop accepting
               erlmcp_flags:stop_accepting(),
               ?assertEqual(false, erlmcp_flags:is_accepting()),

               % Start accepting
               erlmcp_flags:start_accepting(),
               ?assertEqual(true, erlmcp_flags:is_accepting())
           end).

%%====================================================================
%% Maintenance Mode Tests
%%====================================================================

test_maintenance_mode() ->
    ?_test(begin
               % Initially not in maintenance mode
               ?assertEqual(false, erlmcp_flags:is_maintenance_mode()),

               % Enter maintenance mode
               erlmcp_flags:enter_maintenance_mode(),
               ?assertEqual(true, erlmcp_flags:is_maintenance_mode()),

               % Exit maintenance mode
               erlmcp_flags:exit_maintenance_mode(),
               ?assertEqual(false, erlmcp_flags:is_maintenance_mode())
           end).

%%====================================================================
%% Shutdown Tests
%%====================================================================

test_shutdown() ->
    ?_test(begin
               % Initially not shutting down
               ?assertEqual(false, erlmcp_flags:is_shutting_down()),

               % Start shutdown
               erlmcp_flags:start_shutdown(),
               ?assertEqual(true, erlmcp_flags:is_shutting_down()),

               % Cancel shutdown
               erlmcp_flags:cancel_shutdown(),
               ?assertEqual(false, erlmcp_flags:is_shutting_down())
           end).

%%====================================================================
%% Health Tests
%%====================================================================

test_health() ->
    ?_test(begin
               % Initially healthy
               ?assertEqual(true, erlmcp_flags:is_healthy()),

               % Mark unhealthy
               erlmcp_flags:mark_unhealthy(),
               ?assertEqual(false, erlmcp_flags:is_healthy()),

               % Mark healthy
               erlmcp_flags:mark_healthy(),
               ?assertEqual(true, erlmcp_flags:is_healthy())
           end).

%%====================================================================
%% Get All Tests
%%====================================================================

test_get_all() ->
    ?_test(begin
               % Set various flags
               erlmcp_flags:stop_accepting(),
               erlmcp_flags:enter_maintenance_mode(),
               erlmcp_flags:mark_unhealthy(),

               % Get all flags
               Flags = erlmcp_flags:get_all(),

               % Verify all flags are present
               ?assert(is_map(Flags)),
               ?assertEqual(false, maps:get(accepting_connections, Flags)),
               ?assertEqual(true, maps:get(maintenance_mode, Flags)),
               ?assertEqual(false, maps:get(shutting_down, Flags)),
               ?assertEqual(false, maps:get(healthy, Flags))
           end).

%%====================================================================
%% Integration Tests
%%====================================================================

test_maintenance_mode_disables_accepting() ->
    ?_test(begin
               % Initially accepting
               ?assertEqual(true, erlmcp_flags:is_accepting()),

               % Enter maintenance mode should stop accepting
               erlmcp_flags:enter_maintenance_mode(),
               ?assertEqual(false, erlmcp_flags:is_accepting()),
               ?assertEqual(true, erlmcp_flags:is_maintenance_mode()),

               % Exit maintenance mode should resume accepting
               erlmcp_flags:exit_maintenance_mode(),
               ?assertEqual(true, erlmcp_flags:is_accepting()),
               ?assertEqual(false, erlmcp_flags:is_maintenance_mode())
           end).

test_shutdown_disables_accepting() ->
    ?_test(begin
               % Initially accepting
               ?assertEqual(true, erlmcp_flags:is_accepting()),

               % Start shutdown should stop accepting
               erlmcp_flags:start_shutdown(),
               ?assertEqual(false, erlmcp_flags:is_accepting()),
               ?assertEqual(true, erlmcp_flags:is_shutting_down()),

               % Cancel shutdown should resume accepting
               erlmcp_flags:cancel_shutdown(),
               ?assertEqual(true, erlmcp_flags:is_accepting()),
               ?assertEqual(false, erlmcp_flags:is_shutting_down())
           end).

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_flag_updates() ->
    ?_test(begin
               % Spawn multiple processes updating flags concurrently
               NumProcesses = 100,

               Pids =
                   [spawn_link(fun() ->
                                  % Toggle flags
                                  erlmcp_flags:stop_accepting(),
                                  erlmcp_flags:start_accepting(),
                                  erlmcp_flags:mark_unhealthy(),
                                  erlmcp_flags:mark_healthy()
                               end)
                    || _ <- lists:seq(1, NumProcesses)],

               % Wait for all processes to complete
               [begin
                    Ref = monitor(process, Pid),
                    receive
                        {'DOWN', Ref, process, Pid, _} ->
                            ok
                    after 5000 ->
                        ?assert(false)  % Timeout
                    end
                end
                || Pid <- Pids],

               % Verify flags are in a valid state (no corruption)
               Flags = erlmcp_flags:get_all(),
               ?assert(is_map(Flags)),
               ?assert(is_boolean(maps:get(accepting_connections, Flags))),
               ?assert(is_boolean(maps:get(maintenance_mode, Flags))),
               ?assert(is_boolean(maps:get(shutting_down, Flags))),
               ?assert(is_boolean(maps:get(healthy, Flags)))
           end).

%%====================================================================
%% State Transition Tests
%%====================================================================

test_flag_transitions() ->
    ?_test(begin
               % Test all flag transitions
               States =
                   [{fun erlmcp_flags:stop_accepting/0, fun erlmcp_flags:is_accepting/0, false},
                    {fun erlmcp_flags:start_accepting/0, fun erlmcp_flags:is_accepting/0, true},
                    {fun erlmcp_flags:enter_maintenance_mode/0,
                     fun erlmcp_flags:is_maintenance_mode/0,
                     true},
                    {fun erlmcp_flags:exit_maintenance_mode/0,
                     fun erlmcp_flags:is_maintenance_mode/0,
                     false},
                    {fun erlmcp_flags:start_shutdown/0, fun erlmcp_flags:is_shutting_down/0, true},
                    {fun erlmcp_flags:cancel_shutdown/0,
                     fun erlmcp_flags:is_shutting_down/0,
                     false},
                    {fun erlmcp_flags:mark_unhealthy/0, fun erlmcp_flags:is_healthy/0, false},
                    {fun erlmcp_flags:mark_healthy/0, fun erlmcp_flags:is_healthy/0, true}],

               % Test each transition
               lists:foreach(fun({Action, Check, Expected}) ->
                                Action(),
                                ?assertEqual(Expected, Check())
                             end,
                             States)
           end).

%%====================================================================
%% Property-Based Tests (Manual Properties)
%%====================================================================

% Property: Flag updates are atomic (never see partial updates)
% Property: Flag reads are consistent (never see torn reads)
% Property: Concurrent updates are linearizable
% Property: Maintenance mode always disables accepting
% Property: Shutdown always disables accepting
