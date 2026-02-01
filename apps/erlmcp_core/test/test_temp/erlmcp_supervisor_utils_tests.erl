%%%-------------------------------------------------------------------
%%% @doc Supervisor Utils Tests
%%% @end
%%% Tests for erlmcp_supervisor_utils - supervisor introspection utilities
%%%-------------------------------------------------------------------
-module(erlmcp_supervisor_utils_tests).

-author(erlmcp).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures - Simple Test Supervisor
%%%===================================================================

%% Simple test supervisor for testing
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 5,
          period => 60},
    {ok, {SupFlags, []}}.

%% Setup function - start test supervisor
setup() ->
    %% Stop any existing test supervisor first
    _ = case whereis(?MODULE) of
            undefined ->
                ok;
            OldPid ->
                try gen_server:stop(OldPid) of
                    _ ->
                        ok
                catch
                    _:_ ->
                        exit(OldPid, kill)
                end,
                timer:sleep(100)
        end,
    {ok, Pid} = start_link(),
    Pid.

%% Cleanup function - stop supervisor
cleanup(_Pid) ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        SupPid ->
            try gen_server:stop(SupPid) of
                _ ->
                    ok
            catch
                _:_ ->
                    exit(SupPid, kill)
            end
    end,
    timer:sleep(100).

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test get_children_status/1 with empty supervisor
get_children_status_empty_test() ->
    Pid = setup(),

    %% Get children status
    Children = erlmcp_supervisor_utils:get_children_status(?MODULE),

    %% Should return empty list for supervisor with no children
    ?assertEqual([], Children),

    cleanup(Pid).

%% Test get_supervision_tree/1 with empty supervisor
get_supervision_tree_empty_test() ->
    Pid = setup(),

    %% Get supervision tree
    Tree = erlmcp_supervisor_utils:get_supervision_tree(?MODULE),

    %% Should return a map with required fields
    ?assert(is_map(Tree)),
    ?assertEqual(?MODULE, maps:get(supervisor, Tree)),
    ?assertEqual(Pid, maps:get(pid, Tree)),
    ?assert(maps:is_key(health_score, Tree)),
    ?assertEqual([], maps:get(children, Tree)),

    %% Empty supervisor should have perfect health score
    ?assertEqual(1.0, maps:get(health_score, Tree)),

    cleanup(Pid).

%% Test get_supervision_tree_flat/1 with empty supervisor
get_supervision_tree_flat_empty_test() ->
    Pid = setup(),

    %% Get flat tree
    Flat = erlmcp_supervisor_utils:get_supervision_tree_flat(?MODULE),

    %% Should return list with supervisor itself
    ?assert(is_list(Flat)),
    ?assertEqual(1, length(Flat)),
    [#{pid := Pid, supervisor := ?MODULE}] = Flat,

    cleanup(Pid).

%% Test count_processes/1 with empty supervisor
count_processes_empty_test() ->
    Pid = setup(),

    %% Count processes
    Count = erlmcp_supervisor_utils:count_processes(?MODULE),

    %% Should be 0 for supervisor with no children
    ?assertEqual(0, Count),

    cleanup(Pid).

%% Test calculate_health_score/1 with empty supervisor
calculate_health_score_empty_test() ->
    Pid = setup(),

    %% Calculate health score
    Score = erlmcp_supervisor_utils:calculate_health_score(?MODULE),

    %% Empty supervisor should be perfectly healthy
    ?assertEqual(1.0, Score),

    cleanup(Pid).

%% Test export_to_json/1 with empty supervisor
export_to_json_test() ->
    Pid = setup(),

    %% Export to JSON
    Json = erlmcp_supervisor_utils:export_to_json(?MODULE),

    %% Should return a binary
    ?assert(is_binary(Json)),

    %% Should be valid JSON
    Decoded = jsx:decode(Json, [return_maps]),
    ?assert(is_map(Decoded)),
    ?assert(maps:is_key(<<"tree">>, Decoded)),
    ?assert(maps:is_key(<<"metrics">>, Decoded)),
    ?assert(maps:is_key(<<"timestamp">>, Decoded)),

    cleanup(Pid).

%% Test export_to_json_pretty/1
export_to_json_pretty_test() ->
    Pid = setup(),

    %% Export to pretty JSON
    Json = erlmcp_supervisor_utils:export_to_json_pretty(?MODULE),

    %% Should return a binary
    ?assert(is_binary(Json)),

    %% Should be valid JSON
    Decoded = jsx:decode(Json, [return_maps]),
    ?assert(is_map(Decoded)),

    %% Pretty JSON should be larger (have whitespace)
    Compact = jsx:encode(Decoded),
    ?assert(byte_size(Json) >= byte_size(Compact)),

    cleanup(Pid).

%% Test get_process_metrics/1 with valid PID
get_process_metrics_valid_test() ->
    Pid = setup(),

    %% Get metrics for the supervisor itself
    Metrics = erlmcp_supervisor_utils:get_process_metrics(Pid),

    %% Should return a map
    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(memory_bytes, Metrics)),
    ?assert(maps:is_key(message_queue_len, Metrics)),
    ?assert(maps:is_key(reductions, Metrics)),
    ?assert(maps:is_key(status, Metrics)),

    %% Metrics should be non-negative
    ?assert(maps:get(memory_bytes, Metrics) >= 0),
    ?assert(maps:get(message_queue_len, Metrics) >= 0),
    ?assert(maps:get(reductions, Metrics) >= 0),

    cleanup(Pid).

%% Test get_process_metrics/1 with invalid input
get_process_metrics_invalid_test() ->
    %% Test with non-PID input
    ?assertEqual({error, not_pid}, erlmcp_supervisor_utils:get_process_metrics(not_a_pid)),
    ?assertEqual({error, not_pid}, erlmcp_supervisor_utils:get_process_metrics(123)),
    ?assertEqual({error, not_pid}, erlmcp_supervisor_utils:get_process_metrics("pid")),

    %% Test with dead process
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(100), % Ensure process exits
    ?assertEqual({error, dead}, erlmcp_supervisor_utils:get_process_metrics(DeadPid)).

%% Test get_tree_metrics/1 with empty supervisor
get_tree_metrics_test() ->
    Pid = setup(),

    %% Get tree metrics
    Metrics = erlmcp_supervisor_utils:get_tree_metrics(?MODULE),

    %% Should return a map with all required fields
    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(total_supervisors, Metrics)),
    ?assert(maps:is_key(total_workers, Metrics)),
    ?assert(maps:is_key(total_processes, Metrics)),
    ?assert(maps:is_key(total_memory_bytes, Metrics)),
    ?assert(maps:is_key(max_depth, Metrics)),
    ?assert(maps:is_key(health_score, Metrics)),
    ?assert(maps:is_key(unhealthy_count, Metrics)),

    %% Empty supervisor should have 0 children but 1 supervisor (itself)
    ?assertEqual(1, maps:get(total_supervisors, Metrics)),
    ?assertEqual(0, maps:get(total_workers, Metrics)),
    ?assertEqual(1, maps:get(total_processes, Metrics)),
    ?assertEqual(0, maps:get(max_depth, Metrics)),
    ?assertEqual(0, maps:get(unhealthy_count, Metrics)),

    cleanup(Pid).

%% Test find_unhealthy_processes/1 with empty supervisor
find_unhealthy_processes_test() ->
    Pid = setup(),

    %% Find unhealthy processes
    Unhealthy = erlmcp_supervisor_utils:find_unhealthy_processes(?MODULE),

    %% Should return empty list for healthy empty supervisor
    ?assertEqual([], Unhealthy),

    cleanup(Pid).

%% Test get_restart_statistics/1
get_restart_statistics_test() ->
    Pid = setup(),

    %% Get restart statistics - should not crash
    Stats = erlmcp_supervisor_utils:get_restart_statistics(?MODULE),

    %% supervisor:count_children returns a proplist, or error map on exception
    ?assert(is_list(Stats) orelse is_map(Stats)),

    cleanup(Pid).

%% Test validate_supervision_tree/1 - healthy empty supervisor
validate_supervision_tree_ok_test() ->
    Pid = setup(),

    %% Validate tree
    Result = erlmcp_supervisor_utils:validate_supervision_tree(?MODULE),

    %% Should pass validation
    ?assertMatch({ok, #{valid := true, checks := _}}, Result),

    case Result of
        {ok, #{valid := true, checks := Checks}} ->
            %% Should have performed checks
            ?assert(length(Checks) > 0)
    end,

    cleanup(Pid).

%% Test validate_supervision_tree/1 - with invalid supervisor
validate_supervision_tree_error_test() ->
    %% Try to validate a non-existent supervisor
    Result = erlmcp_supervisor_utils:validate_supervision_tree(nonexistent_sup_12345),

    %% Should return error
    ?assertMatch({error, #{valid := false, violations := _}}, Result),

    case Result of
        {error, #{valid := false, violations := Violations}} ->
            %% Should have at least one violation
            ?assert(length(Violations) > 0),
            %% Each violation should have check and reason
            lists:foreach(fun(V) ->
                             ?assert(maps:is_key(check, V)),
                             ?assert(maps:is_key(reason, V))
                          end,
                          Violations)
    end.

%% Test health score calculation with empty supervisor
health_score_empty_supervisor_test() ->
    Pid = setup(),

    %% Get health score
    Score = erlmcp_supervisor_utils:calculate_health_score(?MODULE),

    %% Empty supervisor should be perfectly healthy
    ?assertEqual(1.0, Score),

    cleanup(Pid).

%% Test error handling with invalid supervisor reference
invalid_supervisor_ref_test() ->
    %% Test with non-existent supervisor
    ?assertEqual([], erlmcp_supervisor_utils:get_children_status(nonexistent_sup_12345)),

    %% Test get_supervision_tree with invalid ref
    Tree = erlmcp_supervisor_utils:get_supervision_tree(nonexistent_sup_12345),
    ?assert(is_map(Tree)),
    ?assertEqual(undefined, maps:get(pid, Tree)),
    ?assertEqual(0.0, maps:get(health_score, Tree)),

    %% Test count_processes with invalid ref
    Count = erlmcp_supervisor_utils:count_processes(nonexistent_sup_12345),
    ?assert(is_integer(Count)),
    ?assertEqual(0, Count).

%%%===================================================================
%%% Test Generator
%%%===================================================================

supervisor_utils_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun get_children_status_empty_test/0,
      fun get_supervision_tree_empty_test/0,
      fun get_supervision_tree_flat_empty_test/0,
      fun count_processes_empty_test/0,
      fun calculate_health_score_empty_test/0,
      fun export_to_json_test/0,
      fun export_to_json_pretty_test/0,
      fun get_process_metrics_valid_test/0,
      fun get_tree_metrics_test/0,
      fun find_unhealthy_processes_test/0,
      fun get_restart_statistics_test/0,
      fun validate_supervision_tree_ok_test/0,
      fun health_score_empty_supervisor_test/0,
      fun invalid_supervisor_ref_test/0]}.

%% Standalone tests that don't need fixtures
get_process_metrics_standalone_test() ->
    %% Test with current process
    Self = self(),
    Metrics = erlmcp_supervisor_utils:get_process_metrics(Self),

    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(status, Metrics)),
    ?assert(maps:is_key(memory_bytes, Metrics)),
    ?assert(maps:is_key(message_queue_len, Metrics)),

    %% Should be alive (not error tuple)
    ?assertNotMatch({error, _}, Metrics).
