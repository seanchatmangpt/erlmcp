-module(erlmcp_otp28_supervisor_enhancements_tests).
-author("erlmcp").

%% OTP 28.3.1 Supervisor Enhancement Tests - Chicago School TDD
%% Tests validate supervisor improvements and restart tracking

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    {ok, Apps} = application:ensure_all_started(erlmcp_core),
    {ok, Pid} = erlmcp_otp28_supervisor_enhancements:start_link(),
    {Apps, Pid}.

cleanup({Apps, _Pid}) ->
    lists:foreach(fun(App) -> application:stop(App) end, lists:reverse(Apps)),
    ok.

%%%===================================================================
%%% Child Restart with Tracking Tests
%%%===================================================================

restart_child_with_tracking_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_, _}) ->
         [
          ?_test(begin
                     %% Test: Restart child with tracking
                     {ok, SupPid} = start_dynamic_supervisor(),

                     %% Start a child
                     {ok, ChildPid} = supervisor:start_child(SupPid,
                         #{id => test_child,
                           start => {erlmcp_test_helpers, start_worker, [test_child]}}),

                     %% Restart with tracking
                     Result = erlmcp_otp28_supervisor_enhancements:
                         restart_child_with_tracking(test_child, SupPid),

                     ?assertMatch({ok, _NewPid}, Result),

                     %% Verify restart was tracked
                     timer:sleep(100),
                     History = erlmcp_otp28_supervisor_enhancements:child_restart_history(),
                     ?assert(maps:is_key(test_child, History)),
                     ?assert(length(maps:get(test_child, History)) > 0),

                     %% Cleanup
                     supervisor:stop(SupPid)
                 end),
          ?_test(begin
                     %% Test: Restart non-existent child
                     {ok, SupPid} = start_dynamic_supervisor(),

                     Result = erlmcp_otp28_supervisor_enhancements:
                         restart_child_with_tracking(nonexistent, SupPid),

                     ?assertMatch({error, _}, Result),

                     supervisor:stop(SupPid)
                 end)
         ]
     end}.

%%%===================================================================
%%% Bulk Restart Tests
%%%===================================================================

bulk_restart_children_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_, _}) ->
         [
          ?_test(begin
                     %% Test: Bulk restart multiple children
                     {ok, SupPid} = start_dynamic_supervisor(),

                     %% Start multiple children
                     ChildIds = [child1, child2, child3],
                     lists:foreach(fun(Id) ->
                         {ok, _Pid} = supervisor:start_child(SupPid,
                             #{id => Id,
                               start => {erlmcp_test_helpers, start_worker, [Id]})
                     end, ChildIds),

                     %% Bulk restart
                     {ok, Results} = erlmcp_otp28_supervisor_enhancements:
                         bulk_restart_children(ChildIds, SupPid),

                     ?assertEqual(length(ChildIds), length(Results)),

                     %% Verify all succeeded
                     SuccessCount = lists:foldl(fun({_Id, Result}, Acc) ->
                         case Result of
                             {ok, _} -> Acc + 1;
                             _ -> Acc
                         end
                     end, 0, Results),
                     ?assertEqual(length(ChildIds), SuccessCount),

                     %% Cleanup
                     supervisor:stop(SupPid)
                 end)
         ]
     end}.

%%%===================================================================
%%% Process Monitoring Tests
%%%===================================================================

monitor_child_processes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_, _}) ->
         [
          ?_test(begin
                     %% Test: Monitor all child processes
                     {ok, SupPid} = start_dynamic_supervisor(),

                     %% Start children
                     {ok, _Child1} = supervisor:start_child(SupPid,
                         #{id => child1,
                           start => {erlmcp_test_helpers, start_worker, [child1]}}),
                     {ok, _Child2} = supervisor:start_child(SupPid,
                         #{id => child2,
                           start => {erlmcp_test_helpers, start_worker, [child2]}}),

                     %% Monitor children
                     {ok, Refs} = erlmcp_otp28_supervisor_enhancements:
                         monitor_child_processes(SupPid),

                     ?assert(is_list(Refs)),
                     ?assert(length(Refs) > 0),
                     ?assert(lists:all(fun is_reference/1, Refs)),

                     %% Cleanup
                     supervisor:stop(SupPid)
                 end)
         ]
     end}.

%%%===================================================================
%%% Child Process Info Tests
%%%===================================================================

get_child_process_info_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_, _}) ->
         [
          ?_test(begin
                     %% Test: Get detailed process info
                     {ok, SupPid} = start_dynamic_supervisor(),

                     %% Start child
                     {ok, ChildPid} = supervisor:start_child(SupPid,
                         #{id => test_child,
                           start => {erlmcp_test_helpers, start_worker, [test_child]}}),

                     %% Get info
                     Result = erlmcp_otp28_supervisor_enhancements:
                         get_child_process_info(SupPid, test_child),

                     ?assertMatch({ok, Info}, Result),
                     {ok, Info} = Result,

                     ?assertEqual(test_child, maps:get(id, Info)),
                     ?assertEqual(ChildPid, maps:get(pid, Info)),

                     %% Verify process info fields
                     InfoMap = maps:get(info, Info),
                     ?assert(maps:is_key(memory, InfoMap)),
                     ?assert(maps:is_key(reductions, InfoMap)),

                     %% Cleanup
                     supervisor:stop(SupPid)
                 end),
          ?_test(begin
                     %% Test: Get info for non-existent child
                     {ok, SupPid} = start_dynamic_supervisor(),

                     Result = erlmcp_otp28_supervisor_enhancements:
                         get_child_process_info(SupPid, nonexistent),

                     ?assertMatch({error, not_found}, Result),

                     supervisor:stop(SupPid)
                 end)
         ]
     end}.

%%%===================================================================
%%% Hibernation Configuration Tests
%%%===================================================================

configure_hibernation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_, _}) ->
         [
          ?_test(begin
                     %% Test: Configure hibernation
                     {ok, SupPid} = start_dynamic_supervisor(),

                     Result = erlmcp_otp28_supervisor_enhancements:
                         configure_hibernation(SupPid, 5000),

                     ?assertEqual(ok, Result),

                     %% Get configuration
                     Config = erlmcp_otp28_supervisor_enhancements:
                         get_hibernation_config(SupPid),

                     ?assert(is_map(Config)),
                     ?assert(maps:is_key(enabled, Config)),
                     ?assert(maps:is_key(after_ms, Config)),

                     supervisor:stop(SupPid)
                 end),
          ?_test(begin
                     %% Test: Disable hibernation
                     {ok, SupPid} = start_dynamic_supervisor(),

                     Result = erlmcp_otp28_supervisor_enhancements:
                         configure_hibernation(SupPid, disabled),

                     ?assertEqual(ok, Result),

                     supervisor:stop(SupPid)
                 end)
         ]
     end}.

%%%===================================================================
%%% Supervisor Health Tests
%%%===================================================================

supervisor_detailed_health_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_, _}) ->
         [
          ?_test(begin
                     %% Test: Get detailed health for healthy supervisor
                     {ok, SupPid} = start_dynamic_supervisor(),

                     %% Start children
                     lists:foreach(fun(Id) ->
                         supervisor:start_child(SupPid,
                             #{id => Id,
                               start => {erlmcp_test_helpers, start_worker, [Id]}})
                     end, [child1, child2, child3]),

                     Health = erlmcp_otp28_supervisor_enhancements:
                         supervisor_detailed_health(SupPid),

                     ?assertEqual(healthy, maps:get(health, Health)),
                     ?assertEqual(3, maps:get(total_children, Health)),
                     ?assertEqual(3, maps:get(active_children, Health)),
                     ?assertEqual(0, maps:get(dead_children, Health)),

                     supervisor:stop(SupPid)
                 end),
          ?_test(begin
                     %% Test: Get health for supervisor with dead children
                     {ok, SupPid} = start_dynamic_supervisor(),

                     %% Start and kill child
                     {ok, ChildPid} = supervisor:start_child(SupPid,
                         #{id => dying_child,
                           start => {erlmcp_test_helpers, start_worker, [dying_child]}}),
                     exit(ChildPid, kill),
                     timer:sleep(100),

                     Health = erlmcp_otp28_supervisor_enhancements:
                         supervisor_detailed_health(SupPid),

                     ?assertEqual(degraded, maps:get(health, Health)),
                     ?assert(maps:get(dead_children, Health) > 0),

                     supervisor:stop(SupPid)
                 end)
         ]
     end}.

%%%===================================================================
%%% Performance Metrics Tests
%%%===================================================================

get_performance_metrics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_, _}) ->
         [
          ?_test(begin
                     %% Test: Get performance metrics
                     {ok, SupPid} = start_dynamic_supervisor(),

                     %% Start children
                     lists:foreach(fun(Id) ->
                         supervisor:start_child(SupPid,
                             #{id => Id,
                               start => {erlmcp_test_helpers, start_worker, [Id]}})
                     end, lists:seq(1, 5)),

                     {ok, Metrics} = erlmcp_otp28_supervisor_enhancements:
                         get_performance_metrics(SupPid),

                     ?assert(is_map(Metrics)),
                     ?assertEqual(5, maps:get(total_children, Metrics)),
                     ?assert(maps:get(active_children, Metrics) >= 0),

                     supervisor:stop(SupPid)
                 end)
         ]
     end}.

%%%===================================================================
%%% Restart Statistics Tests
%%%===================================================================

get_restart_statistics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_, _}) ->
         [
          ?_test(begin
                     %% Test: Get restart statistics
                     Stats = erlmcp_otp28_supervisor_enhancements:get_restart_statistics(),

                     ?assert(is_map(Stats)),
                     ?assert(maps:is_key(total_restarts, Stats)),
                     ?assert(is_integer(maps:get(total_restarts, Stats)))
                 end)
         ]
     end}.

%%%===================================================================
%%% Property-Based Tests
%%%===================================================================

%% Property: Restart tracking preserves child IDs
prop_restart_tracking_preserves_ids() ->
    ?FORALL(ChildId, binary(),
        begin
            {ok, SupPid} = start_dynamic_supervisor(),
            supervisor:start_child(SupPid,
                #{id => ChildId,
                  start => {erlmcp_test_helpers, start_worker, [ChildId]}}),

            erlmcp_otp28_supervisor_enhancements:
                restart_child_with_tracking(ChildId, SupPid),

            timer:sleep(100),
            History = erlmcp_otp28_supervisor_enhancements:child_restart_history(),

            supervisor:stop(SupPid),

            maps:is_key(ChildId, History)
        end).

%% Property: Health metrics are non-negative
prop_health_metrics_non_negative() ->
    ?FORALL(_Dummy, nat(),
        begin
            {ok, SupPid} = start_dynamic_supervisor(),
            Health = erlmcp_otp28_supervisor_enhancements:supervisor_detailed_health(SupPid),
            supervisor:stop(SupPid),

            maps:get(total_children, Health, 0) >= 0 andalso
            maps:get(active_children, Health, 0) >= 0 andalso
            maps:get(dead_children, Health, 0) >= 0 andalso
            maps:get(avg_memory, Health, 0) >= 0 andalso
            maps:get(avg_reductions, Health, 0) >= 0
        end).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

integration_supervisor_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_, _}) ->
         [
          ?_test(begin
                     %% Test: Full lifecycle with all enhancements
                     {ok, SupPid} = start_dynamic_supervisor(),

                     %% Start children
                     ChildIds = [child1, child2, child3],
                     lists:foreach(fun(Id) ->
                         supervisor:start_child(SupPid,
                             #{id => Id,
                               start => {erlmcp_test_helpers, start_worker, [Id]})
                     end, ChildIds),

                     %% Monitor all
                     {ok, _Refs} = erlmcp_otp28_supervisor_enhancements:
                         monitor_child_processes(SupPid),

                     %% Get health
                     Health = erlmcp_otp28_supervisor_enhancements:
                         supervisor_detailed_health(SupPid),
                     ?assertEqual(healthy, maps:get(health, Health)),

                     %% Restart with tracking
                     {ok, _} = erlmcp_otp28_supervisor_enhancements:
                         restart_child_with_tracking(child1, SupPid),

                     %% Get history
                     History = erlmcp_otp28_supervisor_enhancements:child_restart_history(),
                     ?assert(maps:is_key(child1, History)),

                     %% Get metrics
                     {ok, Metrics} = erlmcp_otp28_supervisor_enhancements:
                         get_performance_metrics(SupPid),
                     ?assertEqual(3, maps:get(total_children, Metrics)),

                     %% Cleanup
                     supervisor:stop(SupPid)
                 end)
         ]
     end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Start a dynamic supervisor for testing
start_dynamic_supervisor() ->
    supervisor:start_link({local, test_dynamic_sup},
                         erlmcp_test_dynamic_sup, []).

%%%===================================================================
%%% Test Dynamic Supervisor Module
%%%===================================================================

-module(erlmcp_test_dynamic_sup).
-behaviour(supervisor).

-export([init/1]).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
