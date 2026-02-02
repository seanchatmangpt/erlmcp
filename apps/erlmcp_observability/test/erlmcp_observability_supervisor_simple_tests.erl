%%%-------------------------------------------------------------------
%%% @doc Simplified unit tests for erlmcp_observability_supervisor
%%%
%%% Chicago School TDD: Black-box testing of observable behavior
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_observability_supervisor_simple_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

supervisor_start_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Supervisor starts with default options", fun test_start_default/0},
      {"Supervisor starts with custom intensity", fun test_start_intensity/0},
      {"Supervisor is registered", fun test_registered/0},
      {"Supervisor has children", fun test_children/0}
     ]}.

setup() ->
    application:ensure_all_started(telemetry),
    catch unregister(erlmcp_observability_sup),

    %% Start with minimal options to avoid missing dependencies
    {ok, Pid} = erlmcp_observability_sup:start_link([{intensity, 5}, {period, 30}]),
    Pid.

cleanup(Pid) ->
    exit(Pid, normal),
    timer:sleep(100),
    catch unregister(erlmcp_observability_sup).

%%====================================================================
%% Tests
%%====================================================================

test_start_default() ->
    %% Test fresh start
    catch unregister(erlmcp_observability_sup_test1),
    {ok, Pid} = erlmcp_observability_sup:start_link([], erlmcp_observability_sup_test1),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    exit(Pid, normal).

test_start_intensity() ->
    %% Test custom intensity
    catch unregister(erlmcp_observability_sup_test2),
    Opts = [{intensity, 3}, {period, 60}],
    {ok, Pid} = erlmcp_observability_sup:start_link(Opts, erlmcp_observability_sup_test2),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    exit(Pid, normal).

test_registered() ->
    %% Check supervisor is registered
    Pid = whereis(erlmcp_observability_sup),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)).

test_children() ->
    %% Check that children are defined (even if they fail to start)
    Children = supervisor:which_children(erlmcp_observability_sup),
    ?assert(length(Children) > 0),

    %% Check for known child IDs
    ChildIds = [Id || {Id, _Pid, _Type, _Modules} <- Children],
    ?assert(lists:member(erlmcp_event_manager, ChildIds)).

%%====================================================================
%% Unit Tests (No setup/teardown)
%%====================================================================

child_spec_format_test() ->
    %% Test that child spec helper works
    Spec = erlmcp_observability_sup:child_spec(test_worker, worker, permanent, 5000, [test_worker]),
    ?assertEqual(test_worker, maps:get(id, Spec)),
    ?assertEqual(worker, maps:get(type, Spec)),
    ?assertEqual(permanent, maps:get(restart, Spec)),
    ?assertEqual(5000, maps:get(shutdown, Spec)).

supervisor_flags_test() ->
    %% Test supervisor init with options
    Opts = [{intensity, 7}, {period, 45}],
    {ok, {Flags, _Children}} = erlmcp_observability_sup:init(Opts),
    ?assertEqual(one_for_one, maps:get(strategy, Flags)),
    ?assertEqual(7, maps:get(intensity, Flags)),
    ?assertEqual(45, maps:get(period, Flags)).
