%%%====================================================================
%%% @doc Supervisor Auto-Hibernation Tests (OTP 28)
%%%
%%% Tests for OTP 28 supervisor auto-hibernation feature in erlmcp.
%%%
%%% @end
%%%====================================================================
-module(erlmcp_sup_hibernation_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc Test that erlmcp_sup exports hibernate_after/0 callback
hibernate_after_callback_exists_test() ->
    ?assertEqual({hibernate_after, 0}, erlmcp_sup:module_info(exports)),
    ?assert(is_function(erlmcp:hibernate_after, 0)).

%% @doc Test that hibernate_after/0 returns correct timeout
hibernate_after_returns_1000ms_test() ->
    ?assertEqual(1000, erlmcp_sup:hibernate_after()).

%% @doc Test that erlmcp_sup supervisor flags include auto_hibernation
supervisor_flags_include_auto_hibernation_test() ->
    {ok, {SupFlags, _ChildSpecs}} = erlmcp_sup:init([]),
    ?assert(maps:is_key(auto_hibernation, SupFlags)),
    ?assertEqual(erlmcp_sup, maps:get(auto_hibernation, SupFlags)).

%% @doc Test that dynamic supervisors disable auto-hibernation
dynamic_supervisor_disables_hibernation_test() ->
    %% Test erlmcp_server_sup
    {ok, {SupFlags, _ChildSpecs}} = erlmcp_server_sup:init([]),
    ?assert(maps:is_key(auto_hibernation, SupFlags)),
    ?assertEqual(false, maps:get(auto_hibernation, SupFlags)),

    %% Test erlmcp_chaos_worker_sup
    {ok, {SupFlags2, _ChildSpecs2}} = erlmcp_chaos_worker_sup:init([]),
    ?assert(maps:is_key(auto_hibernation, SupFlags2)),
    ?assertEqual(false, maps:get(auto_hibernation, SupFlags2)).

%% @doc Test that supervisor remains functional after configuration
supervisor_functional_after_config_test() ->
    %% This test verifies that adding auto_hibernation doesn't break supervisor
    %% We can't easily test actual hibernation in unit tests, but we can
    %% verify the configuration is valid

    %% Test static supervisor init
    {ok, {SupFlags1, ChildSpecs1}} = erlmcp_sup:init([]),
    ?assertEqual(one_for_one, maps:get(strategy, SupFlags1)),
    ?assertEqual(5, maps:get(intensity, SupFlags1)),
    ?assertEqual(60, maps:get(period, SupFlags1)),
    ?assertEqual(erlmcp_sup, maps:get(auto_hibernation, SupFlags1)),
    ?assert(length(ChildSpecs1) > 0),

    %% Test dynamic supervisor init
    {ok, {SupFlags2, ChildSpecs2}} = erlmcp_server_sup:init([]),
    ?assertEqual(simple_one_for_one, maps:get(strategy, SupFlags2)),
    ?assertEqual(5, maps:get(intensity, SupFlags2)),
    ?assertEqual(60, maps:get(period, SupFlags2)),
    ?assertEqual(false, maps:get(auto_hibernation, SupFlags2)),
    ?assert(length(ChildSpecs2) > 0).

%% @doc Test that hibernation timeout is non-negative
hibernate_after_non_negative_test() ->
    Timeout = erlmcp_sup:hibernate_after(),
    ?assert(Timeout >= 0),
    ?assert(is_integer(Timeout)).

%% @doc Test that hibernation timeout is reasonable (between 100ms and 60s)
hibernate_after_reasonable_range_test() ->
    Timeout = erlmcp_sup:hibernate_after(),
    ?assert(Timeout >= 100),
    ?assert(Timeout =< 60000).

%%%====================================================================
%%% Integration Tests (require started application)
%%%====================================================================

%% @doc Test that supervisor starts with auto-hibernation enabled
supervisor_starts_with_hibernation_test_() ->
    {setup,
     fun setup_application/0,
     fun cleanup_application/1,
     fun(_SetupData) ->
         [
          ?_test(begin
                     {ok, Pid} = erlmcp_sup:start_link(),
                     ?assert(is_pid(Pid)),
                     ?assert(is_process_alive(Pid)),

                     %% Verify supervisor is registered
                     ?assertEqual(Pid, whereis(erlmcp_sup)),

                     %% Clean shutdown
                     ok = supervisor:stop(Pid)
                 end)
         ]
     end}.

%% @doc Test dynamic supervisor starts without auto-hibernation
dynamic_supervisor_starts_without_hibernation_test_() ->
    {setup,
     fun setup_application/0,
     fun cleanup_application/1,
     fun(_SetupData) ->
         [
          ?_test(begin
                     {ok, Pid} = erlmcp_server_sup:start_link(),
                     ?assert(is_pid(Pid)),
                     ?assert(is_process_alive(Pid)),

                     %% Verify supervisor is registered
                     ?assertEqual(Pid, whereis(erlmcp_server_sup)),

                     %% Clean shutdown
                     ok = supervisor:stop(Pid)
                 end)
         ]
     end}.

%% @doc Test that children can be started under hibernating supervisor
children_start_under_hibernating_supervisor_test_() ->
    {setup,
     fun setup_application/0,
     fun cleanup_application/1,
     fun(_SetupData) ->
         [
          ?_test(begin
                     {ok, SupPid} = erlmcp_sup:start_link(),

                     %% Start a child (e.g., erlmcp_core_sup)
                     {ok, ChildPid} = supervisor:start_child(
                                        erlmcp_sup,
                                        #{id => test_child,
                                          start => {erlmcp_core_sup, start_link, []},
                                          restart => permanent,
                                          type => supervisor,
                                          modules => [erlmcp_core_sup]}
                                       ),

                     ?assert(is_pid(ChildPid)),
                     ?assert(is_process_alive(ChildPid)),

                     %% Clean shutdown
                     ok = supervisor:terminate_child(erlmcp_sup, test_child),
                     ok = supervisor:stop(SupPid)
                 end)
         ]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup_application() ->
    %% Start required applications
    application:ensure_all_started(erlmcp),
    ok.

cleanup_application(_SetupData) ->
    %% Stop applications
    application:stop(erlmcp),
    ok.
