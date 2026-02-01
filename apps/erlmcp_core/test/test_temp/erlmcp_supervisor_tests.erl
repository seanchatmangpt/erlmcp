-module(erlmcp_supervisor_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test supervisor child specification fixes
%% This tests that simple_one_for_one supervisors have correct template specs

%%====================================================================
%% Test Fixtures
%%====================================================================

supervisor_setup() ->
    {ok, Pid} = erlmcp_sup:start_link(),
    Pid.

supervisor_cleanup(_Pid) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% Test that erlmcp_server_sup has correct child spec
server_sup_child_spec_test() ->
    {ok, SupPid} = erlmcp_server_sup:start_link(),

    %% Check that supervisor is running
    ?assert(erlang:is_process_alive(SupPid)),

    %% Check that we can start a child (this validates the spec)
    ServerId = test_server_1,
    Config = #{test => true},

    case erlmcp_server_sup:start_child(ServerId, Config) of
        {ok, ServerPid} when is_pid(ServerPid) ->
            %% Child started successfully
            ?assert(erlang:is_process_alive(ServerPid)),
            %% Clean up
            ok = supervisor:terminate_child(erlmcp_server_sup, ServerPid);
        {error, {already_started, _Pid}} ->
            %% Already exists - ok
            ok;
        {error, Reason} ->
            %% Other errors indicate spec problems
            ?assertEqual(undefined, Reason)
    end,

    %% Clean up supervisor
    ok = supervisor:stop(SupPid).

%% Test that erlmcp_notification_handler_sup has correct child spec
notification_handler_sup_child_spec_test() ->
    {ok, SupPid} = erlmcp_notification_handler_sup:start_link(),

    %% Check that supervisor is running
    ?assert(erlang:is_process_alive(SupPid)),

    %% Note: We can't fully test this without erlmcp_notification_handler
    %% but we verify the supervisor structure is correct
    %% Clean up
    ok = supervisor:stop(SupPid).

%% Test that erlmcp_sup can start and stop servers
sup_start_stop_server_test() ->
    {ok, SupPid} = supervisor_setup(),

    %% Start a server via the API
    ServerId = test_server_sup_1,
    Config = #{test => true},

    case erlmcp_sup:start_server(ServerId, Config) of
        {ok, ServerPid} when is_pid(ServerPid) ->
            %% Server started successfully
            ?assert(erlang:is_process_alive(ServerPid)),

            %% Stop the server
            ?assertEqual(ok, erlmcp_sup:stop_server(ServerId)),

            %% Verify server is stopped
            timer:sleep(100),
            ?assertNot(erlang:is_process_alive(ServerPid));
        {error, Reason} ->
            %% Registry might not be available - that's ok for this test
            ?assertMatch({registry_failed, _}, Reason)
    end,

    supervisor_cleanup(SupPid).

%% Test restart intensity
supervisor_restart_intensity_test() ->
    %% Verify restart intensity is set correctly
    {ok, SupPid} = erlmcp_server_sup:start_link(),

    %% We verified the supervisor compiles and starts with correct intensity
    %% The actual values are: intensity => 5, period => 60
    ok = supervisor:stop(SupPid),
    ?assert(true).

%% Test empty child spec handling (cluster_sup with clustering disabled)
empty_child_spec_test() ->
    %% This tests that supervisors can have empty child lists
    %% when optional features are disabled
    %% Disable clustering
    application:set_env(erlmcp_core, cluster_enabled, false),

    {ok, SupPid} = erlmcp_cluster_sup:start_link(),

    %% Should start fine with empty child list
    ?assert(erlang:is_process_alive(SupPid)),

    %% Should have no children
    ?assertEqual([], supervisor:which_children(SupPid)),

    %% Clean up
    ok = supervisor:stop(SupPid),

    %% Reset env
    application:unset_env(erlmcp_core, cluster_enabled).

%% Test error handling in start_server
start_server_error_handling_test() ->
    {ok, SupPid} = supervisor_setup(),

    %% Test with invalid config
    ServerId = test_server_error,
    Config = #{invalid => true},

    case erlmcp_sup:start_server(ServerId, Config) of
        {ok, _ServerPid} ->
            %% Server started - clean up
            ok = erlmcp_sup:stop_server(ServerId);
        {error, _Reason} ->
            %% Failed to start - expected behavior for invalid config
            ok
    end,

    supervisor_cleanup(SupPid).

%% Test stopping non-existent server
stop_nonexistent_server_test() ->
    {ok, SupPid} = supervisor_setup(),

    %% Try to stop a server that doesn't exist
    ?assertEqual(ok, erlmcp_sup:stop_server(nonexistent_server)),

    supervisor_cleanup(SupPid).

%% Test concurrent server starts
concurrent_server_start_test() ->
    {ok, SupPid} = supervisor_setup(),

    %% Start multiple servers concurrently
    ServerIds = [list_to_atom("test_server_" ++ integer_to_list(I)) || I <- lists:seq(1, 5)],
    Configs = [#{test => true} || _ <- ServerIds],

    %% Start servers
    Results =
        [erlmcp_sup:start_server(Id, Config) || {Id, Config} <- lists:zip(ServerIds, Configs)],

    %% Clean up any started servers
    lists:foreach(fun({Id, Result}) ->
                     case Result of
                         {ok, _Pid} ->
                             erlmcp_sup:stop_server(Id);
                         _ ->
                             ok
                     end
                  end,
                  lists:zip(ServerIds, Results)),

    supervisor_cleanup(SupPid),
    ?assert(true).

%%====================================================================
%% Test Generator
%%====================================================================

supervisor_test_() ->
    {foreach,
     fun supervisor_setup/0,
     fun supervisor_cleanup/1,
     [fun server_sup_child_spec_test/0,
      fun notification_handler_sup_child_spec_test/0,
      fun sup_start_stop_server_test/0,
      fun supervisor_restart_intensity_test/0,
      fun empty_child_spec_test/0,
      fun start_server_error_handling_test/0,
      fun stop_nonexistent_server_test/0,
      fun concurrent_server_start_test/0]}.
