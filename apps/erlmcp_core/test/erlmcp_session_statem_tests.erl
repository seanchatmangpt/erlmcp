%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_session_statem
%%% Testing session state machine transitions and guards
%%% Chicago School TDD: Real processes, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_statem_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup and teardown
setup() ->
    {ok, Pid} = erlmcp_session_statem:start_link(<<"test_session">>, #{}),
    Pid.

cleanup(Pid) ->
    catch erlmcp_session_statem:terminate(Pid),
    ok.

%%====================================================================
%% State Transition Tests
%%====================================================================

%% @doc Test new state initialization
new_state_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_new">>, #{}),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   {ok, new} = erlmcp_session_statem:get_state(Pid),
                   {ok, Info} = erlmcp_session_statem:get_info(Pid),
                   ?assertMatch(#{session_id := <<"test_new">>}, Info)
               end)]
     end}.

%% @doc Test new -> auth transition
new_to_auth_transition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
                   %% Initialize session
                   {ok, new} = erlmcp_session_statem:init_session(Pid, #{initialized => true}),
                   %% Authenticate
                   AuthContext = #{authenticated => false},
                   {ok, auth} = erlmcp_session_statem:authenticate(Pid, AuthContext),
                   %% Verify state
                   {ok, auth} = erlmcp_session_statem:get_state(Pid)
               end)]
     end}.

%% @doc Test auth -> active transition
auth_to_active_transition_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_auth_active">>, #{}),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Initialize and authenticate
                   {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
                   AuthContext = #{authenticated => true},
                   {ok, auth} = erlmcp_session_statem:authenticate(Pid, AuthContext),
                   %% Activate
                   {ok, active} = erlmcp_session_statem:activate(Pid),
                   %% Verify state
                   {ok, active} = erlmcp_session_statem:get_state(Pid)
               end)]
     end}.

%% @doc Test active -> idle transition
active_to_idle_transition_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_active_idle">>, #{}),
         %% Initialize and activate
         {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
         AuthContext = #{authenticated => true, pre_authenticated => true},
         {ok, active} = erlmcp_session_statem:activate(Pid),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Deactivate
                   {ok, idle} = erlmcp_session_statem:deactivate(Pid),
                   %% Verify state
                   {ok, idle} = erlmcp_session_statem:get_state(Pid)
               end)]
     end}.

%% @doc Test idle -> active transition (resume)
idle_to_active_transition_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_idle_active">>, #{}),
         %% Initialize, activate, deactivate
         {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
         AuthContext = #{authenticated => true, pre_authenticated => true},
         {ok, active} = erlmcp_session_statem:activate(Pid),
         {ok, idle} = erlmcp_session_statem:deactivate(Pid),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Resume
                   {ok, active} = erlmcp_session_statem:resume(Pid),
                   %% Verify state
                   {ok, active} = erlmcp_session_statem:get_state(Pid)
               end)]
     end}.

%% @doc Test active -> suspended transition
active_to_suspended_transition_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_active_suspended">>, #{}),
         %% Initialize and activate
         {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
         AuthContext = #{authenticated => true, pre_authenticated => true},
         {ok, active} = erlmcp_session_statem:activate(Pid),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Suspend
                   {ok, suspended} = erlmcp_session_statem:suspend(Pid),
                   %% Verify state
                   {ok, suspended} = erlmcp_session_statem:get_state(Pid)
               end)]
     end}.

%% @doc Test suspended -> active transition
suspended_to_active_transition_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_suspended_active">>, #{}),
         %% Initialize, activate, suspend
         {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
         AuthContext = #{authenticated => true, pre_authenticated => true},
         {ok, active} = erlmcp_session_statem:activate(Pid),
         {ok, suspended} = erlmcp_session_statem:suspend(Pid),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Resume
                   {ok, active} = erlmcp_session_statem:resume(Pid),
                   %% Verify state
                   {ok, active} = erlmcp_session_statem:get_state(Pid)
               end)]
     end}.

%% @doc Test any state -> terminated transition
terminate_from_any_state_test_() ->
    {foreach,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_terminate">>, #{}),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     [fun(Pid) ->
           ?_test(begin
                     ok = erlmcp_session_statem:terminate(Pid)
                 end)
      end]}.

%%====================================================================
%% Resource Management Tests
%%====================================================================

%% @doc Test quota setting and checking
quota_management_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_quota">>, #{}),
         %% Activate session
         {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
         AuthContext = #{authenticated => true, pre_authenticated => true},
         {ok, active} = erlmcp_session_statem:activate(Pid),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Set quota
                   Quota = #{max_requests => 1000, max_connections => 10},
                   ok = erlmcp_session_statem:set_quota(Pid, Quota),
                   %% Check quota
                   {ok, true, _Exceeded} = erlmcp_session_statem:check_quota(Pid),
                   %% Update resources
                   ok = erlmcp_session_statem:update_resources(Pid, #{requests => 100}),
                   %% Check quota again
                   {ok, HasQuota, _} = erlmcp_session_statem:check_quota(Pid),
                   ?assert(HasQuota)
               end)]
     end}.

%% @doc Test quota enforcement
quota_enforcement_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_quota_enforce">>, #{}),
         %% Activate session
         {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
         AuthContext = #{authenticated => true, pre_authenticated => true},
         {ok, active} = erlmcp_session_statem:activate(Pid),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Set small quota
                   Quota = #{max_requests => 10, max_connections => 5},
                   ok = erlmcp_session_statem:set_quota(Pid, Quota),
                   %% Use resources within quota
                   ok = erlmcp_session_statem:update_resources(Pid, #{requests => 5}),
                   {ok, true, _} = erlmcp_session_statem:check_quota(Pid),
                   %% Exceed quota
                   {error, quota_exceeded} =
                       erlmcp_session_statem:update_resources(Pid, #{requests => 10})
               end)]
     end}.

%%====================================================================
%% Metrics Tests
%%====================================================================

%% @doc Test session metrics collection
metrics_collection_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_metrics">>, #{}),
         %% Activate session
         {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
         AuthContext = #{authenticated => true, pre_authenticated => true},
         {ok, active} = erlmcp_session_statem:activate(Pid),
         timer:sleep(100),  % Small delay for age calculation
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   {ok, Metrics} = erlmcp_session_statem:get_metrics(Pid),
                   ?assert(maps:is_key(session_age_ms, Metrics)),
                   ?assert(maps:is_key(state_duration_ms, Metrics)),
                   ?assert(maps:is_key(idle_time_ms, Metrics)),
                   ?assert(maps:is_key(auth_attempts, Metrics)),
                   ?assert(maps:is_key(quota_utilization, Metrics))
               end)]
     end}.

%%====================================================================
%% Persistence Tests
%%====================================================================

%% @doc Test session persistence (with ETS backend)
persistence_test_() ->
    {setup,
     fun() ->
         %% Start session backend with ETS
         {ok, BackendPid} = erlmcp_session_backend:start_link(
                              #{backend => erlmcp_session_ets,
                                cleanup_interval => 60000}),
         {ok, Pid} = erlmcp_session_statem:start_link(
                       <<"test_persist">>,
                       #{persistent => true, backend => erlmcp_session_ets}),
         %% Activate session
         {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
         AuthContext = #{authenticated => true, pre_authenticated => true},
         {ok, active} = erlmcp_session_statem:activate(Pid),
         {Pid, BackendPid}
     end,
     fun({Pid, BackendPid}) ->
         catch erlmcp_session_statem:terminate(Pid),
         catch erlmcp_session_backend:cleanup_expired(),
         catch gen_server:stop(BackendPid)
     end,
     fun({Pid, _BackendPid}) ->
         [?_test(begin
                   %% Persist session
                   ok = erlmcp_session_statem:persist(Pid),
                   %% Note: Full load test would require more complex setup
                   %% This test verifies the persist call succeeds
               end)]
     end}.

%%====================================================================
%% Subscription Tests
%%====================================================================

%% @doc Test event subscription
subscription_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_subscribe">>, #{}),
         %% Activate session
         {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
         AuthContext = #{authenticated => true, pre_authenticated => true},
         {ok, active} = erlmcp_session_statem:activate(Pid),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Subscribe
                   ok = erlmcp_session_statem:subscribe(Pid, self()),
                   %% Trigger event
                   ok = erlmcp_session_statem:suspend(Pid),
                   %% Verify event received (timeout after 1s)
                   receive
                       {session_event, <<"test_subscribe">>, {session_suspended, _}} ->
                           ok;
                       _Other ->
                           ?assert(false, received_wrong_event)
                   after 1000 ->
                           ?assert(false, no_event_received)
                   end,
                   %% Unsubscribe
                   ok = erlmcp_session_statem:unsubscribe(Pid, self())
               end)]
     end}.

%%====================================================================
%% Guard Tests
%%====================================================================

%% @doc Test max auth attempts guard
max_auth_attempts_guard_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(
                       <<"test_auth_attempts">>,
                       #{max_auth_attempts => 2}),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Initialize session
                   {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
                   %% First auth attempt (fail)
                   {ok, auth} = erlmcp_session_statem:authenticate(
                                   Pid, #{authenticated => false}),
                   %% Second auth attempt (fail)
                   {ok, auth} = erlmcp_session_statem:authenticate(
                                   Pid, #{authenticated => false}),
                   %% Third attempt should fail
                   {error, max_attempts} = erlmcp_session_statem:authenticate(
                                            Pid, #{authenticated => false})
               end)]
     end}.

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% @doc Test invalid state transitions
invalid_transition_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_invalid">>, #{}),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Try to activate from new without auth
                   {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
                   {error, not_authenticated} = erlmcp_session_statem:activate(Pid)
               end)]
     end}.

%% @doc Test termination from terminated state
already_terminated_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_session_statem:start_link(<<"test_term">>, #{}),
         ok = erlmcp_session_statem:terminate(Pid),
         Pid
     end,
     fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
     fun(Pid) ->
         [?_test(begin
                   %% Try to operate on terminated session
                   {error, session_terminated} = erlmcp_session_statem:get_state(Pid)
               end)]
     end}.

%%====================================================================
%% Concurrent Access Tests
%%====================================================================

%% @doc Test concurrent state transitions
concurrent_transitions_test_() ->
    {timeout, 10,
     {setup,
      fun() ->
          {ok, Pid} = erlmcp_session_statem:start_link(
                        <<"test_concurrent">>,
                        #{max_auth_attempts => 10}),
          {ok, new} = erlmcp_session_statem:init_session(Pid, #{}),
          AuthContext = #{authenticated => true, pre_authenticated => true},
          {ok, active} = erlmcp_session_statem:activate(Pid),
          Pid
      end,
      fun(Pid) -> catch erlmcp_session_statem:terminate(Pid) end,
      fun(Pid) ->
          [?_test(begin
                    %% Spawn multiple processes updating resources
                    Self = self(),
                    NumWorkers = 10,
                    lists:foreach(fun(_) ->
                                          spawn(fun() ->
                                                        ok = erlmcp_session_statem:update_resources(
                                                              Pid, #{requests => 1}),
                                                        Self ! done
                                                end)
                                  end,
                                  lists:seq(1, NumWorkers)),
                    %% Wait for all workers
                    wait_for_done(NumWorkers),
                    %% Verify session still active
                    {ok, active} = erlmcp_session_statem:get_state(Pid)
                end)]
      end}}.

%% @private Wait for N done messages
wait_for_done(0) -> ok;
wait_for_done(N) ->
    receive
        done -> wait_for_done(N - 1)
    after 5000 ->
        error(timeout_waiting_for_workers)
    end.
