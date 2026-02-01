-module(erlmcp_control_plane_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").
-include("erlmcp_messages.hrl").

%%% ====================================================================
%%% Test Setup and Teardown
%%% ====================================================================

control_plane_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(State) ->
        [test_start_stop(State),
         test_register_component(State),
         test_health_check_priority(State),
         test_drain_session_priority(State),
         test_cancel_task_priority(State),
         test_circuit_breaker_priority(State),
         test_latency_tracking(State),
         test_slo_violations(State),
         test_stats_collection(State),
         test_component_cleanup(State)]
     end}.

setup() ->
    %% Start control plane
    {ok, Pid} = erlmcp_control_plane:start_link(),
    #{control_plane_pid => Pid}.

teardown(#{control_plane_pid := Pid}) ->
    %% Stop control plane
    exit(Pid, normal),
    timer:sleep(100),
    ok.

%%% ====================================================================
%%% Test Cases
%%% ====================================================================

test_start_stop(_State) ->
    {"Start and stop control plane",
     fun() ->
        ?assertMatch({ok, _Pid}, erlmcp_control_plane:start_link()),
        Stats = erlmcp_control_plane:get_stats(),
        ?assertMatch(#{total_delivered := 0}, Stats)
     end}.

test_register_component(_State) ->
    {"Register and unregister components",
     fun() ->
        %% Register a test component
        Handler = fun({control, health_check, _Data}) -> ok end,
        ?assertEqual(ok, erlmcp_control_plane:register_component(test_component, Handler)),

        %% Unregister component
        ?assertEqual(ok, erlmcp_control_plane:unregister_component(test_component))
     end}.

test_health_check_priority(_State) ->
    {"Health check messages handled with priority",
     fun() ->
        %% Register handler
        Handler =
            fun({control, health_check, Data}) ->
               ?assertMatch(#{type := liveness}, Data),
               ok
            end,
        erlmcp_control_plane:register_component(health_test, Handler),

        %% Send health check
        {ok, LatencyUs} = erlmcp_control_plane:send_health_check(health_test, liveness),
        ?assert(LatencyUs < 100000, "Health check should complete in <100ms"),

        %% Verify stats
        Stats = erlmcp_control_plane:get_stats(),
        ?assertMatch(#{total_delivered := 1}, Stats),
        ?assertMatch(#{by_type := #{health_check := 1}}, Stats)
     end}.

test_drain_session_priority(_State) ->
    {"Drain session messages preempt data traffic",
     fun() ->
        %% Register handler
        Handler =
            fun({control, drain_session, Data}) ->
               ?assertMatch(#{session_id := <<"test_session">>}, Data),
               ok
            end,
        erlmcp_control_plane:register_component(drain_test, Handler),

        %% Send drain signal
        {ok, _LatencyUs} = erlmcp_control_plane:send_drain_session(drain_test, <<"test_session">>),

        %% Verify stats
        Stats = erlmcp_control_plane:get_stats(),
        ?assertMatch(#{by_type := #{drain_session := 1}}, Stats)
     end}.

test_cancel_task_priority(_State) ->
    {"Cancel task messages bypass queue depth",
     fun() ->
        %% Register handler
        Handler =
            fun({control, cancel_task, Data}) ->
               ?assertMatch(#{task_id := task_123}, Data),
               ok
            end,
        erlmcp_control_plane:register_component(cancel_test, Handler),

        %% Send cancellation
        {ok, _LatencyUs} = erlmcp_control_plane:send_cancel_task(cancel_test, task_123),

        %% Verify stats
        Stats = erlmcp_control_plane:get_stats(),
        ?assertMatch(#{by_type := #{cancel_task := 1}}, Stats)
     end}.

test_circuit_breaker_priority(_State) ->
    {"Circuit breaker trips are immediate",
     fun() ->
        %% Register handler
        Handler =
            fun({control, circuit_breaker, Data}) ->
               ?assertMatch(#{action := open}, Data),
               ok
            end,
        erlmcp_control_plane:register_component(breaker_test, Handler),

        %% Send circuit breaker signal
        {ok, LatencyUs} = erlmcp_control_plane:send_circuit_breaker(breaker_test, open),
        ?assert(LatencyUs < 10000, "Circuit breaker should trip in <10ms"),

        %% Verify stats
        Stats = erlmcp_control_plane:get_stats(),
        ?assertMatch(#{by_type := #{circuit_breaker := 1}}, Stats)
     end}.

test_latency_tracking( _State ) -> { "Latency tracking for priority messages" , fun ( ) -> Handler = fun ( { control , health_check , _Data } ) -> timer : sleep( 10 ) , ok end , erlmcp_control_plane : register_component( latency_test , Handler ) , lists : foreach( fun ( _ ) -> { ok , _LatencyUs } = erlmcp_control_plane : send_health_check( latency_test , liveness ) end , lists : seq( 1 , 10 ) ) , Stats = erlmcp_control_plane : get_stats( ) , ?assertMatch( #{ latency_p50_us := P50 } when P50 > 0 , Stats ) , ?assertMatch( #{ latency_p95_us := P95 } when P95 > 0 , Stats ) , ?assertMatch( #{ latency_p99_us := P99 } when P99 > 0 , Stats ) , ?assertMatch( #{ max_latency_us := Max } when Max > 0 , Stats ) end } .

         %% Register handler with artificial delay

         %% Send multiple health checks

         %% Verify stats include latency metrics

test_slo_violations( _State ) -> { "SLO violations tracked for health checks >100ms" , fun ( ) -> Handler = fun ( { control , health_check , _Data } ) -> timer : sleep( 150 ) , ok end , erlmcp_control_plane : register_component( slo_test , Handler ) , { ok , LatencyUs } = erlmcp_control_plane : send_health_check( slo_test , liveness ) , ?assert( LatencyUs > 100000 , "Should exceed 100ms SLO" ) , Stats = erlmcp_control_plane : get_stats( ) , ?assertMatch( #{ slo_violations := V } when V > 0 , Stats ) end } .

         %% Register handler with excessive delay

                                % Violate 100ms SLO

         %% Send health check that violates SLO

         %% Verify SLO violation recorded

test_stats_collection(_State) ->
    {"Statistics collection and reset",
     fun() ->
        %% Register handler
        Handler = fun(_Msg) -> ok end,
        erlmcp_control_plane:register_component(stats_test, Handler),

        %% Send various priority messages
        erlmcp_control_plane:send_health_check(stats_test, liveness),
        erlmcp_control_plane:send_drain_session(stats_test, <<"session">>),
        erlmcp_control_plane:send_cancel_task(stats_test, task_1),
        erlmcp_control_plane:send_circuit_breaker(stats_test, open),

        %% Verify stats
        Stats = erlmcp_control_plane:get_stats(),
        ?assertEqual(4, maps:get(total_delivered, Stats)),

        %% Reset stats
        ok = erlmcp_control_plane:reset_stats(),
        NewStats = erlmcp_control_plane:get_stats(),
        ?assertEqual(0, maps:get(total_delivered, NewStats))
     end}.

test_component_cleanup(_State) ->
    {"Component cleanup on process termination",
     fun() ->
        %% Register component with process that will crash
        Handler = fun(_Msg) -> ok end,
        erlmcp_control_plane:register_component(cleanup_test, Handler),

        %% Send message to ensure registration
        {ok, _} = erlmcp_control_plane:send_health_check(cleanup_test, liveness),

        %% Unregister component
        ok = erlmcp_control_plane:unregister_component(cleanup_test),

        %% Sending to unregistered component should fail
        ?assertMatch({error, component_not_registered},
                     erlmcp_control_plane:send_health_check(cleanup_test, liveness))
     end}.
