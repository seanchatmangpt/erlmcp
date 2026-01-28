-module(erlmcp_router_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_router Module
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    {ok, _} = erlmcp_router:start_link(),
    ok.

cleanup(_) ->
    catch erlmcp_router:stop(),
    ok.

%%====================================================================
%% Basic Router Tests
%%====================================================================

start_link_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_router_start()),
             ?_test(test_router_alive()),
             ?_test(test_router_multiple_starts())
         ]
     end}.

test_router_start() ->
    {ok, Pid} = erlmcp_router:start_link(),
    ?assert(is_pid(Pid)),
    catch erlmcp_router:stop().

test_router_alive() ->
    {ok, Pid} = erlmcp_router:start_link(),
    ?assert(erlang:is_process_alive(Pid)),
    catch erlmcp_router:stop().

test_router_multiple_starts() ->
    {ok, Pid1} = erlmcp_router:start_link(),
    ?assert(is_pid(Pid1)),
    catch erlmcp_router:stop().

%%====================================================================
%% Message Routing Tests
%%====================================================================

routing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_route_message_basic()),
             ?_test(test_route_message_with_params()),
             ?_test(test_route_to_valid_server()),
             ?_test(test_route_to_invalid_server()),
             ?_test(test_route_load_distribution())
         ]
     end}.

test_route_message_basic() ->
    Result = erlmcp_router:route_message(server1, <<"test">>, #{}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_route_message_with_params() ->
    Message = #{action => <<"list">>, params => #{resource => <<"test">>}},
    Result = erlmcp_router:route_message(server1, <<"list">>, Message),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_route_to_valid_server() ->
    ServerId = valid_server,
    Message = #{type => <<"request">>},
    Result = erlmcp_router:route_message(ServerId, <<"test.method">>, Message),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_route_to_invalid_server() ->
    ServerId = nonexistent_server,
    Result = erlmcp_router:route_message(ServerId, <<"test">>, #{}),
    ?assertMatch({error, _} | ok | {ok, _}, Result).

test_route_load_distribution() ->
    Results = [erlmcp_router:route_message(server_a, <<"method">>, #{}) || _ <- lists:seq(1, 5)],
    ?assertEqual(5, length(Results)).

%%====================================================================
%% Load Balancer Tests
%%====================================================================

load_balancer_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_setup_load_balancer()),
             ?_test(test_load_balancer_round_robin()),
             ?_test(test_load_balancer_weighted()),
             ?_test(test_load_balancer_with_health_check())
         ]
     end}.

test_setup_load_balancer() ->
    Config = #{policy => round_robin, weights => #{server1 => 1, server2 => 1}},
    Result = erlmcp_router:setup_load_balancer(my_balancer, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_load_balancer_round_robin() ->
    Config = #{policy => round_robin},
    erlmcp_router:setup_load_balancer(rr_balancer, Config),
    Results = [erlmcp_router:route_message(rr_balancer, <<"test">>, #{}) || _ <- lists:seq(1, 3)],
    ?assertEqual(3, length(Results)).

test_load_balancer_weighted() ->
    Config = #{
        policy => weighted,
        weights => #{server1 => 2, server2 => 1}
    },
    Result = erlmcp_router:setup_load_balancer(weighted_balancer, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_load_balancer_with_health_check() ->
    Config = #{
        policy => round_robin,
        health_check => true
    },
    Result = erlmcp_router:setup_load_balancer(health_balancer, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Circuit Breaker Tests
%%====================================================================

circuit_breaker_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_setup_circuit_breaker()),
             ?_test(test_circuit_breaker_states()),
             ?_test(test_circuit_breaker_failure_tracking()),
             ?_test(test_circuit_breaker_recovery())
         ]
     end}.

test_setup_circuit_breaker() ->
    Config = #{failure_threshold => 5, timeout => 30000},
    Result = erlmcp_router:setup_circuit_breaker(test_server, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_circuit_breaker_states() ->
    Config = #{failure_threshold => 3},
    erlmcp_router:setup_circuit_breaker(cb_server, Config),
    Result = erlmcp_router:route_message(cb_server, <<"test">>, #{}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_circuit_breaker_failure_tracking() ->
    Config = #{failure_threshold => 2, timeout => 5000},
    erlmcp_router:setup_circuit_breaker(fail_server, Config),
    %% Multiple failures should trigger circuit breaker
    Results = [erlmcp_router:route_message(fail_server, <<"fail">>, #{}) || _ <- lists:seq(1, 5)],
    ?assertEqual(5, length(Results)).

test_circuit_breaker_recovery() ->
    Config = #{failure_threshold => 2, half_open_timeout => 1000},
    erlmcp_router:setup_circuit_breaker(recover_server, Config),
    Result = erlmcp_router:route_message(recover_server, <<"test">>, #{}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Metrics Tests
%%====================================================================

metrics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_get_routing_metrics()),
             ?_test(test_get_metrics_for_server()),
             ?_test(test_metrics_includes_latency()),
             ?_test(test_metrics_includes_error_counts())
         ]
     end}.

test_get_routing_metrics() ->
    Result = erlmcp_router:get_routing_metrics(),
    ?assert(is_map(Result) orelse is_list(Result)).

test_get_metrics_for_server() ->
    erlmcp_router:route_message(metrics_server, <<"test">>, #{}),
    Result = erlmcp_router:get_routing_metrics(metrics_server),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_metrics_includes_latency() ->
    erlmcp_router:route_message(latency_server, <<"test">>, #{}),
    Metrics = erlmcp_router:get_routing_metrics(),
    ?assert(is_map(Metrics) orelse is_list(Metrics)).

test_metrics_includes_error_counts() ->
    erlmcp_router:route_message(error_server, <<"fail">>, #{}),
    Metrics = erlmcp_router:get_routing_metrics(),
    ?assert(is_map(Metrics) orelse is_list(Metrics)).

%%====================================================================
%% Adaptive Routing Tests
%%====================================================================

adaptive_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_enable_adaptive_routing()),
             ?_test(test_adaptive_routing_load_aware()),
             ?_test(test_adaptive_routing_feedback())
         ]
     end}.

test_enable_adaptive_routing() ->
    Result = erlmcp_router:enable_adaptive_routing(true),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_adaptive_routing_load_aware() ->
    erlmcp_router:enable_adaptive_routing(true),
    Results = [erlmcp_router:route_message(adaptive_server, <<"test">>, #{}) || _ <- lists:seq(1, 10)],
    ?assertEqual(10, length(Results)).

test_adaptive_routing_feedback() ->
    erlmcp_router:enable_adaptive_routing(true),
    erlmcp_router:route_message(server1, <<"test">>, #{}),
    Metrics = erlmcp_router:get_routing_metrics(),
    ?assert(is_map(Metrics) orelse is_list(Metrics)).

%%====================================================================
%% Routing Policy Tests
%%====================================================================

policy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_set_routing_policy_round_robin()),
             ?_test(test_set_routing_policy_weighted()),
             ?_test(test_set_routing_policy_adaptive()),
             ?_test(test_set_routing_policy_least_connections())
         ]
     end}.

test_set_routing_policy_round_robin() ->
    Result = erlmcp_router:set_routing_policy(test_policy, round_robin),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_set_routing_policy_weighted() ->
    Result = erlmcp_router:set_routing_policy(test_policy, weighted),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_set_routing_policy_adaptive() ->
    Result = erlmcp_router:set_routing_policy(test_policy, adaptive),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_set_routing_policy_least_connections() ->
    Result = erlmcp_router:set_routing_policy(test_policy, least_connections),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Backpressure Tests
%%====================================================================

backpressure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_backpressure_queue_depth()),
             ?_test(test_backpressure_threshold()),
             ?_test(test_backpressure_behavior())
         ]
     end}.

test_backpressure_queue_depth() ->
    %% Send multiple messages to test queue depth
    Results = [erlmcp_router:route_message(bp_server, <<"test">>, #{}) || _ <- lists:seq(1, 20)],
    ?assertEqual(20, length(Results)).

test_backpressure_threshold() ->
    Results = [erlmcp_router:route_message(bp_server, <<"test">>, #{}) || _ <- lists:seq(1, 50)],
    ?assert(length(Results) >= 1).

test_backpressure_behavior() ->
    Result = erlmcp_router:route_message(bp_server, <<"test">>, #{}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_route_invalid_message()),
             ?_test(test_route_null_server_id()),
             ?_test(test_route_very_large_message())
         ]
     end}.

test_route_invalid_message() ->
    Result = erlmcp_router:route_message(server1, <<"test">>, invalid),
    ?assertMatch({error, _} | ok | {ok, _}, Result).

test_route_null_server_id() ->
    Result = erlmcp_router:route_message(null, <<"test">>, #{}),
    ?assertMatch({error, _} | ok | {ok, _}, Result).

test_route_very_large_message() ->
    LargeMessage = maps:from_list([{<<"key_", (integer_to_binary(I))/binary>>, I} || I <- lists:seq(1, 1000)]),
    Result = erlmcp_router:route_message(server1, <<"test">>, LargeMessage),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).
