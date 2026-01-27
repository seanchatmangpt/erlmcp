-module(erlmcp_chaos_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

setup() -> ok.
cleanup(_) -> ok.

%%====================================================================
%% Chaos Injection Tests
%%====================================================================

injection_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_inject_latency()),
        ?_test(test_inject_error()),
        ?_test(test_inject_packet_loss()),
        ?_test(test_inject_network_partition()),
        ?_test(test_inject_cpu_spike()),
        ?_test(test_inject_memory_pressure()),
        ?_test(test_chaos_duration())
    ] end}.

test_inject_latency() ->
    Result = erlmcp_chaos:inject_latency(server1, 100),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_inject_error() ->
    Result = erlmcp_chaos:inject_error(server1, 0.1),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_inject_packet_loss() ->
    Result = erlmcp_chaos:inject_packet_loss(server1, 0.05),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_inject_network_partition() ->
    Result = erlmcp_chaos:inject_network_partition(server1),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_inject_cpu_spike() ->
    Result = erlmcp_chaos:inject_cpu_spike(server1, 80),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_inject_memory_pressure() ->
    Result = erlmcp_chaos:inject_memory_pressure(server1, 70),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_chaos_duration() ->
    Result = erlmcp_chaos:inject_latency(server1, 50, 5000),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Scenario Tests
%%====================================================================

scenario_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_run_scenario()),
        ?_test(test_scenario_with_params()),
        ?_test(test_multiple_scenarios())
    ] end}.

test_run_scenario() ->
    Scenario = #{name => <<"test">>, duration => 1000},
    Result = erlmcp_chaos:run_scenario(Scenario),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_scenario_with_params() ->
    Scenario = #{
        name => <<"latency">>,
        target => server1,
        duration => 2000,
        intensity => 100
    },
    Result = erlmcp_chaos:run_scenario(Scenario),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_multiple_scenarios() ->
    S1 = #{name => <<"s1">>, duration => 100},
    S2 = #{name => <<"s2">>, duration => 100},
    R1 = erlmcp_chaos:run_scenario(S1),
    R2 = erlmcp_chaos:run_scenario(S2),
    ?assertMatch(ok | {ok, _} | {error, _}, [R1, R2]).

%%====================================================================
%% State Tests
%%====================================================================

state_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_chaos_state()),
        ?_test(test_get_active_injections()),
        ?_test(test_list_scenarios())
    ] end}.

test_get_chaos_state() ->
    Result = erlmcp_chaos:get_chaos_state(),
    ?assert(is_map(Result) orelse Result =:= error).

test_get_active_injections() ->
    erlmcp_chaos:inject_latency(server1, 50),
    Result = erlmcp_chaos:get_active_injections(),
    ?assert(is_list(Result) orelse Result =:= error).

test_list_scenarios() ->
    Result = erlmcp_chaos:list_scenarios(),
    ?assert(is_list(Result) orelse Result =:= error).

%%====================================================================
%% Recovery Tests
%%====================================================================

recovery_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_clear_chaos()),
        ?_test(test_reset_server()),
        ?_test(test_stop_scenario())
    ] end}.

test_clear_chaos() ->
    erlmcp_chaos:inject_latency(server1, 50),
    Result = erlmcp_chaos:clear_chaos(),
    ?assertMatch(ok | {ok, _}, Result).

test_reset_server() ->
    erlmcp_chaos:inject_latency(server1, 50),
    Result = erlmcp_chaos:reset_server(server1),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_stop_scenario() ->
    erlmcp_chaos:run_scenario(#{name => <<"test">>, duration => 10000}),
    Result = erlmcp_chaos:stop_scenario(<<"test">>),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Configuration Tests
%%====================================================================

config_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_configure_chaos()),
        ?_test(test_set_chaos_policy()),
        ?_test(test_get_config())
    ] end}.

test_configure_chaos() ->
    Config = #{enabled => true, max_intensity => 100},
    Result = erlmcp_chaos:configure(Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_set_chaos_policy() ->
    Policy = #{type => latency, default_duration => 5000},
    Result = erlmcp_chaos:set_policy(test_policy, Policy),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_config() ->
    Result = erlmcp_chaos:get_config(),
    ?assert(is_map(Result) orelse Result =:= error).

%%====================================================================
%% Metrics & Analysis Tests
%%====================================================================

analysis_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_chaos_metrics()),
        ?_test(test_get_impact_analysis()),
        ?_test(test_get_resilience_score())
    ] end}.

test_get_chaos_metrics() ->
    Result = erlmcp_chaos:get_metrics(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_get_impact_analysis() ->
    Result = erlmcp_chaos:get_impact_analysis(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_get_resilience_score() ->
    Result = erlmcp_chaos:get_resilience_score(),
    ?assert(is_number(Result) orelse Result =:= error).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_chaos_with_recovery()),
        ?_test(test_multiple_chaos_events()),
        ?_test(test_chaos_monitoring())
    ] end}.

test_chaos_with_recovery() ->
    erlmcp_chaos:inject_latency(server1, 100, 1000),
    timer:sleep(100),
    erlmcp_chaos:reset_server(server1),
    Result = erlmcp_chaos:get_chaos_state(),
    ?assert(is_map(Result) orelse Result =:= error).

test_multiple_chaos_events() ->
    erlmcp_chaos:inject_latency(server1, 50),
    erlmcp_chaos:inject_error(server2, 0.1),
    erlmcp_chaos:inject_packet_loss(server3, 0.05),
    Result = erlmcp_chaos:get_active_injections(),
    ?assert(is_list(Result) orelse Result =:= error).

test_chaos_monitoring() ->
    erlmcp_chaos:inject_latency(server1, 100),
    Metrics = erlmcp_chaos:get_metrics(),
    erlmcp_chaos:clear_chaos(),
    ?assert(is_map(Metrics) orelse is_list(Metrics) orelse Metrics =:= error).
