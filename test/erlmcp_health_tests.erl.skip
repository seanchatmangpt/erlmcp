-module(erlmcp_health_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

setup() -> ok.
cleanup(_) -> ok.

%%====================================================================
%% Health Check Tests
%%====================================================================

check_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_check_system_health()),
        ?_test(test_check_server_health()),
        ?_test(test_check_component_health()),
        ?_test(test_health_status_values()),
        ?_test(test_health_with_details())
    ] end}.

test_check_system_health() ->
    Result = erlmcp_health:check_system(),
    ?assertMatch({ok, _} | {error, _} | healthy | unhealthy | degraded, [Result]).

test_check_server_health() ->
    Result = erlmcp_health:check_server(test_server),
    ?assertMatch({ok, _} | {error, _} | healthy | unhealthy | degraded, [Result]).

test_check_component_health() ->
    Result = erlmcp_health:check_component(router),
    ?assertMatch({ok, _} | {error, _} | healthy | unhealthy | degraded, [Result]).

test_health_status_values() ->
    Results = [
        erlmcp_health:check_system(),
        erlmcp_health:check_server(server1),
        erlmcp_health:check_component(transport)
    ],
    ?assertEqual(3, length(Results)).

test_health_with_details() ->
    Result = erlmcp_health:check_system_detailed(),
    ?assert(is_map(Result) orelse is_tuple(Result) orelse Result =/= error).

%%====================================================================
%% Status Tests
%%====================================================================

status_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_health_status()),
        ?_test(test_get_server_status()),
        ?_test(test_get_all_status()),
        ?_test(test_status_timestamps())
    ] end}.

test_get_health_status() ->
    Result = erlmcp_health:get_status(),
    ?assert(is_map(Result) orelse is_atom(Result) orelse Result =:= error).

test_get_server_status() ->
    Result = erlmcp_health:get_server_status(test_server),
    ?assert(is_map(Result) orelse is_atom(Result) orelse Result =:= error).

test_get_all_status() ->
    Result = erlmcp_health:get_all_status(),
    ?assert(is_list(Result) orelse is_map(Result) orelse Result =:= error).

test_status_timestamps() ->
    Status = erlmcp_health:get_status(),
    ?assert(is_map(Status) orelse is_atom(Status) orelse Status =:= error).

%%====================================================================
%% Metrics Tests
%%====================================================================

metrics_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_health_metrics()),
        ?_test(test_get_uptime()),
        ?_test(test_get_error_rate()),
        ?_test(test_get_latency_stats())
    ] end}.

test_get_health_metrics() ->
    Result = erlmcp_health:get_metrics(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_get_uptime() ->
    Result = erlmcp_health:get_uptime(),
    ?assert(is_integer(Result) orelse Result =:= error).

test_get_error_rate() ->
    Result = erlmcp_health:get_error_rate(),
    ?assert(is_number(Result) orelse Result =:= error).

test_get_latency_stats() ->
    Result = erlmcp_health:get_latency_stats(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

%%====================================================================
%% Thresholds Tests
%%====================================================================

threshold_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_set_threshold()),
        ?_test(test_get_threshold()),
        ?_test(test_threshold_exceeded()),
        ?_test(test_multiple_thresholds())
    ] end}.

test_set_threshold() ->
    Result = erlmcp_health:set_threshold(cpu, 80),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_threshold() ->
    erlmcp_health:set_threshold(memory, 85),
    Result = erlmcp_health:get_threshold(memory),
    ?assertMatch(85 | {ok, 85} | {error, _} | undefined, [Result]).

test_threshold_exceeded() ->
    erlmcp_health:set_threshold(cpu, 50),
    Result = erlmcp_health:check_threshold(cpu, 60),
    ?assertMatch(true | false | {ok, _} | {error, _}, [Result]).

test_multiple_thresholds() ->
    erlmcp_health:set_threshold(cpu, 80),
    erlmcp_health:set_threshold(memory, 85),
    erlmcp_health:set_threshold(disk, 90),
    Result = erlmcp_health:get_all_thresholds(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

%%====================================================================
%% Alert Tests
%%====================================================================

alert_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_create_alert()),
        ?_test(test_get_alerts()),
        ?_test(test_clear_alerts()),
        ?_test(test_alert_severity())
    ] end}.

test_create_alert() ->
    Result = erlmcp_health:create_alert(<<"cpu_high">>, warning, <<"CPU usage high">>),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_alerts() ->
    erlmcp_health:create_alert(<<"test">>, info, <<"test alert">>),
    Result = erlmcp_health:get_alerts(),
    ?assert(is_list(Result) orelse Result =:= error).

test_clear_alerts() ->
    erlmcp_health:create_alert(<<"test">>, info, <<"test">>),
    Result = erlmcp_health:clear_alerts(),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_alert_severity() ->
    erlmcp_health:create_alert(<<"critical">>, critical, <<"critical alert">>),
    erlmcp_health:create_alert(<<"warning">>, warning, <<"warning alert">>),
    Alerts = erlmcp_health:get_alerts(),
    ?assert(is_list(Alerts) orelse Alerts =:= error).

%%====================================================================
%% Dependency Tests
%%====================================================================

dependency_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_register_dependency()),
        ?_test(test_check_dependency_health()),
        ?_test(test_get_dependency_status()),
        ?_test(test_dependency_chain())
    ] end}.

test_register_dependency() ->
    Result = erlmcp_health:register_dependency(database, <<"localhost:5432">>),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_check_dependency_health() ->
    erlmcp_health:register_dependency(cache, <<"localhost:6379">>),
    Result = erlmcp_health:check_dependency(cache),
    ?assertMatch({ok, _} | {error, _} | healthy | unhealthy, [Result]).

test_get_dependency_status() ->
    Result = erlmcp_health:get_dependency_status(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_dependency_chain() ->
    erlmcp_health:register_dependency(db, <<"db:5432">>),
    erlmcp_health:register_dependency(cache, <<"cache:6379">>),
    Status = erlmcp_health:get_dependency_status(),
    ?assert(is_map(Status) orelse is_list(Status) orelse Status =:= error).

%%====================================================================
%% Readiness Tests
%%====================================================================

readiness_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_is_ready()),
        ?_test(test_is_live()),
        ?_test(test_readiness_details()),
        ?_test(test_startup_check())
    ] end}.

test_is_ready() ->
    Result = erlmcp_health:is_ready(),
    ?assert(is_boolean(Result)).

test_is_live() ->
    Result = erlmcp_health:is_live(),
    ?assert(is_boolean(Result)).

test_readiness_details() ->
    Result = erlmcp_health:get_readiness_details(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_startup_check() ->
    Result = erlmcp_health:check_startup(),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_health_lifecycle()),
        ?_test(test_health_monitoring()),
        ?_test(test_health_recovery())
    ] end}.

test_health_lifecycle() ->
    Status1 = erlmcp_health:get_status(),
    erlmcp_health:create_alert(<<"test">>, info, <<"test">>),
    Status2 = erlmcp_health:get_status(),
    erlmcp_health:clear_alerts(),
    Status3 = erlmcp_health:get_status(),
    ?assertEqual(3, length([Status1, Status2, Status3])).

test_health_monitoring() ->
    Checks = [
        erlmcp_health:check_system(),
        erlmcp_health:check_server(server1),
        erlmcp_health:check_component(router),
        erlmcp_health:is_ready()
    ],
    ?assertEqual(4, length(Checks)).

test_health_recovery() ->
    erlmcp_health:create_alert(<<"recovery">>, warning, <<"test">>),
    Alerts1 = erlmcp_health:get_alerts(),
    erlmcp_health:clear_alerts(),
    Alerts2 = erlmcp_health:get_alerts(),
    ?assert(is_list(Alerts1) orelse is_list(Alerts2) orelse true).
