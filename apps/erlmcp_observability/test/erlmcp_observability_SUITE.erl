%%%-------------------------------------------------------------------
%% @doc erlmcp_observability_SUITE - Common Test integration suite
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_observability_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([
    test_metrics_integration/1,
    test_otel_integration/1,
    test_health_integration/1,
    test_full_observability_stack/1
]).

suite() -> [{timetrap, {seconds, 60}}].

all() -> [
    test_metrics_integration,
    test_otel_integration,
    test_health_integration,
    test_full_observability_stack
].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp_observability),
    ct:log("Observability application started"),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_observability),
    ok.

test_metrics_integration(_Config) ->
    ct:log("Testing metrics integration"),
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(tcp, send, 1024, 10),
    Metrics = erlmcp_metrics:get_metrics(),
    ?assert(is_map(Metrics)),
    gen_server:stop(Pid),
    ok.

test_otel_integration(_Config) ->
    ct:log("Testing OTEL integration"),
    erlmcp_otel:init(#{service_name => <<"ct_test">>}),
    Fun = fun() -> ok end,
    Result = erlmcp_otel:with_span(<<"ct.test">>, #{}, Fun),
    ?assertEqual(ok, Result),
    ok.

test_health_integration(_Config) ->
    ct:log("Testing health monitoring integration"),
    {ok, Pid} = erlmcp_health_monitor:start_link(),
    erlmcp_health_monitor:register_component(test_comp, self()),
    Health = erlmcp_health_monitor:get_system_health(),
    ?assert(is_map(Health)),
    gen_server:stop(Pid),
    ok.

test_full_observability_stack(_Config) ->
    ct:log("Testing full observability stack"),

    {ok, MetricsPid} = erlmcp_metrics:start_link(),
    {ok, HealthPid} = erlmcp_health_monitor:start_link(),
    erlmcp_otel:init(#{service_name => <<"full_stack_test">>}),

    Fun = fun() ->
        erlmcp_metrics:record_transport_operation(tcp, send, 512, 5),
        erlmcp_health_monitor:register_component(test_server, self()),
        ok
    end,

    Result = erlmcp_otel:with_span(<<"full.stack.operation">>, #{}, Fun),
    ?assertEqual(ok, Result),

    Metrics = erlmcp_metrics:get_metrics(),
    ?assert(is_map(Metrics)),

    Health = erlmcp_health_monitor:get_system_health(),
    ?assert(is_map(Health)),

    gen_server:stop(MetricsPid),
    gen_server:stop(HealthPid),
    ok.
