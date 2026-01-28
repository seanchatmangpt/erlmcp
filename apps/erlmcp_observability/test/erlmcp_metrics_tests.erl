-module(erlmcp_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests for erlmcp_metrics module (gen_server-based metrics)
%%====================================================================

start_stop_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    ?assert(is_pid(Pid)),
    ok = gen_server:stop(Pid).

record_transport_operation_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Result = erlmcp_metrics:record_transport_operation(tcp, send, 1024, 15),
    ?assertMatch(ok, Result),
    gen_server:stop(Pid).

record_server_operation_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Result = erlmcp_metrics:record_server_operation(test_server, initialize, ok, 50),
    ?assertMatch(ok, Result),
    gen_server:stop(Pid).

record_registry_operation_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Result = erlmcp_metrics:record_registry_operation(register, ok, 10),
    ?assertMatch(ok, Result),
    gen_server:stop(Pid).

get_metrics_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(tcp, send, 512, 10),
    Result = erlmcp_metrics:get_metrics(),
    ?assert(is_map(Result)),
    gen_server:stop(Pid).

metrics_lifecycle_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),

    erlmcp_metrics:record_transport_operation(tcp, send, 1024, 15),
    erlmcp_metrics:record_server_operation(server1, call, ok, 20),
    erlmcp_metrics:record_registry_operation(lookup, ok, 5),

    Metrics = erlmcp_metrics:get_metrics(),
    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(transports, Metrics) orelse maps:is_key(servers, Metrics)),

    gen_server:stop(Pid).
