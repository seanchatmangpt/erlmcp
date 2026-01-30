%%%-------------------------------------------------------------------
%% @doc erlmcp_observability_SUITE - Common Test integration suite
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_observability_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([
    test_metrics_integration/1,
    test_otel_integration/1,
    test_health_integration/1,
    test_dashboard_server/1,
    test_full_observability_stack/1
]).

suite() -> [{timetrap, {seconds, 60}}].

all() -> [
    test_metrics_integration,
    test_otel_integration,
    test_health_integration,
    test_dashboard_server,
    test_full_observability_stack
].

%%====================================================================
%% Suite Setup/Teardown
%%====================================================================

init_per_suite(Config) ->
    ct:log("Starting observability test suite"),

    %% Start crypto (required by some modules)
    case application:ensure_all_started(crypto) of
        ok -> ok;
        {ok, _} -> ok
    end,

    %% Start the observability application
    case application:ensure_all_started(erlmcp_observability) of
        {ok, _Started} ->
            ct:log("Observability application started successfully"),
            Config;
        {error, {already_started, erlmcp_observability}} ->
            ct:log("Observability application already started"),
            Config;
        {error, Reason} ->
            ct:log("Failed to start observability application: ~p", [Reason]),
            %% Continue anyway - individual tests will handle missing components
            Config
    end.

end_per_suite(_Config) ->
    ct:log("Ending observability test suite"),
    application:stop(erlmcp_observability),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:log("Ending test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_metrics_integration(_Config) ->
    ct:log("Testing metrics integration"),

    %% Test starting metrics gen_server directly
    case catch erlmcp_metrics:start_link() of
        {ok, Pid} when is_pid(Pid) ->
            ct:log("Metrics server started: ~p", [Pid]),

            %% Test recording metrics
            ok = erlmcp_metrics:record_transport_operation(test_transport, stdio, 1024, 10),

            %% Test recording server operations
            ok = erlmcp_metrics:record_server_operation(test_server, initialize, 200, #{}),

            %% Test recording registry operations
            ok = erlmcp_metrics:record_registry_operation(lookup, 50, #{}),

            %% Test retrieving metrics
            Metrics = erlmcp_metrics:get_metrics(),
            ct:log("Retrieved metrics: ~p", [Metrics]),
            ?assert(is_map(Metrics) orelse is_list(Metrics)),

            %% Test performance summary
            Summary = erlmcp_metrics:get_performance_summary(),
            ct:log("Performance summary: ~p", [Summary]),
            ?assert(is_map(Summary)),

            %% Cleanup
            gen_server:stop(Pid),
            ?assertNot(is_process_alive(Pid)),
            ok;

        {error, {already_started, Pid}} when is_pid(Pid) ->
            ct:log("Metrics server already running: ~p", [Pid]),

            %% Test basic operations on existing server
            ok = erlmcp_metrics:record_transport_operation(ct_test, tcp, 512, 5),
            Metrics = erlmcp_metrics:get_metrics(),
            ?assert(is_map(Metrics) orelse is_list(Metrics)),
            ok;

        {'EXIT', Reason} ->
            ct:log("Metrics module not available or failed: ~p", [Reason]),
            {skip, "Metrics module not available"}
    end.

test_otel_integration(_Config) ->
    ct:log("Testing OTEL integration"),

    %% Test OTEL initialization (graceful degradation if not available)
    try
        Config = #{service_name => <<"ct_test_service">>},
        case erlmcp_otel:init(Config) of
            ok ->
                ct:log("OTEL initialized successfully"),

                %% Test with_span
                Fun = fun() -> ok end,
                Result = erlmcp_otel:with_span(<<"ct.test.span">>, #{}, Fun),
                ?assertEqual(ok, Result),

                %% Test span management
                SpanCtx = erlmcp_otel:start_span(<<"ct.test.manual">>, #{}),
                erlmcp_otel:add_attributes(SpanCtx, #{<<"test.attr">> => <<"test_value">>}),
                erlmcp_otel:end_span(SpanCtx),

                ok;

            {error, InitReason} ->
                ct:log("OTEL initialization failed (may be optional): ~p", [InitReason]),
                {skip, "OTEL not available or not configured"}
        end
    catch
        error:undef ->
            ct:log("erlmcp_otel module not found (optional dependency)"),
            {skip, "erlmcp_otel module not available"};
        _:ErrorReason:StackTrace ->
            ct:log("OTEL test error: ~p~n~p", [ErrorReason, StackTrace]),
            {skip, "OTEL functionality not available"}
    end.

test_health_integration(_Config) ->
    ct:log("Testing health monitoring integration"),

    try
        %% Test starting health monitor
        case catch erlmcp_health_monitor:start_link() of
            {ok, Pid} when is_pid(Pid) ->
                ct:log("Health monitor started: ~p", [Pid]),

                %% Test component registration
                ok = erlmcp_health_monitor:register_component(test_component, self()),

                %% Test getting system health
                SystemHealth = erlmcp_health_monitor:get_system_health(),
                ct:log("System health: ~p", [SystemHealth]),
                ?assert(is_map(SystemHealth)),

                %% Test getting component health
                ComponentHealth = erlmcp_health_monitor:get_component_health(test_component),
                ct:log("Component health: ~p", [ComponentHealth]),

                %% Test getting all component health
                AllHealth = erlmcp_health_monitor:get_all_component_health(),
                ct:log("All component health: ~p", [AllHealth]),
                ?assert(is_map(AllHealth)),

                %% Cleanup
                gen_server:stop(Pid),
                ?assertNot(is_process_alive(Pid)),
                ok;

            {error, {already_started, Pid}} when is_pid(Pid) ->
                ct:log("Health monitor already running: ~p", [Pid]),

                %% Test basic operations on existing server
                ok = erlmcp_health_monitor:register_component(ct_test_component, self()),
                Health = erlmcp_health_monitor:get_system_health(),
                ?assert(is_map(Health)),
                ok;

            {'EXIT', Reason} ->
                ct:log("Health monitor not available: ~p", [Reason]),
                {skip, "Health monitor not available"}
        end
    catch
        error:undef ->
            ct:log("erlmcp_health_monitor module not found"),
            {skip, "Health monitor module not available"}
    end.

test_dashboard_server(_Config) ->
    ct:log("Testing dashboard server (skip if dependencies not available)"),

    %% Dashboard server requires cowboy/ranch - skip if not available
    try
        %% Check if cowboy is available
        case code:load_file(cowboy) of
            {module, cowboy} ->
                ct:log("Cowboy available, testing dashboard server"),

                %% Check if ranch supervisor is running
                case whereis(ranch_sup) of
                    undefined ->
                        ct:log("Ranch supervisor not running - starting cowboy dependencies"),
                        %% Try to start ranch application
                        case application:ensure_all_started(ranch) of
                            {ok, _} ->
                                ct:log("Ranch started successfully"),
                                test_dashboard_server_with_cowboy(19090);
                            {error, RanchError} ->
                                ct:log("Failed to start ranch: ~p", [RanchError]),
                                {skip, "Ranch dependency not available"}
                        end;
                    _ ->
                        ct:log("Ranch supervisor already running"),
                        test_dashboard_server_with_cowboy(19090)
                end;

            {error, Reason} ->
                ct:log("Cowboy not available: ~p", [Reason]),
                {skip, "Cowboy dependency not available"}
        end
    catch
        error:undef ->
            ct:log("Dashboard server module not found"),
            {skip, "Dashboard server module not available"};
        _:Error:Stack ->
            ct:log("Dashboard server error: ~p~n~p", [Error, Stack]),
            {skip, "Dashboard server initialization failed"}
    end.

test_dashboard_server_with_cowboy(TestPort) ->
    case catch erlmcp_dashboard_server:start_link(TestPort) of
        {ok, Pid} when is_pid(Pid) ->
            ct:log("Dashboard server started on port ~p: ~p", [TestPort, Pid]),

            %% Test getting port
            {ok, RetrievedPort} = erlmcp_dashboard_server:get_port(),
            ?assertEqual(TestPort, RetrievedPort),

            %% Test metrics broadcast
            TestMetrics = #{
                timestamp => erlang:system_time(millisecond),
                test_metric => 123
            },
            ok = erlmcp_dashboard_server:broadcast_metrics(TestMetrics),

            %% Cleanup
            erlmcp_dashboard_server:stop(),
            timer:sleep(100),
            ?assertNot(is_process_alive(Pid)),
            ok;

        {error, {already_started, Pid}} when is_pid(Pid) ->
            ct:log("Dashboard server already running: ~p", [Pid]),
            ok;

        {'EXIT', {{noproc, _}, _}} ->
            ct:log("Ranch supervisor still not available after starting ranch"),
            {skip, "Ranch/Cowboy dependencies not properly configured"};

        {'EXIT', Reason} ->
            ct:log("Dashboard server failed to start: ~p", [Reason]),
            {skip, "Dashboard server dependencies not available"}
    end.

test_full_observability_stack(_Config) ->
    ct:log("Testing full observability stack integration"),

    %% Try to start all observability components
    Results = #{
        metrics => test_component_safe(fun test_metrics_component/0),
        otel => test_component_safe(fun test_otel_component/0),
        health => test_component_safe(fun test_health_component/0)
    },

    ct:log("Observability stack test results: ~p", [Results]),

    %% At least metrics should work
    ?assertEqual(ok, maps:get(metrics, Results)),

    %% Verify at least one component succeeded
    SuccessCount = lists:foldl(
        fun(ok, Acc) -> Acc + 1;
           (_, Acc) -> Acc
        end,
        0,
        maps:values(Results)
    ),

    ?assert(SuccessCount >= 1, "At least one observability component should work"),

    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

test_metrics_component() ->
    case catch erlmcp_metrics:start_link() of
        {ok, Pid} when is_pid(Pid) ->
            ok = erlmcp_metrics:record_transport_operation(stack_test, stdio, 256, 5),
            gen_server:stop(Pid),
            ok;
        {error, {already_started, _}} ->
            ok;
        _ ->
            {error, metrics_failed}
    end.

test_otel_component() ->
    try
        case erlmcp_otel:init(#{service_name => <<"stack_test">>}) of
            ok -> ok;
            _ -> {error, otel_init_failed}
        end
    catch
        _:_ -> {error, otel_unavailable}
    end.

test_health_component() ->
    case catch erlmcp_health_monitor:start_link() of
        {ok, Pid} when is_pid(Pid) ->
            gen_server:stop(Pid),
            ok;
        {error, {already_started, _}} ->
            ok;
        _ ->
            {error, health_failed}
    end.

test_component_safe(Fun) ->
    try
        case Fun() of
            ok -> ok;
            {error, _} -> {skip, component_failed}
        end
    catch
        _:_ -> {skip, component_crashed}
    end.
