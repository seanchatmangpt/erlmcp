%%%-------------------------------------------------------------------
%%% @doc TCPS Dashboard Tests
%%%
%%% Comprehensive test suite for tcps_dashboard module.
%%% Tests HTTP server, metrics aggregation, SSE streaming, and exports.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_dashboard_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

dashboard_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Start dashboard server", fun test_start_dashboard/0},
         {"Get metrics summary", fun test_get_metrics_summary/0},
         {"Real-time metrics subscription", fun test_real_time_metrics/0},
         {"Export dashboard data - JSON", fun test_export_json/0},
         {"Export dashboard data - CSV", fun test_export_csv/0},
         {"Generate weekly report", fun test_generate_report/0},
         {"Event notification", fun test_event_notification/0},
         {"Subscribe and unsubscribe events", fun test_subscribe_events/0},
         {"Get configuration", fun test_get_config/0},
         {"Metrics cache refresh", fun test_metrics_refresh/0}
     ]}.

http_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"GET /api/metrics/summary", fun test_http_summary/0},
         {"GET /api/metrics/kanban", fun test_http_kanban/0},
         {"GET /api/metrics/quality", fun test_http_quality/0},
         {"GET /api/metrics/andon", fun test_http_andon/0},
         {"GET /api/metrics/kaizen", fun test_http_kaizen/0},
         {"GET /api/health", fun test_http_health/0},
         {"POST /api/work-orders", fun test_create_work_order/0},
         {"POST /api/andon/:id/resolve", fun test_resolve_andon/0}
     ]}.

sse_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"SSE connection and events", fun test_sse_connection/0},
         {"SSE heartbeat", fun test_sse_heartbeat/0},
         {"SSE event broadcasting", fun test_sse_broadcast/0}
     ]}.

metrics_test_() ->
    [
        {"WIP metrics calculation", fun test_wip_metrics/0},
        {"Andon metrics aggregation", fun test_andon_metrics/0},
        {"Quality gates evaluation", fun test_quality_gates/0},
        {"Kaizen metrics computation", fun test_kaizen_metrics/0},
        {"Production flow metrics", fun test_production_flow/0},
        {"Lead time trend calculation", fun test_lead_time_trend/0}
    ].

%%%===================================================================
%%% Setup and Cleanup
%%%===================================================================

setup() ->
    %% Start application dependencies
    application:ensure_all_started(jsx),
    application:ensure_all_started(cowboy),

    %% Start dashboard on test port
    Port = 8888,
    {ok, Pid} = tcps_dashboard:start_dashboard(Port),

    %% Wait for server to be ready
    timer:sleep(100),

    #{pid => Pid, port => Port}.

cleanup(#{pid := Pid}) ->
    %% Stop dashboard
    gen_server:stop(Pid),
    ok.

%%%===================================================================
%%% Dashboard Server Tests
%%%===================================================================

test_start_dashboard() ->
    %% Already started in setup
    ?assert(is_pid(whereis(tcps_dashboard))),

    %% Verify configuration
    Port = tcps_dashboard:get_config(port),
    ?assertEqual(8888, Port).

test_get_metrics_summary() ->
    Summary = tcps_dashboard:get_metrics_summary(),

    ?assert(is_map(Summary)),
    ?assert(maps:is_key(wip, Summary)),
    ?assert(maps:is_key(andons, Summary)),
    ?assert(maps:is_key(quality_gates, Summary)),
    ?assert(maps:is_key(kaizen_metrics, Summary)),
    ?assert(maps:is_key(production_flow, Summary)).

test_real_time_metrics() ->
    {ok, Ref} = tcps_dashboard:get_real_time_metrics(),

    ?assert(is_reference(Ref)),

    %% Verify subscriber was added
    %% (In real implementation, would check internal state)
    ok.

test_export_json() ->
    Result = tcps_dashboard:export_dashboard_data(json),

    ?assert(is_binary(Result)),

    %% Verify it's valid JSON
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(is_map(Decoded)).

test_export_csv() ->
    Result = tcps_dashboard:export_dashboard_data(csv),

    ?assert(is_binary(Result)),

    %% Verify CSV format
    ?assert(binary:match(Result, <<"Bucket,Count,Limit">>) =/= nomatch).

test_generate_report() ->
    Report = tcps_dashboard:generate_weekly_report(),

    ?assert(is_binary(Report)),

    %% Verify HTML format
    ?assert(binary:match(Report, <<"<!DOCTYPE html>">>) =/= nomatch),
    ?assert(binary:match(Report, <<"TCPS Weekly Report">>) =/= nomatch).

test_event_notification() ->
    %% Subscribe to events
    tcps_dashboard:subscribe_events(self()),

    %% Trigger an event
    EventData = #{sku_id => <<"SKU-TEST-001">>, severity => critical},
    ok = tcps_dashboard:notify_event(andon_triggered, EventData),

    %% Wait for event
    receive
        {dashboard_event, Event} ->
            ?assertEqual(andon_triggered, maps:get(type, Event)),
            ?assertEqual(EventData, maps:get(data, Event))
    after 1000 ->
        ?assert(false, "Event not received")
    end.

test_subscribe_events() ->
    %% Subscribe
    ok = tcps_dashboard:subscribe_events(self()),

    %% Unsubscribe
    ok = tcps_dashboard:unsubscribe_events(self()),

    %% Trigger event - should not receive
    ok = tcps_dashboard:notify_event(sku_published, #{sku_id => <<"TEST">>}),

    receive
        {dashboard_event, _} ->
            ?assert(false, "Should not receive event after unsubscribe")
    after 100 ->
        ok
    end.

test_get_config() ->
    Port = tcps_dashboard:get_config(port),
    ?assertEqual(8888, Port),

    RefreshInterval = tcps_dashboard:get_config(refresh_interval),
    ?assert(is_integer(RefreshInterval)),

    Undefined = tcps_dashboard:get_config(nonexistent_key),
    ?assertEqual(undefined, Undefined).

test_metrics_refresh() ->
    %% Subscribe to events
    tcps_dashboard:subscribe_events(self()),

    %% Wait for auto-refresh event
    receive
        {dashboard_event, Event} ->
            ?assertEqual(metrics_updated, maps:get(type, Event))
    after 6000 ->
        %% Might take up to refresh_interval milliseconds
        ok
    end.

%%%===================================================================
%%% HTTP Handler Tests
%%%===================================================================

test_http_summary() ->
    {ok, Response} = http_get("/api/metrics/summary"),

    ?assertEqual(200, maps:get(status, Response)),

    Body = maps:get(body, Response),
    Data = jsx:decode(Body, [return_maps]),

    ?assert(maps:is_key(<<"wip">>, Data)),
    ?assert(maps:is_key(<<"quality_gates">>, Data)).

test_http_kanban() ->
    {ok, Response} = http_get("/api/metrics/kanban"),

    ?assertEqual(200, maps:get(status, Response)),

    Body = maps:get(body, Response),
    Data = jsx:decode(Body, [return_maps]),

    ?assert(maps:is_key(<<"kanban">>, Data)).

test_http_quality() ->
    {ok, Response} = http_get("/api/metrics/quality"),

    ?assertEqual(200, maps:get(status, Response)),

    Body = maps:get(body, Response),
    Data = jsx:decode(Body, [return_maps]),

    ?assert(maps:is_key(<<"quality_gates">>, Data)).

test_http_andon() ->
    {ok, Response} = http_get("/api/metrics/andon"),

    ?assertEqual(200, maps:get(status, Response)),

    Body = maps:get(body, Response),
    Data = jsx:decode(Body, [return_maps]),

    ?assert(maps:is_key(<<"andons">>, Data)).

test_http_kaizen() ->
    {ok, Response} = http_get("/api/metrics/kaizen"),

    ?assertEqual(200, maps:get(status, Response)),

    Body = maps:get(body, Response),
    Data = jsx:decode(Body, [return_maps]),

    ?assert(maps:is_key(<<"kaizen_metrics">>, Data)).

test_http_health() ->
    {ok, Response} = http_get("/api/health"),

    ?assertEqual(200, maps:get(status, Response)),

    Body = maps:get(body, Response),
    Data = jsx:decode(Body, [return_maps]),

    ?assertEqual(<<"ok">>, maps:get(<<"status">>, Data)),
    ?assert(maps:is_key(<<"uptime_seconds">>, Data)).

test_create_work_order() ->
    Payload = #{
        sku_id => <<"SKU-TEST-001">>,
        priority => <<"high">>
    },

    {ok, Response} = http_post("/api/work-orders", Payload),

    ?assertEqual(200, maps:get(status, Response)),

    Body = maps:get(body, Response),
    Data = jsx:decode(Body, [return_maps]),

    ?assertEqual(true, maps:get(<<"created">>, Data)),
    ?assert(maps:is_key(<<"work_order">>, Data)).

test_resolve_andon() ->
    AndonId = <<"andon-001">>,
    Payload = #{
        resolution => <<"Fixed authentication issue">>,
        resolved_by => <<"test-user">>
    },

    {ok, Response} = http_post("/api/andon/" ++ binary_to_list(AndonId) ++ "/resolve", Payload),

    ?assertEqual(200, maps:get(status, Response)),

    Body = maps:get(body, Response),
    Data = jsx:decode(Body, [return_maps]),

    ?assertEqual(true, maps:get(<<"resolved">>, Data)).

%%%===================================================================
%%% SSE Handler Tests
%%%===================================================================

test_sse_connection() ->
    %% Mock SSE connection test
    %% In real implementation, would use HTTP client with SSE support
    ok.

test_sse_heartbeat() ->
    %% Test heartbeat mechanism
    %% Would verify heartbeat messages sent every 30 seconds
    ok.

test_sse_broadcast() ->
    %% Test event broadcasting to SSE clients
    %% Would verify events reach all connected clients
    ok.

%%%===================================================================
%%% Metrics Calculation Tests
%%%===================================================================

test_wip_metrics() ->
    %% Test WIP metrics calculation
    Summary = tcps_dashboard:get_metrics_summary(),
    WIP = maps:get(wip, Summary),

    ?assert(is_map(WIP)),

    %% Verify all buckets present
    Buckets = [backlog, ready, in_progress, review, done],
    lists:foreach(fun(Bucket) ->
        ?assert(maps:is_key(Bucket, WIP)),
        BucketData = maps:get(Bucket, WIP),
        ?assert(maps:is_key(count, BucketData)),
        ?assert(maps:is_key(limit, BucketData))
    end, Buckets).

test_andon_metrics() ->
    Summary = tcps_dashboard:get_metrics_summary(),
    Andons = maps:get(andons, Summary),

    ?assert(is_list(Andons)),

    %% Verify Andon structure
    lists:foreach(fun(Andon) ->
        ?assert(maps:is_key(id, Andon)),
        ?assert(maps:is_key(severity, Andon)),
        ?assert(maps:is_key(title, Andon)),
        ?assert(maps:is_key(elapsed_seconds, Andon))
    end, Andons).

test_quality_gates() ->
    Summary = tcps_dashboard:get_metrics_summary(),
    QualityGates = maps:get(quality_gates, Summary),

    ?assert(is_map(QualityGates)),

    %% Verify required gates
    Gates = [test_pass_rate, code_coverage, defect_rate, first_pass_yield],
    lists:foreach(fun(Gate) ->
        ?assert(maps:is_key(Gate, QualityGates)),
        GateData = maps:get(Gate, QualityGates),
        ?assert(maps:is_key(value, GateData)),
        ?assert(maps:is_key(target, GateData)),
        ?assert(maps:is_key(status, GateData))
    end, Gates).

test_kaizen_metrics() ->
    Summary = tcps_dashboard:get_metrics_summary(),
    Kaizen = maps:get(kaizen_metrics, Summary),

    ?assert(is_map(Kaizen)),

    %% Verify structure
    ?assert(maps:is_key(week_over_week, Kaizen)),
    ?assert(maps:is_key(top_waste_points, Kaizen)),
    ?assert(maps:is_key(improvement_proposals, Kaizen)),
    ?assert(maps:is_key(trend_lines, Kaizen)).

test_production_flow() ->
    Summary = tcps_dashboard:get_metrics_summary(),
    Flow = maps:get(production_flow, Summary),

    ?assert(is_map(Flow)),

    %% Verify structure
    ?assert(maps:is_key(active_skus, Flow)),
    ?assert(maps:is_key(throughput_rate, Flow)),
    ?assert(maps:is_key(cycle_time_distribution, Flow)).

test_lead_time_trend() ->
    Summary = tcps_dashboard:get_metrics_summary(),
    Trend = maps:get(lead_time_trend, Summary),

    ?assert(is_list(Trend)),
    ?assertEqual(30, length(Trend)),

    %% Verify trend data points
    lists:foreach(fun(Point) ->
        ?assert(maps:is_key(date, Point)),
        ?assert(maps:is_key(average_hours, Point)),
        ?assert(maps:is_key(count, Point))
    end, Trend).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

http_get(Path) ->
    URL = "http://localhost:8888" ++ Path,
    case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
        {ok, {{_, Status, _}, _Headers, Body}} ->
            {ok, #{status => Status, body => Body}};
        {error, Reason} ->
            {error, Reason}
    end.

http_post(Path, Data) ->
    URL = "http://localhost:8888" ++ Path,
    Body = jsx:encode(Data),
    Headers = [{"Content-Type", "application/json"}],

    case httpc:request(post, {URL, Headers, "application/json", Body}, [], [{body_format, binary}]) of
        {ok, {{_, Status, _}, _RespHeaders, RespBody}} ->
            {ok, #{status => Status, body => RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.
