%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_dashboard_http_handler - HTTP REST API Handler
%%%
%%% Provides REST API endpoints for metrics and introspection:
%%%   GET /api/metrics - Current metrics
%%%   GET /api/metrics/historical?start=X&end=Y - Historical metrics
%%%   GET /api/metrics/export?format=csv|json - Export metrics
%%%   GET /api/introspect/status - System status
%%%   GET /api/introspect/session/:id - Session dump
%%%   GET /api/introspect/streams/:id - Stream info for session
%%%   GET /api/introspect/tasks - Task status
%%%   GET /api/introspect/queues - Queue depths
%%%   GET /api/introspect/health - Health check
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_dashboard_http_handler).

%% Cowboy callbacks
-export([init/2]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Cowboy Callbacks
%%====================================================================

%% @doc Handle HTTP request
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    Req = case {Method, Path} of
              {<<"GET">>, <<"/api/metrics">>} ->
                  handle_get_metrics(Req0);
              {<<"GET">>, <<"/api/metrics/historical">>} ->
                  handle_get_historical(Req0);
              {<<"GET">>, <<"/api/metrics/export">>} ->
                  handle_export_metrics(Req0);
              {<<"GET">>, <<"/api/introspect/status">>} ->
                  handle_introspect_status(Req0);
              {<<"GET">>, <<"/api/introspect/tasks">>} ->
                  handle_introspect_tasks(Req0);
              {<<"GET">>, <<"/api/introspect/queues">>} ->
                  handle_introspect_queues(Req0);
              {<<"GET">>, <<"/api/introspect/health">>} ->
                  handle_introspect_health(Req0);
              {<<"GET">>, _} ->
                  %% Check for parameterized routes
                  handle_parameterized_route(Method, Path, Req0);
              _ ->
                  respond_json(Req0, 404, #{error => <<"Not found">>})
          end,

    {ok, Req, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Handle GET /api/metrics
-spec handle_get_metrics(cowboy_req:req()) -> cowboy_req:req().
handle_get_metrics(Req) ->
    case erlmcp_metrics_aggregator:get_current_metrics() of
        {ok, Metrics} ->
            respond_json(Req, 200, Metrics);
        {error, Reason} ->
            ?LOG_ERROR("Failed to get metrics: ~p", [Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Handle GET /api/metrics/historical
-spec handle_get_historical(cowboy_req:req()) -> cowboy_req:req().
handle_get_historical(Req) ->
    QsVals = cowboy_req:parse_qs(Req),

    StartTime =
        case proplists:get_value(<<"start">>, QsVals) of
            undefined ->
                erlang:system_time(millisecond) - 3600000; % Default: 1 hour ago
            StartBin ->
                binary_to_integer(StartBin)
        end,

    EndTime =
        case proplists:get_value(<<"end">>, QsVals) of
            undefined ->
                erlang:system_time(millisecond); % Default: now
            EndBin ->
                binary_to_integer(EndBin)
        end,

    case erlmcp_metrics_aggregator:get_historical_metrics(StartTime, EndTime) of
        {ok, Metrics} ->
            respond_json(Req,
                         200,
                         #{start_time => StartTime,
                           end_time => EndTime,
                           metrics => Metrics});
        {error, Reason} ->
            ?LOG_ERROR("Failed to get historical metrics: ~p", [Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Handle GET /api/metrics/export
-spec handle_export_metrics(cowboy_req:req()) -> cowboy_req:req().
handle_export_metrics(Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    Format =
        case proplists:get_value(<<"format">>, QsVals) of
            <<"csv">> ->
                csv;
            _ ->
                json
        end,

    case erlmcp_metrics_aggregator:export_metrics(Format) of
        {ok, Data} ->
            ContentType =
                case Format of
                    csv ->
                        <<"text/csv">>;
                    json ->
                        <<"application/json">>
                end,
            cowboy_req:reply(200,
                             #{<<"content-type">> => ContentType,
                               <<"content-disposition">> =>
                                   <<"attachment; filename=\"metrics.",
                                     (atom_to_binary(Format))/binary,
                                     "\"">>},
                             Data,
                             Req);
        {error, Reason} ->
            ?LOG_ERROR("Failed to export metrics: ~p", [Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Handle GET /api/introspect/status
-spec handle_introspect_status(cowboy_req:req()) -> cowboy_req:req().
handle_introspect_status(Req) ->
    try
        Status = erlmcp_introspect:status(),
        respond_json(Req, 200, Status)
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get introspection status: ~p:~p", [Error, Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Handle GET /api/introspect/tasks
-spec handle_introspect_tasks(cowboy_req:req()) -> cowboy_req:req().
handle_introspect_tasks(Req) ->
    try
        Tasks = erlmcp_introspect:tasks(),
        respond_json(Req, 200, Tasks)
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get tasks: ~p:~p", [Error, Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Handle GET /api/introspect/queues
-spec handle_introspect_queues(cowboy_req:req()) -> cowboy_req:req().
handle_introspect_queues(Req) ->
    try
        Queues = erlmcp_introspect:queues(),
        respond_json(Req, 200, Queues)
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get queues: ~p:~p", [Error, Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Handle GET /api/introspect/health
-spec handle_introspect_health(cowboy_req:req()) -> cowboy_req:req().
handle_introspect_health(Req) ->
    try
        {HealthStatus, Metrics} = erlmcp_introspect:health_check(),
        Response = Metrics#{status => HealthStatus},
        StatusCode =
            case HealthStatus of
                healthy ->
                    200;
                degraded ->
                    200;
                critical ->
                    503
            end,
        respond_json(Req, StatusCode, Response)
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to run health check: ~p:~p", [Error, Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Handle parameterized routes like /api/introspect/session/:id
-spec handle_parameterized_route(binary(), binary(), cowboy_req:req()) -> cowboy_req:req().
handle_parameterized_route(<<"GET">>, Path, Req) ->
    case binary:split(Path, <<"/">>, [global]) of
        [<<>>, <<"api">>, <<"introspect">>, <<"session">>, SessionId] when SessionId =/= <<>> ->
            handle_introspect_session(SessionId, Req);
        [<<>>, <<"api">>, <<"introspect">>, <<"streams">>, SessionId] when SessionId =/= <<>> ->
            handle_introspect_streams(SessionId, Req);
        _ ->
            respond_json(Req, 404, #{error => <<"Not found">>})
    end;
handle_parameterized_route(_, _, Req) ->
    respond_json(Req, 404, #{error => <<"Not found">>}).

%% @doc Handle GET /api/introspect/session/:id
-spec handle_introspect_session(binary(), cowboy_req:req()) -> cowboy_req:req().
handle_introspect_session(SessionId, Req) ->
    try
        case erlmcp_introspect:session_dump(SessionId) of
            {ok, SessionData} ->
                respond_json(Req, 200, SessionData);
            {error, not_found} ->
                respond_json(Req, 404, #{error => <<"Session not found">>});
            {error, Reason} ->
                ?LOG_ERROR("Failed to get session dump: ~p", [Reason]),
                respond_json(Req, 500, #{error => <<"Internal server error">>})
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get session dump: ~p:~p", [Error, Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Handle GET /api/introspect/streams/:id
-spec handle_introspect_streams(binary(), cowboy_req:req()) -> cowboy_req:req().
handle_introspect_streams(SessionId, Req) ->
    try
        case erlmcp_introspect:streams(SessionId) of
            {ok, Streams} ->
                respond_json(Req, 200, #{streams => Streams});
            {error, not_found} ->
                respond_json(Req, 404, #{error => <<"Session not found">>});
            {error, Reason} ->
                ?LOG_ERROR("Failed to get streams: ~p", [Reason]),
                respond_json(Req, 500, #{error => <<"Internal server error">>})
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get streams: ~p:~p", [Error, Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Send JSON response
-spec respond_json(cowboy_req:req(), pos_integer(), map()) -> cowboy_req:req().
respond_json(Req, StatusCode, Body) ->
    cowboy_req:reply(StatusCode,
                     #{<<"content-type">> => <<"application/json">>},
                     jsx:encode(Body),
                     Req).
