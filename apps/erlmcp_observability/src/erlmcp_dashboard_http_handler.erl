%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_dashboard_http_handler - HTTP REST API Handler
%%%
%%% Provides REST API endpoints for metrics queries:
%%%   GET /api/metrics - Current metrics
%%%   GET /api/metrics/historical?start=X&end=Y - Historical metrics
%%%   GET /api/metrics/export?format=csv|json - Export metrics
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

    StartTime = case proplists:get_value(<<"start">>, QsVals) of
        undefined -> erlang:system_time(millisecond) - 3600000; % Default: 1 hour ago
        StartBin -> binary_to_integer(StartBin)
    end,

    EndTime = case proplists:get_value(<<"end">>, QsVals) of
        undefined -> erlang:system_time(millisecond); % Default: now
        EndBin -> binary_to_integer(EndBin)
    end,

    case erlmcp_metrics_aggregator:get_historical_metrics(StartTime, EndTime) of
        {ok, Metrics} ->
            respond_json(Req, 200, #{
                start_time => StartTime,
                end_time => EndTime,
                metrics => Metrics
            });
        {error, Reason} ->
            ?LOG_ERROR("Failed to get historical metrics: ~p", [Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Handle GET /api/metrics/export
-spec handle_export_metrics(cowboy_req:req()) -> cowboy_req:req().
handle_export_metrics(Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    Format = case proplists:get_value(<<"format">>, QsVals) of
        <<"csv">> -> csv;
        _ -> json
    end,

    case erlmcp_metrics_aggregator:export_metrics(Format) of
        {ok, Data} ->
            ContentType = case Format of
                csv -> <<"text/csv">>;
                json -> <<"application/json">>
            end,
            cowboy_req:reply(200, #{
                <<"content-type">> => ContentType,
                <<"content-disposition">> => <<"attachment; filename=\"metrics.", (atom_to_binary(Format))/binary, "\"">>
            }, Data, Req);
        {error, Reason} ->
            ?LOG_ERROR("Failed to export metrics: ~p", [Reason]),
            respond_json(Req, 500, #{error => <<"Internal server error">>})
    end.

%% @doc Send JSON response
-spec respond_json(cowboy_req:req(), pos_integer(), map()) -> cowboy_req:req().
respond_json(Req, StatusCode, Body) ->
    cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(Body), Req).
