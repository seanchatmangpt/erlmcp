-module(erlmcp_metrics_http).

%% Cowboy HTTP handler for metrics endpoint
-export([init/2, allowed_methods/2, content_types_provided/2, to_json/2]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Cowboy HTTP Handler
%%====================================================================

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State) ->
    Metrics = erlmcp_metrics_server:get_metrics(),
    Json = jsx:encode(metrics_to_json(Metrics)),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    Req3 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-cache, no-store">>, Req2),
    {Json, Req3, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec metrics_to_json(map()) -> map().
metrics_to_json(Metrics) ->
    maps:map(fun encode_metric_value/2, Metrics).

-spec encode_metric_value(atom() | binary(), term()) -> term().

encode_metric_value(uptime_human, Value) when is_binary(Value) ->
    Value;

encode_metric_value(latency_stats, LatencyStats) when is_map(LatencyStats) ->
    LatencyStats;

encode_metric_value(nodes, Nodes) when is_list(Nodes) ->
    Nodes;

encode_metric_value(system_metrics, Metrics) when is_map(Metrics) ->
    maps:map(fun encode_metric_value/2, Metrics);

encode_metric_value(memory, Memory) when is_map(Memory) ->
    Memory;

encode_metric_value(_Key, Value) ->
    Value.
