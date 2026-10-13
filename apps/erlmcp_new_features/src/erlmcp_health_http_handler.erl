-module(erlmcp_health_http_handler).

%% HTTP Health Check Handler for erlmcp v3.0.0
%%
%% Provides HTTP endpoints for health checks using inets httpd mod_esi.
%%
%% Endpoints:
%%   GET /health         - Basic health (all checks OK)
%%   GET /health/ready   - Readiness probe (can accept traffic)
%%   GET /health/live    - Liveness probe (process is running)
%%   GET /health/detailed - Detailed health report

%% mod_esi Callbacks
-export([do/1]).
-export([session_id/1]).

-define(HEALTH_CHECK_TIMEOUT, 5000).

%%%===================================================================
%%% Types
%%%===================================================================

-type esi_request() :: {proplists:proplist(), string()}.
-type esi_response() :: iolist().

%%%===================================================================
%%% mod_esi Callbacks
%%%===================================================================

%% SessionID for mod_esi compatibility
-spec session_id(term()) -> ok.
session_id(_SessionID) ->
    ok.

%% Main request handler
-spec do(esi_request()) -> esi_response().
do(Req) ->
    case parse_request(Req) of
        {Method, Path, _QueryString} when Method =:= "GET" orelse Method =:= "HEAD" ->
            handle_request(Method, Path);
        _ ->
            send_response(405, <<"Method Not Allowed">>, <<"">>)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_request(esi_request()) -> {string(), string(), string()}.
parse_request(Req) ->
    % Parse inets mod_esi request format
    % Req format: {Env, Input}
    {Env, _Input} = Req,
    Method = proplists:get_value(request_method, Env, "GET"),
    Path = proplists:get_value(path_info, Env, "/"),
    QueryString = proplists:get_value(query_string, Env, ""),
    {Method, Path, QueryString}.

-spec handle_request(Method :: string(), Path :: string()) -> esi_response().
handle_request(Method, Path) ->
    Body = case Path of
        "/" ->
            handle_root();
        "/health" ->
            handle_health();
        "/health/ready" ->
            handle_ready();
        "/health/live" ->
            handle_live();
        "/health/detailed" ->
            handle_detailed();
        _ ->
            send_response(404, <<"Not Found">>, jsx:encode(#{error => <<"Unknown path">>, path => list_to_binary(Path)}))
    end,
    Body.

%% Root endpoint - returns version info
-spec handle_root() -> esi_response().
handle_root() ->
    Response = #{
        service => <<"erlmcp">>,
        version => <<"3.0.0">>,
        status => <<"ok">>,
        endpoints => #{
            health => <<"/health">>,
            ready => <<"/health/ready">>,
            live => <<"/health/live">>,
            detailed => <<"/health/detailed">>
        }
    },
    send_response(200, <<"OK">>, jsx:encode(Response)).

%% Basic health check - all checks must pass
-spec handle_health() -> esi_response().
handle_health() ->
    case erlmcp_health_check:health_check() of
        #{status := ok} = HealthMap ->
            Response = format_health_response(ok, HealthMap),
            send_response(200, <<"OK">>, jsx:encode(Response));
        #{status := warning} = HealthMap ->
            Response = format_health_response(warning, HealthMap),
            send_response(503, <<"Service Unavailable">>, jsx:encode(Response));
        #{status := error} = HealthMap ->
            Response = format_health_response(error, HealthMap),
            send_response(503, <<"Service Unavailable">>, jsx:encode(Response))
    end.

%% Readiness probe - can we accept traffic?
-spec handle_ready() -> esi_response().
handle_ready() ->
    ReadyStatus = case erlmcp_health_http:get_readiness_status() of
        #{ready := true} -> ready;
        _ -> not_ready
    end,
    Response = #{
        ready => ReadyStatus =:= ready,
        status => ReadyStatus,
        timestamp => timestamp_to_iso8601(erlang:timestamp())
    },
    StatusCode = case ReadyStatus of
        ready -> 200;
        not_ready -> 503
    end,
    send_response(StatusCode, <<"OK">>, jsx:encode(Response)).

%% Liveness probe - is the process alive?
-spec handle_live() -> esi_response().
handle_live() ->
    AliveStatus = case erlmcp_health_http:get_liveness_status() of
        #{alive := true} -> alive;
        _ -> dead
    end,
    Response = #{
        alive => AliveStatus =:= alive,
        status => AliveStatus,
        timestamp => timestamp_to_iso8601(erlang:timestamp())
    },
    StatusCode = case AliveStatus of
        alive -> 200;
        dead -> 503
    end,
    send_response(StatusCode, <<"OK">>, jsx:encode(Response)).

%% Detailed health report
-spec handle_detailed() -> esi_response().
handle_detailed() ->
    case erlmcp_health_check:detailed_health_check() of
        #{status := Status} = DetailedMap ->
            Response = format_detailed_response(Status, DetailedMap),
            StatusCode = case Status of
                ok -> 200;
                _ -> 503
            end,
            send_response(StatusCode, <<"OK">>, jsx:encode(Response))
    end.

-spec format_health_response(atom(), map()) -> map().
format_health_response(Status, HealthMap) ->
    HealthMap#{
        timestamp => timestamp_to_iso8601(erlang:timestamp()),
        uptime => erlang:monotonic_time(millisecond)
    }.

-spec format_detailed_response(atom(), map()) -> map().
format_detailed_response(Status, DetailedMap) ->
    Checks = maps:get(checks, DetailedMap, #{}),
    FormattedChecks = lists:map(fun(CheckMap) ->
        Name = maps:get(name, CheckMap, unknown),
        CheckStatus = maps:get(status, CheckMap, unknown),
        Message = maps:get(message, CheckMap, <<"">>),
        Duration = maps:get(duration_ms, CheckMap, 0),
        #{
            name => Name,
            status => CheckStatus,
            message => Message,
            duration_ms => Duration
        }
    end, Checks),
    DetailedMap#{
        timestamp => timestamp_to_iso8601(erlang:timestamp()),
        checks => FormattedChecks
    }.

-spec send_response(pos_integer(), binary(), binary() | string()) -> iolist().
send_response(StatusCode, StatusText, Body) when is_binary(Body) ->
    send_response(StatusCode, StatusText, binary_to_list(Body));

send_response(StatusCode, StatusText, Body) when is_list(Body) ->
    % inets mod_esi response format
    [
        integer_to_list(StatusCode), " ", StatusText, "\r\n",
        "Content-Type: application/json\r\n",
        "Cache-Control: no-cache\r\n",
        "\r\n",
        Body
    ].

-spec timestamp_to_iso8601(erlang:timestamp()) -> binary().
timestamp_to_iso8601({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Min, SecVal}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    MicroSecPart = integer_to_binary(MicroSecs div 1000),
    iolist_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~sZ",
                                   [Year, Month, Day, Hour, Min, SecVal, MicroSecPart])).
