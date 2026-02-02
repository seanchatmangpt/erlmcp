%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Health Check and Monitoring Module
%%%
%%% Provides health monitoring capabilities for all transport types.
%%% Includes connection state validation, performance metrics,
%%% and automated recovery mechanisms.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_health).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, check_health/1, check_health/2, get_health_status/1,
         register_transport/3, unregister_transport/1, update_metrics/3, trigger_health_check/1,
         reset_metrics/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_CHECK_INTERVAL, 30000). % 30 seconds
-define(HEALTH_CHECK_TIMEOUT, 5000). % 5 seconds
-define(METRICS_RETENTION_PERIOD, 300000). % 5 minutes
-define(MAX_METRICS_HISTORY, 100).

-type transport_id() :: atom().
-type health_status() :: healthy | degraded | unhealthy | unknown.
-type health_metrics() ::
    #{timestamp => integer(),
      connection_status => up | down | unknown,
      latency_ms => number() | undefined,
      error_rate => float(),
      throughput => number(),
      last_error => term() | undefined}.
-type transport_health() ::
    #{transport_id => transport_id(),
      status => health_status(),
      metrics => health_metrics(),
      last_check => integer(),
      consecutive_failures => non_neg_integer(),
      last_healthy => integer() | undefined}.

-record(state,
        {check_interval = ?DEFAULT_CHECK_INTERVAL :: pos_integer(),
         health_checks = #{} :: #{transport_id() => transport_health()},
         check_timer :: reference() | undefined,
         metrics_history = #{} :: #{transport_id() => [health_metrics()]}}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start health monitor with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start health monitor with custom options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Check health of a specific transport
-spec check_health(transport_id()) -> {ok, health_status()} | {error, term()}.
check_health(TransportId) ->
    check_health(TransportId, ?HEALTH_CHECK_TIMEOUT).

%% @doc Check health of a specific transport with custom timeout
-spec check_health(transport_id(), timeout()) -> {ok, health_status()} | {error, term()}.
check_health(TransportId, Timeout) ->
    gen_server:call(?MODULE, {check_health, TransportId}, Timeout).

%% @doc Get current health status for a transport
-spec get_health_status(transport_id()) -> {ok, transport_health()} | {error, term()}.
get_health_status(TransportId) ->
    gen_server:call(?MODULE, {get_health_status, TransportId}, 5000).

%% @doc Register a transport for health monitoring
-spec register_transport(transport_id(), pid(), map()) -> ok | {error, term()}.
register_transport(TransportId, Pid, Config) ->
    gen_server:call(?MODULE, {register_transport, TransportId, Pid, Config}).

%% @doc Unregister a transport from health monitoring
-spec unregister_transport(transport_id()) -> ok.
unregister_transport(TransportId) ->
    gen_server:call(?MODULE, {unregister_transport, TransportId}).

%% @doc Update transport metrics
-spec update_metrics(transport_id(), atom(), number()) -> ok.
update_metrics(TransportId, MetricName, Value) ->
    gen_server:cast(?MODULE, {update_metrics, TransportId, MetricName, Value}).

%% @doc Trigger immediate health check for a transport
-spec trigger_health_check(transport_id()) -> ok.
trigger_health_check(TransportId) ->
    gen_server:cast(?MODULE, {trigger_health_check, TransportId}).

%% @doc Reset metrics for a transport
-spec reset_metrics(transport_id()) -> ok.
reset_metrics(TransportId) ->
    gen_server:cast(?MODULE, {reset_metrics, TransportId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    CheckInterval = maps:get(check_interval, Opts, ?DEFAULT_CHECK_INTERVAL),
    Timer = erlang:send_after(CheckInterval, self(), scheduled_check),
    {ok, #state{check_interval = CheckInterval, check_timer = Timer}}.

handle_call({check_health, TransportId}, _From, State) ->
    Result = perform_health_check(TransportId, State),
    {reply, Result, State};
handle_call({get_health_status, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.health_checks, undefined) of
        undefined ->
            {reply, {error, not_registered}, State};
        Health ->
            {reply, {ok, Health}, State}
    end;
handle_call({register_transport, TransportId, Pid, Config}, _From, State) ->
    InitialHealth =
        #{transport_id => TransportId,
          status => unknown,
          metrics => initial_metrics(),
          last_check => erlang:system_time(millisecond),
          consecutive_failures => 0,
          last_healthy => undefined},
    NewHealthChecks = maps:put(TransportId, InitialHealth, State#state.health_checks),
    MonitorRef = monitor(process, Pid),
    ?LOG_INFO("Registered transport ~p for health monitoring", [TransportId]),
    {reply, ok, State#state{health_checks = NewHealthChecks}};
handle_call({unregister_transport, TransportId}, _From, State) ->
    NewHealthChecks = maps:remove(TransportId, State#state.health_checks),
    NewHistory = maps:remove(TransportId, State#state.metrics_history),
    ?LOG_INFO("Unregistered transport ~p from health monitoring", [TransportId]),
    {reply, ok, State#state{health_checks = NewHealthChecks, metrics_history = NewHistory}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_metrics, TransportId, MetricName, Value}, State) ->
    NewState =
        case maps:get(TransportId, State#state.health_checks, undefined) of
            undefined ->
                State;
            _Health ->
                update_transport_metrics(TransportId, MetricName, Value, State)
        end,
    {noreply, NewState};
handle_cast({trigger_health_check, TransportId}, State) ->
    NewState =
        case maps:get(TransportId, State#state.health_checks, undefined) of
            undefined ->
                State;
            _Health ->
                {ok, _Status} = perform_health_check(TransportId, State),
                State
        end,
    {noreply, NewState};
handle_cast({reset_metrics, TransportId}, State) ->
    NewState =
        case maps:get(TransportId, State#state.health_checks, undefined) of
            undefined ->
                State;
            _Health ->
                reset_transport_metrics(TransportId, State)
        end,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(scheduled_check, State) ->
    % Perform health checks on all registered transports
    NewState = perform_all_health_checks(State),
    Timer = erlang:send_after(State#state.check_interval, self(), scheduled_check),
    {noreply, NewState#state{check_timer = Timer}};
handle_info({'DOWN', _MonitorRef, process, _Pid, _Reason}, State) ->
    % Handle process death - transport died
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{check_timer = Timer}) ->
    case Timer of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(Timer)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Perform health check on a specific transport
-spec perform_health_check(transport_id(), #state{}) -> {ok, health_status()} | {error, term()}.
perform_health_check(TransportId, State) ->
    case maps:get(TransportId, State#state.health_checks, undefined) of
        undefined ->
            {error, not_registered};
        Health ->
            % Check if transport process is alive
            case whereis(TransportId) of
                undefined ->
                    {ok, unhealthy};
                _Pid ->
                    % Perform transport-specific health check
                    check_transport_status(TransportId, Health)
            end
    end.

%% @doc Check transport-specific status
-spec check_transport_status(transport_id(), transport_health()) -> {ok, health_status()}.
check_transport_status(TransportId, _Health) ->
    % Determine transport type and perform appropriate check
    TransportType = get_transport_type(TransportId),
    Result =
        case TransportType of
            tcp ->
                check_tcp_transport(TransportId);
            stdio ->
                check_stdio_transport(TransportId);
            sse ->
                check_sse_transport(TransportId);
            http ->
                check_http_transport(TransportId);
            websocket ->
                check_ws_transport(TransportId);
            _ ->
                {ok, #{status => unknown}}
        end,

    % Convert result to health_status
    case Result of
        {ok, #{status := Status}} ->
            {ok, Status};
        {ok, Status} when is_atom(Status) ->
            {ok, Status};
        {error, _Reason} ->
            {ok, unhealthy}
    end.

%% @doc Get transport type from transport ID
-spec get_transport_type(transport_id()) -> atom().
get_transport_type(TransportId) ->
    % First try to get type from registry
    case erlmcp_transport_registry:get_transport(TransportId) of
        {ok, TransportInfo} ->
            maps:get(type, TransportInfo, unknown);
        {error, _} ->
            % Fallback to name-based detection
            get_transport_type_from_name(TransportId)
    end.

%% @doc Get transport type from transport ID name (fallback)
-spec get_transport_type_from_name(transport_id()) -> atom().
get_transport_type_from_name(TransportId) ->
    TransportIdStr = atom_to_list(TransportId),
    case string:find(TransportIdStr, "ws") of
        nomatch ->
            case string:find(TransportIdStr, "http") of
                nomatch ->
                    case string:find(TransportIdStr, "tcp") of
                        nomatch ->
                            case string:find(TransportIdStr, "stdio") of
                                nomatch ->
                                    case string:find(TransportIdStr, "sse") of
                                        nomatch ->
                                            unknown;
                                        _ ->
                                            sse
                                    end;
                                _ ->
                                    stdio
                            end;
                        _ ->
                            tcp
                    end;
                _ ->
                    http
            end;
        _ ->
            websocket
    end.

%% @doc Check TCP transport health
-spec check_tcp_transport(transport_id()) -> {ok, health_status()} | {error, term()}.
check_tcp_transport(TransportId) ->
    case erlmcp_transport_registry:get_transport(TransportId) of
        {ok, TransportInfo} ->
            Config = maps:get(config, TransportInfo, #{}),
            Host = maps:get(host, Config, "localhost"),
            Port = maps:get(port, Config, undefined),
            case Port of
                undefined ->
                    {error, no_port_configured};
                _ ->
                    % Attempt connection to verify endpoint is reachable
                    StartTime = erlang:monotonic_time(microsecond),
                    case gen_tcp:connect(Host,
                                         Port,
                                         [binary, {active, false}],
                                         ?HEALTH_CHECK_TIMEOUT)
                    of
                        {ok, Socket} ->
                            EndTime = erlang:monotonic_time(microsecond),
                            LatencyUs = EndTime - StartTime,
                            LatencyMs = LatencyUs / 1000,
                            gen_tcp:close(Socket),
                            {ok, #{status => healthy, latency_ms => LatencyMs}};
                        {error, Reason} ->
                            {error, {tcp_connect_failed, Reason}}
                    end
            end;
        {error, not_found} ->
            {error, transport_not_registered}
    end.

%% @doc Check stdio transport health
-spec check_stdio_transport(transport_id()) -> {ok, health_status()} | {error, term()}.
check_stdio_transport(TransportId) ->
    % For stdio, check that file descriptors are accessible
    % We can verify stdout is writable by checking standard_io process
    try
        % Check if standard_io process is alive (manages stdin/stdout)
        case whereis(standard_io) of
            undefined ->
                {error, standard_io_not_available};
            StdIoPid when is_pid(StdIoPid) ->
                % Verify the process is responsive
                case is_process_alive(StdIoPid) of
                    true ->
                        % Try a simple io operation to verify stdout is writable
                        % We use io:format with a null device to avoid side effects
                        case catch io:format("") of
                            ok ->
                                {ok, #{status => healthy}};
                            _ ->
                                {error, stdout_not_writable}
                        end;
                    false ->
                        {error, standard_io_dead}
                end
        end
    catch
        _:Error ->
            {error, {stdio_check_failed, Error}}
    end.

%% @doc Check SSE transport health
-spec check_sse_transport(transport_id()) -> {ok, health_status()} | {error, term()}.
check_sse_transport(TransportId) ->
    case erlmcp_transport_registry:get_transport(TransportId) of
        {ok, TransportInfo} ->
            Config = maps:get(config, TransportInfo, #{}),
            % Build URL from config
            Host = maps:get(host, Config, "localhost"),
            Port = maps:get(port, Config, 8080),
            Path = maps:get(path, Config, "/mcp/sse"),
            Scheme =
                case maps:get(ssl, Config, false) of
                    true ->
                        "https";
                    false ->
                        "http"
                end,
            URL = lists:flatten(
                      io_lib:format("~s://~s:~p~s", [Scheme, Host, Port, Path])),
            % Make HTTP HEAD request to verify endpoint is responding
            check_http_endpoint(URL);
        {error, not_found} ->
            {error, transport_not_registered}
    end.

%% @doc Check HTTP transport health
-spec check_http_transport(transport_id()) -> {ok, health_status()} | {error, term()}.
check_http_transport(TransportId) ->
    case erlmcp_transport_registry:get_transport(TransportId) of
        {ok, TransportInfo} ->
            Config = maps:get(config, TransportInfo, #{}),
            % Build URL from config
            Host = maps:get(host, Config, "localhost"),
            Port = maps:get(port, Config, 8080),
            Path = maps:get(path, Config, "/mcp"),
            Scheme =
                case maps:get(ssl, Config, false) of
                    true ->
                        "https";
                    false ->
                        "http"
                end,
            URL = lists:flatten(
                      io_lib:format("~s://~s:~p~s", [Scheme, Host, Port, Path])),
            % Make HTTP HEAD request to verify endpoint is responding
            check_http_endpoint(URL);
        {error, not_found} ->
            {error, transport_not_registered}
    end.

%% @doc Check WebSocket transport health
-spec check_ws_transport(transport_id()) -> {ok, health_status()} | {error, term()}.
check_ws_transport(TransportId) ->
    case erlmcp_transport_registry:get_transport(TransportId) of
        {ok, TransportInfo} ->
            Pid = maps:get(pid, TransportInfo, undefined),
            case Pid of
                undefined ->
                    {error, no_transport_pid};
                _ ->
                    % Check if transport process is alive
                    case is_process_alive(Pid) of
                        true ->
                            % For WebSocket, we consider it healthy if the process is alive
                            % Real ping/pong would require protocol-specific handling
                            % which is better done within the websocket handler itself
                            {ok, #{status => healthy}};
                        false ->
                            {error, transport_process_dead}
                    end
            end;
        {error, not_found} ->
            {error, transport_not_registered}
    end.

%% @doc Helper function to check HTTP endpoint health
-spec check_http_endpoint(string()) -> {ok, health_status()} | {error, term()}.
check_http_endpoint(URL) ->
    % Use gun for HTTP client
    StartTime = erlang:monotonic_time(microsecond),

    % Parse URL
    URIMap = uri_string:parse(URL),
    Scheme =
        case maps:get(scheme, URIMap, "http") of
            "https" ->
                https;
            _ ->
                http
        end,
    Host = maps:get(host, URIMap, "localhost"),
    Port =
        maps:get(port,
                 URIMap,
                 case Scheme of
                     https ->
                         443;
                     _ ->
                         80
                 end),
    Path = maps:get(path, URIMap, "/"),

    % Open connection with gun
    Opts =
        #{protocols => [http],
          retry => 0,
          connect_timeout => ?HEALTH_CHECK_TIMEOUT},

    case gun:open(Host, Port, Opts) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid, ?HEALTH_CHECK_TIMEOUT) of
                {ok, _Protocol} ->
                    % Send HEAD request
                    StreamRef = gun:head(ConnPid, Path),
                    case gun:await(ConnPid, StreamRef, ?HEALTH_CHECK_TIMEOUT) of
                        {response, _IsFin, Status, _Headers} ->
                            EndTime = erlang:monotonic_time(microsecond),
                            LatencyUs = EndTime - StartTime,
                            LatencyMs = LatencyUs / 1000,
                            gun:close(ConnPid),
                            % Accept 2xx, 3xx, 4xx as healthy (server is responding)
                            % Only 5xx or connection failure is unhealthy
                            if Status >= 200, Status < 500 ->
                                   {ok, #{status => healthy, latency_ms => LatencyMs}};
                               true ->
                                   {error, {http_status, Status}}
                            end;
                        {error, Reason} ->
                            gun:close(ConnPid),
                            {error, {http_request_failed, Reason}}
                    end;
                {error, Reason} ->
                    gun:close(ConnPid),
                    {error, {connection_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {gun_open_failed, Reason}}
    end.

%% @doc Perform health checks on all registered transports
-spec perform_all_health_checks(#state{}) -> #state{}.
perform_all_health_checks(State) ->
    TransportIds = maps:keys(State#state.health_checks),
    lists:foldl(fun(TransportId, AccState) ->
                   {ok, _Status} = perform_health_check(TransportId, AccState),
                   AccState
                end,
                State,
                TransportIds).

%% @doc Update transport metrics
-spec update_transport_metrics(transport_id(), atom(), number(), #state{}) -> #state{}.
update_transport_metrics(TransportId, MetricName, Value, State) ->
    case maps:get(TransportId, State#state.health_checks, undefined) of
        undefined ->
            State;
        Health ->
            CurrentMetrics = maps:get(metrics, Health),
            UpdatedMetrics = maps:put(MetricName, Value, CurrentMetrics),
            UpdatedHealth = maps:put(metrics, UpdatedMetrics, Health),
            UpdatedHealthChecks = maps:put(TransportId, UpdatedHealth, State#state.health_checks),
            State#state{health_checks = UpdatedHealthChecks}
    end.

%% @doc Reset metrics for a transport
-spec reset_transport_metrics(transport_id(), #state{}) -> #state{}.
reset_transport_metrics(TransportId, State) ->
    case maps:get(TransportId, State#state.health_checks, undefined) of
        undefined ->
            State;
        Health ->
            UpdatedHealth = maps:put(metrics, initial_metrics(), Health),
            UpdatedHealthChecks = maps:put(TransportId, UpdatedHealth, State#state.health_checks),
            State#state{health_checks = UpdatedHealthChecks}
    end.

%% @doc Create initial metrics
-spec initial_metrics() -> health_metrics().
initial_metrics() ->
    #{timestamp => erlang:system_time(millisecond),
      connection_status => unknown,
      latency_ms => undefined,
      error_rate => 0.0,
      throughput => 0,
      last_error => undefined}.
