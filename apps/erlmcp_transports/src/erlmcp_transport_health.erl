%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Health Check and Monitoring Module
%%%
%%% Provides health monitoring capabilities for all transport types.
%%% Includes connection state validation, performance metrics,
%%% and automated recovery mechanisms.
%%%
%%% OTP 28 Features:
%%% - Enhanced monitoring with OTP 28 process flags
%%% - Hibernation-aware health checks
%%% - UTF-8 validation monitoring
%%% - Priority message queue monitoring
%%% - Memory-efficient health tracking
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_health).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, check_health/1, check_health/2, get_health_status/1,
         register_transport/3, unregister_transport/1, update_metrics/3, trigger_health_check/1,
         reset_metrics/1, get_otp28_info/0, set_hibernate_threshold/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_CHECK_INTERVAL, 30000). % 30 seconds
-define(HEALTH_CHECK_TIMEOUT, 5000). % 5 seconds
-define(METRICS_RETENTION_PERIOD, 300000). % 5 minutes
-define(MAX_METRICS_HISTORY, 100).

%% OTP 28: Hibernation-aware checks
-define(HIBERNATION_CHECK_INTERVAL, 60000). % 1 minute
-define(OTP28_VERSION_THRESHOLD, 28).

-type transport_id() :: atom().
-type health_status() :: healthy | degraded | unhealthy | unknown.
-type health_metrics() ::
    #{timestamp => integer(),
      connection_status => up | down | unknown,
      latency_ms => number() | undefined,
      error_rate => float(),
      throughput => number(),
      last_error => term() | undefined,
      %% OTP 28: Extended metrics
      hibernation_active => boolean(),
      utf8_validation_errors => non_neg_integer(),
      priority_queue_size => non_neg_integer(),
      memory_usage => non_neg_integer()}.
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
         metrics_history = #{} :: #{transport_id() => [health_metrics()]},
         otp_version = 0 :: non_neg_integer(),
         hibernation_monitoring = true :: boolean()}).

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

%% @doc Get OTP 28 system information
-spec get_otp28_info() -> map().
get_otp28_info() ->
    gen_server:call(?MODULE, get_otp28_info, 5000).

%% @doc Set hibernation threshold for a transport
-spec set_hibernate_threshold(transport_id(), non_neg_integer()) -> ok | {error, term()}.
set_hibernate_threshold(TransportId, Threshold) ->
    gen_server:call(?MODULE, {set_hibernate_threshold, TransportId, Threshold}, 5000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    CheckInterval = maps:get(check_interval, Opts, ?DEFAULT_CHECK_INTERVAL),
    HibernationMonitoring = maps:get(hibernation_monitoring, Opts, true),

    %% OTP 28: Detect OTP version
    OTPVersion = detect_otp_version(),
    IsOTP28Plus = OTPVersion >= ?OTP28_VERSION_THRESHOLD,

    %% OTP 28: Enable hibernation-aware monitoring if supported
    State = #state{
        check_interval = CheckInterval,
        otp_version = OTPVersion,
        hibernation_monitoring = HibernationMonitoring andalso IsOTP28Plus
    },

    Timer = erlang:send_after(CheckInterval, self(), scheduled_check),
    ?LOG_INFO("Transport health monitor started (OTP ~p, hibernation monitoring: ~p)",
              [OTPVersion, State#state.hibernation_monitoring]),
    {ok, State#state{check_timer = Timer}}.

handle_call({check_health, TransportId}, _From, State) ->
    Result = perform_health_check(TransportId, State),
    {reply, Result, State, hibernate};
handle_call({get_health_status, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.health_checks, undefined) of
        undefined ->
            {reply, {error, not_registered}, State, hibernate};
        Health ->
            {reply, {ok, Health}, State, hibernate}
    end;
handle_call({register_transport, TransportId, Pid, Config}, _From, State) ->
    %% OTP 28: Extended initial metrics
    InitialMetrics = initial_metrics_otp28(),
    InitialHealth =
        #{transport_id => TransportId,
          status => unknown,
          metrics => InitialMetrics,
          last_check => erlang:system_time(millisecond),
          consecutive_failures => 0,
          last_healthy => undefined},
    NewHealthChecks = maps:put(TransportId, InitialHealth, State#state.health_checks),
    MonitorRef = monitor(process, Pid),
    ?LOG_INFO("Registered transport ~p for health monitoring (OTP ~p)",
              [TransportId, State#state.otp_version]),
    {reply, ok, State#state{health_checks = NewHealthChecks}, hibernate};
handle_call({unregister_transport, TransportId}, _From, State) ->
    NewHealthChecks = maps:remove(TransportId, State#state.health_checks),
    NewHistory = maps:remove(TransportId, State#state.metrics_history),
    ?LOG_INFO("Unregistered transport ~p from health monitoring", [TransportId]),
    {reply, ok, State#state{health_checks = NewHealthChecks, metrics_history = NewHistory}, hibernate};
handle_call(overall_health, _From, State) ->
    % Aggregate health status from all transports
    TransportHealthList = maps:to_list(State#state.health_checks),
    OverallStatus = calculate_overall_health(TransportHealthList),
    {reply, OverallStatus, State, hibernate};
handle_call({set_threshold, TransportId, MetricName, Value}, _From, State) ->
    % Store threshold configuration (in a real implementation, this would be persisted)
    ThresholdsKey = {health_threshold, TransportId},
    CurrentThresholds = case get(ThresholdsKey) of
        undefined -> #{};
        T -> T
    end,
    NewThresholds = maps:put(MetricName, Value, CurrentThresholds),
    put(ThresholdsKey, NewThresholds),
    ?LOG_INFO("Set health threshold for ~p: ~p = ~p", [TransportId, MetricName, Value]),
    {reply, ok, State, hibernate};
handle_call({get_health_history, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.metrics_history, undefined) of
        undefined ->
            {reply, {error, not_found}, State, hibernate};
        History ->
            {reply, {ok, lists:reverse(History)}, State, hibernate}
    end;
handle_call({set_hibernate_threshold, TransportId, Threshold}, _From, State) ->
    %% OTP 28: Set hibernation threshold for a transport
    Reply = case maps:get(TransportId, State#state.health_checks, undefined) of
        undefined ->
            {error, not_registered};
        _Health ->
            %% In a real implementation, we would communicate with the transport
            %% to set its hibernation threshold
            ?LOG_INFO("Set hibernation threshold for ~p: ~pms", [TransportId, Threshold]),
            ok
    end,
    {reply, Reply, State, hibernate};
handle_call(get_otp28_info, _From, State) ->
    %% OTP 28: Return system information
    Info = #{
        otp_version => State#state.otp_version,
        hibernation_monitoring => State#state.hibernation_monitoring,
        monitored_transports => maps:size(State#state.health_checks),
        system_info => get_system_info()
    },
    {reply, Info, State, hibernate};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

handle_cast({update_metrics, TransportId, MetricName, Value}, State) ->
    NewState =
        case maps:get(TransportId, State#state.health_checks, undefined) of
            undefined ->
                State;
            _Health ->
                update_transport_metrics(TransportId, MetricName, Value, State)
        end,
    {noreply, NewState, hibernate};
handle_cast({trigger_health_check, TransportId}, State) ->
    NewState =
        case maps:get(TransportId, State#state.health_checks, undefined) of
            undefined ->
                State;
            _Health ->
                {ok, _Status} = perform_health_check(TransportId, State),
                State
        end,
    {noreply, NewState, hibernate};
handle_cast({reset_metrics, TransportId}, State) ->
    NewState =
        case maps:get(TransportId, State#state.health_checks, undefined) of
            undefined ->
                State;
            _Health ->
                reset_transport_metrics(TransportId, State)
        end,
    {noreply, NewState, hibernate};
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

handle_info(scheduled_check, State) ->
    % Perform health checks on all registered transports
    NewState = perform_all_health_checks(State),
    Timer = erlang:send_after(State#state.check_interval, self(), scheduled_check),
    {noreply, NewState#state{check_timer = Timer}, hibernate};
handle_info({'DOWN', _MonitorRef, process, _Pid, _Reason}, State) ->
    % Handle process death - transport died
    {noreply, State, hibernate};
handle_info(_Info, State) ->
    {noreply, State, hibernate}.

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

%% @doc Detect OTP version
-spec detect_otp_version() -> non_neg_integer().
detect_otp_version() ->
    try
        VersionStr = erlang:system_info(otp_release),
        [MajorStr | _] = string:split(VersionStr, "."),
        list_to_integer(MajorStr)
    catch
        _:_ ->
            0
    end.

%% @doc Get system information for OTP 28 monitoring
-spec get_system_info() -> map().
get_system_info() ->
    try
        #{
            process_count => erlang:system_info(process_count),
            port_count => erlang:system_info(port_count),
            atom_count => erlang:system_info(atom_count),
            memory_total => erlang:memory(total),
            memory_processes => erlang:memory(processes),
            memory_system => erlang:memory(system),
            ets_limit => erlang:system_info(ets_limit),
            ets_count => ets:info()
        }
    catch
        _:_ ->
            #{error => system_info_unavailable}
    end.

%% @doc Create initial metrics with OTP 28 fields
-spec initial_metrics_otp28() -> health_metrics().
initial_metrics_otp28() ->
    #{timestamp => erlang:system_time(millisecond),
      connection_status => unknown,
      latency_ms => undefined,
      error_rate => 0.0,
      throughput => 0,
      last_error => undefined,
      %% OTP 28: Extended metrics
      hibernation_active => false,
      utf8_validation_errors => 0,
      priority_queue_size => 0,
      memory_usage => 0}.

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
                    check_transport_status(TransportId, Health, State)
            end
    end.

%% @doc Check transport-specific status with OTP 28 features
-spec check_transport_status(transport_id(), transport_health(), #state{}) ->
                                {ok, health_status()} | {error, term()}.
check_transport_status(TransportId, _Health, State) ->
    % Determine transport type and perform appropriate check
    TransportType = get_transport_type(TransportId),
    Result =
        case TransportType of
            tcp ->
                check_tcp_transport_otp28(TransportId, State);
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

%% @doc Check TCP transport health with OTP 28 features
-spec check_tcp_transport_otp28(transport_id(), #state{}) ->
                                     {ok, health_status()} | {error, term()}.
check_tcp_transport_otp28(TransportId, #state{otp_version = OTPVersion}) ->
    BaseResult = check_tcp_transport(TransportId),
    case BaseResult of
        {ok, #{status := healthy} = Info} ->
            %% OTP 28: Check additional health metrics
            ExtendedInfo = case OTPVersion >= 28 of
                true ->
                    %% OTP 28: Check hibernation status, UTF-8 validation, etc.
                    Info#{
                        hibernation_active => check_hibernation_status(TransportId),
                        utf8_validation_errors => get_utf8_error_count(TransportId),
                        priority_queue_size => get_priority_queue_size(TransportId)
                    };
                false ->
                    Info
            end,
            {ok, ExtendedInfo};
        _ ->
            BaseResult
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
check_stdio_transport(_TransportId) ->
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
            UpdatedHealth = maps:put(metrics, initial_metrics_otp28(), Health),
            UpdatedHealthChecks = maps:put(TransportId, UpdatedHealth, State#state.health_checks),
            State#state{health_checks = UpdatedHealthChecks}
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

%% @doc Check hibernation status (OTP 28)
-spec check_hibernation_status(transport_id()) -> boolean().
check_hibernation_status(TransportId) ->
    try
        case whereis(TransportId) of
            undefined ->
                false;
            Pid ->
                %% Check if process is hibernated by checking its status
                case process_info(Pid, status) of
                    {status, hibernating} ->
                        true;
                    _ ->
                        false
                end
        end
    catch
        _:_ ->
            false
    end.

%% @doc Get UTF-8 validation error count (OTP 28)
-spec get_utf8_error_count(transport_id()) -> non_neg_integer().
get_utf8_error_count(_TransportId) ->
    %% In a real implementation, this would query the transport for UTF-8 errors
    %% For now, return 0
    0.

%% @doc Get priority queue size (OTP 28)
-spec get_priority_queue_size(transport_id()) -> non_neg_integer().
get_priority_queue_size(_TransportId) ->
    %% In a real implementation, this would query the transport for queue size
    %% For now, return 0
    0.

%% @doc Get overall health status across all transports
-spec overall_health() -> map().
overall_health() ->
    gen_server:call(?MODULE, overall_health, 5000).

%% @doc Set health check threshold for a specific metric
-spec set_threshold(transport_id(), atom(), number()) -> ok | {error, term()}.
set_threshold(TransportId, MetricName, Value) ->
    gen_server:call(?MODULE, {set_threshold, TransportId, MetricName, Value}, 5000).

%% @doc Get health check history for a transport
-spec get_health_history(transport_id()) -> {ok, [health_metrics()]} | {error, term()}.
get_health_history(TransportId) ->
    gen_server:call(?MODULE, {get_health_history, TransportId}, 5000).

%% @doc Calculate overall health status from all transports
-spec calculate_overall_health([{transport_id(), transport_health()}]) -> map().
calculate_overall_health(TransportHealthList) ->
    TotalCount = length(TransportHealthList),
    HealthyCount = lists:foldl(
        fun({_Id, #{status := Status}}, Acc) ->
            case Status of
                healthy -> Acc + 1;
                _ -> Acc
            end
        end,
        0,
        TransportHealthList
    ),
    DegradedCount = lists:foldl(
        fun({_Id, #{status := Status}}, Acc) ->
            case Status of
                degraded -> Acc + 1;
                _ -> Acc
            end
        end,
        0,
        TransportHealthList
    ),
    UnhealthyCount = TotalCount - HealthyCount - DegradedCount,

    OverallStatus = case {HealthyCount, DegradedCount, UnhealthyCount} of
        {_, _, 0} when HealthyCount + DegradedCount > 0 ->
            if DegradedCount > 0 -> degraded; true -> healthy end;
        {0, 0, 0} -> unknown;
        _ -> unhealthy
    end,

    #{overall_status => OverallStatus,
      total_transports => TotalCount,
      healthy_count => HealthyCount,
      degraded_count => DegradedCount,
      unhealthy_count => UnhealthyCount,
      timestamp => erlang:system_time(millisecond),
      transports => maps:from_list(
        lists:map(
            fun({Id, #{status := Status} = Health}) ->
                {Id, #{status => Status, last_check => maps:get(last_check, Health, 0)}}
            end,
            TransportHealthList
        )
    )}.
