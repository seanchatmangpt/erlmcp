%%%-------------------------------------------------------------------
%%% @doc
%%% Health Check HTTP Handler for Docker Swarm / Kubernetes
%%%
%%% Provides REST endpoints for orchestration health checks:
%%%   GET /health  - Basic health (returns 200 if healthy, 503 if not)
%%%   GET /ready   - Readiness probe (ready to serve traffic)
%%%   GET /live    - Liveness probe (process is alive)
%%%   GET /metrics - Prometheus metrics endpoint
%%%
%%% These endpoints are used by Docker Swarm, Kubernetes, and load balancers
%%% to determine service health and routing decisions.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_health_http).
-behaviour(cowboy_handler).

%% Cowboy callbacks
-export([init/2]).

%% HTTP Handler Functions
-export([handle_health/1, handle_ready/1, handle_live/1, handle_metrics/1]).

-include_lib("kernel/include/logger.hrl").

%% Health Response Headers
-define(HEALTH_HEADERS, #{
    <<"content-type">> => <<"application/json">>,
    <<"cache-control">> => <<"no-cache, no-store, must-revalidate">>,
    <<"pragma">> => <<"no-cache">>,
    <<"expires">> => <<"0">>
}).

%% Health thresholds
-define(MEMORY_WARNING_THRESHOLD, 0.80).    % 80% memory usage
-define(MEMORY_CRITICAL_THRESHOLD, 0.90).   % 90% memory usage
-define(PROCESS_WARNING_THRESHOLD, 0.80).   % 80% process limit
-define(PROCESS_CRITICAL_THRESHOLD, 0.90).  % 90% process limit

%%====================================================================
%% Cowboy Callbacks
%%====================================================================

%% @doc Handle incoming HTTP request
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    Req = case {Method, Path} of
        {<<"GET">>, <<"/health">>} ->
            handle_health(Req0);
        {<<"GET">>, <<"/ready">>} ->
            handle_ready(Req0);
        {<<"GET">>, <<"/live">>} ->
            handle_live(Req0);
        {<<"GET">>, <<"/metrics">>} ->
            handle_metrics(Req0);
        {<<"GET">>, <<"/healthz">>} ->
            %% Kubernetes-style health endpoint
            handle_health(Req0);
        {<<"GET">>, <<"/readiness">>} ->
            handle_ready(Req0);
        {<<"GET">>, <<"/liveness">>} ->
            handle_live(Req0);
        _ ->
            respond_json(Req0, 404, #{error => <<"Not found">>})
    end,

    {ok, Req, State}.

%%====================================================================
%% Health Check Endpoints
%%====================================================================

%% @doc GET /health - Comprehensive health check
%% Returns overall health status of the service with detailed system information
-spec handle_health(cowboy_req:req()) -> cowboy_req:req().
handle_health(Req) ->
    %% Collect comprehensive health information
    SystemStatus = check_system_status(),
    MemoryStatus = check_memory_status(),
    EtsStatus = check_ets_tables(),
    ProcessStatus = check_process_status(),

    %% Determine overall health
    AllHealthy = (SystemStatus =:= healthy) andalso
                 (MemoryStatus =:= healthy) andalso
                 (EtsStatus =:= healthy) andalso
                 (ProcessStatus =:= healthy),

    Response = #{
        status => status_to_binary(AllHealthy),
        version => get_version(),
        uptime => get_uptime(),
        node => node_bin(),
        timestamp => get_timestamp(),
        checks => #{
            system => #{
                status => status_to_binary(SystemStatus =:= healthy),
                details => SystemStatus
            },
            memory => #{
                status => status_to_binary(MemoryStatus =:= healthy),
                details => MemoryStatus
            },
            ets_tables => #{
                status => status_to_binary(EtsStatus =:= healthy),
                details => EtsStatus
            },
            processes => #{
                status => status_to_binary(ProcessStatus =:= healthy),
                details => ProcessStatus
            }
        },
        metrics => get_system_metrics()
    },

    StatusCode = case AllHealthy of
        true -> 200;
        false -> 503
    end,

    respond_json(Req, StatusCode, Response).

%% @doc GET /ready - Readiness probe
%% Returns 200 if the service is ready to accept traffic
-spec handle_ready(cowboy_req:req()) -> cowboy_req:req().
handle_ready(Req) ->
    %% Check if critical services are ready
    ReadyChecks = [
        {registry, fun() -> check_registry_ready() end},
        {session_manager, fun() -> check_session_manager_ready() end},
        {http_server, fun() -> check_http_server_ready() end},
        {observability, fun() -> check_observability_ready() end},
        {transports, fun() -> check_transports_ready() end}
    ],

    Results = [{Name, CheckFun()} || {Name, CheckFun} <- ReadyChecks],
    AllReady = lists:all(fun({_, Status}) -> Status =:= ready end, Results),

    Response = #{
        ready => AllReady,
        node => node_bin(),
        timestamp => get_timestamp(),
        checks => maps:from_list(Results)
    },

    StatusCode = case AllReady of
        true -> 200;
        false -> 503
    end,

    respond_json(Req, StatusCode, Response).

%% @doc GET /live - Liveness probe
%% Returns 200 if the process is alive and responding
-spec handle_live(cowboy_req:req()) -> cowboy_req:req().
handle_live(Req) ->
    Response = #{
        alive => true,
        node => node_bin(),
        uptime => get_uptime(),
        timestamp => get_timestamp()
    },
    respond_json(Req, 200, Response).

%% @doc GET /metrics - Prometheus metrics endpoint
%% Returns metrics in Prometheus text format
-spec handle_metrics(cowboy_req:req()) -> cowboy_req:req().
handle_metrics(Req) ->
    case get_prometheus_metrics() of
        {ok, Metrics} ->
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Metrics, Req);
        {error, Reason} ->
            ?LOG_ERROR("Failed to get metrics: ~p", [Reason]),
            cowboy_req:reply(500, #{}, <<"Error fetching metrics">>, Req)
    end.

%%====================================================================
%% Internal Functions - System Status Checks
%%====================================================================

%% @doc Check system status (application running, node up)
-spec check_system_status() -> healthy | degraded | unhealthy.
check_system_status() ->
    %% Check if application is running
    AppStatus = case application:which_applications() of
        Apps when is_list(Apps) ->
            case lists:keyfind(erlmcp_core, 1, Apps) of
                {erlmcp_core, _, _} -> true;
                false -> false
            end;
        _ -> false
    end,

    %% Check node status
    NodeStatus = case net_adm:ping(node()) of
        pong -> true;
        pang -> false
    end,

    %% Check scheduler utilization
    {TotalRunQueue, _} = erlang:statistics(run_queue_lengths),
    SchedulerStatus = TotalRunQueue < 1000,

    case {AppStatus, NodeStatus, SchedulerStatus} of
        {true, true, true} -> healthy;
        {true, true, false} -> degraded;
        _ -> unhealthy
    end.

%% @doc Check memory status
-spec check_memory_status() -> healthy | degraded | unhealthy.
check_memory_status() ->
    %% Collect memory metrics
    TotalMem = erlang:memory(total),
    SystemMem = erlang:memory(system),
    ProcessMem = erlang:memory(processes),
    EtsMem = erlang:memory(ets),

    %% Calculate memory ratios
    ProcessRatio = ProcessMem / (TotalMem + 1),
    EtsRatio = EtsMem / (TotalMem + 1),

    %% Check memory thresholds
    Critical = (ProcessRatio > ?MEMORY_CRITICAL_THRESHOLD) orelse
               (EtsRatio > ?MEMORY_CRITICAL_THRESHOLD),
    Warning = (ProcessRatio > ?MEMORY_WARNING_THRESHOLD) orelse
              (EtsRatio > ?MEMORY_WARNING_THRESHOLD),

    if
        Critical -> unhealthy;
        Warning -> degraded;
        true -> healthy
    end.

%% @doc Check ETS tables status
-spec check_ets_tables() -> healthy | degraded | unhealthy.
check_ets_tables() ->
    %% Get all ETS tables
    AllTables = ets:all(),
    TableCount = length(AllTables),

    %% Check for critical tables
    CriticalTables = [
        erlmcp_registry,
        erlmcp_session_table,
        erlmcp_receipt_chain,
        erlmcp_audit_log
    ],

    %% Check if critical tables exist and are accessible
    TablesStatus = lists:map(fun(Table) ->
        case ets:info(Table) of
            undefined -> missing;
            Info when is_list(Info) ->
                %% Check table health (size, memory)
                Size = proplists:get_value(size, Info, 0),
                Memory = proplists:get_value(memory, Info, 0),
                {ok, Size, Memory}
        end
    end, CriticalTables),

    %% Count missing tables
    MissingCount = length([Status || Status <- TablesStatus, Status =:= missing]),

    %% Determine status
    if
        MissingCount > 0 -> unhealthy;
        TableCount > 10000 -> degraded;  % Too many tables
        true -> healthy
    end.

%% @doc Check process status
-spec check_process_status() -> healthy | degraded | unhealthy.
check_process_status() ->
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),
    ProcessRatio = ProcessCount / ProcessLimit,

    if
        ProcessRatio > ?PROCESS_CRITICAL_THRESHOLD -> unhealthy;
        ProcessRatio > ?PROCESS_WARNING_THRESHOLD -> degraded;
        true -> healthy
    end.

%%====================================================================
%% Internal Functions - Readiness Checks
%%====================================================================

%% @doc Check if registry is ready
-spec check_registry_ready() -> ready | not_ready.
check_registry_ready() ->
    case whereis(erlmcp_registry) of
        undefined -> not_ready;
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> ready;
                false -> not_ready
            end
    end.

%% @doc Check if session manager is ready
-spec check_session_manager_ready() -> ready | not_ready.
check_session_manager_ready() ->
    case whereis(erlmcp_session_manager) of
        undefined -> not_ready;
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> ready;
                false -> not_ready
            end
    end.

%% @doc Check if HTTP server is ready
-spec check_http_server_ready() -> ready | not_ready.
check_http_server_ready() ->
    case whereis(erlmcp_health_http_server) of
        undefined -> not_ready;
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> ready;
                false -> not_ready
            end
    end.

%% @doc Check if observability is ready
-spec check_observability_ready() -> ready | not_ready.
check_observability_ready() ->
    %% Check critical observability components
    CriticalProcesses = [
        erlmcp_health_monitor,
        erlmcp_metrics,
        erlmcp_recovery_manager
    ],

    AllAlive = lists:all(fun(ProcessName) ->
        case whereis(ProcessName) of
            undefined -> false;
            Pid when is_pid(Pid) -> is_process_alive(Pid)
        end
    end, CriticalProcesses),

    case AllAlive of
        true -> ready;
        false -> not_ready
    end.

%% @doc Check if transports are ready
-spec check_transports_ready() -> ready | not_ready.
check_transports_ready() ->
    case whereis(erlmcp_transport_sup) of
        undefined -> not_ready;
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> ready;
                false -> not_ready
            end
    end.

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @doc Send JSON response
-spec respond_json(cowboy_req:req(), pos_integer(), map()) -> cowboy_req:req().
respond_json(Req, StatusCode, Body) ->
    cowboy_req:reply(StatusCode, ?HEALTH_HEADERS, json_encode(Body), Req).

%% @doc Encode map to JSON (using OTP 27+ json module)
-spec json_encode(map()) -> binary().
json_encode(Map) when is_map(Map) ->
    try
        json:encode(Map)
    catch
        _:_ ->
            %% Fallback: convert to simple JSON-like binary
            fallback_json_encode(Map)
    end.

%% @doc Fallback JSON encoder for older OTP versions
-spec fallback_json_encode(map()) -> binary().
fallback_json_encode(Map) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        [encode_value(K), $:, encode_value(V) | Acc]
    end, [], Map),
    iolist_to_binary([${, lists:join($,, lists:reverse(Pairs)), $}]).

%% @doc Encode value as JSON
-spec encode_value(term()) -> iolist().
encode_value(true) -> <<"true">>;
encode_value(false) -> <<"false">>;
encode_value(null) -> <<"null">>;
encode_value(V) when is_integer(V) -> integer_to_binary(V);
encode_value(V) when is_float(V) -> float_to_binary(V, [{scientific, 10}]);
encode_value(V) when is_atom(V) -> <<"\"", (atom_to_binary(V, utf8))/binary, "\"">>;
encode_value(V) when is_binary(V) -> <<"\"", V/binary, "\"">>;
encode_value(V) when is_list(V) -> <<"\"", (list_to_binary(V))/binary, "\"">>;
encode_value(V) when is_map(V) -> fallback_json_encode(V).

%% @doc Convert status to binary
-spec status_to_binary(boolean() | healthy | degraded | unhealthy) -> binary().
status_to_binary(true) -> <<"healthy">>;
status_to_binary(false) -> <<"unhealthy">>;
status_to_binary(healthy) -> <<"healthy">>;
status_to_binary(degraded) -> <<"degraded">>;
status_to_binary(unhealthy) -> <<"unhealthy">>;
status_to_binary(ready) -> <<"ready">>;
status_to_binary(not_ready) -> <<"not_ready">>.

%% @doc Get application version
-spec get_version() -> binary().
get_version() ->
    case application:get_key(erlmcp_core, vsn) of
        {ok, Vsn} -> list_to_binary(Vsn);
        undefined -> <<"unknown">>
    end.

%% @doc Get node uptime in seconds
-spec get_uptime() -> non_neg_integer().
get_uptime() ->
    case erlang:statistics(wall_clock) of
        {UpTimeMs, _} -> UpTimeMs div 1000;
        _ -> 0
    end.

%% @doc Get current timestamp as ISO 8601 binary
-spec get_timestamp() -> binary().
get_timestamp() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    iolist_to_binary(
        io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
            [Y, Mo, D, H, Mi, S])).

%% @doc Get node name as binary
-spec node_bin() -> binary().
node_bin() ->
    atom_to_binary(node(), utf8).

%% @doc Get comprehensive system metrics
-spec get_system_metrics() -> map().
get_system_metrics() ->
    #{
        %% Memory metrics
        memory_total => erlang:memory(total),
        memory_processes => erlang:memory(processes),
        memory_system => erlang:memory(system),
        memory_ets => erlang:memory(ets),
        memory_atom => erlang:memory(atom),
        memory_binary => erlang:memory(binary),
        memory_code => erlang:memory(code),

        %% Process metrics
        process_count => erlang:system_info(process_count),
        process_limit => erlang:system_info(process_limit),

        %% ETS metrics
        ets_count => length(ets:all()),

        %% Atom metrics
        atom_count => erlang:system_info(atom_count),
        atom_limit => erlang:system_info(atom_limit),

        %% Port metrics
        port_count => erlang:system_info(port_count),
        port_limit => erlang:system_info(port_limit),

        %% Scheduler metrics
        scheduler_count => erlang:system_info(schedulers),
        scheduler_online => erlang:system_info(schedulers_online),
        run_queue => element(1, erlang:statistics(run_queue_lengths)),

        %% Garbage collection metrics
        gc_count => element(1, erlang:statistics(garbage_collection)),
        words_reclaimed => element(2, erlang:statistics(garbage_collection)),

        %% IO metrics
        io_input => element(1, erlang:statistics(io)),
        io_output => element(2, erlang:statistics(io)),

        %% Reduction metrics
        reductions => element(1, erlang:statistics(reductions)),

        %% Context switches
        context_switches => erlang:statistics(context_switches)
    }.

%% @doc Get Prometheus metrics
-spec get_prometheus_metrics() -> {ok, binary()} | {error, term()}.
get_prometheus_metrics() ->
    try
        %% Collect application metrics
        Metrics = get_system_metrics(),
        {ok, format_prometheus_metrics(Metrics)}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% @doc Format metrics as Prometheus text format
-spec format_prometheus_metrics(map()) -> binary().
format_prometheus_metrics(Metrics) ->
    Lines = [
        [<<"# HELP erlmcp_process_count Current number of processes">>,
         <<"# TYPE erlmcp_process_count gauge">>,
         io_lib:format("erlmcp_process_count ~p", [maps:get(process_count, Metrics)])],

        [<<"# HELP erlmcp_process_limit Maximum number of processes">>,
         <<"# TYPE erlmcp_process_limit gauge">>,
         io_lib:format("erlmcp_process_limit ~p", [maps:get(process_limit, Metrics)])],

        [<<"# HELP erlmcp_memory_bytes Total memory in bytes">>,
         <<"# TYPE erlmcp_memory_bytes gauge">>,
         io_lib:format("erlmcp_memory_bytes ~p", [maps:get(memory_total, Metrics)])],

        [<<"# HELP erlmcp_memory_processes_bytes Memory used by processes">>,
         <<"# TYPE erlmcp_memory_processes_bytes gauge">>,
         io_lib:format("erlmcp_memory_processes_bytes ~p", [maps:get(memory_processes, Metrics)])],

        [<<"# HELP erlmcp_memory_system_bytes Memory used by system">>,
         <<"# TYPE erlmcp_memory_system_bytes gauge">>,
         io_lib:format("erlmcp_memory_system_bytes ~p", [maps:get(memory_system, Metrics)])],

        [<<"# HELP erlmcp_ets_count Number of ETS tables">>,
         <<"# TYPE erlmcp_ets_count gauge">>,
         io_lib:format("erlmcp_ets_count ~p", [maps:get(ets_count, Metrics)])],

        [<<"# HELP erlmcp_atom_count Number of atoms">>,
         <<"# TYPE erlmcp_atom_count gauge">>,
         io_lib:format("erlmcp_atom_count ~p", [maps:get(atom_count, Metrics)])],

        [<<"# HELP erlmcp_port_count Number of ports">>,
         <<"# TYPE erlmcp_port_count gauge">>,
         io_lib:format("erlmcp_port_count ~p", [maps:get(port_count, Metrics)])],

        [<<"# HELP erlmcp_scheduler_count Number of schedulers">>,
         <<"# TYPE erlmcp_scheduler_count gauge">>,
         io_lib:format("erlmcp_scheduler_count ~p", [maps:get(scheduler_count, Metrics)])],

        [<<"# HELP erlmcp_run_queue_length Run queue length">>,
         <<"# TYPE erlmcp_run_queue_length gauge">>,
         io_lib:format("erlmcp_run_queue_length ~p", [maps:get(run_queue, Metrics)])],

        [<<"# HELP erlmcp_uptime_seconds Uptime in seconds">>,
         <<"# TYPE erlmcp_uptime_seconds gauge">>,
         io_lib:format("erlmcp_uptime_seconds ~p", [get_uptime()])]
    ],

    FormattedLines = [iolist_to_binary(Line) || Line <- Lines],
    iolist_to_binary(lists:join([<<"\n">>], FormattedLines) ++ [<<"\n">>]).
