%% @doc Enterprise Health Check System for erlmcp v3
%% This module provides comprehensive health monitoring endpoints and probes
%% for enterprise deployments.
-module(erlmcp_health_check).

-behaviour(gen_server).

%% API
-export([start_link/0, health_check/1, liveness_check/1, readiness_check/1,
         get_health_status/0, register_component/2, unregister_component/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the health check server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Perform comprehensive health check
-spec health_check(Req :: map()) -> map().
health_check(Req) ->
    case get_health_status() of
        #{status := healthy} ->
            #{
                status => healthy,
                timestamp => timestamp(),
                version => erlmcp_version(),
                components => get_component_health(),
                details => health_details(),
                checks => perform_detailed_checks(Req)
            };
        #{status := degraded} = Result ->
            Result;
        #{status := unhealthy} = Result ->
            Result
    end.

%% @doc Kubernetes liveness probe
-spec liveness_check(Req :: map()) -> map().
liveness_check(Req) ->
    #{
        status => alive,
        timestamp => timestamp(),
        uptime => erlang:system_time(second),
        memory => get_memory_info(),
        processes => erlang:system_info(process_count)
    }.

%% @doc Kubernetes readiness probe
-spec readiness_check(Req :: map()) -> map().
readiness_check(Req) ->
    %% Check all critical components
    CriticalChecks = [
        {core, check_core()},
        {registry, check_registry()},
        {sessions, check_sessions()},
        {database, check_database()},
        {external_services, check_external_services()}
    ],

    Results = lists:map(fun({Component, Check}) ->
        #{component => Component, status => Check}
    end, CriticalChecks),

    AllReady = lists:all(fun(#{status := Status}) -> Status =:= ready end, Results),

    #{
        status => case AllReady of true -> ready; false => not_ready end,
        timestamp => timestamp(),
        components => Results,
        details => readiness_details()
    }.

%% @doc Get current health status
-spec get_health_status() -> map().
get_health_status() ->
    gen_server:call(?MODULE, get_health_status).

%% @doc Register a component for health monitoring
-spec register_component(Component :: atom(), HealthFun :: fun(() -> healthy | degraded | unhealthy)) -> ok.
register_component(Component, HealthFun) ->
    gen_server:cast(?MODULE, {register_component, Component, HealthFun}).

%% @doc Unregister a component from health monitoring
-spec unregister_component(Component :: atom()) -> ok.
unregister_component(Component) ->
    gen_server:cast(?MODULE, {unregister_component, Component}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize health monitoring
    {ok, #{components => #{}, last_check => timestamp()}}.

handle_call(get_health_status, _From, State) ->
    HealthStatus = calculate_health_status(State),
    {reply, HealthStatus, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({register_component, Component, HealthFun}, State) ->
    NewComponents = maps:put(Component, HealthFun, State#{}),
    {noreply, State#{components := NewComponents}};

handle_cast({unregister_component, Component}, State) ->
    NewComponents = maps:remove(Component, State#{}),
    {noreply, State#{components := NewComponents}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Calculate overall health status
calculate_health_status(State) ->
    Components = maps:get(components, State, #{}),
    Checks = maps:map(fun(_, HealthFun) -> HealthFun() end, Components),

    AllHealthy = lists:all(fun(Status) -> Status =:= healthy end, maps:values(Checks)),
    AllReady = lists:all(fun(Status) -> Status =:= ready orelse Status =:= healthy end, maps:values(Checks)),

    #{
        status => case AllHealthy of true -> healthy; false -> case AllReady of true -> degraded; false -> unhealthy end end,
        timestamp => timestamp(),
        components => Checks,
        last_check => maps:get(last_check, State),
        metrics => get_system_metrics()
    }.

%% @private Get component health status
get_component_health() ->
    %% Core component
    Core = case check_core() of
        healthy -> #{status => healthy, last_check => timestamp()};
        degraded -> #{status => degraded, last_check => timestamp()};
        unhealthy -> #{status => unhealthy, last_check => timestamp()}
    end,

    %% Registry component
    Registry = case check_registry() of
        healthy -> #{status => healthy, last_check => timestamp()};
        degraded -> #{status => degraded, last_check => timestamp()};
        unhealthy -> #{status => unhealthy, last_check => timestamp()}
    end,

    %% Sessions component
    Sessions = case check_sessions() of
        healthy -> #{status => healthy, last_check => timestamp()};
        degraded -> #{status => degraded, last_check => timestamp()};
        unhealthy -> #{status => unhealthy, last_check => timestamp()}
    end,

    #{core => Core, registry => Registry, sessions => Sessions}.

%% @private Get health details
health_details() ->
    #{
        system_info => #{
            otp_version => erlang:system_info(otp_release),
            scheduler_count => erlang:system_info(schedulers),
            process_count => erlang:system_info(process_count),
            memory_total => erlang:memory(total),
            memory_processes => erlang:memory(processes),
            memory_system => erlang:memory(system)
        },
        uptime => erlang:system_time(second) - erlang:start_time(),
        load => get_system_load()
    }.

%% @private Perform detailed checks
perform_detailed_checks(Req) ->
    Checks = [
        {cpu_usage, check_cpu_usage()},
        {memory_usage, check_memory_usage()},
        {disk_space, check_disk_space()},
        {network_connectivity, check_network_connectivity()},
        {database_connections, check_database_connections()}
    ],

    lists:map(fun({Check, Result}) ->
        #{check => Check, result => Result, timestamp => timestamp()}
    end, Checks).

%% @private Check core health
check_core() ->
    try
        %% Check if core processes are running
        case erlmcp_core:health_check() of
            ok -> healthy;
            {degraded, _} -> degraded;
            {error, _} -> unhealthy
        end
    catch
        _:_ -> unhealthy
    end.

%% @private Check registry health
check_registry() ->
    try
        case erlmcp_registry:health_check() of
            ok -> healthy;
            {degraded, _} -> degraded;
            {error, _} -> unhealthy
        end
    catch
        _:_ -> unhealthy
    end.

%% @private Check sessions health
check_sessions() ->
    try
        case erlmcp_sessions:health_check() of
            ok -> healthy;
            {degraded, _} -> degraded;
            {error, _} -> unhealthy
        end
    catch
        _:_ -> unhealthy
    end.

%% @private Check database health
check_database() ->
    try
        case erlmcp_db:health_check() of
            ready -> ready;
            connecting -> degraded;
            error -> not_ready
        end
    catch
        _:_ -> not_ready
    end.

%% @private Check external services
check_external_services() ->
    %% Check external dependencies
    Services = [
        {api_gateway, check_service_url("http://api-gateway:8080/health")},
        {auth_service, check_service_url("http://auth-service:8080/health")},
        {logging_service, check_service_url("http://logging-service:8080/health")}
    ],

    Results = lists:map(fun({Service, Status}) ->
        #{service => Service, status => Status}
    end, Services),

    case lists:any(fun(#{status := not_ready}) -> true; (_) -> false end, Results) of
        true -> not_ready;
        false -> ready
    end.

%% @private Check service URL
check_service_url(Url) ->
    case httpc:request(get, {Url, []}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _, _}} -> ready;
        {ok, {{_, 503, _}, _, _}} -> not_ready;
        {error, _} -> not_ready
    end.

%% @private Readiness details
readiness_details() ->
    #{
        required_components => [core, registry, sessions, database],
        optional_components => [external_services],
        checks_performed => get_readiness_metrics()
    }.

%% @private Get system metrics
get_system_metrics() ->
    #{
        cpu_usage => cpu_sup:util([time]),
        memory_usage => erlang:memory(),
        process_count => erlang:system_info(process_count),
        load_average => get_load_average()
    }.

%% @private Get memory info
get_memory_info() ->
    #{
        total => erlang:memory(total),
        processes => erlang:memory(processes),
        system => erlang:memory(system),
        atom => erlang:memory(atom),
        binary => erlang:memory(binary),
        ets => erlang:memory(ets)
    }.

%% @private Check CPU usage
check_cpu_usage() ->
    CpuUsage = cpu_sup:util([time]),
    case CpuUsage > 80 of
        true -> unhealthy;
        false -> case CpuUsage > 60 of true -> degraded; false -> healthy end
    end.

%% @private Check memory usage
check_memory_usage() ->
    TotalMemory = erlang:memory(total),
    SystemMemory = erlang:memory(system),
    Usage = (TotalMemory - SystemMemory) / TotalMemory,
    case Usage > 0.9 of
        true -> unhealthy;
        false -> case Usage > 0.7 of true -> degraded; false -> healthy end
    end.

%% @private Check disk space
check_disk_space() ->
    case disk_space_check() of
        {ok, Usage} -> case Usage > 0.9 of true -> unhealthy; false -> case Usage > 0.8 of true -> degraded; false -> healthy end end;
        {error, _} -> unhealthy
    end.

%% @private Check network connectivity
check_network_connectivity() ->
    case network_connectivity_check() of
        true -> healthy;
        false -> unhealthy
    end.

%% @private Check database connections
check_database_connections() ->
    case database_connection_check() of
        ok -> healthy;
        {error, _} -> unhealthy
    end.

%% @private Get system load
get_system_load() ->
    case os:type() of
        {unix, _} ->
            case file:read_file("/proc/loadavg") of
                {ok, Content} ->
                    case binary:split(Content, <<" ">>, [global, trim]) of
                        [Load1, Load2, Load3 | _] ->
                            #{1min => binary_to_float(Load1),
                              5min => binary_to_float(Load2),
                              15min => binary_to_float(Load3)};
                        _ -> #{}
                    end;
                _ -> #{}
            end;
        _ -> #{}
    end.

%% @private Get load average
get_load_average() ->
    case os:type() of
        {unix, _} ->
            case file:read_file("/proc/loadavg") of
                {ok, Content} ->
                    case binary:split(Content, <<" ">>, [global, trim]) of
                        [Load1, Load2, Load3 | _] ->
                            #{1min => binary_to_float(Load1),
                              5min => binary_to_float(Load2),
                              15min => binary_to_float(Load3)};
                        _ -> #{}
                    end;
                _ -> #{}
            end;
        _ -> #{}
    end.

%% @private Get current version
erlmcp_version() ->
    case application:get_key(erlmcp_core, vsn) of
        {ok, Version} -> Version;
        _ -> "unknown"
    end.

%% @private Get timestamp
timestamp() ->
    erlang:system_time(millisecond).

%% @private Disk space check (placeholder)
disk_space_check() ->
    %% Implement actual disk space check
    {ok, 0.5}.

%% @private Network connectivity check (placeholder)
network_connectivity_check() ->
    %% Implement actual network connectivity check
    true.

%% @private Database connection check (placeholder)
database_connection_check() ->
    %% Implement actual database connection check
    ok.

%% @private Get readiness metrics
get_readiness_metrics() ->
    %% Implement actual readiness metrics collection
    [].