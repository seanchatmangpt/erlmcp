-module(erlmcp_api_health).

-behaviour(gen_server).

%% API exports
-export([start_link/0, check_health/0, check_service_health/1, get_health_status/0,
         register_health_check/1, unregister_health_check/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp_api_gateway.hrl").

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_health() ->
    gen_server:call(?MODULE, check_health).

check_service_health(Service) ->
    gen_server:call(?MODULE, {check_service_health, Service}).

get_health_status() ->
    gen_server:call(?MODULE, get_health_status).

register_health_check(HealthCheck) ->
    gen_server:cast(?MODULE, {register_health_check, HealthCheck}).

unregister_health_check(ServiceId) ->
    gen_server:cast(?MODULE, {unregister_health_check, ServiceId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize health check storage
    ets:new(health_checks, [
        named_table,
        public,
        set,
        {keypos, 2}
    ]),

    ets:new(health_status, [
        named_table,
        public,
        set,
        {keypos, 2}
    ]),

    ets:new(health_history, [
        named_table,
        public,
        bag,
        {keypos, 3}
    ]),

    %% Load configuration
    Config = load_health_config(),

    %% Register default health checks
    register_default_health_checks(),

    %% Start health check scheduler
    erlang:send_after(1000, self(), run_health_checks),

    State = #{
        config => Config,
        health_checks => ets:tab2list(health_checks),
        status => #{}
    },

    {ok, State}.

handle_call(check_health, _From, State) ->
    %% Check overall system health
    Status = calculate_overall_health(State),

    {reply, {ok, Status}, State};

handle_call({check_service_health, Service}, _From, State) ->
    case check_specific_service(Service, State) of
        {ok, Status} ->
            {reply, {ok, Status}, State};
        {error, not_found} ->
            {reply, {error, service_not_found}, State}
    end;

handle_call(get_health_status, _From, State) ->
    %% Get current health status
    Status = get_current_health_status(State),
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({register_health_check, HealthCheck}, State) ->
    %% Register new health check
    ServiceId = HealthCheck#health_check.service_id,
    CheckFun = HealthCheck#health_check.check_function,
    Interval = HealthCheck#health_check.interval,
    Timeout = HealthCheck#health_check.timeout,
    Critical = HealthCheck#health_check.critical,

    NewHealthCheck = HealthCheck#health_check{
        registered_at => erlang:system_time(millisecond),
        last_checked => 0,
        status => unknown
    },

    %% Store health check
    true = ets:insert(health_checks, {ServiceId, NewHealthCheck}),

    %% Initialize status
    true = ets:insert(health_status, {ServiceId, unknown, 0}),

    %% Store in history
    true = ets:insert(health_history, {ServiceId, erlang:system_time(millisecond), unknown}),

    {noreply, State#{health_checks => ets:tab2list(health_checks)}};

handle_cast({unregister_health_check, ServiceId}, State) ->
    %% Remove health check
    true = ets:delete(health_checks, ServiceId),
    true = ets:delete(health_status, ServiceId),

    %% Keep in history for retention
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(run_health_checks, State) ->
    %% Run all health checks
    Now = erlang:system_time(millisecond),

    %% Get all health checks
    HealthChecks = ets:tab2list(health_checks),

    %% Run each health check
    lists:foreach(fun({ServiceId, HealthCheck}) ->
        NextCheck = HealthCheck#health_check.last_checked + HealthCheck#health_check.interval,

        if
            Now >= NextCheck ->
                run_individual_health_check(ServiceId, HealthCheck),
                true = ets:insert(health_checks, {ServiceId, HealthCheck#health_check{last_checked = Now}});
            true ->
                ok
        end
    end, HealthChecks),

    %% Schedule next run
    NextRun = min_interval(HealthChecks),
    erlang:send_after(NextRun, self(), run_health_checks),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_health_config() ->
    %% Load health check configuration
    #{
        check_interval => 30000, %% 30 seconds
        timeout => 5000, %% 5 seconds
        critical_threshold => 3, %% 3 failures before critical
        retention_period => 86400000 %% 24 hours
    }.

register_default_health_checks() ->
    %% Register default health checks for all services
    %% API Gateway health check
    GatewayCheck = #health_check{
        service_id => gateway,
        name => <<"API Gateway">>,
        check_function = fun check_gateway_health/0,
        interval => 30000, %% 30 seconds
        timeout => 5000, %% 5 seconds
        critical => true,
        description => <<"Check API gateway connectivity and performance">>
    },

    %% Database health check
    DatabaseCheck = #health_check{
        service_id => database,
        name => <<"Database">>,
        check_function = fun check_database_health/0,
        interval => 60000, %% 60 seconds
        timeout => 10000, %% 10 seconds
        critical => true,
        description => <<"Check database connectivity and performance">>
    },

    %% Rate limiter health check
    RateLimiterCheck = #health_check{
        service_id => rate_limiter,
        name => <<"Rate Limiter">>,
        check_function = fun check_rate_limiter_health/0,
        interval => 30000, %% 30 seconds
        timeout => 2000, %% 2 seconds
        critical => false,
        description => <<"Check rate limiter service">>
    },

    %% Analytics health check
    AnalyticsCheck = #health_check{
        service_id => analytics,
        name => <<"Analytics">>,
        check_function = fun check_analytics_health/0,
        interval => 60000, %% 60 seconds
        timeout => 5000, %% 5 seconds
        critical => false,
        description => <<"Check analytics service">>
    },

    %% Authentication health check
    AuthCheck = #health_check{
        service_id => authentication,
        name => <<"Authentication">>,
        check_function = fun check_authentication_health/0,
        interval => 30000, %% 30 seconds
        timeout => 3000, %% 3 seconds
        critical => true,
        description => <<"Check authentication service">>
    },

    %% Register all health checks
    true = ets:insert(health_checks, {gateway, GatewayCheck}),
    true = ets:insert(health_checks, {database, DatabaseCheck}),
    true = ets:insert(health_checks, {rate_limiter, RateLimiterCheck}),
    true = ets:insert(health_checks, {analytics, AnalyticsCheck}),
    true = ets:insert(health_checks, {authentication, AuthCheck}).

run_individual_health_check(ServiceId, HealthCheck) ->
    CheckFun = HealthCheck#health_check.check_function,
    Timeout = HealthCheck#health_check.timeout,

    %% Run health check with timeout
    case erlang:spawn_monitor(fun() ->
        try
            Result = CheckFun(),
            gen_server:call(?MODULE, {health_check_result, ServiceId, Result})
        catch
            _:Reason ->
                gen_server:call(?MODULE, {health_check_result, ServiceId, {error, Reason}})
        end
    end) of
        {Pid, Ref} ->
            receive
                {'DOWN', Ref, process, Pid, normal} ->
                    ok;
                {'DOWN', Ref, process, Pid, Reason} ->
                    gen_server:call(?MODULE, {health_check_result, ServiceId, {error, Reason}})
            after Timeout ->
                exit(Pid, kill),
                gen_server:call(?MODULE, {health_check_result, ServiceId, {error, timeout}})
            end
    end.

check_gateway_health() ->
    %% Check API gateway health
    #{
        status => healthy,
        response_time => measure_response_time(),
        throughput => get_throughput(),
        error_rate => get_error_rate()
    }.

check_database_health() ->
    %% Check database health
    case whereis(mnesia) of
        undefined ->
            {error, mnesia_not_running};
        _ ->
            case mnesia:system_info(is_running) of
                yes ->
                    #{
                        status => healthy,
                        ram_nodes => length(mnesia:system_info(db_nodes)),
                        disk_nodes => length(mnesia:system_info(disc_copies))
                    };
                no ->
                    {error, mnesia_not_running}
            end
    end.

check_rate_limiter_health() ->
    %% Check rate limiter health
    case whereis(erlmcp_api_rate_limiter) of
        undefined ->
            {error, rate_limiter_not_running};
        _ ->
            #{
                status => healthy,
                active_buckets => get_active_buckets(),
                total_buckets => get_total_buckets()
            }
    end.

check_analytics_health() ->
    %% Check analytics service health
    case whereis(erlmcp_api_analytics) of
        undefined ->
            {error, analytics_not_running};
        _ ->
            #{
                status => healthy,
                events_collected => get_events_collected(),
                retention_days => get_retention_days()
            }
    end.

check_authentication_health() ->
    %% Check authentication service health
    case whereis(erlmcp_api_auth) of
        undefined ->
            {error, auth_not_running};
        _ ->
            #{
                status => healthy,
                active_tokens => get_active_tokens(),
                consumers => get_consumer_count()
            }
    end.

measure_response_time() ->
    %% Measure API gateway response time
    Start = erlang:system_time(millisecond),
    %% Simulate API call
    case catch gen_server:call(?MODULE, ping) of
        ok ->
            erlang:system_time(millisecond) - Start;
        _ ->
            0
    end.

get_throughput() ->
    %% Get current throughput
    case ets:info(api_metrics, size) of
        undefined ->
            0;
        Size ->
            Size
    end.

get_error_rate() ->
    %% Get current error rate
    case ets:match_object(api_metrics, {error_count, '_'}) of
        [{_, Count}] ->
            Count;
        _ ->
            0
    end.

get_active_buckets() ->
    %% Get active rate limit buckets
    case ets:info(rate_limit_counters, size) of
        undefined ->
            0;
        Size ->
            Size
    end.

get_total_buckets() ->
    %% Get total buckets (this is a placeholder)
    1000.

get_events_collected() ->
    %% Get total events collected
    case ets:info(api_metrics, size) of
        undefined ->
            0;
        Size ->
            Size
    end.

get_retention_days() ->
    %% Get retention period in days
    30.

get_active_tokens() ->
    %% Get active tokens
    case ets:info(tokens, size) of
        undefined ->
            0;
        Size ->
            Size
    end.

get_consumer_count() ->
    %% Get consumer count
    case ets:info(consumers, size) of
        undefined ->
            0;
        Size ->
            Size
    end.

min_interval(HealthChecks) ->
    %% Calculate minimum interval for next health check run
    lists:min([Check#health_check.interval || {_, Check} <- HealthChecks]).

calculate_overall_health(State) ->
    %% Calculate overall system health
    HealthChecks = ets:tab2list(health_checks),

    lists:foldl(fun({ServiceId, HealthCheck}, Acc) ->
        case ets:lookup(health_status, ServiceId) of
            [{ServiceId, Status, Timestamp}] ->
                case Status of
                    healthy ->
                        Acc#{ServiceId => Status};
                    unhealthy ->
                        Acc#{ServiceId => Status};
                    critical ->
                        Acc#{ServiceId => Status};
                    unknown ->
                        Acc#{ServiceId => Status}
                end;
            [] ->
                Acc#{ServiceId => unknown}
        end
    end, #{}, HealthChecks).

check_specific_service(Service, State) ->
    case ets:lookup(health_status, Service) of
        [{Service, Status, Timestamp}] ->
            {ok, #{service => Service, status => Status, timestamp => Timestamp}};
        [] ->
            {error, not_found}
    end.

get_current_health_status(State) ->
    %% Get current health status
    HealthChecks = ets:tab2list(health_checks),

    lists:foldl(fun({ServiceId, _HealthCheck}, Acc) ->
        case ets:lookup(health_status, ServiceId) of
            [{ServiceId, Status, Timestamp}] ->
                Acc#{ServiceId => #{status => Status, timestamp => Timestamp}};
            [] ->
                Acc#{ServiceId => #{status => unknown, timestamp => 0}}
        end
    end, #{}, HealthChecks).

%% Handle health check results
handle_call({health_check_result, ServiceId, Result}, _From, State) ->
    Now = erlang:system_time(millisecond),

    %% Determine health status
    Status = case Result of
        #{status := healthy} -> healthy;
        #{status := unhealthy} -> unhealthy;
        {error, _} -> critical;
        _ -> unknown
    end,

    %% Update status
    true = ets:insert(health_status, {ServiceId, Status, Now}),

    %% Add to history
    true = ets:insert(health_history, {ServiceId, Now, Status}),

    %% Log health check result
    log_health_check(ServiceId, Status, Result),

    {reply, ok, State}.

log_health_check(ServiceId, Status, Result) ->
    %% Log health check result
    LogEntry = #{
        service_id => ServiceId,
        timestamp => erlang:system_time(millisecond),
        status => Status,
        result => Result
    },

    %% Keep logs in history for analysis
    true = ets:insert(health_history, {ServiceId, LogEntry}).