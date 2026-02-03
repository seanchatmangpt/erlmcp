%% @doc Enterprise Health Monitor
%% Monitors health of all enterprise integrations
-module(erlmcp_enterprise_health).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([check_health/1, get_health_status/1, get_all_health_status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    health_status :: map(),  % Service -> health status
    check_intervals :: map(),  % Service -> interval in ms
    last_checks :: map(),  % Service -> last check time
    failed_checks :: map()  % Service -> consecutive failures
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec check_health(Service :: atom()) -> {ok, map()} | {error, term()}.
check_health(Service) ->
    gen_server:call(?MODULE, {check_health, Service}).

-spec get_health_status(Service :: atom()) -> {ok, map()} | {error, not_found}.
get_health_status(Service) ->
    gen_server:call(?MODULE, {get_health_status, Service}).

-spec get_all_health_status() -> map().
get_all_health_status() ->
    gen_server:call(?MODULE, {get_all_health_status}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Initialize health status
    HealthStatus = #{
        identity => #{status => unknown, last_check => 0, failures => 0},
        monitoring => #{status => unknown, last_check => 0, failures => 0},
        logging => #{status => unknown, last_check => 0, failures => 0},
        business_intel => #{status => unknown, last_check => 0, failures => 0},
        service_bus => #{status => unknown, last_check => 0, failures => 0},
        data_warehouse => #{status => unknown, last_check => 0, failures => 0},
        devops => #{status => unknown, last_check => 0, failures => 0},
        api_gateway => #{status => unknown, last_check => 0, failures => 0},
        cloud => #{status => unknown, last_check => 0, failures => 0},
        security => #{status => unknown, last_check => 0, failures => 0},
        config_mgmt => #{status => unknown, last_check => 0, failures => 0},
        container => #{status => unknown, last_check => 0, failures => 0}
    },

    %% Set check intervals (in milliseconds)
    CheckIntervals = #{
        identity => 30000,  % 30 seconds
        monitoring => 60000,  % 1 minute
        logging => 30000,
        business_intel => 60000,
        service_bus => 30000,
        data_warehouse => 60000,
        devops => 30000,
        api_gateway => 60000,
        cloud => 60000,
        security => 30000,
        config_mgmt => 60000,
        container => 30000
    },

    State = #state{
        health_status = HealthStatus,
        check_intervals = CheckIntervals,
        last_checks = #{},
        failed_checks = #{}
    },

    %% Start health check timers
    start_health_check_timers(State),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({check_health, Service}, _From, State) ->
    case maps:find(Service, State#state.health_status) of
        {ok, _} ->
            %% Perform health check
            Health = check_service_health(Service),
            LastChecks = maps:put(Service, erlang:system_time(millisecond), State#state.last_checks),

            %% Update health status
            FailedChecks = case Health#{status} of
                healthy ->
                    maps:put(Service, 0, State#state.failed_checks);
                unhealthy ->
                    Current = maps:get(Service, State#state.failed_checks, 0),
                    maps:put(Service, Current + 1, State#state.failed_checks);
                _ ->
                    State#state.failed_checks
            end,

            NewHealthStatus = maps:put(Service, Health, State#state.health_status),

            %% Check if service is degraded
            case Health#{status} of
                degraded ->
                    erlmcp_enterprise_bus:publish(service_degraded, {Service, Health});
                unhealthy ->
                    erlmcp_enterprise_bus:publish(service_unhealthy, {Service, Health});
                _ ->
                    ok
            end,

            {reply, {ok, Health}, State#state{
                health_status = NewHealthStatus,
                last_checks = LastChecks,
                failed_checks = FailedChecks
            }};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_health_status, Service}, _From, State) ->
    case maps:find(Service, State#state.health_status) of
        {ok, Health} ->
            {reply, {ok, Health}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_all_health_status}, _From, State) ->
    {reply, {ok, State#state.health_status}, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info({health_check, Service}, State) ->
    %% Perform periodic health check
    case maps:find(Service, State#state.health_status) of
        {ok, _} ->
            {reply, {ok, Health}, State} = handle_call({check_health, Service}, self(), State),
            {noreply, State};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec start_health_check_timers(#state{}) -> ok.
start_health_check_timers(State) ->
    maps:fold(fun(Service, Interval, _Acc) ->
        erlang:send_after(Interval, self(), {health_check, Service}),
        erlang:send_after(Interval, self(), schedule_check),
        ok
    end, ok, State#state.check_intervals).

-spec check_service_health(atom()) -> map().
check_service_health(identity) ->
    %% Check identity provider connections
    case erlmcp_identity_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(monitoring) ->
    %% Check monitoring system connections
    case erlmcp_monitoring_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(logging) ->
    %% Check logging platform connections
    case erlmcp_logging_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(business_intel) ->
    %% Check BI tool connections
    case erlmcp_bizintel_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(service_bus) ->
    %% Check service bus connections
    case erlmcp_servicebus_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(data_warehouse) ->
    %% Check data warehouse connections
    case erlmcp_data_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(devops) ->
    %% Check DevOps tool connections
    case erlmcp_devops_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(api_gateway) ->
    %% Check API gateway connections
    case erlmcp_apigw_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(cloud) ->
    %% Check cloud platform connections
    case erlmcp_cloud_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(security) ->
    %% Check security system connections
    case erlmcp_security_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false => unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(config_mgmt) ->
    %% Check configuration management connections
    case erlmcp_config_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(container) ->
    %% Check container orchestration connections
    case erlmcp_container_adapter:status() of
        {ok, Status} ->
            #{
                status => case Status#{connected} of
                    true -> healthy;
                    false -> unhealthy
                end,
                details => Status,
                timestamp => erlang:system_time(millisecond)
            };
        {error, Reason} ->
            #{
                status => unhealthy,
                error => Reason,
                timestamp => erlang:system_time(millisecond)
            }
    end;

check_service_health(Service) ->
    %% Unknown service
    #{
        status => unknown,
        service => Service,
        timestamp => erlang:system_time(millisecond)
    }.