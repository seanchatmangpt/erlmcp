-module(erlmcp_graceful_degradation).

%% API
-export([
    init/0,
    register_service/3, unregister_service/1,
    degrade_service/2, restore_service/1,
    get_service_level/1, get_system_health/0,
    set_degradation_policy/2, trigger_degradation/2
]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type service_id() :: atom().
-type service_level() :: operational | degraded | critical | offline.
-type criticality() :: critical | high | medium | low.

-record(service_info, {
    id :: service_id(),
    pid :: pid() | undefined,
    level = operational :: service_level(),
    criticality :: criticality(),
    degradation_policy :: map(),
    last_degraded :: undefined | erlang:timestamp(),
    degradation_count = 0 :: non_neg_integer()
}).

%% ETS table for service tracking
-define(SERVICES_TABLE, erlmcp_services).

%%====================================================================
%% API Functions
%%====================================================================

-spec init() -> ok.
init() ->
    case ets:info(?SERVICES_TABLE) of
        undefined ->
            ets:new(?SERVICES_TABLE, [named_table, public, {keypos, #service_info.id}]),
            ok;
        _ ->
            ok
    end.

-spec register_service(service_id(), pid(), #{criticality := criticality()}) -> ok.
register_service(ServiceId, Pid, Config) ->
    init(),
    Criticality = maps:get(criticality, Config, medium),
    DegradationPolicy = maps:get(degradation_policy, Config, default_policy()),
    
    Service = #service_info{
        id = ServiceId,
        pid = Pid,
        criticality = Criticality,
        degradation_policy = DegradationPolicy
    },
    
    ets:insert(?SERVICES_TABLE, Service),
    
    % Monitor the service process
    monitor(process, Pid),
    
    ?LOG_INFO("Service ~p registered with criticality ~p", [ServiceId, Criticality]),
    ok.

-spec unregister_service(service_id()) -> ok.
unregister_service(ServiceId) ->
    case ets:lookup(?SERVICES_TABLE, ServiceId) of
        [#service_info{pid = Pid}] when is_pid(Pid) ->
            demonitor(Pid, [flush]);
        _ ->
            ok
    end,
    ets:delete(?SERVICES_TABLE, ServiceId),
    ok.

-spec degrade_service(service_id(), service_level()) -> ok | {error, term()}.
degrade_service(ServiceId, TargetLevel) ->
    case ets:lookup(?SERVICES_TABLE, ServiceId) of
        [Service] ->
            NewService = Service#service_info{
                level = TargetLevel,
                last_degraded = erlang:timestamp(),
                degradation_count = Service#service_info.degradation_count + 1
            },
            ets:insert(?SERVICES_TABLE, NewService),
            
            ?LOG_WARNING("Service ~p degraded to level ~p", [ServiceId, TargetLevel]),
            
            % Notify service of degradation if it supports it
            case Service#service_info.pid of
                Pid when is_pid(Pid) ->
                    Pid ! {service_degradation, TargetLevel};
                _ ->
                    ok
            end,
            
            % Check if system-wide degradation is needed
            maybe_trigger_system_degradation(),
            ok;
        [] ->
            {error, service_not_found}
    end.

-spec restore_service(service_id()) -> ok | {error, term()}.
restore_service(ServiceId) ->
    case ets:lookup(?SERVICES_TABLE, ServiceId) of
        [Service] ->
            NewService = Service#service_info{
                level = operational
            },
            ets:insert(?SERVICES_TABLE, NewService),
            
            ?LOG_INFO("Service ~p restored to operational level", [ServiceId]),
            
            % Notify service of restoration
            case Service#service_info.pid of
                Pid when is_pid(Pid) ->
                    Pid ! {service_restoration, operational};
                _ ->
                    ok
            end,
            ok;
        [] ->
            {error, service_not_found}
    end.

-spec get_service_level(service_id()) -> service_level() | not_found.
get_service_level(ServiceId) ->
    case ets:lookup(?SERVICES_TABLE, ServiceId) of
        [#service_info{level = Level}] -> Level;
        [] -> not_found
    end.

-spec get_system_health() -> map().
get_system_health() ->
    AllServices = ets:tab2list(?SERVICES_TABLE),
    
    % Categorize services by level
    ServicesByLevel = lists:foldl(fun(Service, Acc) ->
        Level = Service#service_info.level,
        Current = maps:get(Level, Acc, []),
        Acc#{Level => [Service#service_info.id | Current]}
    end, #{}, AllServices),
    
    % Count services by criticality and level
    CriticalServices = [S || S <- AllServices, S#service_info.criticality =:= critical],
    CriticalOperational = [S || S <- CriticalServices, S#service_info.level =:= operational],
    
    % Determine overall system health
    SystemLevel = determine_system_level(AllServices),
    
    #{
        system_level => SystemLevel,
        services_by_level => ServicesByLevel,
        total_services => length(AllServices),
        critical_services => length(CriticalServices),
        critical_operational => length(CriticalOperational),
        degradation_summary => get_degradation_summary(AllServices)
    }.

-spec set_degradation_policy(service_id(), map()) -> ok | {error, term()}.
set_degradation_policy(ServiceId, Policy) ->
    case ets:lookup(?SERVICES_TABLE, ServiceId) of
        [Service] ->
            NewService = Service#service_info{
                degradation_policy = Policy
            },
            ets:insert(?SERVICES_TABLE, NewService),
            ok;
        [] ->
            {error, service_not_found}
    end.

-spec trigger_degradation(service_id(), term()) -> ok.
trigger_degradation(ServiceId, Reason) ->
    case ets:lookup(?SERVICES_TABLE, ServiceId) of
        [Service] ->
            Policy = Service#service_info.degradation_policy,
            TargetLevel = determine_degradation_level(Reason, Policy),
            degrade_service(ServiceId, TargetLevel);
        [] ->
            ?LOG_WARNING("Cannot degrade unknown service ~p", [ServiceId]),
            ok
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec default_policy() -> map().
default_policy() ->
    #{
        max_failures => 3,
        degradation_timeout => 30000, % 30 seconds
        auto_restore => true,
        failure_thresholds => #{
            high_memory => degraded,
            process_death => critical,
            timeout => degraded,
            overload => degraded
        }
    }.

-spec determine_degradation_level(term(), map()) -> service_level().
determine_degradation_level(Reason, Policy) ->
    Thresholds = maps:get(failure_thresholds, Policy, #{}),
    maps:get(Reason, Thresholds, degraded).

-spec determine_system_level([#service_info{}]) -> service_level().
determine_system_level(Services) ->
    CriticalServices = [S || S <- Services, S#service_info.criticality =:= critical],
    
    case CriticalServices of
        [] ->
            % No critical services, check overall health
            determine_general_system_level(Services);
        CriticalList ->
            % Check critical services first
            CriticalLevels = [S#service_info.level || S <- CriticalList],
            case lists:any(fun(Level) -> Level =:= offline end, CriticalLevels) of
                true -> critical;
                false ->
                    case lists:any(fun(Level) -> Level =:= critical end, CriticalLevels) of
                        true -> critical;
                        false ->
                            case lists:any(fun(Level) -> Level =:= degraded end, CriticalLevels) of
                                true -> degraded;
                                false -> operational
                            end
                    end
            end
    end.

-spec determine_general_system_level([#service_info{}]) -> service_level().
determine_general_system_level(Services) ->
    case Services of
        [] -> operational;
        _ ->
            Levels = [S#service_info.level || S <- Services],
            OfflineCount = length([L || L <- Levels, L =:= offline]),
            CriticalCount = length([L || L <- Levels, L =:= critical]),
            DegradedCount = length([L || L <- Levels, L =:= degraded]),
            Total = length(Levels),
            
            if
                OfflineCount >= Total * 0.5 -> critical;  % 50% offline
                CriticalCount >= Total * 0.3 -> critical; % 30% critical
                DegradedCount >= Total * 0.6 -> degraded; % 60% degraded
                true -> operational
            end
    end.

-spec get_degradation_summary([#service_info{}]) -> map().
get_degradation_summary(Services) ->
    DegradedServices = [S || S <- Services, S#service_info.level =/= operational],
    
    #{
        total_degraded => length(DegradedServices),
        degradation_events => lists:sum([S#service_info.degradation_count || S <- Services]),
        recent_degradations => count_recent_degradations(Services, 300) % Last 5 minutes
    }.

-spec count_recent_degradations([#service_info{}], pos_integer()) -> non_neg_integer().
count_recent_degradations(Services, Seconds) ->
    Now = erlang:timestamp(),
    Threshold = Now - {0, 0, Seconds * 1000000},
    
    length([S || S <- Services, 
             S#service_info.last_degraded =/= undefined,
             S#service_info.last_degraded > Threshold]).

-spec maybe_trigger_system_degradation() -> ok.
maybe_trigger_system_degradation() ->
    SystemHealth = get_system_health(),
    SystemLevel = maps:get(system_level, SystemHealth),
    
    case SystemLevel of
        operational ->
            ok;
        degraded ->
            ?LOG_WARNING("System is in degraded state", []),
            % Could trigger additional degradation measures
            ok;
        critical ->
            ?LOG_ERROR("System is in critical state", []),
            % Could trigger emergency procedures
            ok
    end.