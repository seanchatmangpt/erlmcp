-module(erlmcp_health_check_registry).
-export([check/0, check/1]).

%% Registry health check implementation
%% Monitors gproc registry, service registration, and routing consistency

%% Registry info records
-record(registry_info, {
    registered_services :: list(),
    routing_table :: map(),
    gproc_stats :: map(),
    performance_metrics :: map(),
    consistency_metrics :: map()
}).

-record(registry_summary, {
    service_count :: non_neg_integer(),
    registered_count :: non_neg_integer(),
    critical_count :: non_neg_integer(),
    warning_count :: non_neg_integer(),
    error_message :: binary(),
    warning_message :: binary(),
    ok_message :: binary(),
    details :: map()
}).

check() ->
    check(#{}).

check(Options) ->
    try
        % Get registry information
        RegistryInfo = get_registry_info(Options),

        % Check registry consistency
        ConsistencyStatus = check_registry_consistency(RegistryInfo),

        % Check service registrations
        RegistrationStatus = check_service_registrations(RegistryInfo),

        % Check routing table
        RoutingStatus = check_routing_table(RegistryInfo),

        % Check registry performance
        PerformanceStatus = check_registry_performance(RegistryInfo),

        % Generate summary
        Summary = generate_registry_summary(ConsistencyStatus, RegistrationStatus,
                                           RoutingStatus, PerformanceStatus, RegistryInfo),

        case Summary#registry_summary.critical_count > 0 of
            true ->
                {error, Summary#registry_summary.error_message, Summary#registry_summary.details};
            _ when Summary#registry_summary.warning_count > 0 ->
                {warning, Summary#registry_summary.warning_message, Summary#registry_summary.details};
            true ->
                {ok, Summary#registry_summary.ok_message, Summary#registry_summary.details}
        end
    catch
        Error:Reason ->
            {error, io_lib:format("Registry check failed: ~p:~p", [Error, Reason]), #{error => {Error, Reason}}}
    end.

get_registry_info(Options) ->
    % Get registered services
    RegisteredServices = get_registered_services(),

    % Get routing table
    RoutingTable = get_routing_table(),

    % Get gproc statistics
    GProcStats = get_gproc_stats(),

    % Get performance metrics
    PerformanceMetrics = get_performance_metrics(),

    % Get consistency metrics
    ConsistencyMetrics = get_consistency_metrics(),

    #registry_info{
        registered_services = RegisteredServices,
        routing_table = RoutingTable,
        gproc_stats = GProcStats,
        performance_metrics = PerformanceMetrics,
        consistency_metrics = ConsistencyMetrics
    }.

get_registered_services() ->
    % Get list of registered services
    % This would typically query gproc registry
    try
        % Use gproc to get registered services
        Services = gproc:info(),
        lists:filtermap(fun({Key, Value}) ->
            case Key of
                {n, l, ServiceName} when is_atom(ServiceName) ->
                    {true, {ServiceName, Value}};
                {n, l, ServiceName} when is_binary(ServiceName) ->
                    {true, {binary_to_atom(ServiceName, utf8), Value}};
                _ ->
                    false
            end
        end, Services)
    catch
        _ ->
            []
    end.

get_routing_table() ->
    % Get routing table
    % This would typically inspect the routing registry
    try
        RoutingTable = get_routing_info_from_registry(),
        RoutingTable
    catch
        _ ->
            #{}
    end.

get_routing_info_from_registry() ->
    % Get routing information from the registry
    % This would inspect the actual routing table
    case erlang:whereis(erlmcp_mcp_proxy_relay) of
        undefined ->
            #{};
        Pid ->
            case process_info(Pid, [dictionary]) of
                Info when is_list(Info) ->
                    Dict = proplists:get_value(dictionary, Info, []),
                    case lists:keyfind('$routing_table', 1, Dict) of
                        {_, RoutingTable} ->
                            RoutingTable;
                        false ->
                            #{}
                    end;
                _ ->
                    #{}
            end
    end.

get_gproc_stats() ->
    % Get gproc statistics
    try
        Stats = gproc:info(),
        #{
            total_entries => length(Stats),
            node_count => node_count(),
            memory_usage => calculate_gproc_memory_usage(Stats)
        }
    catch
        _ ->
            #{}
    end.

get_performance_metrics() ->
    % Get performance metrics from the registry
    #{
        lookup_time => get_average_lookup_time(),
        registration_time => get_average_registration_time(),
        unregistration_time => get_average_unregistration_time(),
        error_rate => get_registry_error_rate()
    }.

get_consistency_metrics() ->
    % Get consistency metrics
    Metrics = #{
        replication_lag => get_replication_lag(),
        out_of_sync_services => get_out_of_sync_services()
    },
    Metrics#{consistency_score => get_consistency_score(Metrics)}.

node_count() ->
    % Get number of nodes in the cluster
    length(nodes()) + 1.

calculate_gproc_memory_usage(Stats) ->
    % Calculate memory usage of gproc registry
    length(Stats) * 128.  % Rough estimate per entry

get_average_lookup_time() ->
    % Get average lookup time
    0.

get_average_registration_time() ->
    % Get average registration time
    0.

get_average_unregistration_time() ->
    % Get average unregistration time
    0.

get_registry_error_rate() ->
    % Get registry error rate
    0.

get_replication_lag() ->
    % Get replication lag
    0.

get_consistency_score(_ConsistencyMetrics) ->
    % Get consistency score from metrics
    100.

get_out_of_sync_services() ->
    % Get list of out-of-sync services
    [].

check_registry_consistency(#registry_info{consistency_metrics = ConsistencyMetrics}) ->
    % Check registry consistency
    case get_consistency_score(ConsistencyMetrics) of
        Score when Score < 90 -> error;
        Score when Score < 95 -> warning;
        _ -> ok
    end.

check_service_registrations(#registry_info{registered_services = Services}) ->
    % Check service registrations
    ServiceCount = length(Services),
    if
        ServiceCount == 0 -> warning;
        ServiceCount > 1000 -> error;
        ServiceCount > 500 -> warning;
        true -> ok
    end.

check_routing_table(#registry_info{routing_table = RoutingTable}) ->
    % Check routing table integrity
    case validate_routing_table(RoutingTable) of
        ok -> ok;
        {warning, _} -> warning;
        {error, _} -> error
    end.

check_registry_performance(#registry_info{performance_metrics = Performance}) ->
    % Check registry performance
    ErrorRate = maps:get(error_rate, Performance, 0),
    if
        ErrorRate > 0.1 -> error;
        ErrorRate > 0.05 -> warning;
        true -> ok
    end.

validate_routing_table(RoutingTable) ->
    % Validate routing table integrity
    case maps:size(RoutingTable) of
        0 -> {warning, "Empty routing table"};
        _ ->
            % Check for valid entries
            ValidEntries = maps:filter(fun(_Service, Routes) ->
                is_valid_route_entry(Routes)
            end, RoutingTable),

            ValidCount = maps:size(ValidEntries),
            TotalCount = maps:size(RoutingTable),

            if
                ValidCount / TotalCount < 0.8 -> {error, "High rate of invalid routing entries"};
                ValidCount / TotalCount < 0.95 -> {warning, "Some invalid routing entries"};
                true -> ok
            end
    end.

is_valid_route_entry(Routes) when is_list(Routes) ->
    % Check if route entry is valid
    lists:all(fun(Route) ->
        is_map(Route) andalso is_binary(maps:get(host, Route, undefined))
    end, Routes);
is_valid_route_entry(_) ->
    false.

generate_registry_summary(ConsistencyStatus, RegistrationStatus, RoutingStatus, PerformanceStatus, RegistryInfo) ->
    WarningCount = lists:sum([
        1 || Status <- [ConsistencyStatus, RegistrationStatus, RoutingStatus, PerformanceStatus],
             Status =:= warning
    ]),

    CriticalCount = lists:sum([
        1 || Status <- [ConsistencyStatus, RegistrationStatus, RoutingStatus, PerformanceStatus],
             Status =:= critical
    ]),

    Details = #{
        consistency_status => ConsistencyStatus,
        registration_status => RegistrationStatus,
        routing_status => RoutingStatus,
        performance_status => PerformanceStatus,
        warning_count => WarningCount,
        critical_count => CriticalCount
    },

    case CriticalCount > 0 of
        true ->
            #registry_summary{
                service_count = maps:size(RegistryInfo#registry_info.routing_table),
                registered_count = length(RegistryInfo#registry_info.registered_services),
                critical_count = CriticalCount,
                warning_count = WarningCount,
                error_message = <<"Critical registry issues detected">>,
                warning_message = <<"Registry issues present">>,
                ok_message = <<"Registry usage within normal bounds">>,
                details = Details
            };
        _ when WarningCount > 0 ->
            #registry_summary{
                service_count = maps:size(RegistryInfo#registry_info.routing_table),
                registered_count = length(RegistryInfo#registry_info.registered_services),
                critical_count = CriticalCount,
                warning_count = WarningCount,
                error_message = <<"Registry issues detected">>,
                warning_message = <<"Registry usage elevated">>,
                ok_message = <<"Registry usage within normal bounds">>,
                details = Details
            };
        true ->
            #registry_summary{
                service_count = maps:size(RegistryInfo#registry_info.routing_table),
                registered_count = length(RegistryInfo#registry_info.registered_services),
                critical_count = CriticalCount,
                warning_count = WarningCount,
                error_message = <<"">>,
                warning_message = <<"">>,
                ok_message = <<"Registry usage within normal bounds">>,
                details = Details
            }
    end.