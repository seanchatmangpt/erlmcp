%% @doc Enterprise Monitoring Integration Example
%% Demonstrates integration with monitoring systems (Splunk, Datadog, New Relic)
-module(monitoring_integration_example).

-export([main/0, setup_monitoring_providers/0, test_metric_collection/1,
         test_log_collection/1, test_alerting/2, test_dashboards/1]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Main Functions
%%====================================================================

-spec main() -> ok.
main() ->
    %% Start the enterprise integrations
    ok = start_integrations(),

    %% Setup monitoring providers
    ok = setup_monitoring_providers(),

    %% Test metric collection
    case test_metric_collection(splunk) of
        ok ->
            ?LOG_INFO("Metric collection test successful"),

            %% Test log collection
            case test_log_collection(splunk) of
                ok ->
                    ?LOG_INFO("Log collection test successful"),

                    %% Test alerting
                    AlertRule = #{
                        query => "index=erlmcp error_count > 100",
                        condition => "> 100",
                        message => "High error rate detected",
                        email => ["admin@example.com"]
                    },
                    test_alerting(splunk, AlertRule),

                    %% Test dashboards
                    test_dashboards(splunk);
                {error, Reason} ->
                    ?LOG_ERROR("Log collection failed: ~p", [Reason])
            end;
        {error, Reason} ->
            ?LOG_ERROR("Metric collection failed: ~p", [Reason])
    end.

-spec start_integrations() -> ok.
start_integrations() ->
    %% Start the enterprise integrations application
    case application:ensure_all_started(erlmcp_enterprise_integrations) of
        {ok, _} -> ok;
        {error, Reason} -> throw({failed_to_start_integrations, Reason})
    end.

-spec setup_monitoring_providers() -> ok.
setup_monitoring_providers() ->
    %% Configure and start monitoring system adapters

    %% Splunk Configuration
    splunk_config = #{
        provider => splunk,
        endpoint => "https://your-splunk-instance:8088",
        api_key => <<"your-splunk-api-key">>,
        index => "erlmcp",
        timeout => 30000
    },

    case erlmcp_monitoring_adapter:configure(splunk, splunk_config) of
        ok -> ?LOG_INFO("Splunk adapter configured");
        {error, Reason} -> ?LOG_ERROR("Failed to configure Splunk: ~p", [Reason])
    end,

    %% Datadog Configuration
    datadog_config = #{
        provider => datadog,
        endpoint => "https://api.datadoghq.com",
        api_key => <<"your-datadog-api-key">>,
        application_key => <<"your-datadog-app-key">>,
        timeout => 30000
    },

    case erlmcp_monitoring_adapter:configure(datadog, datadog_config) of
        ok -> ?LOG_INFO("Datadog adapter configured");
        {error, Reason} -> ?LOG_ERROR("Failed to configure Datadog: ~p", [Reason])
    end,

    %% New Relic Configuration
    newrelic_config = #{
        provider => newrelic,
        endpoint => "https://api.newrelic.com",
        api_key => <<"your-newrelic-api-key">>,
        account_id => <<"your-account-id">>,
        timeout => 30000
    },

    case erlmcp_monitoring_adapter:configure(newrelic, newrelic_config) of
        ok -> ?LOG_INFO("New Relic adapter configured");
        {error, Reason} -> ?LOG_ERROR("Failed to configure New Relic: ~p", [Reason])
    end,

    ok.

-spec test_metric_collection(provider()) -> ok | {error, term()}.
test_metric_collection(Provider) ->
    %% Send various types of metrics to the monitoring system
    Metrics = [
        #{
            name => <<"erlmcp.requests">>,
            type => counter,
            data => #{
                value => 1000,
                tags => #{
                    endpoint => <<"/api/users">>,
                    method => <<"GET">>,
                    status => <<"200">>
                }
            }
        },
        #{
            name => <<"erlmcp.response_time">>,
            type => gauge,
            data => #{
                value => 150.5,
                tags => #{
                    endpoint => <<"/api/users">>,
                    method => <<"GET">>
                }
            }
        },
        #{
            name => <<"erlmcp.error_count">>,
            type => histogram,
            data => #{
                value => 5,
                tags => #{
                    error_type => <<"timeout">>
                }
            }
        }
    ],

    lists:foldl(fun(Metric, Acc) ->
        case erlmcp_monitoring_adapter:send_metric(
               maps:get(name, Metric),
               maps:get(type, Metric),
               maps:get(data, Metric)
           ) of
            {ok, _} -> Acc;
            {error, Reason} ->
                ?LOG_ERROR("Failed to send metric ~p: ~p", [maps:get(name, Metric), Reason]),
                {error, Reason}
        end
    end, ok, Metrics).

-spec test_log_collection(provider()) -> ok | {error, term()}.
test_log_collection(Provider) ->
    %% Send various types of log entries to the monitoring system
    LogEntries = [
        #{
            category => <<"application">>,
            data => #{
                level => info,
                message => <<"Application started successfully">>,
                component => <<"erlmcp_server">>,
                node => list_to_binary(atom_to_list(node()))
            }
        },
        #{
            category => <<"error">>,
            data => #{
                level => error,
                message => <<"Failed to connect to database">>,
                error => <<"connection_timeout">>,
                stack_trace => <<"stack_trace_here">>
            }
        },
        #{
            category => <<"audit">>,
            data => #{
                level => info,
                message => <<"User authentication successful">>,
                user_id => <<"user123">>,
                action => <<"login">>,
                ip_address => <<"192.168.1.100">>
            }
        }
    ],

    lists:foldl(fun(Log, Acc) ->
        Context = #{
            timestamp => erlang:system_time(millisecond),
            trace_id => generate_uuid()
        },
        case erlmcp_monitoring_adapter:send_log(
               maps:get(category, Log),
               maps:get(data, Log),
               Context
           ) of
            {ok, _} -> Acc;
            {error, Reason} ->
                ?LOG_ERROR("Failed to send log ~p: ~p", [maps:get(category, Log), Reason]),
                {error, Reason}
        end
    end, ok, LogEntries).

-spec test_alerting(provider(), map()) -> ok | {error, term()}.
test_alerting(Provider, AlertRule) ->
    %% Set up an alert rule in the monitoring system
    AlertName = "HighErrorRate_" ++ generate_timestamp(),

    case erlmcp_monitoring_adapter:setup_alert(AlertName, AlertRule, #{}) of
        {ok, AlertId} ->
            ?LOG_INFO("Alert set up: ~p (~p)", [AlertName, AlertId]),

            %% Test the alert by sending trigger data
            TriggerMetric = #{
                name => <<"erlmcp.error_count">>,
                type => counter,
                data => #{
                    value => 150,
                    tags => #{
                        error_type => <<"timeout">>
                    }
                }
            },

            erlmcp_monitoring_adapter:send_metric(
                maps:get(name, TriggerMetric),
                maps:get(type, TriggerMetric),
                maps:get(data, TriggerMetric)
            );

        {error, Reason} ->
            ?LOG_ERROR("Failed to set up alert: ~p", [Reason]),
            {error, Reason}
    end.

-spec test_dashboards(provider()) -> ok | {error, term()}.
test_dashboards(Provider) ->
    %% Create and retrieve dashboards
    DashboardConfigs = [
        #{
            name => "SystemOverview",
            description => "System-wide overview dashboard",
            panels => [
                #{
                    title => "Request Rate",
                    type => "timeseries",
                    query => "erlmcp.requests{*}"
                },
                #{
                    title => "Response Time",
                    type => "timeseries",
                    query => "erlmcp.response_time{*}"
                }
            ]
        },
        #{
            name => "ErrorAnalysis",
            description => "Error analysis dashboard",
            panels => [
                #{
                    title => "Error Count",
                    type => "timeseries",
                    query => "erlmcp.error_count{*}"
                },
                #{
                    title => "Error Distribution",
                    type => "pie",
                    query => "sum(erlmcp.error_count{*} by {error_type})"
                }
            ]
        }
    ],

    lists:foldl(fun(Config, Acc) ->
        case erlmcp_monitoring_adapter:create_dashboard(
               maps:get(name, Config),
               Config,
               #{}
           ) of
            {ok, DashboardId} ->
                ?LOG_INFO("Dashboard created: ~p (~p)", [maps:get(name, Config), DashboardId]),

                %% Retrieve the dashboard
                case erlmcp_monitoring_adapter:get_dashboard(
                       maps:get(name, Config),
                       #{}
                   ) of
                    {ok, DashboardData} ->
                        ?LOG_INFO("Dashboard retrieved: ~p", [maps:get(name, Config)]),
                        Acc;
                    {error, Reason} ->
                        ?LOG_ERROR("Failed to retrieve dashboard: ~p", [Reason]),
                        {error, Reason}
                end;
            {error, Reason} ->
                ?LOG_ERROR("Failed to create dashboard: ~p", [Reason]),
                {error, Reason}
        end
    end, ok, DashboardConfigs).

%%====================================================================
%% Helper Functions
%%====================================================================

generate_uuid() ->
    %% Generate a unique identifier for the request
    binary_to_list(erlang:ref_to_list(make_ref())).

generate_timestamp() ->
    %% Generate a timestamp string
    lists:flatten(io_lib:format("~p", [erlang:system_time(millisecond)])).