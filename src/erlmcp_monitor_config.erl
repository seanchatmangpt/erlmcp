%%%-------------------------------------------------------------------
%%% @doc ErlMCP Monitor Configuration Management
%%% Manages alert rules, thresholds, and monitoring configuration.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_monitor_config).

%% API
-export([
    load_config/0,
    load_config/1,
    save_config/1,
    validate_config/1,
    get_alert_rules/0,
    create_alert_rule/1,
    update_alert_rule/2,
    delete_alert_rule/1,
    get_default_thresholds/0,
    merge_configs/2
]).

%% Types
-type config() :: #{
    check_interval_ms := non_neg_integer(),
    alert_cooldown_ms := non_neg_integer(),
    health_check_timeout_ms := non_neg_integer(),
    metrics_retention_hours := non_neg_integer(),
    dashboard_enabled := boolean(),
    alert_handlers := [atom()],
    thresholds := thresholds(),
    alert_rules := [alert_rule_config()]
}.

-type thresholds() :: #{
    latency_warning_ms := number(),
    latency_critical_ms := number(),
    error_rate_warning_percent := number(),
    error_rate_critical_percent := number(),
    memory_usage_warning_percent := number(),
    memory_usage_critical_percent := number(),
    connection_failure_warning_rate := number(),
    connection_failure_critical_rate := number(),
    cpu_usage_warning_percent := number(),
    cpu_usage_critical_percent := number()
}.

-type alert_rule_config() :: #{
    id := binary(),
    name := binary(),
    description => binary(),
    metric := binary(),
    condition := binary(), % "gt", "lt", "eq", "ne"
    threshold := number(),
    severity := critical | warning | info,
    cooldown_ms := non_neg_integer(),
    enabled := boolean(),
    tags => [binary()]
}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Load configuration from default location
-spec load_config() -> {ok, config()} | {error, term()}.
load_config() ->
    load_config("config/monitor.config").

%% @doc Load configuration from file
-spec load_config(ConfigFile :: string()) -> {ok, config()} | {error, term()}.
load_config(ConfigFile) ->
    case file:consult(ConfigFile) of
        {ok, Terms} ->
            Config = proplists:get_value(monitor, Terms, #{}),
            case validate_config(Config) of
                ok -> {ok, merge_configs(get_default_config(), Config)};
                {error, Reason} -> {error, Reason}
            end;
        {error, enoent} ->
            {ok, get_default_config()};
        {error, Reason} ->
            {error, {config_load_failed, Reason}}
    end.

%% @doc Save configuration to file
-spec save_config(Config :: config()) -> ok | {error, term()}.
save_config(Config) ->
    case validate_config(Config) of
        ok ->
            ConfigFile = "config/monitor.config",
            filelib:ensure_dir(ConfigFile),
            
            Terms = [{monitor, Config}],
            Content = io_lib:format("~p.~n", [Terms]),
            
            file:write_file(ConfigFile, Content);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Validate configuration
-spec validate_config(Config :: map()) -> ok | {error, term()}.
validate_config(Config) when is_map(Config) ->
    try
        validate_required_fields(Config),
        validate_field_types(Config),
        validate_alert_rules(maps:get(alert_rules, Config, [])),
        validate_thresholds(maps:get(thresholds, Config, #{})),
        ok
    catch
        throw:{validation_error, Reason} -> {error, Reason};
        Error:Reason -> {error, {validation_failed, Error, Reason}}
    end;
validate_config(_) ->
    {error, config_must_be_map}.

%% @doc Get configured alert rules
-spec get_alert_rules() -> [alert_rule_config()].
get_alert_rules() ->
    {ok, Config} = load_config(),
    maps:get(alert_rules, Config, []).

%% @doc Create new alert rule
-spec create_alert_rule(Rule :: alert_rule_config()) -> ok | {error, term()}.
create_alert_rule(Rule) ->
    case validate_alert_rule(Rule) of
        ok ->
            {ok, Config} = load_config(),
            Rules = maps:get(alert_rules, Config, []),
            
            RuleId = maps:get(id, Rule),
            case lists:keyfind(RuleId, 2, Rules) of
                false ->
                    NewRules = [Rule | Rules],
                    NewConfig = maps:put(alert_rules, NewRules, Config),
                    save_config(NewConfig);
                _ ->
                    {error, rule_already_exists}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Update existing alert rule
-spec update_alert_rule(RuleId :: binary(), Updates :: map()) -> ok | {error, term()}.
update_alert_rule(RuleId, Updates) ->
    {ok, Config} = load_config(),
    Rules = maps:get(alert_rules, Config, []),
    
    case lists:keyfind(RuleId, 2, Rules) of
        false ->
            {error, rule_not_found};
        {_, ExistingRule} ->
            UpdatedRule = maps:merge(ExistingRule, Updates),
            case validate_alert_rule(UpdatedRule) of
                ok ->
                    NewRules = lists:keyreplace(RuleId, 2, Rules, {rule, UpdatedRule}),
                    NewConfig = maps:put(alert_rules, NewRules, Config),
                    save_config(NewConfig);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc Delete alert rule
-spec delete_alert_rule(RuleId :: binary()) -> ok | {error, term()}.
delete_alert_rule(RuleId) ->
    {ok, Config} = load_config(),
    Rules = maps:get(alert_rules, Config, []),
    
    case lists:keyfind(RuleId, 2, Rules) of
        false ->
            {error, rule_not_found};
        _ ->
            NewRules = lists:keydelete(RuleId, 2, Rules),
            NewConfig = maps:put(alert_rules, NewRules, Config),
            save_config(NewConfig)
    end.

%% @doc Get default thresholds
-spec get_default_thresholds() -> thresholds().
get_default_thresholds() ->
    #{
        latency_warning_ms => 1000,
        latency_critical_ms => 5000,
        error_rate_warning_percent => 5.0,
        error_rate_critical_percent => 10.0,
        memory_usage_warning_percent => 80.0,
        memory_usage_critical_percent => 95.0,
        connection_failure_warning_rate => 5.0,
        connection_failure_critical_rate => 15.0,
        cpu_usage_warning_percent => 70.0,
        cpu_usage_critical_percent => 90.0
    }.

%% @doc Merge two configurations
-spec merge_configs(Default :: config(), Override :: map()) -> config().
merge_configs(Default, Override) ->
    % Deep merge for nested maps
    maps:fold(
        fun(Key, Value, Acc) ->
            case {maps:get(Key, Acc, undefined), Value} of
                {ExistingMap, NewMap} when is_map(ExistingMap), is_map(NewMap) ->
                    maps:put(Key, maps:merge(ExistingMap, NewMap), Acc);
                {_, NewValue} ->
                    maps:put(Key, NewValue, Acc)
            end
        end,
        Default,
        Override
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Get default configuration
get_default_config() ->
    #{
        check_interval_ms => 5000,
        alert_cooldown_ms => 300000, % 5 minutes
        health_check_timeout_ms => 2000,
        metrics_retention_hours => 24,
        dashboard_enabled => true,
        alert_handlers => [console, log, webhook],
        thresholds => get_default_thresholds(),
        alert_rules => get_default_alert_rules()
    }.

%% @doc Get default alert rules configuration
get_default_alert_rules() ->
    [
        #{
            id => <<"system_high_latency">>,
            name => <<"System High Latency">>,
            description => <<"Alert when average response time exceeds threshold">>,
            metric => <<"avg_response_time_ms">>,
            condition => <<"gt">>,
            threshold => 1000,
            severity => warning,
            cooldown_ms => 300000,
            enabled => true,
            tags => [<<"performance">>, <<"latency">>]
        },
        
        #{
            id => <<"system_critical_latency">>,
            name => <<"System Critical Latency">>,
            description => <<"Critical alert when response time is extremely high">>,
            metric => <<"avg_response_time_ms">>,
            condition => <<"gt">>,
            threshold => 5000,
            severity => critical,
            cooldown_ms => 180000,
            enabled => true,
            tags => [<<"performance">>, <<"latency">>, <<"critical">>]
        },
        
        #{
            id => <<"high_error_rate">>,
            name => <<"High Error Rate">>,
            description => <<"Alert when error rate exceeds acceptable threshold">>,
            metric => <<"error_rate_percent">>,
            condition => <<"gt">>,
            threshold => 5.0,
            severity => warning,
            cooldown_ms => 180000,
            enabled => true,
            tags => [<<"errors">>, <<"reliability">>]
        },
        
        #{
            id => <<"critical_error_rate">>,
            name => <<"Critical Error Rate">>,
            description => <<"Critical alert for very high error rates">>,
            metric => <<"error_rate_percent">>,
            condition => <<"gt">>,
            threshold => 10.0,
            severity => critical,
            cooldown_ms => 120000,
            enabled => true,
            tags => [<<"errors">>, <<"reliability">>, <<"critical">>]
        },
        
        #{
            id => <<"high_memory_usage">>,
            name => <<"High Memory Usage">>,
            description => <<"Alert when memory usage is high">>,
            metric => <<"memory_usage_percent">>,
            condition => <<"gt">>,
            threshold => 80.0,
            severity => warning,
            cooldown_ms => 600000,
            enabled => true,
            tags => [<<"memory">>, <<"resources">>]
        },
        
        #{
            id => <<"critical_memory_usage">>,
            name => <<"Critical Memory Usage">>,
            description => <<"Critical alert for very high memory usage">>,
            metric => <<"memory_usage_percent">>,
            condition => <<"gt">>,
            threshold => 95.0,
            severity => critical,
            cooldown_ms => 300000,
            enabled => true,
            tags => [<<"memory">>, <<"resources">>, <<"critical">>]
        },
        
        #{
            id => <<"connection_failures">>,
            name => <<"Connection Failures">>,
            description => <<"Alert when connection failure rate is high">>,
            metric => <<"connection_failure_rate">>,
            condition => <<"gt">>,
            threshold => 5.0,
            severity => warning,
            cooldown_ms => 240000,
            enabled => true,
            tags => [<<"connectivity">>, <<"reliability">>]
        },
        
        #{
            id => <<"service_down">>,
            name => <<"Service Down">>,
            description => <<"Critical alert when service is completely unavailable">>,
            metric => <<"overall_health_score">>,
            condition => <<"lt">>,
            threshold => 0.1,
            severity => critical,
            cooldown_ms => 60000,
            enabled => true,
            tags => [<<"availability">>, <<"critical">>]
        }
    ].

%% @doc Validate required configuration fields
validate_required_fields(Config) ->
    RequiredFields = [
        check_interval_ms,
        alert_cooldown_ms,
        health_check_timeout_ms
    ],
    
    lists:foreach(
        fun(Field) ->
            case maps:is_key(Field, Config) of
                true -> ok;
                false -> throw({validation_error, {missing_field, Field}})
            end
        end,
        RequiredFields
    ).

%% @doc Validate field types
validate_field_types(Config) ->
    FieldTypes = [
        {check_interval_ms, fun is_positive_integer/1},
        {alert_cooldown_ms, fun is_positive_integer/1},
        {health_check_timeout_ms, fun is_positive_integer/1},
        {metrics_retention_hours, fun is_positive_integer/1},
        {dashboard_enabled, fun is_boolean/1},
        {alert_handlers, fun is_handler_list/1}
    ],
    
    lists:foreach(
        fun({Field, Validator}) ->
            case maps:get(Field, Config, undefined) of
                undefined -> ok; % Optional field
                Value ->
                    case Validator(Value) of
                        true -> ok;
                        false -> throw({validation_error, {invalid_field_type, Field, Value}})
                    end
            end
        end,
        FieldTypes
    ).

%% @doc Validate alert rules list
validate_alert_rules(Rules) when is_list(Rules) ->
    lists:foreach(fun validate_alert_rule/1, Rules);
validate_alert_rules(_) ->
    throw({validation_error, alert_rules_must_be_list}).

%% @doc Validate single alert rule
validate_alert_rule(Rule) when is_map(Rule) ->
    RequiredFields = [id, name, metric, condition, threshold, severity],
    
    % Check required fields
    lists:foreach(
        fun(Field) ->
            case maps:is_key(Field, Rule) of
                true -> ok;
                false -> throw({validation_error, {missing_alert_rule_field, Field}})
            end
        end,
        RequiredFields
    ),
    
    % Validate field values
    validate_alert_rule_fields(Rule);
validate_alert_rule(_) ->
    throw({validation_error, alert_rule_must_be_map}).

%% @doc Validate alert rule field values
validate_alert_rule_fields(Rule) ->
    % Validate severity
    Severity = maps:get(severity, Rule),
    case lists:member(Severity, [critical, warning, info]) of
        true -> ok;
        false -> throw({validation_error, {invalid_severity, Severity}})
    end,
    
    % Validate condition
    Condition = maps:get(condition, Rule),
    case lists:member(Condition, [<<"gt">>, <<"lt">>, <<"eq">>, <<"ne">>]) of
        true -> ok;
        false -> throw({validation_error, {invalid_condition, Condition}})
    end,
    
    % Validate threshold is numeric
    Threshold = maps:get(threshold, Rule),
    case is_number(Threshold) of
        true -> ok;
        false -> throw({validation_error, {invalid_threshold, Threshold}})
    end.

%% @doc Validate thresholds map
validate_thresholds(Thresholds) when is_map(Thresholds) ->
    lists:foreach(
        fun({Key, Value}) ->
            case is_number(Value) of
                true -> ok;
                false -> throw({validation_error, {invalid_threshold_value, Key, Value}})
            end
        end,
        maps:to_list(Thresholds)
    );
validate_thresholds(_) ->
    throw({validation_error, thresholds_must_be_map}).

%% @doc Check if value is a positive integer
is_positive_integer(N) when is_integer(N), N > 0 -> true;
is_positive_integer(_) -> false.

%% @doc Check if value is a valid handler list
is_handler_list(List) when is_list(List) ->
    lists:all(fun is_atom/1, List);
is_handler_list(_) -> false.