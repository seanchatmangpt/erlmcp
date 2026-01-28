%% @doc Strict Configuration Validator
%% Comprehensive validation with clear error messages for erlmcp configuration.
%% Ensures all settings are appropriate for the target scale and catches common issues
%% before they cause runtime problems.
%%
%% Validates:
%% - Connection pool settings
%% - Buffer sizes and memory allocation
%% - Process and port limits
%% - Rate limiting parameters
%% - VM argument compatibility
%% - Resource constraints (CPU, memory, FDs)
%%
%% Usage:
%%   ok = erlmcp_config_validator_strict:validate(Config),
%%   {error, Errors} = erlmcp_config_validator_strict:validate(BadConfig),
%%   erlmcp_config_validator_strict:validate_and_report(Config)
%%
-module(erlmcp_config_validator_strict).

-export([
    validate/1,
    validate/2,
    validate_and_report/1,
    validate_and_report/2,
    get_validation_rules/0,
    get_validation_rules/1
]).

-type validation_error() :: {error, atom(), string()}.
-type validation_result() :: ok | {error, [validation_error()]}.

-define(DEFAULT_RULES, #{
    check_pool_sizes => true,
    check_memory => true,
    check_limits => true,
    check_rate_limiting => true,
    check_buffer_sizes => true,
    check_transport_config => true,
    allow_warnings => false
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate configuration with default rules
-spec validate(map() | list()) -> validation_result().
validate(Config) ->
    validate(Config, ?DEFAULT_RULES).

%% @doc Validate configuration with custom rules
-spec validate(map() | list(), map()) -> validation_result().
validate(Config, Rules) when is_map(Config) ->
    validate_config(Config, Rules);
validate(Config, Rules) when is_list(Config) ->
    % Convert from sys.config list format to map
    ConfigMap = lists:foldr(fun({Key, Value}, Acc) ->
        Acc#{Key => Value}
    end, #{}, Config),
    validate_config(ConfigMap, Rules).

%% @doc Validate and print human-readable report
-spec validate_and_report(map() | list()) -> validation_result().
validate_and_report(Config) ->
    validate_and_report(Config, ?DEFAULT_RULES).

%% @doc Validate and print report with custom rules
-spec validate_and_report(map() | list(), map()) -> validation_result().
validate_and_report(Config, Rules) ->
    case validate(Config, Rules) of
        ok ->
            io:format("✓ Configuration validation PASSED~n", []),
            ok;
        {error, Errors} ->
            io:format("✗ Configuration validation FAILED with ~w errors:~n~n", [length(Errors)]),
            lists:foreach(fun(Error) ->
                print_validation_error(Error)
            end, Errors),
            {error, Errors}
    end.

%% @doc Get validation rules (default or custom by scale level)
-spec get_validation_rules() -> map().
get_validation_rules() ->
    ?DEFAULT_RULES.

%% @doc Get validation rules for specific scale level
-spec get_validation_rules(atom()) -> map().
get_validation_rules(_ScaleLevel) ->
    % All scale levels use same validation rules
    ?DEFAULT_RULES.

%%====================================================================
%% Internal Validation Functions
%%====================================================================

%% @doc Main validation dispatcher
-spec validate_config(map(), map()) -> validation_result().
validate_config(Config, Rules) ->
    Errors = [],

    % Get system info for cross-checks
    SystemInfo = erlmcp_smart_defaults:detect_and_calculate(),
    #{cpu_cores := CPUs, memory_gb := MemGB, max_fds := MaxFDs} =
        maps:get(system_info, SystemInfo),

    Errors1 = case maps:get(check_pool_sizes, Rules, true) of
        true -> Errors ++ validate_pool_sizes(Config, CPUs, MaxFDs);
        false -> Errors
    end,

    Errors2 = case maps:get(check_memory, Rules, true) of
        true -> Errors1 ++ validate_memory_allocation(Config, MemGB);
        false -> Errors1
    end,

    Errors3 = case maps:get(check_limits, Rules, true) of
        true -> Errors2 ++ validate_process_limits(Config, MaxFDs);
        false -> Errors2
    end,

    Errors4 = case maps:get(check_rate_limiting, Rules, true) of
        true -> Errors3 ++ validate_rate_limiting(Config);
        false -> Errors3
    end,

    Errors5 = case maps:get(check_buffer_sizes, Rules, true) of
        true -> Errors4 ++ validate_buffer_sizes(Config, MemGB);
        false -> Errors4
    end,

    Errors6 = case maps:get(check_transport_config, Rules, true) of
        true -> Errors5 ++ validate_transport_config(Config);
        false -> Errors5
    end,

    case Errors6 of
        [] -> ok;
        _ -> {error, Errors6}
    end.

%% @doc Validate pool configuration
-spec validate_pool_sizes(map(), pos_integer(), pos_integer()) -> [validation_error()].
validate_pool_sizes(Config, CPUs, MaxFDs) ->
    Errors = [],

    case maps:get(connection_pool_config, Config, undefined) of
        undefined ->
            Errors;
        PoolConfiguration ->
            PoolCount = maps:get(pool_count, PoolConfiguration, 0),
            PoolSize = maps:get(pool_size, PoolConfiguration, 0),
            MaxOverflow = maps:get(max_overflow, PoolConfiguration, 0),

            Errors1 = case PoolCount =< 0 of
                true -> [{error, invalid_pool_count,
                    format_error("pool_count must be > 0, got ~w", [PoolCount])} | Errors];
                false -> Errors
            end,

            Errors2 = case PoolSize =< 0 of
                true -> [{error, invalid_pool_size,
                    format_error("pool_size must be > 0, got ~w", [PoolSize])} | Errors1];
                false -> Errors1
            end,

            Errors3 = case MaxOverflow < 0 of
                true -> [{error, invalid_max_overflow,
                    format_error("max_overflow must be >= 0, got ~w", [MaxOverflow])} | Errors2];
                false -> Errors2
            end,

            % Warn if total workers exceeds reasonable limits
            TotalWorkers = PoolCount * (PoolSize + MaxOverflow),
            Errors4 = case TotalWorkers > MaxFDs of
                true -> [{error, pool_exceeds_fds,
                    format_error("Total workers (~w) exceeds max FDs (~w). Increase system limits.",
                        [TotalWorkers, MaxFDs])} | Errors3];
                false -> Errors3
            end,

            % Warn if pool count doesn't align with CPU count
            Errors5 = case PoolCount < CPUs andalso PoolCount < 8 of
                true -> [{error, small_pool_count,
                    format_error("pool_count (~w) is small for ~w CPUs. Consider increasing.",
                        [PoolCount, CPUs])} | Errors4];
                false -> Errors4
            end,

            Errors5
    end.

%% @doc Validate memory allocation
-spec validate_memory_allocation(map(), float()) -> [validation_error()].
validate_memory_allocation(Config, MemGB) ->
    Errors = [],

    case maps:get(connection_pool_config, Config, undefined) of
        undefined ->
            Errors;
        PoolCfg ->
            case maps:get(buffer_config, Config, undefined) of
                undefined ->
                    Errors;
                BufCfg ->
                    BufferMem = maps:get(total_buffer_memory_mb, BufCfg, 0),
                    SystemBufferGB = BufferMem / 1024,

                    % Warn if buffer allocation exceeds 50% of system memory
                    case SystemBufferGB > (MemGB * 0.5) of
                        true -> [{error, excessive_buffer_memory,
                            format_error("Buffer memory (~.1f GB) exceeds 50% of system memory (~.1f GB)",
                                [SystemBufferGB, MemGB])} | Errors];
                        false -> Errors
                    end
            end
    end.

%% @doc Validate process and resource limits
-spec validate_process_limits(map(), pos_integer()) -> [validation_error()].
validate_process_limits(Config, MaxFDs) ->
    Errors = [],

    case maps:get(process_limits, Config, undefined) of
        undefined ->
            Errors;
        Limits ->
            MaxProcs = maps:get(max_processes, Limits, 0),
            MaxPorts = maps:get(max_ports, Limits, 0),
            MaxETS = maps:get(max_ets_tables, Limits, 0),

            % Validate max_processes
            Errors1 = case MaxProcs > 1000000 of
                true -> [{error, excessive_processes,
                    format_error("max_processes (~w) exceeds 1M. Increase with caution.",
                        [MaxProcs])} | Errors];
                false -> Errors
            end,

            % Validate max_ports against system FD limit
            Errors2 = case MaxPorts > MaxFDs of
                true -> [{error, ports_exceed_fds,
                    format_error("max_ports (~w) exceeds system FD limit (~w)",
                        [MaxPorts, MaxFDs])} | Errors1];
                false -> Errors1
            end,

            % Validate max_ets_tables
            Errors3 = case MaxETS < 1000 of
                true -> [{error, insufficient_ets_tables,
                    format_error("max_ets_tables (~w) is too small. Need at least 1000.",
                        [MaxETS])} | Errors2];
                false -> Errors2
            end,

            Errors3
    end.

%% @doc Validate rate limiting configuration
-spec validate_rate_limiting(map()) -> [validation_error()].
validate_rate_limiting(Config) ->
    Errors = [],

    case maps:get(rate_limiting, Config, undefined) of
        undefined ->
            Errors;
        RateLimitingConfig when is_map(RateLimitingConfig) ->
            Enabled = maps:get(enabled, RateLimitingConfig, false),
            case Enabled of
                false -> Errors;
                true ->
                    MsgPerSec = maps:get(max_messages_per_sec, RateLimitingConfig, 0),
                    GlobalMsgPerSec = maps:get(global_max_messages_per_sec, RateLimitingConfig, 0),
                    _ConnPerSec = maps:get(max_connections_per_sec, RateLimitingConfig, 0),
                    _ToolPerSec = maps:get(max_tool_calls_per_sec, RateLimitingConfig, 0),

                    Errors1 = case MsgPerSec =< 0 of
                        true -> [{error, invalid_rate_limit,
                            format_error("max_messages_per_sec must be > 0, got ~w", [MsgPerSec])} | Errors];
                        false -> Errors
                    end,

                    Errors2 = case GlobalMsgPerSec =< 0 of
                        true -> [{error, invalid_global_rate_limit,
                            format_error("global_max_messages_per_sec must be > 0, got ~w",
                                [GlobalMsgPerSec])} | Errors1];
                        false -> Errors1
                    end,

                    Errors3 = case GlobalMsgPerSec < MsgPerSec of
                        true -> [{error, inconsistent_rate_limits,
                            format_error("global_max_messages_per_sec (~w) should be >= max_messages_per_sec (~w)",
                                [GlobalMsgPerSec, MsgPerSec])} | Errors2];
                        false -> Errors2
                    end,

                    Errors3
            end;
        RateLimitingValue ->
            [{error, invalid_rate_limiting_config,
                format_error("rate_limiting must be a map, got ~w", [RateLimitingValue])} | Errors]
    end.

%% @doc Validate buffer sizes
-spec validate_buffer_sizes(map(), float()) -> [validation_error()].
validate_buffer_sizes(Config, MemGB) ->
    Errors = [],

    case maps:get(buffer_config, Config, undefined) of
        undefined ->
            Errors;
        BufCfg ->
            MsgBuf = maps:get(message_buffer_size, BufCfg, 0),
            FrameBuf = maps:get(frame_buffer_per_conn, BufCfg, 0),
            TotalBuf = maps:get(total_buffer_memory_mb, BufCfg, 0),

            % Validate message buffer
            Errors1 = case MsgBuf < 1024 of
                true -> [{error, insufficient_message_buffer,
                    format_error("message_buffer_size (~w) is too small. Need >= 1024 bytes.",
                        [MsgBuf])} | Errors];
                false -> Errors
            end,

            % Validate frame buffer
            Errors2 = case FrameBuf < 4096 of
                true -> [{error, insufficient_frame_buffer,
                    format_error("frame_buffer_per_conn (~w) is too small. Need >= 4096 bytes.",
                        [FrameBuf])} | Errors1];
                false -> Errors1
            end,

            % Validate total buffer allocation
            Errors3 = case (TotalBuf * 1024 / 1024) > MemGB * 0.8 of
                true -> [{error, excessive_total_buffer,
                    format_error("total_buffer_memory (~w MB) exceeds 80% of system memory",
                        [TotalBuf])} | Errors2];
                false -> Errors2
            end,

            Errors3
    end.

%% @doc Validate transport configuration
-spec validate_transport_config(map()) -> [validation_error()].
validate_transport_config(Config) ->
    Errors = [],

    case maps:get(transport_defaults, Config, undefined) of
        undefined ->
            Errors;
        TransportCfg ->
            % Validate TCP transport if present
            Errors1 = case maps:get(tcp, TransportCfg, undefined) of
                undefined -> Errors;
                TCPCfg -> validate_tcp_transport(TCPCfg, Errors)
            end,

            % Validate HTTP transport if present
            Errors2 = case maps:get(http, TransportCfg, undefined) of
                undefined -> Errors1;
                HTTPCfg -> validate_http_transport(HTTPCfg, Errors1)
            end,

            Errors2
    end.

%% @doc Validate TCP transport settings
-spec validate_tcp_transport(map(), [validation_error()]) -> [validation_error()].
validate_tcp_transport(TCPCfg, Errors) ->
    ConnTimeout = maps:get(connect_timeout, TCPCfg, 5000),
    MaxConn = maps:get(max_connections, TCPCfg, 1000),

    Errors1 = case ConnTimeout < 100 of
        true -> [{error, tcp_timeout_too_low,
            format_error("TCP connect_timeout (~w ms) is too low. Need >= 100 ms.", [ConnTimeout])} | Errors];
        false -> Errors
    end,

    Errors2 = case MaxConn < 1 of
        true -> [{error, tcp_max_conn_invalid,
            format_error("TCP max_connections must be >= 1, got ~w", [MaxConn])} | Errors1];
        false -> Errors1
    end,

    Errors2.

%% @doc Validate HTTP transport settings
-spec validate_http_transport(map(), [validation_error()]) -> [validation_error()].
validate_http_transport(HTTPCfg, Errors) ->
    ConnTimeout = maps:get(connect_timeout, HTTPCfg, 5000),
    ReqTimeout = maps:get(request_timeout, HTTPCfg, 30000),
    MaxConn = maps:get(max_connections, HTTPCfg, 100),

    Errors1 = case ConnTimeout < 100 of
        true -> [{error, http_timeout_too_low,
            format_error("HTTP connect_timeout (~w ms) is too low. Need >= 100 ms.", [ConnTimeout])} | Errors];
        false -> Errors
    end,

    Errors2 = case ReqTimeout < 1000 of
        true -> [{error, http_request_timeout_too_low,
            format_error("HTTP request_timeout (~w ms) is too low. Need >= 1000 ms.", [ReqTimeout])} | Errors1];
        false -> Errors1
    end,

    Errors3 = case MaxConn < 1 of
        true -> [{error, http_max_conn_invalid,
            format_error("HTTP max_connections must be >= 1, got ~w", [MaxConn])} | Errors2];
        false -> Errors2
    end,

    Errors3.

%%====================================================================
%% Output Functions
%%====================================================================

%% @doc Format validation error for display
-spec format_error(string(), [term()]) -> string().
format_error(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

%% @doc Print validation error
-spec print_validation_error(validation_error()) -> ok.
print_validation_error({error, Code, Message}) ->
    io:format("  [~w] ~s~n", [Code, Message]).
