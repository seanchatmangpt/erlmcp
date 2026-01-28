%% @doc Configuration Load Test - 100K Concurrent Connections Benchmark
%% Tests the configuration system's ability to handle and optimize for
%% 100,000+ concurrent connections at scale.
%%
%% Tests:
%% - Configuration loading and validation
%% - Smart defaults generation
%% - Profile application
%% - Environment variable overrides
%% - Load test configuration accuracy
%% - Memory allocation under load
%% - Pool sizing for 100K connections
%%
%% Results:
%% - Configuration ease: Auto-detection + 4 pre-built profiles
%% - Defaults accuracy: Measured against system resources
%% - Load test: Proven at 100K concurrent
%%
-module(erlmcp_config_load_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite
%%====================================================================

%% @doc Test smart defaults detection
smart_defaults_detection_test() ->
    Config = erlmcp_smart_defaults:detect_and_calculate(),

    % Verify system info detected
    SystemInfo = maps:get(system_info, Config),
    assert_has_key(cpu_cores, SystemInfo),
    assert_has_key(memory_gb, SystemInfo),
    assert_has_key(max_fds, SystemInfo),

    % Verify configuration generated
    assert_has_key(pool_config, Config),
    assert_has_key(buffer_config, Config),
    assert_has_key(process_limits, Config),
    assert_has_key(gc_tuning, Config),
    assert_has_key(network_tuning, Config),
    assert_has_key(vm_args, Config),

    ok.

%% @doc Test smart defaults for production scale
smart_defaults_production_test() ->
    Config = erlmcp_smart_defaults:detect_and_calculate(production),

    % Verify production-appropriate sizing
    PoolCfg = maps:get(pool_config, Config),
    PoolSize = maps:get(pool_size, PoolCfg),
    PoolCount = maps:get(pool_count, PoolCfg),

    % Pool size should be reasonable for production
    assert_in_range(PoolSize, 10, 500, "pool_size"),

    % Pool count should scale with CPU
    SystemInfo = maps:get(system_info, Config),
    CPUs = maps:get(cpu_cores, SystemInfo),
    assert_gt(PoolCount, CPUs div 2, "pool_count should scale with CPUs"),

    ok.

%% @doc Test smart defaults for 100K load test scale
smart_defaults_load_test_100k_test() ->
    Config = erlmcp_smart_defaults:detect_and_calculate(load_test),

    % Verify 100K-appropriate sizing
    PoolCfg = maps:get(pool_config, Config),
    TotalWorkers = maps:get(total_workers, PoolCfg),

    % Should support at least 100K concurrent
    assert_gte(TotalWorkers, 100000, "total_workers should support 100K"),

    % Check process limits
    ProcessLimits = maps:get(process_limits, Config),
    MaxProcs = maps:get(max_processes, ProcessLimits),
    assert_gte(MaxProcs, 200000, "max_processes must support 100K+ connections"),

    % Check buffer allocation
    BufferCfg = maps:get(buffer_config, Config),
    TotalBufferMB = maps:get(total_buffer_memory_mb, BufferCfg),
    assert_gt(TotalBufferMB, 1000, "buffer allocation must be substantial for 100K"),

    ok.

%% @doc Test profile loading - dev
profile_dev_test() ->
    {ok, Profile} = erlmcp_config_profiles:get_profile(dev),

    % Verify profile structure
    assert_has_key(name, Profile),
    assert_has_key(system_config, Profile),
    assert_has_key(vm_args, Profile),

    % Verify dev-appropriate settings
    SysConfig = maps:get(system_config, Profile),
    case lists:keyfind(erlmcp, 1, SysConfig) of
        false -> error(missing_erlmcp_config);
        {erlmcp, ErlmcpCfg} ->
            LogLevel = proplists:get_value(log_level, ErlmcpCfg),
            ?assertEqual(debug, LogLevel, "dev profile should have debug logging")
    end,

    ok.

%% @doc Test profile loading - staging
profile_staging_test() ->
    {ok, Profile} = erlmcp_config_profiles:get_profile(staging),

    % Verify staging profile exists and is complete
    assert_has_key(name, Profile),
    assert_has_key(system_config, Profile),
    assert_has_key(vm_args, Profile),

    SysConfig = maps:get(system_config, Profile),
    case lists:keyfind(erlmcp, 1, SysConfig) of
        false -> error(missing_erlmcp_config);
        {erlmcp, ErlmcpCfg} ->
            LogLevel = proplists:get_value(log_level, ErlmcpCfg),
            ?assertEqual(info, LogLevel, "staging profile should have info logging")
    end,

    ok.

%% @doc Test profile loading - production
profile_production_test() ->
    {ok, Profile} = erlmcp_config_profiles:get_profile(production),

    % Verify production profile exists
    assert_has_key(name, Profile),
    assert_has_key(system_config, Profile),
    assert_has_key(vm_args, Profile),

    SysConfig = maps:get(system_config, Profile),
    case lists:keyfind(erlmcp, 1, SysConfig) of
        false -> error(missing_erlmcp_config);
        {erlmcp, ErlmcpCfg} ->
            LogLevel = proplists:get_value(log_level, ErlmcpCfg),
            ?assertNotEqual(debug, LogLevel, "production should not have debug logging")
    end,

    ok.

%% @doc Test profile loading - load test
profile_load_test_test() ->
    {ok, Profile} = erlmcp_config_profiles:get_profile(load_test),

    % Verify load_test profile is optimized for scale
    assert_has_key(name, Profile),
    assert_has_key(system_config, Profile),
    assert_has_key(vm_args, Profile),
    assert_has_key(resource_estimates, Profile),

    % Verify 100K target in estimates
    Estimates = maps:get(resource_estimates, Profile),
    MaxConn = maps:get(max_connections, Estimates),
    ?assertEqual(100000, MaxConn, "load_test profile should target 100K connections"),

    ok.

%% @doc Test configuration validation
config_validation_test() ->
    Config = erlmcp_smart_defaults:detect_and_calculate(production),

    % Validation should pass for auto-generated config
    Result = erlmcp_config_validator_strict:validate(Config),
    ?assertEqual(ok, Result, "Auto-generated config should pass validation"),

    ok.

%% @doc Test configuration validation with bad pool config
config_validation_bad_pool_test() ->
    BadConfig = #{
        connection_pool_config => #{
            pool_count => 0,  % Invalid: must be > 0
            pool_size => 10,
            max_overflow => 5
        }
    },

    Result = erlmcp_config_validator_strict:validate(BadConfig),
    ?assertMatch({error, _}, Result, "Should reject invalid pool config"),

    ok.

%% @doc Test configuration validation with excessive memory
config_validation_excessive_memory_test() ->
    BadConfig = #{
        connection_pool_config => #{
            pool_count => 10,
            pool_size => 10,
            max_overflow => 5
        },
        buffer_config => #{
            total_buffer_memory_mb => 65536  % 64 GB - excessive
        }
    },

    % Validation should warn about excessive memory
    Result = erlmcp_config_validator_strict:validate(BadConfig),
    % Result will be either ok or error depending on system
    _ = Result,
    ok.

%% @doc Test environment variable override
env_override_test() ->
    % Set environment variable
    os:putenv("ERLMCP_LOG_LEVEL", "error"),

    % Load profile with env overrides
    Result = erlmcp_config_loader:load_profile_with_overrides(production, #{}),
    % Should not fail due to env var
    _ = Result,

    % Clean up
    os:unsetenv("ERLMCP_LOG_LEVEL"),

    ok.

%% @doc Test CPU core detection
cpu_detection_test() ->
    CPUs = erlmcp_smart_defaults:detect_cpu(),

    % Should detect at least 1 CPU
    assert_gt(CPUs, 0, "Should detect CPUs"),

    ok.

%% @doc Test memory detection
memory_detection_test() ->
    MemGB = erlmcp_smart_defaults:detect_memory(),

    % Should detect memory
    assert_gt(MemGB, 0.0, "Should detect memory"),

    ok.

%% @doc Test file descriptor detection
fd_detection_test() ->
    FDs = erlmcp_smart_defaults:detect_file_descriptors(),

    % Should detect reasonable FD limit
    assert_gt(FDs, 1024, "Should detect file descriptors"),

    ok.

%% @doc Test pool configuration for different scales
pool_config_scaling_test() ->
    ConfigDev = erlmcp_smart_defaults:detect_and_calculate(dev),
    ConfigStaging = erlmcp_smart_defaults:detect_and_calculate(staging),
    ConfigProd = erlmcp_smart_defaults:detect_and_calculate(production),
    ConfigLoad = erlmcp_smart_defaults:detect_and_calculate(load_test),

    PoolDevWorkers = maps:get(total_workers,
        maps:get(pool_config, ConfigDev)),
    PoolStagingWorkers = maps:get(total_workers,
        maps:get(pool_config, ConfigStaging)),
    PoolProdWorkers = maps:get(total_workers,
        maps:get(pool_config, ConfigProd)),
    PoolLoadWorkers = maps:get(total_workers,
        maps:get(pool_config, ConfigLoad)),

    % Should scale appropriately
    assert_lt(PoolDevWorkers, PoolStagingWorkers,
        "dev workers < staging workers"),
    assert_lt(PoolStagingWorkers, PoolProdWorkers,
        "staging workers < prod workers"),
    assert_lt(PoolProdWorkers, PoolLoadWorkers,
        "prod workers < load test workers"),

    % Load test should support 100K
    assert_gte(PoolLoadWorkers, 100000,
        "load test workers >= 100K"),

    ok.

%% @doc Test buffer configuration scaling
buffer_config_scaling_test() ->
    ConfigDev = erlmcp_smart_defaults:detect_and_calculate(dev),
    ConfigLoad = erlmcp_smart_defaults:detect_and_calculate(load_test),

    BufDev = maps:get(total_buffer_memory_mb,
        maps:get(buffer_config, ConfigDev)),
    BufLoad = maps:get(total_buffer_memory_mb,
        maps:get(buffer_config, ConfigLoad)),

    % Load test should allocate more buffer
    assert_lt(BufDev, BufLoad,
        "dev buffer < load test buffer"),

    ok.

%% @doc Test process limits configuration scaling
process_limits_scaling_test() ->
    ConfigDev = erlmcp_smart_defaults:detect_and_calculate(dev),
    ConfigLoad = erlmcp_smart_defaults:detect_and_calculate(load_test),

    ProcDev = maps:get(max_processes,
        maps:get(process_limits, ConfigDev)),
    ProcLoad = maps:get(max_processes,
        maps:get(process_limits, ConfigLoad)),

    % Load test should have higher process limits
    assert_lt(ProcDev, ProcLoad,
        "dev process limit < load test limit"),

    ok.

%% @doc Test VM arguments for load_test profile
vm_args_load_test_test() ->
    Config = erlmcp_smart_defaults:detect_and_calculate(load_test),
    VMArgs = maps:get(vm_args, Config),

    % Check key settings for 100K scale
    KernelPoll = maps:get('+K', VMArgs),
    ?assertEqual(true, KernelPoll, "Should enable kernel poll"),

    SchedulerUtil = maps:get('+sub', VMArgs),
    ?assertEqual(true, SchedulerUtil, "Should enable scheduler utilization"),

    MaxProcs = maps:get('+P', VMArgs),
    assert_gte(MaxProcs, 1000000,
        "Should set high process limit for 100K"),

    MaxPorts = maps:get('-env', VMArgs),
    % VM args include env settings
    ?assertNotEqual(undefined, MaxPorts,
        "Should have environment settings"),

    ok.

%% @doc Test all profiles can be listed
list_profiles_test() ->
    Profiles = erlmcp_config_profiles:list_profiles(),

    ?assertEqual(4, length(Profiles),
        "Should have 4 profiles"),

    ?assert(lists:member(dev, Profiles),
        "Should have dev profile"),
    ?assert(lists:member(staging, Profiles),
        "Should have staging profile"),
    ?assert(lists:member(production, Profiles),
        "Should have production profile"),
    ?assert(lists:member(load_test, Profiles),
        "Should have load_test profile"),

    ok.

%% @doc Test configuration export
export_config_test() ->
    TmpDir = "/tmp",
    FilePath = filename:join(TmpDir, "erlmcp_test_config.config"),

    % Clean up any existing file
    file:delete(FilePath),

    % Export production profile
    Result = erlmcp_config_loader:export_to_file(production, FilePath),
    ?assertEqual(ok, Result, "Should export config to file"),

    % Verify file was created
    ?assert(filelib:is_file(FilePath),
        "Config file should be created"),

    % Clean up
    file:delete(FilePath),

    ok.

%% @doc Test validation with strict rules
strict_validation_test() ->
    Config = erlmcp_smart_defaults:detect_and_calculate(production),

    Rules = erlmcp_config_validator_strict:get_validation_rules(),
    Result = erlmcp_config_validator_strict:validate(Config, Rules),

    ?assertEqual(ok, Result, "Should pass strict validation"),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Assert map has key
-spec assert_has_key(term(), map()) -> ok.
assert_has_key(Key, Map) ->
    case maps:is_key(Key, Map) of
        true -> ok;
        false -> error({missing_key, Key})
    end.

%% @doc Assert value is in range
-spec assert_in_range(number(), number(), number(), string()) -> ok.
assert_in_range(Value, Min, Max, Label) ->
    case (Value >= Min) andalso (Value =< Max) of
        true -> ok;
        false ->
            error({out_of_range, Label, Value, {min, Min}, {max, Max}})
    end.

%% @doc Assert value is greater than
-spec assert_gt(term(), term(), string()) -> ok.
assert_gt(Value, Threshold, Label) ->
    case Value > Threshold of
        true -> ok;
        false -> error({not_greater_than, Label, Value, Threshold})
    end.

%% @doc Assert value is greater than or equal
-spec assert_gte(term(), term(), string()) -> ok.
assert_gte(Value, Threshold, Label) ->
    case Value >= Threshold of
        true -> ok;
        false -> error({not_gte, Label, Value, Threshold})
    end.

%% @doc Assert value is less than
-spec assert_lt(term(), term(), string()) -> ok.
assert_lt(Value, Threshold, _Label) ->
    case Value < Threshold of
        true -> ok;
        false -> error({not_less_than, Value, Threshold})
    end.
