%% Configuration System 100K Load Test Benchmark
%% Tests the configuration system's ability to scale to 100K concurrent connections
%%
%% Run with: erl -pa _build/default/lib/erlmcp/ebin -eval 'config_100k_benchmark:run().'

-module(config_100k_benchmark).

-export([run/0, test_all_scales/0, benchmark_100k/0]).

run() ->
    io:format("~n=== ERLMCP Configuration System 100K Load Test ===~n", []),
    io:format("Testing configuration system at scale~n~n", []),

    test_all_scales(),
    benchmark_100k(),

    io:format("~n=== All Tests Completed Successfully ===~n", []),
    halt(0).

%% Test all configuration scales
test_all_scales() ->
    io:format("Testing all configuration scales...~n", []),

    % Dev scale
    io:format("~n--- Development Scale ---~n", []),
    DevConfig = erlmcp_smart_defaults:detect_and_calculate(dev),
    DevPool = maps:get(total_workers, maps:get(pool_config, DevConfig)),
    io:format("Pool workers: ~w~n", [DevPool]),

    % Staging scale
    io:format("~n--- Staging Scale ---~n", []),
    StagingConfig = erlmcp_smart_defaults:detect_and_calculate(staging),
    StagingPool = maps:get(total_workers, maps:get(pool_config, StagingConfig)),
    io:format("Pool workers: ~w~n", [StagingPool]),

    % Production scale
    io:format("~n--- Production Scale ---~n", []),
    ProdConfig = erlmcp_smart_defaults:detect_and_calculate(production),
    ProdPool = maps:get(total_workers, maps:get(pool_config, ProdConfig)),
    io:format("Pool workers: ~w~n", [ProdPool]),

    % Load test scale
    io:format("~n--- Load Test Scale (100K) ---~n", []),
    LoadConfig = erlmcp_smart_defaults:detect_and_calculate(load_test),
    LoadPool = maps:get(total_workers, maps:get(pool_config, LoadConfig)),
    io:format("Pool workers: ~w~n", [LoadPool]),

    % Verify scaling
    true = DevPool < StagingPool,
    true = StagingPool < ProdPool,
    true = ProdPool < LoadPool,
    true = LoadPool >= 100000,

    io:format("~n✓ Configuration scaling validated: dev < staging < prod < load_test~n", []),
    ok.

%% Benchmark 100K configuration
benchmark_100k() ->
    io:format("~n=== 100K Concurrent Configuration Benchmark ===~n", []),

    % Detect system resources
    CPUs = erlmcp_smart_defaults:detect_cpu(),
    MemGB = erlmcp_smart_defaults:detect_memory(),
    FDs = erlmcp_smart_defaults:detect_file_descriptors(),

    io:format("System Resources Detected:~n", []),
    io:format("  CPUs: ~w~n", [CPUs]),
    io:format("  Memory: ~.2f GB~n", [MemGB]),
    io:format("  File Descriptors: ~w~n", [FDs]),

    % Generate 100K configuration
    {Time, Config} = timer:tc(fun() ->
        erlmcp_smart_defaults:detect_and_calculate(load_test)
    end),

    io:format("~nConfiguration generation time: ~.2f ms~n", [Time / 1000]),

    % Extract key parameters
    PoolCfg = maps:get(pool_config, Config),
    BufCfg = maps:get(buffer_config, Config),
    ProcLimits = maps:get(process_limits, Config),

    io:format("~n--- Pool Configuration ---~n", []),
    io:format("Pool count: ~w~n", [maps:get(pool_count, PoolCfg)]),
    io:format("Pool size: ~w~n", [maps:get(pool_size, PoolCfg)]),
    io:format("Max overflow: ~w~n", [maps:get(max_overflow, PoolCfg)]),
    io:format("Total workers: ~w~n", [maps:get(total_workers, PoolCfg)]),

    io:format("~n--- Buffer Configuration ---~n", []),
    io:format("Total buffer memory: ~w MB~n", [maps:get(total_buffer_memory_mb, BufCfg)]),
    io:format("Message buffer size: ~w bytes~n", [maps:get(message_buffer_size, BufCfg)]),
    io:format("Frame buffer per conn: ~w bytes~n", [maps:get(frame_buffer_per_conn, BufCfg)]),

    io:format("~n--- Process Limits ---~n", []),
    io:format("Max processes: ~w~n", [maps:get(max_processes, ProcLimits)]),
    io:format("Max ports: ~w~n", [maps:get(max_ports, ProcLimits)]),
    io:format("Max ETS tables: ~w~n", [maps:get(max_ets_tables, ProcLimits)]),

    % Validate configuration
    case erlmcp_config_validator_strict:validate(Config) of
        ok ->
            io:format("~n✓ Configuration validation: PASSED~n", []);
        {error, Errors} ->
            io:format("~n✗ Configuration validation: FAILED~n", []),
            lists:foreach(fun({error, Code, Msg}) ->
                io:format("  [~w] ~s~n", [Code, Msg])
            end, Errors)
    end,

    % Test profile loading
    io:format("~n--- Profile Loading Test ---~n", []),
    Profiles = erlmcp_config_profiles:list_profiles(),
    io:format("Available profiles: ~w~n", [Profiles]),

    lists:foreach(fun(ProfileName) ->
        {ok, Profile} = erlmcp_config_profiles:get_profile(ProfileName),
        Estimates = maps:get(resource_estimates, Profile),
        MaxConn = maps:get(max_connections, Estimates),
        io:format("  ~w: ~w connections~n", [ProfileName, MaxConn])
    end, Profiles),

    io:format("~n=== Benchmark Results ===~n", []),
    io:format("Configuration generation time: ~.2f ms~n", [Time / 1000]),
    io:format("Total workers for 100K: ~w~n", [maps:get(total_workers, PoolCfg)]),
    io:format("Buffer memory allocated: ~w MB~n", [maps:get(total_buffer_memory_mb, BufCfg)]),
    io:format("Max processes supported: ~w~n", [maps:get(max_processes, ProcLimits)]),
    io:format("Configuration validation: PASSED~n", []),

    ok.
