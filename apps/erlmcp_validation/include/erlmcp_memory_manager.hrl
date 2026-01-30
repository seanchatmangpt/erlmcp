%% ============================================================================
%% erlmcp_memory_manager.hrl - Memory Manager Records and Definitions
%% ============================================================================

%% Memory usage statistics (summary)
-record(memory_stats, {
    total_memory = 0 :: non_neg_integer(),
    used_memory = 0 :: non_neg_integer(),
    cache_memory = 0 :: non_neg_integer(),
    process_memory = 0 :: non_neg_integer(),
    system_memory = 0 :: non_neg_integer(),
    used_percent = 0.0 :: float(),
    cache_entries = 0 :: non_neg_integer()
}).

%% Detailed memory statistics (from erlang:memory/0)
-record(memory_stats_detailed, {
    total = 0 :: non_neg_integer(),
    processes = 0 :: non_neg_integer(),
    processes_used = 0 :: non_neg_integer(),
    system = 0 :: non_neg_integer(),
    atom = 0 :: non_neg_integer(),
    atom_used = 0 :: non_neg_integer(),
    binary = 0 :: non_neg_integer(),
    code = 0 :: non_neg_integer(),
    ets = 0 :: non_neg_integer()
}).

%% Memory manager statistics
-record(memory_manager_stats, {
    cache_hits = 0 :: non_neg_integer(),
    cache_misses = 0 :: non_neg_integer(),
    cache_evictions = 0 :: non_neg_integer(),
    gc_runs = 0 :: non_neg_integer(),
    memory_pressure_alerts = 0 :: non_neg_integer(),
    purges = 0 :: non_neg_integer()
}).

%% Constants
-define(DEFAULT_MAX_CACHE_SIZE, 100).
-define(DEFAULT_SPEC_MEMORY_LIMIT, 100 * 1024 * 1024).
-define(MEMORY_CHECK_INTERVAL, 5000).
-define(GC_INTERVAL, 60000).
-define(MEMORY_PRESSURE_THRESHOLD, 0.85).
