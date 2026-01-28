%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test Suite: Error Handling at 100K Scale
%%%
%%% Stress tests erlmcp_error module under high concurrency with:
%%% - 100K concurrent error contexts
%%% - Parallel error logging (multi-node simulation)
%%% - Error categorization throughput
%%% - Statistics collection at scale
%%% - Memory efficiency validation
%%% - Real-world failure scenarios
%%%
%%% Run with:
%%%   rebar3 ct --suite erlmcp_error_100k_stress_SUITE
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_error_100k_stress_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Common Test Callbacks
%%====================================================================

all() ->
    [
        {group, context_creation},
        {group, error_logging},
        {group, categorization},
        {group, metrics},
        {group, recovery},
        {group, concurrent_scenarios}
    ].

groups() ->
    [
        {context_creation, [parallel], [
            test_create_100k_contexts_sequential,
            test_create_100k_contexts_parallel,
            test_context_update_performance
        ]},
        {error_logging, [parallel], [
            test_log_100k_errors_sequential,
            test_log_100k_errors_parallel,
            test_log_errors_with_context,
            test_log_errors_all_severity_levels
        ]},
        {categorization, [parallel], [
            test_categorize_100k_mixed_errors,
            test_categorize_error_performance,
            test_retryable_check_throughput
        ]},
        {metrics, [parallel], [
            test_collect_100k_errors,
            test_metrics_accuracy,
            test_stats_concurrent_access
        ]},
        {recovery, [parallel], [
            test_backoff_delays_100k,
            test_extract_error_info_100k,
            test_recovery_decisions_accuracy
        ]},
        {concurrent_scenarios, [sequential], [
            test_mixed_workload_100k,
            test_network_failure_simulation,
            test_cascading_errors,
            test_error_recovery_cycle
        ]}
    ].

init_per_suite(Config) ->
    % Start erlmcp_error collector
    {ok, _} = erlmcp_error:start_error_collector(),
    erlmcp_error:reset_error_stats(),
    Config.

end_per_suite(_Config) ->
    erlmcp_error:reset_error_stats(),
    ok.

init_per_group(concurrent_scenarios, Config) ->
    % Sequential group setup
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    % Reset stats before each test
    erlmcp_error:reset_error_stats(),
    [{test_case, TestCase} | Config].

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Context Creation Tests
%%====================================================================

test_create_100k_contexts_sequential(Config) ->
    ct:log("Creating 100K contexts sequentially..."),
    {Time, Contexts} = timer:tc(fun() ->
        [erlmcp_error:new_context(
            list_to_atom("op_" ++ integer_to_list(I rem 10)),
            #{connection_id => <<"conn-", (integer_to_binary(I))/binary>>}
        ) || I <- lists:seq(1, 100000)]
    end),

    TimeMs = Time div 1000,
    ContextCount = length(Contexts),
    Throughput = (ContextCount * 1000) div TimeMs,

    ct:pal("Sequential context creation:~n"
           "  Contexts: ~w~n"
           "  Time: ~wms~n"
           "  Throughput: ~w contexts/sec",
           [ContextCount, TimeMs, Throughput]),

    % Performance assertions
    true = ContextCount =:= 100000,
    true = TimeMs < 2000,
    {save_config, [{sequential_throughput, Throughput} | Config]}.

test_create_100k_contexts_parallel(Config) ->
    ct:log("Creating 100K contexts in parallel (10 workers)..."),
    NumWorkers = 10,
    ContextsPerWorker = 10000,

    {Time, AllContexts} = timer:tc(fun() ->
        Parent = self(),
        Workers = [
            spawn(fun() ->
                Contexts = [
                    erlmcp_error:new_context(
                        list_to_atom("worker_" ++ integer_to_list(W)),
                        #{
                            connection_id => <<"conn-", (integer_to_binary(W))/binary,
                                               "-", (integer_to_binary(I))/binary>>
                        }
                    )
                    || I <- lists:seq(1, ContextsPerWorker)
                ],
                Parent ! {self(), Contexts}
            end)
            || W <- lists:seq(1, NumWorkers)
        ],
        [receive {_Pid, Contexts} -> Contexts end || _ <- Workers]
    end),

    TimeMs = Time div 1000,
    TotalContexts = NumWorkers * ContextsPerWorker,
    Throughput = (TotalContexts * 1000) div TimeMs,

    ct:pal("Parallel context creation:~n"
           "  Total contexts: ~w~n"
           "  Workers: ~w~n"
           "  Time: ~wms~n"
           "  Throughput: ~w contexts/sec",
           [TotalContexts, NumWorkers, TimeMs, Throughput]),

    true = length(lists:flatten(AllContexts)) =:= TotalContexts,
    true = TimeMs < 1500,
    {save_config, [{parallel_throughput, Throughput} | Config]}.

test_context_update_performance(Config) ->
    % Create base context and perform many updates
    Context = erlmcp_error:new_context(test_op),

    {Time, UpdatedContexts} = timer:tc(fun() ->
        lists:foldl(fun(I, Ctx) ->
            erlmcp_error:add_context(Ctx, custom_field, I)
        end, Context, lists:seq(1, 100000))
    end),

    TimeMs = Time div 1000,
    ct:pal("Context update performance:~n"
           "  Updates: 100000~n"
           "  Time: ~wms~n"
           "  Throughput: ~w updates/sec",
           [TimeMs, (100000 * 1000) div TimeMs]),

    true = TimeMs < 1000.

%%====================================================================
%% Error Logging Tests
%%====================================================================

test_log_100k_errors_sequential(Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Logging 100K errors sequentially..."),

    Context = erlmcp_error:new_context(test_op),
    Error = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>),

    {Time, _} = timer:tc(fun() ->
        _ = [erlmcp_error:log_error(Error, Context) || _ <- lists:seq(1, 100000)]
    end),

    TimeMs = Time div 1000,
    Throughput = (100000 * 1000) div TimeMs,

    ct:pal("Sequential error logging:~n"
           "  Errors: 100000~n"
           "  Time: ~wms~n"
           "  Throughput: ~w errors/sec",
           [TimeMs, Throughput]),

    true = TimeMs < 5000,
    {save_config, [{logging_throughput, Throughput} | Config]}.

test_log_100k_errors_parallel(Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Logging 100K errors in parallel (20 workers)..."),

    NumWorkers = 20,
    ErrorsPerWorker = 5000,
    Context = erlmcp_error:new_context(parallel_log_test),
    Error = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>),

    {Time, _} = timer:tc(fun() ->
        Parent = self(),
        Workers = [
            spawn(fun() ->
                _ = [erlmcp_error:log_error(Error, Context) || _ <- lists:seq(1, ErrorsPerWorker)],
                Parent ! {self(), done}
            end)
            || _ <- lists:seq(1, NumWorkers)
        ],
        [receive {_Pid, done} -> ok end || _ <- Workers]
    end),

    TimeMs = Time div 1000,
    TotalErrors = NumWorkers * ErrorsPerWorker,
    Throughput = (TotalErrors * 1000) div TimeMs,

    ct:pal("Parallel error logging:~n"
           "  Total errors: ~w~n"
           "  Workers: ~w~n"
           "  Time: ~wms~n"
           "  Throughput: ~w errors/sec",
           [TotalErrors, NumWorkers, TimeMs, Throughput]),

    true = TimeMs < 3000.

test_log_errors_with_context(Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Logging 50K errors with full context..."),

    State = #{
        operation => test_op,
        client_id => <<"client-1">>,
        user_id => <<"user-1">>,
        request_id => <<"req-1">>,
        phase => initialized,
        transport => tcp
    },

    Error = erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"bad params">>),

    {Time, _} = timer:tc(fun() ->
        _ = [
            erlmcp_error:log_error_with_context(Error, error, State)
            || _ <- lists:seq(1, 50000)
        ]
    end),

    TimeMs = Time div 1000,
    ct:pal("Contextual error logging (50K):~n"
           "  Time: ~wms~n"
           "  Throughput: ~w errors/sec",
           [TimeMs, (50000 * 1000) div TimeMs]),

    true = TimeMs < 3000.

test_log_errors_all_severity_levels(Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Logging errors with all severity levels..."),

    Levels = [debug, info, warning, error, critical],
    ErrorsPerLevel = 10000,
    Context = erlmcp_error:new_context(severity_test),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Level) ->
            Error = erlmcp_error:error(?JSONRPC_INTERNAL_ERROR, <<"test">>),
            _ = [erlmcp_error:log_error(Error, Context, Level) || _ <- lists:seq(1, ErrorsPerLevel)]
        end, Levels)
    end),

    TimeMs = Time div 1000,
    TotalErrors = length(Levels) * ErrorsPerLevel,

    ct:pal("Multi-level error logging (50K total):~n"
           "  Time: ~wms~n"
           "  Throughput: ~w errors/sec",
           [TimeMs, (TotalErrors * 1000) div TimeMs]),

    true = TimeMs < 5000.

%%====================================================================
%% Categorization Tests
%%====================================================================

test_categorize_100k_mixed_errors(Config) ->
    ct:log("Categorizing 100K mixed errors..."),

    ErrorTypes = [
        erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"t">>),
        erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"p">>),
        erlmcp_error:error(?MCP_ERROR_RESOURCE_NOT_FOUND, <<"nf">>),
        erlmcp_error:error(?MCP_ERROR_RATE_LIMITED, <<"rl">>)
    ],

    {Time, Results} = timer:tc(fun() ->
        [erlmcp_error:categorize(lists:nth((I rem 4) + 1, ErrorTypes))
         || I <- lists:seq(1, 100000)]
    end),

    TimeMs = Time div 1000,
    Throughput = (100000 * 1000) div TimeMs,

    % Count categories
    Transient = length([ok || transient <- Results]),
    Permanent = length([ok || permanent <- Results]),

    ct:pal("Error categorization (100K):~n"
           "  Time: ~wms~n"
           "  Throughput: ~w categorizations/sec~n"
           "  Transient: ~w~n"
           "  Permanent: ~w",
           [TimeMs, Throughput, Transient, Permanent]),

    true = TimeMs < 1000,
    true = Transient > 0,
    true = Permanent > 0.

test_categorize_error_performance(_Config) ->
    ct:log("Testing categorization performance..."),

    {Time, _} = timer:tc(fun() ->
        _ = [erlmcp_error:categorize(?MCP_ERROR_TIMEOUT) || _ <- lists:seq(1, 100000)]
    end),

    TimeMs = Time div 1000,
    ct:pal("Simple categorization (100K):~n"
           "  Time: ~wms~n"
           "  Throughput: ~w/sec",
           [TimeMs, (100000 * 1000) div TimeMs]),

    true = TimeMs < 100.

test_retryable_check_throughput(_Config) ->
    ct:log("Testing retryable check performance..."),

    Error = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"t">>),

    {Time, _} = timer:tc(fun() ->
        _ = [erlmcp_error:is_retryable(Error) || _ <- lists:seq(1, 100000)]
    end),

    TimeMs = Time div 1000,
    ct:pal("Retryable check (100K):~n"
           "  Time: ~wms~n"
           "  Throughput: ~w/sec",
           [TimeMs, (100000 * 1000) div TimeMs]),

    true = TimeMs < 100.

%%====================================================================
%% Metrics and Statistics Tests
%%====================================================================

test_collect_100k_errors(Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Collecting 100K errors into statistics..."),

    {Time, _} = timer:tc(fun() ->
        _ = [erlmcp_error:collect_error(transient, 1) || _ <- lists:seq(1, 100000)]
    end),

    TimeMs = Time div 1000,
    timer:sleep(500),
    Stats = erlmcp_error:get_error_stats(),
    Total = maps:get(total, Stats, 0),

    ct:pal("Error collection (100K):~n"
           "  Time: ~wms~n"
           "  Collected: ~w~n"
           "  Throughput: ~w/sec",
           [TimeMs, Total, (100000 * 1000) div TimeMs]),

    true = Total >= 95000,  % Allow for timing variance
    {save_config, [{collection_time, TimeMs} | Config]}.

test_metrics_accuracy(Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Testing metrics accuracy..."),

    % Collect different error types
    NumTransient = 30000,
    NumPermanent = 20000,
    NumUnknown = 5000,

    [erlmcp_error:collect_error(transient, 1) || _ <- lists:seq(1, NumTransient)],
    [erlmcp_error:collect_error(permanent, 1) || _ <- lists:seq(1, NumPermanent)],
    [erlmcp_error:collect_error(unknown, 1) || _ <- lists:seq(1, NumUnknown)],

    timer:sleep(500),
    Stats = erlmcp_error:get_error_stats(),

    Transient = maps:get(transient, Stats, 0),
    Permanent = maps:get(permanent, Stats, 0),
    Total = maps:get(total, Stats, 0),

    ct:pal("Metrics accuracy:~n"
           "  Expected transient: ~w, Got: ~w~n"
           "  Expected permanent: ~w, Got: ~w~n"
           "  Expected total: ~w, Got: ~w",
           [NumTransient, Transient, NumPermanent, Permanent,
            NumTransient + NumPermanent + NumUnknown, Total]),

    % Allow 5% variance due to timing
    true = abs(Transient - NumTransient) < NumTransient div 20,
    true = abs(Permanent - NumPermanent) < NumPermanent div 20.

test_stats_concurrent_access(Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Testing concurrent stats access..."),

    NumWorkers = 50,
    AccessesPerWorker = 2000,

    {Time, _} = timer:tc(fun() ->
        Parent = self(),
        Workers = [
            spawn(fun() ->
                _ = [
                    erlmcp_error:get_error_stats()
                    || _ <- lists:seq(1, AccessesPerWorker)
                ],
                Parent ! {self(), done}
            end)
            || _ <- lists:seq(1, NumWorkers)
        ],
        [receive {_Pid, done} -> ok end || _ <- Workers]
    end),

    TimeMs = Time div 1000,
    TotalAccesses = NumWorkers * AccessesPerWorker,

    ct:pal("Concurrent stats access (100K accesses):~n"
           "  Workers: ~w~n"
           "  Time: ~wms~n"
           "  Throughput: ~w accesses/sec",
           [NumWorkers, TimeMs, (TotalAccesses * 1000) div TimeMs]),

    true = TimeMs < 5000.

%%====================================================================
%% Recovery Tests
%%====================================================================

test_backoff_delays_100k(_Config) ->
    ct:log("Testing backoff delay calculation at 100K scale..."),

    {Time, Delays} = timer:tc(fun() ->
        [erlmcp_error:backoff_delay(I rem 5, 5) || I <- lists:seq(1, 100000)]
    end),

    TimeMs = Time div 1000,
    MinDelay = lists:min(Delays),
    MaxDelay = lists:max(Delays),
    AvgDelay = lists:sum(Delays) div length(Delays),

    ct:pal("Backoff delay calculation (100K):~n"
           "  Time: ~wms~n"
           "  Min delay: ~wms~n"
           "  Max delay: ~wms~n"
           "  Avg delay: ~wms",
           [TimeMs, MinDelay, MaxDelay, AvgDelay]),

    true = TimeMs < 1000,
    true = MaxDelay < 35000.

test_extract_error_info_100k(_Config) ->
    ct:log("Testing error info extraction at 100K scale..."),

    Context = erlmcp_error:new_context(test_op),
    Error = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>, #{}, Context),

    {Time, _} = timer:tc(fun() ->
        _ = [erlmcp_error:extract_error_info(Error) || _ <- lists:seq(1, 100000)]
    end),

    TimeMs = Time div 1000,
    ct:pal("Error info extraction (100K):~n"
           "  Time: ~wms~n"
           "  Throughput: ~w/sec",
           [TimeMs, (100000 * 1000) div TimeMs]),

    true = TimeMs < 1000.

test_recovery_decisions_accuracy(_Config) ->
    ct:log("Testing recovery decision accuracy..."),

    TransientErr = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"t">>),
    PermanentErr = erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"p">>),

    {Time, Results} = timer:tc(fun() ->
        [
            [
                {erlmcp_error:should_retry(TransientErr, 1), should_retry_transient},
                {erlmcp_error:should_retry(PermanentErr, 1), should_retry_permanent},
                {erlmcp_error:should_retry(TransientErr, 0), should_retry_zero_attempts},
                {erlmcp_error:is_retryable(TransientErr), is_retryable_transient},
                {erlmcp_error:is_retryable(PermanentErr), is_retryable_permanent}
            ]
            || _ <- lists:seq(1, 20000)
        ]
    end),

    TimeMs = Time div 1000,

    % Verify accuracy
    TransientRetries = length([ok || {true, should_retry_transient} <- lists:flatten(Results)]),
    PermanentRetries = length([ok || {true, should_retry_permanent} <- lists:flatten(Results)]),
    ZeroRetries = length([ok || {false, should_retry_zero_attempts} <- lists:flatten(Results)]),

    ct:pal("Recovery decision accuracy (100K):~n"
           "  Time: ~wms~n"
           "  Transient retries: ~w/20000~n"
           "  Permanent retries: ~w/20000~n"
           "  Zero attempt retries: ~w/20000",
           [TimeMs, TransientRetries, PermanentRetries, ZeroRetries]),

    true = TransientRetries =:= 20000,
    true = PermanentRetries =:= 0,
    true = ZeroRetries =:= 20000.

%%====================================================================
%% Concurrent Scenario Tests
%%====================================================================

test_mixed_workload_100k(_Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Running mixed workload with 100K operations..."),

    {Time, _} = timer:tc(fun() ->
        Parent = self(),
        % Simulate 10 concurrent connection processes each handling 10K operations
        Workers = [
            spawn(fun() ->
                Context = erlmcp_error:new_context(
                    list_to_atom("worker_" ++ integer_to_list(W))
                ),
                _ = [
                    begin
                        Error = erlmcp_error:error(
                            ?MCP_ERROR_TIMEOUT,
                            <<"error">>,
                            #{attempt => I}
                        ),
                        erlmcp_error:log_error(Error, Context),
                        erlmcp_error:categorize(Error),
                        erlmcp_error:should_retry(Error, 2),
                        erlmcp_error:extract_error_info(Error)
                    end
                    || I <- lists:seq(1, 10000)
                ],
                Parent ! {self(), done}
            end)
            || W <- lists:seq(1, 10)
        ],
        [receive {_Pid, done} -> ok end || _ <- Workers]
    end),

    TimeMs = Time div 1000,
    ct:pal("Mixed workload (100K operations, 10 workers):~n"
           "  Time: ~wms~n"
           "  Throughput: ~w ops/sec",
           [TimeMs, (100000 * 1000) div TimeMs]),

    true = TimeMs < 10000.

test_network_failure_simulation(_Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Simulating network failure scenarios..."),

    % Simulate various failure modes
    FailureErrors = [
        ?MCP_ERROR_TIMEOUT,
        ?MCP_ERROR_TRANSPORT_ERROR,
        ?MCP_ERROR_RATE_LIMITED
    ],

    {Time, RecoveryDecisions} = timer:tc(fun() ->
        [
            erlmcp_error:should_retry(
                erlmcp_error:error(lists:nth((I rem 3) + 1, FailureErrors), <<"fail">>),
                1
            )
            || I <- lists:seq(1, 50000)
        ]
    end),

    TimeMs = Time div 1000,
    Retries = length([ok || true <- RecoveryDecisions]),

    ct:pal("Network failure simulation (50K errors):~n"
           "  Time: ~wms~n"
           "  Errors to retry: ~w~n"
           "  Recovery rate: ~.2f%",
           [TimeMs, Retries, (Retries / 50000) * 100]),

    true = Retries > 0.

test_cascading_errors(_Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Simulating cascading error scenarios..."),

    % Simulate cascading failures (error leads to more errors)
    {Time, ErrorChain} = timer:tc(fun() ->
        lists:foldl(fun(_I, Acc) ->
            PrevError = lists:last(Acc),
            NewError = erlmcp_error:wrap_error(PrevError, cascading_op),
            Acc ++ [NewError]
        end,
        [erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"initial">>)],
        lists:seq(1, 1000))
    end),

    TimeMs = Time div 1000,
    ChainLength = length(ErrorChain),

    ct:pal("Cascading error chain:~n"
           "  Time: ~wms~n"
           "  Chain length: ~w~n"
           "  Ops per error: ~w",
           [TimeMs, ChainLength, TimeMs div max(1, ChainLength)]),

    true = ChainLength > 900.

test_error_recovery_cycle(_Config) ->
    erlmcp_error:reset_error_stats(),
    ct:log("Simulating error recovery cycles..."),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Cycle) ->
            % Simulate a request that fails and retries
            Context = erlmcp_error:new_context(
                list_to_atom("cycle_" ++ integer_to_list(Cycle))
            ),
            InitialError = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>),

            % Check if retryable
            case erlmcp_error:should_retry(InitialError, 1) of
                true ->
                    % Calculate backoff
                    _Delay = erlmcp_error:backoff_delay(1, 3),
                    % Log the recovery attempt
                    erlmcp_error:log_error(InitialError, Context),
                    erlmcp_error:collect_error(transient, 1),
                    ok;
                false ->
                    erlmcp_error:log_error(InitialError, Context),
                    erlmcp_error:collect_error(permanent, 1)
            end
        end, lists:seq(1, 10000))
    end),

    TimeMs = Time div 1000,
    timer:sleep(500),
    Stats = erlmcp_error:get_error_stats(),
    Total = maps:get(total, Stats, 0),

    ct:pal("Error recovery cycles (10K):~n"
           "  Time: ~wms~n"
           "  Errors collected: ~w~n"
           "  Recovery throughput: ~w cycles/sec",
           [TimeMs, Total, (10000 * 1000) div TimeMs]),

    true = Total >= 9500.
